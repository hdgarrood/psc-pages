{-# LANGUAGE TupleSections #-}

-- | Functions and types relevant to producing some form of output after
-- rendering documentation.

module PscPages.Output where

import qualified Data.Map as M

import Control.Applicative
import Control.Monad
import Control.Arrow (first)

import qualified Language.PureScript as P
import qualified Language.PureScript.Constants as C

import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import PscPages.Render
import PscPages.Types

fileInfoToString :: FileInfo -> FilePath
fileInfoToString (Local fn) = fn
fileInfoToString (FromDep _ fn) = fn

addDefaultImport :: P.ModuleName -> P.Module -> P.Module
addDefaultImport toImport m@(P.Module coms mn decls exps)  =
  if isExistingImport `any` decls || mn == toImport then m
  else P.Module coms mn (P.ImportDeclaration toImport P.Implicit Nothing : decls) exps
  where
  isExistingImport (P.ImportDeclaration mn' _ _) | mn' == toImport = True
  isExistingImport (P.PositionedDeclaration _ _ d) = isExistingImport d
  isExistingImport _ = False

importPrim :: P.Module -> P.Module
importPrim = addDefaultImport (P.ModuleName [P.ProperName C.prim])

importPrelude :: P.Module -> P.Module
importPrelude = addDefaultImport (P.ModuleName [P.ProperName C.prelude])

desugar :: [P.Module] -> Either P.MultipleErrors [P.Module]
desugar = P.evalSupplyT 0 . desugar'
  where
  desugar' :: [P.Module] -> P.SupplyT (Either P.MultipleErrors) [P.Module]
  desugar' = mapM P.desugarDoModule >=> P.desugarCasesModule >=> P.desugarImports

parseFile :: FilePath -> IO (FilePath, String)
parseFile input' = (,) input' <$> readFile input'

-- |
-- Given:
--  * a list of target PureScript source files to generate docs for
--  * another list of PureScript source files which should contain all
--    necessary code to parse and desugar the target input files
--  * an output function, which specifies what to do after parsing and
--    desugaring,
--
-- Read, parse, and desugar the input files and hand them off to the output
-- function. Note that bookmarks will be generated for the union of target and
-- dependencies files, but the list of modules which are passed to the output
-- function contains only those which were in the target list.
parseAndDesugar ::
  [FilePath]
  -> [(PackageName, FilePath)]
  -> (M.Map P.ModuleName PackageName -> Bookmarks -> [P.Module] -> IO ())
  -> IO ()
parseAndDesugar =
  parseAndDesugar' False

-- | Like parseAndDesugar, except that the Prelude modules will be included
-- in the output.
parseAndDesugarWithPrelude ::
  [FilePath]
  -> [(PackageName, FilePath)]
  -> (M.Map P.ModuleName PackageName -> Bookmarks -> [P.Module] -> IO ())
  -> IO ()
parseAndDesugarWithPrelude =
  parseAndDesugar' True

parseAndDesugar' ::
  Bool
  -> [FilePath]
  -> [(PackageName, FilePath)]
  -> (M.Map P.ModuleName PackageName -> Bookmarks -> [P.Module] -> IO ())
  -> IO ()
parseAndDesugar' withPrelude inputFiles depsFiles callback = do
  inputFiles' <- mapM (parseAs Local) inputFiles
  depsFiles'  <- mapM (\(pkgName, f) -> parseAs (FromDep pkgName) f) depsFiles
  let preludeInfo = if withPrelude then Local else FromDep (PackageName "prelude")
  let allFiles = (preludeInfo "<prelude>", P.prelude) : (inputFiles' ++ depsFiles')

  case P.parseModulesFromFiles fileInfoToString allFiles of
    Left err -> do
      hPutStrLn stderr "parse failed:"
      hPutStrLn stderr $ show err
      exitFailure
    Right ms -> do
      let depsModules = getDepsModuleNames (map (\(fp, m) -> (,m) <$> fp) ms)
      case P.sortModules . map (importPrim . importPrelude . snd) $ ms of
        Left e' -> do
          hPutStrLn stderr "sortModules failed:"
          hPutStrLn stderr e'
          exitFailure
        Right (ms', _) ->
          case desugar ms' of
            Left err -> do
              hPutStrLn stderr "desugar failed:"
              hPutStrLn stderr $ P.prettyPrintMultipleErrors False err
              exitFailure
            Right modules ->
              let modules' = map (addPackage depsModules) modules
                  bookmarks = concatMap collectBookmarks modules'
              in  callback depsModules bookmarks (takeLocals modules')

  where
  parseAs :: (FilePath -> a) -> FilePath -> IO (a, String)
  parseAs g = fmap (first g) . parseFile

  getDepsModuleNames :: [InPackage (FilePath, P.Module)] -> M.Map P.ModuleName PackageName
  getDepsModuleNames = foldl go M.empty
    where
    go deps p = deps # case p of
      Local _ -> id
      FromDep pkgName (_, m) -> M.insert (P.getModuleName m) pkgName
    (#) = flip ($)

  addPackage :: M.Map P.ModuleName PackageName -> P.Module -> InPackage P.Module
  addPackage depsModules m =
    case M.lookup (P.getModuleName m) depsModules of
      Just pkgName -> FromDep pkgName m
      Nothing -> Local m
