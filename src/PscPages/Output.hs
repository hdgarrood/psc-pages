
-- | Functions and types relevant to producing some form of output after
-- rendering documentation.

module PscPages.Output where

import Control.Applicative
import Control.Monad
import Control.Arrow (first)
import Data.List (nub)

import qualified Language.PureScript as P
import qualified Language.PureScript.Constants as C

import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import PscPages.Render

-- | A function that takes a list of bookmarks and a list of modules, and
-- returns an action that will probably do something like take the rendered
-- documentation and write it to disk.
type OutputFn = Bookmarks -> [P.Module] -> IO ()

-- | Specifies whether a PureScript source file is considered as:
--
-- 1) a target source file, i.e., we want to see its modules in the output
-- 2) a dependencies source file, i.e. we do not want to get output from it,
-- it is only there to enable desugaring, and to ensure that links between
-- modules are constructed correctly.
data FileInfo
  = TargetFile FilePath
  | DepsFile FilePath

fileInfoToString :: FileInfo -> FilePath
fileInfoToString (TargetFile fn) = fn
fileInfoToString (DepsFile fn) = fn

isTarget :: FileInfo -> Bool
isTarget (TargetFile _) = True
isTarget _ = False

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
--    desugaring.
--
-- Read, parse, and desugar the input files and hand them off to the output
-- function. Note that bookmarks will be generated for the union of target and
-- dependencies files, but the list of modules which are passed to the output
-- function contains only those which were in the target list.
parseAndDesugar :: [FilePath] -> [FilePath] -> OutputFn -> IO ()
parseAndDesugar = parseAndDesugar' False

-- | Like parseAndDesugar, except that the Prelude modules will be included
-- in the output.
parseAndDesugarWithPrelude :: [FilePath] -> [FilePath] -> OutputFn -> IO ()
parseAndDesugarWithPrelude = parseAndDesugar' True

parseAndDesugar' :: Bool -> [FilePath] -> [FilePath] -> OutputFn -> IO ()
parseAndDesugar' withPrelude inputFiles depsFiles outputFn = do
  inputFiles' <- parseAs TargetFile inputFiles
  depsFiles'  <- parseAs DepsFile depsFiles
  let preludeInfo = if withPrelude then TargetFile else DepsFile
  let allFiles = (preludeInfo "Prelude", P.prelude) : (inputFiles' ++ depsFiles')

  case P.parseModulesFromFiles fileInfoToString allFiles of
    Left err -> do
      hPutStrLn stderr "parse failed:"
      hPutStrLn stderr $ show err
      exitFailure
    Right ms -> do
      let targetModuleNames = map (P.getModuleName . snd) $ filter (isTarget . fst) ms
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
              let bookmarks = concatMap collectBookmarks modules
                  modules' = filter ((`elem` targetModuleNames) . P.getModuleName) modules
              in  outputFn bookmarks modules'

  where
  parseAs :: (FilePath -> a) -> [FilePath] -> IO [(a, String)]
  parseAs g = fmap (map (first g)) . mapM parseFile . nub

