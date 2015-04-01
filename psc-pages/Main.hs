{-# LANGUAGE OverloadedStrings, TemplateHaskell, TupleSections #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Control.Arrow (first)
import Data.Ord (comparing)
import Data.Char (toUpper)
import Data.List (nub, sortBy)
import Data.String (fromString)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Version (showVersion)
import Data.Foldable (for_)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

import qualified Data.Text.Lazy.IO as TL

import Options.Applicative

import qualified Language.PureScript as P
import qualified Language.PureScript.Constants as C
import qualified Paths_psc_pages as Paths

import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStrLn, stderr)
import System.FilePath ((</>), takeDirectory)
import System.Directory (createDirectoryIfMissing)

import Text.Blaze.Html ((!))
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html.Renderer.Text as H
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.FileEmbed

import PscPages

app :: ([FilePath], FilePath) -> IO ()
app (input, outputDir) = do
  files <- mapM (fmap (first Just) . parseFile) (nub input)
  let e = P.parseModulesFromFiles (fromMaybe "") ((Nothing, P.prelude) : files)
  case e of
    Left err -> do
      hPutStrLn stderr $ show err
      exitFailure
    Right ms ->
      case P.sortModules . map (importPrim . importPrelude . snd) $ ms of
        Left e -> do
          hPutStrLn stderr e
          exitFailure
        Right (ms', _) ->
          case desugar ms' of
            Left err -> do
              hPutStrLn stderr $ P.prettyPrintMultipleErrors False err
              exitFailure
            Right modules -> do
              let bookmarks = concatMap collectBookmarks modules

              let stylesheetFile = outputDir </> "style.css"
              mkdirp stylesheetFile
              T.writeFile stylesheetFile stylesheet

              let bootstrapFile = outputDir </> "bootstrap.min.css"
              mkdirp bootstrapFile
              T.writeFile bootstrapFile bootstrap

              let contentsFile = outputDir </> "index.html"
              TL.writeFile contentsFile (H.renderHtml $ contentsPageHtml modules)

              let indexFile = outputDir </> "index/index.html"
              mkdirp indexFile
              TL.writeFile indexFile (H.renderHtml indexPageHtml)

              for_ ['a'..'z'] $ \c -> do
                let letterFile = outputDir </> ("index/" ++ c : ".html")
                TL.writeFile letterFile (H.renderHtml $ letterPageHtml c bookmarks)

              for_ modules (renderModule outputDir bookmarks)
              exitSuccess
  where
  stylesheet :: T.Text
  stylesheet = T.decodeUtf8 $(embedFile "static/style.css")

  bootstrap :: T.Text
  bootstrap = T.decodeUtf8 $(embedFile "static/bootstrap.min.css")

  parseFile :: FilePath -> IO (FilePath, String)
  parseFile input = (,) input <$> readFile input

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

collectBookmarks :: P.Module -> [(P.ModuleName, String)]
collectBookmarks (P.Module _ moduleName ds _) = map (moduleName, ) $ mapMaybe getDeclarationTitle ds

renderModule :: FilePath -> [(P.ModuleName, String)] -> P.Module -> IO ()
renderModule outputDir bookmarks m@(P.Module _ moduleName _ exps) = do
  let filename = outputDir </> filePathFor moduleName
      html = H.renderHtml $ moduleToHtml bookmarks m
  mkdirp filename
  TL.writeFile filename html

mkdirp :: FilePath -> IO ()
mkdirp = createDirectoryIfMissing True . takeDirectory

contentsPageHtml :: [P.Module] -> H.Html
contentsPageHtml ms = do
  template "index.html" "Contents" $ do
    H.h2 $ text "Modules"
    H.ul $ for_ (sortBy (comparing $ \(P.Module _ moduleName _ _) -> moduleName) ms) $ \(P.Module _ moduleName _ _) -> H.li $
      H.a ! A.href (fromString (filePathFor moduleName `relativeTo` "index.html")) $ text (show moduleName)

indexPageHtml :: H.Html
indexPageHtml = do
  template "index/index.html" "Index" $ do
    H.ul $ for_ ['a'..'z'] $ \c ->
      H.li $ H.a ! A.href (fromString (c : ".html")) $ text [toUpper c]

sp :: H.Html
sp = text " "

letterPageHtml :: Char -> [(P.ModuleName, String)] -> H.Html
letterPageHtml c bs = do
  let filename = "index/" ++ c : ".html"
  template filename [toUpper c] $ do
    H.ul $ for_ (sortBy (comparing snd) . nub . filter matches $ bs) $ \(mn, s) -> H.li $ H.code $ do
      H.a ! A.href (fromString ((filePathFor mn `relativeTo` filename) ++ "#" ++ s)) $ text s
      sp *> text ("(" ++ show mn ++ ")")
  where
  matches (_, (c':_)) = toUpper c == toUpper c'
  matches _ = False


inputFiles :: Parser [FilePath]
inputFiles = many . strArgument $
     metavar "FILE"
  <> help "The input .purs file(s)"

outputDirectory :: Parser FilePath
outputDirectory = strOption $
     short 'o'
  <> long "output"
  <> help "The output .js file"

main :: IO ()
main = execParser opts >>= app
  where
  opts        = info (helper <*> ((,) <$> inputFiles <*> outputDirectory)) infoModList
  infoModList = fullDesc <> headerInfo <> footerInfo
  headerInfo  = header   "psc-pages - Generate HTML documentation from PureScript source files"
  footerInfo  = footer $ "psc-pages " ++ showVersion Paths.version
