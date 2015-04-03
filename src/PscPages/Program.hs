{-# LANGUAGE OverloadedStrings, TemplateHaskell, TupleSections #-}

module PscPages.Program where

import Control.Applicative
import Control.Monad.Writer
import Data.Ord (comparing)
import Data.Char (toUpper)
import Data.List (nub, sortBy)
import Data.String (fromString)
import Data.Version (showVersion)
import Data.Foldable (for_)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

import qualified Data.Text.Lazy.IO as TL

import Options.Applicative

import qualified Language.PureScript as P
import qualified Paths_psc_pages as Paths

import System.Exit (exitSuccess)
import System.FilePath ((</>), takeDirectory)
import System.Directory (createDirectoryIfMissing)
import qualified System.FilePath.Glob as Glob

import Text.Blaze.Html ((!))
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html.Renderer.Text as H
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.FileEmbed

import PscPages.Render
import PscPages.HtmlHelpers
import PscPages.AsHtml
import PscPages.AsHoogle
import PscPages.Output

app :: ([FilePath], FilePath) -> IO ()
app (input, outputDir) =
  parseAndDesugar input [] (outputAsHtml outputDir)

appHoogle :: String -> String -> FilePath -> FilePath -> IO ()
appHoogle name vers inputDir outputFile = do
  let srcPattern = Glob.compile "src/**/*.purs"
      depsPattern = Glob.compile "bower_components/*/src/**/*.purs"
  inputFiles <- Glob.globDir1 srcPattern inputDir
  depsFiles <- Glob.globDir1 depsPattern inputDir
  parseAndDesugar inputFiles depsFiles $ \bookmarks modules ->
    let outputText = outputPackageAsHoogle name vers bookmarks modules
    in T.writeFile outputFile outputText

outputAsHtml :: FilePath -> OutputFn
outputAsHtml outputDir bookmarks modules = do
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

  for_ modules (renderModule' outputDir bookmarks)
  exitSuccess

stylesheet :: T.Text
stylesheet = T.decodeUtf8 $(embedFile "static/style.css")

bootstrap :: T.Text
bootstrap = T.decodeUtf8 $(embedFile "static/bootstrap.min.css")

renderModule' :: FilePath -> [(P.ModuleName, String)] -> P.Module -> IO ()
renderModule' outputDir bookmarks m@(P.Module _ moduleName _ _) = do
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

outputPackageAsHoogle :: String -> String -> Bookmarks -> [P.Module] -> T.Text
outputPackageAsHoogle name vers _ modules =
  packageAsHoogle (renderPackage name vers modules)

inputFilesP :: Parser [FilePath]
inputFilesP = many . strArgument $
     metavar "FILE"
  <> help "The input .purs file(s)"

outputDirectoryP :: Parser FilePath
outputDirectoryP = strOption $
     short 'o'
  <> long "output"
  <> help "The output directory for HTML files"

main :: IO ()
main = execParser opts >>= app
  where
  opts        = info (helper <*> ((,) <$> inputFilesP <*> outputDirectoryP)) infoModList
  infoModList = fullDesc <> headerInfo <> footerInfo
  headerInfo  = header   "psc-pages - Generate HTML documentation from PureScript source files"
  footerInfo  = footer $ "psc-pages " ++ showVersion Paths.version
