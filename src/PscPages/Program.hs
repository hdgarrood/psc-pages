module PscPages.Program where

import Control.Applicative
import Control.Monad.Writer
import Data.Version (showVersion)

import Options.Applicative

import qualified System.FilePath.Glob as Glob

import PscPages.AsHtml
import PscPages.AsHoogle
import PscPages.Output
import PscPages.IOUtils
import qualified Paths_psc_pages as Paths

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
    in writeTextFile outputFile outputText

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
