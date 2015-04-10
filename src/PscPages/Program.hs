{-# LANGUAGE TupleSections #-}

module PscPages.Program where

import Data.Version (showVersion)
import Data.Maybe
import Data.List (stripPrefix)
import Data.List.Split (splitOn)

import Control.Applicative
import Control.Monad.Writer

import Options.Applicative

import System.FilePath ((</>))
import System.Directory (getCurrentDirectory)
import qualified System.FilePath.Glob as Glob

import PscPages.Types
import PscPages.AsHtml
import PscPages.AsHoogle
import PscPages.Output
import PscPages.PackageMeta
import PscPages.IOUtils
import qualified Paths_psc_pages as Paths

app :: FilePath -> IO ()
app outputDir = do
  inputFiles <- globRelative "src/**/*.purs"
  depsFiles' <- globRelative "bower_components/*/src/**/*.purs"
  let depsFiles = mapMaybe withPackageName depsFiles'

  pkgMeta <- getPackageMeta

  parseAndDesugar inputFiles depsFiles $ \deps bookmarks modules ->
    let outputBaseDir = outputBaseDirectory pkgMeta
        outputDir' = outputDir </> outputBaseDir
    in do
      putStrLn ("Writing generated documentation into " ++ outputDir' ++ "...")
      mkdirp outputDir'
      forM_ [outputHoogle, outputHtml] $ \f ->
        f outputDir' pkgMeta deps bookmarks modules

withPackageName :: FilePath -> Maybe (PackageName, FilePath)
withPackageName fp = (,fp) <$> getPackageName fp

getPackageName :: FilePath -> Maybe PackageName
getPackageName fp = do
  let xs = splitOn "/" fp
  ys <- stripPrefix ["bower_components"] xs
  y <- headMay ys
  return (PackageName y)

headMay :: [a] -> Maybe a
headMay (x:_) = Just x
headMay [] = Nothing

globRelative :: String -> IO [FilePath]
globRelative pat = do
  filesAbsolute <- Glob.glob pat
  currentDir <- getCurrentDirectory
  return (mapMaybe (stripPrefix (currentDir ++ "/")) filesAbsolute)

outputDirectoryP :: Parser FilePath
outputDirectoryP = strOption $
     short 'o'
  <> long "output"
  <> help "The output directory for HTML files"

main :: IO ()
main = execParser opts >>= app
  where
  opts        = info (helper <*> outputDirectoryP) infoModList
  infoModList = fullDesc <> headerInfo <> footerInfo
  headerInfo  = header   "psc-pages - Generate HTML documentation from PureScript source files"
  footerInfo  = footer $ "psc-pages " ++ showVersion Paths.version
