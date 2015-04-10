{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module PscPages.AsHoogle where

-- | Functions for rendering generated documentation from PureScript code as
-- | an input file, suitable for supplying to Hoogle to build a database from.

import Data.Foldable (foldMap)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T

import qualified Text.Blaze.Html as H
import Text.Blaze.Html.Renderer.Text (renderHtml)

import qualified Language.PureScript as P

import System.FilePath ((</>))

import PscPages.Render
import PscPages.RenderedCode
import PscPages.Output
import PscPages.IOUtils
import PscPages.PackageMeta

outputHoogle :: OutputFn
outputHoogle outputDir pkgMeta _ bookmarks modules =
  let hoogle = outputPackageAsHoogle pkgMeta bookmarks modules
  in  writeTextFile (outputDir </> name ++ ".txt") hoogle
  where
  name = runPackageName (pkgMetaName pkgMeta)

outputPackageAsHoogle :: PackageMeta -> Bookmarks -> [P.Module] -> T.Text
outputPackageAsHoogle pkgMeta _ modules =
  packageAsHoogle (renderPackage pkgMeta modules)

packageAsHoogle :: RenderedPackage -> Text
packageAsHoogle RenderedPackage{..} = preamble <> modules
  where
  preamble =
    T.unlines [ "@package " <> T.pack rpName
              , "@version " <> T.pack rpVersion
              , ""
              ]
  modules =
    foldMap moduleAsHoogle rpModules

codeAsHoogle :: RenderedCode -> Text
codeAsHoogle = outputWith elemAsText
  where
  elemAsText (Syntax x)  = T.pack x
  elemAsText (Ident x)   = T.pack x
  elemAsText (Ctor x _)  = T.pack x
  elemAsText (Kind x)    = T.pack x
  elemAsText (Keyword x) = T.pack x
  elemAsText Space       = " "

declAsHoogle :: RenderedDeclaration -> Text
declAsHoogle RenderedDeclaration{..} =
  commentsAsHoogle rdComments <> codeAsHoogle rdCode
  where

moduleAsHoogle :: RenderedModule -> Text
moduleAsHoogle RenderedModule{..} =
  commentsAsHoogle rmComments
    <> "module " <> T.pack rmName <> " where\n\n"
    <> foldMap ((<> "\n\n") . declAsHoogle . snd) rmDeclarations

commentsAsHoogle :: Maybe H.Html -> Text
commentsAsHoogle =
  maybe ""
        (LT.toStrict . LT.unlines . fmap ("-- | " <>) . LT.lines . renderHtml)
