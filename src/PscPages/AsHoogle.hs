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

import PscPages.Render
import PscPages.RenderedCode

renderPackageAsText :: RenderedPackage -> Text
renderPackageAsText RenderedPackage{..} = preamble <> modules
  where
  preamble =
    T.unlines [ "@package " <> T.pack rpName
              , "@version " <> T.pack rpVersion
              , ""
              ]
  modules =
    foldMap renderedModuleAsText rpModules

renderedCodeAsText :: RenderedCode -> Text
renderedCodeAsText = outputWith elemAsHtml
  where
  elemAsHtml (Syntax x)  = T.pack x
  elemAsHtml (Ident x)   = T.pack x
  elemAsHtml (Ctor x _)  = T.pack x
  elemAsHtml (Kind x)    = T.pack x
  elemAsHtml (Keyword x) = T.pack x
  elemAsHtml Space       = " "

renderedDeclAsText :: RenderedDeclaration -> Text
renderedDeclAsText RenderedDeclaration{..} =
  renderCommentsAsText rdComments <> renderedCodeAsText rdCode
  where

renderedModuleAsText :: RenderedModule -> Text
renderedModuleAsText RenderedModule{..} =
  renderCommentsAsText rmComments
    <> "module " <> T.pack rmName <> " where\n\n"
    <> foldMap ((<> "\n\n") . renderedDeclAsText . snd) rmDeclarations

renderCommentsAsText :: Maybe H.Html -> Text
renderCommentsAsText =
  maybe ""
        (LT.toStrict . LT.unlines . fmap ("-- | " <>) . LT.lines . renderHtml)
