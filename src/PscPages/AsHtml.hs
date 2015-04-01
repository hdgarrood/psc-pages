{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module PscPages.AsHtml where

-- | Functions for rendering generated documentation from PureScript code as
-- | HTML.

import Data.String (fromString)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Data.List.Split (splitOn)

import Text.Blaze.Html ((!))
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Language.PureScript as P

import System.FilePath ((</>))

import PscPages.HtmlHelpers
import PscPages.RenderedCode hiding (sp)
import PscPages.Render

-- A tuple containing:
--    - bookmarks,
--    - source module name (that is, the name of the module which is currently
--      being generated)
type LinksContext = ([(P.ModuleName, String)], P.ModuleName)

template :: FilePath -> String -> H.Html -> H.Html
template curFile title body = do
  H.docType
  H.html $ do
    H.head $ do
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href (fromString ("bootstrap.min.css" `relativeTo` curFile))
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href (fromString ("style.css" `relativeTo` curFile))
      H.title $ H.toHtml title
    H.body $ do
      H.div ! A.class_ "navbar navbar-default" $ do
        H.div ! A.class_ "container" $ do
          H.div ! A.class_ "navbar-header" $ do
            H.a ! A.class_ "navbar-brand" $ text "Core Libraries"
          H.ul ! A.class_ "nav navbar-nav" $ do
            H.li $ H.a ! A.href (fromString ("index.html" `relativeTo` curFile)) $ text "Contents"
            H.li $ H.a ! A.href (fromString ("index/index.html" `relativeTo` curFile)) $ text "Index"

      H.div ! A.class_ "container" ! A.id "content" $ do
        H.h1 $ text title
        body

relativeTo :: FilePath -> FilePath -> FilePath
relativeTo to from = go (splitOn "/" to) (splitOn "/" from)
  where
  go (x : xs) (y : ys) | x == y = go xs ys
  go xs ys = intercalate "/" $ replicate (length ys - 1) ".." ++ xs

filePathFor :: P.ModuleName -> FilePath
filePathFor (P.ModuleName parts) = go parts
  where
  go [] = "index.html"
  go (x : xs) = show x </> go xs

moduleToHtml :: [(P.ModuleName, String)] -> P.Module -> H.Html
moduleToHtml bookmarks (P.Module coms moduleName ds exps) =
  template (filePathFor moduleName) (show moduleName) $ do
    for_ (renderComments coms) id
    for_ (filter (P.isExported exps) ds) (declToHtml linksContext exps)
  where
  linksContext :: LinksContext
  linksContext = (bookmarks, moduleName)

declToHtml :: LinksContext -> Maybe [P.DeclarationRef] -> P.Declaration -> H.Html
declToHtml ctx exps decl = do
  for_ (getDeclarationTitle decl) $ \s ->
    H.a ! A.name (fromString s) ! A.href (fromString ('#' : s)) $
      H.h2 $ H.code $ text s
  case renderDeclaration exps decl of
    Just rd -> renderedDeclAsHtml ctx rd
    Nothing -> return ()

renderedDeclAsHtml :: LinksContext -> RenderedDeclaration -> H.Html
renderedDeclAsHtml ctx (RenderedDeclaration {..}) = do
  para "decl" (H.code (renderedCodeAsHtml ctx rdCode))
  H.ul (mapM_ (H.li . H.code . renderedCodeAsHtml ctx) rdChildren)
  case rdComments of
    Just cs -> cs
    Nothing -> return ()

renderedCodeAsHtml :: LinksContext -> RenderedCode -> H.Html
renderedCodeAsHtml ctx = outputWith elemAsHtml
  where
  elemAsHtml (Syntax x)  = withClass "syntax" (text x)
  elemAsHtml (Ident x)   = withClass "ident" (text x)
  elemAsHtml (Ctor x mn) = linkToConstructor ctx x mn (withClass "ctor" (text x))
  elemAsHtml (Kind x)    = text x
  elemAsHtml (Keyword x) = withClass "keyword" (text x)
  elemAsHtml Space       = text " "

linkToConstructor :: LinksContext -> String -> Maybe P.ModuleName -> H.Html -> H.Html
linkToConstructor (bookmarks, srcMn) ctor' mn contents
  | (fromMaybe srcMn mn, ctor') `notElem` bookmarks = contents
  | otherwise = case mn of
      Nothing -> linkTo ('#' : ctor') contents
      Just destMn ->
        let uri = filePathFor destMn `relativeTo` filePathFor srcMn
        in  linkTo (uri ++ "#" ++ ctor') contents
