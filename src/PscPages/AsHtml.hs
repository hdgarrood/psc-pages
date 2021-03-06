{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Functions for rendering generated documentation from PureScript code as
-- HTML.

module PscPages.AsHtml where

import Control.Applicative
import Data.Ord (comparing)
import Data.Char (toUpper)
import Data.String (fromString)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.List (nub, intercalate, sortBy)
import Data.List.Split (splitOn)

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE

import Text.Blaze.Html ((!))
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html.Renderer.Text as H
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Language.PureScript as P

import System.FilePath ((</>))

import Data.FileEmbed

import PscPages.HtmlHelpers
import PscPages.RenderedCode hiding (sp)
import PscPages.Render
import PscPages.Output
import PscPages.IOUtils

-- | A tuple containing:
-- * bookmarks,
-- * source module name (that is, the name of the module which is currently
--   being generated).
type LinksContext = (Bookmarks, P.ModuleName)

outputAsHtml :: FilePath -> OutputFn
outputAsHtml outputDir bookmarks modules = do
  let stylesheetFile = outputDir </> "style.css"
  mkdirp stylesheetFile
  writeTextFile stylesheetFile stylesheet

  let bootstrapFile = outputDir </> "bootstrap.min.css"
  mkdirp bootstrapFile
  writeTextFile bootstrapFile bootstrap

  let contentsFile = outputDir </> "index.html"
  writeLazyTextFile contentsFile (H.renderHtml $ contentsPageHtml modules)

  let indexFile = outputDir </> "index/index.html"
  mkdirp indexFile
  writeLazyTextFile indexFile (H.renderHtml indexPageHtml)

  for_ ['a'..'z'] $ \c -> do
    let letterFile = outputDir </> ("index/" ++ c : ".html")
    writeLazyTextFile letterFile (H.renderHtml $ letterPageHtml c bookmarks)

  for_ modules (renderModule' outputDir bookmarks)

stylesheet :: T.Text
stylesheet = TE.decodeUtf8 $(embedFile "static/style.css")

bootstrap :: T.Text
bootstrap = TE.decodeUtf8 $(embedFile "static/bootstrap.min.css")

renderModule' :: FilePath -> [(P.ModuleName, String)] -> P.Module -> IO ()
renderModule' outputDir bookmarks m@(P.Module _ moduleName _ _) = do
  let filename = outputDir </> filePathFor moduleName
      html = H.renderHtml $ moduleToHtml bookmarks m
  mkdirp filename
  writeLazyTextFile filename html

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
moduleToHtml bookmarks m =
  template (filePathFor moduleName) (show moduleName) $ do
    for_ (rmComments rm) id
    for_ (rmDeclarations rm) (declAsHtml linksContext)
  where
  rm = renderModule m
  moduleName = P.getModuleName m

  linksContext :: LinksContext
  linksContext = (bookmarks, moduleName)

declAsHtml :: LinksContext -> (String, RenderedDeclaration) -> H.Html
declAsHtml ctx (title, RenderedDeclaration{..}) = do
  H.a ! A.name (fromString title) ! A.href (fromString ('#' : title)) $
    H.h2 $ H.code $ text title
  para "decl" (H.code (codeAsHtml ctx rdCode))
  H.ul (mapM_ (H.li . H.code . codeAsHtml ctx) rdChildren)
  case rdComments of
    Just cs -> cs
    Nothing -> return ()

codeAsHtml :: LinksContext -> RenderedCode -> H.Html
codeAsHtml ctx = outputWith elemAsHtml
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
