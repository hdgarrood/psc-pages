{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | Functions for rendering generated documentation from PureScript code as
-- HTML.

module PscPages.AsHtml where

import Control.Applicative
import Control.Monad
import Data.Ord (comparing)
import Data.Char (toUpper)
import Data.String (fromString)
import Data.Foldable (for_)
import Data.List (nub, intercalate, sortBy)
import Data.List.Split (splitOn)
import Data.Version
import qualified Data.Map as M

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Text.Blaze.Html ((!))
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html.Renderer.Text as H
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Language.PureScript as P

import System.FilePath ((</>))

import Data.FileEmbed

import PscPages.Types
import PscPages.RenderedCode hiding (sp)
import PscPages.Render
import PscPages.PackageMeta
import PscPages.Utils.IO
import PscPages.Utils.HtmlHelpers

data LinksContext = LinksContext
  { ctxPackageMeta      :: PackageMeta
  , ctxDepModules       :: M.Map P.ModuleName PackageName
  , ctxBookmarks        :: Bookmarks
  , ctxSourceModuleName :: P.ModuleName
  }
  deriving (Show)

data DocLink
  -- | A link to a declaration in the same module; in this case, only the title
  -- is needed to generate the link.
  = SameModule String

  -- | A link to a declaration in a different module, but still in the current
  -- package; we need to store the current module, the other declaration's
  -- module, and the title.
  | LocalModule P.ModuleName P.ModuleName String

  -- | A link to a declaration in a different package. We store: current module
  -- name, name of the other package, version of the other package, name of
  -- the module in the other package that the declaration is in, and
  -- declaration title.
  | DepsModule P.ModuleName PackageName Version P.ModuleName String

outputHtml :: OutputFn
outputHtml outputDir pkgMeta deps bookmarks modules = do
  let stylesheetFile = outputDir </> "style.css"
  mkParentDir stylesheetFile
  writeTextFile stylesheetFile stylesheet

  let bootstrapFile = outputDir </> "bootstrap.min.css"
  mkParentDir bootstrapFile
  writeTextFile bootstrapFile bootstrap

  let pkgId = packageIdentifier pkgMeta
  let contentsFile = outputDir </> "index.html"
  writeLazyTextFile contentsFile (H.renderHtml $ contentsPageHtml pkgId modules)

  let indexFile = outputDir </> "index/index.html"
  mkParentDir indexFile
  writeLazyTextFile indexFile (H.renderHtml (indexPageHtml pkgId))

  for_ ['a'..'z'] $ \c -> do
    let letterFile = outputDir </> ("index/" ++ c : ".html")
    writeLazyTextFile letterFile (H.renderHtml $ letterPageHtml pkgId c (takeLocals bookmarks))

  for_ modules (renderModule' outputDir pkgMeta deps bookmarks)

stylesheet :: T.Text
stylesheet = TE.decodeUtf8 $(embedFile "static/style.css")

bootstrap :: T.Text
bootstrap = TE.decodeUtf8 $(embedFile "static/bootstrap.min.css")

renderModule' :: FilePath -> PackageMeta -> M.Map P.ModuleName PackageName -> Bookmarks -> P.Module -> IO ()
renderModule' outputDir pkgMeta deps bookmarks m@(P.Module _ moduleName _ _) = do
  let filename = outputDir </> filePathFor moduleName
      html = H.renderHtml $ moduleToHtml pkgMeta deps bookmarks m
  mkParentDir filename
  writeLazyTextFile filename html

contentsPageHtml :: String -> [P.Module] -> H.Html
contentsPageHtml pkgId ms = do
  template pkgId "index.html" "Contents" $ do
    H.h2 $ text "Modules"
    H.ul $ for_ (sortBy (comparing $ \(P.Module _ moduleName _ _) -> moduleName) ms) $ \(P.Module _ moduleName _ _) -> H.li $
      H.a ! A.href (fromString (filePathFor moduleName `relativeTo` "index.html")) $ text (show moduleName)

indexPageHtml :: String -> H.Html
indexPageHtml pkgId = do
  template pkgId "index/index.html" "Index" $ do
    H.ul $ for_ ['a'..'z'] $ \c ->
      H.li $ H.a ! A.href (fromString (c : ".html")) $ text [toUpper c]

letterPageHtml :: String -> Char -> [(P.ModuleName, String)] -> H.Html
letterPageHtml pkgId c bs = do
  let filename = "index/" ++ c : ".html"
  template pkgId filename [toUpper c] $ do
    H.ul $ for_ (sortBy (comparing snd) . nub . filter matches $ bs) $ \(mn, s) -> H.li $ H.code $ do
      H.a ! A.href (fromString ((filePathFor mn `relativeTo` filename) ++ "#" ++ s)) $ text s
      sp *> text ("(" ++ show mn ++ ")")
  where
  matches (_, (c':_)) = toUpper c == toUpper c'
  matches _ = False

template :: String -> FilePath -> String -> H.Html -> H.Html
template pkgId curFile title body = do
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
            H.a ! A.class_ "navbar-brand" $ text pkgId
          H.ul ! A.class_ "nav navbar-nav" $ do
            H.li $ H.a ! A.href (fromString ("index.html" `relativeTo` curFile)) $ text "Contents"
            H.li $ H.a ! A.href (fromString ("index/index.html" `relativeTo` curFile)) $ text "Index"

      H.div ! A.class_ "container" ! A.id "content" $ do
        H.h1 $ text title
        body

-- | if `to` and `from` are both files in the current package, generate a
-- FilePath for `to` relative to `from`.
relativeTo :: FilePath -> FilePath -> FilePath
relativeTo to from = go (splitOn "/" to) (splitOn "/" from)
  where
  go (x : xs) (y : ys) | x == y = go xs ys
  go xs ys = intercalate "/" $ replicate (length ys - 1) ".." ++ xs

-- | Generate a FilePath for module documentation for a module in the current
-- package.
filePathFor :: P.ModuleName -> FilePath
filePathFor (P.ModuleName parts) = go parts
  where
  go [] = "index.html"
  go (x : xs) = show x </> go xs

-- | Like `relativeTo`, but in the case where `to` is in another package.
relativeToOtherPackage :: PackageName -> Version -> FilePath -> FilePath -> FilePath
relativeToOtherPackage (PackageName name) (showVersion -> vers) to from =
  intercalate "/" (dots ++ [name, vers] ++ splitOn "/" to)
  where
  dots = replicate (length from - 1) ".."

moduleToHtml :: PackageMeta -> M.Map P.ModuleName PackageName -> Bookmarks -> P.Module -> H.Html
moduleToHtml pkgMeta deps bookmarks m =
  template (packageIdentifier pkgMeta) (filePathFor moduleName) (show moduleName) $ do
    for_ (rmComments rm) id
    for_ (rmDeclarations rm) (declAsHtml linksContext)
  where
  rm = renderModule m
  moduleName = P.getModuleName m

  linksContext :: LinksContext
  linksContext = LinksContext pkgMeta deps bookmarks moduleName

declAsHtml :: LinksContext -> (String, RenderedDeclaration) -> H.Html
declAsHtml ctx (title, RenderedDeclaration{..}) = do
  H.a ! A.name (fromString title) ! A.href (fromString ('#' : title)) $
    H.h2 $ H.code $ text title
  para "decl" (H.code (codeAsHtml ctx rdCode))
  H.ul (mapM_ (H.li . H.code . codeAsHtml ctx) rdChildren)
  case rdComments of
    Just cs -> cs
    Nothing -> return ()
  for_ rdSourceSpan (linkToSource ctx)

codeAsHtml :: LinksContext -> RenderedCode -> H.Html
codeAsHtml ctx = outputWith elemAsHtml
  where
  elemAsHtml (Syntax x)  = withClass "syntax" (text x)
  elemAsHtml (Ident x)   = withClass "ident" (text x)
  elemAsHtml (Ctor x mn) = linkToConstructor ctx x mn (withClass "ctor" (text x))
  elemAsHtml (Kind x)    = text x
  elemAsHtml (Keyword x) = withClass "keyword" (text x)
  elemAsHtml Space       = text " "

getLink :: LinksContext -> String -> ContainingModule -> Maybe DocLink
getLink LinksContext{..} ctor' containingMod = do
  let bookmark = (fromContainingModule ctxSourceModuleName containingMod, ctor')
  guard (bookmark `elem` ignorePackages ctxBookmarks)

  case containingMod of
    ThisModule -> return (SameModule ctor')
    OtherModule destMn ->
      case M.lookup destMn ctxDepModules of
        Nothing -> return (LocalModule ctxSourceModuleName destMn ctor')
        Just pkgName -> do
          pkgVersion <- M.lookup pkgName (pkgMetaDependencies ctxPackageMeta)
          return (DepsModule ctxSourceModuleName pkgName pkgVersion destMn ctor')

renderLink :: DocLink -> H.Html -> H.Html
renderLink (SameModule x) = linkTo ('#' : x)
renderLink (LocalModule srcMn destMn x) =
  let uri = filePathFor destMn `relativeTo` filePathFor srcMn
  in  linkTo (uri ++ "#" ++ x)
renderLink (DepsModule srcMn pkgName pkgVersion destMn x) =
  let relativeTo' = relativeToOtherPackage pkgName pkgVersion
      uri = filePathFor destMn `relativeTo'` filePathFor srcMn
  in  linkTo (uri ++ "#" ++ x)

linkToConstructor :: LinksContext -> String -> ContainingModule -> H.Html -> H.Html
linkToConstructor ctx ctor' containMn =
  maybe id renderLink (getLink ctx ctor' containMn)

linkToSource :: LinksContext -> P.SourceSpan -> H.Html
linkToSource ctx (P.SourceSpan name start end) =
  linkTo (concat
            [githubBaseUrl, "/tree/master/", relativeToBase name, "#", fragment])
         (text "Source")
  where
  (P.SourcePos startLine _) = start
  (P.SourcePos endLine _) = end
  (pkgMetaGithub -> (GithubUser user, GithubRepo repo)) = ctxPackageMeta ctx

  relativeToBase = intercalate "/" . dropWhile (/= "src") . splitOn "/"
  githubBaseUrl = concat ["https://github.com/", user, "/", repo]
  fragment = "L" ++ show startLine ++ "-L" ++ show endLine
