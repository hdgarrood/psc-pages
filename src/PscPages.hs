{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PscPages where

import Control.Applicative
import Control.Monad
import Data.Monoid ((<>), mempty, Monoid)
import Data.Default (def)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.String (fromString)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Foldable (for_)

import Data.Text (Text)
import qualified Data.Text as T

import qualified Language.PureScript as P

import System.FilePath ((</>))

import Text.Blaze.Html ((!))
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import PscPages.RenderedCode

import qualified Cheapskate

getDeclarationTitle :: P.Declaration -> Maybe String
getDeclarationTitle (P.TypeDeclaration name _)          = Just (show name)
getDeclarationTitle (P.ExternDeclaration _ name _ _)    = Just (show name)
getDeclarationTitle (P.DataDeclaration _ name _ _)      = Just (show name)
getDeclarationTitle (P.ExternDataDeclaration name _)    = Just (show name)
getDeclarationTitle (P.TypeSynonymDeclaration name _ _) = Just (show name)
getDeclarationTitle (P.TypeClassDeclaration name _ _ _) = Just (show name)
getDeclarationTitle (P.PositionedDeclaration _ _ d)     = getDeclarationTitle d
getDeclarationTitle _                                   = Nothing

para :: H.AttributeValue -> H.Html -> H.Html
para className content = H.p ! A.class_ className $ content

withClass :: H.AttributeValue -> H.Html -> H.Html
withClass className content = H.span ! A.class_ className $ content

text :: String -> H.Html
text = H.toHtml

intercalateA_ :: (Applicative m) => m b -> [m a] -> m ()
intercalateA_ _   []     = pure ()
intercalateA_ _   [x]    = void x
intercalateA_ sep (x:xs) = (x <* sep) *> intercalateA_ sep xs

mintersperse :: (Monoid m) => m -> [m] -> m
mintersperse _ []       = mempty
mintersperse _ [x]      = x
mintersperse sep (x:xs) = x <> sep <> mintersperse sep xs

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
    renderComments coms
    for_ (filter (P.isExported exps) ds) (declToHtml exps)
  where
  declToHtml :: Maybe [P.DeclarationRef] -> P.Declaration -> H.Html
  declToHtml exps decl = do
    for_ (getDeclarationTitle decl) $ \s ->
      H.a ! A.name (fromString s) ! A.href (fromString ('#' : s)) $
        H.h2 $ H.code $ text s
    case renderDeclaration exps decl of
      Just rd -> H.toHtml rd
      Nothing -> return ()

data RenderedDeclaration = RenderedDeclaration
  { rdComments :: Maybe H.Html
  , rdCode     :: RenderedCode
  , rdChildren :: [RenderedCode]
  }

basicDeclaration :: RenderedCode -> Maybe RenderedDeclaration
basicDeclaration code = Just (RenderedDeclaration Nothing code [])

instance H.ToMarkup RenderedDeclaration where
  toMarkup (RenderedDeclaration {..}) = do
    para "decl" (H.code (asHtml rdCode))
    H.ul (mapM_ (H.li . H.code . asHtml) rdChildren)
    case rdComments of
      Just cs -> cs
      Nothing -> return ()

renderDeclaration :: Maybe [P.DeclarationRef] -> P.Declaration -> Maybe RenderedDeclaration
renderDeclaration _ (P.TypeDeclaration ident' ty) =
  basicDeclaration code
  where
  code = ident (show ident')
          <> sp <> syntax "::" <> sp
          <> renderType ty
renderDeclaration _ (P.ExternDeclaration _ ident' _ ty) =
  basicDeclaration code
  where
  code = ident (show ident')
          <> sp <> syntax "::" <> sp
          <> renderType ty
renderDeclaration exps (P.DataDeclaration dtype name args ctors) =
  Just (RenderedDeclaration Nothing code children)
  where
  typeApp  = foldl P.TypeApp (P.TypeConstructor (P.Qualified Nothing name)) (map toTypeVar args)
  exported = filter (P.isDctorExported name exps . fst) ctors
  code = keyword (show dtype) <> sp <> renderType typeApp
  children = map renderCtor exported
  renderCtor (ctor, tys) =
          let typeApp' = foldl P.TypeApp (P.TypeConstructor (P.Qualified Nothing ctor)) tys
          in renderType typeApp'
renderDeclaration _ (P.ExternDataDeclaration name kind') =
  basicDeclaration code
  where
  code = keywordData <> sp
          <> renderType (P.TypeConstructor (P.Qualified Nothing name))
          <> sp <> syntax "::" <> sp
          <> kind (P.prettyPrintKind kind') -- TODO: Proper renderKind function?
renderDeclaration _ (P.TypeSynonymDeclaration name args ty) =
  basicDeclaration code
  where
  typeApp = foldl P.TypeApp (P.TypeConstructor (P.Qualified Nothing name)) (map toTypeVar args)
  code = mintersperse sp
          [ keywordType
          , renderType typeApp
          , syntax "="
          , renderType ty
          ]
renderDeclaration _ (P.TypeClassDeclaration name args implies ds) = do
  Just (RenderedDeclaration Nothing code children)
  where
  code = mintersperse sp $
           [keywordClass]
            ++ maybe [] (:[]) superclasses
            ++ [renderType classApp]
            ++ if (not (null ds)) then [keywordWhere] else []

  superclasses
    | null implies = Nothing
    | otherwise = Just $
        syntax "("
         <> mintersperse (syntax "," <> sp) (map renderImplies implies)
         <> syntax ") <="

  renderImplies (pn, tys) =
    let supApp = foldl P.TypeApp (P.TypeConstructor pn) tys
    in renderType supApp

  classApp  = foldl P.TypeApp (P.TypeConstructor (P.Qualified Nothing name)) (map toTypeVar args)

  children = map renderClassMember ds

  renderClassMember (P.PositionedDeclaration _ _ d) = renderClassMember d
  renderClassMember (P.TypeDeclaration ident' ty) =
    mintersperse sp
      [ ident (show ident')
      , syntax "::"
      , renderType ty
      ]
  renderClassMember _ = error "Invalid argument to renderClassMember."
renderDeclaration _ (P.TypeInstanceDeclaration name constraints className tys _) = do
  basicDeclaration code
  where
  code =
    mintersperse sp $
      [ keywordInstance
      , ident (show name)
      , syntax "::"
      ] ++ maybe [] (:[]) constraints'
        ++ [ renderType classApp
           , keywordWhere
           ]

  constraints'
    | null constraints = Nothing
    | otherwise = Just (syntax "(" <> renderedConstraints <> syntax ") =>")

  renderedConstraints = mintersperse (syntax "," <> sp) (map renderConstraint constraints)

  renderConstraint (pn, tys') =
    let supApp = foldl P.TypeApp (P.TypeConstructor pn) tys'
    in renderType supApp

  classApp = foldl P.TypeApp (P.TypeConstructor className) tys
renderDeclaration exps (P.PositionedDeclaration _ com d) =
  case renderDeclaration exps d of
    Just rd -> Just (rd { rdComments = Just (renderComments com) })
    other -> other
renderDeclaration _ _ = Nothing

renderComments :: [P.Comment] -> H.Html
renderComments cs = do
  let raw = concatMap toLines cs

  when (all hasPipe raw) $
    H.toHtml . Cheapskate.markdown def . fromString . unlines . map stripPipes $ raw
  where

  toLines (P.LineComment s) = [s]
  toLines (P.BlockComment s) = lines s

  hasPipe s = case dropWhile (== ' ') s of { ('|':_) -> True; _ -> False }

  stripPipes = dropPipe . dropWhile (== ' ')

  dropPipe ('|':' ':s) = s
  dropPipe ('|':s) = s
  dropPipe s = s

linkToConstructor :: P.ModuleName -> [(P.ModuleName, String)] -> P.Qualified P.ProperName -> H.Html -> H.Html
linkToConstructor srcModule bookmarks (P.Qualified mn ctor) contents | (fromMaybe srcModule mn, show ctor) `notElem` bookmarks = contents
linkToConstructor _ _ (P.Qualified Nothing ctor) contents = H.a ! A.href (fromString ('#' : show ctor)) $ contents
linkToConstructor srcModule _ (P.Qualified (Just destModule) ctor) contents = H.a ! A.href (fromString (uri ++ "#" ++ show ctor)) $ contents
  where
  uri = filePathFor destModule `relativeTo` filePathFor srcModule

toTypeVar :: (String, Maybe P.Kind) -> P.Type
toTypeVar (s, Nothing) = P.TypeVar s
toTypeVar (s, Just k) = P.KindedType (P.TypeVar s) k

for :: [a] -> (a -> b) -> [b]
for = flip map

renderType :: P.Type -> RenderedCode
renderType =
  go 0
  . P.everywhereOnTypes dePrim
  . P.everywhereOnTypesTopDown convertForAlls
  . P.everywhereOnTypes convert

  where
  go :: Int -> P.Type -> RenderedCode
  go _ P.TypeWildcard = syntax "_"
  go _ (P.TypeVar var) = ident var
  go _ (P.PrettyPrintObject row) =
    syntax "{" <> sp <> renderRow row <> sp <> syntax "}"
  go _ (P.PrettyPrintArray ty) =
    syntax "[" <> go 0 ty <> syntax "]"
  go _ (P.TypeConstructor ctor'@(P.Qualified _ name)) =
    ctor (show ctor') (Just (show name))
  go n (P.ConstrainedType deps ty) =
    syntax "(" <> constraints <> syntax ")" <> sp
      <> syntax "=>" <> sp <> go n ty
    where
    constraints = mintersperse (syntax "," <> sp) $ for deps $ \(pn, tys) ->
        let instApp = foldl P.TypeApp (P.TypeConstructor pn) tys
        in  go 0 instApp
  go _ P.REmpty = syntax "()"
  go _ row@P.RCons{} =
    syntax "(" <> renderRow row <> syntax ")"
  go n (P.PrettyPrintFunction arg ret) | n < 1 =
    go (n + 1) arg <> sp <> syntax "->" <> sp <> go 0 ret
  go n (P.PrettyPrintForAll idents ty) | n < 1 =
    keywordForall <> sp <> idents'
      <> syntax "." <> sp <> go 0 ty
    where
    idents' = mintersperse sp (map ident idents)
  go n (P.TypeApp ty1 ty2) | n < 1 =
    go n ty1 <> sp <> go (n + 1) ty2
  go _ ty = do
    syntax "(" <> go 0 ty <> syntax ")"

typeToHtml :: P.Type -> H.Html
typeToHtml = asHtml . renderType

dePrim :: P.Type -> P.Type
dePrim ty@(P.TypeConstructor (P.Qualified _ name))
  | ty == P.tyBoolean || ty == P.tyNumber || ty == P.tyString =
    P.TypeConstructor $ P.Qualified Nothing name
dePrim other = other

convert :: P.Type -> P.Type
convert (P.TypeApp (P.TypeApp f arg) ret) | f == P.tyFunction = P.PrettyPrintFunction arg ret
convert (P.TypeApp a el) | a == P.tyArray = P.PrettyPrintArray el
convert (P.TypeApp o r) | o == P.tyObject = P.PrettyPrintObject r
convert other = other

convertForAlls :: P.Type -> P.Type
convertForAlls (P.ForAll i ty _) = go [i] ty
  where
  go idents (P.ForAll ident' ty' _) = go (ident' : idents) ty'
  go idents other = P.PrettyPrintForAll idents other
convertForAlls other = other

renderRow :: P.Type -> RenderedCode
renderRow = uncurry renderRow' . P.rowToList
  where
  renderRow' h t = renderHead h <> renderTail t

renderHead :: [(String, P.Type)] -> RenderedCode
renderHead = mintersperse (syntax "," <> sp) . map renderLabel

renderLabel :: (String, P.Type) -> RenderedCode
renderLabel (label, ty) =
  ident label <> sp <> syntax "::" <> sp <> renderType ty

renderTail :: P.Type -> RenderedCode
renderTail P.REmpty = mempty
renderTail other = sp <> syntax "|" <> sp <> renderType other

-- TODO: constructor links
asHtml :: RenderedCode -> H.Html
asHtml = outputWith elemAsHtml
  where
  elemAsHtml (Syntax x)  = withClass "syntax" (text x)
  elemAsHtml (Ident x)   = withClass "ident" (text x)
  elemAsHtml (Ctor x _)  = withClass "ctor" (text x)
  elemAsHtml (Kind x)    = text x
  elemAsHtml (Keyword x) = withClass "keyword" (text x)
  elemAsHtml Space       = text " "
