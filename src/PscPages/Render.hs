{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PscPages.Render where

-- | Functions and data types for rendering generated documentation for
-- | PureScript code. 

import Control.Applicative
import Control.Monad
import Data.Version (showVersion)
import Data.Monoid ((<>), mempty, Monoid)
import Data.Default (def)
import Data.String (fromString)
import Data.Maybe (mapMaybe)

import qualified Text.Blaze.Html as H

import qualified Language.PureScript as P

import PscPages.RenderedCode
import PscPages.PackageMeta

import qualified Cheapskate

mintersperse :: (Monoid m) => m -> [m] -> m
mintersperse _ []       = mempty
mintersperse _ [x]      = x
mintersperse sep (x:xs) = x <> sep <> mintersperse sep xs

type Bookmarks = [(P.ModuleName, String)]

collectBookmarks :: P.Module -> Bookmarks
collectBookmarks (P.Module _ moduleName ds _) =
  map (moduleName, ) $ mapMaybe getDeclarationTitle ds

getDeclarationTitle :: P.Declaration -> Maybe String
getDeclarationTitle (P.TypeDeclaration name _)               = Just (show name)
getDeclarationTitle (P.ExternDeclaration _ name _ _)         = Just (show name)
getDeclarationTitle (P.DataDeclaration _ name _ _)           = Just (show name)
getDeclarationTitle (P.ExternDataDeclaration name _)         = Just (show name)
getDeclarationTitle (P.TypeSynonymDeclaration name _ _)      = Just (show name)
getDeclarationTitle (P.TypeClassDeclaration name _ _ _)      = Just (show name)
getDeclarationTitle (P.TypeInstanceDeclaration name _ _ _ _) = Just (show name)
getDeclarationTitle (P.PositionedDeclaration _ _ d)          = getDeclarationTitle d
getDeclarationTitle _                                        = Nothing

data RenderedPackage = RenderedPackage
  { rpName    :: String
  , rpVersion :: String
  , rpModules :: [RenderedModule]
  }

data RenderedModule = RenderedModule
  { rmName         :: String
  , rmComments     :: Maybe H.Html
  , rmDeclarations :: [(String, RenderedDeclaration)]
  }

data RenderedDeclaration = RenderedDeclaration
  { rdComments   :: Maybe H.Html
  , rdCode       :: RenderedCode
  , rdChildren   :: [RenderedCode]
  , rdSourceSpan :: Maybe P.SourceSpan
  }

renderPackage :: PackageMeta -> [P.Module] -> RenderedPackage
renderPackage pkgMeta mods =
  RenderedPackage name vers (map renderModule mods)
  where
  name = runPackageName (pkgMetaName pkgMeta)
  vers = showVersion (pkgMetaVersion pkgMeta)

renderModule :: P.Module -> RenderedModule
renderModule m@(P.Module coms moduleName  _ exps) =
  RenderedModule (show moduleName) comments declarations
  where
  comments = renderComments coms
  declarations = mapMaybe go (P.exportedDeclarations m)
  go decl = (,) <$> getDeclarationTitle decl <*> renderDeclaration exps decl

basicDeclaration :: RenderedCode -> Maybe RenderedDeclaration
basicDeclaration code = Just (RenderedDeclaration Nothing code [] Nothing)

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
  Just (RenderedDeclaration Nothing code children Nothing)
  where
  typeApp  = foldl P.TypeApp (P.TypeConstructor (P.Qualified Nothing name)) (map toTypeVar args)
  exported = filter (P.isDctorExported name exps . fst) ctors
  code = keyword (show dtype) <> sp <> renderType typeApp
  children = map renderCtor exported
  renderCtor (ctor', tys) =
          let typeApp' = foldl P.TypeApp (P.TypeConstructor (P.Qualified Nothing ctor')) tys
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
  Just (RenderedDeclaration Nothing code children Nothing)
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
renderDeclaration exps (P.PositionedDeclaration srcSpan com d) =
  fmap (addComments . addSourceSpan) (renderDeclaration exps d)
  where
  addComments rd   = rd { rdComments   = renderComments com }
  addSourceSpan rd = rd { rdSourceSpan = Just srcSpan }
renderDeclaration _ _ = Nothing

renderComments :: [P.Comment] -> Maybe H.Html
renderComments cs = do
  let raw = concatMap toLines cs
  guard (all hasPipe raw && not (null raw))
  return (go raw)
  where
  go = H.toHtml
       . Cheapskate.markdown def
       . fromString
       . unlines
       . map stripPipes

  toLines (P.LineComment s) = [s]
  toLines (P.BlockComment s) = lines s

  hasPipe s = case dropWhile (== ' ') s of { ('|':_) -> True; _ -> False }

  stripPipes = dropPipe . dropWhile (== ' ')

  dropPipe ('|':' ':s) = s
  dropPipe ('|':s) = s
  dropPipe s = s

toTypeVar :: (String, Maybe P.Kind) -> P.Type
toTypeVar (s, Nothing) = P.TypeVar s
toTypeVar (s, Just k) = P.KindedType (P.TypeVar s) k

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
  go _ (P.TypeConstructor (P.Qualified mn name)) =
    ctor (show name) mn
  go n (P.ConstrainedType deps ty) =
    syntax "(" <> constraints <> syntax ")" <> sp
      <> syntax "=>" <> sp <> go n ty
    where
    constraints = mintersperse (syntax "," <> sp) (map renderDep deps)
    renderDep (pn, tys) =
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
