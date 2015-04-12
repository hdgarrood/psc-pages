{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | Functions for rendering documentation generated from PureScript code.

module PscPages.Render where

import Control.Monad
import Data.Either
import Data.Version (showVersion)
import Data.Monoid ((<>), mempty, Monoid)
import Data.Default (def)
import Data.String (fromString)
import Data.Maybe (mapMaybe)
import Data.List (nub)

import qualified Text.Blaze.Html as H

import qualified Language.PureScript as P

import PscPages.RenderedCode
import PscPages.Types

import qualified Cheapskate

mintersperse :: (Monoid m) => m -> [m] -> m
mintersperse _ []       = mempty
mintersperse _ [x]      = x
mintersperse sep (x:xs) = x <> sep <> mintersperse sep xs

collectBookmarks :: InPackage P.Module -> Bookmarks
collectBookmarks (Local m) = map Local (collectBookmarks' m)
collectBookmarks (FromDep pkg m) = map (FromDep pkg) (collectBookmarks' m)

collectBookmarks' :: P.Module -> [(P.ModuleName, String)]
collectBookmarks' m =
  map (P.getModuleName m, )
      (mapMaybe getDeclarationTitle
                (P.exportedDeclarations m))


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
  declarations = groupChildren declarationsWithChildren
  declarationsWithChildren = mapMaybe go (P.exportedDeclarations m)
  go decl = getDeclarationTitle decl
             >>= renderDeclaration exps decl

-- | An intermediate stage which we go through during rendering.
--
-- In the first pass, we take all top level declarations in the module, and
-- render those which should appear at the top level in the output, as well as
-- those which should appear as children of other declarations in the output.
--
-- In the second pass, we move all children under their respective parents,
-- or discard them if none are found.
--
-- This two-pass system is only necessary for type instance declarations, since
-- they appear at the top level in the AST, and since they might need to appear
-- as children in two places (for example, if a data type defined in a module
-- is an instance of a type class also defined in that module).
--
-- This data type is used as an intermediate type between the two stages. The
-- Left case is a child declaration, together with a list of parent declaration
-- titles which this may appear as a child of.
--
-- The Right case is a top level declaration which should pass straight through
-- the second stage; the only way it might change is if child declarations are
-- added to it.
type IntermediateDeclaration
  = Either ([String], RenderedChildDeclaration) RenderedDeclaration

-- | Move child declarations into their respective parents; the second pass.
-- See the comments under the type synonym IntermediateDeclaration for more
-- information.
groupChildren :: [IntermediateDeclaration] -> [RenderedDeclaration]
groupChildren (partitionEithers -> (children, toplevels)) =
  foldl go toplevels children
  where
  go ds (parentTitles, child) =
    map (\d ->
      if rdTitle d `elem` parentTitles
        then d { rdChildren = rdChildren d ++ [child] }
        else d) ds

basicDeclaration :: String -> RenderedCode -> Maybe IntermediateDeclaration
basicDeclaration title code = Just (Right (RenderedDeclaration title Nothing code Nothing []))

renderDeclaration :: Maybe [P.DeclarationRef] -> P.Declaration -> String -> Maybe IntermediateDeclaration
renderDeclaration _ (P.TypeDeclaration ident' ty) title =
  basicDeclaration title code
  where
  code = ident (show ident')
          <> sp <> syntax "::" <> sp
          <> renderType ty
renderDeclaration _ (P.ExternDeclaration _ ident' _ ty) title =
  basicDeclaration title code
  where
  code = ident (show ident')
          <> sp <> syntax "::" <> sp
          <> renderType ty
renderDeclaration exps (P.DataDeclaration dtype name args ctors) title =
  Just (Right (RenderedDeclaration title Nothing code Nothing children))
  where
  typeApp  = foldl P.TypeApp (P.TypeConstructor (P.Qualified Nothing name)) (map toTypeVar args)
  exported = filter (P.isDctorExported name exps . fst) ctors
  code = keyword (show dtype) <> sp <> renderType typeApp
  children = map renderCtor exported
  -- TODO: Comments for data constructors?
  renderCtor (ctor', tys) =
          let typeApp' = foldl P.TypeApp (P.TypeConstructor (P.Qualified Nothing ctor')) tys
              childCode = renderType typeApp'
          in  RenderedChildDeclaration (show ctor') Nothing childCode Nothing ChildDataConstructor
renderDeclaration _ (P.ExternDataDeclaration name kind') title =
  basicDeclaration title code
  where
  code = keywordData <> sp
          <> renderType (P.TypeConstructor (P.Qualified Nothing name))
          <> sp <> syntax "::" <> sp
          <> kind (P.prettyPrintKind kind') -- TODO: Proper renderKind function?
renderDeclaration _ (P.TypeSynonymDeclaration name args ty) title =
  basicDeclaration title code
  where
  typeApp = foldl P.TypeApp (P.TypeConstructor (P.Qualified Nothing name)) (map toTypeVar args)
  code = mintersperse sp
          [ keywordType
          , renderType typeApp
          , syntax "="
          , renderType ty
          ]
renderDeclaration _ (P.TypeClassDeclaration name args implies ds) title = do
  Just (Right (RenderedDeclaration title Nothing code Nothing children))
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

  classApp = foldl P.TypeApp (P.TypeConstructor (P.Qualified Nothing name)) (map toTypeVar args)

  children = map renderClassMember ds

  -- TODO: Comments for type class members
  renderClassMember (P.PositionedDeclaration _ _ d) = renderClassMember d
  renderClassMember (P.TypeDeclaration ident' ty) =
    let childCode =
          mintersperse sp
            [ ident (show ident')
            , syntax "::"
            , renderType ty
            ]
    in  RenderedChildDeclaration (show ident') Nothing childCode Nothing ChildTypeClassMember
  renderClassMember _ = error "Invalid argument to renderClassMember."
renderDeclaration _ (P.TypeInstanceDeclaration name constraints className tys _) title = do
  Just (Left (classNameString : typeNameStrings, childDecl))
  where
  classNameString = unQual className
  typeNameStrings = nub (concatMap (P.everythingOnTypes (++) extractProperNames) tys)
  unQual x = let (P.Qualified _ y) = x in show y

  extractProperNames (P.TypeConstructor n) = [unQual n]
  extractProperNames (P.SaturatedTypeSynonym n _) = [unQual n]
  extractProperNames _ = []

  childDecl = RenderedChildDeclaration title Nothing code Nothing ChildInstance

  code =
    mintersperse sp $
      [ keywordInstance
      , ident (show name)
      , syntax "::"
      ] ++ maybe [] (:[]) constraints'
        ++ [ renderType classApp ]

  constraints'
    | null constraints = Nothing
    | otherwise = Just (syntax "(" <> renderedConstraints <> syntax ") =>")

  renderedConstraints = mintersperse (syntax "," <> sp) (map renderConstraint constraints)

  renderConstraint (pn, tys') =
    let supApp = foldl P.TypeApp (P.TypeConstructor pn) tys'
    in renderType supApp

  classApp = foldl P.TypeApp (P.TypeConstructor className) tys
renderDeclaration exps (P.PositionedDeclaration srcSpan com d') title =
  fmap (addComments . addSourceSpan) (renderDeclaration exps d' title)
  where
  addComments (Left (t, d)) = Left (t, d { rcdComments = renderComments com })
  addComments (Right d) = Right (d { rdComments = renderComments com })

  addSourceSpan (Left (t, d)) = Left (t, d { rcdSourceSpan = Just srcSpan })
  addSourceSpan (Right d) = Right (d { rdSourceSpan = Just srcSpan })
renderDeclaration _ _ _ = Nothing

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
    ctor (show name) (toContainingModule mn)
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
