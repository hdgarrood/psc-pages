{-# LANGUAGE OverloadedStrings, TemplateHaskell, TupleSections #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Control.Arrow (first)
import Data.Ord (comparing)
import Data.Char (toUpper)
import Data.Default (def)
import Data.List (nub, sortBy, intercalate)
import Data.List.Split (splitOn)
import Data.String (fromString)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Version (showVersion)
import Data.Foldable (for_)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

import qualified Data.Text.Lazy.IO as TL

import Options.Applicative

import qualified Language.PureScript as P
import qualified Language.PureScript.Constants as C
import qualified Paths_psc_pages as Paths

import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStrLn, stderr)
import System.FilePath ((</>), takeDirectory)
import System.Directory (createDirectoryIfMissing)

import Text.Blaze.Html ((!))
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html.Renderer.Text as H
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.FileEmbed

import qualified Cheapskate

app :: ([FilePath], FilePath) -> IO ()
app (input, outputDir) = do
  files <- mapM (fmap (first Just) . parseFile) (nub input)
  let e = P.parseModulesFromFiles (fromMaybe "") ((Nothing, P.prelude) : files)
  case e of
    Left err -> do
      hPutStrLn stderr $ show err
      exitFailure
    Right ms ->
      case P.sortModules . map (importPrim . importPrelude . snd) $ ms of
        Left e -> do
          hPutStrLn stderr e
          exitFailure
        Right (ms', _) ->
          case desugar ms' of
            Left err -> do
              hPutStrLn stderr $ P.prettyPrintErrorStack False err
              exitFailure
            Right modules -> do
              let bookmarks = concatMap collectBookmarks modules    
                
              let stylesheetFile = outputDir </> "style.css"
              mkdirp stylesheetFile
              T.writeFile stylesheetFile stylesheet
              
              let contentsFile = outputDir </> "index.html"
              TL.writeFile contentsFile (H.renderHtml $ contentsPageHtml modules)
              
              let indexFile = outputDir </> "index/index.html"
              mkdirp indexFile
              TL.writeFile indexFile (H.renderHtml indexPageHtml)
              
              for_ ['a'..'z'] $ \c -> do
                let letterFile = outputDir </> ("index/" ++ c : ".html")
                TL.writeFile letterFile (H.renderHtml $ letterPageHtml c bookmarks)
              
              for_ modules (renderModule outputDir bookmarks)
              exitSuccess
  where
  parseFile :: FilePath -> IO (FilePath, String)
  parseFile input = (,) input <$> readFile input
  
  addDefaultImport :: P.ModuleName -> P.Module -> P.Module
  addDefaultImport toImport m@(P.Module mn decls exps)  =
    if isExistingImport `any` decls || mn == toImport then m
    else P.Module mn (P.ImportDeclaration toImport P.Unqualified Nothing : decls) exps
    where
    isExistingImport (P.ImportDeclaration mn' _ _) | mn' == toImport = True
    isExistingImport (P.PositionedDeclaration _ _ d) = isExistingImport d
    isExistingImport _ = False

  importPrim :: P.Module -> P.Module
  importPrim = addDefaultImport (P.ModuleName [P.ProperName C.prim])

  importPrelude :: P.Module -> P.Module
  importPrelude = addDefaultImport (P.ModuleName [P.ProperName C.prelude])
  
  desugar :: [P.Module] -> Either P.ErrorStack [P.Module]
  desugar = P.evalSupplyT 0 . desugar'
    where
    desugar' :: [P.Module] -> P.SupplyT (Either P.ErrorStack) [P.Module]
    desugar' = mapM P.desugarDoModule >=> P.desugarCasesModule >=> lift . P.desugarImports

collectBookmarks :: P.Module -> [(P.ModuleName, String)]
collectBookmarks (P.Module moduleName ds _) = map (moduleName, ) $ mapMaybe getDeclarationTitle ds  

getDeclarationTitle :: P.Declaration -> Maybe String
getDeclarationTitle (P.TypeDeclaration name _)                      = Just (show name)
getDeclarationTitle (P.ExternDeclaration _ name _ _)                = Just (show name)
getDeclarationTitle (P.DataDeclaration _ name _ _)                  = Just (show name)
getDeclarationTitle (P.ExternDataDeclaration name _)                = Just (show name)
getDeclarationTitle (P.TypeSynonymDeclaration name _ _)             = Just (show name)
getDeclarationTitle (P.TypeClassDeclaration name _ _ _)   = Just (show name)
getDeclarationTitle (P.TypeInstanceDeclaration name _ _ _ _)        = Just (show name)
getDeclarationTitle (P.PositionedDeclaration _ _ d)                 = getDeclarationTitle d
getDeclarationTitle _                                               = Nothing
    
renderModule :: FilePath -> [(P.ModuleName, String)] -> P.Module -> IO ()
renderModule outputDir bookmarks m@(P.Module moduleName _ exps) = do
  let filename = outputDir </> filePathFor moduleName
      html = H.renderHtml $ moduleToHtml bookmarks m
  mkdirp filename
  TL.writeFile filename html

mkdirp :: FilePath -> IO ()
mkdirp = createDirectoryIfMissing True . takeDirectory

relativeTo :: FilePath -> FilePath -> FilePath
relativeTo to from = go (splitOn "/" to) (splitOn "/" from)
  where
  go (x : xs) (y : ys) | x == y = go xs ys
  go xs ys = intercalate "/" $ replicate (length ys - 1) ".." ++ xs

stylesheet :: T.Text
stylesheet = T.decodeUtf8 $(embedFile "static/style.css")

filePathFor :: P.ModuleName -> FilePath
filePathFor (P.ModuleName parts) = go parts
  where
  go [] = "index.html"
  go (x : xs) = show x </> go xs

text :: String -> H.Html
text = H.toHtml

sp :: H.Html
sp = text " "

withClass :: H.AttributeValue -> H.Html -> H.Html
withClass className content = H.span ! A.class_ className $ content

para :: H.AttributeValue -> H.Html -> H.Html
para className content = H.p ! A.class_ className $ content

intercalateA_ :: (Applicative m) => m b -> [m a] -> m ()
intercalateA_ _   []     = pure ()
intercalateA_ _   [x]    = void x
intercalateA_ sep (x:xs) = (x <* sep) *> intercalateA_ sep xs

template :: FilePath -> String -> H.Html -> H.Html
template curFile title body = do
  H.docType
  H.html $ do
    H.head $ do
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href (fromString ("style.css" `relativeTo` curFile))
      H.title $ H.toHtml title
    H.body $ do
      H.div ! A.id "header" $ do
        H.h1 $ text title
      body
      H.div ! A.id "footer" $ 
        H.small $ do
          text $ "Generated by psc-pages " ++ showVersion Paths.version ++ " | "
          H.a ! A.href (fromString ("index.html" `relativeTo` curFile)) $ text "Contents"
          text " | "
          H.a ! A.href (fromString ("index/index.html" `relativeTo` curFile)) $ text "Index"

contentsPageHtml :: [P.Module] -> H.Html
contentsPageHtml ms = do
  template "index.html" "Contents" $ do
    H.h2 $ text "Modules"
    H.ul $ for_ (sortBy (comparing $ \(P.Module moduleName _ _) -> moduleName) ms) $ \(P.Module moduleName _ _) -> H.li $
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
  
moduleToHtml :: [(P.ModuleName, String)] -> P.Module -> H.Html
moduleToHtml bookmarks (P.Module moduleName ds exps) = 
  template (filePathFor moduleName) (show moduleName) $ for_ (filter (P.isExported exps) ds) (declToHtml exps)
  where
  declToHtml :: Maybe [P.DeclarationRef] -> P.Declaration -> H.Html
  declToHtml exps decl = do
    for_ (getDeclarationTitle decl) $ \s -> 
      H.a ! A.name (fromString s) $ mempty
    renderDeclaration exps decl
    where
  
  renderDeclaration :: Maybe [P.DeclarationRef] -> P.Declaration -> H.Html
  renderDeclaration _ (P.TypeDeclaration ident ty) =
    para "decl" $ H.code $ do
      withClass "ident" . text . show $ ident
      sp *> withClass "syntax" (text "::") <* sp
      typeToHtml ty
  renderDeclaration _ (P.ExternDeclaration _ ident _ ty) =
    para "decl" $ H.code $ do
      withClass "ident" . text . show $ ident
      sp *> withClass "syntax" (text "::") <* sp
      typeToHtml ty
  renderDeclaration exps (P.DataDeclaration dtype name args ctors) = do
    let typeApp  = foldl P.TypeApp (P.TypeConstructor (P.Qualified Nothing name)) (map toTypeVar args)
        exported = filter (P.isDctorExported name exps . fst) ctors
    para "decl" $ H.code $ do
      withClass "keyword" . text $ show dtype  
      sp
      typeToHtml typeApp
    unless (null exported) $ do
      H.h3 $ text "Constructors"
      H.ul $ for_ exported $ \(ctor, tys) -> H.li . H.code $ do
        let typeApp = foldl P.TypeApp (P.TypeConstructor (P.Qualified Nothing ctor)) tys
        typeToHtml typeApp
  renderDeclaration _ (P.ExternDataDeclaration name kind) = do
    para "decl" $ H.code $ do
      withClass "keyword" . text $ "data"  
      sp
      typeToHtml $ P.TypeConstructor (P.Qualified Nothing name)
      sp *> withClass "syntax" (text "::") <* sp
      text $ P.prettyPrintKind kind
  renderDeclaration _ (P.TypeSynonymDeclaration name args ty) = do
    let typeApp  = foldl P.TypeApp (P.TypeConstructor (P.Qualified Nothing name)) (map toTypeVar args)
    para "decl" $ H.code $ do
      withClass "keyword" . text $ "type" 
      sp
      typeToHtml typeApp
      sp *> withClass "syntax" (text "=") <* sp
      typeToHtml ty
  renderDeclaration _ (P.TypeClassDeclaration name args implies ds) = do
    para "decl" $ H.code $ do
      withClass "keyword" (text "class") <* sp
      case implies of
        [] -> ""
        _ -> do withClass "syntax" $ text "("
                intercalateA_ (withClass "syntax" "," <* sp) $ flip map implies $ \(pn, tys') -> do
                  let supApp = foldl P.TypeApp (P.TypeConstructor pn) tys'
                  typeToHtml supApp
                withClass "syntax" $ text ") <= "
      let classApp  = foldl P.TypeApp (P.TypeConstructor (P.Qualified Nothing name)) (map toTypeVar args)
      typeToHtml classApp
    unless (null ds) $ do
      H.h3 $ text "Type Class Members"
      H.div ! A.class_ "typeclass" $ mapM_ renderClassMember ds
    where
    renderClassMember :: P.Declaration -> H.Html
    renderClassMember (P.PositionedDeclaration _ _ d) = renderClassMember d
    renderClassMember (P.TypeDeclaration ident ty) =
      para "decl" $ H.code $ do
        withClass "ident" . text . show $ ident
        sp *> withClass "syntax" (text "::") <* sp
        typeToHtml ty
    renderClassMember _ = error "Invalid argument to renderClassMember."
  renderDeclaration _ (P.TypeInstanceDeclaration name constraints className tys _) = do
    para "decl" $ H.code $ do
      withClass "keyword" (text "instance") <* sp
      withClass "ident" (text (show name)) <* sp
      withClass "syntax" (text "::") <* sp
      case constraints of
        [] -> ""
        _ -> do withClass "syntax" $ text "("
                intercalateA_ (withClass "syntax" "," <* sp) $ flip map constraints $ \(pn, tys') -> do
                  let supApp = foldl P.TypeApp (P.TypeConstructor pn) tys'
                  typeToHtml supApp
                withClass "syntax" $ text ") => "
      let classApp = foldl P.TypeApp (P.TypeConstructor className) tys
      typeToHtml classApp
  renderDeclaration exps (P.PositionedDeclaration _ com d) = do
    renderDeclaration exps d
    renderComments com
  renderDeclaration _ _ = return ()
  
  renderComments :: [P.Comment] -> H.Html
  renderComments cs = do
    let raw = concatMap toLines cs
    
    if all hasPipe raw
      then H.toHtml . Cheapskate.markdown def . fromString . unlines . map stripPipes $ raw
      else H.pre . H.code . H.toHtml $ unlines raw
    where
  
    toLines (P.LineComment s) = [s]
    toLines (P.BlockComment s) = lines s
    
    hasPipe s = case dropWhile (== ' ') s of { ('|':_) -> True; _ -> False }
    
    stripPipes = dropPipe . dropWhile (== ' ')
  
    dropPipe ('|':' ':s) = s
    dropPipe ('|':s) = s
    dropPipe s = s
    
  linkToConstructor :: P.Qualified P.ProperName -> H.Html -> H.Html
  linkToConstructor (P.Qualified mn ctor) contents | (fromMaybe moduleName mn, show ctor) `notElem` bookmarks = contents
  linkToConstructor (P.Qualified Nothing ctor) contents = H.a ! A.href (fromString ('#' : show ctor)) $ contents
  linkToConstructor (P.Qualified (Just destModule) ctor) contents = H.a ! A.href (fromString (uri ++ "#" ++ show ctor)) $ contents
    where
    uri = filePathFor destModule `relativeTo` filePathFor moduleName
  
  toTypeVar :: (String, Maybe P.Kind) -> P.Type
  toTypeVar (s, Nothing) = P.TypeVar s
  toTypeVar (s, Just k) = P.KindedType (P.TypeVar s) k
  
  typeToHtml :: P.Type -> H.Html
  typeToHtml = go 0 . P.everywhereOnTypes dePrim . P.everywhereOnTypesTopDown convertForAlls . P.everywhereOnTypes convert
    where
    go :: Int -> P.Type -> H.Html
    go _ P.TypeWildcard = withClass "syntax" (text "_")
    go _ (P.TypeVar var) = withClass "ident" (text var)
    go _ (P.PrettyPrintObject row) = do
      withClass "syntax" (text "{") <* sp
      rowToHtml row
      sp *> withClass "syntax" (text "}")
    go _ (P.PrettyPrintArray ty) = do
      withClass "syntax" (text "[")
      go 0 ty
      withClass "syntax" (text "]")
    go _ (P.TypeConstructor ctor@(P.Qualified _ name)) = linkToConstructor ctor $ withClass "ctor" (text (show name))
    go n (P.ConstrainedType deps ty) = do
      withClass "syntax" (text "(")
      intercalateA_ (withClass "syntax" (text ",") <* sp) $ flip map deps $ \(pn, tys) -> do
        let instApp = foldl P.TypeApp (P.TypeConstructor pn) tys
        go 0 instApp
      withClass "syntax" (text ")") *> sp
      withClass "syntax" (text "=>") *> sp
      go n ty
    go _ P.REmpty = withClass "syntax" (text "()")
    go _ row@P.RCons{} = do
      withClass "syntax" (text "(")
      rowToHtml row
      withClass "syntax" (text ")")
    go n (P.PrettyPrintFunction arg ret) | n < 1 = do
      go (n + 1) arg *> sp
      withClass "syntax" (text "->") *> sp
      go 0 ret
    go n (P.PrettyPrintForAll idents ty) | n < 1 = do
      withClass "keyword" (text "forall") *> sp
      intercalateA_ sp $ flip map idents $ withClass "ident" . text
      withClass "syntax" (text ".") *> sp
      go 0 ty
    go n (P.TypeApp ty1 ty2) | n < 1 = do
      go n ty1
      sp
      go (n + 1) ty2
    go _ ty = do
      withClass "syntax" (text "(")
      go 0 ty
      withClass "syntax" (text ")")
  
    dePrim ty@(P.TypeConstructor (P.Qualified _ name))
      | ty == P.tyBoolean || ty == P.tyNumber || ty == P.tyString =
        P.TypeConstructor $ P.Qualified Nothing name
    dePrim other = other
    
    convert (P.TypeApp (P.TypeApp f arg) ret) | f == P.tyFunction = P.PrettyPrintFunction arg ret
    convert (P.TypeApp a el) | a == P.tyArray = P.PrettyPrintArray el
    convert (P.TypeApp o r) | o == P.tyObject = P.PrettyPrintObject r
    convert other = other
    
    convertForAlls (P.ForAll ident ty _) = go [ident] ty
      where
      go idents (P.ForAll ident' ty' _) = go (ident' : idents) ty'
      go idents other = P.PrettyPrintForAll idents other
    convertForAlls other = other
  
  rowToHtml :: P.Type -> H.Html
  rowToHtml = uncurry rowToHtml' . P.rowToList
    where
    rowToHtml' h t = do
      headToHtml h
      tailToHtml t    
        
    headToHtml = intercalateA_ (withClass "syntax" (text ",") <* sp) . map labelToHtml    
        
    labelToHtml (label, ty) = do
      withClass "ident" (text label) <* sp
      withClass "syntax" (text "::") <* sp
      typeToHtml ty
        
    tailToHtml P.REmpty = return ()
    tailToHtml other = do
      sp *> withClass "syntax" (text "|") <* sp
      typeToHtml other

inputFiles :: Parser [FilePath]
inputFiles = many . strArgument $
     metavar "FILE"
  <> help "The input .purs file(s)"
  
outputDirectory :: Parser FilePath
outputDirectory = strOption $
     short 'o'
  <> long "output"
  <> help "The output .js file"

main :: IO ()
main = execParser opts >>= app
  where
  opts        = info (helper <*> ((,) <$> inputFiles <*> outputDirectory)) infoModList
  infoModList = fullDesc <> headerInfo <> footerInfo
  headerInfo  = header   "psc-pages - Generate HTML documentation from PureScript source files"
  footerInfo  = footer $ "psc-pages " ++ showVersion Paths.version
