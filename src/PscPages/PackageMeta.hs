{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- | Data types and functions for obtaining and operating on package metadata
-- descriptions, from bower.json for example.

-- TODO Check that the correct git tag is checked out, by shelling out to git?

module PscPages.PackageMeta where

import Prelude hiding (userError)

import Data.Maybe
import Data.String (fromString)
import Data.List (stripPrefix, isSuffixOf)
import Data.List.Split (splitOn)
import Data.Version
import Text.ParserCombinators.ReadP (readP_to_S)
import qualified Data.Map as M

import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Aeson as A
import Data.Aeson.Lens (key, _String)

import qualified Text.PrettyPrint.Boxes as Boxes

import Control.Applicative
import Control.Category ((>>>))
import Control.Lens ((^?), to)
import Control.Arrow ((***))
import Control.Exception (catch, IOException)
import Control.Monad.Except
import Control.Monad.Writer

import System.Directory (doesFileExist)
import System.Process (readProcess)
import System.Exit (exitFailure)
import qualified System.FilePath.Glob as Glob
import Network.Wreq (get, responseBody)
import Network.HTTP.Client (HttpException(StatusCodeException))
import Network.HTTP.Types (notFound404)

import PscPages.Types
import qualified PscPages.Utils.TracedJson as TJ

packageIdentifier :: PackageMeta -> String
packageIdentifier pkgMeta =
  concat [name, "-", version]
  where
  name = runPackageName (pkgMetaName pkgMeta)
  version = showVersion (pkgMetaVersion pkgMeta)

outputBaseDirectory :: PackageMeta -> FilePath
outputBaseDirectory pkgMeta =
  concat [ name, "/", version ]
  where
  name = runPackageName (pkgMetaName pkgMeta)
  version = showVersion (pkgMetaVersion pkgMeta)

-- | Attempt to retrieve package metadata from the current directory.
-- Calls exitFailure if no package metadata could be retrieved.
getPackageMeta :: IO PackageMeta
getPackageMeta =
  runGetMetaM getPackageMeta'
    >>= either (\e -> printError e >> exitFailure)
               handleWarnings
  where
  printError err = do
    Boxes.printBox $ case err of
      UserError e ->
        vcat
          [ para (concat
            [ "There is a problem with your package, which meant that "
            , "documentation could not be generated."
            ])
          , para "Details:"
          , indented (displayUserError e)
          ]
      InternalError e -> do
        vcat
          [ para "Internal error: this is probably a bug. Please report it:"
          , indented (para "https://github.com/purescript/pursuit/issues/new")
          , successivelyIndented (displayInternalError e)
          ]
      OtherError e -> do
        vcat
          [ para "An error occurred."
          , para "Details:"
          , indented (para (displayOtherError e))
          ]

  handleWarnings (x, warns) = do
    let names = map getPackageName warns
    case length warns of
      0 -> return ()
      1 -> pluraliseMsg "package" "was" "this" "this" names
      _ -> pluraliseMsg "packages" "were" "any of these" "these" names
    return x

  getPackageName :: PackageWarning -> String
  getPackageName (ResolutionNotVersion (PackageName n)) = n

  pluraliseMsg packages were anyOfThese these names =
    Boxes.printBox $ vcat $
      [ para (concat
        ["The following ", packages, " ", were, " not resolved to a version:"])
      ] ++ (map (indented . para) names) ++
      [ para (concat
        ["Links to types in ", anyOfThese, " ", packages, " will not work. In "
        , "order to make links work, edit your bower.json to specify a version"
        , " or a version range for ", these, " ", packages, ", and rerun "
        , "`bower install`."
        ])
      ]

para :: String -> Boxes.Box
para = Boxes.para Boxes.left 79

indented :: Boxes.Box -> Boxes.Box
indented b = Boxes.hcat Boxes.left [Boxes.emptyBox 1 2, b]

successivelyIndented :: [String] -> Boxes.Box
successivelyIndented [] =
  Boxes.nullBox
successivelyIndented (x:xs) =
  Boxes.vcat Boxes.left [para x, indented (successivelyIndented xs)]

vcat :: [Boxes.Box] -> Boxes.Box
vcat = Boxes.vcat Boxes.left

-- | An error which meant that it was not possible to retrieve metadata for a
-- package.
data PackageError
  = UserError UserError
  | InternalError InternalError
  | OtherError OtherError
  deriving (Show)

-- | An error that should be fixed by the user.
data UserError
  = BowerJsonNotFound
  | CouldntParseBowerJson String
  | BowerJsonNameMissing
  | BowerJsonVersionMissing
  | BowerJsonInvalidVersion
  | NotOnGithub
  | NotOnBower PackageName
  deriving (Show)

displayUserError :: UserError -> Boxes.Box
displayUserError e = case e of
  BowerJsonNotFound ->
    para (concat
      [ "The bower.json file was not found. Please create one, or run "
      , "`pulp init`."
      ])
  CouldntParseBowerJson err ->
    vcat
      [ successivelyIndented
        ["The bower.json file could not be parsed as JSON:", "aeson reported: " ++ show err]
      , para "Please ensure that your bower.json file is valid JSON."
      ]
  BowerJsonNameMissing ->
    vcat
      [ successivelyIndented ["In bower.json:", "the \"name\" key was not found."]
      , para "Please give your package a name first."
      ]
  BowerJsonVersionMissing ->
    vcat
      [ successivelyIndented ["In bower.json:", "the \"version\" key was not found."]
      , para "Please add the current version to your bower.json file."
      ]
  BowerJsonInvalidVersion ->
    vcat
      [ successivelyIndented ["In bower.json:", "the \"version\" key could not be parsed."]
      , para (concat
        [ "Please ensure the version in bower.json is formatted correctly. "
        , "The expected format is MAJOR.MINOR.PATCH, for example, \"1.5.3\"."
        ])
      ]
  NotOnGithub ->
    para (concat
      [ "The entry for this package in the Bower registry does not point to a "
      , "GitHub repository. Currently, pursuit does not support packages which "
      , "are not hosted on GitHub. If you would prefer not to host your "
      , "package on GitHub, please open an issue: "
      , "https://github.com/purescript/pursuit/issues/new"
      ])
  NotOnBower (PackageName name) ->
    para (concat
      [ "Your package (" ++ name ++ ") does not yet appear to be on the Bower "
      , "registry. Please add it before continuing."
      ])

-- | An error that probably indicates a bug in this module.
data InternalError
  = MalformedJson JsonSource TJ.Error
  deriving (Show)

displayInternalError :: InternalError -> [String]
displayInternalError e = case e of
  MalformedJson src r ->
    ["Malformed json " ++ displayJsonSource src ++ ":"] ++ TJ.displayError r

data JsonSource
  = FromFile FilePath
  | FromBowerListPaths
  | FromBowerApi
  deriving (Show)

displayJsonSource :: JsonSource -> String
displayJsonSource s = case s of
  FromFile fp ->
    "in file " ++ show fp
  FromBowerListPaths ->
    "in the output of `bower list --paths`"
  FromBowerApi ->
    "from the Bower registry API"

data OtherError
  = HttpExceptionThrown HttpException
  | ProcessFailed String [String] IOException
  deriving (Show)

displayOtherError :: OtherError -> String
displayOtherError = undefined

data PackageWarning
  = ResolutionNotVersion PackageName
  deriving (Show)


type GetMetaM = WriterT [PackageWarning] (ExceptT PackageError IO)

runGetMetaM :: GetMetaM a -> IO (Either PackageError (a, [PackageWarning]))
runGetMetaM = runExceptT . runWriterT

warn :: PackageWarning -> GetMetaM ()
warn w = tell [w]

userError :: UserError -> GetMetaM a
userError = throwError . UserError

internalError :: InternalError -> GetMetaM a
internalError = throwError . InternalError

otherError :: OtherError -> GetMetaM a
otherError = throwError . OtherError

catchLeft :: Applicative f => Either a b -> (a -> f b) -> f b
catchLeft a f = either f pure a

getPackageMeta' :: GetMetaM PackageMeta
getPackageMeta' = do
  exists <- liftIO (doesFileExist "bower.json")
  unless exists (userError BowerJsonNotFound)

  bowerJson <- liftIO (parseJsonFile "bower.json")
                  >>= flip catchLeft
                        (userError . CouldntParseBowerJson)

  pkgMetaName <- PackageName <$> takeKeyOr BowerJsonNameMissing bowerJson "name"
  versionStr <- takeKeyOr BowerJsonVersionMissing bowerJson "version"
  pkgMetaVersion <- maybe (userError BowerJsonInvalidVersion)
                          return
                          (parseVersion' versionStr)

  pkgMetaGithub <- getBowerInfo pkgMetaName
  pkgMetaDependencies <- M.fromList <$> getBowerDepsVersions

  return PackageMeta{..}

  where
  takeKeyOr e json name =
    let mValue = json ^? key name . _String . to T.unpack
    in  maybe (userError e) return mValue

parseJsonFile :: FilePath -> IO (Either String A.Value)
parseJsonFile fp = do
  jsonText <- liftIO (B.readFile fp)
  return (A.eitherDecodeStrict jsonText)

(<#>) :: Functor f => f a -> (a -> b) -> f b
(<#>) = flip fmap

infixl 1 <#>

getBowerInfo :: PackageName -> GetMetaM (GithubUser, GithubRepo)
getBowerInfo pkgName = do
  r' <- liftIO (catch (Right <$> get (bowerPackageUrl pkgName))
                      (return . Left))
  r <- either handleError return r'

  case getUrl r of
    Right url ->
      maybe (userError NotOnGithub) return (extractGithub url)
    Left err ->
      internalError (MalformedJson FromBowerApi err)

  where
  handleError (StatusCodeException status _ _)
    | status == notFound404 = userError (NotOnBower pkgName)
  handleError e = otherError (HttpExceptionThrown e)

  getUrl r =
    TJ.parse (fromMaybe "" (r ^? responseBody))
      >>= TJ.takeKey "url"
      >>= TJ.asString
      <#> T.unpack

bowerDomainName :: String
bowerDomainName = "bower.herokuapp.com"

bowerPackageUrl :: PackageName -> String
bowerPackageUrl (PackageName name) =
  concat [ "https://"
         , bowerDomainName
         , "/packages/"
         , name
         ]

extractGithub :: String -> Maybe (GithubUser, GithubRepo)
extractGithub =
  stripPrefix "git://github.com/"
   >>> fmap (splitOn "/")
   >=> takeTwo
   >>> fmap (GithubUser *** (GithubRepo . dropDotGit))

  where
  takeTwo :: [a] -> Maybe (a, a)
  takeTwo [x, y] = Just (x, y)
  takeTwo _ = Nothing

  dropDotGit :: String -> String
  dropDotGit str
    | ".git" `isSuffixOf` str = take (length str - 4) str
    | otherwise = str

parseVersion' :: String -> Maybe Version
parseVersion' str =
  case filter (null . snd) $ readP_to_S parseVersion str of
    [(vers, "")] -> Just vers
    _            -> Nothing

readProcess' :: String -> [String] -> String -> GetMetaM String
readProcess' prog args stdin = do
  out <- liftIO (catch (Right <$> readProcess prog args stdin)
                       (return . Left))
  either (otherError . ProcessFailed prog args) return out

-- Go through all bower dependencies which contain purescript code, and
-- extract their versions.
--
-- In the case where a bower dependency is taken from a particular version,
-- that's easy; take that version. In any other case (eg, a branch, or a commit
-- sha) we print a warning that documentation links will not work, and avoid
-- linking to documentation for any types from that package.
--
-- The rationale for this is: people will prefer to use a released version
-- where possible. If they are not using a released version, then this is
-- probably for a reason. However, docs are only ever available for released
-- versions. Therefore there will probably be no version of the docs which is
-- appropriate to link to, and we should omit links.
getBowerDepsVersions :: GetMetaM [(PackageName, Version)]
getBowerDepsVersions = do
  pathsBS <- fromString <$> readProcess' "bower" ["list", "--paths", "--json"] ""

  paths <- catchLeft (TJ.parse pathsBS >>= TJ.asObjectOfString)
                     (internalError . MalformedJson FromBowerListPaths)

  catMaybes <$> mapM getVersion paths

  where
  getVersion :: (Text, Text) -> GetMetaM (Maybe (PackageName, Version))
  getVersion (pkgNameText, pathText) = do
    let path = T.unpack pathText
    let jsonPath = path ++ "/.bower.json"
    let pkgName = PackageName (T.unpack pkgNameText)

    let malformedJson = internalError . MalformedJson (FromFile jsonPath)

    isPs <- liftIO (isPureScript path)
    if not isPs
      then return Nothing
      else do
        jsonBS <- liftIO (B.readFile jsonPath)

        (typ, tag) <- catchLeft (getTypeAndTag jsonBS)
                                malformedJson
        case typ of
          "version" -> do
            let tag' = T.unpack (fromMaybe tag (T.stripPrefix "v" tag))
            return ((pkgName,) <$> parseVersion' tag')
          _ -> do
            warn (ResolutionNotVersion pkgName)
            return Nothing

  -- | Returns whether it looks like there is a purescript package checked out
  -- in the given directory.
  isPureScript :: FilePath -> IO Bool
  isPureScript dir = do
    files <- liftIO (Glob.globDir1 psSourceFiles dir)
    return (not (null files))

  psSourceFiles :: Glob.Pattern
  psSourceFiles = Glob.compile "src/**/*.purs"

  -- | Get the resolution type and tag from the JSON
  getTypeAndTag jsonBS = do
    res <- TJ.parseStrict jsonBS >>= TJ.takeKey "_resolution"
    typ <- TJ.takeKey "type" res >>= TJ.asString
    tag <- TJ.takeKey "tag" res >>= TJ.asString
    return (typ, tag)
