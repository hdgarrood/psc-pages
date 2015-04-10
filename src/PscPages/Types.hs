module PscPages.Types
  ( module PscPages.Types
  , module ReExports
  )
  where

import Data.Maybe
import Data.Version
import qualified Language.PureScript as P
import qualified Text.Blaze.Html as H
import qualified Data.Map as M

import PscPages.RenderedCode as ReExports (RenderedCode, ContainingModule(..))

data InPackage a
  = Local a
  | FromDep PackageName a
  deriving (Show)

takeLocal :: InPackage a -> Maybe a
takeLocal (Local a) = Just a
takeLocal _ = Nothing

takeLocals :: [InPackage a] -> [a]
takeLocals = mapMaybe takeLocal

ignorePackage :: InPackage a -> a
ignorePackage (Local x) = x
ignorePackage (FromDep _ x) = x

ignorePackages :: [InPackage a] -> [a]
ignorePackages = map ignorePackage

instance Functor InPackage where
  fmap f (Local x) = Local (f x)
  fmap f (FromDep pkgName x) = FromDep pkgName (f x)

type Bookmark = InPackage (P.ModuleName, String)
type Bookmarks = [Bookmark]

-- | Specifies whether a PureScript source file is considered as:
--
-- 1) with the `Local` constructor, a target source file, i.e., we want to see
--    its modules in the output
-- 2) with the `FromDep` constructor, a dependencies source file, i.e. we do
--    not want its modules in the output; it is there to enable desugaring, and
--    to ensure that links between modules are constructed correctly.
type FileInfo = InPackage FilePath

toContainingModule :: Maybe P.ModuleName -> ContainingModule
toContainingModule Nothing = ThisModule 
toContainingModule (Just mn) = OtherModule mn

fromContainingModule :: P.ModuleName -> ContainingModule -> P.ModuleName
fromContainingModule def ThisModule = def
fromContainingModule _ (OtherModule mn) = mn

-------------------------------------------------
-- Intermediate ADTs for rendered documentation

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

-----------------------
-- Documentation output

-- | A function that takes a bunch of stuff relating to a package, and returns
-- an action that will probably do something like take the rendered
-- documentation and write it to disk.
type OutputFn =
  FilePath ->
  PackageMeta ->
  M.Map P.ModuleName PackageName ->
  Bookmarks ->
  [P.Module] ->
  IO ()

-------------------
-- Package metadata

-- | The metadata associated with a package, to be used while generating
-- documentation.
data PackageMeta = PackageMeta
  { pkgMetaName         :: PackageName
  , pkgMetaVersion      :: Version
  , pkgMetaDependencies :: M.Map PackageName Version
  , pkgMetaGithub       :: (GithubUser, GithubRepo)
  }
  deriving (Show, Eq, Ord)

newtype PackageName
  = PackageName { runPackageName :: String }
  deriving (Show, Eq, Ord)
newtype GithubUser
  = GithubUser { runGithubUser :: String }
  deriving (Show, Eq, Ord)
newtype GithubRepo
  = GithubRepo { runGithubRepo :: String }
  deriving (Show, Eq, Ord)
