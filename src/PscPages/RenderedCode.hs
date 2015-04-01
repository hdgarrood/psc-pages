{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PscPages.RenderedCode
 ( RenderedCodeElement(..)
 , RenderedCode
 , outputWith
 , sp
 , syntax
 , ident
 , ctor
 , kind
 , keyword
 , keywordForall
 , keywordData
 , keywordNewtype
 , keywordType
 , keywordClass
 , keywordInstance
 , keywordWhere
 ) where

import Data.Foldable
import Data.Monoid

import qualified Language.PureScript as P

data RenderedCodeElement
  = Syntax String
  | Ident String
  | Ctor String (Maybe P.ModuleName) -- ^ Constructor text and optional containing module name.
  | Kind String
  | Keyword String
  | Space
  deriving (Show, Eq, Ord)

newtype RenderedCode
  = RC { unRC :: [RenderedCodeElement] }
  deriving (Show, Eq, Ord, Monoid)

outputWith :: Monoid a => (RenderedCodeElement -> a) -> RenderedCode -> a
outputWith f = foldMap f . unRC

sp :: RenderedCode
sp = RC [Space]

syntax :: String -> RenderedCode
syntax x = RC [Syntax x]

ident :: String -> RenderedCode
ident x = RC [Ident x]

ctor :: String -> Maybe P.ModuleName -> RenderedCode
ctor x m = RC [Ctor x m]

kind :: String -> RenderedCode
kind x = RC [Kind x]

keyword :: String -> RenderedCode
keyword kw = RC [Keyword kw]

keywordForall :: RenderedCode
keywordForall = keyword "forall"

keywordData :: RenderedCode
keywordData = keyword "data"

keywordNewtype :: RenderedCode
keywordNewtype = keyword "newtype"

keywordType :: RenderedCode
keywordType = keyword "type"

keywordClass :: RenderedCode
keywordClass = keyword "class"

keywordInstance :: RenderedCode
keywordInstance = keyword "instance"

keywordWhere :: RenderedCode
keywordWhere = keyword "where"
