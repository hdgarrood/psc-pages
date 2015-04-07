{-# LANGUAGE TupleSections #-}

-- | A utility module for dealing with reading JSON, and generating good error
-- messages in the case of JSON with a bad schema.

module PscPages.TracedJson where

import Control.Applicative
import Control.Monad
import Data.Monoid (First)
import qualified Data.HashMap.Strict as HashMap

import qualified Data.Aeson as A
import Data.Aeson.Lens (key, nth)
import Control.Lens ((^?), Getting)

import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

data Json
  = Json [PathPart] A.Value
  deriving (Show)

data Error
  = Error [PathPart] ErrorSpecifics
  deriving (Show)

displayError :: Error -> [String]
displayError (Error [] specifics) =
  displayErrorSpecifics specifics
displayError (Error path specifics) =
  [ "at " ++ displayPath path ++ ":"
  ] ++ displayErrorSpecifics specifics

data PathPart
  = N Int
  | K T.Text
  deriving (Show)

displayPathPart :: PathPart -> String
displayPathPart (N n) = "[" ++ show n ++ "]"
displayPathPart (K k) = "[" ++ show k ++ "]"

displayPath :: [PathPart] -> String
displayPath = concatMap displayPathPart

data ErrorSpecifics
  = CouldntParse String
  | KeyMissing T.Text
  | OutOfBounds Int
  | WrongType Type A.Value -- ^ expected type, actual value
  deriving (Show)

displayErrorSpecifics :: ErrorSpecifics -> [String]
displayErrorSpecifics s = case s of
  CouldntParse e ->
    [ "the JSON could not be parsed:"
    , e
    ]
  KeyMissing k ->
    [ "the object does not have the requested property (" ++ show k ++ ")."
    ]
  OutOfBounds n ->
    [ "the array has no element at the requested index (" ++ show n ++ ")."
    ]
  WrongType ty val ->
    [ "a value of type " ++ displayType ty ++ " was expected; instead, " ++
      "the following value was found:"
    , show val
    ]

data Type
  = JsonObject
  | JsonArray
  | JsonNumber
  | JsonString
  | JsonBoolean
  deriving (Show)

type J = Either Error

displayType :: Type -> String
displayType t = case t of
  JsonObject  -> "object"
  JsonArray   -> "array"
  JsonNumber  -> "number"
  JsonString  -> "string"
  JsonBoolean -> "boolean"

parse' :: (a -> Either String A.Value) -> a -> J Json
parse' f = either (err [] . CouldntParse) (Right . Json []) . f

parse :: BL.ByteString -> J Json
parse = parse' A.eitherDecode

parseStrict :: B.ByteString -> J Json
parseStrict = parse' A.eitherDecodeStrict

err :: [PathPart] -> ErrorSpecifics -> J a
err p s = Left (Error p s)

takeKey :: T.Text -> Json -> J Json
takeKey = take' KeyMissing K key

takeNth :: Int -> Json -> J Json
takeNth = take' OutOfBounds N nth

take' :: (t -> ErrorSpecifics)
      -> (t -> PathPart)
      -> (t -> Getting (First A.Value) A.Value A.Value)
      -> t
      -> Json
      -> J Json
take' specifics toPart lens x (Json path val) =
  maybe (err path (specifics x))
        Right
        (Json (path ++ [toPart x]) <$> val ^? lens x)

asString :: Json -> J T.Text
asString (Json _ (A.String t)) = Right t
asString (Json path val) = err path (WrongType JsonString val)

asObject :: Json -> J [(T.Text, A.Value)]
asObject (Json _ (A.Object m)) = Right (HashMap.toList m)
asObject (Json path val) = err path (WrongType JsonObject val)

asObjectOfString :: Json -> J [(T.Text, T.Text)]
asObjectOfString json@(Json path _) = do
  props <- asObject json
  forM props $ \(k, v) ->
    let json' = Json (path ++ [K k]) v
    in  (k,) <$> asString json'
