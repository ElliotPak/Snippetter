-- | Contains a bunch of helper functions, intended to be used when creating
--   Builders.
module Snippetter.Helpers where

import Data.Aeson.Types (Object, Value(Object, String))
import qualified Data.HashMap.Strict as H
import Data.Hashable
import Data.List.Split
import qualified Data.Text as T
import Prelude hiding (lookup)
import Snippetter.Build
import Snippetter.Utilities
import System.FilePath

-- | Looks up a parameter and, if found, converts it to a type via the provided
--   function.
--   Evaluates to seperate errors if the key is missing or can't be converted
--   to a specific type.
lookupParams :: (Value -> Maybe a) -> T.Text -> Params -> Either BuilderError a
lookupParams get key map = do
  thing <- lookupEither (AbsentKey key) keyT map
  maybeToEither (WrongKeyType key) (get thing)
  where
    keyT = key

-- | lookupParams with a default parameter. If any error occurs, the default
--   value is the result instead.
lookupDefault :: (Value -> Maybe a) -> a -> T.Text -> Params -> a
lookupDefault un def text params =
  case lookupParams un text params of
    Left l -> def
    Right r -> r

-- | Unpack Text from an Aeson Value.
unText :: Value -> Maybe T.Text
unText (String s) = Just s
unText _ = Nothing

-- | Unpack an Object from an Aeson Value.
unObject :: Value -> Maybe Params
unObject (Object o) = Just o
unObject _ = Nothing

-- | Shorthand for @lookupParams unText@.
lookupText :: T.Text -> Params -> Either BuilderError T.Text
lookupText = lookupParams unText

-- | Shorthand for @lookupParams unObject@.
lookupObject :: T.Text -> Params -> Either BuilderError Params
lookupObject = lookupParams unObject

-- | Shorthand for @lookupDefault unObject@.
lookupObjectDefault :: Params -> T.Text -> Params -> Params
lookupObjectDefault = lookupDefault unObject

-- | Shorthand for @lookupDefault unText@.
lookupTextDefault :: T.Text -> T.Text -> Params -> T.Text
lookupTextDefault = lookupDefault unText
