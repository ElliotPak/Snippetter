{-# LANGUAGE OverloadedStrings #-}

-- | Contains a bunch of helper functions. Some are used throughout other parts
--   of the library, and some are intended to be used when creating Macros.

module Snippetter.Utilities where

import Data.Aeson.Types (Object, Value (Object, String))
import Data.Hashable
import Data.HashMap.Strict (HashMap, lookup)
import Data.List.Split
import Prelude hiding (lookup)
import System.FilePath
import qualified Data.Text as T

-- | Maps the Left value of an Either.
mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left $ f x
mapLeft _ (Right x) = Right x

-- | Converts a Maybe to an Either.
maybeToEither :: l -> Maybe r -> Either l r
maybeToEither fail (Nothing) = Left fail
maybeToEither _    (Just a)  = Right a

-- | Looks up a key in a HashMap, returning the result if it finds something,
--   and a default value if it doesn't.
lookupEither :: (Eq k, Hashable k) => e -> k -> HashMap k v -> Either e v
lookupEither def key value =
    case (lookup key value) of
      Nothing -> Left def
      Just a  -> Right a

-- | Indent a Text by a specified amount of spaces.
indentText :: Int -> T.Text -> T.Text
indentText ind = (T.intercalate "\n") . map ((<>) $ T.replicate ind " ") . T.lines

-- | Indent Text by 4 spaces per indentation level.
indentFour :: Int -> T.Text -> T.Text
indentFour ind = indentText (4 * ind)

-- | Add text on a newline if it's more than one line, or add it to the text
--   otherwise.
addSingleLineText :: T.Text -> T.Text -> T.Text
addSingleLineText base single
  | T.count "\n" single == 0 = base <> (T.stripStart single)
  | otherwise                = base <> "\n" <> single

-- | Infix operator for addSingleLineText.
a <\> b = addSingleLineText a b
