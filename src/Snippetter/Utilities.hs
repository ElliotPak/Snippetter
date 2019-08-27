{-# LANGUAGE OverloadedStrings #-}

-- | Contains a bunch of helper functions. Some are used throughout other parts
--   of the library, and some are intended to be used when creating Macros.
module Snippetter.Utilities where

import Control.Monad.Trans.Except
import Data.Aeson.Types (Object, Value(Object, String))
import Data.HashMap.Strict (HashMap, lookup)
import Data.Hashable
import Data.List.Split
import qualified Data.Text as T
import Prelude hiding (lookup)
import System.FilePath

-- | Retrieves the value from a @Maybe@ if it's @Just@, and errors otherwise.
unRight :: Maybe a -> a
unRight (Just x) = x
unRight _ = error "unRight on left value"

-- | Maps the Left value of an Either.
mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left $ f x
mapLeft _ (Right x) = Right x

-- | Maps the error values of a Result type.
--   Right now this type is an alias for ExceptTs.
mapResultError :: Monad m => ExceptT e m a -> (e -> e') -> ExceptT e' m a
mapResultError except mapping = ExceptT answer
  where
    ran = runExceptT except
    answer = fmap (mapLeft mapping) ran

-- | Converts a Maybe to an Either.
maybeToEither :: l -> Maybe r -> Either l r
maybeToEither fail Nothing = Left fail
maybeToEither _ (Just a) = Right a

-- | Looks up a key in a HashMap, returning the result if it finds something,
--   and a default value if it doesn't.
lookupEither :: (Eq k, Hashable k) => e -> k -> HashMap k v -> Either e v
lookupEither def key value =
  case lookup key value of
    Nothing -> Left def
    Just a -> Right a

-- | Indent a Text by a specified amount of spaces.
indentText :: Int -> T.Text -> T.Text
indentText ind = T.intercalate "\n" . map ((<>) $ T.replicate ind " ") . T.lines

-- | Indent a String by a specified amount of spaces.
indentStr :: Int -> String -> String
indentStr ind = unlines . map ((<>) $ replicate ind ' ') . lines

-- | Like @indentFour@, except the first line is indented with "  - " instead
-- of 4 spaces of 4 spaces.
indentWithListMarker :: T.Text -> T.Text
indentWithListMarker text = indentText 2 (h <> t)
  where
    lines = T.lines text
    t = T.unlines $ (map (indentText 2) . tail) lines
    h = "- " <> head lines <> "\n"

-- | Indent a Text by 4 spaces.
indentFour :: T.Text -> T.Text
indentFour = indentText 4

-- | Indent a String by 4 spaces.
indentFourStr :: String -> String
indentFourStr = indentStr 4

-- | Add text on a newline if it's more than one line, or add it to the text
--   otherwise.
addSingleLineText :: T.Text -> T.Text -> T.Text
addSingleLineText base single
  | T.count "\n" single == 0 = base <> T.stripStart single
  | otherwise = base <> "\n" <> single

-- | Infix operator for addSingleLineText.
a <\> b = addSingleLineText a b

-- | Append text with a newline as long as neither text is null
appendWithNewLine :: T.Text -> T.Text -> T.Text
appendWithNewLine aa bb
  | aa == "" && bb == "" = ""
  | aa == "" = bb
  | bb == "" = aa
  | otherwise = aa <> "\n" <> bb

-- | Infix operator for appendWithNewLine.
a <\\> b = appendWithNewLine a b

-- | Remove duplicates from a list.
--   Taken from https://stackoverflow.com/a/16108856
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates =
  foldl
    (\seen x ->
       if x `elem` seen
         then seen
         else seen ++ [x])
    []
