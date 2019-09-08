{-# LANGUAGE OverloadedStrings #-}

-- | Contains a bunch of helper functions. Some are used throughout other parts
--   of the library, and some are intended to be used when creating Macros.
module Snippetter.Utilities where

import Control.Monad.Except
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Aeson.Types (Object, Value(Object, String))
import Data.HashMap.Strict (HashMap, lookup)
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.List.Split
import qualified Data.Text as T
import Prelude hiding (lookup)
import System.FilePath

type Result e m a = ExceptT e m a

result = ExceptT

runResult = runExceptT

resultE :: Monad m => e -> Result e m a
resultE = throwE

resultLiftEither :: Monad m => Either e a -> Result e m a
resultLiftEither = liftEither

resultLift :: Monad m => m a -> Result e m a
resultLift = lift

-- | Maps the error values of a Result type.
--   Right now this type is an alias for ExceptTs.
mapResultError :: Monad m => Result e m a -> (e -> e') -> Result e' m a
mapResultError except mapping = result answer
  where
    ran = runResult except
    answer = fmap (mapLeft mapping) ran

-- | Shorthand for a builder's parameters.
-- Identical to the type of an Aeson object.
type Params = HM.HashMap T.Text Value

-- | Creates an empty Params.
emptyParams :: Params
emptyParams = HM.empty

-- | Check if a Params is empty.
nullParams :: Params -> Bool
nullParams = HM.null

-- | The union of two Params. If a key occurs in both, the valu from the first
-- will be the value in the result.
paramUnion :: Params -> Params -> Params
paramUnion = HM.union

-- | Retrieves the value from a @Maybe@ if it's @Just@, and errors otherwise.
unRight :: Maybe a -> a
unRight (Just x) = x
unRight _ = error "unRight on left value"

unJust :: Maybe a -> a
unJust (Just a) = a
unJust _ = error "unJust on Nothing"

-- | Maps the Left value of an Either.
mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left $ f x
mapLeft _ (Right x) = Right x

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
