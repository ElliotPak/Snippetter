-- | Contains a bunch of helper functions, intended to be used when creating
--   Macros.

module Snippetter.Helpers where

import Snippetter.Layout
import Snippetter.Utilities
import Data.Aeson.Types (Object, Value (Object, String))
import Data.Hashable
import Data.HashMap.Strict (HashMap, lookup)
import Data.List.Split
import Prelude hiding (lookup)
import System.FilePath
import qualified Data.Text as T

-- | Looks up a parameter and, if found, converts it to a type via the provided
--   function.
--   Evaluates to seperate errors if the key is missing or can't be converted
--   to a specific type.
lookupParams :: (Value -> Maybe a) -> String -> Params -> Either MacroError a
lookupParams get key map = do
    thing <- lookupEither (AbsentKey key) keyT map
    maybeToEither (WrongKeyType key) (get thing)
    where keyT = T.pack key

-- | Unpack a String from an Aeson Value.
unString :: Value -> Maybe String
unString (String s) = Just $ T.unpack s
unString _          = Nothing

-- | Unpack an Object from an Aeson Value.
unObject :: Value -> Maybe Params
unObject (Object o) = Just o
unObject _          = Nothing

-- | Shorthand for @lookupParams unString@.
lookupString :: String -> Params -> Either MacroError String
lookupString = lookupParams unString

-- | Shorthand for @lookupParams unObject@.
lookupObject :: String -> Params -> Either MacroError Params
lookupObject = lookupParams unObject

-- | Helper function for making an Action that adds a String.
addString :: String -> Action
addString s =  Action $ Add $ T.pack s

-- | Helper function for making an Action that adds some Content.
add :: Contentable c => c -> Action
add c = Action $ Add c

-- | Helper function for making an Action that replaces some String with
--   Content.
replace :: Actionable a => String -> a -> Action
replace t d = Action $ Replace (T.pack t) d

-- | Helper function for making an Action that replaces some String with
--   another String.
replaceText :: String -> String -> Action
replaceText t d = replace t [add $ T.pack d]

-- | Helper function for a Transform containing a Snippet.
modSnippet :: FilePath -> (T.Text -> T.Text) -> Transform
modSnippet path func = Transform (Snippet path) func

-- | Helper function for a Transform containing a Snippet.
modSnippetError :: FilePath -> (T.Text -> Either DocError T.Text) -> TransformError
modSnippetError path func = TransformError (Snippet path) func

-- | (TEMPORARY AND UNFINISHED)
--
--   The difference in paths between two files.
relativePath :: FilePath -> FilePath -> FilePath
relativePath from to = rel "" splitFrom splitTo
    where
        splitTo   = splitOn [pathSeparator] to
        splitFrom = splitOn [pathSeparator] from
        rel path []     []     = path
        rel path (x:[]) []     = path
        rel path (x:xs) []     = rel (".." </> path) xs []
        rel path []     (y:ys) = rel (path </> y) [] ys
        rel path (x:xs) (y:ys)
          | path /= "" && xs /= [] = rel (".." </> path </> y) xs ys
          | path /= "" && xs == [] = rel (path </> y) xs ys
          | x == y                 = rel path xs ys
          | otherwise              = rel (".." </> path </> y) xs ys
