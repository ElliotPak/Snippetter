-- | Contains a bunch of helper functions, intended to be used when creating
--   Macros.

module Snippetter.Helpers where

import Snippetter.LayoutBase
import Snippetter.LayoutTypes
import Snippetter.Utilities
import Data.Aeson.Types (Object, Value (Object, String))
import Data.Hashable
import Data.List.Split
import Prelude hiding (lookup)
import System.FilePath
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H

-- | Looks up a parameter and, if found, converts it to a type via the provided
--   function.
--   Evaluates to seperate errors if the key is missing or can't be converted
--   to a specific type.
lookupParams :: (Value -> Maybe a) -> T.Text -> Params -> Either MacroError a
lookupParams get key map = do
    thing <- lookupEither (AbsentKey key) keyT map
    maybeToEither (WrongKeyType key) (get thing)
    where keyT = key

-- | lookupParams with a default parameter. If any error occurs, the default
--   value is the result instead.
lookupDefault :: (Value -> Maybe a) -> a -> T.Text -> Params -> a
lookupDefault un def text params =
    case (lookupParams un text params) of
      Left l  -> def
      Right r -> r

-- | Unpack Text from an Aeson Value.
unText :: Value -> Maybe T.Text
unText (String s) = Just s
unText _          = Nothing

-- | Unpack an Object from an Aeson Value.
unObject :: Value -> Maybe Params
unObject (Object o) = Just o
unObject _          = Nothing

-- | Shorthand for @lookupParams unText@.
lookupText :: T.Text -> Params -> Either MacroError T.Text
lookupText = lookupParams unText

-- | Shorthand for @lookupParams unObject@.
lookupObject :: T.Text -> Params -> Either MacroError Params
lookupObject = lookupParams unObject

-- | Shorthand for @lookupDefault unObject@.
lookupObjectDefault :: Params -> T.Text -> Params -> Params
lookupObjectDefault = lookupDefault unObject

-- | Shorthand for @lookupDefault unText@.
lookupTextDefault :: T.Text -> T.Text -> Params -> T.Text
lookupTextDefault = lookupDefault unText

-- | Helper function for making an Action that adds a Text.
addText :: T.Text -> Action
addText s =  Action $ Add s

-- | Helper function for making an Action that adds some Content.
add :: Contentable c => c -> Action
add c = Action $ Add c

-- | Helper function for making an Action that replaces some Text with
--   Content.
replace :: Actionable a => T.Text -> a -> Action
replace t d = Action $ Replace t d

-- | Helper function to apply a macro solely across a file with no default
--   parameters.
macroOnFile :: Macro -> FilePath -> SubMacro
macroOnFile m f = SubMacro m H.empty [] f

-- | Helper function for making an Action that replaces some Text with
--   another Text.
replaceText :: T.Text -> T.Text -> Action
replaceText t d = replace t [add d]

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
