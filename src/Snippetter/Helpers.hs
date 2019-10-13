{-# LANGUAGE OverloadedStrings #-}

-- | Contains a bunch of helper functions, intended to be used when creating
--   Builders.
module Snippetter.Helpers
  ( -- * Parameter lookup
    lookupParams
  , lookupText
  , lookupTextDefault
  , lookupObject
  , lookupObjectDefault
  -- * Content/Action shorthands
  , string
  , transformSafe
  , add
  , replace
  , addText
  , replaceText
  , singleSubBuilder
  , subBuilderOnFile
  ) where

import Data.Yaml (Object, Value(Object, String))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable
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

-- | Shorthand for creating a @text@ (the @Content@) from a @String@.
string str = text $ T.pack str

-- | Shorthand for creating an @Action@ that adds one @Content@ to text.
add :: Content -> Action
add c = singleContentAction c func "Add: "
  where
    func a b = a <> b

-- | Shorthand for creating an @Action@ that replaces all occurances of some text
--   with the @Content@.
replace :: T.Text -> Content -> Action
replace text c =
  singleContentAction c func $ "Replace \"" <> text <> "\" with: "
  where
    func a b = T.replace text b a

-- | Shorthand for creating a Content that applies a function to another
-- @Content@, except the function doesn't fail.
transformSafe :: Content -> (T.Text -> T.Text) -> Content
transformSafe c f = transform c func
  where 
    func t = return $ f t

-- | Shorthand for creating an @Action@ that adds text.
addText :: T.Text -> Action
addText t = add $ text t

-- | Shorthand for creating an @Action@ that replaces all occurances of some text
--   with other text.
replaceText :: T.Text -> T.Text -> Action
replaceText t1 t2 = replace t1 $ text t2

-- | Public function for creating a @SubBuilder@ with just one @SubBuilderExec@.
singleSubBuilder ::
     NamedBuilder -> Params -> [PathedParams] -> FilePathSet -> Content
singleSubBuilder m p pp fp = subBuilder $ SubBuilderExec m p pp fp

-- | Shorthand for creating a @SubBuilder@ that executes the builder on one file.
subBuilderOnFile :: NamedBuilder -> FilePath -> Content
subBuilderOnFile m f =
  subBuilder $ SubBuilderExec m emptyParams [] (HS.singleton f)
