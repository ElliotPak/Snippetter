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
  -- * Builders and BuilderMap
  , helperBmap
  , multiBuilder
  -- * Helpers for 'SBListFunc's
  , paramsFromPair
  , pfp
  , hasParam
  , orderByParam
  , mapParam
  ) where

import Data.Yaml
  ( Parser
  , Object
  , Value(Object, String, Number, Bool, Array)
  , parseJSON
  , parseEither
  , FromJSON
  )
import Debug.Trace
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable
import qualified Data.Text as T
import qualified Data.Vector as V
import Prelude hiding (lookup)
import Snippetter.Build
import Snippetter.Layout
import Snippetter.Utilities
import Snippetter.IO
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

-- | Unpack an Object from an Aeson Value.
unArray :: Value -> Maybe [Value]
unArray (Array a) = Just (V.toList a)
unArray _ = Nothing

-- | Shorthand for @lookupParams unText@.
lookupText :: T.Text -> Params -> Either BuilderError T.Text
lookupText = lookupParams unText

-- | Shorthand for @lookupParams unObject@.
lookupObject :: T.Text -> Params -> Either BuilderError Params
lookupObject = lookupParams unObject

-- | Shorthand for @lookupParams unArray@.
lookupArray :: T.Text -> Params -> Either BuilderError [Value]
lookupArray = lookupParams unArray

lookupArrayOf :: FromJSON a => T.Text -> Params -> Either BuilderError [a]
lookupArrayOf key map = do
  arr <- lookupArray key map
  let errorMapping x = MiscBuilderError $ T.pack x
  let mapping x = mapLeft errorMapping (parseEither parseJSON x)
  mapM mapping arr

lookupArrayOfObject key map =
  lookupArrayOf key map :: Either BuilderError [Object]

lookupArrayOfText key map =
  lookupArrayOf key map :: Either BuilderError [T.Text]

-- | Shorthand for @lookupDefault unText@.
lookupTextDefault :: T.Text -> T.Text -> Params -> T.Text
lookupTextDefault = lookupDefault unText

-- | Shorthand for @lookupDefault unObject@.
lookupObjectDefault :: Params -> T.Text -> Params -> Params
lookupObjectDefault = lookupDefault unObject

-- | Shorthand for @lookupDefault unArray@.
lookupArrayDefault :: [Value] -> T.Text -> Params -> [Value]
lookupArrayDefault = lookupDefault unArray

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

-- | Shorthand for creating a @SubBuilder@ with just one @SubBuilderExec@.
singleSubBuilder ::
     NamedPageBuilder -> Params -> [PathedParams] -> [FilePath] -> SBListFunc -> Content
singleSubBuilder m p pp fp func = subBuilder [SubBuilderExec m p pp fp func] id

-- | Shorthand for creating a @SubBuilder@ that executes the builder on one file.
subBuilderOnFile :: NamedPageBuilder -> FilePath -> Content
subBuilderOnFile m f =
  subBuilder [SubBuilderExec m emptyParams [] [f] id] id

subBuildersOnFiles :: [(NamedPageBuilder, [FilePath])] -> Content
subBuildersOnFiles mf = subBuilder (map foo mf) id
  where
    foo (nb, paths) = SubBuilderExec nb emptyParams [] paths id

-- | Get the 'Params' from a 'SBList' entry.
paramsFromPair :: (NamedPageBuilder, PathedParams) -> Params
paramsFromPair (_, pp) = params pp

-- | Get the 'Params' from a 'SBList' entry (shorthand).
pfp = paramsFromPair

-- | Is true if the given 'SBEntry' has a value for the given key.
hasParam :: T.Text -> SBEntry -> Bool
hasParam t a = HM.member t $ pfp a

-- | Given two 'SBEntry's and a key, order the two by the key.
orderByParam :: T.Text -> SBEntry -> SBEntry -> Ordering
orderByParam t a1 a2 = unwrap (HM.lookup t $ pfp a1) (HM.lookup t $ pfp a2)
  where
    unwrap (Just r1) (Just r2) = oo r1 r2
    unwrap _ _ = EQ
    oo (String o1) (String o2) = compare o1 o2
    oo (Number o1) (Number o2) = compare o1 o2
    oo (Bool o1) (Bool o2) = compare o1 o2
    oo _ _ = EQ

-- | Map the Params part of the given 'SBEntry'.
mapParam :: (Params -> Params) -> SBEntry -> SBEntry
mapParam func (b, (PathedParams p fp)) = (b, PathedParams (func p) fp)

helperBmap :: MonadReadWorld m => BuilderMap m
helperBmap =
  insertMetaBuilders
    [ ("multi-builder", multiBuilder)
    ] emptyBuilderMap

multiBuilder :: MonadReadWorld m => MetaBuilder m
multiBuilder bmap path params = do
  builderName <- resultLiftEither $ lookupText "builder-name" params
  let bmapErrorMapping _ = AbsentKey "builder-name"
  builder <- resultLiftEither $ mapLeft bmapErrorMapping (getPageBuilder builderName bmap)
  theseParams <- resultLiftEither $ lookupArrayOfObject "params" params
  files <- resultLiftEither $ lookupArrayOfText "files" params
  def <- resultLiftEither $ lookupObject "default" params
  let pathedParams = map (`PathedParams` path) theseParams
  let filesText = map T.unpack files
  all <- mergeParams pathedParams filesText def `mapResultError` BuilderYamlError
  filenameKey <- resultLiftEither $ lookupText "filename-key" params
  let mapParams :: PathedParams -> Either BuilderError SiteAction
      mapParams m = do
        outname <- lookupText filenameKey (Snippetter.Utilities.params m)
        return $ Build builder m (T.unpack outname)
  resultLiftEither $ mapM mapParams all
