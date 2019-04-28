{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Hssb.Data where

import Prelude hiding (lookup)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap, lookup, fromList)
import Data.Aeson.Types (Object, Value (Object, String))

type MacroResult = Either DocError Doc
type MacroParams = HashMap T.Text Value
type Macro = MacroParams -> MacroResult

instance Show Macro where
    show m = "MACRO"

type Doc = [Content]

data DocError =
    AbsentKey String |
    WrongKeyType String |
    InvalidPath FilePath |
    NotYaml FilePath |
    InvalidFileFormat FilePath |
    MiscError String
    deriving (Show)

data Content = 
    Snippet String |
    Replacement String Doc |
    PlainText String |
    ApplyMacroToFile Macro FilePath |
    SubDocument Doc
    deriving (Show)

startDoc :: Content -> MacroResult
startDoc content = Right [content]

addContent :: Doc -> Content -> MacroResult
addContent doc content = Right $ doc ++ [content]

replaceWithText :: String -> String -> Content
replaceWithText search replace = Replacement search $ [PlainText replace]

lookupEither :: String -> HashMap T.Text a -> Either DocError a
lookupEither key map =
    case (lookup keyT map) of
      Just a -> Right a
      Nothing -> Left $ AbsentKey $ key
    where keyT = T.pack key

lookupString :: String -> MacroParams -> Either DocError String
lookupString key map =
    case (lookupEither key map) of
      Left x           -> Left x
      Right (String o) -> Right $ T.unpack o
      Right _          -> Left $ WrongKeyType $ key

lookupObject :: String -> MacroParams -> Either DocError Object
lookupObject key map = 
    case (lookupEither key map) of
      Left x           -> Left x
      Right (Object o) -> Right o
      Right _          -> Left $ WrongKeyType $ key
