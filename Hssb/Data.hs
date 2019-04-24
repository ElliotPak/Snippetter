module Hssb.Data where

import Prelude hiding (lookup)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap, lookup, fromList)
import Data.Aeson (decode)
import Data.Aeson.Types (Object, Value (Object, String))

lookupEither :: T.Text -> HashMap T.Text a -> Either DocError a
lookupEither key map =
    case (lookup key map) of
      Just a -> Right a
      Nothing -> Left $ AbsentKey $ T.unpack key

lookupString :: String -> MacroParams -> Either DocError String
lookupString key map =
    case (lookupEither keyT map) of
      Left x           -> Left x
      Right (String o) -> Right $ T.unpack o
      Right _          -> Left $ WrongKeyType $ key
    where keyT = T.pack key

lookupObject :: String -> MacroParams -> Either DocError Object
lookupObject key map = 
    case (lookupEither keyT map) of
      Left x           -> Left x
      Right (Object o) -> Right o
      Right _          -> Left $ WrongKeyType $ key
    where keyT = T.pack key

type MacroResult = Either DocError Doc
type MacroParams = HashMap T.Text Value

data DocError =
    AbsentKey String |
    WrongKeyType String |
    MiscError String
    deriving (Show)

data Content = 
    Snippet String |
    Replacement String Doc |
    PlainText String
    deriving (Show)

type Doc = [Content]

startDoc :: Content -> MacroResult
startDoc content = Right [content]

addContent :: Doc -> Content -> MacroResult
addContent doc content = Right $ doc ++ [content]

replaceWithText :: String -> String -> Content
replaceWithText search replace = Replacement search $ [PlainText replace]
