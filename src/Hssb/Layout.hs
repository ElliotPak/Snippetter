{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Hssb.Layout where

import Prelude hiding (lookup)
import System.Directory (doesFileExist)
import Data.Aeson.Types (Object, Value (Object, String))
import Data.HashMap.Strict (HashMap, lookup)
import Data.Yaml ((.:))
import Control.Monad.Trans.Except
import Control.Monad.Except
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Data.Yaml as Y

type IODocResult a = ExceptT DocError IO a
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

data LayoutFile = LayoutFile {
    output    :: String,
    macroName :: String,
    contents  :: HashMap T.Text Value
} deriving (Show)

instance Y.FromJSON LayoutFile where
    parseJSON = Y.withObject "LayoutFile" $ \o -> do
        output <- o .: T.pack "output"
        macroName <- o .: T.pack "macro-name"
        let contents = o
        return LayoutFile{..}

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
      Nothing -> Left $ AbsentKey key
    where keyT = T.pack key

lookupString :: String -> MacroParams -> Either DocError String
lookupString key map =
    case (lookupEither key map) of
      Left x           -> Left x
      Right (String o) -> Right $ T.unpack o
      Right _          -> Left $ WrongKeyType key

lookupObject :: String -> MacroParams -> Either DocError Object
lookupObject key map = 
    case (lookupEither key map) of
      Left x           -> Left x
      Right (Object o) -> Right o
      Right _          -> Left $ WrongKeyType key

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left $ f x
mapLeft _ (Right x) = Right x

contentsIfExists :: FilePath -> IODocResult String
contentsIfExists path = do
    exists <- liftIO $ doesFileExist path
    case exists of
      False -> throwE $ InvalidPath path
      True  -> liftIO $ readFile path

decodeValue :: String -> Either Y.ParseException Value
decodeValue str = Y.decodeEither' $ B.pack str

valueIfExists :: FilePath -> IODocResult Value
valueIfExists path = do
    contents <- contentsIfExists path
    liftEither $ mapLeft (\x -> NotYaml path) $ decodeValue contents

loadEntryFile :: Macro -> FilePath -> IODocResult Doc
loadEntryFile macro path = do
    value <- valueIfExists path
    doc <- liftEither $ decodeEntryFile macro path value
    withMacros <- applyMacrosToFile doc
    return withMacros

applyMacrosToFile = mapM applyMacrosToContent

applyMacrosToContent :: Content -> IODocResult Content
applyMacrosToContent (ApplyMacroToFile m f) = do
    subEntry <- loadEntryFile m f
    return $ SubDocument subEntry
applyMacrosToContent (Replacement s d) = do
    replacement <- applyMacrosToFile d
    return $ Replacement s replacement
applyMacrosToContent c = return c

decodeEntryFile :: Macro -> FilePath -> Value -> MacroResult
decodeEntryFile macro path (Object params) = macro params
decodeEntryFile _     path _               = Left $ InvalidFileFormat path
