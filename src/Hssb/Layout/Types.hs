{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Hssb.Layout.Types where

import Control.Monad.Trans.Except
import Data.Aeson.Types (Object, Value (Object, String))
import Data.HashMap.Strict (HashMap, lookup)
import Data.Yaml ((.:))
import qualified Data.Yaml as Y
import qualified Data.Text as T

type IODocResult a = ExceptT DocError IO a
type MacroResult = Either DocError Doc
type MacroParams = HashMap T.Text Value
type Macro = MacroParams -> MacroResult
type Doc = [Content]

instance Show Macro where
    show m = "MACRO"

data DocError =
    AbsentKey String |
    WrongKeyType String |
    InvalidPath FilePath |
    NotYaml FilePath |
    InvalidFileFormat FilePath |
    MiscError String
    deriving (Show)

class Monad m => MonadReadFile m where
    getFileContents :: FilePath -> m String

instance MonadReadFile IO where
    getFileContents = Prelude.readFile

data Contentt = forall c. (Contentable c) => Contentt c
data Stringify = forall c. (Stringifyable c) => Stringify c

class Contentable c where
    wrapContent :: c -> Contentt
    (++) :: Doc -> c -> Doc
    readFiles :: MonadReadFile m => c -> m Stringify
    wrapContent content = Contentt content

class Stringifyable c where
    wrapStringify :: c -> Stringify
    wrapStringify content = Stringify content
    stringify :: c -> String

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

replaceWithText :: String -> String -> Content
replaceWithText search replace = Replacement search $ [PlainText replace]
