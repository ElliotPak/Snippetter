{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Hssb.Layout.Types where

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Data.Aeson.Types (Object, Value (Object, String))
import Data.HashMap.Strict (HashMap, lookup)
import Data.Yaml ((.:))
import System.Directory (doesFileExist)
import qualified Data.Yaml as Y
import qualified Data.Text as T

type DocResult m a = ExceptT DocError m a
type MacroResult = Either DocError Doc
type MacroParams = HashMap T.Text Value
type Macro = MacroParams -> MacroResult
type Doc = [Action]

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
    getFileContents :: FilePath -> DocResult m T.Text
    fileExists :: FilePath -> m Bool

instance MonadReadFile IO where
    getFileContents s = do
        str <- liftIO (Prelude.readFile s)
        return (T.pack str)
    fileExists = doesFileExist

class Contentable c where
    resolve :: MonadReadFile m => c -> DocResult m T.Text

data Content = forall c. (Contentable c) => Content c

data Snippet = Snippet FilePath deriving (Show)
data MacroOnFile = MacroOnFile Macro FilePath deriving (Show)

instance Contentable Content where
    resolve (Content c) = resolve c

instance Contentable T.Text where
    resolve s = return s

instance Contentable String where
    resolve s = return (T.pack s)

instance Contentable Snippet where
    resolve (Snippet p) = getFileContents p

instance Contentable MacroOnFile where
    resolve (MacroOnFile m f) = undefined

class Actionable a where
    execute :: T.Text -> a -> T.Text
    resolveContents :: MonadReadFile m => a -> DocResult m a

data Action = forall a. (Actionable a) => Action a

data Add = Add Content
data Replace = Replace T.Text Action

instance Actionable Action where
    execute s (Action a)       = execute s a
    resolveContents (Action a) = do
        aa <- resolveContents a
        return $ Action aa

instance Actionable Add where
    execute s (Add (Content c)) = undefined
    resolveContents (Add a)     = do
        aa <- resolve a
        return $ Add (Content (aa))

instance Actionable Replace where
    execute s (Replace t d)       = T.replace t (execute T.empty d) s
    resolveContents (Replace t d) = do
        dd <- resolveContents d
        return $ Replace t $ Action dd

instance Actionable Doc where
    execute s d = foldl execute s d
    resolveContents doc = return doc

instance Actionable String where
    execute s d = T.pack d
    resolveContents str = return str

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
