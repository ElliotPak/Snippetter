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
type ExecutableDoc = [Execute]

data DocError =
    AbsentKey String |
    WrongKeyType String |
    InvalidPath FilePath |
    NotYaml FilePath |
    InvalidFileFormat FilePath |
    MiscError String
    deriving (Show)

data LayoutFile = LayoutFile {
    output    :: String,
    macroName :: String,
    contents  :: HashMap T.Text Value
} deriving (Show)

class Monad m => MonadReadFile m where
    getFileContents :: FilePath -> DocResult m T.Text
    fileExists :: FilePath -> m Bool

class Contentable c where
    resolve :: MonadReadFile m => c -> DocResult m T.Text

class Actionable a where
    resolveContents :: MonadReadFile m => a -> DocResult m Execute

class Executable e where
    execute :: T.Text -> e -> T.Text

data Content = forall c. (Contentable c) => Content c
data Action = forall a. (Actionable a) => Action a
data Execute = forall e. (Executable e) => Execute e

data Snippet = Snippet FilePath
data MacroOnFile = MacroOnFile Macro FilePath

data Add = forall c. (Contentable c) => Add c
data Replace = forall a. (Actionable a) => Replace T.Text a

data AddExec = forall e. (Executable e) => AddExec e
data ReplaceExec = forall e. (Executable e) => ReplaceExec T.Text e

instance MonadReadFile IO where
    getFileContents s = do
        str <- liftIO (Prelude.readFile s)
        return (T.pack str)
    fileExists = doesFileExist

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

instance Actionable Action where
    resolveContents (Action a) = do
        aa <- resolveContents a
        return $ Execute aa

instance Actionable Add where
    resolveContents (Add a)     = do
        aa <- resolve a
        return $ Execute (AddExec aa)

instance Actionable Replace where
    resolveContents (Replace t d) = do
        dd <- resolveContents d
        return $ Execute (ReplaceExec t dd)

instance Actionable Doc where
    resolveContents doc = do
        dd <- mapM resolveContents doc
        return $ Execute dd

instance Actionable String where
    resolveContents str = return $ Execute str

instance Executable Execute where
    execute s (Execute a) = execute s a

instance Executable ExecutableDoc where
    execute s d = foldl execute s d

instance Executable String where
    execute s d = T.pack d

instance Executable T.Text where
    execute s d = d

instance Executable ReplaceExec where
    execute s (ReplaceExec t d) = T.replace t (execute T.empty d) s

instance Executable AddExec where
    execute s (AddExec e) = T.concat [s, execute T.empty e]

instance Y.FromJSON LayoutFile where
    parseJSON = Y.withObject "LayoutFile" $ \o -> do
        output <- o .: T.pack "output"
        macroName <- o .: T.pack "macro-name"
        let contents = o
        return LayoutFile{..}
