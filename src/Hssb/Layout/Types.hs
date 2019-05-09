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

class NeedsFiles n where
    getNeededFiles :: MonadReadFile m => n -> DocResult m [FilePath]
    getNeededFiles _ = return []

class NeedsFiles c => Contentable c where
    resolve        :: MonadReadFile m => c -> DocResult m T.Text
    asDoc          :: c -> Doc
    asDoc c = [Action $ Add c]

class NeedsFiles a => Actionable a where
    resolveContents :: MonadReadFile m => a -> T.Text -> DocResult m T.Text

data Content = forall c. (Contentable c) => Content c
data Action = forall a. (Actionable a) => Action a

data Snippet = Snippet FilePath
data MacroOnFile = MacroOnFile Macro FilePath

data Add = forall c. (Contentable c) => Add c
data Replace = forall a. (Actionable a) => Replace T.Text a

instance MonadReadFile IO where
    getFileContents s = do
        str <- liftIO (Prelude.readFile s)
        return (T.pack str)
    fileExists = doesFileExist

instance NeedsFiles Content where
    getNeededFiles (Content c) = getNeededFiles c
instance Contentable Content where
    resolve (Content c) = resolve c

instance NeedsFiles T.Text where
instance Contentable T.Text where
    resolve s = return s

instance NeedsFiles String where
instance Contentable String where
    resolve s = return (T.pack s)

instance NeedsFiles Snippet where
    getNeededFiles (Snippet s) = return [s]
instance Contentable Snippet where
    resolve (Snippet p) = getFileContents p

instance NeedsFiles Action where
    getNeededFiles (Action a) = getNeededFiles a
instance Actionable Action where
    resolveContents (Action a) = resolveContents a

instance NeedsFiles Add where
    getNeededFiles (Add a) = getNeededFiles a
instance Actionable Add where
    resolveContents (Add a) t = do
        aa <- resolve a
        return $ T.concat [aa, t]

instance NeedsFiles Replace where
    getNeededFiles (Replace t d) = getNeededFiles d
instance Actionable Replace where
    resolveContents (Replace t d) text = do
        dd <- resolveContents d T.empty
        return $ T.replace t dd text

instance NeedsFiles Doc where
    getNeededFiles doc = do
        mapped <- mapM getNeededFiles doc
        return $ concat mapped
instance Actionable Doc where
    resolveContents (x:xs) text = do
        head <- resolveContents x text
        resolveContents xs head
    resolveContents [] text = return text

instance Y.FromJSON LayoutFile where
    parseJSON = Y.withObject "LayoutFile" $ \o -> do
        output <- o .: T.pack "output"
        macroName <- o .: T.pack "macro-name"
        let contents = o
        return LayoutFile{..}
