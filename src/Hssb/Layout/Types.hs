{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Hssb.Layout.Types where

import Control.Monad.Except
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Data.Aeson.Types (Object, Value (Object, String))
import Data.HashMap.Strict (HashMap, lookup)
import Data.Yaml ((.:))
import System.Directory (doesFileExist)
import qualified Data.Yaml as Y
import qualified Data.Text as T

type DocResult m a = ExceptT DocError m a
type Result = Either DocError Doc
type MacroResult = Either MacroError Doc
type Params = HashMap T.Text Value
type Macro = Params -> Either MacroError Doc
type Doc = [Action]
type TextFunc = T.Text -> T.Text
type TextFuncError = T.Text -> Either DocError T.Text

data MacroParams = MacroParams {
    params :: Params,
    path   :: Maybe FilePath
  } deriving (Show)

data MacroError =
    AbsentKey String |
    WrongKeyType String
    deriving (Show)

data DocError =
    MacroError MacroError (Maybe FilePath) |
    InvalidPath FilePath |
    NotYaml FilePath |
    InvalidFileFormat FilePath |
    MissingMacro String |
    MiscError String
    deriving (Show)

data PathedLayoutEntry = PathedLayoutEntry (Maybe FilePath) LayoutEntry

data LayoutEntry =
    ExecMacro FilePath String Params |
    ExecCopy FilePath FilePath
    deriving (Show)

data Snippet = Snippet FilePath
data MacroOnFile = MacroOnFile Macro FilePath
data Transform = forall c. (Contentable c) => Transform c TextFunc
data TransformError = forall c. (Contentable c) => TransformError c TextFuncError

data Add = forall c. (Contentable c) => Add c
data Replace = forall a. (Actionable a) => Replace T.Text a

data Content = forall c. (Contentable c) => Content c
data Action = forall a. (Actionable a) => Action a

convertMacroError :: Maybe FilePath -> MacroError -> DocError
convertMacroError path err = MacroError err path

addPath :: FilePath -> LayoutEntry -> PathedLayoutEntry
addPath path layout = PathedLayoutEntry (Just path) layout

instance Y.FromJSON LayoutEntry where
    parseJSON = Y.withObject "LayoutFile" $ \o -> do
        kind <- o .: T.pack "type"
        case (T.unpack kind) of
          "macro" -> ExecMacro <$> o .: T.pack "output"
                         <*> o .: T.pack "macro-name"
                         <*> o .: T.pack "values"
          "copy" -> ExecCopy <$> o .: T.pack "from"
                         <*> o .: T.pack "to"

instance Show Macro where
    show s = "MACRO"

class Monad m => MonadReadFile m where
    getFileContents :: FilePath -> DocResult m T.Text
    fileExists :: FilePath -> m Bool

instance MonadReadFile IO where
    getFileContents s = do
        str <- liftIO (Prelude.readFile s)
        return (T.pack str)
    fileExists = doesFileExist

class NeedsFiles n where
    getNeededFiles :: MonadReadFile m => n -> DocResult m [FilePath]
    getNeededFiles _ = return []

instance NeedsFiles T.Text where

instance NeedsFiles Content where
    getNeededFiles (Content c) = getNeededFiles c

instance NeedsFiles Snippet where
    getNeededFiles (Snippet s) = return [s]

instance NeedsFiles Transform where
    getNeededFiles (Transform c f) = getNeededFiles c

instance NeedsFiles TransformError where
    getNeededFiles (TransformError c f) = getNeededFiles c

instance NeedsFiles Action where
    getNeededFiles (Action a) = getNeededFiles a

instance NeedsFiles Add where
    getNeededFiles (Add a) = getNeededFiles a

instance NeedsFiles Replace where
    getNeededFiles (Replace t d) = getNeededFiles d

instance NeedsFiles Doc where
    getNeededFiles doc = do
        mapped <- mapM getNeededFiles doc
        return $ concat mapped

class NeedsFiles c => Contentable c where
    resolve        :: MonadReadFile m => c -> DocResult m T.Text
    asDoc          :: c -> Doc
    asDoc c = [Action $ Add c]

instance Contentable Content where
    resolve (Content c) = resolve c

instance Contentable T.Text where
    resolve s = return s

instance Contentable Snippet where
    resolve (Snippet p) = getFileContents p

instance Contentable Transform where
    resolve (Transform c f) = do
        sub <- resolve c
        return $ f sub

instance Contentable TransformError where
    resolve (TransformError c f) = do
        sub <- resolve c
        liftEither $ f sub

class NeedsFiles a => Actionable a where
    resolveContents :: MonadReadFile m => a -> T.Text -> DocResult m T.Text

instance Actionable Action where
    resolveContents (Action a) = resolveContents a

instance Actionable Add where
    resolveContents (Add a) t = do
        aa <- resolve a
        return $ T.concat [aa, t]

instance Actionable Replace where
    resolveContents (Replace t d) text = do
        dd <- resolveContents d T.empty
        return $ T.replace t dd text

instance Actionable Doc where
    resolveContents (x:xs) text = do
        head <- resolveContents x text
        resolveContents xs head
    resolveContents [] text = return text
