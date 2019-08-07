{-# LANGUAGE OverloadedStrings #-}

module Snippetter.Layout where

import Snippetter.Utilities
import Snippetter.Build
import Snippetter.IO
import Prelude hiding (writeFile)
import Data.Yaml ((.:))
import Control.Monad.Except
import Control.Monad.Trans.Except
import qualified Data.Text as T
import qualified Data.Yaml as Y
import qualified Data.HashMap.Strict as H

-- | Possible errors when a @SiteAction@ is being made/executed.
data LayoutError =
    LayoutDocError DocError
  | LayoutYamlError YamlError
  | LayoutFileError FileError
  | MiscLayoutError T.Text
    deriving (Show, Eq)

-- | The result of a function that makes/executes a @SiteAction@.
type LayoutResult a = Either LayoutError a

-- | The result of a function that makes/executes a @SiteAction@ and can read files.
type LayoutFileResult m a = ExceptT LayoutError m a

-- | A map of @T.Text@ to @Macro@s.
type MacroMap = H.HashMap T.Text Macro

-- | Site actions as immediately loaded from a YAML file.
data Layout =
    LayoutBuild FilePath T.Text Params |
    LayoutCopy FilePath FilePath
    deriving (Show, Eq)

instance Y.FromJSON Layout where
    parseJSON = Y.withObject "LayoutFile" $ \o -> do
        kind <- o .: T.pack "type"
        case (T.unpack kind) of
          "macro" -> LayoutBuild <$> o .: T.pack "output"
                         <*> o .: T.pack "macro-name"
                         <*> o .: T.pack "values"
          "copy" -> LayoutCopy <$> o .: T.pack "from"
                         <*> o .: T.pack "to"

-- | A Layout value that may have a file path associated with it.
--   If loaded from a file, the path should be assigned when doing so.
--   If defined in a source file, the path should be @Nothing@.
data PathedLayout = PathedLayout {
    layout :: Layout,
    lpath   :: Maybe FilePath
  } deriving (Show, Eq)

-- | Describes an action taken to build the site.
data SiteAction =
    Build Macro PathedParams FilePath |
    Copy FilePath FilePath

-- | Retrieves contents of the specific file, and maps possible errors to
--   @LayoutFileError@s.
fileContentsInLayout :: MonadReadFile m => FilePath -> LayoutFileResult m T.Text
fileContentsInLayout path = getFileContents path `mapResultError` mapping
    where mapping err = LayoutFileError err

-- | Adds a path to a @Layout@.
addPath :: FilePath -> Layout -> PathedLayout
addPath path layout = PathedLayout layout (Just path)

-- | Load a YAML file as a list of @Layout@s.
yamlAsLayout :: MonadReadFile m => FilePath -> LayoutFileResult m [Layout]
yamlAsLayout path = let errorMapping x = LayoutYamlError x in
    (yamlIfExists path :: MonadReadFile m => YamlResult m [Layout])
        `mapResultError` errorMapping

-- | Loads a list of pathed layouts from a file.
loadLayoutFile :: MonadReadFile m => FilePath -> LayoutFileResult m [PathedLayout]
loadLayoutFile path = do
    y <- yamlAsLayout path
    return $ map (addPath path) y

executeSiteAction :: MonadWorld m => SiteAction -> LayoutFileResult m ()
executeSiteAction (Build m pp fp) = do
    executed <- executeMacro m pp `mapResultError` LayoutDocError
    writeFile fp executed `mapResultError` LayoutFileError
executeSiteAction (Copy from to) =
    copyFile from to `mapResultError` LayoutFileError

executeSiteAction' :: MonadWorld m => SiteAction -> m ()
executeSiteAction' sa@(Copy to from) = do
    notifyProgress $ "Copying \"" <> (T.pack from) <> "\" to \"" <> (T.pack to) <> "\"..."
    result <- runExceptT $ executeSiteAction sa
    case result of
      Right r -> notifySuccess $ "Successfully copied \"" <> (T.pack from) <> "\" to \"" <> (T.pack to) <> "\"."
      Left l  -> notifySuccess $ "Failed to copy \"" <> (T.pack from) <> "\" to \"" <> (T.pack to) <> "\":"
executeSiteAction' sa@(Build m pp fp) = do
    notifyProgress $ "Building \"" <> (T.pack fp) <> "\"..."
    result <- runExceptT $ executeSiteAction sa
    case result of
      Right r -> notifySuccess $ "Successfully built \"" <> (T.pack fp) <> "\"."
      Left l  -> notifySuccess $ "Failed to build \"" <> (T.pack fp) <> "\":"

executeLayoutFile :: MonadWorld m =>
    FilePath -> H.HashMap T.Text Macro -> m ()
executeLayoutFile path map = do
    actions <- runExceptT $ loadSiteActions path map
    case actions of
      Right r -> mapM_ executeSiteAction' r
      Left l  -> notifyFailure $ "Failed to load \"" <> (T.pack path) <> "\"."

-- | Converts a @PathedLayout@ to a @SiteAction@, when given a mapping of
--   strings to macros.
layoutToAction ::
    H.HashMap T.Text Macro -> PathedLayout -> Either LayoutError SiteAction
layoutToAction map (PathedLayout layout input) = ll input layout
    where
        ll path (LayoutBuild output macroName contents) = do
            let mp = PathedParams contents input
            let err = MissingMacro macroName
            macro <- mapLeft LayoutDocError $ lookupEither err macroName map
            return $ Build macro mp output
        ll path (LayoutCopy from to) = return $ Copy from to

-- | Loads a list of @SiteAction@ from a file, when given a mapping of strings
--   to macros
loadSiteActions :: MonadReadFile m =>
    FilePath -> H.HashMap T.Text Macro -> LayoutFileResult m [SiteAction]
loadSiteActions path map = do
    layout <- loadLayoutFile path
    liftEither $ mapM (layoutToAction map) layout

-- | Determine the files required to evaluate a @SiteAction@.
filesNeeded :: MonadReadFile m => SiteAction -> LayoutFileResult m [FilePath]
filesNeeded (Build m mp f) =
        filesForMacro m mp `mapResultError` LayoutDocError
filesNeeded (Copy from to) = return [from]
