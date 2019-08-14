{-# LANGUAGE OverloadedStrings #-}

module Snippetter.Layout where

import Control.Monad.Except
import Control.Monad.Trans.Except
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import Data.Yaml ((.:))
import qualified Data.Yaml as Y
import Prelude hiding (writeFile)
import Snippetter.Build
import Snippetter.IO
import Snippetter.Utilities

-- | Possible errors when a @SiteAction@ is being made/executed.
data LayoutError
  = LayoutDocError DocError
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
data Layout
  = LayoutBuild FilePath T.Text Params
  | LayoutCopy FilePath FilePath
  | LayoutMove FilePath FilePath
  | LayoutDelete FilePath
  | LayoutRunProcess T.Text T.Text
  deriving (Show, Eq)

instance Y.FromJSON Layout where
  parseJSON =
    Y.withObject "LayoutFile" $ \o -> do
      kind <- o .: T.pack "type"
      case T.unpack kind of
        "macro" ->
          LayoutBuild <$> o .: T.pack "output" <*> o .: T.pack "macro-name" <*>
          o .: T.pack "values"
        "copy" -> LayoutCopy <$> o .: T.pack "from" <*> o .: T.pack "to"
        "move" -> LayoutMove <$> o .: T.pack "from" <*> o .: T.pack "to"
        "delete" -> LayoutDelete <$> o .: T.pack "file"
        "run-process" ->
          LayoutRunProcess <$> o .: T.pack "process" <*> o .: T.pack "stdin"

-- | A Layout value that may have a file path associated with it.
--   If loaded from a file, the path should be assigned when doing so.
--   If defined in a source file, the path should be @Nothing@.
data PathedLayout =
  PathedLayout
    { layout :: Layout
    , lpath :: Maybe FilePath
    }
  deriving (Show, Eq)

-- | Describes an action taken to build the site.
data SiteAction
  = Build Macro PathedParams FilePath
  | Copy FilePath FilePath
  | Move FilePath FilePath
  | Delete FilePath
  | Run T.Text [T.Text] T.Text

-- | Retrieves contents of the specific file, and maps possible errors to
--   @LayoutFileError@s.
fileContentsInLayout :: MonadReadFile m => FilePath -> LayoutFileResult m T.Text
fileContentsInLayout path = getFileContents path `mapResultError` mapping
  where
    mapping = LayoutFileError

-- | Adds a path to a @Layout@.
addPath :: FilePath -> Layout -> PathedLayout
addPath path layout = PathedLayout layout (Just path)

-- | Load a YAML file as a list of @Layout@s.
yamlAsLayout :: MonadReadFile m => FilePath -> LayoutFileResult m [Layout]
yamlAsLayout path =
  let errorMapping = LayoutYamlError
   in (yamlIfExists path :: MonadReadFile m =>
                              YamlResult m [Layout]) `mapResultError`
      errorMapping

-- | Loads a list of pathed layouts from a file.
loadLayoutFile ::
     MonadReadFile m => FilePath -> LayoutFileResult m [PathedLayout]
loadLayoutFile path = do
  y <- yamlAsLayout path
  return $ map (addPath path) y

executeSiteAction' :: MonadWorld m => SiteAction -> LayoutFileResult m ()
executeSiteAction' (Build m pp fp) = do
  executed <- executeMacro m pp `mapResultError` LayoutDocError
  writeFile fp executed `mapResultError` LayoutFileError
executeSiteAction' (Copy from to) =
  copyFile from to `mapResultError` LayoutFileError
executeSiteAction' (Move from to) =
  moveFile from to `mapResultError` LayoutFileError
executeSiteAction' (Delete file) =
  deleteFile file `mapResultError` LayoutFileError
executeSiteAction' (Run process args stdin) =
  packRunProcess process args stdin `mapResultError` LayoutFileError

siteActionTenses :: SiteAction -> (T.Text, T.Text, T.Text)
siteActionTenses (Build _ _ _) = ("Building", "built", "build")
siteActionTenses (Copy _ _) = ("Copying", "copied", "copy")
siteActionTenses (Move _ _) = ("Moving", "moved", "move")
siteActionTenses (Delete _) = ("Deleting", "deleted", "delete")
siteActionTenses (Run _ _ _) = ("Running", "ran", "run")

siteActionName :: SiteAction -> T.Text
siteActionName (Build _ _ fp) = T.pack fp
siteActionName (Delete file) = T.pack file
siteActionName (Copy from to) = T.pack from <> "\" to \"" <> T.pack to
siteActionName (Move from to) = T.pack from <> "\" to \"" <> T.pack to
siteActionName (Run process args _) = process <> " " <> T.intercalate " " args

executeSiteAction :: MonadWorld m => SiteAction -> m ()
executeSiteAction sa = do
  let name = siteActionName sa
  let (tenseA, tenseB, tenseC) = siteActionTenses sa
  notifyProgress $ tenseA <> " \"" <> name <> "\"..."
  result <- runExceptT $ executeSiteAction' sa
  case result of
    Right r ->
      notifySuccess $ "Successfully " <> tenseB <> " \"" <> name <> "\"."
    Left l -> notifyFailure $ "Failed to " <> tenseC <> " \"" <> name <> "\":"

executeLayoutFile :: MonadWorld m => FilePath -> H.HashMap T.Text Macro -> m ()
executeLayoutFile path map = do
  actions <- runExceptT $ loadSiteActions path map
  case actions of
    Right r -> mapM_ executeSiteAction r
    Left l -> notifyFailure $ "Failed to load \"" <> T.pack path <> "\"."

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
    ll path (LayoutMove from to) = return $ Move from to
    ll path (LayoutDelete file) = return $ Delete file
    ll path (LayoutRunProcess process stdin) = return $ Run pHead pTail stdin
      where
        split = T.splitOn " " process
        pHead = head split
        pTail = tail split

-- | Loads a list of @SiteAction@ from a file, when given a mapping of strings
--   to macros
loadSiteActions ::
     MonadReadFile m
  => FilePath
  -> H.HashMap T.Text Macro
  -> LayoutFileResult m [SiteAction]
loadSiteActions path map = do
  layout <- loadLayoutFile path
  liftEither $ mapM (layoutToAction map) layout

-- | Determine the files required to evaluate a @SiteAction@.
filesNeeded :: MonadReadFile m => SiteAction -> LayoutFileResult m [FilePath]
filesNeeded (Build m mp f) = filesForMacro m mp `mapResultError` LayoutDocError
filesNeeded (Copy from to) = return [from]
