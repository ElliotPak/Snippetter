{-# LANGUAGE OverloadedStrings #-}

-- | Contains functions/types related to loading and executing layout files.
module Snippetter.Layout where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
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
  deriving (Eq)

instance Show LayoutError where
  show (LayoutDocError e) = "While building a page:\n" <> indentFourStr (show e)
  show (LayoutYamlError e) = "While decoding YAML:\n" <> indentFourStr (show e)
  show (LayoutFileError e) = "While reading a file:\n" <> indentFourStr (show e)
  show (MiscLayoutError t) = "An error occured:" <> T.unpack t

-- | The result of a function that makes/executes a @SiteAction@ and can read files.
type LayoutResult m a = Result LayoutError m a

-- | A map of @T.Text@ to @Builder@s.
type BuilderMap = HM.HashMap T.Text Builder

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
        "builder" ->
          LayoutBuild <$> o .: T.pack "output" <*> o .: T.pack "builder-name" <*>
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
  = Build Builder PathedParams FilePath
  | Copy FilePath FilePath
  | Move FilePath FilePath
  | Delete FilePath
  | Run T.Text [T.Text] T.Text

-- | Retrieves contents of the specific file, and maps possible errors to
--   @LayoutFileError@s.
fileContentsInLayout :: MonadReadWorld m => FilePath -> LayoutResult m T.Text
fileContentsInLayout path = getFileContents path `mapResultError` mapping
  where
    mapping = LayoutFileError

-- | Adds a path to a @Layout@.
addPath :: FilePath -> Layout -> PathedLayout
addPath path layout = PathedLayout layout (Just path)

-- | Load a YAML file as a list of @Layout@s.
yamlAsLayout :: MonadReadWorld m => FilePath -> LayoutResult m [Layout]
yamlAsLayout path =
  let errorMapping = LayoutYamlError
   in (yamlIfExists path :: MonadReadWorld m =>
                              YamlResult m [Layout]) `mapResultError`
      errorMapping

-- | Loads a list of pathed layouts from a file.
loadLayoutFile :: MonadReadWorld m => FilePath -> LayoutResult m [PathedLayout]
loadLayoutFile path = do
  y <- yamlAsLayout path
  return $ map (addPath path) y

-- | Actually executes the @SiteAction@ and returns the result for it.
executeSiteAction' :: MonadWriteWorld m => SiteAction -> LayoutResult m ()
executeSiteAction' (Build m pp fp) = do
  executed <- executeBuilder m pp `mapResultError` LayoutDocError
  writeFile fp executed `mapResultError` LayoutFileError
executeSiteAction' (Copy from to) =
  copyFile from to `mapResultError` LayoutFileError
executeSiteAction' (Move from to) =
  moveFile from to `mapResultError` LayoutFileError
executeSiteAction' (Delete file) =
  deleteFile file `mapResultError` LayoutFileError
executeSiteAction' (Run process args stdin) =
  packRunProcess process args stdin `mapResultError` LayoutFileError

-- | Get the tense to be used in status messages for each @SiteAction@.
siteActionTenses :: SiteAction -> (T.Text, T.Text, T.Text)
siteActionTenses (Build _ _ _) = ("Building", "built", "build")
siteActionTenses (Copy _ _) = ("Copying", "copied", "copy")
siteActionTenses (Move _ _) = ("Moving", "moved", "move")
siteActionTenses (Delete _) = ("Deleting", "deleted", "delete")
siteActionTenses (Run _ _ _) = ("Running", "ran", "run")

-- | Get the subject to be used in status messages for each @SiteAction@.
siteActionName :: SiteAction -> T.Text
siteActionName (Build _ _ fp) = T.pack fp
siteActionName (Delete file) = T.pack file
siteActionName (Copy from to) = T.pack from <> "\" to \"" <> T.pack to
siteActionName (Move from to) = T.pack from <> "\" to \"" <> T.pack to
siteActionName (Run process args _) = process <> " " <> T.intercalate " " args

-- | Execute a @SiteAction@ and notify the user of the results.
executeSiteAction :: MonadWriteWorld m => SiteAction -> m ()
executeSiteAction sa = do
  let name = siteActionName sa
  let (tenseA, tenseB, tenseC) = siteActionTenses sa
  notifyProgress $ tenseA <> " \"" <> name <> "\"..."
  result <- runResult $ executeSiteAction' sa
  case result of
    Right r ->
      notifySuccess $ "Successfully " <> tenseB <> " \"" <> name <> "\"."
    Left l ->
      notifyFailure $
      "Failed to " <>
      tenseC <> " \"" <> name <> "\":\n" <> indentFour (T.pack $ show l)

actOnLayoutFile ::
     MonadWriteWorld m => (SiteAction -> m ()) -> FilePath -> BuilderMap -> m ()
actOnLayoutFile act path map = do
  actions <- runResult $ loadSiteActions path map
  case actions of
    Right r -> mapM_ act r
    Left l ->
      notifyFailure $
      "Failed to load \"" <>
      T.pack path <> "\":\n" <> indentFour (T.pack $ show l)

-- | Load a layout file and execute all @SiteAction@s resulting from it.
executeLayoutFile :: MonadWriteWorld m => FilePath -> BuilderMap -> m ()
executeLayoutFile = actOnLayoutFile executeSiteAction

-- | Converts a @PathedLayout@ to a @SiteAction@, when given a mapping of
--   strings to builders.
layoutToAction :: BuilderMap -> PathedLayout -> Either LayoutError SiteAction
layoutToAction map (PathedLayout layout input) = ll input layout
  where
    ll path (LayoutBuild output builderName contents) = do
      let mp = PathedParams contents input
      let err = MissingBuilder builderName
      builder <- mapLeft LayoutDocError $ lookupEither err builderName map
      return $ Build builder mp output
    ll path (LayoutCopy from to) = return $ Copy from to
    ll path (LayoutMove from to) = return $ Move from to
    ll path (LayoutDelete file) = return $ Delete file
    ll path (LayoutRunProcess process stdin) = return $ Run pHead pTail stdin
      where
        split = T.splitOn " " process
        pHead = head split
        pTail = tail split

-- | Loads a list of @SiteAction@ from a file, when given a mapping of strings
--   to builders
loadSiteActions ::
     MonadReadWorld m => FilePath -> BuilderMap -> LayoutResult m [SiteAction]
loadSiteActions path map = do
  layout <- loadLayoutFile path
  resultLiftEither $ mapM (layoutToAction map) layout

-- | Determine files needed to execute a @SiteAction@.
saNeededFiles :: MonadReadWorld m => SiteAction -> LayoutResult m FilePathSet
saNeededFiles (Build m pp f) =
  filesForBuilder m pp `mapResultError` LayoutDocError
saNeededFiles (Copy from to) = return $ HS.singleton from
saNeededFiles (Move from to) = return $ HS.singleton from
saNeededFiles (Delete file) = return $ HS.singleton file
saNeededFiles (Run process args stdin) = return HS.empty

-- | Determine the output file of a @SiteAction@.
saOutputFile :: SiteAction -> Maybe FilePath
saOutputFile (Build m pp f) = Just f
saOutputFile (Copy from to) = Just to
saOutputFile (Move from to) = Just to
saOutputFile (Delete file) = Nothing
saOutputFile (Run process args stdin) = Nothing
