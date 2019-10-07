{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Contains functions/types related to loading and executing layout files.
module Snippetter.Layout
  ( -- * Results, Errors, and Important Things
    LayoutError
  , LayoutResult
  , BuilderMap
  , emptyBuilderMap
  , insertNamedBuilder
  , insertNamedBuilders
  -- * Site Actions
  , SiteAction (..)
  , PathedSiteAction (..)
  , SiteActionSet
  , extractAction
  , extractPath
  , loadLayoutFile
  , loadLayoutFiles
  , saNeededFiles
  , psaNeededFiles
  , saOutputFile
  , psaOutputFile
  , showActions
  , previewActions
  , showActionsDeps
  , showActionsOutput
  , executeActions
  -- * Layout files
  , showLayout
  , previewLayout
  , showLayoutDeps
  , showLayoutOutput
  , executeLayout
  ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Data.Yaml ((.:))
import Data.Hashable
import Data.List
import Data.Maybe
import qualified Data.Yaml as Y
import Prelude hiding (writeFile)
import Control.Monad
import GHC.Generics
import Snippetter.Build
import Snippetter.IO
import Snippetter.Utilities

-- | Possible errors when a @SiteAction@ is being made/executed.
data LayoutError
  = LayoutDocError DocError
  | LayoutYamlError YamlError
  | LayoutFileError FileError
  | MissingBuilder T.Text
  | MiscLayoutError T.Text
  deriving (Eq)

instance Show LayoutError where
  show (LayoutDocError e) = show e
  show (LayoutYamlError e) = show e
  show (LayoutFileError e) = show e
  show (MissingBuilder t) =
    "The builder \"" <> T.unpack t <> "\" was missing from the builder map."
  show (MiscLayoutError t) =
    "An error occured in the layout processing phase:" <> T.unpack t

-- | The result of a function that makes/executes a @SiteAction@.
type LayoutResult m a = Result LayoutError m a

-- | A map of @T.Text@ to @Builder@s.
type BuilderMap = HM.HashMap T.Text NamedBuilder

-- | Insert a @Builder@ into a @BuilderMap@ as a @NamedBuilder@.
insertNamedBuilder :: T.Text -> Builder -> BuilderMap -> BuilderMap
insertNamedBuilder name builder = HM.insert name (NamedBuilder name builder)

-- | Insert @Builder@s into a @BuilderMap@ as @NamedBuilder@s.
insertNamedBuilders :: [(T.Text, Builder)] -> BuilderMap -> BuilderMap
insertNamedBuilders bindings bmap =
  foldr (uncurry insertNamedBuilder) bmap bindings

-- | Creates an empty @BuilderMap@.
emptyBuilderMap :: BuilderMap
emptyBuilderMap = HM.empty

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
        "build" ->
          LayoutBuild <$> o .: T.pack "output-file" <*>
          o .: T.pack "builder-name" <*>
          o .: T.pack "parameters"
        "copy" -> LayoutCopy <$> o .: T.pack "from" <*> o .: T.pack "to"
        "move" -> LayoutMove <$> o .: T.pack "from" <*> o .: T.pack "to"
        "delete" -> LayoutDelete <$> o .: T.pack "file"
        "run-process" ->
          LayoutRunProcess <$> o .: T.pack "process" <*> o .: T.pack "stdin"
        _ -> fail "not a valid layout file type"

-- | A Layout value that may have a file path associated with it.
-- If loaded from a file, the path should be assigned when doing so.
-- If defined in a source file, the path should be @Nothing@.
data PathedLayout
  = PathedLayout Layout (Maybe FilePath)
  deriving (Show, Eq)

-- | Describes an action taken to build the site.
data SiteAction
  = Build NamedBuilder PathedParams FilePath
  | Copy FilePath FilePath
  | Move FilePath FilePath
  | Delete FilePath
  | Run T.Text [T.Text] T.Text
  deriving (Show, Eq)

instance Hashable SiteAction where
    hashWithSalt salt (Build nb pp fp) = salt `hashWithSalt` 
        (0 :: Int) `hashWithSalt` fp
    hashWithSalt salt (Copy from to) = salt `hashWithSalt` 
        (1 :: Int) `hashWithSalt` from `hashWithSalt` to
    hashWithSalt salt (Move from to) = salt `hashWithSalt` 
        (2 :: Int) `hashWithSalt` from `hashWithSalt` to
    hashWithSalt salt (Delete file) = salt `hashWithSalt` 
        (3 :: Int) `hashWithSalt` file
    hashWithSalt salt (Run process _ _) = salt `hashWithSalt` 
        (4 :: Int) `hashWithSalt` process

-- | A 'SiteAction' that may have a file path associated with it, which
-- represents where it came from.
-- If loaded from a file, the path should be assigned when doing so.
-- If defined in a source file, the path should be @Nothing@.
data PathedSiteAction
  = PathedSiteAction SiteAction (Maybe FilePath)
  deriving (Show, Eq, Generic)

type SiteActionSet = HS.HashSet PathedSiteAction

instance Hashable PathedSiteAction

-- | Extract the 'SiteAction' from a 'PathedSiteAction'.
extractAction :: PathedSiteAction -> SiteAction
extractAction (PathedSiteAction sa _) = sa

-- | Extract the 'Maybe FilePath' from a 'PathedSiteAction'.
extractPath :: PathedSiteAction -> Maybe FilePath
extractPath (PathedSiteAction _ f) = f

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
   in (yamlIfExists "List of layouts" path :: MonadReadWorld m =>
                                                YamlResult m [Layout]) `mapResultError`
      errorMapping

-- | Loads a list of pathed layouts from a file.
loadLayoutFileRaw :: MonadReadWorld m => FilePath -> LayoutResult m [PathedLayout]
loadLayoutFileRaw path = do
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
siteActionDesc :: SiteAction -> T.Text
siteActionDesc (Build _ _ fp) = "Building \"" <> T.pack fp <> "\""
siteActionDesc (Copy from to) =
  "Copying \"" <> T.pack from <> "\" to \"" <> T.pack to <> "\""
siteActionDesc (Move from to) =
  "Moving \"" <> T.pack from <> "\" to \"" <> T.pack to <> "\""
siteActionDesc (Delete file) = "Deleting \"" <> T.pack file <> "\""
siteActionDesc (Run process args _) =
  "Running \"" <> process <> " " <> T.intercalate " " args <> "\""

-- | Show the user the dependencies of these 'SiteAction's.
showActionsDeps :: MonadWriteWorld m => [PathedSiteAction] -> m ()
showActionsDeps pathedActions = do
  result <- runResult $ mapM psaNeededFiles pathedActions
  case result of
    Right r -> do
        let deps = foldr HS.union HS.empty r
        notifyInfo $ "Dependencies: " <>
            T.pack (intercalate ", " $ HS.toList deps) <> "\n"
    Left l -> 
      notifyFailure $
      "Failed to display dependencies :\n" <> indentFour (T.pack $ show l)

-- | Show the user the output files of these 'SiteAction's.
showActionsOutput :: MonadWriteWorld m => [PathedSiteAction] -> m ()
showActionsOutput pathedActions =
  notifyInfo $ "Outputs: " <> outputs
    where
      outputs = T.intercalate ", " (map T.pack justActions) <> "\n"
      justActions = mapMaybe saOutputFile actions
      actions = map extractAction pathedActions

-- | Show the user these 'SiteAction's.
showActions :: MonadWriteWorld m => [PathedSiteAction] -> m ()
showActions pathedActions = do
  let actions = map extractAction pathedActions
  result <- runResult $ mapM saShow actions
  case result of
    Right r ->
        notifyInfo $ indentMultiWithListMarker r
    Left l -> 
      notifyFailure $
      "Failed to display dependencies :\n" <> indentFour (T.pack $ show l)

-- | Show the user previews of these 'SiteAction's.
previewActions :: MonadWriteWorld m => [PathedSiteAction] -> m ()
previewActions pathedActions = do
  let actions = map extractAction pathedActions
  result <- runResult $ mapM saPreview actions
  case result of
    Right r ->
        notifyInfo $ indentMultiWithListMarker r
    Left l -> 
      notifyFailure $
      "Failed to display dependencies :\n" <> indentFour (T.pack $ show l)

-- | Execute a @SiteAction@ and notify the user of the results.
executeActions :: MonadWriteWorld m => [PathedSiteAction] -> m ()
executeActions pathedActions = do
  let perSa sa = profileResult (siteActionDesc sa <> "... ") $ executeSiteAction' sa
  let actions = map extractAction pathedActions
  runResult $ mapM_ perSa actions
  return ()

-- | Execute the given operation on all @SiteAction@s resulting from a layout
-- file.
actOnLayoutFiles ::
     MonadWriteWorld m => ([PathedSiteAction] -> m ()) -> BuilderMap -> [FilePath] -> m ()
actOnLayoutFiles act bmap path = do
  actions <- runResult $ loadLayoutFiles bmap path
  case actions of
    Right r -> act r
    Left l ->
      notifyFailure $
      "Failed to load site actions:\n" <> indentFour (T.pack $ show l)

-- | Load a layout file and execute all @SiteAction@s resulting from it,
-- notifying the user of the results of each.
executeLayout :: MonadWriteWorld m => BuilderMap -> [FilePath] -> m ()
executeLayout = actOnLayoutFiles executeActions

-- | Load a layout file and show the user the dependencies of all
-- @SiteAction@s resulting from it.
showLayoutDeps :: MonadWriteWorld m => BuilderMap -> [FilePath] -> m ()
showLayoutDeps = actOnLayoutFiles showActionsDeps

-- | Load a layout file and show the user the output files of all
-- @SiteAction@s resulting from it.
showLayoutOutput :: MonadWriteWorld m => BuilderMap -> [FilePath] -> m ()
showLayoutOutput = actOnLayoutFiles showActionsOutput

-- | Load a layout file and show the user @SiteAction@s resulting from it.
showLayout :: MonadWriteWorld m => BuilderMap -> [FilePath] -> m ()
showLayout = actOnLayoutFiles showActions

-- | Load a layout file and show the user previews of @SiteAction@s resulting
-- from it.
previewLayout :: MonadWriteWorld m => BuilderMap -> [FilePath] -> m ()
previewLayout = actOnLayoutFiles previewActions

-- | Converts a @PathedLayout@ to a @SiteAction@, when given a mapping of
--   strings to builders.
layoutToAction :: BuilderMap -> PathedLayout -> Either LayoutError PathedSiteAction
layoutToAction map (PathedLayout layout input) = do
    sa <- ll input layout
    return $ PathedSiteAction sa input
  where
    ll path (LayoutBuild output builderName contents) = do
      let mp = PathedParams contents input
      let err = MissingBuilder builderName
      builder <- lookupEither err builderName map
      return $ Build builder mp output
    ll path (LayoutCopy from to) = return $ Copy from to
    ll path (LayoutMove from to) = return $ Move from to
    ll path (LayoutDelete file) = return $ Delete file
    ll path (LayoutRunProcess process stdin) = return $ Run pHead pTail stdin
      where
        split = T.splitOn " " process
        pHead = head split
        pTail = tail split

-- | Loads a list of @SiteAction@ from a layout file.
loadLayoutFile ::
     MonadReadWorld m => BuilderMap -> FilePath -> LayoutResult m [PathedSiteAction]
loadLayoutFile map path = do
  layout <- loadLayoutFileRaw path
  resultLiftEither $ mapM (layoutToAction map) layout

-- | Loads a list of @SiteAction@ from multiple layout files.
loadLayoutFiles ::
     MonadReadWorld m => BuilderMap -> [FilePath] -> LayoutResult m [PathedSiteAction]
loadLayoutFiles bmap paths = do
    actions <- mapM (loadLayoutFile bmap) paths
    return $ concat actions

-- | Determine files needed to execute a @SiteAction@.
saNeededFiles :: MonadReadWorld m => SiteAction -> LayoutResult m FilePathSet
saNeededFiles (Build m pp f) =
  filesForBuilder m pp `mapResultError` LayoutDocError
saNeededFiles (Copy from to) = return $ HS.singleton from
saNeededFiles (Move from to) = return $ HS.singleton from
saNeededFiles (Delete file) = return $ HS.singleton file
saNeededFiles (Run process args stdin) = return HS.empty

-- | Determine files needed to execute a 'PathedSiteAction'.
psaNeededFiles :: MonadReadWorld m => PathedSiteAction -> LayoutResult m FilePathSet
psaNeededFiles (PathedSiteAction sa path) =
  case path of
    Nothing -> saNeededFiles sa
    Just a -> do
        needed <- saNeededFiles sa
        return $ HS.insert a needed

-- | Determine the output file of a @SiteAction@.
saOutputFile :: SiteAction -> Maybe FilePath
saOutputFile (Build m pp f) = Just f
saOutputFile (Copy from to) = Just to
saOutputFile (Move from to) = Just to
saOutputFile (Delete file) = Nothing
saOutputFile (Run process args stdin) = Nothing

-- | Determine files needed to execute a 'PathedSiteAction'.
psaOutputFile :: PathedSiteAction -> Maybe FilePath
psaOutputFile (PathedSiteAction sa path) =
  saOutputFile sa

saShow :: MonadReadWorld m => SiteAction -> LayoutResult m T.Text
saShow (Build m pp f) = showBuilder m pp `mapResultError` LayoutDocError
saShow (Copy from to) = return $ "Copy \"" <> T.pack from <> "\" to \"" <> T.pack to <> "\""
saShow (Move from to) = return $ "Move \"" <> T.pack from <> "\" to \"" <> T.pack to <> "\""
saShow (Delete file) = return $ "Delete \"" <> T.pack file <> "\""
saShow (Run process args stdin) = return $ "Run \"" <> process <> " " <> T.intercalate " " args <> "\""

saPreview :: MonadReadWorld m => SiteAction -> LayoutResult m T.Text
saPreview (Build m pp f) =
  previewBuilder m pp `mapResultError` LayoutDocError
saPreview sa = saShow sa
