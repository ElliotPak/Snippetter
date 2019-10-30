{-# LANGUAGE OverloadedStrings #-}

-- | Contains functions to load and execute layout files based on file
-- dependencies.
module Snippetter.DepManager 
  ( -- * Site Actions
    updateActions
  , showActionsNeeded
  , previewActionsNeeded
  , showActionsDepsNeeded
  , showActionsOutputNeeded
    -- * Layout files
  , updateLayout
  , showLayoutNeeded
  , previewLayoutNeeded
  , showLayoutDepsNeeded
  , showLayoutOutputNeeded
  ) where

import Control.Monad
import Control.Monad.State.Lazy
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe
import qualified Data.Text as T
import Debug.Trace
import Snippetter.Build
import qualified Snippetter.FileGraph as FG
import Snippetter.IO
import Snippetter.Layout
import Snippetter.Utilities

-- | Possible errors of a dependency manager operation.
data DepManError
  = LayoutDepManError LayoutError
  | FileDepManError FileError
  | GraphDepManError FG.GraphError
  | CyclicFileGraph [FG.SCComponent]
  | CantFindChildren FilePath
  deriving (Eq)

instance Show DepManError where
  show (LayoutDepManError e) = show e
  show (FileDepManError e) = show e
  show (GraphDepManError e) = show e
  show (CyclicFileGraph g) =
    "The following cyclic dependencies were found: \n" <>
    T.unpack (FG.showSCC g)
  show (CantFindChildren f) =
    "Somehow can't find the children of \"" <>
    f <> "\". Maybe the FileGraph didn't build correctly somehow?"

-- | The result of a dependency manager operation.
type DepManResult m a = Result DepManError m a

-- | Information of site actions and their dependencies.
data DepInfo =
  DepInfo
    { graph :: FG.FileGraph
    , actions :: [PathedSiteAction]
    , outputToAction :: HM.HashMap FilePath PathedSiteAction
    }

-- | Map output files to their siteactions.
actionMap :: [PathedSiteAction] -> HM.HashMap FilePath PathedSiteAction
actionMap = foldr foo HM.empty
  where
    foo a b =
      case psaOutputFile a of
        Nothing -> b
        Just c -> HM.insert c a b

-- | All @SiteAction@s that need to be updated according to the graph.
actionsToUpdate ::
     MonadReadWorld m
  => FG.FileGraph
  -> [PathedSiteAction]
  -> DepManResult m [PathedSiteAction]
actionsToUpdate graph actions =
  filterM (FG.shouldUpdateSiteAction graph) actions
    `mapResultError` GraphDepManError

-- | Builds dependency information from the specified 'SiteAction's.
getDepInfos ::
     MonadReadWorld m => [PathedSiteAction] -> DepManResult m DepInfo
getDepInfos actions = do
  graph <- graphFromActions actions
  return $ DepInfo graph actions (actionMap actions)

-- | Create a @FileGraph@ based on a @[SiteAction]@.
graphFromActions ::
     MonadReadWorld m => [PathedSiteAction] -> DepManResult m FG.FileGraph
graphFromActions pathedActions = do
  let actions = map extractAction pathedActions
  graph <- foldM (flip FG.addSiteAction) FG.empty pathedActions
                `mapResultError` GraphDepManError
  let cycles = FG.getSCC graph
  if not (null cycles)
    then resultE $ CyclicFileGraph cycles
    else return graph

-- | Execute all @SiteAction@s resulting from a layout file if their outputs
-- aren't up to date but their dependencies are.
updateLayoutFile :: MonadWriteWorld m => BuilderMap m -> FilePath -> m ()
updateLayoutFile map file = updateLayout map [file]

-- | Execute all @SiteAction@s resulting from the layout files if their outputs
-- aren't up to date but their dependencies are.
updateLayout :: MonadWriteWorld m => BuilderMap m -> [FilePath] -> m ()
updateLayout map files =
  profileWorldAction $ whenResult load updateActions
  where
    load = loadLayoutFiles map files `mapResultError` LayoutDepManError

-- | Execute the @SiteAction@s if their outputs aren't up to date but their
-- dependencies are.
updateActions :: MonadWriteWorld m => [PathedSiteAction] -> m ()
updateActions actions =
  whenResult
    (profileResult "Determining dependencies..." $
     getDepInfos actions) $
  determineRoots actions

-- | Show the user the 'SiteAction's that aren't up to date but whose
-- dependencies are.
showActionsNeeded :: MonadWriteWorld m => [PathedSiteAction] -> m ()
showActionsNeeded = actOnAllChildren showActions

-- | Preview the user the 'SiteAction's that aren't up to date but whose
-- dependencies are.
previewActionsNeeded :: MonadWriteWorld m => [PathedSiteAction] -> m ()
previewActionsNeeded = actOnAllChildren previewActions

-- | Show the user the dependencies of the 'SiteAction's that aren't up to date
-- but whose dependencies are.
showActionsDepsNeeded :: MonadWriteWorld m => [PathedSiteAction] -> m ()
showActionsDepsNeeded = actOnAllChildren showActionsDeps

-- | Show the user the output files of the 'SiteAction's that aren't up to date
-- but whose dependencies are.
showActionsOutputNeeded :: MonadWriteWorld m => [PathedSiteAction] -> m ()
showActionsOutputNeeded = actOnAllChildren showActionsOutput

-- | Show the user the 'SiteAction's resulting from the layout file that aren't
-- up to date but whose dependencies are.
showLayoutNeeded ::
     MonadWriteWorld m => BuilderMap m -> [FilePath] -> m ()
showLayoutNeeded = actOnLayoutFiles showActions

-- | Preview the user the 'SiteAction's resulting from the layout file that
-- aren't up to date but whose dependencies are.
previewLayoutNeeded ::
     MonadWriteWorld m => BuilderMap m -> [FilePath] -> m ()
previewLayoutNeeded = actOnLayoutFiles previewActions

-- | Show the user the dependencies of the 'SiteAction's resulting from the
-- layout file that aren't up to date but whose dependencies are.
showLayoutDepsNeeded ::
     MonadWriteWorld m => BuilderMap m -> [FilePath] -> m ()
showLayoutDepsNeeded = actOnLayoutFiles showActionsDeps

-- | Show the user the output files of the 'SiteAction's resulting from the
-- layout file that aren't up to date but whose dependencies are.
showLayoutOutputNeeded ::
     MonadWriteWorld m => BuilderMap m -> [FilePath] -> m ()
showLayoutOutputNeeded = actOnLayoutFiles showActionsOutput

-- | Execute the given operation on all @SiteAction@s resulting from a layout
-- file. This differs from the "Snippetter.Layout" equivalent in that it only
-- does the action on the 'SiteAction's that would be updated.
actOnLayoutFiles ::
     MonadWriteWorld m => ([PathedSiteAction] -> m ()) -> BuilderMap m -> [FilePath] -> m ()
actOnLayoutFiles act map path = do
  actions <- runResult $ loadLayoutFiles map path `mapResultError` LayoutDepManError
  case actions of
    Right r -> actOnAllChildren act r
    Left l ->
      notifyFailure $
      "Failed to load site actions:\n" <> indentFour (T.pack $ show l)

-- | Execute the given action on the specified 'SiteAction's and all of their
-- children when they're not up to date.
actOnAllChildren ::
     MonadWriteWorld m => ([PathedSiteAction] -> m ()) -> [PathedSiteAction] -> m ()
actOnAllChildren act actions = whenResult (getAllChildren actions) act

-- | Get the @SiteAction@s that eventually depend on the given 'SiteAction's
-- that aren't up to date.
getAllChildren ::
     MonadWriteWorld m => [PathedSiteAction] -> DepManResult m [PathedSiteAction]
getAllChildren actions = do
  depInfo <- profileResult "Determining dependencies..." $
      getDepInfos actions
  roots <- profileResult "Determining build order..." $
      actionsToUpdate (graph depInfo) actions
  return $ childrenOfSiteActions depInfo roots

-- | Get the @SiteAction@s that eventually depend on these ones.
childrenOfSiteActions :: DepInfo -> [PathedSiteAction] -> [PathedSiteAction]
childrenOfSiteActions deps actions =
  FG.getAllChildrenActions (HS.fromList actions) (graph deps)

-- | Determines which files are roots and starts updating them.
determineRoots :: MonadWriteWorld m => [PathedSiteAction] -> DepInfo -> m ()
determineRoots actions dep =
  whenResult
    (profileResult "Determining build order..." $
     actionsToUpdate (graph dep) actions) $
  startSiteActionsUpdate dep

data UpdateState =
  UpdateState
    { usQueue :: [PathedSiteAction]
    , usDepInfo :: DepInfo
    }

-- | Execute each @SiteAction@ if it's not up to date and its dependencies are.
startSiteActionsUpdate :: MonadWriteWorld m => DepInfo -> [PathedSiteAction] -> m ()
startSiteActionsUpdate dep actions =
  if null actionRoots
    then notifySuccess "No updates needed."
    else evalStateT statefulUpdate init
  where
    init = UpdateState actionRoots dep
    actionRoots =
      HS.toList $ FG.getRootsOfActions (HS.fromList actions) (graph dep)

-- | Update the @SiteAction@ at the head of the queue and add the children that
-- need updating to the end. Repeats itself until nothing remains in the queue.
statefulUpdate :: MonadWriteWorld m => StateT UpdateState m ()
statefulUpdate = do
  state <- get
  let queue = usQueue state
  let deps = usDepInfo state
  -- pop from queue
  let (sa, poppedQueue) = (head queue, tail queue)
  put $ UpdateState poppedQueue deps
  -- update site action
  lift $ updateIfReady (graph deps) sa
  -- determine which children need updating, and add to queue
  result <- lift $ runResult $ FG.childrenToUpdate sa (graph deps)
  case result of
    Left l -> lift $ notifyFailure $ T.pack (show l)
    Right r -> put $ UpdateState (poppedQueue ++ r) deps
  -- repeat if queue is not empty
  finalState <- get
  let finalQueue = usQueue finalState
  unless (null finalQueue) statefulUpdate

-- | Evaluate a @SiteAction@ if it's not up to date and its dependencies are.
updateIfReady :: MonadWriteWorld m => FG.FileGraph -> PathedSiteAction -> m ()
updateIfReady graph sa = do
  shouldUpdate <- runResult $
    FG.shouldUpdateSiteAction graph sa `mapResultError` GraphDepManError
  case shouldUpdate of
    Left l -> notifyFailure $ T.pack (show l)
    Right r -> when r (executeActions [sa])
