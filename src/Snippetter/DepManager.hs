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
    , actions :: [SiteAction]
    , outputToAction :: HM.HashMap FilePath SiteAction
    }

-- | Map output files to their siteactions.
actionMap :: [SiteAction] -> HM.HashMap FilePath SiteAction
actionMap = foldr foo HM.empty
  where
    foo a b =
      case saOutputFile a of
        Nothing -> b
        Just c -> HM.insert c a b

-- | Add entries to a @FileGraph@ based on a @SiteAction@ and its dependencies.
addDependencies ::
     MonadReadWorld m
  => SiteAction
  -> FG.FileGraph
  -> DepManResult m FG.FileGraph
addDependencies sa graph = do
  deps <- saNeededFiles sa `mapResultError` LayoutDepManError
  return $
    case saOutputFile sa of
      Nothing -> FG.addFiles deps graph
      Just a -> FG.addParents a deps graph

-- | Check if the @SiteAction@'s output file is up to date.
outputUpToDate ::
     MonadReadWorld m => FG.FileGraph -> SiteAction -> DepManResult m Bool
outputUpToDate graph sa =
  case saOutputFile sa of
    Just a -> FG.isUpToDate a graph `mapResultError` GraphDepManError
    Nothing -> return True

-- | Check if the @SiteAction@'s dependencies are up to date.
depsUpToDate ::
     MonadReadWorld m => FG.FileGraph -> SiteAction -> DepManResult m Bool
depsUpToDate graph sa = do
  deps <- saNeededFiles sa `mapResultError` LayoutDepManError
  FG.areUpToDate deps graph `mapResultError` GraphDepManError

-- | Check if a @SiteAction@ should be updated. This is the case when its
-- output file is not up to date but its dependencies are.
shouldUpdateSiteAction ::
     MonadReadWorld m => FG.FileGraph -> SiteAction -> DepManResult m Bool
shouldUpdateSiteAction graph sa = do
  o <- outputUpToDate graph sa
  d <- depsUpToDate graph sa
  return (not o && d)

-- | All @SiteAction@s that need to be updated according to the graph.
actionsToUpdate ::
     MonadReadWorld m
  => FG.FileGraph
  -> [SiteAction]
  -> DepManResult m [SiteAction]
actionsToUpdate graph = filterM (shouldUpdateSiteAction graph)

-- | Builds dependency information from the specified 'SiteAction's'.
getDepInfos ::
     MonadReadWorld m => [SiteAction] -> DepManResult m DepInfo
getDepInfos actions = do
  graph <- graphFromActions actions
  return $ DepInfo graph actions (actionMap actions)

-- | Create a @FileGraph@ based on a @[SiteAction]@.
graphFromActions ::
     MonadReadWorld m => [SiteAction] -> DepManResult m FG.FileGraph
graphFromActions actions = do
  graph <- foldM (flip addDependencies) FG.empty actions
  let cycles = FG.getSCC graph
  if not (null cycles)
    then resultE $ CyclicFileGraph cycles
    else return graph

-- | Execute all @SiteAction@s resulting from a layout file if their outputs
-- aren't up to date but their dependencies are.
updateLayoutFile :: MonadWriteWorld m => BuilderMap -> FilePath -> m ()
updateLayoutFile map file = updateLayout map [file]

-- | Execute all @SiteAction@s resulting from the layout files if their outputs
-- aren't up to date but their dependencies are.
updateLayout :: MonadWriteWorld m => BuilderMap -> [FilePath] -> m ()
updateLayout map files =
  profileWorldAction $ whenResult load updateActions
  where
    load = loadLayoutFiles map files `mapResultError` LayoutDepManError

-- | Execute the @SiteAction@s if their outputs aren't up to date but their
-- dependencies are.
updateActions :: MonadWriteWorld m => [SiteAction] -> m ()
updateActions actions =
  whenResult
    (profileResult "Determining dependencies..." $
     getDepInfos actions) $
  determineRoots actions

-- | Show the user the 'SiteAction's that aren't up to date but whose
-- dependencies are.
showActionsNeeded :: MonadWriteWorld m => [SiteAction] -> m ()
showActionsNeeded = actOnAllChildren showActions

-- | Preview the user the 'SiteAction's that aren't up to date but whose
-- dependencies are.
previewActionsNeeded :: MonadWriteWorld m => [SiteAction] -> m ()
previewActionsNeeded = actOnAllChildren previewActions

-- | Show the user the dependencies of the 'SiteAction's that aren't up to date
-- but whose dependencies are.
showActionsDepsNeeded :: MonadWriteWorld m => [SiteAction] -> m ()
showActionsDepsNeeded = actOnAllChildren showActionsDeps

-- | Show the user the output files of the 'SiteAction's that aren't up to date
-- but whose dependencies are.
showActionsOutputNeeded :: MonadWriteWorld m => [SiteAction] -> m ()
showActionsOutputNeeded = actOnAllChildren showActionsOutput

-- | Show the user the 'SiteAction's resulting from the layout file that aren't
-- up to date but whose dependencies are.
showLayoutNeeded ::
     MonadWriteWorld m => BuilderMap -> [FilePath] -> m ()
showLayoutNeeded = actOnLayoutFiles showActions

-- | Preview the user the 'SiteAction's resulting from the layout file that
-- aren't up to date but whose dependencies are.
previewLayoutNeeded ::
     MonadWriteWorld m => BuilderMap -> [FilePath] -> m ()
previewLayoutNeeded = actOnLayoutFiles previewActions

-- | Show the user the dependencies of the 'SiteAction's resulting from the
-- layout file that aren't up to date but whose dependencies are.
showLayoutDepsNeeded ::
     MonadWriteWorld m => BuilderMap -> [FilePath] -> m ()
showLayoutDepsNeeded = actOnLayoutFiles showActionsDeps

-- | Show the user the output files of the 'SiteAction's resulting from the
-- layout file that aren't up to date but whose dependencies are.
showLayoutOutputNeeded ::
     MonadWriteWorld m => BuilderMap -> [FilePath] -> m ()
showLayoutOutputNeeded = actOnLayoutFiles showActionsOutput

-- | Execute the given operation on all @SiteAction@s resulting from a layout
-- file. This differs from the "Snippetter.Layout" equivalent in that it only
-- does the action on the 'SiteAction's' that would be updated.
actOnLayoutFiles ::
     MonadWriteWorld m => ([SiteAction] -> m ()) -> BuilderMap -> [FilePath] -> m ()
actOnLayoutFiles act map path = do
  actions <- runResult $ loadLayoutFiles map path `mapResultError` LayoutDepManError
  case actions of
    Right r -> actOnAllChildren act r
    Left l ->
      notifyFailure $
      "Failed to load site actions:\n" <> indentFour (T.pack $ show l)

-- | Execute the given action on the specified 'SiteAction's' and all of their
-- children when they're not up to date.
actOnAllChildren ::
     MonadWriteWorld m => ([SiteAction] -> m ()) -> [SiteAction] -> m ()
actOnAllChildren act actions = whenResult (getAllChildren actions) act

-- | Get the @SiteAction@s that eventually depend on the given 'SiteAction's'
-- that aren't up to date.
getAllChildren ::
     MonadWriteWorld m => [SiteAction] -> DepManResult m [SiteAction]
getAllChildren actions = do
  depInfo <- profileResult "Determining dependencies..." $
      getDepInfos actions
  roots <- profileResult "Determining build order..." $
      actionsToUpdate (graph depInfo) actions
  return $ childrenOfSiteActions depInfo roots

-- | Get the @SiteAction@s that eventually depend on these ones.
childrenOfSiteActions :: DepInfo -> [SiteAction] -> [SiteAction]
childrenOfSiteActions deps actions = mapMaybe lookup $ HS.toList children
  where
    outputs = mapMaybe saOutputFile actions
    children = FG.getAllChildren' (HS.fromList outputs) (graph deps)
    lookup foo = HM.lookup foo (outputToAction deps)

-- | Determines which files are roots and starts updating them.
determineRoots :: MonadWriteWorld m => [SiteAction] -> DepInfo -> m ()
determineRoots actions dep =
  whenResult
    (profileResult "Determining build order..." $
     actionsToUpdate (graph dep) actions) $
  startSiteActionsUpdate dep

data UpdateState =
  UpdateState
    { usQueue :: [SiteAction]
    , usDepInfo :: DepInfo
    }

-- | Execute each @SiteAction@ if it's not up to date and its dependencies are.
startSiteActionsUpdate :: MonadWriteWorld m => DepInfo -> [SiteAction] -> m ()
startSiteActionsUpdate dep actions =
  if null actionRoots
    then notifySuccess "No updates needed."
    else evalStateT statefulUpdate init
  where
    init = UpdateState actionRoots dep
    actionRoots =
      mapMaybe (`HM.lookup` outputToAction dep) $
      HS.toList $
      FG.getRoots (HS.fromList $ mapMaybe saOutputFile actions) (graph dep)

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
  result <- lift $ runResult $ childrenToUpdate deps sa
  case result of
    Left l -> lift $ notifyFailure $ T.pack (show l)
    Right r -> put $ UpdateState (poppedQueue ++ r) deps
  -- repeat if queue is not empty
  finalState <- get
  let finalQueue = usQueue finalState
  unless (null finalQueue) statefulUpdate

-- | Evaluate a @SiteAction@ if it's not up to date and its dependencies are.
updateIfReady :: MonadWriteWorld m => FG.FileGraph -> SiteAction -> m ()
updateIfReady graph sa = do
  shouldUpdate <- runResult $ shouldUpdateSiteAction graph sa
  case shouldUpdate of
    Left l -> notifyFailure $ T.pack (show l)
    Right r -> when r (executeActions [sa])

-- | Get the @SiteAction@s that depend on this one and are able to be updated.
childrenToUpdate ::
     MonadReadWorld m => DepInfo -> SiteAction -> DepManResult m [SiteAction]
childrenToUpdate deps sa =
  case saOutputFile sa of
    Nothing -> return []
    Just f ->
      case FG.getChildren f (graph deps) of
        Nothing -> resultE $ CantFindChildren f
        Just k -> do
          let kids = HS.toList k
          let lookup foo = HM.lookup foo (outputToAction deps)
          filterM (shouldUpdateSiteAction $ graph deps) $ mapMaybe lookup kids
