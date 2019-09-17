{-# LANGUAGE OverloadedStrings #-}

-- | Contains functions to load and execute layout files based on file
-- dependencies.
module Snippetter.DepManager where

import Control.Monad
import Control.Monad.State.Lazy
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe
import qualified Data.Text as T
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

type DepManResult m a = Result DepManError m a

-- | Information of site actions and their dependencies.
data DepInfo =
  DepInfo
    { graph :: FG.FileGraph
    , actions :: [SiteAction]
    , outputToAction :: HM.HashMap FilePath SiteAction
    , layoutFiles :: [FilePath]
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
      Just a -> FG.addChildren a deps graph

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

-- | Builds dependency information from the specified layout files.
getDepInfosSilent ::
     MonadReadWorld m => [FilePath] -> BuilderMap -> DepManResult m DepInfo
getDepInfosSilent files map = do
  actionsRaw <-
    mapM (`loadSiteActions` map) files `mapResultError` LayoutDepManError
  let actions = concat actionsRaw
  graph <- graphFromActions actions
  return $ DepInfo graph actions (actionMap actions) files

-- | Builds dependency information from the specified layout files, outputting
-- the results of its steps as it goes.
getDepInfos ::
     MonadWriteWorld m => [FilePath] -> BuilderMap -> DepManResult m DepInfo
getDepInfos files map = do
  actionsRaw <-
    profileResult "Loading layout files..." $
    mapM (`loadSiteActions` map) files `mapResultError` LayoutDepManError
  let actions = concat actionsRaw
  graph <-
    profileResult "Determining dependencies..." $ graphFromActions actions
  return $ DepInfo graph actions (actionMap actions) files

-- | Builds dependency information from a layout file, outputting the results
-- of its steps as it goes.
getDepInfo ::
     MonadWriteWorld m => FilePath -> BuilderMap -> DepManResult m DepInfo
getDepInfo layoutFile = getDepInfos [layoutFile]

-- | Builds dependency information from a layout file.
getDepInfoSilent ::
     MonadReadWorld m => FilePath -> BuilderMap -> DepManResult m DepInfo
getDepInfoSilent layoutFile = getDepInfosSilent [layoutFile]

-- | Create a @FileGraph@ based on a @[SiteAction]@.
graphFromActions ::
     MonadReadWorld m => [SiteAction] -> DepManResult m FG.FileGraph
graphFromActions actions = do
  graph <- foldM (flip addDependencies) FG.empty actions
  let cycles = FG.getSCC graph
  if not (null cycles)
    then resultE $ CyclicFileGraph cycles
    else return graph

-- | Update all @SiteAction@s resulting from a layout file.
updateLayoutFile :: MonadWriteWorld m => FilePath -> BuilderMap -> m ()
updateLayoutFile layoutFile map = whenResult (getDepInfo layoutFile map) act
  where
    act r = updateNeededSiteActions r (actions r)

-- | Update all @SiteAction@s resulting from layout files.
updateLayoutFiles :: MonadWriteWorld m => [FilePath] -> BuilderMap -> m ()
updateLayoutFiles files map = whenResult (getDepInfos files map) act
  where
    act r = updateNeededSiteActions r (actions r)

-- | Executes @updateSiteActions@ only on files who have not yet updated.
updateNeededSiteActions :: MonadWriteWorld m => DepInfo -> [SiteAction] -> m ()
updateNeededSiteActions dep actions =
  whenResult
    (profileResult "Determining build order..." $
     actionsToUpdate (graph dep) actions) $
  updateSiteActions dep

-- | Evaluate a @SiteAction@ if it's not up to date and its dependencies are.
updateSiteAction :: MonadWriteWorld m => FG.FileGraph -> SiteAction -> m ()
updateSiteAction graph sa = do
  shouldUpdate <- runResult $ shouldUpdateSiteAction graph sa
  case shouldUpdate of
    Left l -> notifyFailure $ T.pack (show l)
    Right r -> when r (executeSiteAction sa)

data UpdateState =
  UpdateState
    { usQueue :: [SiteAction]
    , usDepInfo :: DepInfo
    }

-- | Execute each @SiteAction@ if it's not up to date and its dependencies are.
updateSiteActions :: MonadWriteWorld m => DepInfo -> [SiteAction] -> m ()
updateSiteActions dep actions = evalStateT statefulUpdate init
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
  -- | update site action
  lift $ updateSiteAction (graph deps) sa
  -- determine which children need updating, and add to queue
  result <- lift $ runResult $ childrenToUpdate deps sa
  case result of
    Left l -> lift $ notifyFailure $ T.pack (show l)
    Right r -> put $ UpdateState (poppedQueue ++ r) deps
  -- repeat if queue is not empty
  finalState <- get
  let finalQueue = usQueue finalState
  unless (null finalQueue) statefulUpdate

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
