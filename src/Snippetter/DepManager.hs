-- | Contains functions to load and execute layout files based on file
-- dependencies.
module Snippetter.DepManager where

import Control.Monad
import qualified Data.Text as T
import Snippetter.Build
import qualified Snippetter.FileGraph as FG
import Snippetter.IO
import Snippetter.Layout
import Snippetter.Utilities

data DepManError
  = LayoutDepManError LayoutError
  | FileDepManError FileError
  | GraphDepManError FG.GraphError
  | CyclicFileGraph [FG.SCComponent]
  deriving (Eq)

instance Show DepManError where
  show (LayoutDepManError e) = show e
  show (FileDepManError e) = show e
  show (GraphDepManError e) = show e
  show (CyclicFileGraph g) =
    "The following cyclic dependencies were found: \n" <>
    T.unpack (FG.showSCC g)

type DepManResult m a = Result DepManError m a

data DepInfo =
  DepInfo
    { graph :: FG.FileGraph
    , actions :: [SiteAction]
    , layoutFiles :: [FilePath]
    }

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
      Just a -> FG.addChild a deps graph

-- | Check if a @SiteAction@ should be updated. This is the case when its
-- output file is not up to date but its dependencies are.
shouldUpdateSiteAction ::
     MonadReadWorld m => FG.FileGraph -> SiteAction -> DepManResult m Bool
shouldUpdateSiteAction graph sa = do
  outputUpToDate <-
    case saOutputFile sa of
      Just a -> FG.isUpToDate a graph `mapResultError` GraphDepManError
      Nothing -> return True
  deps <- saNeededFiles sa `mapResultError` LayoutDepManError
  depsUpToDate <- FG.areUpToDate deps graph `mapResultError` GraphDepManError
  return (not outputUpToDate && depsUpToDate)

-- | Evaluate a @SiteAction@ if it's not up to date and its dependencies are.
updateSiteAction ::
     MonadWriteWorld m => FG.FileGraph -> SiteAction -> DepManResult m ()
updateSiteAction graph sa = do
  shouldUpdate <- shouldUpdateSiteAction graph sa
  when shouldUpdate (resultLift $ executeSiteAction sa)

-- | Update all @SiteAction@s resulting from a layout file.
updateLayoutFile ::
     MonadWriteWorld m => FilePath -> BuilderMap -> DepManResult m ()
updateLayoutFile layoutFile map = do
  depInfo <- getDepInfo layoutFile map
  mapM_ (updateSiteAction $ graph depInfo) (actions depInfo)

-- | Builds dependency information from all layout files in a directory
-- (recursively).
getDepInfoWalking ::
     MonadReadWorld m => FilePath -> BuilderMap -> DepManResult m DepInfo
getDepInfoWalking root map = do
  yamlFiles <- pathWalkEndingIn root ".yaml" `mapResultError` FileDepManError
  actionsRaw <-
    mapM (`loadSiteActions` map) yamlFiles `mapResultError` LayoutDepManError
  let actions = concat actionsRaw
  graph <- graphFromActions actions
  return $ DepInfo graph actions yamlFiles

-- | Builds dependency information from a layout file.
getDepInfo ::
     MonadReadWorld m => FilePath -> BuilderMap -> DepManResult m DepInfo
getDepInfo layoutFile map = do
  actions <- loadSiteActions layoutFile map `mapResultError` LayoutDepManError
  graph <- graphFromActions actions
  return $ DepInfo graph actions [layoutFile]

-- | Create a @FileGraph@ based on a @[SiteAction]@.
graphFromActions ::
     MonadReadWorld m => [SiteAction] -> DepManResult m FG.FileGraph
graphFromActions actions = do
  graph <- foldM (flip addDependencies) FG.empty actions
  let cycles = FG.getSCC graph
  if not (null cycles)
    then resultE $ CyclicFileGraph cycles
    else return graph
