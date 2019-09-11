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
  | CyclicFileGraph [FG.SCComponent]
  deriving (Eq)

instance Show DepManError where
  show (LayoutDepManError e) = show e
  show (FileDepManError e) = show e
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
