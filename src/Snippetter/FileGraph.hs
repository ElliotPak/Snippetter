-- | A graph, used for site dependency management.
module Snippetter.FileGraph where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe
import Snippetter.Build
import Snippetter.Layout

data FileGraph =
  FileGraph
    { files :: FilePathSet
    , connections :: HM.HashMap FilePath FilePathSet
    }

-- | Creates an empty FileGraph.
empty :: FileGraph
empty = FileGraph HS.empty HM.empty

-- | Creates an singleton FileGraph from one mapping.
graphFromMapping :: FilePath -> FilePathSet -> FileGraph
graphFromMapping path children =
  FileGraph (HS.singleton path) (HM.singleton path children)

-- | Creates a FileGraph from multiple mappings.
graphFromMappings :: [(FilePath, FilePathSet)] -> FileGraph
graphFromMappings mappings = addMappings mappings empty

-- | Adds a mapping to an existing FileGraph. If a mapping for that file
-- already exists, the children will be added to the ones that already exist.
addMapping :: FilePath -> FilePathSet -> FileGraph -> FileGraph
addMapping path children graph = FileGraph newFiles newDeps
  where
    con = connections graph
    newFiles = HS.insert path (files graph)
    newDeps =
      if HM.member path con
        then HM.adjust (HS.union children) path con
        else HM.insert path children con

-- | Adds multiple mappings to an existing FileGraph. If a mapping for that
-- file already exists, the children will be added to the ones that already
-- exist.
addMappings :: [(FilePath, FilePathSet)] -> FileGraph -> FileGraph
addMappings mappings graph = foldl add graph mappings
  where
    add graph (path, children) = addMapping path children graph

-- | Returns the set of children of the given file.
getChildren :: FilePath -> FileGraph -> Maybe FilePathSet
getChildren path graph = HM.lookup path (connections graph)

-- | Returns the set of children of the given file (and errors if no mappings
-- for that file exist)
getChildren' :: FilePath -> FileGraph -> FilePathSet
getChildren' path graph =
  fromMaybe (error "node doesn't exist") (getChildren path graph)
