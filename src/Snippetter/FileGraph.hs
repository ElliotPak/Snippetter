-- | A graph, used for site dependency management.
module Snippetter.FileGraph where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe
import Snippetter.Build
import Snippetter.Layout

-- | Shorthand for a mapping from a file to a set of files.
type FileMapping = HM.HashMap FilePath FilePathSet

-- | A file graph. Contains a list of files and their parent/child
-- relationships to each other.
data FileGraph =
  FileGraph
    { files :: FilePathSet
    , parentToChild :: FileMapping
    , childToParent :: FileMapping
    }

-- | Get parent-to-child mappings, excluding files that have no children.
notEmptyParentToChild :: FileGraph -> FileMapping
notEmptyParentToChild = HM.filter (not . HS.null) . parentToChild

-- | Get child-to-parent mappings, excluding files that have no parents.
notEmptyChildToParent :: FileGraph -> FileMapping
notEmptyChildToParent = HM.filter (not . HS.null) . childToParent

-- | Creates an empty FileGraph.
empty :: FileGraph
empty = FileGraph HS.empty HM.empty HM.empty

-- | Given a parent and a list of children, create a mapping of children to parents.
invertMapping :: FilePath -> FilePathSet -> FileMapping
invertMapping parent children = HM.fromList $ map inverse $ HS.toList children
  where
    inverse x = (x, HS.singleton parent)

-- | Update the file set if it exists, or insert a new one if it doesn't.
addOrAdjust :: FilePath -> FilePathSet -> FileMapping -> FileMapping
addOrAdjust key set map =
  if HM.member key map && not (HS.null set)
    then HM.adjust (HS.union set) key map
    else HM.insert key set map

-- | Update the file set if it exists, or insert a new one if it doesn't. Uses
-- with @'invertMapping'@ in order to add or adjust the @childToParent@
-- mapping.
addOrAdjustInvert :: FilePath -> FilePathSet -> FileMapping -> FileMapping
addOrAdjustInvert key set mapping = foldr func mapping inverted
  where
    inverted = HM.toList $ invertMapping key set
    func = uncurry addOrAdjust

-- | Creates an singleton FileGraph from one mapping.
graphFromChild :: FilePath -> FilePathSet -> FileGraph
graphFromChild path children = addChild path children empty

-- | Creates a FileGraph from multiple mappings.
graphFromChildren :: [(FilePath, FilePathSet)] -> FileGraph
graphFromChildren mappings = addChildren mappings empty

-- | Map the key to an empty set.
addBlankEntry :: FilePath -> FileMapping -> FileMapping
addBlankEntry key = addOrAdjust key HS.empty

-- | Map each key in the set to empty sets.
addBlankEntries :: FilePathSet -> FileMapping -> FileMapping
addBlankEntries children mapping =
  foldr addBlankEntry mapping $ HS.toList children

-- | Adds a mapping to an existing FileGraph. If a mapping for that file
-- already exists, the children will be added to the ones that already exist.
addChild :: FilePath -> FilePathSet -> FileGraph -> FileGraph
addChild newParent newChildren graph = FileGraph newFiles newP2C newC2P
  where
    newFiles = HS.unions [HS.singleton newParent, newChildren, files graph]
    newP2C =
      addOrAdjust newParent newChildren $
      addBlankEntries newChildren $ parentToChild graph
    newC2P =
      addOrAdjustInvert newParent newChildren $
      addBlankEntry newParent $ childToParent graph

-- | Adds multiple mappings to an existing FileGraph. If a mapping for that
-- file already exists, the children will be added to the ones that already
-- exist.
addChildren :: [(FilePath, FilePathSet)] -> FileGraph -> FileGraph
addChildren mappings graph = foldl add graph mappings
  where
    add graph (path, children) = addChild path children graph

-- | Returns the set of children of the given file.
getChildren :: FilePath -> FileGraph -> Maybe FilePathSet
getChildren path graph = HM.lookup path (parentToChild graph)

-- | Returns the set of children of the given file (and errors if no mappings
-- for that file exist)
getChildren' :: FilePath -> FileGraph -> FilePathSet
getChildren' path graph =
  fromMaybe (error "node doesn't exist") (getChildren path graph)

-- | Returns the set of parents of the given file.
getParents :: FilePath -> FileGraph -> Maybe FilePathSet
getParents path graph = HM.lookup path (childToParent graph)

-- | Returns the set of parents of the given file (and errors if no mappings
-- for that file exist)
getParents' :: FilePath -> FileGraph -> FilePathSet
getParents' path graph =
  fromMaybe (error "node doesn't exist") (getParents path graph)
