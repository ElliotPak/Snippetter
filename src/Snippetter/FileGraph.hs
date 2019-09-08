-- | A graph, used for site dependency management.
module Snippetter.FileGraph where

import Control.Monad
import Control.Monad.State
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Snippetter.Build
import Snippetter.IO
import Snippetter.Utilities

-- | Shorthand for a mapping from a file to a set of files.
type FileMapping = HM.HashMap FilePath FilePathSet

data GraphError
  = GraphFileError FileError
  | MissingKey FilePath
  | OtherGraphError T.Text
  deriving (Eq)

instance Show GraphError where
  show (GraphFileError e) = "While reading a file:\n" <> indentFourStr (show e)
  show (MissingKey k) =
    "The following key was missing from the file graph: " <> k
  show (OtherGraphError t) = "A YAML error occured: " <> T.unpack t

type GraphResult m a = Result GraphError m a

-- | A file graph. Contains a list of files and their parent/child
-- relationships to each other.
data FileGraph =
  FileGraph
    { files :: FilePathSet
    , parentToChild :: FileMapping
    , childToParent :: FileMapping
    }
  deriving (Show, Eq)

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
  if HM.member key map
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
addChildren mappings graph = foldr add graph mappings
  where
    add (path, children) = addChild path children

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

-- | Checks if the a file in the graph is up to date.
isUpToDate :: MonadReadWorld m => FilePath -> FileGraph -> GraphResult m Bool
isUpToDate file graph =
  case getParents file graph of
    Nothing -> resultE $ MissingKey file
    Just p -> do
      results <- resultLift $ mapM (isOlder file) (HS.toList p)
      return $ and results

-- | Checks if the first file is older than the second.
isOlder :: MonadReadWorld m => FilePath -> FilePath -> m Bool
isOlder target dep = do
  targetExists <- fileExists target
  if not targetExists
    then return True
    else do
      targetDate <- fileModifyTime target
      depDate <- fileModifyTime dep
      return $ targetDate > depDate

-- | Get a list of strongly connected components in a graph. If there are none,
-- it's acyclic.
getSCC :: FileGraph -> [SCComponent]
getSCC graph =
  filter (\a -> length a > 1) $ sccComponents $ execState stateFunc init
  where
    init = SCCState graph 0 HM.empty [] []
    stateFunc = do
      state <- get
      let graph = sccGraph state
      forM_ (HS.toList $ files graph) $ \child ->
        if not (child `HM.member` sccMappings state)
          then sccPerNode child
          else modify id
      return $ sccComponents state

data SCCState =
  SCCState
    { sccGraph :: FileGraph
    , sccIndex :: Int
    , sccMappings :: HM.HashMap FilePath (Int, Int)
    , sccStack :: [FilePath]
    , sccComponents :: [SCComponent]
    }
  deriving (Show, Eq)

type SCComponent = [FilePath]

sccPerNode :: FilePath -> State SCCState ()
sccPerNode node = do
  state <- get
  let graph = sccGraph state
  let children = parentToChild graph HM.! node
  modify $ sccInitial node
  forM_ (HS.toList children) $ \child ->
    if not (child `HM.member` sccMappings state)
      then do
        sccPerNode child
        modify $ sccUnDefMod node child
      else if child `elem` sccStack state
             then modify $ sccOnStackMod node child
             else modify id
  modify $ sccAddComponent node

sccUnDefMod :: FilePath -> FilePath -> SCCState -> SCCState
sccUnDefMod v w state = state {sccMappings = HM.adjust mod v mappings}
  where
    mappings = sccMappings state
    (vInd, vLow) = mappings HM.! v
    (wInd, wLow) = mappings HM.! w
    mod _ = (vInd, min vLow wLow)

sccOnStackMod :: FilePath -> FilePath -> SCCState -> SCCState
sccOnStackMod v w state = state {sccMappings = HM.adjust mod v mappings}
  where
    mappings = sccMappings state
    (vInd, vLow) = mappings HM.! v
    (wInd, wLow) = mappings HM.! w
    mod _ = (vInd, min vLow wInd)

sccInitial :: FilePath -> SCCState -> SCCState
sccInitial node state =
  state
    { sccIndex = newInd
    , sccMappings = HM.insert node (newInd, newInd) $ sccMappings state
    , sccStack = sccStack state ++ [node]
    }
  where
    newInd = sccIndex state + 1

sccAddComponent :: FilePath -> SCCState -> SCCState
sccAddComponent node state =
  if ind == low
    then state {sccStack = newStack, sccComponents = newComponents}
    else state
  where
    (ind, low) = sccMappings state HM.! node
    stackInd = unJust $ elemIndex node (sccStack state)
    (newStack, change) = splitAt stackInd (sccStack state)
    newComponents = change : sccComponents state
