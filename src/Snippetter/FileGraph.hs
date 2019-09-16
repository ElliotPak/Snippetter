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
  show (GraphFileError e) = show e
  show (MissingKey k) =
    "The following key was missing from the file graph: " <> k
  show (OtherGraphError t) =
    "An error occured while determining dependencies: " <> T.unpack t

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

-- | Update the file set if it exists, or insert a new one if it doesn't.
addOrAdjust :: FilePath -> FilePathSet -> FileMapping -> FileMapping
addOrAdjust key set map =
  if HM.member key map
    then HM.adjust (HS.union set) key map
    else HM.insert key set map

addOrAdjustSingle :: FilePath -> FilePath -> FileMapping -> FileMapping
addOrAdjustSingle aa bb = addOrAdjust aa (HS.singleton bb)

-- | Creates an singleton FileGraph from one mapping.
graphFromMapping :: FilePath -> FilePathSet -> FileGraph
graphFromMapping path children = addChildren path children empty

-- | Creates a FileGraph from multiple mappings.
graphFromMappings :: [(FilePath, FilePathSet)] -> FileGraph
graphFromMappings mappings = addMultipleChildren mappings empty

-- | Map the key to an empty set.
addBlankEntry :: FilePath -> FileMapping -> FileMapping
addBlankEntry key = addOrAdjust key HS.empty

-- | Map each key in the set to empty sets.
addBlankEntries :: FilePathSet -> FileMapping -> FileMapping
addBlankEntries children mapping =
  foldr addBlankEntry mapping $ HS.toList children

-- | Add a file to the graph without any parent-child relationship.
addFile :: FilePath -> FileGraph -> FileGraph
addFile newChild graph = FileGraph newFiles newP2C newC2P
  where
    newFiles = HS.unions [HS.singleton newChild, files graph]
    newP2C = addBlankEntry newChild $ parentToChild graph
    newC2P = addBlankEntry newChild $ childToParent graph

-- | Add multiple files to the graph without any parent-child relationship.
addFiles :: FilePathSet -> FileGraph -> FileGraph
addFiles mappings graph = foldr addFile graph mappings

-- | Adds a single parent-child mapping to an existing @FileGraph@.
addChild :: FilePath -> FilePath -> FileGraph -> FileGraph
addChild pp cc graph = FileGraph newFiles newP2C newC2P
  where
    newFiles = HS.union (HS.fromList [pp, cc]) (files graph)
    newP2C =
      addOrAdjustSingle pp cc $
      addBlankEntry pp $ addBlankEntry cc $ parentToChild graph
    newC2P =
      addOrAdjustSingle cc pp $
      addBlankEntry pp $ addBlankEntry cc $ childToParent graph

-- | Maps multiple children to the same parent in an existing @FileGraph@.
addChildren :: FilePath -> FilePathSet -> FileGraph -> FileGraph
addChildren newParent newChildren graph =
  foldr add graph $ HS.toList newChildren
  where
    add = addChild newParent

-- | Adds multiple mappings to an existing FileGraph. If a mapping for that
-- file already exists, the children will be added to the ones that already
-- exist.
addMultipleChildren :: [(FilePath, FilePathSet)] -> FileGraph -> FileGraph
addMultipleChildren mappings graph = foldr add graph mappings
  where
    add (path, children) = addChildren path children

-- | Returns the set of children of the given file.
getChildren :: FilePath -> FileGraph -> Maybe FilePathSet
getChildren path graph = HM.lookup path (parentToChild graph)

-- | Returns the set of children of the given file (and errors if no mappings
-- for that file exist)
getChildren' :: FilePath -> FileGraph -> FilePathSet
getChildren' path graph =
  fromMaybe (error "node doesn't exist") (getChildren path graph)

-- | Returns the set of direct parents of the given file.
getParents :: FilePath -> FileGraph -> Maybe FilePathSet
getParents path graph = HM.lookup path (childToParent graph)

-- | Returns the set of direct parents of the given file (and will error if no
-- mappings for that file exist)
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

-- | Checks if a set of files in the graph are up to date.
areUpToDate ::
     MonadReadWorld m => FilePathSet -> FileGraph -> GraphResult m Bool
areUpToDate targets graph = do
  list <- mapM (`isUpToDate` graph) (HS.toList targets)
  return $ and list

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

-- | Checks if the node is its own parent.
isOwnParent :: FilePath -> FileGraph -> Bool
isOwnParent node graph = node `HS.member` (parentToChild graph HM.! node)

-- | Get a list of strongly connected components in a graph. If there are none,
-- it's acyclic. Does not include single-filepath components unless that
-- filepath has itself as its parent.
getSCC :: FileGraph -> [SCComponent]
getSCC graph =
  filter (\a -> length a > 1 || (length a == 1 && isOwnParent (head a) graph)) $
  sccComponents $ execState stateFunc init
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

showSCC :: [SCComponent] -> T.Text
showSCC scc = indentMultiWithListMarker $ map showSingleSCC scc

showSingleSCC :: SCComponent -> T.Text
showSingleSCC component =
  T.pack (foldr foo "" component <> "\"" <> head component <> "\"")
  where
    foo prev comp = "\"" <> prev <> "\" -> " <> comp

sccPerNode :: FilePath -> State SCCState ()
sccPerNode node = do
  modify $ sccInitial node
  state <- get
  let graph = sccGraph state
  let children = parentToChild graph HM.! node
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

-- | For a given set of nodes, get the "parents" of all of those nodes.
getRoots :: FilePathSet -> FileGraph -> FilePathSet
getRoots set graph = evalState stateFunc init
  where
    init = RootState graph HM.empty HS.empty
    stateFunc = do
      forM_ (HS.toList set) $ \child -> rootPerNode child
      gets rootParents

data RootState =
  RootState
    { rootGraph :: FileGraph
    , rootVisited :: FileMapping
    , rootParents :: FilePathSet
    }

rootPerNode :: FilePath -> State RootState ()
rootPerNode path = do
  state <- get
  let graph = rootGraph state
  let visited = rootVisited state
  let parents = rootParents state
  unless (HM.member path visited) $ do
    let children = getAllChildrenCache path graph visited
    let childrenSet = HS.fromList $ HM.keys children
    let newParents = HS.insert path parents
    put $ state {rootVisited = children, rootParents = newParents}

getAllChildren :: FilePath -> FileGraph -> FilePathSet
getAllChildren path graph =
  HS.fromList $ HM.keys $ getAllChildrenCache path graph HM.empty

getAllChildrenCache :: FilePath -> FileGraph -> FileMapping -> FileMapping
getAllChildrenCache path graph = execState (getAllChildrenCache' path graph)

getAllChildrenCache' :: FilePath -> FileGraph -> State FileMapping FilePathSet
getAllChildrenCache' path graph = do
  cache <- get
  if HM.member path cache
    then return $ cache HM.! path
    else do
      let kids f = getAllChildrenCache' f graph
      result <- mapM kids $ HS.toList $ parentToChild graph HM.! path
      modify $ HM.insert path (HS.unions result)
      return $ HS.unions result
