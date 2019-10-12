{-# LANGUAGE DeriveGeneric #-}

-- | A graph, used for site dependency management. This is used by other
-- components: you shouldn't need to create your own 'FileGraph' when building
-- your own site.
module Snippetter.FileGraph
  ( -- * Basics
    FileGraph (nodes, parentToChild, childToParent)
  , GraphError
  , GraphResult
  , SCComponent
    -- * Creation and Modification
  , empty
  , fromSiteActions
  , addSiteAction
    -- * Retrieving info
  , getSCC
  , showSCC
  , shouldUpdateSiteAction
  , childrenToUpdate
  , getRootsOfActions
  , getAllChildrenActions
  ) where

import Control.Monad
import Control.Monad.State
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable
import Data.List
import Data.Maybe
import Data.Foldable
import qualified Data.Text as T
import Debug.Trace
import GHC.Generics
import Snippetter.Build
import Snippetter.Layout
import Snippetter.IO
import Snippetter.Utilities

-- | Possible errors when executing graph operations.
data GraphError
  = GraphFileError FileError
  | GraphLayoutError LayoutError
  | MissingKey Node
  | OtherGraphError T.Text
  deriving (Eq)

instance Show GraphError where
  show (GraphFileError e) = show e
  show (MissingKey k) =
    "The following key was missing from the file graph: " <> show k
  show (OtherGraphError t) =
    "An error occured while determining dependencies: " <> T.unpack t

-- | The result of a graph function that can fail.
type GraphResult m a = Result GraphError m a

-- | A file graph. Contains a list of files and their parent/child
-- relationships to each other.
-- 
-- This isn't enforced by the FileGraph, but file A should be considered a
-- parent of file B if A is required to create B, which would also make file B
-- a child of file A. 
data FileGraph =
  FileGraph
    { nodes :: NodeSet
    , parentToChild :: Mapping
    , childToParent :: Mapping
    }
  deriving (Show, Eq)

data Node
  = File FilePath
  | Action PathedSiteAction
  deriving (Show, Eq, Generic)

data NodeType = FileType | ActionType deriving (Show, Eq)

instance Hashable Node

type NodeSet = HS.HashSet Node

isNodeType :: Node -> NodeType -> Bool
isNodeType (File _) FileType = True
isNodeType (Action _) ActionType = True
isNodeType _ _ = False

fileFromNode :: Node -> Maybe FilePath
fileFromNode (File a) = Just a
fileFromNode (Action _) = Nothing

actionFromNode :: Node -> Maybe PathedSiteAction
actionFromNode (Action a) = Just a
actionFromNode (File _) = Nothing

-- | Shorthand for a mapping from a file to a set of files.
type Mapping = HM.HashMap Node NodeSet

-- | Get parent-to-child mappings, excluding files that have no children.
notEmptyParentToChild :: FileGraph -> Mapping
notEmptyParentToChild = HM.filter (not . HS.null) . parentToChild

-- | Get child-to-parent mappings, excluding files that have no parents.
notEmptyChildToParent :: FileGraph -> Mapping
notEmptyChildToParent = HM.filter (not . HS.null) . childToParent

-- | Creates an empty FileGraph.
empty :: FileGraph
empty = FileGraph HS.empty HM.empty HM.empty

-- | Update the file set if it exists, or insert a new one if it doesn't.
addOrAdjust :: Node -> NodeSet -> Mapping -> Mapping
addOrAdjust key set map =
  if HM.member key map
    then HM.adjust (HS.union set) key map
    else HM.insert key set map

addOrAdjustSingle :: Node -> Node -> Mapping -> Mapping
addOrAdjustSingle aa bb = addOrAdjust aa (HS.singleton bb)

-- | Map the key to an empty set.
addBlankNode :: Node -> Mapping -> Mapping
addBlankNode key = addOrAdjust key HS.empty

-- | Map each key in the set to empty sets.
addBlankNodes :: NodeSet -> Mapping -> Mapping
addBlankNodes children mapping =
  foldr addBlankNode mapping $ HS.toList children

-- | Add a file to the graph without any parent-child relationship.
addNode :: Node -> FileGraph -> FileGraph
addNode newChild graph = FileGraph newNodes newP2C newC2P
  where
    newNodes = HS.unions [HS.singleton newChild, nodes graph]
    newP2C = addBlankNode newChild $ parentToChild graph
    newC2P = addBlankNode newChild $ childToParent graph

-- | Add multiple files to the graph without any parent-child relationship.
addNodes :: NodeSet -> FileGraph -> FileGraph
addNodes mappings graph = foldr addNode graph mappings

-- | Adds a single parent-child mapping to an existing @FileGraph@.
addEdge :: Node -> Node -> FileGraph -> FileGraph
addEdge pp cc graph = FileGraph newFiles newP2C newC2P
  where
    newFiles = HS.union (HS.fromList [pp, cc]) (nodes graph)
    newP2C =
      addOrAdjustSingle pp cc $
      addBlankNode pp $ addBlankNode cc $ parentToChild graph
    newC2P =
      addOrAdjustSingle cc pp $
      addBlankNode pp $ addBlankNode cc $ childToParent graph

-- | Maps multiple children to the same parent in an existing @FileGraph@.
addChildren :: Node -> NodeSet -> FileGraph -> FileGraph
addChildren newParent newChildren graph =
  foldr add graph $ HS.toList newChildren
  where
    add = addEdge newParent

-- | Adds multiple mappings to an existing FileGraph. If a mapping for that
-- file already exists, the children will be added to the ones that already
-- exist.
addMultipleChildren :: [(Node, NodeSet)] -> FileGraph -> FileGraph
addMultipleChildren mappings graph = foldr add graph mappings
  where
    add (path, children) = addChildren path children

-- | Maps multiple parents to the same child in an existing @FileGraph@.
addParents :: Node -> NodeSet -> FileGraph -> FileGraph
addParents newChild newParents graph = foldr add graph $ HS.toList newParents
  where
    add = flip addEdge newChild

-- | Add entries to a @FileGraph@ based on a @SiteAction@ and its
-- output/dependencies.
addSiteAction ::
     MonadReadWorld m
  => PathedSiteAction
  -> FileGraph
  -> GraphResult m FileGraph
addSiteAction sa graph = do
  deps <- psaNeededFiles sa `mapResultError` GraphLayoutError
  let saNode = Action sa
  let depsNodes = mapSet File deps
  let addedDeps = addParents saNode depsNodes graph
  case psaOutputFile sa of
    Nothing -> return addedDeps
    Just f -> return $ addEdge saNode (File f) addedDeps

fromSiteActions :: 
     MonadReadWorld m => [PathedSiteAction] -> GraphResult m FileGraph
fromSiteActions = foldrM addSiteAction empty

-- | Returns the set of children of the given file.
getChildren :: Node -> FileGraph -> Maybe NodeSet
getChildren path graph = HM.lookup path (parentToChild graph)

-- | Returns the set of children of the given file (and errors if no mappings
-- for that file exist)
getChildren' :: Node -> FileGraph -> NodeSet
getChildren' path graph =
  fromMaybe (error "node doesn't exist") (getChildren path graph)

-- | Returns the set of direct parents of the given file.
getParents :: Node -> FileGraph -> Maybe NodeSet
getParents path graph = HM.lookup path (childToParent graph)

-- | Returns the set of direct parents of the given file (and will error if no
-- mappings for that file exist)
getParents' :: Node -> FileGraph -> NodeSet
getParents' path graph =
  fromMaybe (error "node doesn't exist") (getParents path graph)

typeLookup ::
     NodeType -> (Node -> FileGraph -> NodeSet) ->
     Node -> FileGraph -> Maybe NodeSet
typeLookup nodeType lookup node graph =
  if not $ node `HS.member` nodes graph
    then Nothing
    else Just $ rec node
  where rec :: Node -> NodeSet
        rec node = 
          if isNodeType node nodeType
            then HS.singleton node
            else do
                let newNodes = lookup node graph
                HS.unions $ map rec $ HS.toList newNodes

getFileParents :: Node -> FileGraph -> Maybe FilePathSet
getFileParents node graph = do
  lookedUp <-  typeLookup FileType getParents' node graph
  Just $ mapSetMaybe fileFromNode lookedUp

getFileChildren :: Node -> FileGraph -> Maybe FilePathSet
getFileChildren node graph = do
  lookedUp <-  typeLookup FileType getChildren' node graph
  Just $ mapSetMaybe fileFromNode lookedUp

getActionParents :: Node -> FileGraph -> Maybe SiteActionSet
getActionParents node graph = do
  lookedUp <-  typeLookup ActionType getParents' node graph
  Just $ mapSetMaybe actionFromNode lookedUp

getActionChildren :: Node -> FileGraph -> Maybe SiteActionSet
getActionChildren node graph = do
  lookedUp <-  typeLookup ActionType getChildren' node graph
  Just $ mapSetMaybe actionFromNode lookedUp

-- | Checks if the file in the graph is up to date.
isUpToDate :: MonadReadWorld m =>
    FilePath -> FileGraph -> GraphResult m Bool
isUpToDate file graph = let node = File file in
  case getFileParents node graph of
    Nothing -> resultE $ MissingKey node
    Just p -> do
      results <- resultLift $ mapM (isYounger file) (HS.toList p)
      if null results
        then return True
        else return $ and results

-- | Checks if a set of files in the graph are up to date.
areUpToDate :: MonadReadWorld m =>
     FilePathSet -> FileGraph -> GraphResult m Bool
areUpToDate targets graph = do
  list <- mapM (`isUpToDate` graph) (HS.toList targets)
  return $ and list

-- | Check if the 'PathedSiteAction's output files are up to date.
outputsUpToDate :: MonadReadWorld m =>
    PathedSiteAction -> FileGraph -> GraphResult m Bool
outputsUpToDate sa graph = let node = Action sa in
  case getFileChildren node graph of
    Nothing -> resultE $ MissingKey node
    Just files -> areUpToDate files graph

-- | Check if the 'PathedSiteAction's dependencies are up to date.
depsUpToDate :: MonadReadWorld m =>
    PathedSiteAction -> FileGraph -> GraphResult m Bool
depsUpToDate sa graph = let node = Action sa in
  case getFileParents node graph of
    Nothing -> resultE $ MissingKey node
    Just files -> areUpToDate files graph

-- | Check if a 'SiteAction' should be updated. This is the case when its
-- output file is not up to date but its dependencies are.
shouldUpdateSiteAction ::
     MonadReadWorld m => FileGraph -> PathedSiteAction -> GraphResult m Bool
shouldUpdateSiteAction graph sa = do
  o <- outputsUpToDate sa graph
  d <- depsUpToDate sa graph
  return (not o && d)

-- | Get all children 'PathedSiteAction's of the given one that need to be
-- updated.
childrenToUpdate :: MonadReadWorld m =>
    PathedSiteAction -> FileGraph -> GraphResult m [PathedSiteAction]
childrenToUpdate sa graph = let node = Action sa in
  case getActionChildren node graph of
    Nothing -> resultE $ MissingKey node
    Just files ->
      filterM (shouldUpdateSiteAction graph) $ HS.toList files

-- | Checks if the first file is younger than the second. If the first file is
-- absent, then it's older (to force a rebuild). If the second file is absent,
-- it's younger (to prevent a rebuild when a dependency does not exist).
isYounger :: MonadReadWorld m => FilePath -> FilePath -> m Bool
isYounger target dep = do
  resultT <- runResult $ fileModifyTime target
  case resultT of
    Left _ -> return False
    Right t -> do
      resultD <- runResult $ fileModifyTime dep
      case resultD of
        Left _ -> return True
        Right d -> return $ t > d

-- | Checks if the node is its own parent.
isOwnParent :: Node -> FileGraph -> Bool
isOwnParent node graph = node `HS.member` (parentToChild graph HM.! node)

-- | Get a list of strongly connected components in a graph. Single-node SCCs
-- that don't have themselves as their own parent are excluded. Therefore, if
-- there are no SCCs, the graph is acyclic, and if there are SCCs, they
-- represent cycles within the graph.
getSCC :: FileGraph -> [SCComponent]
getSCC graph =
  map (mapMaybe fileFromNode) $
  filter (\a -> length a > 1 || (length a == 1 && isOwnParent (head a) graph)) $
  sccComponents $ execState stateFunc init
  where
    init = SCCState graph 0 HM.empty [] []
    stateFunc = do
      state <- get
      let graph = sccGraph state
      forM_ (HS.toList $ nodes graph) $ \child ->
        if not (child `HM.member` sccMappings state)
          then sccPerNode child
          else modify id
      return $ sccComponents state

data SCCState =
  SCCState
    { sccGraph :: FileGraph
    , sccIndex :: Int
    , sccMappings :: HM.HashMap Node (Int, Int)
    , sccStack :: [Node]
    , sccComponents :: [SCComponent']
    }
  deriving (Show, Eq)

-- | Represents the 'FilePath's in a strongly connected component.
type SCComponent = [FilePath]

-- | Represents the 'Node's in a strongly connected component.
-- Only used internally.
type SCComponent' = [Node]

-- | Convert a @[SCComponent]@ to a human-readable string representation.
showSCC :: [SCComponent] -> T.Text
showSCC scc = indentMultiWithListMarker $ map showSingleSCC scc

showSingleSCC :: SCComponent -> T.Text
showSingleSCC component =
  T.pack (foldl foo "" component <> "\"" <> show (head component) <> "\"")
  where
    foo prev f = "\"" <> prev <> "\" -> " <> f

sccPerNode :: Node -> State SCCState ()
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

sccUnDefMod :: Node -> Node -> SCCState -> SCCState
sccUnDefMod v w state = state {sccMappings = HM.adjust mod v mappings}
  where
    mappings = sccMappings state
    (vInd, vLow) = mappings HM.! v
    (wInd, wLow) = mappings HM.! w
    mod _ = (vInd, min vLow wLow)

sccOnStackMod :: Node -> Node -> SCCState -> SCCState
sccOnStackMod v w state = state {sccMappings = HM.adjust mod v mappings}
  where
    mappings = sccMappings state
    (vInd, vLow) = mappings HM.! v
    (wInd, wLow) = mappings HM.! w
    mod _ = (vInd, min vLow wInd)

sccInitial :: Node -> SCCState -> SCCState
sccInitial node state =
  state
    { sccIndex = newInd
    , sccMappings = HM.insert node (newInd, newInd) $ sccMappings state
    , sccStack = sccStack state ++ [node]
    }
  where
    newInd = sccIndex state + 1

sccAddComponent :: Node -> SCCState -> SCCState
sccAddComponent node state =
  if ind == low
    then state {sccStack = newStack, sccComponents = newComponents}
    else state
  where
    (ind, low) = sccMappings state HM.! node
    stackInd = unJust $ elemIndex node (sccStack state)
    (newStack, change) = splitAt stackInd (sccStack state)
    newComponents = change : sccComponents state

-- | For a given set of nodes, filter out nodes from that set that have another
-- node in that set as an indirect parent. All nodes in the input set will
-- either be a node in the output set, or the indirect child of a node in the
-- output set.
getRoots :: NodeSet -> FileGraph -> NodeSet
getRoots set graph = HS.difference set $ getAllChildren' set graph

-- | For a given set of 'PathedSiteAction's, filter out actions from that set
-- that have another action in that set as an indirect parent. All nodes in the
-- input set will either be a node in the output set, or the indirect child of
-- a node in the output set (and therefore omitted).
getRootsOfActions :: SiteActionSet -> FileGraph -> SiteActionSet
getRootsOfActions actions graph = do
  let as = mapSet Action actions
  let roots = getRoots as graph
  mapSetMaybe actionFromNode roots

data RootState =
  RootState
    { rootGraph :: FileGraph
    , rootVisited :: NodeSet
    }

rootPerNode :: Node -> State RootState ()
rootPerNode path = do
  state <- get
  let graph = rootGraph state
  let visited = rootVisited state
  let children = HS.delete path $ getAllChildrenCache path graph visited
  put $ state {rootVisited = HS.union children visited}

-- | Get all children of the provided node.
getAllChildren :: Node -> FileGraph -> NodeSet
getAllChildren path = getAllChildren' (HS.singleton path)

-- | Get all children of all provided nodes.
getAllChildren' :: NodeSet -> FileGraph -> NodeSet
getAllChildren' set graph = evalState stateFunc init
  where
    init = RootState graph HS.empty
    stateFunc = do
      forM_ (HS.toList set) $ \child -> rootPerNode child
      gets rootVisited

-- | Get all children 'PathedSiteAction's of all the provided ones.
getAllChildrenActions ::
    SiteActionSet -> FileGraph -> [PathedSiteAction]
getAllChildrenActions actions graph = do
    let nodes = mapSet Action actions
    let allChildren = getAllChildren' nodes graph
    mapMaybe actionFromNode $ HS.toList allChildren

getAllChildrenCache :: Node -> FileGraph -> NodeSet -> NodeSet
getAllChildrenCache path graph cache = HS.union state result
  where
    (result, state) = runState (getAllChildrenCache' path graph) cache

getAllChildrenCache' :: Node -> FileGraph -> State NodeSet NodeSet
getAllChildrenCache' path graph = do
  cache <- get
  if HS.member path cache
    then return cache
    else do
      modify $ HS.insert path
      let kids f = getAllChildrenCache' f graph
      result <- mapM kids $ HS.toList $ parentToChild graph HM.! path
      get
