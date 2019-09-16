module Tests.FileGraph
  ( tests
  ) where

import Control.Monad.Trans.Except
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Yaml as Y
import Snippetter.Build
import qualified Snippetter.FileGraph as FG
import Snippetter.Helpers
import Snippetter.IO
import Snippetter.Layout
import Test.Tasty
import Test.Tasty.HUnit
import Tests.Helpers

tests =
  [ testCase "Getting Parents/Children" testGettingParentsAndChildren
  , testGroup "Building Graphs" testBuildingGraphs
  , testGroup "Checking Children" testCheckChildren
  , testGroup "Checking Parents" testCheckParents
  , testGroup "SCC" testCheckSCC
  ]

testGettingParentsAndChildren = do
  let graph = FG.graphFromMapping "foo" $ HS.fromList ["bar", "baz"]
  FG.parentToChild graph @?=
    HM.fromList
      [ ("foo", HS.fromList ["bar", "baz"])
      , ("bar", HS.empty)
      , ("baz", HS.empty)
      ]
  FG.childToParent graph @?=
    HM.fromList
      [ ("bar", HS.singleton "foo")
      , ("baz", HS.singleton "foo")
      , ("foo", HS.empty)
      ]
  FG.notEmptyParentToChild graph @?=
    HM.fromList [("foo", HS.fromList ["bar", "baz"])]
  FG.notEmptyChildToParent graph @?=
    HM.fromList [("bar", HS.singleton "foo"), ("baz", HS.singleton "foo")]

testBuildingGraphs =
  [ testCase "Singleton graph" $ do
      let graph = FG.graphFromMapping "foo" $ HS.fromList ["bar", "baz"]
      FG.files graph @?= HS.fromList ["foo", "bar", "baz"]
      FG.notEmptyParentToChild graph @?=
        HM.fromList [("foo", HS.fromList ["bar", "baz"])]
      FG.notEmptyChildToParent graph @?=
        HM.fromList [("bar", HS.singleton "foo"), ("baz", HS.singleton "foo")]
  , testCase "Multiple graphs" $ do
      let mappings =
            [ ("foo", HS.fromList ["bar", "baz"])
            , ("yay", HS.fromList ["nay", "bray"])
            ]
      let graph = FG.graphFromMappings mappings
      FG.files graph @?= HS.fromList ["foo", "yay", "bar", "baz", "nay", "bray"]
      FG.notEmptyParentToChild graph @?= HM.fromList mappings
      FG.notEmptyChildToParent graph @?=
        HM.fromList
          [ ("bar", HS.singleton "foo")
          , ("baz", HS.singleton "foo")
          , ("nay", HS.singleton "yay")
          , ("bray", HS.singleton "yay")
          ]
  , testCase "Adding single mapping" $ do
      let mappings =
            [ ("foo", HS.fromList ["bar", "baz"])
            , ("yay", HS.fromList ["nay", "bray"])
            ]
      let newChild = ("one", HS.fromList ["two", "three"])
      let graph =
            uncurry FG.addChildren newChild $ FG.graphFromMappings mappings
      FG.files graph @?=
        HS.fromList
          ["foo", "yay", "bar", "baz", "nay", "bray", "one", "two", "three"]
      FG.notEmptyParentToChild graph @?=
        HM.union (uncurry HM.singleton newChild) (HM.fromList mappings)
      FG.notEmptyChildToParent graph @?=
        HM.fromList
          [ ("bar", HS.singleton "foo")
          , ("baz", HS.singleton "foo")
          , ("nay", HS.singleton "yay")
          , ("bray", HS.singleton "yay")
          , ("two", HS.singleton "one")
          , ("three", HS.singleton "one")
          ]
  , testCase "Adding multiple mapping" $ do
      let mappings =
            [ ("foo", HS.fromList ["bar", "baz"])
            , ("yay", HS.fromList ["nay", "bray"])
            ]
      let newChildren =
            [ ("one", HS.fromList ["two", "three"])
            , ("four", HS.fromList ["five", "six"])
            ]
      let graph =
            FG.addMultipleChildren newChildren $ FG.graphFromMappings mappings
      FG.files graph @?=
        HS.fromList
          [ "foo"
          , "yay"
          , "bar"
          , "baz"
          , "nay"
          , "bray"
          , "one"
          , "two"
          , "three"
          , "four"
          , "five"
          , "six"
          ]
      FG.notEmptyParentToChild graph @?=
        HM.union (HM.fromList newChildren) (HM.fromList mappings)
      FG.notEmptyChildToParent graph @?=
        HM.fromList
          [ ("bar", HS.singleton "foo")
          , ("baz", HS.singleton "foo")
          , ("nay", HS.singleton "yay")
          , ("bray", HS.singleton "yay")
          , ("two", HS.singleton "one")
          , ("three", HS.singleton "one")
          , ("five", HS.singleton "four")
          , ("six", HS.singleton "four")
          ]
  , testCase "Same file, different deps" $ do
      let mappings =
            [ ("foo", HS.fromList ["bar", "baz"])
            , ("foo", HS.fromList ["nay", "bray"])
            ]
      let graph = FG.graphFromMappings mappings
      FG.files graph @?= HS.fromList ["foo", "bar", "baz", "nay", "bray"]
      FG.notEmptyParentToChild graph @?=
        HM.singleton "foo" (HS.fromList ["bar", "baz", "nay", "bray"])
      FG.notEmptyChildToParent graph @?=
        HM.fromList
          [ ("bar", HS.singleton "foo")
          , ("baz", HS.singleton "foo")
          , ("nay", HS.singleton "foo")
          , ("bray", HS.singleton "foo")
          ]
  , testCase "Looping graph" $ do
      let mappings =
            [ ("foo", HS.fromList ["bar", "asdf"])
            , ("bar", HS.singleton "yay")
            , ("yay", HS.singleton "foo")
            , ("asdf", HS.singleton "hjkl")
            ]
      let graph = FG.graphFromMappings mappings
      FG.files graph @?= HS.fromList ["hjkl", "foo", "bar", "yay", "asdf"]
      FG.notEmptyParentToChild graph @?= HM.fromList mappings
      FG.notEmptyChildToParent graph @?=
        HM.fromList
          [ ("hjkl", HS.singleton "asdf")
          , ("foo", HS.singleton "yay")
          , ("yay", HS.singleton "bar")
          , ("bar", HS.singleton "foo")
          , ("asdf", HS.singleton "foo")
          ]
  ]

testCheckChildren =
  [ testCase "Empty graph" $ FG.getChildren "test" FG.empty @?= Nothing
  , testCase "Populated graph, exists" $
    FG.getChildren "foo" graph @?= Just (HS.fromList ["bar", "baz"])
  , testCase "Populated graph, doesn't exist" $
    FG.getChildren "whatever" graph @?= Nothing
  , testCase "Populated graph, no children" $
    FG.getChildren "bar" graph @?= Just HS.empty
  ]
  where
    graph =
      FG.graphFromMappings
        [ ("foo", HS.fromList ["bar", "baz"])
        , ("yay", HS.fromList ["nay", "bray"])
        ]

testCheckParents =
  [ testCase "Empty graph" $ FG.getParents "test" FG.empty @?= Nothing
  , testCase "Populated graph, exists" $
    FG.getParents "bar" graph @?= Just (HS.fromList ["foo"])
  , testCase "Populated graph, doesn't exist" $
    FG.getParents "whatever" graph @?= Nothing
  , testCase "Populated graph, no parents" $
    FG.getParents "foo" graph @?= Just HS.empty
  ]
  where
    graph =
      FG.graphFromMappings
        [ ("foo", HS.fromList ["bar", "baz"])
        , ("yay", HS.fromList ["nay", "bray"])
        ]

testCheckSCC =
  [ testCase "Empty graph" $ FG.getSCC FG.empty @?= []
  , testCase "Acyclic graph" $
    FG.getSCC
      (FG.graphFromMappings
         [("foo", HS.fromList ["bar"]), ("bar", HS.singleton "yay")]) @?=
    []
  , testCase "Looping graph 1" $
    FG.getSCC
      (FG.graphFromMappings
         [("foo", HS.fromList ["bar"]), ("bar", HS.singleton "foo")]) @?=
    [["foo", "bar"]]
  , testCase "Looping graph 2" $
    FG.getSCC
      (FG.graphFromMappings
         [ ("foo", HS.fromList ["bar", "asdf"])
         , ("bar", HS.singleton "yay")
         , ("yay", HS.singleton "foo")
         , ("asdf", HS.singleton "hjkl")
         ]) @?=
    [["foo", "bar", "yay"]]
  , testCase "Self loop" $
    FG.getSCC (FG.graphFromMappings [("foo", HS.fromList ["foo"])]) @?=
    [["foo"]]
  , testCase "Multiple loops" $
    FG.getSCC
      (FG.graphFromMappings
         [ ("foo", HS.fromList ["bar", "asdf"])
         , ("bar", HS.singleton "yay")
         , ("yay", HS.singleton "foo")
         , ("asdf", HS.singleton "hjkl")
         , ("hjkl", HS.singleton "zxcv")
         , ("zxcv", HS.singleton "asdf")
         ]) @?=
    [["foo", "bar", "yay"], ["hjkl", "zxcv", "asdf"]]
  ]
