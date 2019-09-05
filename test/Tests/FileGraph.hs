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
  [ testGroup "Building Graphs" testBuildingGraphs
  , testGroup "Checking Children" testCheckChildren
  ]

testBuildingGraphs =
  [ testCase "Singleton graph" $ do
      let graph = FG.graphFromMapping "foo" $ HS.fromList ["bar", "baz"]
      FG.files graph @?= HS.singleton "foo"
      FG.connections graph @?= HM.fromList [("foo", HS.fromList ["bar", "baz"])]
  , testCase "Multiple graphs" $ do
      let mappings =
            [ ("foo", HS.fromList ["bar", "baz"])
            , ("yay", HS.fromList ["nay", "bray"])
            ]
      let graph = FG.graphFromMappings mappings
      FG.files graph @?= HS.fromList ["foo", "yay"]
      FG.connections graph @?= HM.fromList mappings
  , testCase "Adding single mapping" $ do
      let mappings =
            [ ("foo", HS.fromList ["bar", "baz"])
            , ("yay", HS.fromList ["nay", "bray"])
            ]
      let newMapping = ("one", HS.fromList ["two", "three"])
      let graph =
            uncurry FG.addMapping newMapping $ FG.graphFromMappings mappings
      FG.files graph @?= HS.fromList ["foo", "yay", "one"]
      FG.connections graph @?=
        HM.union (uncurry HM.singleton newMapping) (HM.fromList mappings)
  , testCase "Adding multiple mapping" $ do
      let mappings =
            [ ("foo", HS.fromList ["bar", "baz"])
            , ("yay", HS.fromList ["nay", "bray"])
            ]
      let newMappings =
            [ ("one", HS.fromList ["two", "three"])
            , ("four", HS.fromList ["five", "six"])
            ]
      let graph = FG.addMappings newMappings $ FG.graphFromMappings mappings
      FG.files graph @?= HS.fromList ["foo", "yay", "one", "four"]
      FG.connections graph @?=
        HM.union (HM.fromList newMappings) (HM.fromList mappings)
  , testCase "Same file, different deps" $ do
      let mappings =
            [ ("foo", HS.fromList ["bar", "baz"])
            , ("foo", HS.fromList ["nay", "bray"])
            ]
      let graph = FG.graphFromMappings mappings
      FG.files graph @?= HS.fromList ["foo"]
      FG.connections graph @?=
        HM.singleton "foo" (HS.fromList ["bar", "baz", "nay", "bray"])
  ]

testCheckChildren =
  [ testCase "Empty graph" $ FG.getChildren "test" FG.empty @?= Nothing
  , testCase "Populated graph, exists" $
    FG.getChildren "foo" graph @?= Just (HS.fromList ["bar", "baz"])
  , testCase "Populated graph, doesn't exist" $
    FG.getChildren "whatever" graph @?= Nothing
  ]
  where
    graph =
      FG.graphFromMappings
        [ ("foo", HS.fromList ["bar", "baz"])
        , ("yay", HS.fromList ["nay", "bray"])
        ]
