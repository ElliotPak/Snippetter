{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Snippetter.Utilities
import Test.Tasty
import Test.Tasty.HUnit
import Tests.Helpers

tests =
  [ testGroup "SCC" testCheckSCC
  , testGroup "Roots" testRoots
  ]

files =
  [ ("foo", ("", startTime))
  , ("bar", ("", startTime))
  , ("baz", ("", startTime))
  , ("asdf", ("", startTime))
  , ("hjkl", ("", startTime))
  , ("zxcv", ("", startTime))
  ]

pFiles :: 
     (Eq a, Show a, Eq e, Show e)
  => Result e MockIO a
  -> a
  -> Assertion
pFiles = passMockFiles files

fromRawActions :: 
     MonadReadWorld m => [SiteAction] -> FG.GraphResult m FG.FileGraph
fromRawActions acts = FG.fromSiteActions $ map foo acts
  where
    foo sa = PathedSiteAction sa Nothing

testCheckSCC =
  [ testCase "Empty graph" $ FG.getSCC FG.empty @?= []
  , testCase "Acyclic graph" $
    pFiles (scc [Copy "foo" "bar", Copy "bar" "yay"])
      []
  , testCase "Looping graph 1" $
    pFiles (scc
      [Copy "foo" "bar", Copy "bar" "foo"])
      [["foo", "bar"]]
  , testCase "Looping graph 2" $
    pFiles (scc
      [ Copy "foo" "bar"
      , Copy "foo" "asdf"
      , Copy "bar" "yay"
      , Copy "yay" "foo"
      , Copy "asdf" "hjkl"
      ])
      [["bar", "yay", "foo"]]
  , testCase "Self loop" $
    pFiles (scc [Copy "foo" "foo"]) [["foo"]]
  , testCase "Multiple loops" $
    pFiles (scc
      [ Copy "foo" "bar"
      , Copy "foo" "asdf"
      , Copy "bar" "yay"
      , Copy "yay" "foo"
      , Copy "asdf" "hjkl"
      , Copy "hjkl" "zxcv"
      , Copy "zxcv" "asdf"
      ])
      [["bar", "yay", "foo"], ["asdf", "hjkl", "zxcv"]]
  ]
      where
        scc acts = do
          graph <- fromRawActions acts
          return $ FG.getSCC graph

testRoots =
  [ testCase "Empty graph" $ 
    pFiles (roots HS.empty []) HS.empty
  , testCase "a -> b -> c, roots of a" $
    pFiles (roots set1 graph1) set1
  , testCase "a -> b -> c, roots of b" $
    pFiles (roots set2 graph1) set2
  , testCase "a -> b -> c, roots of c" $
    pFiles (roots set3 graph1) set3
  , testCase "a -> b -> c, roots of ab" $
    pFiles (roots (HS.fromList [n1, n2]) graph1) set1
  , testCase "a -> b -> c, roots of bc" $
    pFiles (roots (HS.fromList [n3, n2]) graph1) set2
  , testCase "a -> b -> c, roots of abc" $
    pFiles (roots (HS.fromList [n1, n2, n3]) graph1) set1
  , testCase "a -> bc -> d, roots of bcd" $
    pFiles (roots (HS.fromList [n2, n4, pCombined]) graph2) $ HS.fromList [n2, n4]
  , testCase "a -> bc -> d, roots of bc" $
    pFiles (roots (HS.fromList [n2, n4]) graph2) $ HS.fromList [n2, n4]
  , testCase "a -> bc -> d, roots of abd" $
    pFiles (roots (HS.fromList [n1, n2, pCombined]) graph2) set1
  , testCase "a -> bc -> d, roots of acd" $
    pFiles (roots (HS.fromList [n1, n4, pCombined]) graph2) set1
  , testCase "a -> bc -> d, roots of abc" $
    pFiles (roots (HS.fromList [n1, n4, n2]) graph2) set1
  , testCase "a -> bc -> d, roots of ad" $
    pFiles (roots (HS.fromList [n1, pCombined]) graph2) set1
  , testCase "a -> bc -> d, roots of abcd" $
    pFiles (roots (HS.fromList [n1, n2, n4, pCombined]) graph2) set1
  ]
  where
    n1 = PathedSiteAction (Copy "foo" "bar") Nothing
    n2 = PathedSiteAction (Copy "bar" "yay") Nothing
    n3 = PathedSiteAction (Copy "yay" "asdf") Nothing
    n4 = PathedSiteAction (Copy "bar" "asdf") Nothing
    set1 = HS.singleton n1
    set2 = HS.singleton n2
    set3 = HS.singleton n3
    graph1 =
        [Copy "foo" "bar", Copy "bar" "yay", Copy "yay" "asdf"]
    graph2 =
        [ Copy "foo" "bar"
        , Copy "bar" "yay"
        , Copy "bar" "asdf"
        , combined 
        ]
    combined = Build (NamedBuilder "combined" bb) emptyPathedParams "somewhere"
    pCombined = PathedSiteAction combined Nothing
    bb params = return $ doc (snippet "yay") [add $ snippet "asdf"]
    roots rootsOf acts = do
      graph <- fromRawActions acts
      return $ FG.getRootsOfActions rootsOf graph
