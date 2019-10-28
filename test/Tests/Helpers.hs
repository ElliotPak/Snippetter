{-# LANGUAGE OverloadedStrings #-}

module Tests.Helpers
  ( tests
  ) where

import Control.Monad.Trans.Except
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Snippetter.Build
import Snippetter.Helpers
import Test.Tasty
import Test.Tasty.HUnit
import TestAssist

tests =
  [ testGroup "Built-in Actions" testBuiltIn
  ]

testBuiltIn = [testGroup "Add" testAdd, testGroup "Replace" testReplace]

testAdd =
  [testCase "Executing" $ passIO (actExecute (add $ text "foo") "bar") "barfoo"]

testReplace =
  [ testCase "Executing" $
    passIO (actExecute (replace "x" $ text "foo") "xbarxx") "foobarfoofoo"
  ]
