{-# LANGUAGE OverloadedStrings #-}

module Tests.Actions
  ( tests
  ) where

import Control.Monad.Trans.Except
import qualified Data.Text as T
import Snippetter.Build
import Test.Tasty
import Test.Tasty.HUnit
import Tests.Helpers

tests = [testGroup "Add" testAdd, testGroup "Replace" testReplace]

testAdd = []

testReplace = []
