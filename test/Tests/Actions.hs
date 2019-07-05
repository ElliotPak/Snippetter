{-# LANGUAGE OverloadedStrings #-}

module Tests.Actions (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Tests.Helpers
import Snippetter.LayoutBase
import Snippetter.LayoutTypes
import Control.Monad.Trans.Except
import qualified Data.Text as T

tests =
    [ testGroup "Add" testAdd
    , testGroup "Replace" testReplace
    ]

testAdd = []
testReplace = []
