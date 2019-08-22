{-# LANGUAGE OverloadedStrings #-}

module Tests.Utilities
  ( tests
  ) where

import qualified Data.Text as T
import Snippetter.Utilities
import Test.Tasty
import Test.Tasty.HUnit
import Tests.Helpers

tests =
  [ testGroup "indentText" testIndentText
  , testGroup "addSingleLineText" testAddSingleLineText
  , testGroup "appendWithNewLine" testAppendWithNewLine
  ]

testIndentText =
  [ testCase "Indenting no spaces, single line" $ indentText 0 "foo" @?= "foo"
  , testCase "Indenting no spaces, multiple lines" $
    indentText 0 "foo\nbar\nbaz" @?= "foo\nbar\nbaz"
  , testCase "Indenting spaces, single line" $ indentText 4 "foo" @?= "    foo"
  , testCase "Indenting spaces, multiple lines" $
    indentText 4 "foo\nbar\nbaz" @?= "    foo\n    bar\n    baz"
  , testCase "Indenting spaces, multiple lines, different indents" $
    indentText 4 "foo\n       bar\n   baz" @?=
    "    foo\n           bar\n       baz"
  ]

testAddSingleLineText =
  [ testCase "Text is a single line" $ "foo " <\> "bar baz" @?= "foo bar baz"
  , testCase "Text is not a single line" $
    "foo " <\> "bar\nbaz" @?= "foo \nbar\nbaz"
  ]

testAppendWithNewLine =
  [ testCase "both are empty" $ "" <\\> "" @?= ""
  , testCase "aa is empty, bb is not" $ "" <\\> "bar" @?= "bar"
  , testCase "bb is empty, aa is not" $ "foo" <\\> "" @?= "foo"
  , testCase "neither is empty" $ "foo" <\\> "bar" @?= "foo\nbar"
  ]
