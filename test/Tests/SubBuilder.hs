{-# LANGUAGE OverloadedStrings #-}

module Tests.SubBuilder
  ( tests
  ) where

import Control.Monad.Trans.Except
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Yaml as Y
import Snippetter.Build
import Snippetter.Helpers
import Snippetter.Utilities
import Test.Tasty
import Test.Tasty.HUnit
import Tests.Helpers

tests =
  [ testGroup "File deps" testSubBuilderFiles
  , testGroup "Preview" testSubBuilderPreview
  , testGroup "Show" testSubBuilderShow
  , testGroup "Resolving" testSubBuilderEvaluate
  ]

basicBuilder :: Builder
basicBuilder params = do
  temp <- lookupText "title" params
  return [add $ text temp]

sameFileBuilder :: Builder
sameFileBuilder params = return [add $ Snippet "foo"]

fileBuilder :: Builder
fileBuilder params = do
  temp <- lookupText "title" params
  return [add $ Snippet $ T.unpack temp]

fromText str =
  unObject (Y.decodeEither' (B.pack str) :: Either Y.ParseException Y.Value)
  where
    unObject (Right (Y.Object o)) = o

makePathed o = PathedParams o Nothing

params1 = fromText "title: foo"

params2 = fromText "title: bar"

params3 = fromText "title: baz"

paramsNoTitle = fromText "something: test"

smEmpty = singleSubBuilder basicBuilder emptyParams [] HS.empty

smEmptySameFile = singleSubBuilder sameFileBuilder emptyParams [] HS.empty

smEmptyFile = singleSubBuilder fileBuilder emptyParams [] HS.empty

smSingle =
  singleSubBuilder basicBuilder emptyParams [makePathed params2] HS.empty

smSingleFile =
  singleSubBuilder fileBuilder emptyParams [makePathed params2] HS.empty

smSingleSameFile =
  singleSubBuilder sameFileBuilder emptyParams [makePathed params2] HS.empty

smMultiple =
  singleSubBuilder
    basicBuilder
    emptyParams
    [makePathed params1, makePathed params2, makePathed params3]
    HS.empty

smMultipleFile =
  singleSubBuilder
    fileBuilder
    emptyParams
    [makePathed params1, makePathed params2, makePathed params3]
    HS.empty

smMultipleSameFile =
  singleSubBuilder
    sameFileBuilder
    emptyParams
    [makePathed params1, makePathed params2, makePathed params3]
    HS.empty

smDefault =
  singleSubBuilder basicBuilder params1 [makePathed paramsNoTitle] HS.empty

smDefault2 = singleSubBuilder basicBuilder params2 [makePathed params1] HS.empty

smDefaultNoParams = singleSubBuilder basicBuilder params1 [] HS.empty

smComplexShow1 =
  singleSubBuilder basicBuilder params1 [] $ HS.fromList ["foo", "bar"]

smComplexShow2 =
  singleSubBuilder basicBuilder emptyParams [] $ HS.fromList ["foo", "bar"]

smComplexShow3 =
  singleSubBuilder basicBuilder params1 [makePathed params2, makePathed params3] $
  HS.fromList ["foo", "bar"]

smComplexShow4 =
  singleSubBuilder
    basicBuilder
    emptyParams
    [makePathed params2, makePathed params3] $
  HS.fromList ["foo", "bar"]

testSubBuilderFiles =
  [ testCase "Empty, Builder uses no files" $
    retPassIO (conNeededFiles smEmpty) HS.empty
  , testCase "Empty, Builder uses the same file constantly" $
    retPassIO (conNeededFiles smEmptySameFile) HS.empty
  , testCase "Empty, Builder uses file specified in params" $
    retPassIO (conNeededFiles smEmptyFile) HS.empty
  , testCase "Single params, macro uses no files" $
    retPassIO (conNeededFiles smSingle) HS.empty
  , testCase "Single params, macro uses same file constantly" $
    retPassIO (conNeededFiles smSingleSameFile) $ HS.fromList ["foo"]
  , testCase "Single params, macro uses file specified in params" $
    retPassIO (conNeededFiles smSingleFile) $ HS.fromList ["bar"]
  , testCase "Multiple params, macro uses no files" $
    retPassIO (conNeededFiles smMultiple) HS.empty
  , testCase "Multiple params, macro uses same file constantly" $
    retPassIO (conNeededFiles smMultipleSameFile) $ HS.fromList ["foo"]
  , testCase "Multiple params, macro uses file specified in params" $
    retPassIO (conNeededFiles smMultipleFile) $
    HS.fromList ["foo", "bar", "baz"]
  ]

testSubBuilderShow =
  [ testCase "No params" $ conShow smEmpty @?= "Builder executions:"
  , testCase "Single params" $
    conShow smSingle @?=
    "Builder executions:\n    Execution with these params:\n      - title: bar"
  , testCase "Multiple params" $
    conShow smMultiple @?=
    "Builder executions:\n    Execution with these params:\n      - title: foo\n      - title: bar\n      - title: baz"
  , testCase "Complex show 1" $
    conShow smComplexShow1 @?=
    "Builder executions:\n    Default values:\n      - title: foo\n    Execution on these files:\n      - bar\n      - foo"
  , testCase "Complex show 2" $
    conShow smComplexShow2 @?=
    "Builder executions:\n    Execution on these files:\n      - bar\n      - foo"
  , testCase "Complex show 3" $
    conShow smComplexShow3 @?=
    "Builder executions:\n    Default values:\n      - title: foo\n    Execution with these params:\n      - title: bar\n      - title: baz\n    Execution on these files:\n      - bar\n      - foo"
  , testCase "Complex show 4" $
    conShow smComplexShow4 @?=
    "Builder executions:\n    Execution with these params:\n      - title: bar\n      - title: baz\n    Execution on these files:\n      - bar\n      - foo"
  ]

testSubBuilderPreview =
  [ testCase "No params" $ retPassIO (conPreview smEmpty) "Builder executions:"
  , testCase "Single params" $
    retPassIO
      (conPreview smSingle)
      "Builder executions:\n    Execution with these params:\n      - title: bar"
  , testCase "Multiple params" $
    retPassIO
      (conPreview smMultiple)
      "Builder executions:\n    Execution with these params:\n      - title: foo\n      - title: bar\n      - title: baz"
  , testCase "Complex preview 1" $
    retPassFileRead
      "- title: from-file"
      (conPreview smComplexShow1)
      "Builder executions:\n    Execution with these params:\n      - title: from-file\n      - title: from-file"
  , testCase "Complex preview 2" $
    retPassFileRead
      "- title: from-file"
      (conPreview smComplexShow2)
      "Builder executions:\n    Execution with these params:\n      - title: from-file\n      - title: from-file"
  , testCase "Complex preview 3" $
    retPassFileRead
      "- title: from-file"
      (conPreview smComplexShow3)
      "Builder executions:\n    Execution with these params:\n      - title: bar\n      - title: baz\n      - title: from-file\n      - title: from-file"
  , testCase "Complex preview 4" $
    retPassFileRead
      "- title: from-file"
      (conPreview smComplexShow4)
      "Builder executions:\n    Execution with these params:\n      - title: bar\n      - title: baz\n      - title: from-file\n      - title: from-file"
  ]

testSubBuilderEvaluate =
  [ testCase "No params" $ retPassIO (conEvaluate smEmpty) (T.pack "")
  , testCase "No params with default" $
    retPassIO (conEvaluate smDefaultNoParams) (T.pack "")
  , testCase "Single params" $ retPassIO (conEvaluate smSingle) (T.pack "bar")
  , testCase "Single params with required default" $
    retPassIO (conEvaluate Tests.SubBuilder.smDefault) (T.pack "foo")
  , testCase "Single params with overwritten default" $
    retPassIO (conEvaluate smDefault2) (T.pack "foo")
  , testCase "Multiple params" $
    retPassIO (conEvaluate smMultiple) (T.pack "foobarbaz")
  ]
