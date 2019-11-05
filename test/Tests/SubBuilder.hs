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
import TestAssist

files =
  [ ("foo", ("- title: from-file", startTime))
  , ("bar", ("- title: from-file", startTime))
  , ("baz", ("- title: from-file", startTime))
  ]

tests =
  [ testGroup "File deps" testSubBuilderFiles
  , testGroup "Preview" testSubBuilderPreview
  , testGroup "Show" testSubBuilderShow
  , testGroup "Resolving" testSubBuilderEvaluate
  ]

basicBuilder = NamedPageBuilder "basic" basicBuilder'

basicBuilder' :: PageBuilder
basicBuilder' params = do
  temp <- lookupText "title" params
  return $ text temp

sameFileBuilder = NamedPageBuilder "same" sameFileBuilder'

sameFileBuilder' :: PageBuilder
sameFileBuilder' params = return $ snippet "foo"

fileBuilder = NamedPageBuilder "file" fileBuilder'

fileBuilder' :: PageBuilder
fileBuilder' params = do
  temp <- lookupText "title" params
  return $ snippet $ T.unpack temp

params1' =  paramsFromText "title: foo"

params2' = paramsFromText "title: bar"

params3' = paramsFromText "title: baz"

paramsNoTitle' = paramsFromText "something: test"

params1 = pathedP params1'

params2 = pathedP params2'

params3 = pathedP params3'

paramsNoTitle = pathedP paramsNoTitle'

smEmpty = singleSubBuilder basicBuilder emptyParams [] [] id

smEmptySameFile = singleSubBuilder sameFileBuilder emptyParams [] [] id

smEmptyFile = singleSubBuilder fileBuilder emptyParams [] [] id

smSingle =
  singleSubBuilder basicBuilder emptyParams [params2] [] id

smSingleFile =
  singleSubBuilder fileBuilder emptyParams [params2] [] id

smSingleSameFile =
  singleSubBuilder sameFileBuilder emptyParams [params2] [] id

smMultiple =
  singleSubBuilder
    basicBuilder
    emptyParams
    [params1, params2, params3]
    [] id

smMultipleFile =
  singleSubBuilder
    fileBuilder
    emptyParams
    [params1, params2, params3]
    [] id

smMultipleSameFile =
  singleSubBuilder
    sameFileBuilder
    emptyParams
    [params1, params2, params3]
    [] id

smDefault =
  singleSubBuilder basicBuilder params1' [paramsNoTitle] [] id

smDefault2 = singleSubBuilder basicBuilder params2' [params1] [] id

smDefaultNoParams = singleSubBuilder basicBuilder params1' [] [] id

smComplexShow1 =
  singleSubBuilder basicBuilder params1' [] ["foo", "bar"] id

smComplexShow2 =
  singleSubBuilder basicBuilder emptyParams [] ["foo", "bar"] id

smComplexShow3 =
  singleSubBuilder basicBuilder params1' [params2, params3]
  ["foo", "bar"] id

smComplexShow4 =
  singleSubBuilder
    basicBuilder
    emptyParams
    [params2, params3]
  ["foo", "bar"] id

smTwoBuilders1 = 
  subBuilder
    [ SubBuilderExec basicBuilder emptyParams [params1] [] id
    , SubBuilderExec fileBuilder emptyParams [params2] [] id
    ] id

smTwoBuilders2 = 
  subBuilder
    [ SubBuilderExec basicBuilder emptyParams [params2] [] id
    , SubBuilderExec basicBuilder emptyParams [params1] [] id
    ] id

smFunc' aa bb = 
  subBuilder
    [ SubBuilderExec basicBuilder emptyParams [params1, params2, params3] [] aa
    ] bb

smFunc'' aa bb = 
  subBuilder
    [ SubBuilderExec fileBuilder emptyParams [params1, params2, params3] [] aa
    ] bb

smFunc''' aa bb cc = 
  subBuilder
    [ SubBuilderExec fileBuilder emptyParams [params1, params2, params3] [] aa
    , SubBuilderExec basicBuilder emptyParams [params1, params2, params3] [] bb
    ] cc

smFunc1 = smFunc' (\a -> return $ head a) id

smFunc2 = smFunc' (tail) id

smFunc3 = smFunc'' (\a -> return $ head a) id

smFunc4 = smFunc'' (tail) id

testSubBuilderFiles =
  [ testCase "Empty, Builder uses no files" $
    passIO (conNeededFiles smEmpty) HS.empty
  , testCase "Empty, Builder uses the same file constantly" $
    passIO (conNeededFiles smEmptySameFile) HS.empty
  , testCase "Empty, Builder uses file specified in params" $
    passIO (conNeededFiles smEmptyFile) HS.empty
  , testCase "Single params, macro uses no files" $
    passIO (conNeededFiles smSingle) HS.empty
  , testCase "Single params, macro uses same file constantly" $
    passIO (conNeededFiles smSingleSameFile) $ HS.fromList ["foo"]
  , testCase "Single params, macro uses file specified in params" $
    passIO (conNeededFiles smSingleFile) $ HS.fromList ["bar"]
  , testCase "Multiple params, macro uses no files" $
    passIO (conNeededFiles smMultiple) HS.empty
  , testCase "Multiple params, macro uses same file constantly" $
    passIO (conNeededFiles smMultipleSameFile) $ HS.fromList ["foo"]
  , testCase "Multiple params, macro uses file specified in params" $
    passIO (conNeededFiles smMultipleFile) $ HS.fromList ["foo", "bar", "baz"]
  , testCase "ListFunc 1 - head" $
    passIO (conNeededFiles smFunc3) $ HS.fromList ["foo"]
  , testCase "ListFunc 1 - tail" $
    passIO (conNeededFiles smFunc4) $ HS.fromList ["bar", "baz"]
  ]

testSubBuilderShow =
  [ testCase "No params" $ conShow smEmpty @?= "Builder executions:"
  , testCase "Single params" $
    conShow smSingle @?=
    "Builder executions:\n  - Execution with these params:\n      - title: bar"
  , testCase "Multiple params" $
    conShow smMultiple @?=
    "Builder executions:\n  - Execution with these params:\n      - title: foo\n      - title: bar\n      - title: baz"
  , testCase "Complex show 1" $
    conShow smComplexShow1 @?=
    "Builder executions:\n  - Default values:\n      - title: foo\n    Execution on these files:\n      - foo\n      - bar"
  , testCase "Complex show 2" $
    conShow smComplexShow2 @?=
    "Builder executions:\n  - Execution on these files:\n      - foo\n      - bar"
  , testCase "Complex show 3" $
    conShow smComplexShow3 @?=
    "Builder executions:\n  - Default values:\n      - title: foo\n    Execution with these params:\n      - title: bar\n      - title: baz\n    Execution on these files:\n      - foo\n      - bar"
  , testCase "Complex show 4" $
    conShow smComplexShow4 @?=
    "Builder executions:\n  - Execution with these params:\n      - title: bar\n      - title: baz\n    Execution on these files:\n      - foo\n      - bar"
  ]

testSubBuilderPreview =
  [ testCase "No params" $ passIO (conPreview smEmpty) "Empty sub-builder execution"
  , testCase "Single params" $
    passIO
      (conPreview smSingle)
      "The following actions:\n  - Add: \"bar\""
  , testCase "Multiple params" $
    passIO
      (conPreview smMultiple)
      "The following actions:\n  - Add: \"foo\"\n  - Add: \"bar\"\n  - Add: \"baz\""
  , testCase "Complex preview 1" $
    passMockFiles
      files
      (conPreview smComplexShow1)
      "The following actions:\n  - Add: \"from-file\"\n  - Add: \"from-file\""
  , testCase "Complex preview 2" $
    passMockFiles
      files
      (conPreview smComplexShow2)
      "The following actions:\n  - Add: \"from-file\"\n  - Add: \"from-file\""
  , testCase "Complex preview 3" $
    passMockFiles
      files
      (conPreview smComplexShow3)
      "The following actions:\n  - Add: \"bar\"\n  - Add: \"baz\"\n  - Add: \"from-file\"\n  - Add: \"from-file\""
  , testCase "Complex preview 4" $
    passMockFiles
      files
      (conPreview smComplexShow4)
      "The following actions:\n  - Add: \"bar\"\n  - Add: \"baz\"\n  - Add: \"from-file\"\n  - Add: \"from-file\""
  ]

testSubBuilderEvaluate =
  [ testCase "No params" $ passIO (conEvaluate smEmpty) (T.pack "")
  , testCase "No params with default" $
    passIO (conEvaluate smDefaultNoParams) (T.pack "")
  , testCase "Single params" $ passIO (conEvaluate smSingle) (T.pack "bar")
  , testCase "Single params with required default" $
    passIO (conEvaluate Tests.SubBuilder.smDefault) (T.pack "foo")
  , testCase "Single params with overwritten default" $
    passIO (conEvaluate smDefault2) (T.pack "foo")
  , testCase "Multiple params" $
    passIO (conEvaluate smMultiple) (T.pack "foobarbaz")
  , testCase "Two builders 1" $
    passMockFiles files (conEvaluate smTwoBuilders1) (T.pack "foo- title: from-file")
  , testCase "Two builders (that are the same)" $
    passMockFiles files (conEvaluate smTwoBuilders2) (T.pack "barfoo")
  , testCase "ListFunc 1 - head" $
    passMockFiles files (conEvaluate smFunc1) (T.pack "foo")
  , testCase "ListFunc 1 - tail" $
    passMockFiles files (conEvaluate smFunc2) (T.pack "barbaz")
  , testCase "ListFunc 2 - head" $
    passMockFiles files (conEvaluate smFunc3) (T.pack "- title: from-file")
  , testCase "ListFunc 2 - tail" $
    passMockFiles files (conEvaluate smFunc4) (T.pack "- title: from-file- title: from-file")
  ]
