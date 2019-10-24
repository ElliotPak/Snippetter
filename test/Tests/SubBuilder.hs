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

basicBuilder = UnnamedBuilder basicBuilder'

basicBuilder' :: Builder
basicBuilder' params = do
  temp <- lookupText "title" params
  return $ text temp

sameFileBuilder = UnnamedBuilder sameFileBuilder'

sameFileBuilder' :: Builder
sameFileBuilder' params = return $ snippet "foo"

fileBuilder = UnnamedBuilder fileBuilder'

fileBuilder' :: Builder
fileBuilder' params = do
  temp <- lookupText "title" params
  return $ snippet $ T.unpack temp

fromText str =
  unObject (Y.decodeEither' (B.pack str) :: Either Y.ParseException Y.Value)
  where
    unObject (Right (Y.Object o)) = o

makePathed o = PathedParams o Nothing

params1' =  fromText "title: foo"

params2' = fromText "title: bar"

params3' = fromText "title: baz"

paramsNoTitle' = fromText "something: test"

params1 = makePathed params1'

params2 = makePathed params2'

params3 = makePathed params3'

paramsNoTitle = makePathed paramsNoTitle'

smEmpty = singleSubBuilder basicBuilder emptyParams [] []

smEmptySameFile = singleSubBuilder sameFileBuilder emptyParams [] []

smEmptyFile = singleSubBuilder fileBuilder emptyParams [] []

smSingle =
  singleSubBuilder basicBuilder emptyParams [params2] []

smSingleFile =
  singleSubBuilder fileBuilder emptyParams [params2] []

smSingleSameFile =
  singleSubBuilder sameFileBuilder emptyParams [params2] []

smMultiple =
  singleSubBuilder
    basicBuilder
    emptyParams
    [params1, params2, params3]
    []

smMultipleFile =
  singleSubBuilder
    fileBuilder
    emptyParams
    [params1, params2, params3]
    []

smMultipleSameFile =
  singleSubBuilder
    sameFileBuilder
    emptyParams
    [params1, params2, params3]
    []

smDefault =
  singleSubBuilder basicBuilder params1' [paramsNoTitle] []

smDefault2 = singleSubBuilder basicBuilder params2' [params1] []

smDefaultNoParams = singleSubBuilder basicBuilder params1' [] []

smComplexShow1 =
  singleSubBuilder basicBuilder params1' [] ["foo", "bar"]

smComplexShow2 =
  singleSubBuilder basicBuilder emptyParams [] ["foo", "bar"]

smComplexShow3 =
  singleSubBuilder basicBuilder params1' [params2, params3]
  ["foo", "bar"]

smComplexShow4 =
  singleSubBuilder
    basicBuilder
    emptyParams
    [params2, params3]
  ["foo", "bar"]

smTwoBuilders1 = 
  subBuilder
    [ SubBuilderExec basicBuilder emptyParams [params1] []
    , SubBuilderExec fileBuilder emptyParams [params2] []
    ]

smTwoBuilders2 = 
  subBuilder
    [ SubBuilderExec basicBuilder emptyParams [params2] []
    , SubBuilderExec basicBuilder emptyParams [params1] []
    ]

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
  [ testCase "No params" $ passIO (conPreview smEmpty) "Builder executions:"
  , testCase "Single params" $
    passIO
      (conPreview smSingle)
      "Builder executions:\n  - Execution with these params:\n      - title: bar"
  , testCase "Multiple params" $
    passIO
      (conPreview smMultiple)
      "Builder executions:\n  - Execution with these params:\n      - title: foo\n      - title: bar\n      - title: baz"
  , testCase "Complex preview 1" $
    passMockFiles
      files
      (conPreview smComplexShow1)
      "Builder executions:\n  - Execution with these params:\n      - title: from-file\n      - title: from-file"
  , testCase "Complex preview 2" $
    passMockFiles
      files
      (conPreview smComplexShow2)
      "Builder executions:\n  - Execution with these params:\n      - title: from-file\n      - title: from-file"
  , testCase "Complex preview 3" $
    passMockFiles
      files
      (conPreview smComplexShow3)
      "Builder executions:\n  - Execution with these params:\n      - title: bar\n      - title: baz\n      - title: from-file\n      - title: from-file"
  , testCase "Complex preview 4" $
    passMockFiles
      files
      (conPreview smComplexShow4)
      "Builder executions:\n  - Execution with these params:\n      - title: bar\n      - title: baz\n      - title: from-file\n      - title: from-file"
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
  ]
