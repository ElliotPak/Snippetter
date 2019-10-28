{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module TestAssist where

import qualified Control.Monad.State.Lazy as S
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
import Snippetter.IO
import Snippetter.Utilities
import System.Exit
import Test.Tasty.HUnit

data MockIOState =
  MockIOState
    { mockFilesystem :: MockFs
    , notifications :: [(NotifyType, T.Text)]
    , time :: UTCTime
    }

type MockFs = HM.HashMap FilePath (T.Text, UTCTime)

type MockIO = S.State MockIOState

instance MonadReadWorld MockIO where
  getFileContents path = do
    state <- S.get
    if HM.member path (mockFilesystem state)
      then return $ fst $ mockFilesystem state HM.! path
      else resultE $ NotFound path
  fileExists path = HM.member path . mockFilesystem <$> S.get
  fileModifyTime path = do
    state <- S.get
    if HM.member path (mockFilesystem state)
      then return $ snd $ mockFilesystem state HM.! path
      else resultE $ NotFound path
  currentTime = S.gets time

instance MonadWriteWorld MockIO where
  writeFile path text = S.modify (writeMockFile path text)
  deleteFile path = S.modify (deleteMockFile path)
  copyFile from to = S.modify (copyMockFile from to)
  moveFile from to = S.modify (moveMockFile from to)
  runProcess name args stdin = return (ExitSuccess, "", "")
  notifyUser typ message =
    S.modify $ \state ->
      MockIOState
        (mockFilesystem state)
        (notifications state ++ [(typ, message)])
        (incrementTime state)
  clearNotify =
    S.modify $ \state ->
      MockIOState
        (mockFilesystem state)
        (init $ notifications state)
        (time state)

incrementTime :: MockIOState -> UTCTime
incrementTime state = addUTCTime 1 $ time state

changeMockFs :: (MockFs -> MockFs) -> MockIOState -> MockIOState
changeMockFs func state =
  MockIOState newMockFs (notifications state) (incrementTime state)
  where
    newMockFs = func (mockFilesystem state)

writeMockFile :: FilePath -> T.Text -> MockIOState -> MockIOState
writeMockFile path text state =
  changeMockFs (HM.insert path (text, time state)) state

deleteMockFile :: FilePath -> MockIOState -> MockIOState
deleteMockFile path = changeMockFs (HM.delete path)

copyMockFile :: FilePath -> FilePath -> MockIOState -> MockIOState
copyMockFile from to state =
  changeMockFs (HM.insert to (text, time state)) state
  where
    text = fst $ mockFilesystem state HM.! from

moveMockFile :: FilePath -> FilePath -> MockIOState -> MockIOState
moveMockFile from to state =
  changeMockFs (HM.delete from . HM.insert to (text, time state)) state
  where
    text = fst $ mockFilesystem state HM.! from

runMockIO = flip S.runState

-- | Helper function for testing IO DocResults that return Right values
passIO :: (Eq a, Show a, Eq e, Show e) => Result e IO a -> a -> Assertion
passIO et expected = runResult et >>= assertEqual "" (Right expected)

-- | Helper function for testing IO DocResults that return Left values
failIO :: (Eq a, Show a, Eq e, Show e) => Result e IO a -> e -> Assertion
failIO et expected = runResult et >>= assertEqual "" (Left expected)

passMock ::
     (Eq a, Show a, Eq e, Show e)
  => MockIOState
  -> Result e MockIO a
  -> a
  -> Assertion
passMock state et expected = do
  let (result, _) = runMockIO state $ runResult et
  assertEqual "" (Right expected) result

failMock ::
     (Eq a, Show a, Eq e, Show e)
  => MockIOState
  -> Result e MockIO a
  -> e
  -> Assertion
failMock state et expected = do
  let (result, _) = runMockIO state $ runResult et
  assertEqual "" (Left expected) result

getMockState ::
     (Eq a, Show a, Eq e, Show e)
  => MockIOState
  -> Result e MockIO a
  -> MockIOState
getMockState state et =
  let (_, state) = runMockIO state $ runResult et
   in state

startTime = UTCTime (fromGregorian 2019 1 1) 0

startTimePlus digit = UTCTime (fromGregorian 2019 1 1) (realToFrac digit)

startTimeMinus digit = UTCTime (fromGregorian 2019 1 1) (realToFrac (-digit))

passMockFiles files = passMock (MockIOState (HM.fromList files) [] startTime)

failMockFiles files = failMock (MockIOState (HM.fromList files) [] startTime)

getMockStateFiles files =
  getMockState (MockIOState (HM.fromList files) [] startTime)
