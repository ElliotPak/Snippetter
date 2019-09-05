{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.Helpers where

import qualified Control.Monad.State.Lazy as S
import Control.Monad.Trans.Except
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Time.Clock
import Snippetter.IO
import System.Exit
import Test.Tasty.HUnit

data MockIOState =
  MockIOState
    { mockFilesystem :: MockFs
    , notifications :: [(NotifyType, T.Text)]
    }

type MockFs = HM.HashMap FilePath T.Text

type MockIO = S.State MockIOState

instance MonadReadWorld MockIO where
  getFileContents path = do
    state <- S.get
    if HM.member path (mockFilesystem state)
      then return $ mockFilesystem state HM.! path
      else throwE $ NotFound path
  fileExists path = HM.member path . mockFilesystem <$> S.get

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
  clearNotify =
    S.modify $ \state ->
      MockIOState (mockFilesystem state) (init $ notifications state)

changeMockFs :: (MockFs -> MockFs) -> MockIOState -> MockIOState
changeMockFs func state = MockIOState newMockFs (notifications state)
  where
    newMockFs = func (mockFilesystem state)

writeMockFile :: FilePath -> T.Text -> MockIOState -> MockIOState
writeMockFile path text = changeMockFs (HM.insert path text)

deleteMockFile :: FilePath -> MockIOState -> MockIOState
deleteMockFile path = changeMockFs (HM.delete path)

copyMockFile :: FilePath -> FilePath -> MockIOState -> MockIOState
copyMockFile from to state = changeMockFs (HM.insert to text) state
  where
    text = mockFilesystem state HM.! from

moveMockFile :: FilePath -> FilePath -> MockIOState -> MockIOState
moveMockFile from to state =
  changeMockFs (HM.delete from . HM.insert to text) state
  where
    text = mockFilesystem state HM.! from

runMockIO = flip S.runState

-- | Helper function for testing IO DocResults that return Right values
passIO :: (Eq a, Show a, Eq e, Show e) => ExceptT e IO a -> a -> Assertion
passIO et expected = runExceptT et >>= assertEqual "" (Right expected)

-- | Helper function for testing IO DocResults that return Left values
failIO :: (Eq a, Show a, Eq e, Show e) => ExceptT e IO a -> e -> Assertion
failIO et expected = runExceptT et >>= assertEqual "" (Left expected)

passMock ::
     (Eq a, Show a, Eq e, Show e)
  => MockIOState
  -> ExceptT e MockIO a
  -> a
  -> Assertion
passMock state et expected = do
  let (result, _) = runMockIO state $ runExceptT et
  assertEqual "" (Right expected) result

failMock ::
     (Eq a, Show a, Eq e, Show e)
  => MockIOState
  -> ExceptT e MockIO a
  -> e
  -> Assertion
failMock state et expected = do
  let (result, _) = runMockIO state $ runExceptT et
  assertEqual "" (Left expected) result

getMockState ::
     (Eq a, Show a, Eq e, Show e)
  => MockIOState
  -> ExceptT e MockIO a
  -> MockIOState
getMockState state et =
  let (_, state) = runMockIO state $ runExceptT et
   in state

passMockFiles files = passMock (MockIOState (HM.fromList files) [])

failMockFiles files = failMock (MockIOState (HM.fromList files) [])

getMockStateFiles files = getMockState (MockIOState (HM.fromList files) [])
