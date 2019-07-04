{-# LANGUAGE FlexibleInstances #-}

module Tests.Helpers where

import Test.Tasty.HUnit
import Snippetter.LayoutBase
import Control.Monad.Trans.Except
import qualified Data.Text as T
import qualified Control.Monad.State.Lazy as S

data MockIOState = MockIOState
    { mockIOWritten  :: [String]
    , mockIORead     :: DocResult MockIO T.Text
    , mockIOExists   :: MockIO Bool
    }

type MockIO = S.State MockIOState

instance MonadReadFile MockIO where
    getFileContents s = S.get >>= mockIORead
    fileExists s      = S.get >>= mockIOExists

runMockIO = flip S.runState

successfulRead :: T.Text -> MockIOState
successfulRead text = MockIOState
    { mockIOWritten = []
    , mockIORead = return text
    , mockIOExists = return True
    }

emptyRead = successfulRead T.empty

missingFile :: FilePath -> MockIOState
missingFile path = MockIOState
    { mockIOWritten = []
    , mockIORead = throwE $ InvalidPath path
    , mockIOExists = return False
    }

-- | Helper function for testing IO DocResults that return Right values
retPassIO :: (Eq a, Show a) => DocResult IO a -> a -> Assertion
retPassIO et expected = runExceptT et >>= assertEqual "" (Right expected)

-- | Helper function for testing IO DocResults that return Left values
retFailIO :: (Eq a, Show a) => DocResult IO a -> DocError -> Assertion
retFailIO et expected = runExceptT et >>= assertEqual "" (Left expected)

retPassMock :: (Eq a, Show a) =>
    MockIOState -> DocResult MockIO a -> a -> Assertion
retPassMock state et expected = do
    let (result, _) = runMockIO state $ runExceptT et
    assertEqual "" (Right expected) result

retFailMock :: (Eq a, Show a) =>
    MockIOState -> DocResult MockIO a -> DocError -> Assertion
retFailMock state et expected = do
    let (result, _) = runMockIO state $ runExceptT et
    assertEqual "" (Left expected) result

retPassFileRead contents = retPassMock $ successfulRead contents
retPassMissing file = retFailMock $ missingFile file
