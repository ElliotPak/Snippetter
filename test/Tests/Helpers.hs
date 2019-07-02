module Tests.Helpers where

import Test.Tasty.HUnit
import Snippetter.LayoutBase
import Control.Monad.Trans.Except

-- newtype MockFileReader m = MockFileReader
--     { file :: ReaderT }
--     deriving (Applicative, Functor, Monad, MonadTrans)

-- | Helper function for testing DocResults that return Right values
retPass :: (Eq a, Show a) => DocResult IO a -> a -> Assertion
retPass et expected = runExceptT et >>= assertEqual "" (Right expected)

-- | Helper function for testing DocResults that return Left values
retFail :: (Eq a, Show a) => DocResult IO a -> DocError -> Assertion
retFail et expected = runExceptT et >>= assertEqual "" (Left expected)
