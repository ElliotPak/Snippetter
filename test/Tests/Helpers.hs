module Tests.Helpers where

import Snippetter.LayoutBase

-- newtype MockFileReader m = MockFileReader
--     { file :: ReaderT }
--     deriving (Applicative, Functor, Monad, MonadTrans)

wrapReadFileValue :: a -> DocResult IO a
wrapReadFileValue val = return val
