module Hssb.Layout where

import Control.Monad.Trans.Except
import Control.Monad.Except
import Data.Aeson.Types (Object, Value (Object, String))
import Data.HashMap.Strict (HashMap, lookup)
import Data.Yaml ((.:))
import Hssb.Layout.Types
import Prelude hiding (lookup)
import System.Directory (doesFileExist)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Data.Yaml as Y

contentsIfExists :: MonadReadFile m => FilePath -> DocResult m T.Text
contentsIfExists path = do
    exists <- lift $ fileExists path
    case exists of
      False -> throwE $ InvalidPath path
      True  -> getFileContents path

decodeValue :: T.Text -> Either Y.ParseException Value
decodeValue str = Y.decodeEither' $ B.pack $ T.unpack str

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left $ f x
mapLeft _ (Right x) = Right x

valueIfExists :: MonadReadFile m => FilePath -> DocResult m Value
valueIfExists path = do
    contents <- contentsIfExists path
    liftEither $ mapLeft (\x -> NotYaml path) $ decodeValue contents

loadEntryFile :: MonadReadFile m => Macro -> FilePath -> DocResult m T.Text
loadEntryFile macro path = do
    value <- valueIfExists path
    doc <- liftEither $ decodeEntryFile macro path value
    resolved <- resolveContents doc
    return (execute T.empty resolved)

decodeEntryFile :: Macro -> FilePath -> Value -> MacroResult
decodeEntryFile macro path (Object params) = macro params
decodeEntryFile _     path _               = Left $ InvalidFileFormat path
