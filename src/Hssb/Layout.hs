module Hssb.Layout where

import Hssb.Data
import System.Directory (doesFileExist)
import Data.Aeson.Types (Value(Object), Object)
import qualified Data.ByteString.Char8 as B
import qualified Data.Yaml as Y

loadEntryFile :: Macro -> FilePath -> IO (MacroResult)
loadEntryFile macro path = do
    exists <- doesFileExist path
    if (not exists) then return $ Left $ InvalidLayoutFile path
    else decodeEntryFile macro path

decodeEntryFile :: Macro -> FilePath -> IO (MacroResult)
decodeEntryFile macro path = do
    contents <- readFile path
    let layout = Y.decodeEither' $ B.pack contents :: Either Y.ParseException Value
    return $ case layout of
      Right (Object params) -> macro params
      otherwise             -> Left $ InvalidLayoutFile path
