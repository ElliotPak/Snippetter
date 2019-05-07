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

contentsIfExists :: FilePath -> IODocResult String
contentsIfExists path = do
    exists <- liftIO $ doesFileExist path
    case exists of
      False -> throwE $ InvalidPath path
      True  -> liftIO $ readFile path

decodeValue :: String -> Either Y.ParseException Value
decodeValue str = Y.decodeEither' $ B.pack str

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left $ f x
mapLeft _ (Right x) = Right x

valueIfExists :: FilePath -> IODocResult Value
valueIfExists path = do
    contents <- contentsIfExists path
    liftEither $ mapLeft (\x -> NotYaml path) $ decodeValue contents

loadEntryFile :: Macro -> FilePath -> IODocResult Doc
loadEntryFile macro path = do
    value <- valueIfExists path
    doc <- liftEither $ decodeEntryFile macro path value
    withMacros <- applyMacrosToFile doc
    return withMacros

applyMacrosToFile = mapM applyMacrosToContent

applyMacrosToContent :: Content -> IODocResult Content
applyMacrosToContent (ApplyMacroToFile m f) = do
    subEntry <- loadEntryFile m f
    return $ SubDocument subEntry
applyMacrosToContent (Replacement s d) = do
    replacement <- applyMacrosToFile d
    return $ Replacement s replacement
applyMacrosToContent c = return c

decodeEntryFile :: Macro -> FilePath -> Value -> MacroResult
decodeEntryFile macro path (Object params) = macro params
decodeEntryFile _     path _               = Left $ InvalidFileFormat path
