{-# LANGUAGE RecordWildCards #-}

module Hssb.Layout where

import Hssb.Data
import System.Directory (doesFileExist)
import Data.Aeson.Types (Value(Object), Object)
import Data.HashMap.Strict (HashMap, lookup)
import Data.Yaml ((.:))
import Control.Monad.Trans.Except
import Control.Monad.Except
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Data.Yaml as Y

type IODocResult a = ExceptT DocError IO a

data LayoutFile = LayoutFile {
    output    :: String,
    macroName :: String,
    contents  :: HashMap T.Text Value
} deriving (Show)

instance Y.FromJSON LayoutFile where
    parseJSON = Y.withObject "LayoutFile" $ \o -> do
        output <- o .: T.pack "output"
        macroName <- o .: T.pack "macro-name"
        let contents = o
        return LayoutFile{..}

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left $ f x
mapLeft _ (Right x) = Right x

contentsIfExists :: FilePath -> IODocResult String
contentsIfExists path = do
    exists <- liftIO $ doesFileExist path
    case exists of
      False -> throwE $ InvalidPath path
      True  -> liftIO $ readFile path

decodeValue :: String -> Either Y.ParseException Value
decodeValue str = Y.decodeEither' $ B.pack str

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

applyMacrosToFile :: Doc -> IODocResult Doc
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
decodeEntryFile macro path layout =
    case layout of
      Object params -> macro params
      otherwise     -> Left  $ InvalidFileFormat path
