module Hssb.Layout where

import Control.Monad.Trans.Except
import Control.Monad.Except
import Data.Aeson.Types (Object, Value (Object, String, Array))
import Data.HashMap.Strict (HashMap, lookup)
import Data.Vector
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

determineNeededFiles :: MonadReadFile m =>
                        Macro -> FilePath -> DocResult m [FilePath]
determineNeededFiles macro path = do
    doc <- loadEntryFile macro path
    getNeededFiles doc

loadEntryFile :: MonadReadFile m => Macro -> FilePath -> DocResult m Doc
loadEntryFile macro path = do
    value <- valueIfExists path
    liftEither $ executeMacro macro path value

executeEntryFile :: MonadReadFile m => Macro -> FilePath -> DocResult m T.Text
executeEntryFile macro path = do
    doc <- loadEntryFile macro path
    resolved <- resolveContents doc
    return (execute T.empty resolved)

executeMacro :: Macro -> FilePath -> Value -> MacroResult
executeMacro macro path (Object params) = macro params
executeMacro macro path (Array vec)     = do
    let l = toList vec
    mapped <- Prelude.mapM (executeMacro macro path) l :: Either DocError [Doc]
    return $ Prelude.concat mapped
executeMacro _     path _               = Left $ InvalidFileFormat path

instance NeedsFiles MacroOnFile where
    getNeededFiles (MacroOnFile m f) = do
        containing <- determineNeededFiles m f
        return $ f : containing
instance Contentable MacroOnFile where
    resolve (MacroOnFile m f) = do
        doc <- executeEntryFile m f
        return doc
