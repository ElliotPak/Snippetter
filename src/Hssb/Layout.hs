module Hssb.Layout where

import Control.Monad.Trans.Except
import Control.Monad.Except
import Data.Aeson.Types (Object, Value (Object, String, Array))
import Data.HashMap.Strict (HashMap, lookup)
import Data.Yaml ((.:))
import Hssb.Layout.Types
import Hssb.Utilities
import Prelude hiding (lookup)
import System.Directory (doesFileExist)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Data.Yaml as Y

data SiteAction =
    Build Macro MacroParams FilePath |
    Copy FilePath FilePath
    deriving (Show)

contentsIfExists :: MonadReadFile m => FilePath -> DocResult m T.Text
contentsIfExists path = do
    exists <- lift $ fileExists path
    case exists of
      False -> throwE $ InvalidPath path
      True  -> getFileContents path

decodeYaml :: Y.FromJSON a => T.Text -> Either Y.ParseException a
decodeYaml str = Y.decodeEither' $ B.pack $ T.unpack str

yamlIfExists :: (MonadReadFile m, Y.FromJSON a) =>
    FilePath -> DocResult m a
yamlIfExists path = do
    contents <- contentsIfExists path
    liftEither $ mapLeft (\x -> NotYaml path) $ decodeYaml contents

valueIfExists :: MonadReadFile m => FilePath -> DocResult m Value
valueIfExists = yamlIfExists

loadLayoutFile :: MonadReadFile m => FilePath -> DocResult m LayoutFile
loadLayoutFile = yamlIfExists

layoutToAction :: HashMap String Macro -> LayoutFile -> Either DocError SiteAction
layoutToAction map layout = do
    let mp = contents layout
    let f  = output layout
    let mn = macroName layout
    m <- lookupEither (MissingMacro mn) mn map
    return $ Build m mp f

loadBuildAction :: MonadReadFile m =>
    FilePath -> HashMap String Macro -> DocResult m SiteAction
loadBuildAction path map = do
    layout <- loadLayoutFile path
    liftEither $ layoutToAction map layout

filesNeeded :: MonadReadFile m => Macro -> MacroParams -> DocResult m [FilePath]
filesNeeded m mp = (liftEither $ m mp) >>= getNeededFiles

filesNeededForAction :: MonadReadFile m => SiteAction -> DocResult m [FilePath]
filesNeededForAction (Build m mp f) = filesNeeded m mp

executeSiteAction :: MonadReadFile m => SiteAction -> DocResult m T.Text
executeSiteAction (Build m mp f) = do 
    executed <- liftEither $ m mp
    resolveContents executed T.empty
executeSiteAction _              = undefined

executeLayoutFile :: MonadReadFile m =>
    FilePath -> HashMap String Macro -> DocResult m T.Text
executeLayoutFile path map = loadBuildAction path map >>= executeSiteAction

macroOnEntryFile :: MonadReadFile m => Macro -> FilePath -> DocResult m Doc
macroOnEntryFile macro path = do
    values <- yamlIfExists path :: MonadReadFile m => DocResult m [MacroParams]
    liftEither $ macroOnValues macro values

macroOnValues :: Macro -> [MacroParams] -> Either DocError Doc
macroOnValues m v = do
    mapped <- mapM m v
    return $ concat mapped

instance NeedsFiles MacroOnFile where
    getNeededFiles (MacroOnFile m f) = do
        entries <- macroOnEntryFile m f
        containing <- getNeededFiles entries
        return $ f : containing
instance Contentable MacroOnFile where
    resolve (MacroOnFile m f) = do
        doc <- macroOnEntryFile m f
        resolveContents doc T.empty
