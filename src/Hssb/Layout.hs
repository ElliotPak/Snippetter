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

executeMacro :: Macro -> MacroParams -> Result
executeMacro m (MacroParams params path) =
    mapLeft (convertMacroError path) $ m params

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

loadLayoutFile :: MonadReadFile m => FilePath -> DocResult m [PathedLayoutEntry]
loadLayoutFile path = do
    y <- yamlIfExists path :: MonadReadFile m => DocResult m [LayoutEntry]
    return $ map (addPath path) y

layoutToAction ::
    HashMap String Macro -> PathedLayoutEntry -> Either DocError SiteAction
layoutToAction map (PathedLayoutEntry input layout) = ll path layout
    where
        ll path (ExecMacro output macroName contents) = do
            let mp = MacroParams contents input
            let err = MissingMacro macroName
            macro <- lookupEither err macroName map
            return $ Build macro mp output
        ll path (ExecCopy from to) = return $ Copy from to

loadSiteActions :: MonadReadFile m =>
    FilePath -> HashMap String Macro -> DocResult m [SiteAction]
loadSiteActions path map = do
    layout <- loadLayoutFile path
    liftEither $ mapM (layoutToAction map) layout

filesNeeded :: MonadReadFile m => SiteAction -> DocResult m [FilePath]
filesNeeded (Build m mp f) =
    (liftEither $ executeMacro m mp) >>= getNeededFiles
filesNeeded (Copy from to) = return [to]

executeSiteAction :: MonadReadFile m => SiteAction -> DocResult m T.Text
executeSiteAction (Build m mp f) = do 
    executed <- liftEither $ executeMacro m mp
    resolveContents executed T.empty
executeSiteAction _              = undefined

executeLayoutFile :: MonadReadFile m =>
    FilePath -> HashMap String Macro -> DocResult m T.Text
executeLayoutFile path macros = do
    actions <- loadSiteActions path macros
    texts <- mapM executeSiteAction actions
    return $ T.concat texts

macroOnEntryFile :: MonadReadFile m => Macro -> FilePath -> DocResult m Doc
macroOnEntryFile macro path = do
    values <- yamlIfExists path :: MonadReadFile m => DocResult m [Params]
    let addPath x = MacroParams x $ Just path
    let mp = map addPath values
    liftEither $ macroOnValues macro mp

macroOnValues :: Macro -> [MacroParams] -> Either DocError Doc
macroOnValues m v = do
    mapped <- mapM (executeMacro m) v
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
