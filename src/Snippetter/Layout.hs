module Snippetter.Layout where

import Snippetter.Utilities
import Snippetter.Build
import Snippetter.IO
import Data.Yaml ((.:))
import Control.Monad.Except
import Control.Monad.Trans.Except
import qualified Data.Text as T
import qualified Data.Yaml as Y
import qualified Data.HashMap.Strict as H

type LayoutResult a = Either LayoutError a
type LayoutFileResult m a = ExceptT LayoutError m a
type MacroMap = H.HashMap T.Text Macro

errorMappingDoc = LayoutDocError

-- | Site actions as immediately loaded from a YAML file.
data Layout =
    LayoutBuild FilePath T.Text Params |
    LayoutCopy FilePath FilePath
    deriving (Show, Eq)

data LayoutError =
    LayoutDocError DocError
  | LayoutYamlError YamlError
  | LayoutFileError FileError
  | MiscLayoutError T.Text
    deriving (Show, Eq)

instance Y.FromJSON Layout where
    parseJSON = Y.withObject "LayoutFile" $ \o -> do
        kind <- o .: T.pack "type"
        case (T.unpack kind) of
          "macro" -> LayoutBuild <$> o .: T.pack "output"
                         <*> o .: T.pack "macro-name"
                         <*> o .: T.pack "values"
          "copy" -> LayoutCopy <$> o .: T.pack "from"
                         <*> o .: T.pack "to"

-- | A Layout value that may have a file path associated with it.
--   If loaded from a file, the path should be assigned when doing so.
--   If defined in a source file, the path should be @Nothing@.
data PathedLayout = PathedLayout {
    layout :: Layout,
    lpath   :: Maybe FilePath
  } deriving (Show, Eq)

-- | Describes an action taken to build the site.
data SiteAction =
    Build Macro PathedParams FilePath |
    Copy FilePath FilePath

addPath :: FilePath -> Layout -> PathedLayout
addPath path layout = PathedLayout layout (Just path)

yamlAsLayout :: MonadReadFile m => FilePath -> LayoutFileResult m [Layout]
yamlAsLayout path = let errorMapping x = LayoutYamlError x in
    (yamlIfExists path :: MonadReadFile m => YamlResult m [Layout])
        `mapResultError` errorMapping

-- | Loads a list of pathed layouts from a file.
loadLayoutFile :: MonadReadFile m => FilePath -> LayoutFileResult m [PathedLayout]
loadLayoutFile path = do
    y <- yamlAsLayout path
    return $ map (addPath path) y

-- | Converts a PathedLayout to a SiteAction, when given a mapping of strings
--   to macros.
layoutToAction ::
    H.HashMap T.Text Macro -> PathedLayout -> Either LayoutError SiteAction
layoutToAction map (PathedLayout layout input) = ll input layout
    where
        ll path (LayoutBuild output macroName contents) = do
            let mp = PathedParams contents input
            let err = MissingMacro macroName
            macro <- mapLeft errorMappingDoc $ lookupEither err macroName map
            return $ Build macro mp output
        ll path (LayoutCopy from to) = return $ Copy from to

-- | Loads a list of SiteActions from a file, when given a mapping of strings
--   to macros
loadSiteActions :: MonadReadFile m =>
    FilePath -> H.HashMap T.Text Macro -> LayoutFileResult m [SiteAction]
loadSiteActions path map = do
    layout <- loadLayoutFile path
    liftEither $ mapM (layoutToAction map) layout

-- | Determine the files required to evaluate a SiteAction.
filesNeeded :: MonadReadFile m => SiteAction -> LayoutFileResult m [FilePath]
filesNeeded (Build m mp f) =
    ((liftEither $ executeMacro m mp) >>= docNeededFiles)
        `mapResultError` errorMappingDoc
filesNeeded (Copy from to) = return [to]

fileContentsInLayout :: MonadReadFile m => FilePath -> LayoutFileResult m T.Text
fileContentsInLayout path = getFileContents path `mapResultError` mapping
    where mapping err = LayoutFileError err
