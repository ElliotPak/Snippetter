{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Contains information on how to convert layout files, entries, snippets and
--   macros into the sum of these parts. @Snippetter.LayoutTypes@ contains the
--   types that actually use this.

module Snippetter.LayoutBase where

import Snippetter.Utilities
import Control.Monad.Except
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Data.Aeson.Types (Object, Value (Object, String))
import Data.HashMap.Strict (HashMap, lookup)
import Data.Yaml
import System.Directory (doesFileExist)
import qualified Data.Yaml as Y
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B

-- | The @NeedsFiles@ class is used for file dependencies.
--   Any data type that represents some operation that requires files should be an
--   instance of this class.
class NeedsFiles n where
    getNeededFiles :: MonadReadFile m => n -> DocResult m [FilePath]
    getNeededFiles _ = return []

instance NeedsFiles T.Text

-- | The @Previewable@ class is used as an advanced "show". In addition to
--   showing a datatype's values it also supports a "dry run", which reads
--   files in order to determine what macros etc. will actually be evaluated,
--   instead of just showing values.
class Previewable p where
    preview       :: Int -> p -> T.Text
    previewDryRun :: MonadReadFile m => Int -> p -> DocResult m T.Text
    previewDryRun indent pv = return $ preview indent pv

instance Previewable T.Text where
    preview indent text = indentFour indent text

instance Previewable p => Previewable [p] where
    preview indent = T.intercalate "\n" . map (preview (indent + 1))
    previewDryRun indent list = do
        mapped <- mapM (previewDryRun (indent + 1)) list
        return $ T.intercalate "\n" mapped

-- | The @Contentable@ class is used to represent operations that evaluate to
--   text.
class (NeedsFiles c, Previewable c) => Contentable c where
    resolve    :: MonadReadFile m => c -> DocResult m T.Text

instance Contentable T.Text where
    resolve s = return s

-- | The @Actionable@ class is used to represent operations that combine text
--   in some way.
class (NeedsFiles a, Previewable a) => Actionable a where
    resolveContents :: MonadReadFile m => a -> T.Text -> DocResult m T.Text

-- | An existential type that holds any action.
data Action = forall a. (Actionable a) => Action a

instance NeedsFiles Action where
    getNeededFiles (Action a) = getNeededFiles a

instance Previewable Action where
    preview indent (Action a)       = preview indent a
    previewDryRun indent (Action a) = previewDryRun indent a

instance Actionable Action where
    resolveContents (Action a) = resolveContents a

-- | Shorthand for a list of Actions.
type Doc = [Action]

instance NeedsFiles Doc where
    getNeededFiles doc = do
        mapped <- mapM getNeededFiles doc
        return $ concat mapped

instance Actionable Doc where
    resolveContents (x:xs) text = do
        head <- resolveContents x text
        resolveContents xs head
    resolveContents [] text = return text

-- | The @MonadReadFile@ class is used to represent monads that can read files.
--   It can also be used for mocking purposes.
class Monad m => MonadReadFile m where
    getFileContents :: FilePath -> DocResult m T.Text
    fileExists :: FilePath -> m Bool

instance MonadReadFile IO where
    getFileContents s = do
        str <- liftIO (Prelude.readFile s)
        return (T.pack str)
    fileExists = doesFileExist

-- | The return type for an operation relating to building a page.
--   This is intended to be used with a monad such as MonadReadFile.
type DocResult m a = ExceptT DocError m a

-- | The return type for an operation relating to building a page that doesn't
--   require some other monad.
type Result = Either DocError Doc

-- | The return type of a Macro function.
type MacroResult = Either MacroError Doc

-- | Shorthand for a macro's parameters.
-- Identical to the type of an Aeson object.
type Params = HashMap T.Text Value

-- | Shorthand for a macro's type signature.
type Macro = Params -> MacroResult

instance Show Macro where
    show s = "MACRO"

-- | A Params value that may have a file path associated with it.
--   If loaded from a file, the path should be assigned when doing so.
--   If defined in a source file, the path should be @Nothing@.
data PathedParams = PathedParams {
    params :: Params,
    ppath   :: Maybe FilePath
  } deriving (Show)

-- | Things that can go wrong while a macro is being run.
data MacroError =
    AbsentKey T.Text |
    WrongKeyType T.Text |
    MiscMacroError T.Text
    deriving (Show)

-- | Things that can go wrong while a page is being built.
data DocError =
    MacroError MacroError (Maybe FilePath) |
    InvalidPath FilePath |
    NotYaml FilePath |
    InvalidFileFormat FilePath |
    MissingMacro T.Text |
    MiscError T.Text
    deriving (Show)

-- | Site actions as immediately loaded from a YAML file.
data Layout =
    LayoutBuild FilePath T.Text Params |
    LayoutCopy FilePath FilePath
    deriving (Show)

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
  } deriving (Show)

-- | Describes an action taken to build the site.
data SiteAction =
    Build Macro PathedParams FilePath |
    Copy FilePath FilePath
    deriving (Show)

-- | Converts a MacroError to a DocError.
convertMacroError :: Maybe FilePath -> MacroError -> DocError
convertMacroError path err = MacroError err path

-- | Converts a Layout to a PathedLayout.
addPath :: FilePath -> Layout -> PathedLayout
addPath path layout = PathedLayout layout (Just path)

-- | Executes a macro, evaluating it with the given parameters.
executeMacro :: Macro -> PathedParams -> Result
executeMacro m (PathedParams params path) =
    mapLeft (convertMacroError path) $ m params

-- | Returns the contents of a file if it exists.
contentsIfExists :: MonadReadFile m => FilePath -> DocResult m T.Text
contentsIfExists path = do
    exists <- lift $ fileExists path
    case exists of
      False -> throwE $ InvalidPath path
      True  -> getFileContents path

-- | Decodes an Aeson-parsable ADT from the supplied text.
decodeYaml :: Y.FromJSON a => T.Text -> Either Y.ParseException a
decodeYaml str = Y.decodeEither' $ B.pack $ T.unpack str

-- | Loads an Aeson-parsable ADT from the supplied YAML file.
yamlIfExists :: (MonadReadFile m, Y.FromJSON a) =>
    FilePath -> DocResult m a
yamlIfExists path = do
    contents <- contentsIfExists path
    liftEither $ mapLeft (\x -> NotYaml path) $ decodeYaml contents

-- | Loads a list of pathed layouts from a file.
loadLayoutFile :: MonadReadFile m => FilePath -> DocResult m [PathedLayout]
loadLayoutFile path = do
    y <- yamlIfExists path :: MonadReadFile m => DocResult m [Layout]
    return $ map (addPath path) y

-- | Converts a PathedLayout to a SiteAction, when given a mapping of strings
--   to macros.
layoutToAction ::
    HashMap T.Text Macro -> PathedLayout -> Either DocError SiteAction
layoutToAction map (PathedLayout layout input) = ll input layout
    where
        ll path (LayoutBuild output macroName contents) = do
            let mp = PathedParams contents input
            let err = MissingMacro macroName
            macro <- lookupEither err macroName map
            return $ Build macro mp output
        ll path (LayoutCopy from to) = return $ Copy from to

-- | Loads a list of SiteActions from a file, when given a mapping of strings
--   to macros
loadSiteActions :: MonadReadFile m =>
    FilePath -> HashMap T.Text Macro -> DocResult m [SiteAction]
loadSiteActions path map = do
    layout <- loadLayoutFile path
    liftEither $ mapM (layoutToAction map) layout

-- | Determine the files required to evaluate a SiteAction.
filesNeeded :: MonadReadFile m => SiteAction -> DocResult m [FilePath]
filesNeeded (Build m mp f) =
    (liftEither $ executeMacro m mp) >>= getNeededFiles
filesNeeded (Copy from to) = return [to]

--   Evaluate a site action and convert its result to text.
--   Only defined for Build.
evaluateBuildAction :: MonadReadFile m => SiteAction -> DocResult m T.Text
evaluateBuildAction (Build m mp f) = do 
    executed <- liftEither $ executeMacro m mp
    resolveContents executed T.empty
evaluateBuildAction _              = undefined

-- | Create a Doc by running a Macro, using the values of a YAML collection as
--   its parameters.
macroOnEntryFile :: MonadReadFile m => Macro -> FilePath -> DocResult m Doc
macroOnEntryFile macro path = do
    values <- yamlIfExists path :: MonadReadFile m => DocResult m [Params]
    let addPath x = PathedParams x $ Just path
    let mp = map addPath values
    liftEither $ macroOnValues macro mp

-- | Create a Doc by running a Macro consecutively, using each entry in the
--   PathedParams list as parameters.
macroOnValues :: Macro -> [PathedParams] -> Either DocError Doc
macroOnValues m v = do
    mapped <- mapM (executeMacro m) v
    return $ concat mapped
