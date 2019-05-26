{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Contains basically all information on how to convert layout files,
--   entries, snippets and macros into the sum of these parts.

module Hssb.Layout where

import Hssb.Utilities
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

-- | The @Contentable@ class is used to represent operations that evaluate to
--   text.
class NeedsFiles c => Contentable c where
    resolve        :: MonadReadFile m => c -> DocResult m T.Text

instance Contentable T.Text where
    resolve s = return s

-- | The @Actionable@ class is used to represent operations that combine text
--   in some way.
class NeedsFiles a => Actionable a where
    resolveContents :: MonadReadFile m => a -> T.Text -> DocResult m T.Text

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

-- | Shorthand for a function that transforms text.
type TextFunc = T.Text -> T.Text

-- | Shorthand for a function that transforms text and can fail.
type TextFuncError = T.Text -> Either DocError T.Text

-- | A Params value that may have a file path associated with it.
--   If loaded from a file, the path should be assigned when doing so.
--   If defined in a source file, the path should be @Nothing@.
data PathedParams = PathedParams {
    params :: Params,
    ppath   :: Maybe FilePath
  } deriving (Show)

-- | Things that can go wrong while a macro is being run.
data MacroError =
    AbsentKey String |
    WrongKeyType String |
    MiscMacroError String
    deriving (Show)

-- | Things that can go wrong while a page is being built.
data DocError =
    MacroError MacroError (Maybe FilePath) |
    InvalidPath FilePath |
    NotYaml FilePath |
    InvalidFileFormat FilePath |
    MissingMacro String |
    MiscError String
    deriving (Show)

-- | Site actions as immediately loaded from a YAML file.
data Layout =
    LayoutBuild FilePath String Params |
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

-- | Represents a file to be loaded.  
-- This content is resolved by inserting the contents of the file at the
-- specified path.
data Snippet = Snippet FilePath

instance NeedsFiles Snippet where
    getNeededFiles (Snippet s) = return [s]

instance Contentable Snippet where
    resolve (Snippet p) = getFileContents p

-- | Represents using a macro with a files contents.  
-- This content is resolved by loading the YAML file at the specified path, and
-- running the macro multiple times, with each entry as its parameters.
data MacroOnFile = MacroOnFile Macro FilePath

instance NeedsFiles MacroOnFile where
    getNeededFiles (MacroOnFile m f) = do
        entries <- macroOnEntryFile m f
        containing <- getNeededFiles entries
        return $ f : containing

instance Contentable MacroOnFile where
    resolve (MacroOnFile m f) = do
        doc <- macroOnEntryFile m f
        resolveContents doc T.empty

-- | Represents a transformation of other content.
-- This content is resolved by resolving its child and applying its function to
-- them.
data Transform = forall c. (Contentable c) => Transform c TextFunc

instance NeedsFiles Transform where
    getNeededFiles (Transform c f) = getNeededFiles c

instance Contentable Transform where
    resolve (Transform c f) = do
        sub <- resolve c
        return $ f sub

-- | Represents a transformation of other content that can fail.
-- This content is resolved by resolving its child and applying its function to
-- them.
data TransformError = forall c. (Contentable c) => TransformError c TextFuncError

instance NeedsFiles TransformError where
    getNeededFiles (TransformError c f) = getNeededFiles c

instance Contentable TransformError where
    resolve (TransformError c f) = do
        sub <- resolve c
        liftEither $ f sub

-- | An action that appends content.
-- Once its content is resolved, it will append its text to the other text
-- passed in.
data Add = forall c. (Contentable c) => Add c

instance NeedsFiles Add where
    getNeededFiles (Add a) = getNeededFiles a

instance Actionable Add where
    resolveContents (Add a) t = do
        aa <- resolve a
        return $ T.concat [aa, t]

-- | An action that replaces text.
-- Once its content is resolved, it will replace the supplied text with the
-- resolved content.
data Replace = forall a. (Actionable a) => Replace T.Text a

instance NeedsFiles Replace where
    getNeededFiles (Replace t d) = getNeededFiles d

instance Actionable Replace where
    resolveContents (Replace t d) text = do
        dd <- resolveContents d T.empty
        return $ T.replace t dd text

-- | An existential type that holds any action.
data Action = forall a. (Actionable a) => Action a

instance NeedsFiles Action where
    getNeededFiles (Action a) = getNeededFiles a

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
    HashMap String Macro -> PathedLayout -> Either DocError SiteAction
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
    FilePath -> HashMap String Macro -> DocResult m [SiteAction]
loadSiteActions path map = do
    layout <- loadLayoutFile path
    liftEither $ mapM (layoutToAction map) layout

-- | Determine the files required to evaluate a SiteAction.
filesNeeded :: MonadReadFile m => SiteAction -> DocResult m [FilePath]
filesNeeded (Build m mp f) =
    (liftEither $ executeMacro m mp) >>= getNeededFiles
filesNeeded (Copy from to) = return [to]

-- | (TEMPORARY)
--
--   Evaluate a site action and convert its result to text.
--   Currently only defined for Build.
--
--   Will eventually actually execute things (essentially will be of type IO
--   ()) instead of just evaluating to text.
executeSiteAction :: MonadReadFile m => SiteAction -> DocResult m T.Text
executeSiteAction (Build m mp f) = do 
    executed <- liftEither $ executeMacro m mp
    resolveContents executed T.empty
executeSiteAction _              = undefined

-- | Execute all SiteActions in a layout file.
executeLayoutFile :: MonadReadFile m =>
    FilePath -> HashMap String Macro -> DocResult m T.Text
executeLayoutFile path macros = do
    actions <- loadSiteActions path macros
    texts <- mapM executeSiteAction actions
    return $ T.concat texts

-- | Create a Doc by running a Macro, using the values of a YAML collection as
--   its parameters.
macroOnEntryFile :: MonadReadFile m => Macro -> FilePath -> DocResult m Doc
macroOnEntryFile macro path = do
    values <- yamlIfExists path :: MonadReadFile m => DocResult m [Params]
    let addPath x = PathedParams x $ Just path
    let mp = map addPath values
    liftEither $ macroOnValues macro mp

-- | Create a Doc by running a Macro consecutively, using each entry in the
--   PathedParams list as params.
macroOnValues :: Macro -> [PathedParams] -> Either DocError Doc
macroOnValues m v = do
    mapped <- mapM (executeMacro m) v
    return $ concat mapped
