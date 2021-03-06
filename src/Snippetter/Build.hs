{-# LANGUAGE OverloadedStrings #-}

-- | Contains functions/types related to builders and building files, including
-- content, actions, the functions that manipulate those, and more.
module Snippetter.Build
  ( -- * Results, Errors, and Important Things
    DocError (..)
  , BuilderError(..)
  , DocResult
  , PageResult
  , PageBuilder
  , NamedPageBuilder(..)
  , executeBuilder
  , filesForBuilder
  , showBuilder
  , previewBuilder
  -- * Content
  , Content
  , conNeededFiles
  , conShow
  , conPreview
  , conEvaluate
  , SubBuilderExec (..)
  , SBEntry
  , SBList
  , SBListFunc
  -- ** Content types
  , text
  , snippet
  , transform
  , doc
  , subBuilder
  , emptyContent
  -- * Actions
  , Action
  , actNeededFiles
  , actShow
  , actPreview
  , actExecute
  -- * Action types
  , noContentAction
  , singleContentAction
  , multiContentAction
  , noAction
  ) where

import Control.Monad
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Data.Functor
import Data.Yaml
import qualified Data.Yaml as Y
import Snippetter.IO
import Snippetter.Utilities
import System.Directory (doesFileExist)

-- | Possible errors when a page is being built.
data DocError
  = DocBuilderError BuilderError NamedPageBuilder (Maybe FilePath)
  | DocFileError FileError
  | DocYamlError YamlError
  | TransformFailed T.Text
  | MiscDocError T.Text
  deriving (Eq)

instance Show DocError where
  show (DocBuilderError e nb fp) =
    "While executing " <>
    show nb <> " with parameters from " <> f <> ":\n" <> indentFourStr (show e)
    where
      f =
        case fp of
          Nothing -> "a Haskell source file"
          Just a -> "the YAML file \"" <> a <> "\""
  show (DocYamlError e) = show e
  show (DocFileError e) = show e
  show (TransformFailed t) =
    "Transforming some content failed with the following: " <> T.unpack t
  show (MiscDocError t) = "An error occured while building:" <> T.unpack t

-- | The result of a function that builds a page.
type DocResult m a = Result DocError m a

-- | The result of a 'PageBuilder' function.
type PageResult = Either BuilderError Content

-- | A 'PageBuilder' function takes in a JSON object and outputs either an
-- error or the contents of a document.
type PageBuilder = Params -> PageResult

-- | A 'PageBuilder' that has a name associated with it.
data NamedPageBuilder = NamedPageBuilder T.Text PageBuilder

instance Eq NamedPageBuilder where
  (NamedPageBuilder t1 _) == (NamedPageBuilder t2 _) = t1 == t2

instance Show NamedPageBuilder where
  show (NamedPageBuilder t _) = "a page builder named " <> T.unpack t

-- | Extract the 'PageBuilder' from a 'NamedPageBuilder'.
extractPageBuilder :: NamedPageBuilder -> PageBuilder
extractPageBuilder (NamedPageBuilder _ b) = b

-- | Things that can go wrong while a builder is being run.
data BuilderError
  = AbsentKey T.Text
  | WrongKeyType T.Text
  | BuilderYamlError YamlError
  | MiscBuilderError T.Text
  deriving (Eq)

instance Show BuilderError where
  show (AbsentKey t) = "The key \"" <> T.unpack t <> "\" was missing."
  show (WrongKeyType t) = "The key \"" <> T.unpack t <> "\" was the wrong type."
  show (MiscBuilderError t) = "An error occured while building: " <> T.unpack t

-- | Retrieves contents of the specific file, and maps possible errors to
--   @DocFileError@s.
fileContentsInDoc :: MonadReadWorld m => FilePath -> DocResult m T.Text
fileContentsInDoc path = getFileContents path `mapResultError` mapping
  where
    mapping = DocFileError

-- | Create a @Doc@ by running a 'PageBuilder' consecutively, using each entry in the
--   list as parameters.
builderWithParams ::
     Monad m => [(NamedPageBuilder, PathedParams)] -> DocResult m Content
builderWithParams list = do
  let func (b, v) = buildDoc b v
  mapped <- mapM func list
  let addFunc a b = a <> b
  let add c = singleContentAction c addFunc "Add: "
  let contentList contents = Doc EmptyContent (map add mapped)
  return $ contentList mapped

-- | Build a @Doc@ by running a 'PageBuilder' with the given parameters.
buildDoc :: Monad m => NamedPageBuilder -> PathedParams -> DocResult m Content
buildDoc nb (PathedParams params path) =
  mapResultError (resultLiftEither $ b params) $ convertBuilderError path
  where
    b = extractPageBuilder nb
    convertBuilderError path err = DocBuilderError err nb path

-- | Evaluates the given 'PageBuilder' to text with the given parameters.
executeBuilder ::
     MonadReadWorld m => NamedPageBuilder -> PathedParams -> DocResult m T.Text
executeBuilder b pp = buildDoc b pp >>= conEvaluate

-- | Determines files needed to run the 'PageBuilder' with the given parameters.
filesForBuilder ::
     MonadReadWorld m => NamedPageBuilder -> PathedParams -> DocResult m FilePathSet
filesForBuilder b pp = buildDoc b pp >>= conNeededFiles

-- | Show the actions the 'NamedPageBuilder' will take to build the page.
showBuilder :: 
     MonadReadWorld m => NamedPageBuilder -> PathedParams -> DocResult m T.Text
showBuilder b pp = do
    doc <- buildDoc b pp
    return $ conShow doc

-- | Preview the actions the 'NamedPageBuilder' will take to build the page.
previewBuilder :: 
     MonadReadWorld m => NamedPageBuilder -> PathedParams -> DocResult m T.Text
previewBuilder b pp = buildDoc b pp >>= conPreview

-- | Represents operations that evaluate to text.
data Content
  = Text T.Text
  | Snippet FilePath
  | Transform Content (T.Text -> Either T.Text T.Text)
  | Doc Content [Action]
  | SubBuilder [SubBuilderExec] SBListFunc
  | EmptyContent

instance Show Content where
  show c = T.unpack $ conShow c

-- | Determine files needed by a Content.
conNeededFiles :: MonadReadWorld m => Content -> DocResult m FilePathSet
conNeededFiles (Text _) = return HS.empty
conNeededFiles (Snippet s) = return $ HS.singleton s
conNeededFiles (Transform c _) = conNeededFiles c
conNeededFiles (SubBuilder sbe f) = sbNeededFiles f sbe
conNeededFiles (Doc c d) = do
  dNeeded <- actListNeededFiles d
  cNeeded <- conNeededFiles c
  return $ HS.union dNeeded cNeeded
conNeededFiles EmptyContent = return HS.empty

-- | Show a piece of @Content@. This is like @show@ except it uses @Text@.
conShow :: Content -> T.Text
conShow (Text t) = "\"" <> t <> "\""
conShow (Snippet s) = "Snippet named \"" <> T.pack s <> "\""
conShow (Transform c f) = "Transformation of: " <\> indentFour pre
  where
    pre = conShow c
conShow (SubBuilder sbe _) =
  "Builder executions:" <\\> (indentMultiWithListMarker $ map sbeShow sbe)
conShow (Doc c d) =
  case c of
    EmptyContent -> "The following actions:\n" <> actions
    _ -> "Doc containing " <\> indentFour (conShow c) <>
           "\n  With the following actions applied to it:\n" <> actions
  where
    actions = T.intercalate "\n" (map (indentWithListMarker . actShow) d)
conShow EmptyContent = "Empty content"

-- | Preview a piece of @Content@, similar to @conShow@, except files may be read
--   to determine extra information.
conPreview :: MonadReadWorld m => Content -> DocResult m T.Text
conPreview (Text t) = return $ "\"" <> t <> "\""
conPreview (Snippet s) = do
  contents <- fileContentsInDoc s
  let nameSegment = "Snippet named \"" <> T.pack s <> "\" with the contents:"
  return $ nameSegment <> "\n" <> indentFour contents
conPreview (Transform c f) = do
  dryRun <- conPreview c
  return $ "Transformation of: " <\> indentFour dryRun
conPreview (SubBuilder sbe f) = do
  result <- sbPreview f sbe
  return $ if T.null result
    then "Empty sub-builder execution"
    else result

conPreview (Doc c d) = do
  cPreviewed <- conPreview c
  dPreviewed <- mapM actPreview d
  let actions = T.intercalate "\n" (map indentWithListMarker dPreviewed)
  return $ case c of
    EmptyContent -> "The following actions:\n" <> actions
    _ -> "Doc containing " <\> indentFour cPreviewed <>
           "\n  With the following actions applied to it:\n" <> actions
conPreview EmptyContent = return "Empty content"

-- | Convert a @Content@ to text.
conEvaluate :: MonadReadWorld m => Content -> DocResult m T.Text
conEvaluate (Text t) = return t
conEvaluate (Snippet s) = fileContentsInDoc s
conEvaluate (Transform c f) = do
  sub <- conEvaluate c
  resultLiftEither $ mapLeft TransformFailed $ f sub
conEvaluate (SubBuilder sbe f) = sbEvaluate f sbe
conEvaluate (Doc c d) = do
  initial <- conEvaluate c
  actListExecute d initial
conEvaluate EmptyContent = return ""

-- | Determine files needed by a @[Action]@
actListNeededFiles :: MonadReadWorld m => [Action] -> DocResult m FilePathSet
actListNeededFiles doc = do
  mapped <- mapM actNeededFiles doc
  return $ HS.unions mapped

-- | Execute all @Actions@ within a @[Action]@.
actListExecute :: MonadReadWorld m => [Action] -> T.Text -> DocResult m T.Text
actListExecute xs text = foldM (flip actExecute) text xs

type SBEntry = (NamedPageBuilder, PathedParams)

type SBList = [SBEntry]

-- | 'SubBuilderExec's will execute this function to sort/filter/map/reduce
-- existing parameters.
type SBListFunc = SBList -> SBList

-- | Specifies the 'PageBuilder' to use in a @SubBuilder@ and what it should execute
--   on.
data SubBuilderExec =
  SubBuilderExec
    { sbBuilder :: NamedPageBuilder -- ^ The 'NamedPageBuilder' to use.
    , sbDefault :: Params -- ^ Default 'Params'. If a 'Params' doesn't have a key from the defaults, the value from those defaults will be used.
    , sbParams :: [PathedParams] -- ^ The 'Params' to operate on.
    , sbFiles :: [FilePath] -- ^ Parameter files to load and also operate on.
    , sbFunc :: SBListFunc -- ^ Parameter list transformation function
    }

instance Show SubBuilderExec where
  show s = T.unpack $ sbeShow s

-- | Load all parameters in a @SubBuilderExec@, with default parameters applied.
sbAllParams :: MonadReadWorld m => SubBuilderExec -> DocResult m [PathedParams]
sbAllParams (SubBuilderExec _ def pp files _) =
  mergeParams pp files def `mapResultError` DocYamlError

sbExecFilteredList ::
     MonadReadWorld m => SubBuilderExec -> DocResult m SBList
sbExecFilteredList exec = do
  params <- sbAllParams exec
  let zipped = zip (repeat $ sbBuilder exec) params
  return $ (sbFunc exec) zipped

sbFilteredList ::
     MonadReadWorld m => SBListFunc -> [SubBuilderExec] -> DocResult m SBList
sbFilteredList f execs = do
  list <- concat <$> mapM sbExecFilteredList execs
  return $ f list

sbFilteredParams ::
     MonadReadWorld m => SBListFunc -> [SubBuilderExec] -> DocResult m [PathedParams]
sbFilteredParams f execs = snd <$> unzip <$> sbFilteredList f execs

-- | Determine files needed by a @SubBuilderExec@.
sbNeededFiles :: MonadReadWorld m => SBListFunc -> [SubBuilderExec] -> DocResult m FilePathSet
sbNeededFiles f execs = do
  list <- sbFilteredList f execs
  entries <- builderWithParams list
  containing <- conNeededFiles entries
  let paramFiles = HS.fromList $ concat $ map sbFiles execs
  return $ paramFiles <> containing

-- | Show a @SubBuilderExec@ (see @conShow@).
sbeShow :: SubBuilderExec -> T.Text
sbeShow (SubBuilderExec m def pp files _) = tDefaults <\\> tParams <\\> tFile
  where
    tDefaults
      | nullParams def = ""
      | otherwise = "Default values:\n" <> showParams def
    tParams
      | null pp = ""
      | otherwise =
        ("Execution with these params:\n" :: T.Text) <>
        T.intercalate "\n" (map showPathedParams pp)
    tFile
      | null files = ""
      | otherwise =
        ("Execution on these files:\n" :: T.Text) <>
        T.intercalate
          "\n"
          (map (indentWithListMarker . T.pack) files)

-- | Preview a @SubBuilderExec@ with file reading (see @conPreview@).
sbPreview :: MonadReadWorld m => SBListFunc -> [SubBuilderExec] -> DocResult m T.Text
sbPreview f execs = do
  list <- sbFilteredList f execs
  case list of 
    [] -> return ""
    _ -> do
      con <- builderWithParams list
      conPreview con

-- | Evaluate a @SubBuilderExec@. This is done by running the builder on all
--   specified parameters, and on all parameters in the specified files.
sbEvaluate :: MonadReadWorld m => SBListFunc -> [SubBuilderExec] -> DocResult m T.Text
sbEvaluate f execs = do
  list <- sbFilteredList f execs
  con <- builderWithParams list
  conEvaluate con

-- | Represents operations that transform text in some way.
data Action
  = NoContentAction (T.Text -> T.Text) T.Text
  | SingleContentAction Content (T.Text -> T.Text -> T.Text) T.Text
  | MultiContentAction [Content] (T.Text -> [T.Text] -> T.Text) T.Text
  | NoAction

instance Show Action where
  show a = T.unpack $ actShow a

-- | Determine files needed by an @Action@ to execute.
actNeededFiles :: MonadReadWorld m => Action -> DocResult m FilePathSet
actNeededFiles (NoContentAction _ _) = return HS.empty
actNeededFiles (SingleContentAction c _ _) = conNeededFiles c
actNeededFiles (MultiContentAction c _ _) = do
  needed <- mapM conNeededFiles c
  return $ HS.unions needed
actNeededFiles NoAction = return HS.empty

-- | Execute the given @Action@ on the specified text.
actExecute :: MonadReadWorld m => Action -> T.Text -> DocResult m T.Text
actExecute (NoContentAction f _) text = return $ f text
actExecute (SingleContentAction c f _) text = do
  evaluated <- conEvaluate c
  return $ f text evaluated
actExecute (MultiContentAction c f _) text = do
  evaluated <- mapM conEvaluate c
  return $ f text evaluated
actExecute NoAction text = return text

-- | Show an @Action@. This is like @show@ except it uses @Text@.
actShow :: Action -> T.Text
actShow (NoContentAction _ t) = t
actShow (SingleContentAction c _ t) = t <\> indentFour (conShow c)
actShow (MultiContentAction c _ t) = t <> "\n" <> content
  where
    content = T.intercalate "\n" (map (indentWithListMarker . conShow) c)
actShow NoAction = "No action"

-- | Preview an @Action@, similar to @actShow@, except files may be read   to
--   determine extra information.
actPreview :: MonadReadWorld m => Action -> DocResult m T.Text
actPreview (NoContentAction _ t) = return t
actPreview (SingleContentAction c _ t) = do
  dryRun <- conPreview c
  return $ t <\> indentFour dryRun
actPreview (MultiContentAction c _ t) = do
  previewed <- mapM conPreview c
  return $
    t <> "\n" <> T.intercalate "\n" (map (indentWithListMarker . conShow) c)
actPreview NoAction = return "No action"

-- | Creates a @Content@ that is literal text.
text = Text

-- | Creates a @Content@ that evaluates to a file's contents.
snippet = Snippet

-- | Creates a @Content@ that applies a function to another @Content@.
transform = Transform

-- | Creates a @Content@ that applies successive @Action@s to a @Content@.
doc = Doc

-- | Creates a @Content@ that executes other 'PageBuilder's with various parameters.
subBuilder = SubBuilder

-- | Creates a @Content@ that evaluates to nothing.
emptyContent = EmptyContent

-- | Creates an @Action@ that modifies text based solely on itself.
noContentAction ::
       (T.Text -> T.Text) -- ^ Function to apply
    -> T.Text -- ^ Preview text
    -> Action
noContentAction = NoContentAction

-- | Creates an @Action@ that modifies text based on the output of a @Content@.
singleContentAction ::
       Content -- ^ 'Content' to evaluate
    -> (T.Text -> T.Text -> T.Text) -- ^ Function to apply (the 'Content' will be the second parameter)
    -> T.Text -- ^ Preview text
    -> Action
singleContentAction = SingleContentAction

-- | Creates an @Action@ that modifies text based on the output of multiple @Content@s.
multiContentAction ::
       [Content] -- ^ 'Content's to evaluate
    -> (T.Text -> [T.Text] -> T.Text) -- ^ Function to apply
    -> T.Text -- ^ Preview text
    -> Action
multiContentAction = MultiContentAction

-- | Creates an @Action@ that does nothing.
noAction = NoAction
