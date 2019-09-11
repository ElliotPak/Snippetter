{-# LANGUAGE OverloadedStrings #-}

-- | Contains functions/types related to builders and building files, including
-- content, actions, the functions that manipulate those, and more.
module Snippetter.Build where

import Control.Monad
import Data.Aeson.Types (Object, Value(Object, String))
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Data.Yaml
import qualified Data.Yaml as Y
import Snippetter.IO
import Snippetter.Utilities
import System.Directory (doesFileExist)

-- | Possible errors when a page is being built.
data DocError
  = DocBuilderError BuilderError NamedBuilder (Maybe FilePath)
  | DocFileError FileError
  | DocYamlError YamlError
  | MissingBuilder T.Text
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
  show (MissingBuilder t) =
    "The builder \"" <> T.unpack t <> "\" was missing from the builder map."
  show (MiscDocError t) = "An error occured while building:" <> T.unpack t

-- | The result a function that builds a page and can read files.
type DocResult m a = Result DocError m a

-- | The result of a @Builder@ function.
type BuilderResult = Either BuilderError Content

-- | Shorthand for a @Builder@'s type signature.
type Builder = Params -> BuilderResult

-- | A @Builder@ with an optional name.
data NamedBuilder
  = NamedBuilder T.Text Builder
  | UnnamedBuilder Builder

instance Eq NamedBuilder where
  (NamedBuilder t1 _) == (NamedBuilder t2 _) = t1 == t2
  _ == _ = False

instance Show NamedBuilder where
  show (NamedBuilder t _) = "a builder named " <> T.unpack t
  show (UnnamedBuilder _) = "an unnamed builder"

-- | Extract the @Builder@ from a @NamedBuilder@.
extractBuilder :: NamedBuilder -> Builder
extractBuilder (NamedBuilder _ b) = b
extractBuilder (UnnamedBuilder b) = b

-- | A @Params@ value that may have a file path associated with it.
--   If loaded from a file, the path should be assigned when doing so.
--   If defined in a source file, the path should be @Nothing@.
data PathedParams =
  PathedParams
    { params :: Params
    , ppath :: Maybe FilePath
    }
  deriving (Show, Eq)

-- | Things that can go wrong while a builder is being run.
data BuilderError
  = AbsentKey T.Text
  | WrongKeyType T.Text
  | MiscBuilderError T.Text
  deriving (Eq)

instance Show BuilderError where
  show (AbsentKey t) = "The key \"" <> T.unpack t <> "\" was missing."
  show (WrongKeyType t) = "The key \"" <> T.unpack t <> "\" was the wrong type."
  show (MiscBuilderError t) = "An error occured while building: " <> T.unpack t

-- | Shorthand for @HashSet FilePath@.
type FilePathSet = HS.HashSet FilePath

-- | Retrieves contents of the specific file, and maps possible errors to
--   @DocFileError@s.
fileContentsInDoc :: MonadReadWorld m => FilePath -> DocResult m T.Text
fileContentsInDoc path = getFileContents path `mapResultError` mapping
  where
    mapping = DocFileError

-- | Load a YAML file as a list of @Params@.
yamlAsParams :: MonadReadWorld m => FilePath -> DocResult m [Params]
yamlAsParams path =
  let errorMapping = DocYamlError
   in (yamlIfExists "List of parameters" path :: MonadReadWorld m =>
                                                   YamlResult m [Params]) `mapResultError`
      errorMapping

-- | Load all paramaters from a (possible) paramater file as @PathedParams@.
paramsFromFile :: MonadReadWorld m => FilePath -> DocResult m [PathedParams]
paramsFromFile file = do
  values <- yamlAsParams file
  let addPath x = PathedParams x $ Just file
  return $ map addPath values

-- | Create a @Doc@ by running a @Builder@, using the values of a YAML collection
--   as its parameters.
builderWithParamsFile ::
     MonadReadWorld m => NamedBuilder -> FilePath -> DocResult m Content
builderWithParamsFile builder path = do
  fromFile <- paramsFromFile path
  builderWithParams builder fromFile

-- | Create a @Doc@ by running a @Builder@ consecutively, using each entry in the
--   PathedParams list as parameters.
builderWithParams ::
     Monad m => NamedBuilder -> [PathedParams] -> DocResult m Content
builderWithParams b v = do
  mapped <- mapM (buildDoc b) v
  return $ ContentList mapped

-- | Build a @Doc@ by running a @Builder@ with the given parameters.
buildDoc :: Monad m => NamedBuilder -> PathedParams -> DocResult m Content
buildDoc nb (PathedParams params path) =
  mapResultError (resultLiftEither $ b params) $ convertBuilderError path
  where
    b = extractBuilder nb
    convertBuilderError path err = DocBuilderError err nb path

-- | Evaluates the given @Builder@ to text with the given parameters.
executeBuilder ::
     MonadReadWorld m => NamedBuilder -> PathedParams -> DocResult m T.Text
executeBuilder b pp = buildDoc b pp >>= conEvaluate

-- | Determines files needed to run the @Builder@ with the given parameters.
filesForBuilder ::
     MonadReadWorld m => NamedBuilder -> PathedParams -> DocResult m FilePathSet
filesForBuilder b pp = buildDoc b pp >>= conNeededFiles

-- | Represents operations that evaluate to text.
data Content
  = Text T.Text
  | Snippet FilePath
  | Transform Content (T.Text -> T.Text)
  | TransformError Content (T.Text -> Either T.Text T.Text)
  | SubBuilder SubBuilderExec
  | Doc Content [Action]
  | ContentList [Content]
  | EmptyContent

instance Show Content where
  show c = T.unpack $ conShow c

-- | Determine files needed by a Content.
conNeededFiles :: MonadReadWorld m => Content -> DocResult m FilePathSet
conNeededFiles (Text _) = return HS.empty
conNeededFiles (Snippet s) = return $ HS.singleton s
conNeededFiles (Transform c _) = conNeededFiles c
conNeededFiles (TransformError c _) = conNeededFiles c
conNeededFiles (SubBuilder sme) = smNeededFiles sme
conNeededFiles (Doc c d) = do
  dNeeded <- actListNeededFiles d
  cNeeded <- conNeededFiles c
  return $ HS.union dNeeded cNeeded
conNeededFiles (ContentList c) = do
  cNeeded <- mapM conNeededFiles c
  return $ HS.unions cNeeded
conNeededFiles EmptyContent = return HS.empty

-- | Show a piece of @Content@. This is like @show@ except it uses @Text@.
conShow :: Content -> T.Text
conShow (Text t) = "\"" <> t <> "\""
conShow (Snippet s) = "Snippet named \"" <> T.pack s <> "\""
conShow (Transform c f) = "Transformation of: " <\> indentFour pre
  where
    pre = conShow c
conShow (TransformError c f) = "Transformation of: " <\> indentFour pre
  where
    pre = conShow c
conShow (SubBuilder sme) = "Builder executions:" <\\> indentFour (smShow sme)
conShow (Doc c d) =
  "Doc containing " <\> indentFour (conShow c) <>
  "\n  With the following actions applied to it:\n" <> actions
  where
    actions = T.intercalate "\n" (map (indentWithListMarker . actShow) d)
conShow (ContentList c) = "The following content:\n" <> contents
  where
    contents = T.intercalate "\n" (map (indentWithListMarker . conShow) c)
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
conPreview (TransformError c f) = do
  dryRun <- conPreview c
  return $ "Transformation of: " <\> indentFour dryRun
conPreview (SubBuilder sme) = do
  previewed <- smPreview sme
  return $ "Builder executions:" <\\> indentFour previewed
conPreview (Doc c d) = do
  cPreviewed <- conPreview c
  dPreviewed <- mapM actPreview d
  return $
    "Doc containing: \n" <>
    indentFour cPreviewed <>
    "\n  With the following actions applied to it:\n" <>
    T.intercalate "\n" (map indentWithListMarker dPreviewed)
conPreview (ContentList c) = do
  cPreviewed <- mapM conPreview c
  return $
    "The following content:\n" <>
    T.intercalate "\n" (map indentWithListMarker cPreviewed)
conPreview EmptyContent = return "Empty content"

-- | Convert a @Content@ to text.
conEvaluate :: MonadReadWorld m => Content -> DocResult m T.Text
conEvaluate (Text t) = return t
conEvaluate (Snippet s) = fileContentsInDoc s
conEvaluate (Transform c f) = do
  sub <- conEvaluate c
  return $ f sub
conEvaluate (TransformError c f) = do
  sub <- conEvaluate c
  resultLiftEither $ mapLeft TransformFailed $ f sub
conEvaluate (SubBuilder sme) = smEvaluate sme
conEvaluate (Doc c d) = do
  initial <- conEvaluate c
  actListExecute d initial
conEvaluate (ContentList c) = do
  contents <- mapM conEvaluate c
  return $ T.concat contents
conEvaluate EmptyContent = return ""

-- | Determine files needed by a @[Action]@.
actListNeededFiles :: MonadReadWorld m => [Action] -> DocResult m FilePathSet
actListNeededFiles doc = do
  mapped <- mapM actNeededFiles doc
  return $ HS.unions mapped

-- | Execute all @Actions@ within a @[Action]@.
actListExecute :: MonadReadWorld m => [Action] -> T.Text -> DocResult m T.Text
actListExecute xs text = foldM (flip actExecute) text xs

-- | Specifies the @Builder@ to use in a @SubBuilder@ and what it should execute
--   on.
data SubBuilderExec =
  SubBuilderExec
    { smBuilder :: NamedBuilder
    , smDefault :: Params
    , smParams :: [PathedParams]
    , smFiles :: FilePathSet
    } -- deriving (Show, Eq)

instance Show SubBuilderExec where
  show s = T.unpack $ smShow s

-- | Load parameters from all parameter files in a @SubBuilderExec@, without
--   default parameters applied.
smFileParams :: MonadReadWorld m => SubBuilderExec -> DocResult m [PathedParams]
smFileParams sm = do
  params <- mapM paramsFromFile (HS.toList $ smFiles sm)
  return $ concat params

-- | Load all parameters in a @SubBuilderExec@, with default parameters applied.
smAllParams :: MonadReadWorld m => SubBuilderExec -> DocResult m [PathedParams]
smAllParams sm = do
  fileParams <- smFileParams sm
  let allParams = smParams sm ++ fileParams
  let defaulted = map (pathedParamDefault $ smDefault sm) allParams
  return defaulted

-- | Add fields present in the first @Params@ to the @PathedParams@ if they're
--   missing from the second.
pathedParamDefault :: Params -> PathedParams -> PathedParams
pathedParamDefault def (PathedParams params ppath) =
  PathedParams (paramUnion params def) ppath

-- | Determine files needed by a @SubBuilderExec@.
smNeededFiles :: MonadReadWorld m => SubBuilderExec -> DocResult m FilePathSet
smNeededFiles sm = do
  params <- smAllParams sm
  entries <- builderWithParams (smBuilder sm) params
  containing <- conNeededFiles entries
  return $ smFiles sm <> containing

-- | Get a textual representation of a @Params@.
showParams :: Params -> T.Text
showParams params
  | nullParams params = ""
  | otherwise = (indentWithListMarker . T.pack . B.unpack . Y.encode) params

-- | Get a textual representation of the @Params@ within a @PathedParams@.
showPathedParams :: PathedParams -> T.Text
showPathedParams (PathedParams params _) = showParams params

-- | Show a @SubBuilderExec@ (see @conShow@).
smShow :: SubBuilderExec -> T.Text
smShow (SubBuilderExec m def pp f) = tDefaults <\\> tParams <\\> tFile
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
      | null f = ""
      | otherwise =
        ("Execution on these files:\n" :: T.Text) <>
        T.intercalate
          "\n"
          (HS.toList (HS.map (indentWithListMarker . T.pack) f))

-- | Preview a @SubBuilderExec@ with file reading (see @conPreview@).
smPreview :: MonadReadWorld m => SubBuilderExec -> DocResult m T.Text
smPreview sm = do
  allParams <- smAllParams sm
  case allParams of
    [] -> return ""
    _ ->
      return $
      ("Execution with these params:\n" :: T.Text) <>
      T.unlines (map showPathedParams allParams)

-- | Evaluate a @SubBuilderExec@. This is done by running the builder on all
--   specified parameters, and on all parameters in the specified files.
smEvaluate :: MonadReadWorld m => SubBuilderExec -> DocResult m T.Text
smEvaluate sme = do
  params <- smAllParams sme
  con <- builderWithParams (smBuilder sme) params
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

-- | Public function for creating a @Text@ (the @Content@) from a @Data.Text@.
text = Text

-- | Public function for creating a @Text@ (the @Content@) from a @String@.
string str = Text $ T.pack str

-- | Public function for creating a @Snippet@.
snippet = Snippet

-- | Public function for creating a @Transform@.
transform = Transform

-- | Public function for creating a @TransformError@.
transformError = TransformError

-- | Public function for creating a @Doc@.
doc = Doc

-- | Public function for creating a @SubBuilder@.
subBuilder = SubBuilder

-- | Public function for creating a @SubBuilderExec@.
subBuilderExec = SubBuilderExec

-- | Public function for creating a @ContentList@.
contentList = ContentList

-- | Public function for creating an @EmptyContent@.
emptyContent = EmptyContent

-- | Public function for creating a @NoContentAction@.
noContentAction = NoContentAction

-- | Public function for creating a @SingleContentAction@.
singleContentAction = SingleContentAction

-- | Public function for creating a @MultiContentAction@.
multiContentAction = MultiContentAction

-- | Public function for creating a @NoAction@.
noAction = NoAction

-- | Shorthand for creating an @Action@ that adds one @Content@ to text.
add :: Content -> Action
add c = SingleContentAction c func "Add: "
  where
    func a b = a <> b

-- | Shorthand for creating an @Action@ that replaces all occurances of some text
--   with the @Content@.
replace :: T.Text -> Content -> Action
replace text c =
  SingleContentAction c func $ "Replace \"" <> text <> "\" with: "
  where
    func a b = T.replace text b a

-- | Shorthand for creating an @Action@ that adds text.
addText :: T.Text -> Action
addText t = add $ text t

-- | Shorthand for creating an @Action@ that replaces all occurances of some text
--   with other text.
replaceText :: T.Text -> T.Text -> Action
replaceText t1 t2 = replace t1 $ text t2

-- | Public function for creating a @SubBuilder@ with just one @SubBuilderExec@.
singleSubBuilder ::
     NamedBuilder -> Params -> [PathedParams] -> FilePathSet -> Content
singleSubBuilder m p pp fp = SubBuilder $ SubBuilderExec m p pp fp

-- | Shorthand for creating a @SubBuilder@ that executes the builder on one file.
subBuilderOnFile :: NamedBuilder -> FilePath -> Content
subBuilderOnFile m f =
  subBuilder $ SubBuilderExec m emptyParams [] (HS.singleton f)
