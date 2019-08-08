{-# LANGUAGE OverloadedStrings #-}

module Snippetter.Build where

import Snippetter.Utilities
import Snippetter.IO
import Control.Monad.Except
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Data.Aeson.Types (Object, Value (Object, String))
import Data.Yaml
import System.Directory (doesFileExist)
import qualified Data.Yaml as Y
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H

-- | Shorthand for a list of @Action@.
type Doc = [Action]

-- | Possible errors when a page is being built.
data DocError =
    DocMacroError MacroError (Maybe FilePath)
  | DocFileError FileError
  | DocYamlError YamlError
  | MissingMacro T.Text
  | MiscDocError T.Text
    deriving (Show, Eq)

-- | The result a function that builds a page and can read files.
type DocFileResult m a = ExceptT DocError m a

-- | The result of an operation relating to building a page that doesn't
--   require some other monad.
type DocResult = Either DocError Doc

-- | The result of a @Macro@ function.
type MacroResult = Either MacroError Doc

-- | Shorthand for a macro's parameters.
-- Identical to the type of an Aeson object.
type Params = H.HashMap T.Text Value

-- | Shorthand for a @Macro@'s type signature.
type Macro = Params -> MacroResult

-- | A @Params@ value that may have a file path associated with it.
--   If loaded from a file, the path should be assigned when doing so.
--   If defined in a source file, the path should be @Nothing@.
data PathedParams = PathedParams {
    params :: Params,
    ppath   :: Maybe FilePath
  } deriving (Show, Eq)

-- | Things that can go wrong while a macro is being run.
data MacroError =
    AbsentKey T.Text
  | WrongKeyType T.Text
  | MiscMacroError T.Text
    deriving (Show, Eq)

-- | Retrieves contents of the specific file, and maps possible errors to
--   @DocFileError@s.
fileContentsInDoc :: MonadReadFile m => FilePath -> DocFileResult m T.Text
fileContentsInDoc path = getFileContents path `mapResultError` mapping
    where mapping err = DocFileError err

-- | Determine files needed by a @Doc@.
docNeededFiles :: MonadReadFile m => Doc -> DocFileResult m [FilePath]
docNeededFiles doc = do
    mapped <- mapM actNeededFiles doc
    return $ concat mapped

-- | Execute all @Actions@ within a @Doc@.
docExecute :: MonadReadFile m => Doc -> T.Text -> DocFileResult m T.Text
docExecute [] text     = return text
docExecute (x:xs) text = do
    head <- actExecute x text
    docExecute xs head

-- | Load a YAML file as a list of @Params@.
yamlAsParams :: MonadReadFile m => FilePath -> DocFileResult m [Params]
yamlAsParams path = let errorMapping x = DocYamlError x in
    (yamlIfExists path :: MonadReadFile m => YamlResult m [Params])
        `mapResultError` errorMapping

-- | Load all paramaters from a (possible) paramater file as @PathedParams@.
paramsFromFile :: MonadReadFile m => FilePath -> DocFileResult m [PathedParams]
paramsFromFile file = do
    values <- yamlAsParams file
    let addPath x = PathedParams x $ Just file
    return $ map addPath values

-- | Create a @Doc@ by running a @Macro@, using the values of a YAML collection
--   as its parameters.
macroOnEntryFile :: MonadReadFile m => Macro -> FilePath -> DocFileResult m Doc
macroOnEntryFile macro path = do
    fromFile <- paramsFromFile path
    liftEither $ macroOnValues macro fromFile

-- | Create a @Doc@ by running a @Macro@ consecutively, using each entry in the
--   PathedParams list as parameters.
macroOnValues :: Macro -> [PathedParams] -> DocResult
macroOnValues m v = do
    mapped <- mapM (buildDoc m) v
    return $ concat mapped

-- | Build a @Doc@ by running a @Macro@ with the given parameters.
buildDoc :: Macro -> PathedParams -> DocResult
buildDoc m (PathedParams params path) =
    mapLeft (convertMacroError path) $ m params
    where convertMacroError path err = DocMacroError err path

executeMacro :: MonadReadFile m => Macro -> PathedParams -> DocFileResult m T.Text
executeMacro m pp = do
    doc <- liftEither $ buildDoc m pp
    docExecute doc T.empty

filesForMacro :: MonadReadFile m => Macro -> PathedParams -> DocFileResult m [FilePath]
filesForMacro m pp =
    (liftEither $ buildDoc m pp) >>= docNeededFiles

-- | Represents operations that evaluate to text.
data Content =
    Text T.Text
  | Snippet FilePath
  | Transform Content (T.Text -> T.Text)
  | TransformError Content (T.Text -> Either T.Text T.Text)
  | SubMacro SubMacroExec
  | Doc [Action]

-- | Specifies the @Macro@ to use in a @SubMacro@ and what it should execute
--   on.
data SubMacroExec = SubMacroExec {
    smMacro   :: Macro
  , smDefault :: Params
  , smParams  :: [PathedParams]
  , smFiles   :: [FilePath]
  } -- deriving (Show, Eq)

-- | Load parameters from all parameter files in a @SubMacroExec@, without
--   default parameters applied.
smFileParams :: MonadReadFile m => SubMacroExec -> DocFileResult m [PathedParams]
smFileParams sm = do
    params <- mapM paramsFromFile (smFiles sm)
    return $ concat params

-- | Load all parameters in a @SubMacroExec@, with default parameters applied.
smAllParams :: MonadReadFile m => SubMacroExec -> DocFileResult m [PathedParams]
smAllParams sm = do
    fileParams <- smFileParams sm
    let allParams = smParams sm ++ fileParams
    let defaulted = map (pathedParamDefault $ smDefault sm) allParams
    return defaulted

-- | Add fields present in the first @Params@ to the @PathedParams@ if they're
--   missing from the second.
pathedParamDefault :: Params -> PathedParams -> PathedParams
pathedParamDefault def (PathedParams params ppath) = 
    PathedParams (H.union params def) ppath

-- | Determine files needed by a @SubMacroExec@.
smNeededFiles :: MonadReadFile m => SubMacroExec -> DocFileResult m [FilePath]
smNeededFiles sm = do
   params <- smAllParams sm
   entries <- liftEither $ macroOnValues (smMacro sm) params
   containing <- docNeededFiles entries
   return $ removeDuplicates $ (smFiles sm) ++ containing

-- | Preview a @SubMacroExec@ (see @conPreview@).
smPreview :: Int -> SubMacroExec -> T.Text
smPreview indent (SubMacroExec m p pp fp) =
    undefined

-- | Preview a @SubMacroExec@ with file readinf (see @conPreviewDryRun@).
smPreviewDryRun :: MonadReadFile m => Int -> SubMacroExec -> DocFileResult m T.Text
smPreviewDryRun indent (SubMacroExec m p pp fp) =
    undefined

-- | Evaluate a @SubMacroExec@. This is done by running the macro on all
--   specified parameters, and on all parameters in the specified files.
smEvaluate :: MonadReadFile m => SubMacroExec -> DocFileResult m T.Text
smEvaluate sme = do
       params <- smAllParams sme
       doc <- liftEither $ macroOnValues (smMacro sme) params
       docExecute doc T.empty

-- | Determine files needed by a Content.
conNeededFiles :: MonadReadFile m => Content -> DocFileResult m [FilePath]
conNeededFiles (Text _) = return []
conNeededFiles (Snippet s) = return [s]
conNeededFiles (Transform c _) = conNeededFiles c
conNeededFiles (TransformError c _) = conNeededFiles c
conNeededFiles (SubMacro sme) = smNeededFiles sme
conNeededFiles (Doc d) = docNeededFiles d

-- | Preview a piece of Content. This is like @show@, except with an extra
--   field to indicate indenting.
conPreview :: Int -> Content -> T.Text
conPreview indent (Text t) = indentFour indent $ "\"" <> t <> "\""
conPreview indent (Snippet s) =
    indentFour indent $ "Snippet named \"" <> (T.pack s) <> "\""
conPreview indent (Transform c f) = do
    indentFour indent "Transformation of: " <\> pre
    where pre = conPreview (indent + 1) c
conPreview indent (TransformError c f) =
    indentFour indent "Transformation of: " <\> pre
    where pre = conPreview (indent + 1) c
conPreview indent (SubMacro sme) = smPreview indent sme
conPreview indent (Doc d) = ""

-- | Preview a piece of Content, similar to @conPreview@, except this version
--   may also read files to determine extra information.
conPreviewDryRun :: MonadReadFile m => Int -> Content -> DocFileResult m T.Text
conPreviewDryRun indent (Text t) = return $ "\"" <> t <> "\""
conPreviewDryRun indent (Snippet s) = do
   contents <- fileContentsInDoc s
   let nameSegment = "Snippet named \"" <> (T.pack s) <> "\" with the contents:"
   return $ indentFour indent nameSegment <> "\n"
       <> indentFour (indent + 1) contents
conPreviewDryRun indent (Transform c f) = do
    dryRun <- conPreviewDryRun (indent + 1) c
    return $ indentFour indent "Transformation of: " <\> dryRun
conPreviewDryRun indent (TransformError c f) = do
    dryRun <- conPreviewDryRun (indent + 1) c
    return $ indentFour indent "Transformation of: " <\> dryRun
conPreviewDryRun indent (SubMacro sme) = smPreviewDryRun indent sme
conPreviewDryRun indent (Doc d) = return ""

-- | Convert a @Content@ to text.
conEvaluate :: MonadReadFile m => Content -> DocFileResult m T.Text
conEvaluate (Text t) = return t
conEvaluate (Snippet s) = fileContentsInDoc s
conEvaluate (Transform c f) = do
    sub <- conEvaluate c
    return $ f sub
conEvaluate (TransformError c f) = do
    sub <- conEvaluate c
    let result = mapLeft (\e -> MiscDocError e) $ f sub
    liftEither $ result
conEvaluate (SubMacro sme) = smEvaluate sme
conEvaluate (Doc d) = docExecute d T.empty

-- | Represents operations that transform text in some way.
data Action =
    SingleContentAction Content (T.Text -> T.Text -> T.Text) T.Text

-- | Determine files needed by an @Action@ to execute.
actNeededFiles :: MonadReadFile m => Action -> DocFileResult m [FilePath]
actNeededFiles (SingleContentAction c _ _) = conNeededFiles c

-- | Execute the given @Action@ on the specified text.
actExecute :: MonadReadFile m => Action -> T.Text -> DocFileResult m T.Text
actExecute (SingleContentAction c f _) text = do
    evaluated <- conEvaluate c
    return $ f text evaluated

actPreview :: Int -> Action -> T.Text
actPreview indent (SingleContentAction c _ t) =
    indentFour indent t <\> conPreview (indent + 1) c

actPreviewDryRun :: MonadReadFile m => Int -> Action -> DocFileResult m T.Text
actPreviewDryRun indent (SingleContentAction c _ t) = do
    dryRun <- conPreviewDryRun (indent + 1) c
    return $ indentFour indent t <\> dryRun

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
-- | Public function for creating a @SubMacro@.
subMacro = SubMacro
-- | Public function for creating a @SubMacroExec@.
subMacroExec = SubMacroExec

-- | Shorthand for creating an @Action@ that adds one @Content@ to text.
add :: Content -> Action
add c = SingleContentAction c func "Add: "
    where func a b = a <> b

-- | Shorthand for creating an @Action@ that replaces all occurances of some text
--   with the @Content@.
replace :: T.Text -> Content -> Action
replace text c = SingleContentAction c func $ "Replace \"" <> text <> "\" with: "
    where func a b = T.replace text b a

-- | Shorthand for creating an @Action@ that adds text.
addText :: T.Text -> Action
addText t = add $ text t

-- | Shorthand for creating an @Action@ that replaces all occurances of some text
--   with other text.
replaceText :: T.Text -> T.Text -> Action
replaceText t1 t2 = replace t1 $ text t2

-- | Public function for creating a @SubMacro@ with just one @SubMacroExec@.
singleSubMacro :: Macro -> Params -> [PathedParams] -> [FilePath] -> Content
singleSubMacro m p pp fp = SubMacro $ SubMacroExec m p pp fp

-- | Shorthand for creating a @SubMacro@ that executes the macro on one file.
macroOnFile :: Macro -> FilePath -> Content
macroOnFile m f = subMacro $ SubMacroExec m H.empty [] [f]
