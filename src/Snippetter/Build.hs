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

-- | Shorthand for a list of Actions.
type Doc = [Action]

-- | The return type for a function that reads files and can fail.
type DocFileResult m a = ExceptT DocError m a

-- | The return type for an operation relating to building a page that doesn't
--   require some other monad.
type DocResult = Either DocError Doc

-- | The return type of a Macro function.
type MacroResult = Either MacroError Doc

-- | Shorthand for a macro's parameters.
-- Identical to the type of an Aeson object.
type Params = H.HashMap T.Text Value

-- | Shorthand for a macro's type signature.
type Macro = Params -> MacroResult

-- | A Params value that may have a file path associated with it.
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

-- | Things that can go wrong while a page is being built.
data DocError =
    DocMacroError MacroError (Maybe FilePath)
  | DocFileError FileError
  | DocYamlError YamlError
  | MissingMacro T.Text
  | MiscDocError T.Text
    deriving (Show, Eq)

docNeededFiles :: MonadReadFile m => Doc -> DocFileResult m [FilePath]
docNeededFiles doc = do
    mapped <- mapM actNeededFiles doc
    return $ concat mapped

docExecute :: MonadReadFile m => Doc -> T.Text -> DocFileResult m T.Text
docExecute [] text     = return text
docExecute (x:xs) text = do
    head <- actExecute x text
    docExecute xs head

yamlAsParams :: MonadReadFile m => FilePath -> DocFileResult m [Params]
yamlAsParams path = let errorMapping x = DocYamlError x in
    (yamlIfExists path :: MonadReadFile m => YamlResult m [Params])
        `mapResultError` errorMapping

-- | Load all paramaters from a parameter file as PathedParams.
paramsFromFile :: MonadReadFile m => FilePath -> DocFileResult m [PathedParams]
paramsFromFile file = do
    values <- yamlAsParams file
    let addPath x = PathedParams x $ Just file
    return $ map addPath values

-- | Create a Doc by running a Macro, using the values of a YAML collection as
--   its parameters.
macroOnEntryFile :: MonadReadFile m => Macro -> FilePath -> DocFileResult m Doc
macroOnEntryFile macro path = do
    fromFile <- paramsFromFile path
    liftEither $ macroOnValues macro fromFile

-- | Create a Doc by running a Macro consecutively, using each entry in the
--   PathedParams list as parameters.
macroOnValues :: Macro -> [PathedParams] -> DocResult
macroOnValues m v = do
    mapped <- mapM (executeMacro m) v
    return $ concat mapped

executeMacro :: Macro -> PathedParams -> DocResult
executeMacro m (PathedParams params path) =
    mapLeft (convertMacroError path) $ m params
    where convertMacroError path err = DocMacroError err path

-- | Content represents operations that evaluate to text.
data Content =
    Text T.Text
  | Snippet FilePath
  | Transform (T.Text -> T.Text) Content
  | TransformError (T.Text -> Either T.Text T.Text) Content
  | SubMacro SubMacroExec
  | Doc [Action]

data SubMacroExec = SubMacroExec {
    smMacro   :: Macro
  , smDefault :: Params
  , smParams  :: [PathedParams]
  , smFiles   :: [FilePath]
  } -- deriving (Show, Eq)

-- | Load parameters from all parameter files in a SubMacro, without default
--   parameters applied.
smFileParams :: MonadReadFile m => SubMacroExec -> DocFileResult m [PathedParams]
smFileParams sm = do
    params <- mapM paramsFromFile (smFiles sm)
    return $ concat params

-- | Load all parameters in a SubMacro, with default parameters applied.
smAllParams :: MonadReadFile m => SubMacroExec -> DocFileResult m [PathedParams]
smAllParams sm = do
    fileParams <- smFileParams sm
    let allParams = smParams sm ++ fileParams
    let defaulted = map (pathedParamUnion $ smDefault sm) allParams
    return defaulted

pathedParamUnion = undefined

smNeededFiles :: MonadReadFile m => SubMacroExec -> DocFileResult m [FilePath]
smNeededFiles sm = do
   params <- smAllParams sm
   entries <- liftEither $ macroOnValues (smMacro sm) params
   containing <- docNeededFiles entries
   return $ removeDuplicates $ (smFiles sm) ++ containing

smPreview :: Int -> SubMacroExec -> T.Text
smPreview indent (SubMacroExec m p pp fp) =
    undefined

smPreviewDryRun :: MonadReadFile m => Int -> SubMacroExec -> DocFileResult m T.Text
smPreviewDryRun indent (SubMacroExec m p pp fp) =
    undefined

smEvaluate :: MonadReadFile m => SubMacroExec -> DocFileResult m T.Text
smEvaluate sme = do
       params <- smAllParams sme
       doc <- liftEither $ macroOnValues (smMacro sme) params
       docExecute doc T.empty

conNeededFiles :: MonadReadFile m => Content -> DocFileResult m [FilePath]
conNeededFiles (Text _) = return []
conNeededFiles (Snippet s) = return [s]
conNeededFiles (Transform _ c) = conNeededFiles c
conNeededFiles (TransformError _ c) = conNeededFiles c
conNeededFiles (SubMacro sme) = smNeededFiles sme
conNeededFiles (Doc d) = docNeededFiles d

conPreview :: Int -> Content -> T.Text
conPreview indent (Text t) = indentFour indent $ "\"" <> t <> "\""
conPreview indent (Snippet s) =
    indentFour indent $ "Snippet named \"" <> (T.pack s) <> "\""
conPreview indent (Transform f c) = do
    indentFour indent "Transformation of: " <\> pre
    where pre = conPreview (indent + 1) c
conPreview indent (TransformError f c) =
    indentFour indent "Transformation of: " <\> pre
    where pre = conPreview (indent + 1) c
conPreview indent (SubMacro sme) = smPreview indent sme
conPreview indent (Doc d) = ""

fileContentsInDoc :: MonadReadFile m => FilePath -> DocFileResult m T.Text
fileContentsInDoc path = getFileContents path `mapResultError` mapping
    where mapping err = DocFileError err

conPreviewDryRun :: MonadReadFile m => Int -> Content -> DocFileResult m T.Text
conPreviewDryRun indent (Text t) = return $ "\"" <> t <> "\""
conPreviewDryRun indent (Snippet s) = do
   contents <- fileContentsInDoc s
   let nameSegment = "Snippet named \"" <> (T.pack s) <> "\" with the contents:"
   return $ indentFour indent nameSegment <> "\n"
       <> indentFour (indent + 1) contents
conPreviewDryRun indent (Transform f c) = do
    dryRun <- conPreviewDryRun (indent + 1) c
    return $ indentFour indent "Transformation of: " <\> dryRun
conPreviewDryRun indent (TransformError f c) = do
    dryRun <- conPreviewDryRun (indent + 1) c
    return $ indentFour indent "Transformation of: " <\> dryRun
conPreviewDryRun indent (SubMacro sme) = smPreviewDryRun indent sme
conPreviewDryRun indent (Doc d) = return ""

conEvaluate :: MonadReadFile m => Content -> DocFileResult m T.Text
conEvaluate (Text t) = return t
conEvaluate (Snippet s) = fileContentsInDoc s
conEvaluate (Transform f c) = do
    sub <- conEvaluate c
    return $ f sub
conEvaluate (TransformError f c) = do
    sub <- conEvaluate c
    let result = mapLeft (\e -> MiscDocError e) $ f sub
    liftEither $ result
conEvaluate (SubMacro sme) = smEvaluate sme
conEvaluate (Doc d) = docExecute d T.empty

-- | Actions represent operations that transform text in some way.
data Action =
    SingleContentAction Content (T.Text -> T.Text -> T.Text)

actNeededFiles :: MonadReadFile m => Action -> DocFileResult m [FilePath]
actNeededFiles (SingleContentAction c _) = conNeededFiles c

actExecute :: MonadReadFile m => Action -> T.Text -> DocFileResult m T.Text
actExecute (SingleContentAction c f) text = do
    evaluated <- conEvaluate c
    return $ f text evaluated

text = Text
snippet = Snippet
transform = Transform
transformError = TransformError
subMacro = SubMacro
doc = Doc

add :: Content -> Action
add c = SingleContentAction c func
    where func a b = a <> b

replace :: T.Text -> Content -> Action
replace text c = SingleContentAction c func
    where func a b = a <> b

addText :: T.Text -> Action
addText t = add $ text t

replaceText :: T.Text -> T.Text -> Action
replaceText t1 t2 = replace t1 $ text t2

macroOnFile :: Macro -> FilePath -> Content
macroOnFile m f = subMacro $ SubMacroExec m H.empty [] [f]
