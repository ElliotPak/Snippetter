{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Snippetter.LayoutTypes where

import Snippetter.LayoutBase
import Snippetter.Utilities
import Control.Monad.Except
import Control.Monad.Trans.Except
import qualified Data.Yaml as Y
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H
import qualified Data.ByteString.Char8 as B

-- | Represents a file to be loaded.  
-- This content is resolved by inserting the contents of the file at the
-- specified path.
data Snippet = Snippet FilePath

instance Previewable Snippet where
    preview indent (Snippet s) =
        indentFour indent $ "Snippet named \"" <> (T.pack s) <> "\""
    previewDryRun indent (Snippet s) = do
        contents <- getFileContents s
        let nameSegment = "Snippet named \"" <> (T.pack s) <> "\" with the contents:"
        return $ indentFour indent nameSegment <> "\n"
            <> indentFour (indent + 1) contents

instance NeedsFiles Snippet where
    getNeededFiles (Snippet s) = return [s]

instance Contentable Snippet where
    resolve (Snippet p) = getFileContents p

-- | Represents using a macro on a set of parameters. These parameters can be
-- provided in Haskell code, or from reading for a file. A set of default
-- parameters is also provided.
-- This content is resolved by running the macro multiple times, with each
-- entry as its parameters. Entries are provided by files/other parameter sets
data SubMacro = SubMacro {
    smMacro   :: Macro
  , smDefault :: Params
  , smParams  :: [PathedParams]
  , smFiles   :: [FilePath]
  }

-- | Load parameters from all parameter files in a SubMacro, without default
--   parameters apaplied.
smFileParams :: MonadReadFile m => SubMacro -> DocResult m [PathedParams]
smFileParams sm = do
    params <- mapM paramsFromFile (smFiles sm)
    return $ concat params

-- | Load all parameters in a SubMacro, with default parameters applied.
smAllParams :: MonadReadFile m => SubMacro -> DocResult m [PathedParams]
smAllParams sm = do
    fileParams <- smFileParams sm
    let allParams = smParams sm ++ fileParams
    let defaulted = map (pathedParamUnion $ smDefault sm) allParams
    return defaulted

instance NeedsFiles SubMacro where
    getNeededFiles sm = do
        params <- smAllParams sm
        entries <- liftEither $ macroOnValues (smMacro sm) params
        containing <- getNeededFiles entries
        return $ (smFiles sm) ++ containing

instance Previewable SubMacro where
    preview indent (SubMacro m def params files) = indentFour indent complete
        where ind = indentFour (indent + 1)
              indd = indentFour (indent + 2)
              tDefaults
                | H.null def = ""
                | otherwise =
                    ind "\nDefault values:\n" <> indd (previewParams def)
              tParams
                | null params = ""
                | otherwise = 
                    ind "\nExecution with these parameters:\n"
                        <> indd (T.unlines $ map previewPathedParams params)
              tFile
                | null files = ""
                | otherwise =
                    ind "\nExecution on these files:\n"
                        <> indd (T.unlines . (map T.pack) $ files)
              complete = "Macro execution with the following:"
                  <> tDefaults <> tParams <> tFile
    previewDryRun indent sm = do
        allParams <- smAllParams sm
        let ind = indentFour (indent + 1)
        return $ "Macro execution with these parameters:\n"
            <> ind (T.unlines $ map previewPathedParams allParams)

instance Contentable SubMacro where
    resolve sm = do
        params <- smAllParams sm
        doc <- liftEither $ macroOnValues (smMacro sm) params
        resolveContents doc T.empty

-- | Shorthand for a function that transforms text.
type TextFunc = T.Text -> T.Text

-- | Shorthand for a function that transforms text and can fail.
type TextFuncError = T.Text -> Either DocError T.Text

-- | Represents a transformation of other content.
-- This content is resolved by resolving its child and applying its function to
-- them.
data Transform = forall c. (Contentable c) => Transform c TextFunc

instance NeedsFiles Transform where
    getNeededFiles (Transform c f) = getNeededFiles c

instance Previewable Transform where
    preview indent (Transform c f) =
        indentFour indent "Transformation of: " <\> preview (indent + 1) c
    previewDryRun indent (Transform c f) = do
        dryRun <- previewDryRun (indent + 1) c
        return $ indentFour indent "Transformation of: " <\> dryRun

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

instance Previewable TransformError where
    preview indent (TransformError c f) =
        indentFour indent "Transformation of: " <\> preview (indent + 1) c
    previewDryRun indent (TransformError c f) = do
        dryRun <- previewDryRun (indent + 1) c
        return $ indentFour indent "Transformation of: " <\> dryRun

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

instance Previewable Add where
    preview indent (Add a) =
        indentFour indent "Add: " <\> preview (indent + 1) a
    previewDryRun indent (Add a) = do
        dryRun <- previewDryRun (indent + 1) a
        return $ indentFour indent "Add: " <\> dryRun

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

instance Previewable Replace where
    preview indent (Replace t d) =
        indentFour indent ("Replace \"" <> t <> "\" with: ") <\> preview indent d
    previewDryRun indent (Replace t d) = do
        dryRun <- previewDryRun indent d
        return $ indentFour indent ("Replace \"" <> t <> "\" with: ") <\> dryRun

instance Actionable Replace where
    resolveContents (Replace t d) text = do
        dd <- resolveContents d T.empty
        return $ T.replace t dd text
