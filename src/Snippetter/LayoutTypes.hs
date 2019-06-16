{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Snippetter.LayoutTypes where

import Snippetter.LayoutBase
import Control.Monad.Except
import Control.Monad.Trans.Except
import qualified Data.Yaml as Y
import qualified Data.Text as T

-- | Represents a file to be loaded.  
-- This content is resolved by inserting the contents of the file at the
-- specified path.
data Snippet = Snippet FilePath

instance NeedsFiles Snippet where
    getNeededFiles (Snippet s) = return [s]

instance Contentable Snippet where
    resolve (Snippet p) = getFileContents p

-- | Represents using a macro on a set of parameters. These parameters can be
-- provided in Haskell code, or from reading for a file, and also has default
-- parameters associated with it.
-- This content is resolved by loading the YAML file at the specified path, and
-- running the macro multiple times, with each entry as its parameters.
data SubMacro = SubMacro Macro Params [Params] FilePath

instance NeedsFiles SubMacro where
    getNeededFiles (SubMacro m def list file) = do
        entries <- macroOnEntryFile m file
        containing <- getNeededFiles entries
        return $ file : containing

instance Contentable SubMacro where
    resolve (SubMacro m def list file) = do
        doc <- macroOnEntryFile m file
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
