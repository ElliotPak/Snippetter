{-# LANGUAGE OverloadedStrings #-}

module Snippetter.MacroTest where

import Control.Monad.Trans.Except
import Data.HashMap.Strict (HashMap, lookup, fromList)
import Snippetter.Layout
import Snippetter.Build
import Snippetter.Helpers
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

macroMap :: HashMap T.Text Macro
macroMap = fromList [ ("title-page", pageTitle),
                      ("title-entry", entryTitle)
                    ]

displayDocResult :: DocFileResult IO T.Text -> IO ()
displayDocResult doc = do
    unwrapped <- runExceptT doc
    putStrLn $ show unwrapped

lookupTitle = lookupText "title"
lookupName = lookupText "name"
lookupDesc = lookupText "desc"
lookupLink = lookupText "link"

pageStandard :: Params -> MacroResult
pageStandard params = do
    title <- lookupTitle params
    desc <- lookupDesc params
    return [add $ snippet "build-snippets/header.html",
            add $ snippet "resources/snippets/pageStandard.html",
            replace "%CONTENT%" $ doc
                [add $ snippet "resources/snippets/title.html",
                 add $ snippet "resources/snippets/paragraph.html",
                 replaceText "%TITLE%" title,
                 replaceText "%DESC%" desc,
                 addText "%ENTRIES%"
                ]
           ]

entryTitle :: Params -> MacroResult
entryTitle params = do
    title <- lookupTitle params
    desc <- lookupDesc params
    link <- lookupLink params
    return [add $ snippet "resources/snippets/entryTitle.html",
            replaceText "%TITLE%" title,
            replaceText "%DESC%" desc,
            replaceText "%LINK%" link
           ]

pageTitle :: Params -> MacroResult
pageTitle params = do
    title <- lookupTitle params
    desc <- lookupDesc params
    entries <- lookupText "entries-file" params
    return [add $ snippet "resources/snippets/pageTitle.html",
            replaceText "%TITLE%" title,
            replaceText "%DESC%" desc,
            replace "%ENTRIES%" $ macroOnFile entryTitle $ T.unpack entries
           ]
