{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Explains how to use Snippetter.
module Snippetter.Tutorial
  ( -- $docs
  ) where

import Snippetter.Utilities
import Snippetter.IO
import Snippetter.Build
import Snippetter.Layout
import Snippetter.Helpers
import Snippetter.DepManager
import Data.Text (Text)

{- $docs
    = Introduction

    Snippetter is a Haskell library that generates text files through
    templating. It's intended use is static website generation, but nothing
    Snippetter does is webpage-specific, meaning you can use it to generate any
    kind of text file. It also can execute other processes on your system if
    you so choose.

    The overall outline of how to use Snippetter is as follows:

      1. Create one or more 'PageBuilder's in your Haskell source, which
         specify how to build pages.
      2. Put them into a 'BuilderMap' via 'insertPageBuilder'.
      3. Create one or more layout files, which specify what actions to
         execute.
      4. Use a function that takes the 'BuilderMap' and one or more layout
         files to build the site.
    
    In addition to this, you may need to create snippets, parameter files, and so on.

    = How to Build

    After you have a 'BuilderMap' and created your layout files, you can use
    one of the following functions to execute the build process:

    * 'updateLayout' will execute each action that the layout file specifies that is
      not "up-to-date" (as in, at least one of its dependencies is older than
      its target file)
    * 'executeLayout' will execute each action that the layout file specifies
      in order, even if they aren't "up-to-date".

    There are also many functions to preview things about the layout files:

    * 'showLayout' shows the steps taken to execute all the actions from the file.
    * 'previewLayout' is like 'showLayout' but with more detail.
    * 'showLayoutDeps' shows the dependencies of all the actions from the file.
    * 'showLayoutOutput' shows the output files of all the actions from the file.

    These functions will work on all files from the layout file, but if you
    only want to include ones that will be updated when invoking
    'updateLayout', you can instead use 'showLayoutNeeded',
    'previewLayoutNeeded', 'showLayoutDepsNeeded', and
    'showLayoutOutputNeeded'.

    There are also variants that operate directly on a list of 'SiteAction's,
    which can be used by replacing @Layout@ in the name with @Actions@. See
    "Snippetter.Layout" and "Snippetter.DepManager" for more info.

    = Page Builders

    'PageBuilder's specify how to take a set of parameters (i.e. a JSON object)
    and convert that into an instance of 'Content', which represents a file
    before it's been built. A builder looks something like the following:

    > pageTitle :: PageBuilder
    > pageTitle params = do
    >   title <- lookupText "title" params
    >   desc <- lookupText "desc" params
    >   entries <- lookupText "entries-file" params
    >   let initial = snippet "resources/snippets/pageTitle.html"
    >   return $
    >     doc initial
    >       [ replaceText "%TITLE%" title
    >       , replaceText "%DESC%" desc
    >       , replace "%ENTRIES%" $
    >         subBuilderOnFile (NamedPageBuilder "entryTitle" entryTitle) $
    >         T.unpack entries
    >       ]
    
    A 'PageBuilder' has the type @Params -> Either BuilderError Content@, where
    the Params is an Aeson object. If you want the process to fail, instead
    of returning, you can have the 'PageBuilder' evaluate to a @Left (some kind
    of BuilderError)@. There are multiple functions within
    "Snippetter.Helpers" that can help with making the 'PageBuilder'. Note that
    most functions take a 'NamedPageBuilder' instead of just a 'PageBuilder',
    so that a name can be associated with it. You should only share names
    between builders if the two builder functions are equal.

    A 'NamedPageBuilder' returns a 'Content' on success, which will be
    evaluated to text when building the page. The \"primitive\" instances of
    'Content' you can create are:

    * 'text' creates a 'Content' that represents literal text.
    * 'snippet' creates a 'Content' that represents a file.
    * 'transform' creates a 'Content' that transforms some other 'Content'. To
      transform using a function that doesn't fail, use 'transformSafe' from
      "Snippetter.Helpers".
    * 'subBuilder' creates a 'Content' that executes one or more
      'NamedPageBuilder' on one or more sets of parameters consecutively. You
      need to provide one or more 'SubBuilderExec's to the 'subBuilder': each
      of them specifies a different 'NamedPageBuilder' and what parameters it
      uses.
    * 'doc' creates a 'Content' that applies one or more 'Action's to another
      'Content'.
    * 'emptyContent' creates a 'Content' that evaluates to nothing.
    
    There are more functions that create 'Content' available in
    "Snippetter.Helpers".

    The files specified in a 'SubBuilderExec' must be YAML files and contain a
    list of objects. The 'SubBuilderExec' will execute on each provided
    'Params', and then each entry in the list from each loaded file. You can
    also specify a function that transforms the list in some way, such as
    sorting or filtering, by providing a 'SBListFunc'. Functions that can help
    with this can be found in "Snippetter.Helpers".

    'doc' requires you to specify 'Action's, and the built in ones (from
    "Snippetter.Helpers") are as follows:

    * 'add' evaluates the given 'Content' and appends it to the end of the text.
    * 'replace' replaces instances of the given text with the result of
      evaluating the given 'Content'.

    You can also use 'noContentAction', 'singleContentAction', and
    'multiContentAction' to create your own 'Action's.

    = Meta Builders

    'MetaBuilders' work similarly to 'PageBuilder's in that they take a
    'Params', but they create other 'PathedSiteAction's instead of files. They
    also operate within the 'MonadReadWorld' monad, meaning they can directly
    read files, get the current time, and so on. There are no equivalents to
    'snippet' or 'subBuilder' for 'MetaBuilder's; instead you can read files
    directly.

    = Layout Files

    Aside from your Haskell source files, the other type of file that you
    should provide is an /layout file/, a YAML file that specifies what actions
    to execute. It should be a list of objects, and each object should be one
    of the following:

    == Page Builder

    /Page builder/ actions specify a 'NamedPageBuilder' to use, what parameters
    to use with it, and the destination file.

    > type: build-page
    > output: STRING
    > builder-name: STRING
    > parameters: OBJECT

    * @output@ corresponds to the output file.
    * @builder-name@ indicates which 'NamedPageBuilder' from the builder map to
      use.
    * @parameters@ indicates the parameters that the 'NamedPageBuilder' will be
      run with.

    == Meta Builder

    /Meta builder/ actions specify a 'NamedMetaBuilder' to use and what
    parameters to use with it.

    > type: build-actions
    > builder-name: STRING
    > parameters: OBJECT

    * @builder-name@ indicates which 'NamedMetaBuilder' from the builder map to
      use.
    * @parameters@ indicates the parameters that the 'NamedMetaBuilder' will be
      run with.
    
    == Copy

    /Copy/ actions will copy a file/folder to the specified destination.

    > type: copy
    > from: STRING
    > to: STRING

    @from@ is the source and @to@ is the destination.
    
    == Move

    /Move/ actions will move a file/folder to the specified destination.

    > type: move
    > from: STRING
    > to: STRING

    @from@ is the source and @to@ is the destination.

    == Delete

    /Delete/ actions delete the file/folder specified by @file@.

    > type: delete
    > file: STRING

    == Run

    /Run/ actions will run a process in the current directory.

    > type: run-process
    > process: STRING
    > stdin: STRING

    @process@ is the executed process and @stdin@ will be used as the process's
    standard input.
-}
