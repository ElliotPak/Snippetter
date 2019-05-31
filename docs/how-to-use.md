# How to Use Snippetter

Snippetter, to be honest, is quite complex, but is also quite extensive and
capable of complicated stuff. This document aims to explain how Snippetter
works and how to use it for your own projects.

## Quick Start

- Create a macro (see [Macros](#macros))
- Create a layout file (see [Layout Files](#layout-files))
- In your main Haskell file, create a `HashMap Text Macro`.
- Import `Snippetter` and run `executeLayoutFile path map`, where `path` is the path to the layout file, and `map` is the `HashMap`.
- The webpage will be saved to the output file specified in your layout file.

## How it all works

### Content

"Content" represents operations that evaluate to text. In order to make a new datatype that acts as content, it must be an instance of `Contentable` (in `Snippetter.Layout`).

Built in `Contentable` types include:

- `Text` (from `Data.Text`) for plain text.
- `Snippet`, which will load the specified file when evaluated.
- `MacroOnFile`, which essentially "maps" the specific macro across the
  contents of the specified parameter file (see below).
- `Transform`, which will apply the specified function to the content that it
  contains. The function must have the type `Text -> Text`.
- `TransformError`, which will apply the specified function (which may fail) to
  the content that it contains. The function must have the type `Text -> Either
  DocError Text`.

### Actions

`Action`s represent operations that, when given text, modify it in some way. In
order to make a new datatype that acts as content, it must be an instance of
`Actionable` (in `Snippetter.Layout`). Unlike content, an existential type
called `Action` exists solely to wrap all `Actionable` data types, and the
ultimate result of a `Macro` is a list of `Action`s.

Built in `Actionable` types include:

- `Add`, which adds the supplied content to the end of the text.
- `Replace`, which replaces occurrences of a string within the text with the
  supplied content.
- `Action`, which executes the action it contains.
- `[Action]`, which executes all actions in the list sequentially.

### The NeedsFiles class

While not currently used, Snippetter can determine what files are needed to
build a webpage. This is done via the `NeedsFiles` class. The `Contentable` and
`Actionable` classes require an instance to also be an instance of `NeedsFiles`
for this reason, although the default implementation returns an empty list.
`Actionable` instances should delegate to the content they contains, and
`Contentable` instances should be decided on a case-by-case basis.

### Macros

A *macro* outlines the process of generating a file. Macros are (currently)
defined within Haskell, and look like this:

```haskell
import Snippetter.Layout
import Snippetter.Helpers

pageTitle :: Params -> Either DocError [Action]
pageTitle params = do
    title <- lookupString "title" params
    desc <- lookupString "desc" params
    entries <- lookupString "entries-file" params
    return [add $ Snippet "resources/snippets/pageTitle.html",
            replaceText "%TITLE%" title,
            replaceText "%DESC%" desc,
            replace "%ENTRIES%" $ add (MacroOnFile entryTitle entries)
           ]
```

The `Params` of a macro are an Aeson object, and its  values can be retrieved
as a specific type (with error handling) with the helper functions in
`Snippetter.Helpers`.

### Parameter Files

*Parameter files* are YAML files containing a list of objects. They are
intended to be used with certain types of content, such as `MacroOnFile`.

### Layout Files

*Layout files* are YAML files that detail actions that Snippetter should
execute. While they can specify files to be copied or moved, it primarily
outlines which files should be created, which macro they should use, and what
parameters to give that macro.  
A layout file looks like this:

```yaml
- type: copy
  from: copyover/
  to: build/
- type: macro
  output: build/index.html
  macro-name: homepage
  values:
    title: Some Website Title
    desc: oh hey its a website, thats pretty neat
    entries-file: resources/entries/index.yaml
```
