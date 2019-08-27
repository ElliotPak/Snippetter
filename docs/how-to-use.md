# How to Use Snippetter

Snippetter, to be honest, is quite complex, but is also quite extensive and
capable of complicated stuff. This document aims to explain how Snippetter
works and how to use it for your own projects.

## Quick Start

- Create a builder (see [Builders](#builders))
- Create a layout file (see [Layout Files](#layout-files))
- In your main Haskell file, create a `HashMap Text Builder`.
- Import `Snippetter` and run `executeLayoutFile path map`, where `path` is the path to the layout file, and `map` is the `HashMap`.
- The webpage will be saved to the output file specified in your layout file.

## How it all works

### Content

"Content" represents operations that evaluate to text.

Built in Content types:

- `Text` (from `Data.Text`) for plain text.
- `Snippet`, which will load the specified file when evaluated.
- `SubBuilder`, which essentially "maps" a specified builder both parameters and the contents of parameter files.
- `Transform`, which will apply the specified function to the content that it
  contains. The function must have the type `Text -> Text`.
- `TransformError`, which will apply the specified function (which may fail) to
  the content that it contains. The function must have the type `Text -> Either
  Text Text`.

### Actions

`Action`s represent operations that, when given text, modify it in some way. The ultimate result of a `Builder` is a list of `Action`s.

Built in actions include:

- `add`, which adds the supplied content to the end of the text.
- `replace`, which replaces occurrences of a string within the text with the
  supplied content.

### Builders

A *builder* outlines the process of generating a file. Builders are (currently)
defined within Haskell, and look like this:

```haskell
import Snippetter.Layout
import Snippetter.Helpers

pageTitle :: Params -> Either BuilderError [Action]
pageTitle params = do
    title <- lookupString "title" params
    desc <- lookupString "desc" params
    entries <- lookupString "entries-file" params
    return [add $ snippet "resources/snippets/pageTitle.html",
            replaceText "%TITLE%" title,
            replaceText "%DESC%" desc,
            replace "%ENTRIES%" (subBuilderOnFile entryTitle entries)
           ]
```

The `Params` of a builder are an Aeson object, and its values can be retrieved
as a specific type (with error handling) with the helper functions in
`Snippetter.Helpers`.

### Parameter Files

*Parameter files* are YAML files containing a list of objects. They are
intended to be used with certain types of content, such as `SubBuilder`.

### Layout Files

*Layout files* are YAML files that detail actions that Snippetter should
execute. While they can specify files to be copied or moved, it primarily
outlines which files should be created, which builder they should use, and what
parameters to give that builder.  
A layout file looks like this:

```yaml
- type: copy
  from: copyover/
  to: build/
- type: builder
  output: build/index.html
  builder-name: homepage
  values:
    title: Some Website Title
    desc: oh hey its a website, thats pretty neat
    entries-file: resources/entries/index.yaml
```
