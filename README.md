# Snippetter

Snippetter is a static website generation library for Haskell.

Snippetter is a **WORK IN PROGRESS**, and currently doesn't have all of its
functionality.

To learn how to use Snippetter, see [docs/how-to-use.md](docs/how-to-use.md).

## Installation

At this stage, you probably shouldn't be using Snippetter, because things may
change wildly while I'm still figuring things out, but after cloning this
repository you can use Cabal to install it like so:

```sh
cabal update
cabal install
```

You must have the Haskell platform installed.

## Future Plans

The next things to be added are, in no particular order:

- More flexible parameter file usage
- Defining simple macros in YAML
- Checking dependencies before building
- Theming
- Separate build folders
- Moving files and shell command execution as site actions
- Optional parameters
