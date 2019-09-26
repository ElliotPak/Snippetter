# Snippetter

Snippetter is a Haskell library that generates text files through templating.
It's intended use is static website generation, but nothing Snippetter does is
webpage-specific, meaning you can use it to generate any kind of text file. It
also can execute other processes on your system if you so choose.

Snippetter is a **WORK IN PROGRESS**, and currently doesn't have all of its
functionality.

To see how to use Snippetter, see the source file for
[Snippetter.Tutorial](src/Snippetter/Tutorial.hs).

To install, add `snippetter >=0.1 && <0.2` to the `build-depends` in your cabal
file.
