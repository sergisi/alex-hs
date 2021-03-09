# Instructions

This exercise, although it is not necessary, Lenses makes it easier
to work with the Accumulator, as it lets me change the module. To install the library, we used a cabal.

Also, as it is marked as Safe haskell the Main.x, it disables the
TemplateHaskell extension that makes it easier to use lenses, so we
made a separeted module for the Types and Data management (Types.hs).

To run use:

```bash
alex Main.x
cabal v2-run
```
