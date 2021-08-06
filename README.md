# ghc-name-catcher

A GHC plugin for extracting names from code.

You can activate ghc-name-catcher by passing the ghc options
`-fplugin=GhcNameCatcher` and `-fplugin-opt=GhcNameCatcher:path/to/output/dir`.
For every module compiled with ghc-name-catcher enabled, a file will be created
in the output directory. Currently the format of this file is a simple CSV where
the first value in every row is a type constructor and all subsequent values in
the row are variables that have been used with that type constructor.

See `test/Spec.hs` and `example/` for usage examples.

## Supported GHC Versions

GHC versions 8.6.3-9.0.1 are supported. If you need another version feel free to
open an issue.
