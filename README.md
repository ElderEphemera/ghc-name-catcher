# ghc-name-catcher

A GHC plugin for extracting names from code.

You can activate ghc-name-catcher by passing the ghc options
`-fplugin=GhcNameCatcher` and `-fplugin-opt=GhcNameCatcher:path/to/output/dir`.
For every module compiled with ghc-name-catcher enabled, a file will be created
in the output directory. Currently the format of this file is a simple CSV where
the first value in every row is a type constructor and all subsequent value in
the row are variable that have been used with that type constructor.

See `test/Spec.hs` for an example of usage.
