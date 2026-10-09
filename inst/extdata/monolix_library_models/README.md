# Monolix library models

The Monolix PK library structural models: the `.txt` files that a `lib:`
reference in a `.mlxtran` resolves to, and the pool that
`read_structural_model()` searches when a model uses `iv()`, `oral()` or
`absorption()` macros.

These models ship with the package, so `lib:` resolution works on any
machine with no configuration. `monolix_library_dir()` tries, in order:

1. `getOption("MN2mrg.monolix_library_path")`
2. the `MN2MRG_MONOLIX_LIBRARY` environment variable
3. this directory

taking the first that exists and holds at least one `.txt` file.

Explicit configuration outranks this directory on purpose, so a site can
point at its own snapshot and actually get it. To take the shipped copy out
of consideration entirely, set
`options(MN2mrg.monolix_library_packaged = "")` and declare a path.

A directory holding no `.txt` file is skipped rather than treated as a
match, and when no candidate resolves the error names every location tried.
There is no silent fallback.

See `?monolix_library_dir` for the full resolution order.
