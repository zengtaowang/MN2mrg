# MN2mrg

For a visual tour of the app with screenshots, see
[GET_STARTED.md](GET_STARTED.md).

An R package with a Shiny front end for translating population
pharmacokinetic and pharmacodynamic (PK/PD) models from Monolix and
NONMEM into mrgsolve, and for running the downstream simulations a
pharmacometrician typically needs after estimation.

## Why

Manual translation of Monolix or NONMEM population models to mrgsolve
is time-consuming, error-prone, and repetitive across projects.
MN2mrg automates the translation and integrates it with the
post-estimation workflow:

* Dosing regimen exploration
* Sensitivity analysis
* Visual predictive checks (VPC), simulated in-session on the host
  running the app (no job scheduler required)
* Forest plots for covariate effects
* Reproducibility bundles (download a `.zip` that regenerates the VPC
  off-app for submission archives; the script needs MN2mrg installed)

## Status

Version 0.0.1.0000, in active development. The source has been
restructured into a proper R package: exported functions live under
`R/`, the Shiny app lives at `inst/shiny/app.R`, and
`MN2mrg::run_app()` is the single entry point.

## Installation

```r
# From GitHub. Add ref = "<tag>" to pin a release instead of the
# default branch.
remotes::install_github("zengtaowang/MN2mrg")

# Or from a local clone:
devtools::install()
```

`lixoftConnectors` is required for Monolix
model translation (the package installs and its NONMEM-only paths run
without it). It ships
with MonolixSuite; see
<https://monolixsuite.slp-software.com/r-functions/2024R1/package-lixoftconnectors>
for installation instructions.

## Launching the Shiny app

```r
library(MN2mrg)
run_app()
```

`run_app()` forwards its arguments to `shiny::runApp()`, so
`run_app(port = 8080, launch.browser = FALSE)` works as expected.

## Programmatic use

The translation functions can be called directly, for example to
translate a model in a batch job. The Dosing Simulation and Sensitivity
Analysis tabs are app-only and have no package function.

### Translate a NONMEM model

```r
library(MN2mrg)

res <- nonmem2mrgsolve("path/to/run.mod")   # finds run.ext next to the .mod
writeLines(res$code, "run.cpp")
cat(res$message, sep = "\n")                 # warnings, e.g. PHI() passed through
```

Models that use the M3 method (`PHI()`) are translated with `PHI()` left
in place, so the BQL branch needs a manual edit before the `.cpp`
compiles. `res$message` says when this applies.

### Translate a Monolix project

```r
res <- monolix2mrgsolve(
  project_path = "path/to/run.mlxtran",
  monolix_path = "/path/to/MonolixSuite2024R1"
)
writeLines(res$code, "run.cpp")
```

This needs a MonolixSuite installation (see Installation). Covariate and
regressor values written into `$PARAM` default to 1 (continuous) and 0
(categorical); pass `cont_covariate_values`, `cat_covariate_values` or
`regressor_values` to change them.

### Downloaded bundles

The VPC and forest-plot tabs can download a script that regenerates the
result outside the app. Both scripts call `library(MN2mrg)`, so the package
must be installed on the machine that runs them. The VPC script records
the MN2mrg version it was generated with in a comment next to that call.

## Development

* R (>= 4.4.0).
* `devtools::document()` regenerates `NAMESPACE` and `man/*.Rd` after
  any change under `R/`.
* Quality gate: `devtools::check()` should return zero errors.

## Repository layout

```
MN2mrg/
├── R/                          Exported package functions
├── inst/
│   ├── shiny/app.R              Shiny app source (run_app() launches this)
│   ├── templates/               forest_template.R, copied into the forest bundle
│   └── extdata/
│       ├── monolix_library_models/  Monolix PK library structural models
│       └── sge.tmpl             SGE template, retained but unused (HPC off)
├── man/                        Roxygen-generated .Rd (do not hand-edit)
├── DESCRIPTION                 Package metadata + dependencies
└── NAMESPACE                   Roxygen-generated exports/imports
```

## Authors

* Zengtao Wang, original author
* Miles Samuel, interim maintainer for the R package refactor
* Mike Heathman, NONMEM and VPC contributor
* Eli Lilly and Company, copyright holder

## License

Copyright 2026 Eli Lilly and Company.

Licensed under the Apache License, Version 2.0. You may not use this
file except in compliance with the License. You may obtain a copy of
the License at <http://www.apache.org/licenses/LICENSE-2.0>, or read the
full text in [`LICENSE`](LICENSE). Attribution notices are in
[`NOTICE`](NOTICE).

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied. See the License for the specific language governing
permissions and limitations under the License.
