# MN2mrg

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
  off-app for submission archives)

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

Every button in the Shiny app is a thin wrapper around an exported
package function. To script a translation, embed a VPC in a Quarto
report, or run a large sensitivity sweep from the command line, call
the functions directly:

```r
library(MN2mrg)

# NONMEM: writes an mrgsolve .cpp
res <- nonmem2mrgsolve("path/to/run.mod")
writeLines(res$code, "run.cpp")

# VPC: prep + simulate + render
prepped <- prep_vpc_dataset(
  data       = data.table::fread("obs.csv", na.strings = "."),
  column_map = list(ID = "ID", TIME = "TIME", DV = "DV",
                    AMT = "AMT", EVID = "EVID", CMT = "CMT"),
  mode       = "nonmem",
  opts       = list(nonmem_ignore_conditions =
                      parse_nonmem_ignore(readLines("run.mod"))$conditions)
)
sim <- run_vpc_sim("run.cpp", prepped$data, n_rep = 500L, seed = 42L)
p   <- render_vpc_plot(sim$obs_df, sim$sim_df,
                       build_vpc_plot_spec(list(title = "VPC: run")))
ggplot2::ggsave("vpc.png", p, width = 7, height = 5, dpi = 300)
```

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
│   ├── templates/               Files shipped inside the reproducibility bundle
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
