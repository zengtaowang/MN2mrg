# Backend utilities for the Forest Plot tab.
#
# The tab is a UI-driven wrapper around the same forest.R script (shipped in
# inst/templates/forest_template.R) and utility-functions.R (shipped in
# inst/templates/utility-functions.R) used by the pmx-skill-forest skill.
# The app collects the same interview answers, writes the same forest.yaml
# (+ optional data_spec.yaml), and runs the unmodified forest.R script in
# the background via callr.

#' List non-THETA $PARAM names eligible as forest-plot covariates
#'
#' Return the parameter names that would pass forest.R's own
#' `select(!contains("THETA"))` filter on the model's `$PARAM` block, i.e.
#' every top-level parameter that is not a THETA. These are the values that
#' the forest-plot UI can offer as covariate candidates.
#'
#' @param mod An mrgsolve model object (as returned by
#'   [mrgsolve::mread()]).
#'
#' @return Character vector of parameter names.
#'
#' @importFrom mrgsolve param
#' @importFrom stringr str_replace str_trim str_split
#'
#' @details
#' `importMethodsFrom(mrgsolve, as.list)` is declared below. Nothing in this
#' package calls `as.list()` on an mrgsolve object any more, but the import is
#' what makes the generic reachable here at all: without it, `as.list` inside
#' this namespace is `base::as.list`, which cannot coerce an mrgsolve S4
#' object and fails with `"no method for coercing this S4 class to a vector"`.
#' That is the defect described in [forest_param_names()], and the import
#' keeps it from reappearing the next time someone reaches for `as.list()` on
#' a model object in `R/`.
#'
#' @importMethodsFrom mrgsolve as.list
#' @export
get_forest_covariate_candidates <- function(mod) {
  pnames <- forest_param_names(param(mod))
  pnames[!grepl("^THETA", pnames)]
}

#' Extract parameter names from an mrgsolve parameter object
#'
#' Pull the names out of whatever [mrgsolve::param()] returned.
#'
#' Reads [names()] rather than coercing with `as.list()`, which is the fix for
#' the Forest Plot tab's "Load Model" failure:
#'
#' ```
#' Error loading model: no method for coercing this S4 class to a vector
#' ```
#'
#' [mrgsolve::param()] returns an S4 `parameter_list`, and mrgsolve defines
#' and exports S4 methods for both `as.list` and `names` on it. The two
#' behave differently when called from inside another package's namespace:
#'
#' - `as.list` is a closure, and its internal dispatch covers only basic
#'   types. Reaching its S4 method requires that method to be in scope, and
#'   this package's `NAMESPACE` imported mrgsolve *functions* and no mrgsolve
#'   *methods*. `as.list` therefore resolved to `base::as.list`, which calls
#'   `as.vector()` on the S4 object and raises the message above.
#' - `names` is a primitive, and primitives dispatch S4 methods internally
#'   whatever the calling namespace has imported. It works with no import.
#'
#' `importMethodsFrom(mrgsolve, as.list)` is declared as well now, so the
#' closure case cannot bite the next caller that reaches for it.
#'
#' The failure was never specific to a particular model: it hit every model,
#' because the call ran inside the namespace. It looked model-specific only
#' because reproducing it at the top level of a script, with mrgsolve
#' attached, resolves `as.list` to mrgsolve's S4 generic and succeeds.
#'
#' A `names()` failure is still reported with the class that caused it and the
#' mrgsolve version in play, rather than as a bare coercion message, because
#' the original report could not be diagnosed from what the UI showed.
#'
#' @param pars Object returned by [mrgsolve::param()].
#'
#' @return Character vector of parameter names; `character()` when `pars`
#'   carries no names.
#'
#' @export
forest_param_names <- function(pars) {
  nms <- tryCatch(names(pars), error = function(e) e)
  if (inherits(nms, "error")) {
    stop("Could not read parameter names from an object of class '",
         paste(class(pars), collapse = "/"),
         "' under mrgsolve ",
         as.character(utils::packageVersion("mrgsolve")),
         ". Underlying error: ", conditionMessage(nms),
         call. = FALSE)
  }
  if (is.null(nms)) character() else nms
}

#' Derive the .ext path from a .cov path
#'
#' Mirror forest.R's own naive substring replace of `"cov"` -> `"ext"` in
#' the covariance-file path so the UI can display the derived path to the
#' user before it is actually used.
#'
#' @param cov_path Character. Path to a NONMEM `.cov` file.
#'
#' @return Character. Corresponding `.ext` path.
#'
#' @export
derive_ext_from_cov <- function(cov_path) {
  str_replace(cov_path, "cov", "ext")
}

#' Check that a bootstrap file's THETA columns match a model's
#'
#' `forest.R` itself does not check that a bootstrap file's THETA columns
#' match the model's THETA parameters; unmatched model THETAs are silently
#' left at their `$PARAM` default. Surface any mismatch to the user
#' explicitly.
#'
#' @param boot_csv_path Character. Path to a bootstrap CSV file.
#' @param mod An mrgsolve model object.
#'
#' @return `NULL` when THETAs align, otherwise a character message listing
#'   the missing/extra columns. Returns a string when the bootstrap file
#'   does not exist.
#'
#' @export
validate_bootstrap_thetas <- function(boot_csv_path, mod) {
  if (!file.exists(boot_csv_path)) {
    return("Bootstrap file not found.")
  }
  header <- names(read.csv(boot_csv_path, nrows = 1))
  boot_thetas <- header[grepl("^THETA", header)]
  # forest_param_names() rather than names(as.list(param(mod))): the latter
  # raised "no method for coercing this S4 class to a vector" for every model,
  # and because this is the function that warns about THETA mismatches, that
  # error removed the warning rather than the plot. forest.R then leaves any
  # unmatched model THETA at its $PARAM default, so the forest plot renders
  # normally off silently-defaulted parameters. See forest_param_names().
  model_thetas <- forest_param_names(param(mod))
  model_thetas <- model_thetas[grepl("^THETA", model_thetas)]

  missing_in_boot <- setdiff(model_thetas, boot_thetas)
  extra_in_boot <- setdiff(boot_thetas, model_thetas)

  msgs <- c()
  if (length(missing_in_boot) > 0) {
    msgs <- c(msgs, paste0(
      "Model THETAs not found in bootstrap file (will stay fixed at their default): ",
      paste(missing_in_boot, collapse = ", ")
    ))
  }
  if (length(extra_in_boot) > 0) {
    msgs <- c(msgs, paste0(
      "Bootstrap file has THETA columns not present in the model: ",
      paste(extra_in_boot, collapse = ", ")
    ))
  }
  if (length(msgs) == 0) return(NULL)
  paste(msgs, collapse = "\n")
}

#' Parse a comma-separated string into a numeric vector
#'
#' Split `"1, 2.5, 5"` into `c(1, 2.5, 5)`, dropping empty tokens and
#' silently coercing unparseable tokens to `NA`.
#'
#' @param x Character. Comma-separated numeric values.
#'
#' @return Numeric vector.
#'
#' @export
parse_numeric_values <- function(x) {
  vals <- str_trim(str_split(x, ",")[[1]])
  vals <- vals[vals != ""]
  suppressWarnings(as.numeric(vals))
}

#' Parse a comma-separated string into a character vector of labels
#'
#' Split `"Male, Female"` into `c("Male", "Female")`, dropping empty
#' tokens. Used as the plot-only display labels for a categorical
#' covariate's numeric codes.
#'
#' @param x Character. Comma-separated label list.
#'
#' @return Character vector of labels.
#'
#' @export
parse_label_list <- function(x) {
  labs <- str_trim(str_split(x, ",")[[1]])
  labs[labs != ""]
}

#' Build a dosing-regimen list for the forest.yaml `regimen:` block
#'
#' Assemble the named list that forest.R reads to construct its reference
#' and covariate-perturbation dosing event records.
#'
#' @param type Character. One of `"single"`, `"ss"`, or `"multiple"`.
#' @param amt Numeric. Dose amount.
#' @param cmt Integer. Dosing compartment.
#' @param ii Numeric. Interdose interval.
#' @param addl Integer. Number of additional doses.
#' @param tau Numeric. Simulation duration for reference summaries.
#' @param pred Character. Name of the prediction column in the mrgsolve
#'   `$CAPTURE` block (default `"Y"`).
#'
#' @return Named list suitable for the `regimen:` section of `forest.yaml`.
#'
#' @export
build_forest_regimen <- function(type, amt, cmt = 1, ii = 24, addl = 1, tau = 24, pred = "Y") {
  list(type = type, amt = amt, cmt = cmt, ii = ii, addl = addl, tau = tau, pred = pred)
}

#' Build the endpoints section of a forest.yaml
#'
#' Assemble the named list keyed by endpoint (`auc`, `aucinf`, `cmax`,
#' `cmin`, `cavg`, or `parameter`) with each entry carrying a display
#' `label` and, for the `parameter` endpoint, a `param` variable name.
#'
#' @param selected Character vector. Chosen endpoint keys.
#' @param labels Named list or character vector keyed by endpoint carrying
#'   the display label for each.
#' @param param_var Character or `NULL`. Model output variable used when
#'   `"parameter"` is among `selected`.
#'
#' @return Named list for the `endpoint:` section of `forest.yaml`.
#'
#' @export
build_forest_endpoints <- function(selected, labels, param_var = NULL) {
  ends <- setNames(vector("list", length(selected)), selected)
  for (k in selected) {
    entry <- list(label = labels[[k]])
    if (k == "parameter") entry$param <- param_var
    ends[[k]] <- entry
  }
  ends
}

#' Build the covariates section of a forest.yaml
#'
#' Assemble a named list keyed by covariate name, with each entry carrying
#' the covariate's `reference` value and the `values` at which it should be
#' perturbed in the forest plot.
#'
#' @param cov_names Character vector. Covariate names.
#' @param references Named list keyed by covariate name of reference
#'   values.
#' @param values_list Named list keyed by covariate name of perturbation
#'   value vectors.
#'
#' @return Named list for the `covariates:` section of `forest.yaml`.
#'
#' @export
build_forest_covariates <- function(cov_names, references, values_list) {
  covs <- setNames(vector("list", length(cov_names)), cov_names)
  for (cn in cov_names) {
    covs[[cn]] <- list(reference = references[[cn]], values = values_list[[cn]])
  }
  covs
}

#' Build the uncertainty section of a forest.yaml
#'
#' Assemble the uncertainty descriptor for one of the three supported
#' propagation methods: NONMEM covariance matrix, PsN bootstrap, or a set
#' of NONMEM Bayes `.ext` files.
#'
#' @param method Character. One of `"covariance"`, `"bootstrap"`, or
#'   `"bayes"`.
#' @param covariance_path Character. Path to a NONMEM `.cov` file
#'   (required when `method = "covariance"`).
#' @param bootstrap_path Character. Path to a PsN bootstrap CSV file
#'   (required when `method = "bootstrap"`).
#' @param bayes_paths Character vector. Paths to Bayes `.ext` files
#'   (required when `method = "bayes"`).
#'
#' @return Named list for the `uncertainty:` section of `forest.yaml`.
#'
#' @export
build_forest_uncertainty <- function(method, covariance_path = NULL, bootstrap_path = NULL, bayes_paths = NULL) {
  if (method == "covariance") {
    list(covariance = covariance_path)
  } else if (method == "bootstrap") {
    list(bootstrap = bootstrap_path)
  } else if (method == "bayes") {
    list(bayes = as.list(bayes_paths))
  } else {
    stop("Unknown uncertainty method: ", method)
  }
}

#' Assemble a complete forest.yaml spec list
#'
#' Combine model, data-spec, output, and per-section builders
#' ([build_forest_regimen()], [build_forest_endpoints()],
#' [build_forest_covariates()], [build_forest_uncertainty()]) into a
#' single list that can be serialised to `forest.yaml` via
#' `yaml::write_yaml()`.
#'
#' @param input_model Character. Path to the mrgsolve `.cpp` model.
#' @param data_spec Character. Path to the `data_spec.yaml`.
#' @param output_directory Character. Where forest.R will write plots.
#' @param output_stem Character. Base filename stem for outputs.
#' @param nRep Integer. Number of posterior replicates.
#' @param output_filetype Character. `"png"` or `"pdf"`.
#' @param width,height Numeric. Plot dimensions in inches.
#' @param text_size,shape_size Numeric. Font and shape sizes for
#'   pmforest.
#' @param shaded_interval Numeric length-2. Shaded reference interval on
#'   the ratio axis.
#' @param quarto_output Logical. Whether forest.R should render a Quarto
#'   summary alongside the plot files.
#' @param regimen,endpoint,covariates,uncertainty Named lists from the
#'   corresponding builders.
#'
#' @return Named list ready for `yaml::write_yaml()`.
#'
#' @export
build_forest_yaml_spec <- function(input_model, data_spec, output_directory, output_stem,
                                    nRep, output_filetype = "png", width = 7, height = 7,
                                    text_size = 3.5, shape_size = 2.5,
                                    shaded_interval = c(0.8, 1.25), quarto_output = TRUE,
                                    regimen, endpoint, covariates, uncertainty) {
  spec <- list(
    input_model = input_model,
    data_spec = data_spec,
    output_directory = output_directory,
    output_stem = output_stem,
    nRep = as.integer(nRep),
    output_filetype = output_filetype,
    width = width,
    height = height,
    `text.size` = text_size,
    `shape.size` = shape_size,
    `shaded.interval` = shaded_interval,
    quarto_output = isTRUE(quarto_output),
    regimen = regimen,
    endpoint = endpoint,
    covariates = covariates,
    uncertainty = uncertainty
  )
  spec
}

#' Build a data_spec.yaml from per-covariate metadata
#'
#' Assemble a yspec-compatible data specification from a named list of
#' per-covariate metadata entries.
#'
#' @param cov_meta Named list keyed by covariate name. Each entry is a
#'   list carrying `type` (`"continuous"` or `"categorical"`), `short`
#'   (display label), plus `unit` for continuous or `values` and `decode`
#'   for categorical.
#'
#' @return Named list ready for `yaml::write_yaml()`.
#'
#' @export
build_forest_data_spec <- function(cov_meta) {
  spec <- list()
  for (cn in names(cov_meta)) {
    m <- cov_meta[[cn]]
    if (identical(m$type, "categorical")) {
      spec[[cn]] <- list(short = m$short, values = m$values, decode = m$decode)
    } else {
      spec[[cn]] <- list(short = m$short, unit = m$unit)
    }
  }
  spec
}

#' Copy the vendored forest.R and utility-functions.R into a working directory
#'
#' Populate `outdir` with the standalone forest reproducibility bundle:
#' `forest.R` (a copy of `inst/templates/forest_template.R`),
#' `utility-functions.R` (a copy of `inst/templates/utility-functions.R`),
#' and an empty `.here` marker so that `here::here()` inside `forest.R`
#' resolves to `outdir` rather than climbing to a higher `.git`/`.Rproj`
#' root.
#'
#' @param outdir Character. Directory to populate. Created if it does not
#'   exist.
#'
#' @return `outdir`, returned invisibly.
#'
#' @export
setup_forest_dir <- function(outdir) {
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

  template_path <- system.file("templates", "forest_template.R",
                               package = "MN2mrg")
  if (!nzchar(template_path)) {
    stop("forest_template.R not shipped with this MN2mrg install; ",
         "reinstall the package.")
  }
  file.copy(template_path, file.path(outdir, "forest.R"), overwrite = TRUE)

  utility_template <- system.file("templates", "utility-functions.R",
                                  package = "MN2mrg")
  if (!nzchar(utility_template)) {
    stop("utility-functions.R template not shipped with this MN2mrg ",
         "install; reinstall the package.")
  }
  file.copy(utility_template,
            file.path(outdir, "utility-functions.R"),
            overwrite = TRUE)

  file.create(file.path(outdir, ".here"))
  invisible(outdir)
}

#' Launch forest.R as a background job with logs and forced-exit on error
#'
#' Run the vendored `forest.R` script in `outdir` via [callr::r_bg()] so
#' the Shiny session stays responsive. Stdout and stderr are redirected to
#' `forest_run.log` inside `outdir` for polling by the UI. `r_bg()`'s `wd`
#' argument is used to set the child process's working directory, which
#' avoids a `setwd()` call in package code that R CMD check would flag.
#'
#' `callr::r_bg()` catches errors thrown inside its `func` and only
#' re-raises them if/when the caller invokes `$get_result()`; the
#' background process itself still exits with status 0. The app polls the
#' OS exit status to distinguish "finished but no plots produced" from a
#' real forest.R failure, so we catch the error ourselves and force a
#' non-zero exit for the failure to be detected.
#'
#' @param outdir Character. Directory populated by [setup_forest_dir()].
#'
#' @return A `callr::r_bg` handle (a `process` object) for the background
#'   job.
#'
#' @importFrom callr r_bg
#' @export
run_forest_job <- function(outdir) {
  log_path <- file.path(outdir, "forest_run.log")
  callr::r_bg(
    func = function() {
      ok <- tryCatch({
        source("forest.R")
        TRUE
      }, error = function(e) {
        message("forest.R failed: ", conditionMessage(e))
        FALSE
      })
      if (!isTRUE(ok)) quit(save = "no", status = 1L)
    },
    stdout = log_path,
    stderr = log_path,
    wd = outdir,
    supervise = TRUE
  )
}

#' Zip the forest reproducibility bundle
#'
#' Bundle the reproducibility artifacts (`forest.R`, `utility-functions.R`,
#' `forest.yaml`, `.here`, and `data_spec.yaml` if present) into a single
#' zip so the user can rerun the plot locally. Uses
#' [withr::with_dir()] to run `utils::zip()` in `outdir` without a
#' persistent `setwd()`.
#'
#' @param outdir Character. Directory that holds the artifacts.
#' @param zip_path Character. Target path for the zip file. Non-existent
#'   parents must exist; the path is normalised (without requiring the
#'   file already exist).
#'
#' @return `zip_path`, invisibly.
#'
#' @importFrom withr with_dir
#' @export
zip_forest_bundle <- function(outdir, zip_path) {
  zip_path <- normalizePath(zip_path, mustWork = FALSE)
  files <- c("forest.R", "utility-functions.R", "forest.yaml", ".here", "data_spec.yaml")
  files <- files[file.exists(file.path(outdir, files))]

  withr::with_dir(outdir, {
    utils::zip(zipfile = zip_path, files = files)
  })
  zip_path
}
