# R/vpc_utils_common.R
# Software-agnostic backend utilities for the unified VPC tab (Monolix + NONMEM).
# Monolix-specific prep lives in R/vpc_utils_monolix.R; NONMEM-specific
# $DATA IGNORE= parsing lives in R/vpc_utils_nonmem.R. This file has no
# dependency on either.
#
# Functions exported for use in inst/shiny/app.R:
#   build_dataspec(column_map, metadata)
#   build_vpc_spec(params)
#   build_vpc_plot_spec(plot_params)
#   prep_vpc_dataset(data, column_map, mode, opts)
#   run_vpc_sim(mod_path, data, n_rep, n_jobs, carry_list)
#   render_vpc_plot(obs, sim, vpc_spec, dataspec, cat_covariate_all_categories)

#' Return the first argument if not NULL, otherwise the fallback
#'
#' Internal null-coalescing operator used throughout the VPC pipeline for
#' `params$x %||% default` idioms. Base R gained `%||%` in 4.4.0 with
#' identical semantics; the local copy is retained for explicitness.
#'
#' @param a First value.
#' @param b Fallback value returned when `a` is `NULL`.
#'
#' @return `a` when it is not `NULL`; `b` otherwise.
#'
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Drop empty-data VPC ggplot layers
#'
#' Walk the layers of a `vpc::vpc()` ggplot output and drop rows whose primary
#' aesthetic (`ymin` or `y`) is `NA`. Used to clean up the legend-only sentinel
#' layers that would otherwise leave empty visual artifacts in the rendered
#' plot.
#'
#' @param p A ggplot object, typically the return of [vpc::vpc()].
#'
#' @return `p` with per-layer data frames pruned of all-NA rows.
#'
#' @importFrom rlang as_name sym .data
#' @importFrom ggplot2 geom_line geom_ribbon aes scale_color_manual
#'   scale_fill_manual guide_legend theme scale_y_log10 scale_y_continuous
#'   scale_x_continuous coord_cartesian label_value
#' @importFrom dplyr rename if_else arrange ungroup row_number distinct pull
#'   coalesce bind_rows any_of
#' @importFrom stats setNames median quantile
#' @importFrom vpc vpc vpc_cens new_vpc_theme
#' @importFrom mrgsolve loadso mrgsim
#'
#' @export
strip_na_layers <- function(p) {
  for (k in seq_along(p$layers)) {
    d <- p$layers[[k]]$data
    if (!is.data.frame(d) || nrow(d) == 0) next
    aes_map  <- p$layers[[k]]$mapping
    get_col  <- function(aes_sym) {
      if (is.null(aes_sym)) return(NULL)
      col <- tryCatch(rlang::as_name(aes_sym), error = function(e) NULL)
      if (!is.null(col) && col %in% names(d)) col else NULL
    }
    check_col <- get_col(aes_map[["ymin"]]) %||% get_col(aes_map[["y"]])
    if (!is.null(check_col)) {
      new_d <- d[!is.na(d[[check_col]]), , drop = FALSE]
      if (is.data.frame(new_d)) p$layers[[k]]$data <- new_d
    }
  }
  p
}

#' Attach the VPC observation/simulation legend to a ggplot
#'
#' Overlay four sentinel geoms (`NA`-data lines and ribbons) so that
#' ggplot's scale-manual guides render a compact "Obs. pct / Obs. median /
#' Sim. PI / Sim. median" legend at the bottom of a `vpc::vpc()` plot.
#'
#' @param p A ggplot object.
#' @param vpc_theme Named list from [vpc::new_vpc_theme()] with colours.
#' @param pi Numeric length-2 vector. Percentile interval used to label the
#'   observation quantile lines.
#'
#' @return `p` with the sentinel legend layers and manual scales attached.
#'
#' @export
add_vpc_legend <- function(p, vpc_theme, pi = c(0.05, 0.95)) {
  obs_pi_col   <- vpc_theme$obs_ci_color     %||% "#e41a1c"
  obs_med_col  <- vpc_theme$obs_median_color %||% "#377eb8"
  sim_pi_fill  <- vpc_theme$sim_pi_fill      %||% "steelblue3"
  sim_med_fill <- vpc_theme$sim_median_fill  %||% "grey60"

  pi_label <- paste0(round(pi[1] * 100, 1), "th/", round(pi[2] * 100, 1), "th")

  d_line <- data.frame(x = NA_real_, y = NA_real_)
  d_rib  <- data.frame(x = NA_real_, ymin = NA_real_, ymax = NA_real_)

  p +
    ggplot2::geom_line(data = d_line, ggplot2::aes(x = x, y = y, color = "obs_pi"),
                       linetype = "dashed", na.rm = TRUE) +
    ggplot2::geom_line(data = d_line, ggplot2::aes(x = x, y = y, color = "obs_med"),
                       linetype = "solid", na.rm = TRUE) +
    ggplot2::geom_ribbon(data = d_rib, ggplot2::aes(x = x, ymin = ymin, ymax = ymax, fill = "sim_pi"),
                         na.rm = TRUE) +
    ggplot2::geom_ribbon(data = d_rib, ggplot2::aes(x = x, ymin = ymin, ymax = ymax, fill = "sim_med"),
                         na.rm = TRUE) +
    ggplot2::scale_color_manual(
      name   = NULL,
      breaks = c("obs_pi", "obs_med"),
      values = c(obs_pi = obs_pi_col, obs_med = obs_med_col),
      labels = c(obs_pi = paste0("Obs. ", pi_label, " pct"), obs_med = "Obs. median"),
      guide  = ggplot2::guide_legend(
        override.aes = list(linetype = c("dashed", "solid"), fill = NA, linewidth = 0.8)
      )
    ) +
    ggplot2::scale_fill_manual(
      name   = NULL,
      values = c(sim_pi = sim_pi_fill, sim_med = sim_med_fill),
      labels = c(sim_pi = paste0("Sim. ", pi_label, " PI"), sim_med = "Sim. median"),
      guide  = ggplot2::guide_legend(
        override.aes = list(alpha = 0.6, color = NA, linetype = 0)
      )
    ) +
    ggplot2::theme(legend.position = "bottom")
}


#' Build a minimal yspec-like column label specification
#'
#' Assemble a lightweight column-label object from a mapping of standard
#' column names to dataset column names, plus optional per-column
#' `unit`/`label` metadata. Consumed by [render_vpc_plot()] to build axis
#' labels ("Time (hr)" etc.).
#'
#' @param column_map Named list mapping standard names to dataset column
#'   names, e.g. `list(TIME = "TAFD", DV = "CONC")`.
#' @param metadata Named list of optional per-column metadata. Recognised
#'   keys are `<STANDARD>_unit` (unit string) and `<STANDARD>_label` (display
#'   label). Missing units default to `""`; missing labels default to the
#'   dataset column name.
#'
#' @return A named list keyed by standard name. Each entry is a list with
#'   `col` (dataset column), `short` (display label), and `unit` (unit
#'   string).
#'
#' @export
build_dataspec <- function(column_map, metadata = list()) {
  dataspec <- list()
  for (std_name in names(column_map)) {
    src_col <- column_map[[std_name]]
    unit    <- metadata[[paste0(std_name, "_unit")]]  %||% ""
    label   <- metadata[[paste0(std_name, "_label")]] %||% src_col
    dataspec[[std_name]] <- list(col = src_col, short = label, unit = unit)
  }
  dataspec
}

#' Format an axis label from a single dataspec entry
#'
#' Render `"Label (unit)"` when the entry carries a unit, otherwise just the
#' label; return `fallback` when the entry is `NULL`.
#'
#' @param entry Single entry from a dataspec (see [build_dataspec()]).
#' @param fallback Character. Value returned when `entry` is `NULL`.
#'
#' @return Character. Formatted axis label.
#'
#' @export
dataspec_label <- function(entry, fallback = "") {
  if (is.null(entry)) return(fallback)
  unit  <- trimws(entry$unit  %||% "")
  short <- entry$short %||% fallback
  if (nchar(unit) > 0) paste0(short, " (", unit, ")") else short
}


#' Assemble a `vpc.yaml`-compatible spec list
#'
#' Build a named list matching the top-level `vpc.yaml` schema so it can be
#' serialised with [yaml::write_yaml()] for regulatory reproducibility. All
#' fields are optional and fall back to defaults appropriate for a typical
#' VPC.
#'
#' @param params Named list. Recognised keys mirror the on-disk YAML schema:
#'   `input_filename`, `data_spec`, `input_model`, `output_directory`,
#'   `output_stem`, `nRep`, `output_filetype`, `width`, `height`,
#'   `axis.text`, `axis.title`, `strip.text`, `pi_fill`, `med_fill`,
#'   `quarto_output`, `filters`, and `plots`.
#'
#' @return Named list with all schema fields populated.
#'
#' @export
build_vpc_spec <- function(params = list()) {
  list(
    kind             = "vpc_plot",
    input_filename   = params$input_filename   %||% "",
    data_spec        = params$data_spec        %||% "",
    input_model      = params$input_model      %||% "",
    output_directory = params$output_directory %||% "",
    output_stem      = params$output_stem      %||% "vpc",
    nRep             = as.integer(params$nRep  %||% 500L),
    output_filetype  = params$output_filetype  %||% "png",
    width            = params$width            %||% 7,
    height           = params$height           %||% 5,
    `axis.text`      = params[["axis.text"]]   %||% 11,
    `axis.title`     = params[["axis.title"]]  %||% 11,
    `strip.text`     = params[["strip.text"]]  %||% 11,
    pi_fill          = params$pi_fill          %||% "steelblue3",
    med_fill         = params$med_fill         %||% "grey60",
    quarto_output    = isTRUE(params$quarto_output),
    filters          = params$filters          %||% list(),
    plots            = params$plots            %||% list()
  )
}

#' Build a single plot entry for `vpc_spec$plots`
#'
#' Assemble a named list matching the per-plot section of the `vpc.yaml`
#' schema. Consumed by [build_vpc_spec()] via its `plots` argument, and by
#' [render_vpc_plot()] to drive stratification, log-axis, censoring, and
#' cosmetic choices for one panel.
#'
#' @param plot_params Named list. Recognised keys: `title`, `time_column`,
#'   `sim_column`, `cmt`, `lloq`, `stratify` (named list of
#'   `col_name -> list(type, method, breaks)` where `type` is
#'   `"categorical"` or `"continuous"`, `method` is `"median"`, `"quartile"`,
#'   or `"customized"`, and `breaks` is a numeric vector for the customized
#'   case), `scales`, `logY`, `predCorr`, `pi`, `ci`, `bins`, `custom_bins`,
#'   `obsdv`, `pici`, `censor`, `smooth`, `x_label`, `y_label`, `xlim`,
#'   `ylim`, `x_breaks`, `y_breaks`, `pi_fill`, `med_fill`, `axis_text_size`,
#'   `axis_title_size`, `strip_text_size`, `show_legend`.
#'
#' @return Named list with all per-plot schema fields populated.
#'
#' @export
build_vpc_plot_spec <- function(plot_params = list()) {
  list(
    title       = plot_params$title       %||% "",
    time_column = plot_params$time_column %||% "TIME",
    sim_column  = plot_params$sim_column  %||% "Y",
    cmt         = plot_params$cmt         %||% 0,
    lloq        = plot_params$lloq,
    stratify    = plot_params$stratify    %||% list(),
    scales      = plot_params$scales      %||% "free",
    logY        = isTRUE(plot_params$logY),
    predCorr    = isTRUE(plot_params$predCorr),
    pi          = plot_params$pi          %||% c(0.05, 0.95),
    ci          = plot_params$ci          %||% c(0.025, 0.975),
    bins        = plot_params$bins        %||% "data",
    custom_bins = plot_params$custom_bins %||% "",
    obsdv       = isTRUE(plot_params$obsdv  %||% TRUE),
    pici        = isTRUE(plot_params$pici   %||% TRUE),
    censor      = isTRUE(plot_params$censor),
    smooth      = isTRUE(plot_params$smooth %||% TRUE),
    x_label     = plot_params$x_label     %||% NULL,
    y_label     = plot_params$y_label     %||% NULL,
    xlim        = plot_params$xlim        %||% NULL,
    ylim        = plot_params$ylim        %||% NULL,
    x_breaks    = plot_params$x_breaks    %||% NULL,
    y_breaks    = plot_params$y_breaks    %||% NULL,
    pi_fill     = plot_params$pi_fill     %||% "steelblue3",
    med_fill    = plot_params$med_fill    %||% "grey60",
    axis_text_size  = plot_params$axis_text_size  %||% 11,
    axis_title_size = plot_params$axis_title_size %||% 11,
    strip_text_size = plot_params$strip_text_size %||% 11,
    show_legend     = isTRUE(plot_params$show_legend %||% TRUE)
  )
}


#' Prepare a raw dataset for VPC simulation
#'
#' Clean and standardise a raw modelling dataset (Monolix or NONMEM) so that
#' it can be handed straight to [run_vpc_sim()]. The Monolix path delegates
#' to [prepare_monolix_dataset()] with `filter_mdv = TRUE`, then attaches a
#' `ROW` index. The NONMEM path applies `$DATA IGNORE=(cond)` filters
#' (before renaming), performs `column_map` renaming, coerces numeric
#' columns, derives EVID/CMT/DVID when absent, applies MDV filtering
#' respecting BQL preservation, drops user-specified ignore columns, drops
#' non-CMT character columns (mrgsolve requires numeric data), sorts by
#' `ID`/`TIME`, and attaches `ROW`.
#'
#' @param data Data frame. Raw dataset already loaded from disk.
#' @param column_map Named list. Standard-name-to-source-column mapping;
#'   ignored in Monolix mode where renaming is driven by
#'   `opts$data_info$headerTypes`.
#' @param mode Character. One of `"monolix"` or `"nonmem"`; selects the
#'   preparation pipeline.
#' @param opts Named list passed through to [prepare_monolix_dataset()] in
#'   Monolix mode; in NONMEM mode recognised fields include
#'   `nonmem_ignore_conditions`, `ignore_columns`, and (via `column_map`)
#'   the target column names.
#'
#' @return List with `data` (prepared data frame with a `ROW` index) and
#'   `cat_covariate_all_categories` (named list used to decode numeric
#'   categoricals in downstream plotting; empty in NONMEM mode).
#'
#' @export
prep_vpc_dataset <- function(data, column_map, mode = "monolix", opts = list()) {

  if (mode != "monolix") {
    # Non-Monolix path (e.g. NONMEM): apply column_map renaming + basic prep.
    rename_col <- function(df, src, tgt) {
      if (is.null(src) || src == "") return(df)
      if (src %in% names(df)) {
        if (src != tgt) {
          if (tgt %in% names(df)) df <- dplyr::select(df, -dplyr::all_of(tgt))
          df <- dplyr::rename(df, !!tgt := !!rlang::sym(src))
        }
      } else {
        idx <- which(toupper(names(df)) == toupper(src))
        if (length(idx) == 1) {
          nm <- names(df)[idx]
          if (tgt %in% names(df) && nm != tgt) df <- dplyr::select(df, -dplyr::all_of(tgt))
          df <- dplyr::rename(df, !!tgt := !!rlang::sym(nm))
        }
      }
      df
    }

    # Apply NONMEM $DATA IGNORE conditions BEFORE column renaming so they
    # always reference original NONMEM column names (e.g. CMT, STUDY, AMT).
    # If applied after renaming, conditions like !(CMT > 2) silently skip
    # when the user has mapped CMT → DVID.
    if (!is.null(opts$nonmem_ignore_conditions) && length(opts$nonmem_ignore_conditions) > 0) {
      for (expr_str in opts$nonmem_ignore_conditions) {
        parsed <- tryCatch(parse(text = expr_str), error = function(e) NULL)
        if (is.null(parsed)) next
        used_cols <- tryCatch(all.vars(parsed), error = function(e) character(0))
        if (!all(used_cols %in% names(data))) next
        data <- tryCatch(dplyr::filter(data, !!parsed[[1]]), error = function(e) data)
      }
    }

    for (std in names(column_map))
      data <- rename_col(data, column_map[[std]], std)

    for (col in c("TIME", "DV", "AMT", "EVID", "RATE", "II", "ADDL")) {
      if (col %in% names(data) && !is.numeric(data[[col]]))
        data <- dplyr::mutate(data, !!rlang::sym(col) := as.numeric(.data[[col]]))
    }
    if ("ID" %in% names(data) && !is.numeric(data[["ID"]]))
      data <- dplyr::mutate(data, ID = as.numeric(as.factor(ID)))

    if (!"EVID" %in% names(data))
      data <- dplyr::mutate(data, EVID = dplyr::if_else(is.na(AMT), 0, 1))
    if (!"CMT" %in% names(data))
      data <- dplyr::mutate(data, CMT = dplyr::if_else(EVID == 1, 1L, 0L))

    mdv_col <- names(data)[toupper(names(data)) == "MDV"]
    if (length(mdv_col) > 0) {
      if ("BQL" %in% names(data)) {
        data <- dplyr::filter(data, !(.data[[mdv_col]] == 1 & EVID == 0 & BQL != 1))
      } else {
        data <- dplyr::filter(data, !(.data[[mdv_col]] == 1 & EVID == 0))
      }
    }

    # DVID mirrors CMT — no separate derivation needed for NONMEM
    data <- dplyr::mutate(data, DVID = as.integer(CMT))

    # Drop user-specified ignore columns
    if (!is.null(opts$ignore_columns) && length(opts$ignore_columns) > 0) {
      drop_cols <- intersect(opts$ignore_columns, names(data))
      if (length(drop_cols) > 0)
        data <- dplyr::select(data, -dplyr::all_of(drop_cols))
    }

    # Drop character columns — mrgsolve data_set() requires all columns to be numeric
    char_cols <- setdiff(names(data)[vapply(data, is.character, logical(1))], "CMT")
    if (length(char_cols) > 0)
      data <- dplyr::select(data, -dplyr::all_of(char_cols))

    data <- data %>%
      dplyr::arrange(ID, TIME) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ROW = dplyr::row_number())
    return(list(data = data, cat_covariate_all_categories = list()))
  }

  # Monolix path: delegate to unified helper, then add ROW index.
  opts$filter_mdv <- TRUE
  tad_src <- column_map[["TAD"]]
  if (!is.null(tad_src) && nchar(tad_src) > 0) opts$tad_col <- tad_src
  result          <- prepare_monolix_dataset(data, opts)
  # Rename TAD column if user mapped a non-standard name (prepare_monolix_dataset
  # only renames known Monolix header types; TAD must be handled separately).
  if (!is.null(tad_src) && nchar(tad_src) > 0 && tad_src != "TAD" &&
      tad_src %in% names(result$data)) {
    result$data <- dplyr::rename(result$data, TAD = !!rlang::sym(tad_src))
  }
  result$data     <- result$data %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ROW = dplyr::row_number())
  result
}

#' Simulate one VPC replicate (clustermq-serialisable worker)
#'
#' Compile the mrgsolve model at `mod_path`, seed the RNG deterministically
#' from `seed + i`, and simulate one replicate against `dat`. Defined at
#' top level so that `clustermq::Q()` can serialise it cleanly across worker
#' processes; compiled mrgsolve model objects cannot be serialised, so each
#' worker `mread()`s its own copy from the shared model file.
#'
#' Used only by [run_vpc_sim()]'s `parallel_mode = "hpc"` path, which is
#' retained but switched off for this release. The default `"local"` path
#' does not call this function: it reuses one already-`mread()`ed model
#' rather than re-reading it per replicate.
#'
#' @param i Integer. Replicate index (also used as an RNG offset from
#'   `seed`).
#' @param mod_path Character. Path to the mrgsolve `.cpp` model file.
#' @param dat Data frame. Prepared dataset (typically the `data` element of
#'   [prep_vpc_dataset()]'s return).
#' @param seed Integer or `NULL`. RNG seed; when non-`NULL`, replicate `i`
#'   uses `seed + i`.
#'
#' @return Data frame. mrgsolve simulation output with an added `rep`
#'   column set to `i`.
#'
#' @export
vpc_worker_fn <- function(i, mod_path, dat, seed = NULL) {
  if (!is.null(seed)) set.seed(seed + i)
  mod <- mrgsolve::mread(mod_path, quiet = TRUE)
  mrgsolve::loadso(mod)
  mod %>%
    mrgsolve::data_set(dat) %>%
    mrgsolve::carry_out(ROW) %>%
    mrgsolve::mrgsim(atol = 1e-12, maxsteps = 50000) %>%
    dplyr::mutate(rep = i)
}


#' Default SGE worker-log directory for a VPC run
#'
#' Return the directory [run_vpc_sim()] writes SGE worker logs into when it
#' is not given an explicit `log_dir`: a `vpc_sge_logs/` directory beside the
#' mrgsolve model file.
#'
#' The model's own directory is used deliberately in preference to
#' [tempdir()]. Every SGE worker has to `mread()` `mod_path`, so that
#' directory is cluster-visible by construction, whereas `/tmp` is normally
#' node-local: logs written there land on the execution host and the
#' submitting user can never read them.
#'
#' @param mod_path Character. Path to the mrgsolve `.cpp` model file.
#'
#' @return Character. Path to the worker-log directory. The directory is not
#'   created; [run_vpc_sim()] creates it when it runs in `"hpc"` mode.
#'
#' @export
vpc_sge_log_dir <- function(mod_path) {
  # nzchar(NA_character_) is TRUE, so NA has to be rejected separately or an
  # NA model path would silently produce "NA/vpc_sge_logs".
  if (!is.character(mod_path) || length(mod_path) != 1L ||
      is.na(mod_path) || !nzchar(mod_path)) {
    stop("`mod_path` must be a single non-empty character path.", call. = FALSE)
  }
  file.path(dirname(mod_path), "vpc_sge_logs")
}


#' Run VPC simulations on this host, or dispatch them to SGE
#'
#' Simulate `n_rep` replicates of the mrgsolve model at `mod_path` against
#' `data`. Also computes population predictions with `zero_re()` and merges
#' them back into both the simulation and observation frames.
#'
#' `parallel_mode = "local"` (the default) runs every replicate in-process on
#' the host, sequentially with `purrr::map()`, reusing a single
#' locally-`mread()`ed model: no re-compilation, no parallelism, no job
#' submission of any kind, and `n_jobs` is ignored. It makes no assumption
#' that a scheduler exists.
#'
#' `parallel_mode = "hpc"` dispatches the replicates via [clustermq::Q()] as
#' an SGE array job, each worker `mread()`ing its own copy of the model
#' (compiled mrgsolve model objects cannot be serialised, DLL handles do not
#' survive the trip). This is retained infrastructure rather than a supported
#' path for this release: the Shiny app greys the option out, `clustermq` is
#' only a `Suggests`, and the caller is responsible for a reachable SGE
#' installation and a `clustermq.template` that matches it. A missing
#' `clustermq` or an unusable `n_jobs` is an explicit error here, never a
#' silent fall back to running locally.
#'
#' @param mod_path Character. Path to the mrgsolve `.cpp` model file.
#' @param data Data frame. Prepared dataset (from [prep_vpc_dataset()]).
#' @param n_rep Integer. Number of simulation replicates.
#' @param n_jobs Integer or `NULL`. Number of parallel SGE jobs, capped at
#'   `n_rep`. Required in `"hpc"` mode; unused in `"local"` mode, where the
#'   `NULL` default is the honest value.
#' @param carry_list Character vector. Columns to merge back into the
#'   simulation output after the run; typically `c("DVID", "ROW")` plus any
#'   stratification variables.
#' @param parallel_mode Character. `"local"` (default) to run in-process on
#'   this host with no job submission, or `"hpc"` to dispatch to SGE.
#' @param seed Integer or `NULL`. RNG seed for reproducibility. Replicate `i`
#'   uses `seed + i` in both modes, so the two give identical draws.
#' @param log_dir Character or `NULL`. Directory to write per-task SGE worker
#'   logs into (HPC mode only; ignored in local mode). `NULL` uses
#'   [vpc_sge_log_dir()]. Must be writable and visible from the compute
#'   nodes. The directory is created if it does not exist; failure to create
#'   it is an error, never a silent fall back to discarding logs.
#'
#' @return List with `sim_df` (simulation data frame with `PRED` merged),
#'   `obs_df` (observation data frame with `PRED` merged), and `log_dir`
#'   (the resolved SGE worker-log directory, `NULL` in local mode).
#'
#' @export
run_vpc_sim <- function(mod_path, data, n_rep, n_jobs = NULL,
                        carry_list    = c("DVID", "ROW"),
                        parallel_mode = c("local", "hpc"),
                        seed          = NULL,
                        log_dir       = NULL) {
  parallel_mode <- match.arg(parallel_mode)

  mod <- mrgsolve::mread(mod_path, quiet = TRUE)

  # Population predictions (zero random effects)
  pop_pred <- mod %>%
    mrgsolve::zero_re() %>%
    mrgsolve::data_set(data) %>%
    mrgsolve::carry_out(ROW) %>%
    mrgsolve::mrgsim_df() %>%
    dplyr::mutate(PRED = Y)

  if (parallel_mode == "hpc") {
    # clustermq is a Suggests, not an Imports: the default local path never
    # touches it, so installing this package must not drag it in. Name the
    # missing package here rather than letting the clustermq:: call below
    # fail with a bare "there is no package called" from inside a pool setup.
    if (!requireNamespace("clustermq", quietly = TRUE)) {
      stop("`parallel_mode = \"hpc\"` requires the clustermq package, which ",
           "is a Suggests of MN2mrg and is not installed. Install it, ",
           "or use the default `parallel_mode = \"local\"` to run the ",
           "replicates on this host with no job submission.", call. = FALSE)
    }

    # `n_jobs` defaults to NULL because local mode genuinely does not use it.
    # min(NULL, n_rep) is n_rep, so an unset n_jobs reaching here would
    # silently submit one SGE job per replicate. Reject it instead.
    if (!is.numeric(n_jobs) || length(n_jobs) != 1L || !is.finite(n_jobs) ||
        n_jobs < 1) {
      stop("`parallel_mode = \"hpc\"` requires `n_jobs` as a single finite ",
           "number >= 1; got ", paste(deparse(n_jobs), collapse = " "), ".",
           call. = FALSE)
    }
    n_jobs <- min(as.integer(n_jobs), n_rep)

    # inst/extdata/sge.tmpl defaults `{{ log_file }}` to /dev/null, so
    # without an explicit path every compute-node-side failure (a module
    # mismatch, a missing library, an mrgsolve compile error on the worker)
    # is discarded and the caller sees only clustermq's own summary. Give
    # SGE a real path instead. $TASK_ID is expanded by the scheduler, so the
    # array job writes one log per task rather than interleaving all of them
    # into a single file.
    if (is.null(log_dir)) log_dir <- vpc_sge_log_dir(mod_path)
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(log_dir)) {
      stop("Cannot create the SGE worker-log directory: ", log_dir,
           ". Pass `log_dir` pointing at a writable, cluster-visible path.",
           call. = FALSE)
    }

    sim_list <- clustermq::Q(
      fun      = vpc_worker_fn,
      i        = seq_len(n_rep),
      n_jobs   = n_jobs,
      const    = list(mod_path = mod_path, dat = data, seed = seed),
      template = list(log_file = file.path(log_dir, "vpc_worker_$TASK_ID.log"))
    )
  } else {
    log_dir <- NULL
    # Reuse the `mod` object already mread() above — no need to re-read/
    # re-compile the model on every replicate the way vpc_worker_fn does
    # for clustermq workers (which run in separate processes and must
    # mread() their own copy).
    sim_list <- purrr::map(
      seq_len(n_rep),
      function(i) {
        if (!is.null(seed)) set.seed(seed + i)
        mod %>%
          mrgsolve::data_set(data) %>%
          mrgsolve::carry_out(ROW) %>%
          mrgsolve::mrgsim(atol = 1e-12, maxsteps = 50000) %>%
          dplyr::mutate(rep = i)
      }
    )
  }

  sim_df <- dplyr::bind_rows(sim_list)

  # Merge carry columns (stratification vars, DVID) from original dataset
  carry_cols <- intersect(carry_list, names(data))
  sim_df <- sim_df %>%
    dplyr::left_join(dplyr::select(data, ROW, dplyr::all_of(carry_cols)), by = "ROW")

  # Merge PRED into both simulation output and observation dataset
  pred_df <- dplyr::select(pop_pred, ROW, PRED)
  sim_df  <- dplyr::left_join(sim_df, pred_df, by = "ROW")
  obs_df  <- dplyr::left_join(data,   pred_df, by = "ROW")

  list(sim_df = sim_df, obs_df = obs_df, log_dir = log_dir)
}


#' Render a VPC ggplot from prepared observation and simulation data
#'
#' Produce a single VPC ggplot from pre-filtered observation and simulation
#' frames. Handles continuous- and categorical-covariate stratification
#' (with median/quartile/customised binning), decodes numeric categoricals
#' back to their original labels for strip text, drops empty layers,
#' applies axis breaks and limits, and attaches the standard
#' obs/sim/median/PI legend. When `vpc_spec$censor` is `TRUE` and `lloq`
#' is supplied, also renders and vertically stacks a `vpc::vpc_cens()`
#' below-LLOQ probability panel via `cowplot::plot_grid()`. Does not save
#' to disk.
#'
#' @param obs Data frame. Observation dataset with `DV`, `TIME`, `PRED`,
#'   already filtered to the target DVID.
#' @param sim Data frame. Simulation dataset with `DV`, `TIME`, `PRED`,
#'   `rep`, already filtered to the target DVID.
#' @param vpc_spec Named list. Single plot spec from
#'   [build_vpc_plot_spec()]; also accepts `x_label`/`y_label` overrides
#'   and `xlim`/`ylim` axis-limit pairs.
#' @param dataspec Named list from [build_dataspec()], or `NULL` to fall
#'   back to `vpc_spec$x_label`/`y_label` and hardcoded defaults.
#' @param cat_covariate_all_categories Named list for decoding numeric
#'   categoricals back to their original labels in strip text; `NULL` to
#'   skip.
#'
#' @return A ggplot object, or a cowplot vertical grid when
#'   `vpc_spec$censor` is `TRUE` and cowplot is installed.
#'
#' @export
render_vpc_plot <- function(obs, sim, vpc_spec,
                            dataspec = NULL,
                            cat_covariate_all_categories = NULL) {

  time_col   <- vpc_spec$time_column %||% "TIME"
  sim_col    <- vpc_spec$sim_column  %||% "Y"
  logY       <- isTRUE(vpc_spec$logY)
  pred_corr  <- isTRUE(vpc_spec$predCorr)
  lloq_val   <- vpc_spec$lloq
  censor     <- isTRUE(vpc_spec$censor)
  smooth     <- isTRUE(vpc_spec$smooth %||% TRUE)
  show_legend <- isTRUE(vpc_spec$show_legend %||% TRUE)
  pi         <- vpc_spec$pi  %||% c(0.05, 0.95)
  ci         <- vpc_spec$ci  %||% c(0.025, 0.975)
  xlim_vals  <- vpc_spec$xlim
  ylim_vals  <- vpc_spec$ylim
  x_breaks   <- if (!is.null(vpc_spec$x_breaks)) sort(as.numeric(unlist(vpc_spec$x_breaks))) else NULL
  y_breaks   <- if (!is.null(vpc_spec$y_breaks)) sort(as.numeric(unlist(vpc_spec$y_breaks))) else NULL
  sz_text    <- vpc_spec$axis_text_size  %||% 11
  sz_title   <- vpc_spec$axis_title_size %||% 11
  sz_strip   <- vpc_spec$strip_text_size %||% 11
  pi_fill    <- vpc_spec$pi_fill  %||% "steelblue3"
  med_fill   <- vpc_spec$med_fill %||% "grey60"

  # Axis labels: dataspec > vpc_spec overrides > hardcoded fallback
  x_label <- vpc_spec$x_label %||% dataspec_label(dataspec[["TIME"]], "Time")
  y_label <- vpc_spec$y_label %||% dataspec_label(dataspec[["DV"]],   "Concentration")
  if (pred_corr) y_label <- paste0("Prediction-Corrected ", y_label)

  # Resolve bins: parse "customized" into a numeric vector
  bins <- vpc_spec$bins %||% "data"
  if (identical(bins, "customized")) {
    custom_str <- vpc_spec$custom_bins %||% ""
    parsed     <- suppressWarnings(
      as.numeric(unlist(strsplit(gsub(" ", "", custom_str), ",")))
    )
    parsed <- parsed[!is.na(parsed)]
    bins <- if (length(parsed) >= 2) parsed else "data"
  }

  vpc_theme <- vpc::new_vpc_theme(list(
    sim_pi_fill     = pi_fill,
    sim_median_fill = med_fill,
    loq_color       = "#999999"
  ))

  # -- Stratification processing --
  stratify_cfg  <- vpc_spec$stratify %||% list()
  stratify_vars <- names(stratify_cfg)
  stratify_final <- c()

  for (var_name in stratify_vars) {
    cfg      <- stratify_cfg[[var_name]] %||% list()
    cov_type <- cfg$type %||% "categorical"

    if (cov_type == "continuous" &&
        var_name %in% names(obs) &&
        is.numeric(obs[[var_name]])) {

      binned_col <- paste0(var_name, "_f")
      x_ref  <- obs %>% dplyr::distinct(ID, .keep_all = TRUE) %>% dplyr::pull(!!rlang::sym(var_name))
      min_val <- floor(min(x_ref, na.rm = TRUE))
      max_val <- ceiling(max(x_ref, na.rm = TRUE))

      bin_method <- cfg$method %||% "median"
      breaks <- switch(bin_method,
        median    = c(min_val, stats::median(x_ref, na.rm = TRUE), max_val),
        quartile  = c(min_val, stats::quantile(x_ref, probs = c(0.25, 0.5, 0.75), na.rm = TRUE), max_val),
        customized = {
          cb <- cfg$breaks %||% numeric(0)
          if (length(cb) > 0) c(min_val, sort(cb), max_val)
          else c(min_val, stats::median(x_ref, na.rm = TRUE), max_val)
        },
        c(min_val, stats::median(x_ref, na.rm = TRUE), max_val)
      )

      obs[[binned_col]] <- cut(obs[[var_name]], breaks = breaks, include.lowest = TRUE)
      sim[[binned_col]] <- cut(sim[[var_name]], breaks = breaks, include.lowest = TRUE)
      stratify_final <- c(stratify_final, binned_col)

    } else {
      # Decode numeric categoricals back to original labels for strip text
      if (!is.null(cat_covariate_all_categories) &&
          var_name %in% names(cat_covariate_all_categories)) {
        all_cats <- cat_covariate_all_categories[[var_name]]
        decode   <- function(x) {
          idx <- as.integer(x) + 1L
          ifelse(is.na(idx) | idx < 1L | idx > length(all_cats), as.character(x), all_cats[idx])
        }
        obs[[var_name]] <- decode(obs[[var_name]])
        sim[[var_name]] <- decode(sim[[var_name]])
      }
      stratify_final <- c(stratify_final, var_name)
    }
  }

  if (length(stratify_final) == 0) stratify_final <- NULL

  # vpc's own guess_software()/filter_dv() would otherwise re-drop MDV!=0 rows
  # (e.g. preserved BQL rows) once it classifies this as NONMEM-format data.
  # EVID is left in place so vpc's EVID==0 dosing-row exclusion still applies.
  obs <- dplyr::select(obs, -dplyr::any_of("MDV"))
  sim <- dplyr::select(sim, -dplyr::any_of("MDV"))

  # -- Main VPC plot --
  vpc_plot <- vpc::vpc(
    sim       = sim,
    obs       = obs,
    sim_cols  = list(idv = time_col, dv = sim_col, pred = "PRED", sim = "rep"),
    obs_cols  = list(idv = time_col, dv = "DV",    pred = "PRED"),
    pred_corr = pred_corr,
    stratify  = stratify_final,
    show      = list(obs_dv = TRUE, obs_ci = TRUE),
    lloq      = lloq_val,
    vpc_theme = vpc_theme,
    smooth    = smooth,
    bins      = bins,
    pi        = pi,
    ci        = ci,
    scales    = vpc_spec$scales %||% "free_x",
    log_y     = logY,
    labeller  = ggplot2::label_value
  )
  vpc_plot <- strip_na_layers(vpc_plot)

  if (logY && !is.null(y_breaks)) {
    vpc_plot <- vpc_plot + ggplot2::scale_y_log10(breaks = y_breaks)
  } else if (!logY && !is.null(y_breaks)) {
    vpc_plot <- vpc_plot + ggplot2::scale_y_continuous(breaks = y_breaks)
  }

  if (!is.null(x_breaks))
    vpc_plot <- vpc_plot + ggplot2::scale_x_continuous(breaks = x_breaks)

  if (!is.null(xlim_vals) || !is.null(ylim_vals)) {
    vpc_plot <- vpc_plot +
      ggplot2::coord_cartesian(xlim = xlim_vals, ylim = ylim_vals)
  }

  vpc_plot <- vpc_plot +
    ggplot2::ggtitle(vpc_spec$title %||% "") +
    ggplot2::labs(x = x_label, y = y_label) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text  = ggplot2::element_text(size = sz_text),
      axis.title = ggplot2::element_text(size = sz_title),
      strip.text = ggplot2::element_text(size = sz_strip)
    )

  if (show_legend) vpc_plot <- add_vpc_legend(vpc_plot, vpc_theme, pi = pi)

  # -- Optional censored VPC panel --
  if (censor && !is.null(lloq_val)) {
    vpc_cens_plot <- vpc::vpc_cens(
      sim       = sim,
      obs       = obs,
      sim_cols  = list(idv = time_col, dv = sim_col, pred = "PRED", sim = "rep"),
      obs_cols  = list(idv = time_col, dv = "DV",    pred = "PRED"),
      stratify  = stratify_final,
      show      = list(obs_dv = TRUE, obs_ci = TRUE),
      lloq      = lloq_val + 0.01,
      vpc_theme = vpc_theme,
      ylab      = "Probability of <LLOQ",
      bins      = bins,
      ci        = ci,
      labeller  = ggplot2::label_value
    )
    vpc_cens_plot <- strip_na_layers(vpc_cens_plot)
    vpc_cens_plot <- vpc_cens_plot +
      ggplot2::labs(x = x_label) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        axis.text  = ggplot2::element_text(size = sz_text),
        axis.title = ggplot2::element_text(size = sz_title),
        strip.text = ggplot2::element_text(size = sz_strip)
      )

    if (requireNamespace("cowplot", quietly = TRUE)) {
      return(cowplot::plot_grid(vpc_plot, vpc_cens_plot,
                                ncol = 1, rel_heights = c(2, 1), align = "v"))
    }
  }

  vpc_plot
}
