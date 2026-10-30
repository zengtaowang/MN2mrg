#' Translate a Monolix project to an mrgsolve model
#'
#' Reads a fitted Monolix project through `lixoftConnectors` and returns
#' mrgsolve model code built from its structural model, final population
#' estimates, individual parameter model (including covariate, IOV and
#' correlation structure) and error model. Mirrors [nonmem2mrgsolve()].
#'
#' Requires a MonolixSuite installation, because `lixoftConnectors` ships
#' with MonolixSuite rather than CRAN. Covariate and regressor values written
#' into `$PARAM` default to 1 (continuous covariates and regressors) and 0
#' (categorical covariates); pass the `*_values` arguments to change them.
#'
#' @param project_path Character. Path to the `.mlxtran` project file.
#' @param monolix_path Character. Path to the MonolixSuite installation
#'   directory, e.g. `"/path/to/MonolixSuite2024R1"`.
#' @param regressor_values,cont_covariate_values,cat_covariate_values Numeric
#'   vectors of values for `$PARAM`, in the order of
#'   `components$regressors`, `components$cont_covariate_names` and
#'   `components$cat_covariate_names`. `NULL` uses the defaults.
#'
#' @return A list with
#'   * `code`: character vector of mrgsolve model lines;
#'   * `structural_model`: the expanded structural model text (library
#'     references and macros resolved);
#'   * `library_source`: directory the Monolix library models were resolved
#'     from, or `NA` when it could not be resolved;
#'   * `components`: the parsed pieces `code` was assembled from, which
#'     [assemble_monolix_model()] can reassemble with new covariate values.
#'
#' @importFrom stringr str_c
#' @importFrom purrr map2_chr
#' @importFrom stats setNames
#' @export
monolix2mrgsolve <- function(project_path, monolix_path,
                             regressor_values = NULL,
                             cont_covariate_values = NULL,
                             cat_covariate_values = NULL) {
  if (!requireNamespace("lixoftConnectors", quietly = TRUE)) {
    stop("monolix2mrgsolve() requires the lixoftConnectors package, which ",
         "ships with MonolixSuite and is not installed.", call. = FALSE)
  }
  if (!file.exists(project_path)) {
    stop("Monolix project not found: ", project_path, call. = FALSE)
  }

  lixoftConnectors::initializeLixoftConnectors(software = "monolix",
                                               path = monolix_path)
  lixoftConnectors::loadProject(project_path)

  # Get structural model
  structural_model_path <- lixoftConnectors::getStructuralModel()
  structural_model_text <- read_structural_model(structural_model_path)
  
  # Clean the structural model text
  cleaned_text <- strip_comments(structural_model_text)
  cleaned_text <- collapse_ws(cleaned_text)
  cleaned_text <- sanitize_reserved_names(cleaned_text)
  
  # Extract ODEs
  odes <- extract_odes(cleaned_text)
  odes <- ifelse(str_detect(odes, ";\\s*$"), odes, paste0(odes, ";"))
  # Convert power notation from ^ to pow()
  odes <- map_chr(odes, convert_power_notation)
  # Convert integers to floats for proper division
  odes <- map_chr(odes, convert_integers_to_floats)
  # Convert max() and min() to fmax() and fmin()
  odes <- map_chr(odes, convert_minmax_notation)
  
  # Extract algebraic definitions and separate them
  algebraics <- extract_algebraic_defs(cleaned_text)
  # Convert power notation in algebraics
  algebraics <- map_chr(algebraics, convert_power_notation)
  # Convert integers to floats for proper division
  algebraics <- map_chr(algebraics, convert_integers_to_floats)
  # Convert max() and min() to fmax() and fmin()
  algebraics <- map_chr(algebraics, convert_minmax_notation)
  # Convert rem() notation to C++ equivalent
  algebraics <- map_chr(algebraics, convert_rem_notation)
  
  # Extract depot info (bioavailability, implicit depot compartments, absorption terms)
  depot_info <- extract_depot_info(cleaned_text)
  bioavail_lines <- depot_info$bioavail_lines
  if (length(bioavail_lines) > 0) {
    # Convert power notation and integers to floats in bioavailability
    bioavail_lines <- map_chr(bioavail_lines, convert_power_notation)
    bioavail_lines <- map_chr(bioavail_lines, convert_integers_to_floats)
    # Convert max() and min() to fmax() and fmin()
    bioavail_lines <- map_chr(bioavail_lines, convert_minmax_notation)
  }
  if (length(depot_info$depot_cpts) > 0) {
    message("Depot compartments to add: ", paste(depot_info$depot_cpts, collapse = ", "))
  }
  
  # Get ODE variables
  ode_vars <- str_match(odes, "^dxdt_([a-zA-Z0-9_]+)\\s*=")[,2]
  ode_vars <- ode_vars[!is.na(ode_vars)]
  
  # First pass: identify time-varying variables (includes ODE vars and any that depend on them)
  time_varying_info <- identify_time_varying_vars(algebraics, ode_vars)
  time_varying_indices <- time_varying_info$indices
  time_varying_vars <- time_varying_info$time_varying_vars
  
  # Identify all if-blocks in algebraics
  # An if-block should go to ODE if it references ANY time-varying variable
  if_block_ranges <- list()
  i <- 1
  while (i <= length(algebraics)) {
    if (str_detect(algebraics[i], "^double\\s*if\\b")) {
      start_idx <- i
      end_idx <- i
      # Find the end of the if-block
      while (end_idx <= length(algebraics) && 
             !str_detect(algebraics[end_idx], "^double\\s*end$")) {
        end_idx <- end_idx + 1
      }
      if_block_ranges[[length(if_block_ranges) + 1]] <- start_idx:end_idx
      i <- end_idx + 1
    } else {
      i <- i + 1
    }
  }
  
  # Check each if-block: if it contains ANY time-varying variable, mark entire block as time-varying
  for (block_range in if_block_ranges) {
    block_text <- paste(algebraics[block_range], collapse = " ")
    # Check if any time-varying variable appears in the block
    if (length(time_varying_vars) > 0) {
      contains_time_varying <- any(sapply(time_varying_vars, function(var) {
        str_detect(block_text, paste0("\\b", var, "\\b"))
      }))
      
      if (contains_time_varying) {
        # Mark entire if-block as time-varying
        time_varying_indices <- unique(c(time_varying_indices, block_range))
      }
    }
  }
  
  time_varying_indices <- sort(time_varying_indices)
  
  # Separate equations for ODE block (time-varying) and MAIN block (not time-varying)
  # Guard: if time_varying_indices is empty or contains only 0, all go to MAIN
  if (length(time_varying_indices) == 0 || all(time_varying_indices == 0)) {
    alg_for_ode <- character(0)
    alg_for_main <- algebraics
  } else {
    alg_for_ode <- algebraics[time_varying_indices]
    alg_for_main <- algebraics[-time_varying_indices]
  }
  
  # Process if/else blocks (convert to C++ syntax)
  alg_for_ode <- convert_if_block(alg_for_ode)
  alg_for_main <- convert_if_block(alg_for_main)
  
  # Add semicolons
  add_semis <- function(v) {
    needs <- !str_detect(v, "(;|\\{|\\})\\s*$")
    v[needs] <- paste0(v[needs], ";")
    v
  }
  
  alg_for_ode <- add_semis(alg_for_ode)
  alg_for_main <- add_semis(alg_for_main)
  
  # Get output block
  output_block <- extract_output_block(cleaned_text)
  output_vars <- ""
  capture_block <- ""
  
  if (length(output_block)) {
    # First try to extract variables inside curly braces {}
    vars_raw <- str_match(output_block, "\\{([^}]+)\\}")[,2]
    vars_raw <- vars_raw[!is.na(vars_raw)]
    
    # If no curly braces found, try to extract output = variable pattern
    if (length(vars_raw) == 0) {
      # Pattern: output = variable_name or just variable_name
      for (line in output_block) {
        # Try pattern: output = VAR
        match <- str_match(line, "^\\s*output\\s*=\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\s*$")
        if (!is.na(match[1, 2])) {
          vars_raw <- c(vars_raw, match[1, 2])
        } else {
          # Try pattern: just VAR (single variable name on a line)
          match <- str_match(line, "^\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\s*$")
          if (!is.na(match[1, 2])) {
            vars_raw <- c(vars_raw, match[1, 2])
          }
        }
      }
    }
    
    if (length(vars_raw) > 0) {
      # Remove commas (Monolix allows commas to separate variables, mrgsolve does not)
      output_vars <- str_replace_all(paste(vars_raw, collapse = " "), ",", " ")
      output_vars <- str_squish(output_vars)
      capture_block <- paste0("$CAPTURE ", output_vars)
    }
  }
  
  # Filter out compartment variables from capture block
  # Compartment variables are defined in $CMT and should not be in $CAPTURE
  if (capture_block != "" && length(ode_vars) > 0) {
    # Split output_vars into individual variable names
    output_var_list <- str_split(output_vars, "\\s+")[[1]]
    
    # Remove any variables that match compartment names
    output_var_list_filtered <- output_var_list[!output_var_list %in% ode_vars]
    
    # Rebuild capture block without compartment variables
    if (length(output_var_list_filtered) > 0) {
      output_vars <- paste(output_var_list_filtered, collapse = " ")
      capture_block <- paste0("$CAPTURE Y ", output_vars)
    } else {
      # If all variables were compartments, just capture Y
      output_vars <- ""
      capture_block <- "$CAPTURE Y"
    }
  } else {
    # Even if no other outputs, still capture Y
    capture_block <- "$CAPTURE Y"
  }
  
  # Get parameters
  params <- lixoftConnectors::getEstimatedPopulationParameters()
  pop_params <- params[str_detect(names(params), "_pop")]

  # IOV (inter-occasion variability) parameters use a "gamma_" prefix (not
  # "omega_") for the occasion-level SD in Monolix's IOV syntax, e.g.:
  #   dap = {..., varlevel={id, id*occ}, sd={omega_dap, gamma_dap}}
  # Identify true IOV parameters from the individual parameter model's
  # variability structure (any level other than "id") rather than matching
  # on the "gamma_" prefix directly -- some models also have unrelated
  # structural parameters legitimately named "gamma_pop" (e.g. a Hill
  # coefficient) that must NOT be treated as an IOV variance term.
  #
  # mrgsolve can't collapse the two levels into a single eta: it draws ALL
  # possible occasion-etas once per simulated individual (from $OMEGA), and
  # a data-driven occasion covariate selects which pre-drawn value is
  # "active" at each record. So for parameter <p> with occasion column
  # <lvl>, we generate one independent omega_<p>_iov_<i> line per distinct
  # occasion value i (all sharing the single gamma_<p> variance Monolix
  # estimates), plus a $MAIN if/else cascade (added in the formula loop
  # below) that assigns the active one to omega_<lvl>_<p> for the current
  # record.
  iov_variability <- lixoftConnectors::getIndividualParameterModel()$variability
  iov_level_names <- setdiff(names(iov_variability), "id")
  iov_param_names <- unique(unlist(lapply(iov_level_names, function(lvl) {
    names(iov_variability[[lvl]])[iov_variability[[lvl]]]
  })))

  # param -> occasion level name (e.g. "dap" -> "dose_cumsum")
  iov_param_to_level <- setNames(
    unlist(lapply(iov_level_names, function(lvl) {
      p <- names(iov_variability[[lvl]])[iov_variability[[lvl]]]
      rep(lvl, length(p))
    })),
    iov_param_names
  )

  # Distinct occasion values per level, read from Monolix's own IOV
  # bookkeeping (estimatedRandomEffects.txt has one row per id*occasion) --
  # NOT from the raw dataset column directly, since that can contain extra
  # sentinel values (e.g. 0 for pre-dose/baseline rows) that Monolix never
  # treats as a real occasion for IOV purposes.
  iov_occasion_values <- list()
  if (length(iov_level_names) > 0) {
    proj_name_iov  <- tools::file_path_sans_ext(basename(project_path))
    result_dir_iov <- file.path(dirname(project_path), proj_name_iov)
    eta_file_iov   <- file.path(result_dir_iov, "IndividualParameters",
                                "estimatedRandomEffects.txt")
    if (file.exists(eta_file_iov)) {
      eta_df_iov <- utils::read.table(eta_file_iov, header = TRUE, sep = ",", check.names = FALSE)
      for (lvl in iov_level_names) {
        if (lvl %in% names(eta_df_iov)) {
          iov_occasion_values[[lvl]] <- sort(unique(eta_df_iov[[lvl]]))
        }
      }
    }
  }

  # Build one omega_<p>_iov_<i> entry per (IOV parameter, occasion value),
  # all sharing the single gamma_<p> SD Monolix estimated for that parameter.
  iov_omega_params <- c()
  for (p in iov_param_names) {
    lvl <- iov_param_to_level[[p]]
    occ_vals <- iov_occasion_values[[lvl]]
    gamma_name <- paste0("gamma_", p)
    if (length(occ_vals) == 0 || !gamma_name %in% names(params)) next
    entries <- setNames(rep(params[[gamma_name]], length(occ_vals)),
                        paste0("omega_", p, "_iov_", occ_vals))
    iov_omega_params <- c(iov_omega_params, entries)
  }

  omega_params <- c(params[startsWith(names(params), "omega_")], iov_omega_params)
  beta_params <- params[str_detect(names(params), "beta_")]

  # Extract sigma parameters based on DVID presence
  model_has_dvid <- has_dvid_column()
  dvid_map <- NULL

  if (model_has_dvid) {
    dvid_map <- extract_dvid_mapping(project_path)
    # Preserve the original Monolix observation variable name --
    # obs_variable gets renamed below (y<n> -> y<n>_) to avoid clashing
    # with mrgsolve's reserved y[n] state accessors, but Monolix's own
    # predictions_<obs_variable>.txt files always use the original name.
    if (!is.null(dvid_map)) dvid_map$monolix_obs_variable <- dvid_map$obs_variable
  }

  if (!is.null(dvid_map) && nrow(dvid_map) > 0) {
    # Multiple observations: extract sigma by DVID identifier. The error
    # model's own $parameters (keyed by obs_variable) lists exactly which
    # sigma parameter names apply to that observation -- e.g. "b_PFC" for
    # a proportional error model with an abbreviated suffix -- so we look
    # those up directly instead of guessing "{a|b}{dvid_id}" patterns.
    error_model <- lixoftConnectors::getContinuousObservationModel()
    sigma_params <- list()
    for (i in seq_len(nrow(dvid_map))) {
      dvid_id  <- dvid_map$dvid_identifier[i]
      obs_var  <- dvid_map$obs_variable[i]
      fitted_names <- intersect(error_model$parameters[[obs_var]], names(params))

      a_name <- fitted_names[startsWith(fitted_names, "a")]
      b_name <- fitted_names[startsWith(fitted_names, "b")]

      param_a <- if (length(a_name) == 1) params[[a_name]] else NA_real_
      param_b <- if (length(b_name) == 1) params[[b_name]] else NA_real_

      sigma_params[[i]] <- list(
        dvid_identifier = dvid_id,
        mrgsolve_dvid = dvid_map$mrgsolve_dvid[i],
        a_param = param_a,
        b_param = param_b,
        a_name = if (length(a_name) == 1) a_name else paste0("a", dvid_id),
        b_name = if (length(b_name) == 1) b_name else paste0("b", dvid_id)
      )
    }
  } else {
    # Single observation: extract just "a" and "b"
    sigma_params <- list(list(
      dvid_identifier = "",
      mrgsolve_dvid = 1,
      a_param = if ("a" %in% names(params)) params[["a"]] else NA_real_,
      b_param = if ("b" %in% names(params)) params[["b"]] else NA_real_,
      a_name = "a",
      b_name = "b"
    ))
  }

  # Match both corr_ and corr1_ only
  corr_params <- params[str_detect(names(params), "^corr1?_")]

  # Get regressors and covariates
  regressors <- extract_regressors(cleaned_text)
  data_info <- lixoftConnectors::getData()
  covariate_info_extracted <- extract_covariates(data_info)
  cont_covariate_names <- covariate_info_extracted$continuous
  cat_covariate_names <- covariate_info_extracted$categorical
  
  # Get covariate formulas and extract transformed categorical covariates
  covariate_info <- lixoftConnectors::getCovariateInformation()
  covariate_formulas <- covariate_info$formula
  
  # Extract transformed categorical covariates
  transformed_cat_info <- extract_transformed_categorical_covariates(covariate_formulas)
  transformed_cat_names <- transformed_cat_info$names
  
  # Merge transformed categorical covariates with regular categorical covariates
  cat_covariate_names <- c(cat_covariate_names, transformed_cat_names)
  cat_covariate_names <- unique(cat_covariate_names)
  
  covariate_names <- c(cont_covariate_names, cat_covariate_names)  # Combined for backward compatibility
  
  # Convert continuous covariate formulas to C++ code
  covariate_def_lines <- if (length(covariate_formulas) > 0) {
    convert_covariate_formulas(covariate_formulas)
  } else {
    character()
  }
  
  # Extract categorical covariate categories from beta parameters
  # Pattern: beta_PARAM_COVAR_CATEGORY
  # Example: beta_GSS_POP_T1D -> parameter=GSS, covariate=POP, category=T1D
  cat_covariate_categories <- list()
  if (length(cat_covariate_names) > 0 && length(beta_params) > 0) {
    for (cat_cov in cat_covariate_names) {
      # Find all beta parameters that contain this categorical covariate
      # Pattern: beta_.*_COVARNAME_.*
      pattern <- paste0("^beta_[^_]+_", cat_cov, "_(.+)$")
      matching_betas <- names(beta_params)[str_detect(names(beta_params), pattern)]
      
      if (length(matching_betas) > 0) {
        # Extract category names
        categories <- str_match(matching_betas, pattern)[, 2]
        categories <- categories[!is.na(categories)]
        cat_covariate_categories[[cat_cov]] <- unique(categories)
      }
    }
  }
  
  # Extract reference categories and all categories for categorical covariates
  # The first value in covariate_info$categories is always the reference
  cat_covariate_reference <- list()
  cat_covariate_all_categories <- list()
  
  # First, handle regular categorical covariates from covariate_info
  if (length(cat_covariate_names) > 0 && !is.null(covariate_info$categories)) {
    for (cat_cov in cat_covariate_names) {
      if (cat_cov %in% names(covariate_info$categories)) {
        categories_all <- covariate_info$categories[[cat_cov]]
        if (length(categories_all) > 0) {
          cat_covariate_reference[[cat_cov]] <- categories_all[1]  # First is reference
          cat_covariate_all_categories[[cat_cov]] <- categories_all  # Store all categories
        }
      }
    }
  }
  
  # Second, handle transformed categorical covariates from covariate formulas
  # These override or supplement the regular categorical covariates
  if (length(transformed_cat_names) > 0) {
    for (cat_cov in transformed_cat_names) {
      cat_covariate_reference[[cat_cov]] <- transformed_cat_info$references[[cat_cov]]
      cat_covariate_all_categories[[cat_cov]] <- transformed_cat_info$all_categories[[cat_cov]]
    }
  }
  
  # Identify significant covariates and which parameters they influence
  # Beta parameters follow the format: beta_PARAMETER_COVARIATE[_CATEGORY]
  # However, covariates can be transformed (e.g., wt -> lw70, sex -> tGENDER)
  # The transformed name in beta params may share NO substring with the original.
  # We build a mapping: original_covariate -> c(transformed_name1, transformed_name2, ...)
  # so that significance of a transformed covariate propagates to the original.
  beta_param_names <- names(beta_params)
  
  # --- Build transformed -> original covariate mappings ---
  # For continuous covariates: parse formulas like "lw70 = log(wt/70)"
  # The LHS is the transformed name; any original continuous covariate appearing on the RHS
  # establishes a link.
  cont_transformed_to_original <- list()  # transformed_name -> c(original_cov, ...)
  
  # Build a list of continuous formula strings to process.
  # covariate_formulas can be: (a) a named list with string and list elements,
  # (b) a single character string (when only one formula exists), or (c) a
  # character vector of comma-separated formulas.
  cont_formula_strings <- list()  # named list: transformed_name -> formula_string
  
  if (is.list(covariate_formulas) && length(covariate_formulas) > 0) {
    for (fname in names(covariate_formulas)) {
      formula_item <- covariate_formulas[[fname]]
      # Only process continuous (string) formulas, skip categorical (list) formulas
      if (is.character(formula_item)) {
        cont_formula_strings[[fname]] <- formula_item
      }
    }
  } else if (is.character(covariate_formulas) && length(covariate_formulas) > 0) {
    # Single string or comma-separated string: "lw70 = log(wt/70)" or "lw70 = log(wt/70), la50 = log(age/50)"
    formula_strs <- unlist(str_split(covariate_formulas, ","))
    formula_strs <- str_trim(formula_strs)
    formula_strs <- formula_strs[nchar(formula_strs) > 0]
    for (fstr in formula_strs) {
      # Extract the LHS name as the key
      lhs_match <- str_match(fstr, "^([a-zA-Z0-9_]+)\\s*=")
      if (!is.na(lhs_match[1, 2])) {
        cont_formula_strings[[lhs_match[1, 2]]] <- fstr
      }
    }
  }
  
  # Now process each continuous formula to find original covariates on the RHS
  for (fname in names(cont_formula_strings)) {
    formula_str <- cont_formula_strings[[fname]]
    # Parse: "lw70 = log(wt/70)"
    rhs <- str_trim(str_split(formula_str, "=")[[1]][2])
    if (!is.na(rhs)) {
      # Check which original continuous covariates appear on the RHS
      for (orig_cov in cont_covariate_names) {
        if (str_detect(rhs, paste0("\\b", orig_cov, "\\b"))) {
          cont_transformed_to_original[[fname]] <- c(cont_transformed_to_original[[fname]], orig_cov)
        }
      }
    }
  }
  # Invert: original_cov -> c(transformed_name1, transformed_name2, ...)
  cont_original_to_transformed <- list()
  for (tname in names(cont_transformed_to_original)) {
    for (orig in cont_transformed_to_original[[tname]]) {
      cont_original_to_transformed[[orig]] <- c(cont_original_to_transformed[[orig]], tname)
    }
  }
  
  # For categorical covariates: use transformed_cat_info$original_covariates
  # which maps transformed_name -> original_covariate (the "from" field)
  cat_original_to_transformed <- list()
  if (length(transformed_cat_info$original_covariates) > 0) {
    for (tname in names(transformed_cat_info$original_covariates)) {
      orig <- transformed_cat_info$original_covariates[[tname]]
      cat_original_to_transformed[[orig]] <- c(cat_original_to_transformed[[orig]], tname)
    }
  }
  
  
  # For continuous covariates
  cont_covariate_params <- lapply(cont_covariate_names, function(cov) {
    # Search names: the original covariate name + any transformed names derived from it
    search_names <- c(cov)
    if (cov %in% names(cont_original_to_transformed)) {
      search_names <- c(search_names, cont_original_to_transformed[[cov]])
    }
    extract_influenced_params(search_names, beta_param_names)
  })
  names(cont_covariate_params) <- cont_covariate_names
  
  significant_cont_covariates <- sapply(cont_covariate_params, function(params) {
    length(params) > 0
  })
  
  # For categorical covariates
  # Do NOT propagate significance from transformed covariates to originals.
  # Each categorical covariate is only checked by its own name in beta params.
  # e.g., if only tGENDER (transformed from sex) appears in beta params,
  # sex stays non-significant. But if sex itself also appears in beta params,
  # sex is significant on its own merit.
  cat_covariate_params <- lapply(cat_covariate_names, function(cov) {
    # Only search for this covariate's own name (no transformed names)
    extract_influenced_params(cov, beta_param_names)
  })
  names(cat_covariate_params) <- cat_covariate_names
  
  significant_cat_covariates <- sapply(cat_covariate_params, function(params) {
    length(params) > 0
  })
  
  # Get individual parameter model
  indi_param_model <- lixoftConnectors::getIndividualParameterModel()
  formulas <- indi_param_model$formula
  formulas <- sanitize_reserved_names(formulas)
  
  # Process each formula to create proper parameter definition lines
  formula_lines <- str_split(formulas, "\n")[[1]]
  formula_lines <- formula_lines[str_length(formula_lines) > 0]
  # Remove "Correlations" line and everything after it
  corr_idx <- which(str_detect(formula_lines, "^\\s*Correlations\\s*$"))
  if (length(corr_idx) > 0) {
    formula_lines <- formula_lines[1:(corr_idx[1] - 1)]
  }
  
  param_def_lines <- map(formula_lines, function(formula) {
    # Split on the first = sign only to handle categorical covariates with = inside brackets
    # Example: log(GSS) = log(GSS_pop) + beta_GSS_POP_T2D*[POP = T2D] + eta_ID_GSS
    first_eq_pos <- str_locate(formula, "=")[1]
    
    if (is.na(first_eq_pos)) {
      # No equation found, return as-is
      return(formula)
    }
    
    left_side <- str_trim(str_sub(formula, 1, first_eq_pos - 1))
    right_side <- str_trim(str_sub(formula, first_eq_pos + 1))

    # Replace eta_ID_PARAM and eta_PARAM with omega_PARAM
    # Use word boundary to avoid replacing "eta" in "beta_"
    # First remove ID_ from eta_ID_ patterns, then replace eta_ with omega_
    # Note: an IOV term is named eta_<occasion_level>_PARAM (e.g.
    # eta_dose_cumsum_dap), which does NOT match "eta_ID_" and so falls
    # through to the blind eta_ -> omega_ replace below, naturally
    # producing "omega_dose_cumsum_dap" -- this is exactly the cascade
    # variable name assigned by the $MAIN if/else block generated below.
    right_side <- str_replace_all(right_side, "\\beta_ID_", "eta_")
    right_side <- str_replace_all(right_side, "\\beta_", "omega_")

    # Convert power notation from ^ to pow()
    right_side <- convert_power_notation(right_side)
    # Convert integers to floats for proper division
    right_side <- convert_integers_to_floats(right_side)
    # Convert max() and min() to fmax() and fmin()
    right_side <- convert_minmax_notation(right_side)
    # Convert categorical covariate syntax from [COVAR = VALUE] to COVAR
    right_side <- convert_categorical_covariate(right_side)

    # Determine the underlying parameter name on the left-hand side (works
    # whether the formula is log-transformed, logitnormal, or plain) so we
    # can tell whether this parameter carries IOV and, if so, prepend a
    # $MAIN if/else cascade that selects the pre-drawn occasion-eta before
    # the parameter's own equation line.
    param_name <- if (!is.na(str_match(left_side, "^log\\(\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\s*/\\s*\\(\\s*1\\s*-\\s*\\1\\s*\\)\\s*\\)$")[1, 1])) {
      str_match(left_side, "^log\\(\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\s*/\\s*\\(\\s*1\\s*-\\s*\\1\\s*\\)\\s*\\)$")[1, 2]
    } else if (str_detect(left_side, "^log\\((.+)\\)$")) {
      str_match(left_side, "^log\\((.+)\\)$")[1, 2]
    } else {
      left_side
    }

    iov_cascade_lines <- character(0)
    if (param_name %in% iov_param_names) {
      lvl <- iov_param_to_level[[param_name]]
      occ_vals <- iov_occasion_values[[lvl]]
      if (length(occ_vals) > 0) {
        occ_var <- paste0("omega_", lvl, "_", param_name)
        default_line <- sprintf("double %s = omega_%s_iov_%s;", occ_var, param_name, occ_vals[1])
        other_lines <- if (length(occ_vals) > 1) {
          sprintf("if(%s == %s) %s = omega_%s_iov_%s;", lvl, occ_vals[-1], occ_var, param_name, occ_vals[-1])
        } else character(0)
        iov_cascade_lines <- c(default_line, other_lines)
      }
    }

    # Check if it's a logitnormal distribution: log(VAR / (1 - VAR)) on left side
    # This needs to be converted to odds_VAR intermediate variable
    logitnormal_match <- str_match(left_side, "^log\\(\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\s*/\\s*\\(\\s*1\\s*-\\s*\\1\\s*\\)\\s*\\)$")
    if (!is.na(logitnormal_match[1, 1])) {
      param <- logitnormal_match[1, 2]
      # Create two lines: odds_PARAM = exp(...) and PARAM = odds_PARAM / (1 + odds_PARAM)
      line1 <- str_c("double odds_", param, " = exp(", right_side, ");")
      line2 <- str_c("double ", param, " = odds_", param, " / (1.0 + odds_", param, ");")
      return(c(iov_cascade_lines, line1, line2))
    }

    # Check if it's a log-transformed parameter
    if (str_detect(left_side, "^log\\((.+)\\)$")) {
      param <- str_match(left_side, "^log\\((.+)\\)$")[1,2]
      c(iov_cascade_lines, str_c("double ", param, " = exp(", right_side, ");"))
    } else {
      c(iov_cascade_lines, str_c("double ", left_side, " = ", right_side, ";"))
    }
  })

  
  # Flatten the list in case logitnormal transformation created multiple lines
  param_def_lines <- unlist(param_def_lines)

  # Create omega parameter lines
  # IOV omega slots are named omega_<param>_iov_<occasion> (synthesized
  # above, one independent draw per occasion value) -- they are never
  # correlated with the BSV omega_<param> slot or with each other, so pull
  # them out into their own group and let the existing correlation logic
  # below operate only on the true BSV omega_ entries.
  is_iov_slot <- str_detect(names(omega_params), "_iov_[^_]+$")
  bsv_omega_params <- omega_params[!is_iov_slot]
  iov_slot_params  <- omega_params[is_iov_slot]

  omega_var_name  <- function(x) str_remove(str_remove(x, "^omega_"), "_+$")
  omega_var_label <- function(x) "IIV"

  # If there are correlation parameters, we need to create separate $OMEGA blocks for each correlation group
  if (length(corr_params) > 0) {
    # Extract parameter names from correlation parameters
    # Handles both corr_V_Cl and corr1_V_Cl formats
    corr_param_pairs <- lapply(names(corr_params), function(corr_name) {
      # Remove corr_ or corr1_ prefix
      param_names <- str_remove(corr_name, "^corr1?_")
      str_split(param_names, "_")[[1]]
    })

    # Build correlation groups - parameters that are correlated together form a group
    # Use a graph-based approach to find connected components
    all_correlated_params <- unique(unlist(corr_param_pairs))
    correlation_groups <- list()

    for (param in all_correlated_params) {
      # Find which group this parameter belongs to
      found_group <- FALSE
      for (i in seq_along(correlation_groups)) {
        # Check if this parameter is correlated with any parameter in this group
        for (pair in corr_param_pairs) {
          if (param %in% pair && any(pair %in% correlation_groups[[i]])) {
            correlation_groups[[i]] <- unique(c(correlation_groups[[i]], param))
            found_group <- TRUE
            break
          }
        }
        if (found_group) break
      }

      # If not found in any existing group, create a new group
      if (!found_group) {
        # Find all parameters directly correlated with this one
        related_params <- param
        for (pair in corr_param_pairs) {
          if (param %in% pair) {
            related_params <- unique(c(related_params, pair))
          }
        }
        correlation_groups[[length(correlation_groups) + 1]] <- related_params
      }
    }

    # Merge overlapping groups (iteratively until no more merges)
    merged <- TRUE
    while (merged) {
      merged <- FALSE
      for (i in seq_along(correlation_groups)) {
        if (i > length(correlation_groups)) break
        for (j in seq_along(correlation_groups)) {
          if (j <= i || j > length(correlation_groups)) next
          # Check if groups i and j have any overlap
          if (any(correlation_groups[[i]] %in% correlation_groups[[j]])) {
            # Merge j into i
            correlation_groups[[i]] <- unique(c(correlation_groups[[i]], correlation_groups[[j]]))
            correlation_groups[[j]] <- NULL
            correlation_groups <- Filter(Negate(is.null), correlation_groups)
            merged <- TRUE
            break
          }
        }
        if (merged) break
      }
    }

    # Separate omega parameters into correlated groups and uncorrelated
    omega_names <- omega_var_name(names(bsv_omega_params))
    all_correlated <- unique(unlist(correlation_groups))
    uncorrelated_omega <- bsv_omega_params[!omega_names %in% all_correlated]

    # Build $OMEGA blocks for each correlation group
    omega_param_lines <- character(0)

    for (group in correlation_groups) {
      # Get omega parameters for this group
      group_omega <- bsv_omega_params[omega_names %in% group]

      if (length(group_omega) > 0) {
        # Build the correlated $OMEGA block with @block @correlation
        correlated_lines <- c("$OMEGA @annotated @block @correlation")

        # Add first parameter
        first_param <- names(group_omega)[1]
        first_var <- omega_var_name(first_param)
        first_param <- str_remove(first_param, "_+$")
        first_line <- sprintf("%-14s : %-10g : %s on %s", first_param, group_omega[1]^2, omega_var_label(first_param), first_var)
        correlated_lines <- c(correlated_lines, first_line)

        # Add remaining correlated parameters with correlation values
        if (length(group_omega) > 1) {
          for (i in 2:length(group_omega)) {
            param_name <- names(group_omega)[i]
            var <- omega_var_name(param_name)
            param_name <- str_remove(param_name, "_+$")

            # Find correlation value with previous parameters in this group
            corr_values <- sapply(1:(i-1), function(j) {
              prev_var <- omega_var_name(names(group_omega)[j])

              # Look for corr_VAR1_VAR2 or corr1_VAR1_VAR2 or corr_VAR2_VAR1 or corr1_VAR2_VAR1
              corr_pattern1 <- paste0("^corr1?_", prev_var, "_", var, "$")
              corr_pattern2 <- paste0("^corr1?_", var, "_", prev_var, "$")

              match1 <- names(corr_params)[str_detect(names(corr_params), corr_pattern1)]
              match2 <- names(corr_params)[str_detect(names(corr_params), corr_pattern2)]

              if (length(match1) > 0) {
                return(corr_params[[match1[1]]])
              } else if (length(match2) > 0) {
                return(corr_params[[match2[1]]])
              } else {
                return(0)  # No correlation
              }
            })

            # Format: omega_Cl : corr_value1 corr_value2 ... omega_Cl_value : IIV on Cl
            corr_str <- paste(sprintf("%-10g", corr_values), collapse = " ")
            param_line <- sprintf("%-14s : %s %-10g : %s on %s", param_name, corr_str, group_omega[i]^2, omega_var_label(param_name), var)
            correlated_lines <- c(correlated_lines, param_line)
          }
        }

        # Add this group's block to omega_param_lines
        if (length(omega_param_lines) > 0) {
          omega_param_lines <- c(omega_param_lines, "", correlated_lines)
        } else {
          omega_param_lines <- correlated_lines
        }
      }
    }

    # Build uncorrelated $OMEGA block (standard format)
    if (length(uncorrelated_omega) > 0) {
      uncorrelated_lines <- map2_chr(
        names(uncorrelated_omega),
        uncorrelated_omega,
        ~{
          param_name <- str_remove(.x, "_+$")
          var <- omega_var_name(.x)
          sprintf("%-14s : %-10g : %s on %s", param_name, .y^2, omega_var_label(.x), var)
        }
      )
      uncorrelated_lines <- c("$OMEGA @annotated", uncorrelated_lines)

      # Add uncorrelated block
      if (length(omega_param_lines) > 0) {
        omega_param_lines <- c(omega_param_lines, "", uncorrelated_lines)
      } else {
        omega_param_lines <- uncorrelated_lines
      }
    }

  } else {
    # No correlations - use standard format
    if (length(bsv_omega_params) > 0) {
      omega_param_lines <- map2_chr(
        names(bsv_omega_params),
        bsv_omega_params,
        ~{
          param_name <- str_remove(.x, "_+$")
          var <- omega_var_name(.x)
          sprintf("%-14s : %-10g : %s on %s", param_name, .y^2, omega_var_label(.x), var)
        }
      )
      omega_param_lines <- c("$OMEGA @annotated", omega_param_lines)
    } else {
      # No omega parameters - don't create $OMEGA block
      omega_param_lines <- character(0)
    }
  }

  # IOV omega slots get their own dedicated, always-uncorrelated $OMEGA
  # block: N independent omega_<param>_iov_<i> draws (one per occasion
  # value actually seen in estimatedRandomEffects.txt), each carrying the
  # single gamma_<param> variance Monolix estimated for that parameter.
  if (length(iov_slot_params) > 0) {
    iov_lines <- map2_chr(
      names(iov_slot_params),
      iov_slot_params,
      ~{
        m <- str_match(.x, "^omega_(.+)_iov_(.+)$")
        sprintf("%-14s : %-10g : IOV on %s (occ%s)", .x, .y^2, m[1, 2], m[1, 3])
      }
    )
    iov_lines <- c("$OMEGA @annotated", iov_lines)
    omega_param_lines <- if (length(omega_param_lines) > 0) {
      c(omega_param_lines, "", iov_lines)
    } else {
      iov_lines
    }
  }


  # Create sigma parameter lines (handle both single and multi-observation)
  sigma_param_lines <- c("$SIGMA @annotated")

  for (sigma_info in sigma_params) {
    dvid_id <- sigma_info$dvid_identifier
    if (!is.na(sigma_info$a_param)) {
      annotation <- if (dvid_id == "") "ADD ERROR" else paste0("ADD ERROR for DVID ", dvid_id)
      sigma_param_lines <- c(sigma_param_lines,
        sprintf("%-12s : %-10g : %s", sigma_info$a_name, sigma_info$a_param^2, annotation))
    }
    if (!is.na(sigma_info$b_param)) {
      annotation <- if (dvid_id == "") "PROP ERROR" else paste0("PROP ERROR for DVID ", dvid_id)
      sigma_param_lines <- c(sigma_param_lines,
        sprintf("%-12s : %-10g : %s", sigma_info$b_name, sigma_info$b_param^2, annotation))
    }
  }

  
  # Create $TABLE block from error model
  error_model <- lixoftConnectors::getContinuousObservationModel()
  error_model_distribution <- error_model$distribution
  error_model_formula <- error_model$formula
  
  # Convert error model formulas to $TABLE equations
  error_model_lines <- map2_chr(
    names(error_model_formula),
    error_model_formula,
    function(var_name, formula) {
      # Get the distribution type for this variable
      dist_type <- error_model_distribution[var_name]
      
      # Remove the newline and trim whitespace
      formula <- str_trim(str_replace(formula, "\\n$", ""))
      
      # Remove the " * e" or " *e" or "*e" at the end
      formula <- str_replace(formula, "\\s*\\*\\s*e\\s*$", "")
      
      if (dist_type == "logNormal") {
        # Formula is like: log(y6) = log(A4_ngL) + b6*log(A4_ngL)
        right_side <- str_trim(str_split(formula, "=")[[1]][2])
        paste0("double ", var_name, " = exp(", right_side, ");")
        
      } else if (dist_type == "logitNormal") {
        # Formula is like: log( y5 / ( 408.24 - y5 ) ) = log( GLUC1 / ( 408.24 - GLUC1 ) ) + b5*...
        left_side <- str_trim(str_split(formula, "=")[[1]][1])
        right_side <- str_trim(str_split(formula, "=")[[1]][2])
        
        # Extract the limit value from pattern like "( 408.24 - y5 )"
        limit_match <- str_match(left_side, "\\(\\s*([0-9.]+)\\s*-\\s*[a-zA-Z0-9_]+\\s*\\)")
        limit_value <- if (!is.na(limit_match[1, 2])) limit_match[1, 2] else "1"
        
        odds_line <- paste0("double odds_", var_name, " = exp(", right_side, ");")
        var_line <- paste0("double ", var_name, " = ", limit_value, " * odds_", var_name, " / (1.0 + odds_", var_name, ");")
        paste0(odds_line, "\n", var_line)
        
      } else {
        # Normal distribution - straightforward conversion
        paste0("double ", formula, ";")
      }
    }
  )
  
  # Split any multi-line entries (from logitnormal) and flatten
  error_model_lines <- unlist(str_split(error_model_lines, "\n"))

  # --- Rename y<digits> observation variables to y<digits>_ in $TABLE ---
  # mrgsolve reserves y[n] as ODE state accessors; declaring "double y1 = ..." causes a
  # conflict. Append "_" to any obs variable matching the pattern y followed by digits only.
  # This renames both the declaration (double y1_ = ...) and all RHS references.
  y_digit_vars <- names(error_model_formula)[str_detect(names(error_model_formula), "^y[0-9]+$")]
  if (length(y_digit_vars) > 0) {
    for (yv in y_digit_vars) {
      error_model_lines <- str_replace_all(error_model_lines, paste0("\\b", yv, "\\b"), paste0(yv, "_"))
    }
    # Also update dvid_map obs_variable so DV routing references the renamed variable
    if (!is.null(dvid_map)) {
      dvid_map$obs_variable <- ifelse(
        str_detect(dvid_map$obs_variable, "^y[0-9]+$"),
        paste0(dvid_map$obs_variable, "_"),
        dvid_map$obs_variable
      )
    }
  }

  # --- Resolve Y name conflicts between $ODE/$MAIN and $TABLE ---
  # If the modeler defined a variable named "Y" in the Monolix model,
  # it will appear in odes / alg_for_ode / alg_for_main / param_def_lines etc.
  # When the error model renames the obs variable → Y, this causes a double definition.
  # Fix: rename Y→Y_ in ODE/MAIN vectors AND on the RHS of error_model_lines.
  main_ode_vectors <- list("odes", "alg_for_ode", "alg_for_main", "param_def_lines",
                           "covariate_def_lines", "bioavail_lines")

  # Check for Y defined in ODE/MAIN
  y_in_ode_main <- FALSE
  for (vec_name in main_ode_vectors) {
    vec <- get(vec_name)
    if (any(str_detect(vec, "\\bY\\b"))) {
      y_in_ode_main <- TRUE
      break
    }
  }
  if (y_in_ode_main) {
    for (vec_name in main_ode_vectors) {
      vec <- get(vec_name)
      if (length(vec) > 0) {
        assign(vec_name, str_replace_all(vec, "\\bY\\b", "Y_"))
      }
    }
    for (j in seq_along(error_model_lines)) {
      line <- error_model_lines[j]
      eq_pos <- str_locate(line, "=")[1, "start"]
      if (!is.na(eq_pos)) {
        lhs <- str_sub(line, 1, eq_pos)
        rhs <- str_sub(line, eq_pos + 1)
        rhs <- str_replace_all(rhs, "\\bY\\b", "Y_")
        error_model_lines[j] <- paste0(lhs, rhs)
      }
    }
  }

  # Handle DVID routing: single vs. multi-observation models
  if (!model_has_dvid || is.null(dvid_map)) {
    # Single observation (no DVID column)
    # Rename the observation variable to "Y"
    if (length(error_model_formula) == 1) {
      first_var_match <- str_match(error_model_lines[1], "^double\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\s*=")
      if (!is.na(first_var_match[1, 2])) {
        original_var <- first_var_match[1, 2]
        if (original_var != "Y") {
          error_model_lines <- str_replace_all(error_model_lines,
                                                paste0("\\b", original_var, "\\b"),
                                                "Y")
        }
      }
    }
  } else {
    # Multiple observations (with DVID column)
    # Keep observation variables separate (y1, y2, ypk, ypd, etc.)
    # Then add routing logic based on DVID

    dvid_routing_lines <- character()

    for (i in seq_len(nrow(dvid_map))) {
      obs_var <- dvid_map$obs_variable[i]
      mrgsolve_dvid <- dvid_map$mrgsolve_dvid[i]

      if (i == 1) {
        # First observation: default assignment
        dvid_routing_lines <- c(dvid_routing_lines,
                                paste0("double Y = ", obs_var, ";  // Default (DVID=", mrgsolve_dvid, ")"))
      } else {
        # Subsequent observations: conditional assignment
        dvid_routing_lines <- c(dvid_routing_lines,
                                paste0("if(DVID==", mrgsolve_dvid, "){"),
                                paste0("  Y = ", obs_var, ";"),
                                "}")
      }
    }

    # Append routing logic to error model
    error_model_lines <- c(error_model_lines, "", dvid_routing_lines)
  }

  # Create the TABLE block
  table_block <- c("$TABLE", error_model_lines)
  
  # Create CMT block (include depot compartments from depot() with ka)
  cmt_block <- c("$CMT", ode_vars, depot_info$depot_cpts)
  
  # Modify target ODEs to include absorption from depot compartments
  if (length(depot_info$target_absorption) > 0) {
    for (target in names(depot_info$target_absorption)) {
      absorption_term <- depot_info$target_absorption[[target]]
      target_pattern <- paste0("^dxdt_", target, "\\s*=")
      idx <- which(str_detect(odes, target_pattern))
      if (length(idx) > 0) {
        ode_no_semi <- str_remove(odes[idx], ";\\s*$")
        odes[idx] <- paste0(ode_no_semi, " + ", absorption_term, ";")
      }
    }
  }
  
  # Prepend depot compartment ODEs
  if (length(depot_info$depot_odes) > 0) {
    depot_ode_lines <- depot_info$depot_odes
    depot_ode_lines <- ifelse(str_detect(depot_ode_lines, ";\\s*$"), 
                              depot_ode_lines, paste0(depot_ode_lines, ";"))
    odes <- c(depot_ode_lines, odes)
  }
  
  # Deduplicate equations to remove duplicate definitions
  param_def_lines <- deduplicate_equations(param_def_lines)
  covariate_def_lines <- deduplicate_equations(covariate_def_lines)
  bioavail_lines <- deduplicate_equations(bioavail_lines)
  alg_for_main <- deduplicate_equations(alg_for_main)
  alg_for_ode <- deduplicate_equations(alg_for_ode)
  
  # Convert Monolix reserved word "t" (time) to mrgsolve equivalents
  # In $MAIN block: t → TIME (record time)
  # In $ODE block:  t → SOLVERTIME (solver integration time)
  if (length(alg_for_main) > 0) {
    alg_for_main <- str_replace_all(alg_for_main, "\\bt\\b", "TIME")
  }
  if (length(alg_for_ode) > 0) {
    alg_for_ode <- str_replace_all(alg_for_ode, "\\bt\\b", "SOLVERTIME")
  }
  if (length(odes) > 0) {
    odes <- str_replace_all(odes, "\\bt\\b", "SOLVERTIME")
  }
  
  # Extract model description
  model_description <- extract_description(cleaned_text)
  description_lines <- c("$PROB", paste0("Autotranslated from monolix: ", project_path), model_description)


  library_source <- tryCatch(suppressMessages(monolix_library_dir()),
                             error = function(e) NA_character_)

  components <- list(
    structural_model_text = cleaned_text,
    odes = odes,
    ode_vars = ode_vars,
    depot_cpts = depot_info$depot_cpts,
    alg_for_ode = alg_for_ode,
    alg_for_main = alg_for_main,
    pop_params = pop_params,
    omega_params = omega_params,
    iov_param_names = iov_param_names,
    iov_level_names = iov_level_names,
    iov_param_to_level = iov_param_to_level,
    iov_occasion_values = iov_occasion_values,
    beta_params = beta_params,
    corr_params = corr_params,
    sigma_params = sigma_params,
    error_model_lines = error_model_lines,
    param_def_lines = param_def_lines,
    bioavail_lines = bioavail_lines,
    output_vars = output_vars,
    regressors = regressors,
    cont_covariate_names = cont_covariate_names,
    cat_covariate_names = cat_covariate_names,
    cat_covariate_categories = cat_covariate_categories,
    cat_covariate_all_categories = cat_covariate_all_categories,
    cat_covariate_reference = cat_covariate_reference,
    significant_cont_covariates = significant_cont_covariates,
    significant_cat_covariates = significant_cat_covariates,
    cont_covariate_params = cont_covariate_params,
    cat_covariate_params = cat_covariate_params,
    covariate_names = covariate_names,
    covariate_def_lines = covariate_def_lines,
    description_lines = description_lines,
    omega_param_lines = omega_param_lines,
    sigma_param_lines = sigma_param_lines,
    table_block = table_block,
    capture_block = capture_block,
    has_dvid = model_has_dvid,
    dvid_map = dvid_map
  )

  assembled <- assemble_monolix_model(
    components,
    regressor_values = regressor_values,
    cont_covariate_values = cont_covariate_values,
    cat_covariate_values = cat_covariate_values
  )

  list(
    code = assembled$code,
    structural_model = structural_model_text,
    library_source = library_source,
    components = c(components, list(pop_param_lines = assembled$pop_param_lines))
  )
}

#' Assemble mrgsolve model code from parsed Monolix components
#'
#' Builds the `$PARAM` block from the population, covariate effect, regressor,
#' occasion and covariate entries in `components`, then joins it with the
#' other blocks. Split out from [monolix2mrgsolve()] so covariate and
#' regressor values can be changed without re-reading the Monolix project.
#'
#' @param components The `components` element returned by
#'   [monolix2mrgsolve()].
#' @inheritParams monolix2mrgsolve
#'
#' @return A list with `code` (model lines) and `pop_param_lines` (the
#'   `$PARAM` block).
#'
#' @importFrom stringr str_remove
#' @importFrom purrr map2_chr
#' @keywords internal
assemble_monolix_model <- function(components,
                                   regressor_values = NULL,
                                   cont_covariate_values = NULL,
                                   cat_covariate_values = NULL) {
  # `components` may come from app state that also stores the *_values
  # fields; drop them so they cannot shadow the arguments.
  components <- components[setdiff(names(components),
    c("regressor_values", "cont_covariate_values", "cat_covariate_values"))]

  with(components, {
    pop_param_lines <- map2_chr(
      names(pop_params), pop_params,
      ~sprintf("%-12s : %-10g : %s population parameter", .x, .y,
               str_remove(.x, "_pop$"))
    )

    if (length(beta_params) > 0) {
      beta_param_lines <- map2_chr(
        names(beta_params), beta_params,
        ~sprintf("%-12s : %-10g : %s covariate effect", .x, .y,
                 str_remove(.x, "beta_"))
      )
      pop_param_lines <- c(pop_param_lines, beta_param_lines)
    }

    if (length(regressors) > 0) {
      reg_values <- if (!is.null(regressor_values)) regressor_values else rep(1, length(regressors))
      pop_param_lines <- c(
        pop_param_lines,
        sprintf("%-12s : %-10g : %s as regressor", regressors, reg_values, regressors)
      )
    }

    # The occasion column (headerType "occ") is not in the structural model's
    # regressor list, but the IOV cascade in $MAIN reads it directly.
    if (length(iov_level_names) > 0) {
      pop_param_lines <- c(
        pop_param_lines,
        sprintf("%-12s : %-10g : occasion indicator", iov_level_names, 1)
      )
    }

    if (length(covariate_names) > 0) {
      cont_vals <- if (!is.null(cont_covariate_values)) cont_covariate_values else rep(1, length(cont_covariate_names))
      cat_vals  <- if (!is.null(cat_covariate_values)) cat_covariate_values else rep(0, length(cat_covariate_names))
      pop_param_lines <- c(
        pop_param_lines,
        sprintf("%-12s : %-10g : %s as covariate", covariate_names,
                c(cont_vals, cat_vals), covariate_names)
      )
    }

    pop_param_lines <- c("$PARAM @annotated", pop_param_lines,
                         "DVID         : 1          : Observation ID")

    # Without ODEs (e.g. a purely algebraic model) skip $CMT and $ODE and
    # put every equation in $MAIN.
    has_odes <- length(odes) > 0 || length(ode_vars) > 0

    code <- if (has_odes) {
      c(description_lines, "",
        pop_param_lines, "",
        omega_param_lines, "",
        sigma_param_lines, "",
        c("$CMT", ode_vars, depot_cpts), "",
        "$MAIN", covariate_def_lines, param_def_lines, alg_for_main,
        bioavail_lines, "",
        "$ODE", alg_for_ode, "",
        "// Differential equations", odes, "",
        table_block, "",
        capture_block)
    } else {
      c(description_lines, "",
        pop_param_lines, "",
        omega_param_lines, "",
        sigma_param_lines, "",
        "$MAIN", covariate_def_lines, param_def_lines, alg_for_main,
        bioavail_lines, alg_for_ode, "",
        table_block, "",
        capture_block)
    }

    list(code = code, pop_param_lines = pop_param_lines)
  })
}

# Does the loaded project have an observation-ID (DVID) column?
has_dvid_column <- function() {
  tryCatch({
    data_info <- lixoftConnectors::getData()
    "obsid" %in% data_info$headerTypes
  }, error = function(e) FALSE)
}

# Extract DVID mapping from observation model.
# Uses lixoftConnectors::getObservationInformation()$mapping to get the actual dataset DVID
# value for each fitted observation variable, so mrgsolve routing/verification
# use the raw DVID==1 / DVID==3 as it appears in the data, not a resequenced
# 1/2. This matters because prepare_monolix_dataset() (vpc_utils.R) only
# recodes the DVID column to sequential position when it's a character
# column -- a numeric DVID (e.g. 2) is left as 2 unchanged, so mrgsolve_dvid
# must match that raw value or verification will filter the wrong rows.
#
# Previously this was recovered by regexing the <FIT> section out of the raw
# .mlxtran text (data = {'1','2'}, model = {y1, y2}), but that syntax is only
# used for multi-observation projects -- a single-observation project writes
# it unbraced (data = '2', model = y2), which the regex missed entirely,
# silently falling back to a sequential DVID that didn't match the dataset.
extract_dvid_mapping <- function(mlxtran_path = NULL) {
  tryCatch({
    error_model <- lixoftConnectors::getContinuousObservationModel()
    obs_names <- names(error_model$formula)

    if (is.null(obs_names) || length(obs_names) == 0) {
      return(NULL)
    }

    obs_info <- lixoftConnectors::getObservationInformation()
    mapping  <- obs_info$mapping  # named chr: names = obs_variable, value = raw dataset DVID

    data_dvid <- unname(mapping[obs_names])
    # A fitted obs var missing from mapping (shouldn't normally happen) falls
    # back to its own name as the identifier.
    missing <- is.na(data_dvid)
    data_dvid[missing] <- obs_names[missing]

    result <- data.frame(obs_variable = obs_names, dvid_identifier = data_dvid,
                          stringsAsFactors = FALSE)

    result$mrgsolve_dvid <- suppressWarnings(as.numeric(result$dvid_identifier))
    if (all(is.na(result$mrgsolve_dvid))) {
      # dvid_identifier values are non-numeric strings (e.g. "HUM_CSF_PK") -- a
      # numeric sort is meaningless here, so fall back to sequential DVIDs in
      # the current row order. This order must stay in sync with
      # dvid_identifier, since prepare_monolix_dataset() (vpc_utils.R) recodes
      # string DVID values via match(DVID, dvid_map$dvid_identifier), which
      # returns row position -- not the mrgsolve_dvid value itself.
      result$mrgsolve_dvid <- seq_len(nrow(result))
    } else {
      result <- result[order(result$mrgsolve_dvid, na.last = TRUE), ]
    }
    rownames(result) <- NULL
    result[, c("obs_variable", "dvid_identifier", "mrgsolve_dvid")]
  }, error = function(e) NULL)
}

# Parameter names influenced by a set of covariate search names.
# Underscore-delimited matching avoids substring false positives, e.g.
# searching for "GEN" must not match "tGEN" in "beta_BASE_tGEN_Female".
extract_influenced_params <- function(search_names, beta_param_names) {
  all_params <- character()
  for (sname in search_names) {
    # Match covariate name as a complete underscore-delimited segment:
    # preceded by _ and followed by _ or end of string
    pattern <- paste0("_", sname, "(_|$)")
    matching_betas <- beta_param_names[str_detect(beta_param_names, pattern)]
    if (length(matching_betas) == 0) next
    params <- sapply(matching_betas, function(beta_name) {
      match <- str_match(beta_name, "^beta_([^_]+)")
      if (!is.na(match[1, 2])) return(match[1, 2])
      return(NA)
    })
    all_params <- c(all_params, params[!is.na(params)])
  }
  unique(all_params)
}
