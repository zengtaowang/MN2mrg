#' Strip a NONMEM or Monolix inline comment from a code line
#'
#' Remove everything from the first `;` or `//` to the end of the string.
#' Used as the first pass over every parsed model line before further syntax
#' translation.
#'
#' @param x Character vector of code lines.
#'
#' @return `x` with trailing comments removed.
#'
#' @importFrom stringr str_replace str_replace_all str_squish str_detect
#'   str_locate str_sub str_match str_match_all str_trim str_length
#'   str_split str_remove regex fixed
#' @importFrom purrr map_chr
#' @export
strip_comments <- function(x) {
  x <- str_replace(x, ";.*$", "")
  x <- str_replace(x, "//.*$", "")
  x
}

#' Collapse consecutive whitespace to single spaces
#'
#' Thin wrapper around [stringr::str_squish()] used to normalise whitespace
#' after multi-line equation reassembly.
#'
#' @param s Character vector.
#'
#' @return `s` with runs of internal whitespace collapsed to single spaces
#'   and leading/trailing whitespace removed.
#'
#' @export
collapse_ws <- function(s) str_squish(s)

#' Convert bare integer literals to floating-point literals
#'
#' Replace bare integer literals immediately preceding an arithmetic operator
#' with their float equivalents (`2` -> `2.0`) so that mrgsolve's C++ code
#' generator does not perform integer division. Scientific notation is
#' protected during the pass.
#'
#' @param x Character vector of code lines.
#'
#' @return `x` with bare integer literals rewritten as floats.
#'
#' @export
convert_integers_to_floats <- function(x) {
  placeholder <- "SCINUM_PLACEHOLDER"
  protected <- str_replace_all(x, "[0-9]+\\.?[0-9]*[eE][+-]?[0-9]+", function(m) {
    paste0(placeholder, m, placeholder)
  })
  converted <- str_replace_all(protected,
                               "(?<![\\w.])([0-9]+)(?![\\w.])(?=[/+\\-*()])",
                               "\\1.0")
  str_replace_all(converted, paste0(placeholder, "(.*?)", placeholder), "\\1")
}

#' Convert `^` power notation to C++ `pow(base, exp)` calls
#'
#' Iteratively rewrite `base^exp` expressions to `pow(base, exp)`, correctly
#' bracket-matching parenthesised bases and function-call bases like
#' `max(a, b)^n`.
#'
#' @param x Character. A single code line.
#'
#' @return `x` with every `^` operator rewritten as a `pow()` call.
#'
#' @export
convert_power_notation <- function(x) {
  result <- x
  while (str_detect(result, "\\^")) {
    caret_pos <- str_locate(result, "\\^")[1, "start"]
    if (is.na(caret_pos)) break

    before_caret <- substr(result, 1, caret_pos - 1)
    after_caret <- substr(result, caret_pos + 1, nchar(result))

    if (str_sub(before_caret, -1) == ")") {
      paren_count <- 1
      base_end <- nchar(before_caret) - 1
      for (i in base_end:1) {
        char <- str_sub(before_caret, i, i)
        if (char == ")") paren_count <- paren_count + 1
        if (char == "(") {
          paren_count <- paren_count - 1
          if (paren_count == 0) {
            base_start <- i
            break
          }
        }
      }
      ## If the "(" belongs to a function call (e.g. max(a,b)^n), the function
      ## name is part of the power base -- pow(max(a,b), n) -- not split off
      ## into before_base, which would otherwise produce a mangled token like
      ## "maxpow((a,b), n)" for max/min/sin/exp/etc. used as a power base.
      fn_match <- str_match(str_sub(before_caret, 1, base_start - 1), "([a-zA-Z_][a-zA-Z0-9_]*)$")
      if (!is.na(fn_match[1, 2])) {
        base_start <- base_start - nchar(fn_match[1, 2])
      }
      base <- str_sub(before_caret, base_start, base_end + 1)
      before_base <- str_sub(before_caret, 1, base_start - 1)
    } else {
      match <- str_match(before_caret, "([a-zA-Z_][a-zA-Z0-9_]*)$|([0-9]+\\.?[0-9]*)$")
      if (!is.na(match[1, 2])) {
        base <- match[1, 2]
        before_base <- str_sub(before_caret, 1, nchar(before_caret) - nchar(base))
      } else if (!is.na(match[1, 3])) {
        base <- match[1, 3]
        before_base <- str_sub(before_caret, 1, nchar(before_caret) - nchar(base))
      } else {
        break
      }
    }

    exp_match <- str_match(after_caret, "^([a-zA-Z_][a-zA-Z0-9_]*|\\d+\\.?\\d*|\\([^)]*\\))")
    if (is.na(exp_match[1, 1])) break

    exponent <- exp_match[1, 1]
    after_exp <- str_sub(after_caret, nchar(exponent) + 1, nchar(after_caret))
    result <- paste0(before_base, "pow(", base, ", ", exponent, ")", after_exp)
  }
  result
}

#' Convert Monolix `rem(t, tau)` to a periodic-remainder expression
#'
#' Rewrite `rem(t, tau)` calls (Monolix's continuous modulo helper for
#' periodic dosing surrogates) into the equivalent
#' `tau * (SOLVERTIME/tau - floor(SOLVERTIME/tau))` mrgsolve idiom.
#'
#' @param x Character. A single code line.
#'
#' @return `x` with `rem(...)` calls rewritten.
#'
#' @export
convert_rem_notation <- function(x) {
  result <- str_replace_all(x,
                           "rem\\s*\\(\\s*t\\s*,\\s*([0-9.]+)\\s*\\)",
                           "\\1 * (SOLVERTIME/\\1 - floor(SOLVERTIME/\\1))")
  result
}

#' Convert `min()` and `max()` calls to C++ `fmin()` and `fmax()`
#'
#' @param x Character vector of code lines.
#'
#' @return `x` with `min(...)` and `max(...)` calls rewritten to their C++
#'   floating-point equivalents.
#'
#' @export
convert_minmax_notation <- function(x) {
  result <- str_replace_all(x, "\\bmax\\s*\\(", "fmax(")
  result <- str_replace_all(result, "\\bmin\\s*\\(", "fmin(")
  result
}

## Legacy C/C++ math.h global functions (declared via <bits/mathcalls.h>,
## pulled in transitively by mrgsolve headers) that collide with a bare
## Monolix parameter/variable of the same name (e.g. a Hill coefficient
## literally named "gamma"), making `double gamma = ...;` ambiguous.
## NOTE: deliberately excludes y0/y1/j0/j1 -- those are common Monolix
## observation-variable names (e.g. multi-DVID models use y1/y2/y3) and the
## app's DVID routing logic depends on those exact names surviving untouched.
RESERVED_MATH_NAMES <- c(
  "gamma", "tgamma", "lgamma",
  "erf", "erfc", "significand", "drem", "scalb", "scalbn"
)

#' Rename math.h reserved-name collisions to a safe alias
#'
#' Rename bare occurrences of C math.h names (e.g. `gamma`, `erf`) that would
#' collide with global C++ functions pulled in by mrgsolve headers, appending
#' `_p` (for "parameter") to disambiguate. Word-boundary matching leaves
#' suffixed forms like `gamma_pop` or `omega_gamma` untouched.
#'
#' @param x Character vector of code lines.
#'
#' @return `x` with reserved-name collisions renamed.
#'
#' @export
sanitize_reserved_names <- function(x) {
  result <- x
  for (nm in RESERVED_MATH_NAMES) {
    result <- str_replace_all(result, paste0("\\b", nm, "\\b"), paste0(nm, "_p"))
  }
  result
}

#' Convert `[COVAR = VALUE]` categorical-covariate bracket syntax to conditionals
#'
#' Replace Monolix bracket-notation categorical covariate references with a
#' `(COVAR == n)` boolean expression, encoding each distinct value as a
#' sequential integer. Multiple values for the same covariate are numbered in
#' first-seen order.
#'
#' @param x Character. A single code line.
#'
#' @return `x` with `[COVAR = VALUE]` occurrences rewritten to
#'   `(COVAR == n)`.
#'
#' @export
convert_categorical_covariate <- function(x) {
  patterns <- str_match_all(x, "\\[([a-zA-Z0-9_]+)\\s*=\\s*([^\\]]+?)\\s*\\]")[[1]]
  if (nrow(patterns) == 0) return(x)

  covar_map <- list()
  for (i in seq_len(nrow(patterns))) {
    full_match <- patterns[i, 1]
    covar_name <- patterns[i, 2]
    covar_value <- str_trim(patterns[i, 3])

    if (!covar_name %in% names(covar_map)) {
      covar_map[[covar_name]] <- list(values = character(), next_num = 1)
    }
    if (!covar_value %in% covar_map[[covar_name]]$values) {
      covar_map[[covar_name]]$values <- c(covar_map[[covar_name]]$values, covar_value)
    }

    value_num <- which(covar_map[[covar_name]]$values == covar_value)
    replacement <- paste0("(", covar_name, "==", value_num, ")")
    x <- str_replace(x, fixed(full_match), replacement)
  }
  x
}

#' Deduplicate `double var = ...;` equation lines
#'
#' Keep only the first definition of each named variable in an equations
#' vector, dropping any later re-definition. Non-`double`-prefixed lines pass
#' through unchanged.
#'
#' @param equations Character vector of `double var = ...;` equation lines,
#'   possibly interleaved with non-equation lines.
#'
#' @return `equations` with duplicate variable definitions removed.
#'
#' @export
deduplicate_equations <- function(equations) {
  if (is.null(equations) || length(equations) == 0) return(equations)

  seen_vars <- character()
  unique_equations <- character()

  for (eq in equations) {
    var_match <- str_match(eq, "^\\s*double\\s+([a-zA-Z0-9_]+)\\s*=")
    if (!is.na(var_match[1, 2])) {
      var_name <- var_match[1, 2]
      if (!(var_name %in% seen_vars)) {
        seen_vars <- c(seen_vars, var_name)
        unique_equations <- c(unique_equations, eq)
      }
    } else {
      unique_equations <- c(unique_equations, eq)
    }
  }
  unique_equations
}

#' Identify algebraic variables that depend on time
#'
#' Starting from a known-time-varying seed (`SOLVERTIME` and the ODE state
#' variables), iteratively propagate time-varying-ness across a set of
#' algebraic definitions and return the indices of definitions that must
#' therefore live inside the ODE block rather than in `$MAIN`.
#'
#' @param algebraics Character vector of algebraic equations
#'   (`double var = expr;`).
#' @param ode_vars Character vector of ODE state variable names (e.g. the
#'   names on the left-hand side of `dxdt_` lines).
#'
#' @return A list with `indices` (integer positions in `algebraics` that
#'   should move to the ODE block) and `time_varying_vars` (the full closure
#'   of variables identified as time-dependent).
#'
#' @export
identify_time_varying_vars <- function(algebraics, ode_vars) {
  time_varying <- c("SOLVERTIME")

  time_varying <- c(time_varying, ode_vars)

  var_names <- str_match(algebraics, "^double\\s+([a-zA-Z0-9_]+)\\s*=")[, 2]
  var_names <- var_names[!is.na(var_names)]

  max_iterations <- 20
  for (iter in 1:max_iterations) {
    old_time_varying <- time_varying

    for (i in seq_along(algebraics)) {
      eq <- algebraics[i]
      var_name <- var_names[i]

      if (is.na(var_name)) next

      contains_time_var <- any(str_detect(eq, paste0("\\b", time_varying, "\\b")))

      if (contains_time_var && !(var_name %in% time_varying)) {
        time_varying <- c(time_varying, var_name)
      }
    }

    if (length(time_varying) == length(old_time_varying)) break
  }

  indices <- sapply(seq_along(algebraics), function(i) {
    eq <- algebraics[i]
    var_name <- var_names[i]

    is_time_varying <- var_name %in% time_varying
    contains_time_var <- any(str_detect(eq, paste0("\\b", time_varying, "\\b")))

    is_time_varying || contains_time_var
  })

  list(
    indices = which(indices),
    time_varying_vars = time_varying
  )
}

#' Test whether an if-block references any of a set of variables
#'
#' Used when deciding whether an if/elseif/else block belongs in the ODE
#' block (because it touches a time-varying variable).
#'
#' @param block_lines Character vector. Lines making up a single if-block.
#' @param vars Character vector. Variable names to search for.
#'
#' @return `TRUE` if any name in `vars` appears (word-bounded) inside
#'   `block_lines`, `FALSE` otherwise.
#'
#' @export
is_if_block_with_var <- function(block_lines, vars) {
  if (!length(vars)) return(FALSE)
  clean_lines <- str_remove(block_lines, "^double\\s+")
  block_text <- paste(clean_lines, collapse = " ")
  any(str_detect(block_text, paste0("\\b", vars, "\\b")))
}

#' Extract the DESCRIPTION section from a Monolix model file
#'
#' Return the text on and after `DESCRIPTION:` up to (but not including) the
#' `[LONGITUDINAL]` header, dropping blank lines.
#'
#' @param lines Character vector. Lines of a Monolix model file.
#'
#' @return Character vector of description lines, or `character(0)` if no
#'   `DESCRIPTION:` header is present.
#'
#' @export
extract_description <- function(lines) {
  i <- which(str_detect(lines, regex("^\\s*DESCRIPTION\\s*:", ignore_case = TRUE)))[1]
  if (is.na(i)) return(character())
  inline <- str_match(lines[i], "^\\s*DESCRIPTION\\s*:\\s*(.*)$")[, 2]
  inline <- if (!is.na(inline) && nchar(inline)) inline else NULL
  j <- which(str_detect(lines, "^\\s*\\[LONGITUDINAL\\]\\s*$"))[1]
  if (is.na(j)) j <- length(lines) + 1
  after <- if (j > i + 1) lines[(i + 1):(j - 1)] else character()
  out <- c(inline, after)
  out <- out[nchar(str_squish(out)) > 0]
  out
}

# extract_longitudinal(), get_block(), extract_depot_param(), and
# extract_depot_info() now live in R/mlx_parse_utils.R (shared with
# R/vpc_utils_monolix.R). In the installed package they resolve through
# the same NAMESPACE.

#' Legacy wrapper returning only the bioavailability lines from depot info
#'
#' Compatibility shim for callers that only need the F_/D_/ALAG_ lines from
#' [extract_depot_info()].
#'
#' @param clean_lines Character vector. Comment-stripped Monolix model text.
#'
#' @return Character vector of bioavailability/duration/lag lines.
#'
#' @export
extract_bioavailability <- function(clean_lines) {
  info <- extract_depot_info(clean_lines)
  info$bioavail_lines
}

#' Extract ODE definitions from a Monolix EQUATION block
#'
#' Return the concatenated `ddt_var = ...` (or its continuation-line
#' extensions) definitions from the EQUATION: block, rewritten with the
#' mrgsolve prefix `dxdt_` and stripped of Monolix statement terminators.
#'
#' @param clean_lines Character vector. Comment-stripped Monolix model text.
#'
#' @return Character vector of mrgsolve-syntax ODE lines.
#'
#' @export
extract_odes <- function(clean_lines) {
  long  <- extract_longitudinal(clean_lines)
  eqblk <- get_block(long, "EQUATION")
  if (!length(eqblk)) stop("No EQUATION: block found.")
  eqblk <- str_trim(eqblk)
  eqblk <- eqblk[str_length(eqblk) > 0]
  eqblk <- eqblk[!str_detect(eqblk, "(?i)^odeType\\s*=")]

  is_ode_start <- str_detect(eqblk, "^ddt_[a-zA-Z0-9_]+\\s*=")

  is_algebraic_start <- str_detect(eqblk, "^[a-zA-Z_][a-zA-Z0-9_]*\\s*=") & !is_ode_start

  combined_odes <- character()
  current_ode <- ""

  for (i in seq_along(eqblk)) {
    line <- eqblk[i]

    if (is_ode_start[i]) {
      if (nchar(current_ode) > 0) {
        combined_odes <- c(combined_odes, current_ode)
      }
      current_ode <- line
    } else if (is_algebraic_start[i]) {
      if (nchar(current_ode) > 0) {
        combined_odes <- c(combined_odes, current_ode)
        current_ode <- ""
      }
    } else {
      if (nchar(current_ode) > 0) {
        current_ode <- paste(current_ode, line)
      }
    }
  }

  if (nchar(current_ode) > 0) {
    combined_odes <- c(combined_odes, current_ode)
  }

  odes <- combined_odes
  odes <- str_replace(odes, ";\\s*$", "")
  odes <- str_squish(odes)
  odes <- str_replace(odes, "^ddt_", "dxdt_")
  odes
}

#' Extract non-ODE algebraic definitions from a Monolix model
#'
#' Return `double var = ...;` lines drawn from EQUATION:, DEFINITION:, and
#' PK: (excluding `depot(...)` calls), correctly combining multi-line
#' algebraic definitions while dropping continuation lines that belong to
#' an ODE.
#'
#' @param clean_lines Character vector. Comment-stripped Monolix model text.
#'
#' @return Character vector of `double var = ...;` lines suitable for
#'   emission into mrgsolve's `$MAIN` block.
#'
#' @export
extract_algebraic_defs <- function(clean_lines) {
  long <- extract_longitudinal(clean_lines)
  eq   <- get_block(long, "EQUATION")
  def  <- get_block(long, "DEFINITION")
  pk   <- get_block(long, "PK")

  pk_algebraic <- character()
  if (length(pk) > 0) {
    pk_algebraic <- pk[!str_detect(pk, "^\\s*depot\\s*\\(")]
    pk_algebraic <- pk_algebraic[nchar(str_trim(pk_algebraic)) > 0]
  }

  blk <- c(eq, def, pk_algebraic)
  if (!length(blk)) stop("No EQUATION:/DEFINITION:/PK blocks with algebraic equations.")
  blk <- trimws(blk)
  blk <- blk[nchar(blk) > 0]
  blk <- sub(";\\s*$", "", blk)

  in_multiline_ode <- FALSE
  in_multiline_alg <- FALSE
  keep_line <- logical(length(blk))

  for (i in seq_along(blk)) {
    line <- blk[i]

    is_ode_type <- str_detect(line, regex("^odeType\\s*=", ignore_case = TRUE))

    is_ode_start <- str_detect(line, regex("^ddt[_]?[A-Za-z0-9_]*\\s*=", ignore_case = TRUE))

    is_algebraic_start <- str_detect(line, "^[a-zA-Z_][a-zA-Z0-9_]*\\s*=") && !is_ode_start

    is_control_flow <- str_detect(line, "^(if|elseif|else|end)(\\s|$)")

    is_equation_start <- is_ode_start || is_algebraic_start || is_ode_type || is_control_flow

    if (is_ode_type) {
      keep_line[i] <- FALSE
      in_multiline_ode <- FALSE
      in_multiline_alg <- FALSE
    } else if (is_ode_start) {
      keep_line[i] <- FALSE
      in_multiline_ode <- TRUE
      in_multiline_alg <- FALSE
    } else if (is_algebraic_start || is_control_flow) {
      keep_line[i] <- TRUE
      in_multiline_ode <- FALSE
      in_multiline_alg <- !is_control_flow
    } else {
      if (in_multiline_ode) {
        keep_line[i] <- FALSE
      } else if (in_multiline_alg) {
        keep_line[i] <- TRUE
      } else {
        keep_line[i] <- TRUE
      }
    }
  }

  blk <- blk[keep_line]

  combined_alg <- character()
  current_alg <- ""

  for (line in blk) {
    is_control_flow <- str_detect(line, "^(if|elseif|else|end)(\\s|$)")

    is_new_equation <- str_detect(line, "^[a-zA-Z_][a-zA-Z0-9_]*\\s*=")

    if (is_control_flow || is_new_equation) {
      if (nchar(current_alg) > 0) {
        combined_alg <- c(combined_alg, current_alg)
      }
      current_alg <- line
    } else {
      if (nchar(current_alg) > 0) {
        current_alg <- paste(current_alg, line)
      } else {
        current_alg <- line
      }
    }
  }

  if (nchar(current_alg) > 0) {
    combined_alg <- c(combined_alg, current_alg)
  }

  alg <- str_squish(combined_alg)
  paste0("double ", alg)
}

#' Convert Monolix if/elseif/else/end syntax to C++ braces
#'
#' Rewrite Monolix's Fortran-flavoured conditional syntax into C++ curly-brace
#' form and add trailing semicolons on statements inside the block.
#'
#' @param lines Character vector of code lines.
#'
#' @return Character vector with `if ...`, `elseif ...`, `else`, `end`
#'   translated to `if (...) {`, `} else if (...) {`, `} else {`, and `}`.
#'
#' @export
convert_if_block <- function(lines) {
  out <- character()
  in_if <- FALSE
  for (line in lines) {
    l <- str_remove(line, "^double\\s+")
    l <- str_squish(l)

    if (str_detect(l, "^if\\s+")) {
      cond <- str_remove(l, "^if\\s+")
      out <- c(out, paste0("if(", cond, "){"))
      in_if <- TRUE
    } else if (str_detect(l, "^elseif\\s+")) {
      cond <- str_remove(l, "^elseif\\s+")
      out <- c(out, paste0("} else if(", cond, "){"))
    } else if (str_detect(l, "^else\\s*$")) {
      out <- c(out, "} else {")
    } else if (str_detect(l, "^end\\s*$")) {
      out <- c(out, "}")
      in_if <- FALSE
    } else if (in_if) {
      stmt <- str_remove(line, "^double\\s+")
      out <- c(out, paste0(stmt, ";"))
    } else {
      out <- c(out, line)
    }
  }
  out
}

#' Extract the OUTPUT block of a Monolix model
#'
#' @param clean_lines Character vector. Comment-stripped Monolix model text.
#'
#' @return Character vector of OUTPUT: block content, or `character(0)` if
#'   no OUTPUT block exists.
#'
#' @export
extract_output_block <- function(clean_lines) {
  long <- extract_longitudinal(clean_lines)
  out  <- get_block(long, "OUTPUT")
  if (!length(out)) return(character())
  out
}

#' Extract regressor variable names from a Monolix model
#'
#' Locate all lines that mention `regressor` and return the (unique) names
#' on the left-hand side of the assignment.
#'
#' @param clean_lines Character vector. Comment-stripped Monolix model text.
#'
#' @return Character vector of regressor variable names.
#'
#' @export
extract_regressors <- function(clean_lines) {
  long <- extract_longitudinal(clean_lines)
  reg_lines <- long[str_detect(long, "regressor")]
  if (!length(reg_lines)) return(character())
  names <- str_match(reg_lines, "^\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*=")[, 2]
  names <- names[!is.na(names)]
  unique(names)
}

#' Split Monolix dataset headers into continuous and categorical covariates
#'
#' Given a Monolix `getData()`-style list containing `header` and
#' `headerTypes` fields, return the header names that carry `contcov` and
#' `catcov` types.
#'
#' @param data_info List with `header` and `headerTypes` character vectors of
#'   equal length (as returned by `lixoftConnectors::getData()`).
#'
#' @return List with `continuous` and `categorical` character vectors of
#'   column names.
#'
#' @export
extract_covariates <- function(data_info) {
  headers <- data_info$header
  header_types <- data_info$headerTypes
  cont_cov_idx <- which(header_types == "contcov")
  cat_cov_idx <- which(header_types == "catcov")
  list(continuous = headers[cont_cov_idx], categorical = headers[cat_cov_idx])
}

#' Convert Monolix covariate formula strings to C++ `double var = ...;` lines
#'
#' Accept the covariate-formula container (character, list, or comma-joined
#' character), skip transformed categorical covariates (handled separately),
#' and emit one `double var = ...;` line per formula with `pow()`, float
#' literals, `fmin`/`fmax`, and categorical-bracket substitutions applied.
#'
#' @param covariate_formulas Character, character vector, or named list of
#'   covariate formulas (as produced by the Monolix parser).
#'
#' @return Character vector of `double var = expr;` lines suitable for
#'   emission into mrgsolve's `$MAIN` block.
#'
#' @export
convert_covariate_formulas <- function(covariate_formulas) {
  if (!length(covariate_formulas) || is.null(covariate_formulas)) return(character())

  if (length(covariate_formulas) == 1 && is.character(covariate_formulas)) {
    if (str_detect(covariate_formulas, ",")) {
      formulas_list <- str_split(covariate_formulas, ",")[[1]]
      formulas_list <- str_trim(formulas_list)
    } else {
      formulas_list <- covariate_formulas
    }
  } else if (is.list(covariate_formulas)) {
    formulas_list <- character()
    for (name in names(covariate_formulas)) {
      formula_item <- covariate_formulas[[name]]
      if (is.list(formula_item) &&
          !is.null(formula_item$reference) &&
          !is.null(formula_item$from) &&
          !is.null(formula_item$transformed)) {
        next
      } else if (is.character(formula_item)) {
        formulas_list <- c(formulas_list, formula_item)
      }
    }
  } else {
    formulas_list <- as.character(covariate_formulas)
  }

  if (length(formulas_list) == 0) return(character())

  covariate_def_lines <- map_chr(formulas_list, function(formula) {
    formula_trimmed <- str_trim(formula)
    if (nchar(formula_trimmed) == 0) return(NULL)

    first_eq_pos <- str_locate(formula_trimmed, "=")[1]
    if (is.na(first_eq_pos)) return(paste0("double ", formula_trimmed, ";"))

    right_side <- str_trim(str_sub(formula_trimmed, first_eq_pos + 1))
    right_side <- convert_power_notation(right_side)
    right_side <- convert_integers_to_floats(right_side)
    right_side <- convert_minmax_notation(right_side)
    right_side <- convert_categorical_covariate(right_side)

    var_match <- str_match(formula_trimmed, "^([a-zA-Z0-9_]+)\\s*=")
    var_name <- if (!is.na(var_match[1, 2])) var_match[1, 2] else "covariate"
    paste0("double ", var_name, " = ", right_side, ";")
  })

  covariate_def_lines <- covariate_def_lines[!sapply(covariate_def_lines, is.null)]
  covariate_def_lines
}

#' Extract transformed-categorical-covariate metadata
#'
#' Given the covariate-formula container from a Monolix parse, return
#' structured information about transformed categorical covariates:
#' reference levels, source columns, all categories, and category mappings.
#'
#' @param covariate_formulas Named list of covariate-formula items.
#'
#' @return List with `names`, `references`, `all_categories`,
#'   `original_covariates`, and `category_mapping` sub-lists.
#'
#' @export
extract_transformed_categorical_covariates <- function(covariate_formulas) {
  if (!is.list(covariate_formulas) || length(covariate_formulas) == 0) {
    return(list(
      names = character(),
      references = list(),
      all_categories = list(),
      original_covariates = list(),
      category_mapping = list()
    ))
  }

  transformed_names <- character()
  references <- list()
  all_categories <- list()
  original_covariates <- list()
  category_mapping <- list()

  for (name in names(covariate_formulas)) {
    formula_item <- covariate_formulas[[name]]

    if (is.list(formula_item) &&
        !is.null(formula_item$reference) &&
        !is.null(formula_item$from) &&
        !is.null(formula_item$transformed)) {

      transformed_names <- c(transformed_names, name)
      references[[name]] <- formula_item$reference
      original_covariates[[name]] <- formula_item$from

      if (is.list(formula_item$transformed)) {
        category_names <- names(formula_item$transformed)
        all_categories[[name]] <- category_names
        category_mapping[[name]] <- formula_item$transformed
      }
    }
  }

  return(list(
    names = transformed_names,
    references = references,
    all_categories = all_categories,
    original_covariates = original_covariates,
    category_mapping = category_mapping
  ))
}

#' Locate the Monolix library-model directory
#'
#' Resolve the directory holding the Monolix PK library structural models:
#' the `.txt` files that a `lib:` reference in a `.mlxtran` points at, and
#' the pool that [read_structural_model()] searches when a model uses
#' `iv()`/`oral()`/`absorption()` macros.
#'
#' Sources are tried in this order, and the first that exists and holds at
#' least one `.txt` file wins:
#'
#' 1. `getOption("MN2mrg.monolix_library_path")`.
#' 2. The `MN2MRG_MONOLIX_LIBRARY` environment variable.
#' 3. The copy shipped with this package at
#'    `inst/extdata/monolix_library_models/`, which works on any machine
#'    with no configuration at all.
#'
#' Explicit configuration deliberately outranks the shipped copy. The
#' alternative order would make the option and the environment variable dead
#' letters now that the package carries its own models: a site pointing at a
#' validated snapshot of a different MonolixSuite version would silently get
#' the bundled files instead, which is the kind of right-answer-wrong-source
#' failure that is invisible in the output.
#'
#' Set `options(MN2mrg.monolix_library_packaged = "")` to take the
#' shipped copy out of consideration, for a site whose policy is that only an
#' explicitly declared snapshot may be used. The test suite uses the same
#' seam to exercise the configured sources and the no-source error.
#'
#' There is no silent fallback. When nothing resolves, the error names every
#' location tried, so a failure is diagnosable from the message alone. The
#' resolved source is announced with [message()] so that a translation log
#' records which pool the structural model actually came from, rather than
#' only that one was found.
#'
#' @return Character. Path to an existing directory holding at least one
#'   `.txt` library model.
#'
#' @export
monolix_library_dir <- function() {
  candidates <- c(
    "option MN2mrg.monolix_library_path" =
      getOption("MN2mrg.monolix_library_path", ""),
    "environment variable MN2MRG_MONOLIX_LIBRARY" =
      Sys.getenv("MN2MRG_MONOLIX_LIBRARY", ""),
    "packaged copy (inst/extdata/monolix_library_models)" =
      getOption("MN2mrg.monolix_library_packaged",
                system.file("extdata", "monolix_library_models",
                            package = "MN2mrg"))
  )

  for (source in names(candidates)) {
    path <- candidates[[source]]
    if (!nzchar(path) || !dir.exists(path)) next
    if (length(list.files(path, pattern = "\\.txt$")) == 0L) next
    message("[MN2mrg] Monolix library models resolved from ",
            source, ": ", path)
    return(path)
  }

  stop("No Monolix library-model directory could be resolved. Tried:\n",
       paste0("  - ", names(candidates), ": ",
              ifelse(nzchar(candidates), candidates, "(not set)"),
              collapse = "\n"),
       "\nSet options(MN2mrg.monolix_library_path = ...) or the ",
       "MN2MRG_MONOLIX_LIBRARY environment variable to a directory ",
       "containing the Monolix library-model .txt files.",
       call. = FALSE)
}

#' Resolve a Monolix `lib:` structural-model reference to a file path
#'
#' Turn the `lib:oral1_1cpt_kaVCl.txt` form returned by
#' `lixoftConnectors::getStructuralModel()` into an absolute path inside the
#' directory from [monolix_library_dir()]. Any path that is not a `lib:`
#' reference is returned unchanged, so callers can hand this every
#' structural-model path without branching first.
#'
#' This replaces two divergent and independently broken schemes: a hardcoded
#' internal path in this file, which worked only on the network,
#' and a `LIB_MODEL_STRUCTURAL/` placeholder in `inst/shiny/app.R`, which was
#' never wired to any real location and so resolved nowhere at all.
#'
#' @param structural_model_path Character. A structural-model path, or a
#'   `lib:` reference.
#'
#' @return Character. Absolute path to the library model file for a `lib:`
#'   reference; `structural_model_path` unchanged otherwise.
#'
#' @export
resolve_lib_model_path <- function(structural_model_path) {
  if (!is.character(structural_model_path) ||
      length(structural_model_path) != 1L ||
      is.na(structural_model_path)) {
    stop("`structural_model_path` must be a single character path.",
         call. = FALSE)
  }
  if (!grepl("^lib:", structural_model_path)) return(structural_model_path)

  model_file <- sub("^lib:", "", structural_model_path)
  path       <- file.path(monolix_library_dir(), model_file)

  # The previous code read this path with no existence check, so a missing
  # library model surfaced as readLines()'s generic "cannot open the
  # connection" rather than naming the model.
  if (!file.exists(path)) {
    stop("Monolix library model '", model_file, "' not found at ", path,
         ". The library-model directory resolved, but does not contain ",
         "this model.", call. = FALSE)
  }
  path
}

#' Read a Monolix structural model, expanding library references
#'
#' Load the file at `structural_model_path`. If it uses `iv()`/`oral()`/
#' `absorption()`/`compartment()` macros, extract the parameter set, search
#' the on-disk Monolix library-models directory for a match, and splice in
#' the matched library model in place of the macros (rewriting the macros
#' as `depot(...)` calls). If the model is a `lib:` reference, read the
#' library file directly.
#'
#' @param structural_model_path Character. Path to a Monolix structural
#'   model file, or a `lib:...` reference into the shared library-models
#'   directory.
#'
#' @return Character vector of expanded model source lines.
#'
#' @export
read_structural_model <- function(structural_model_path) {
  valid_params <- c("Tk0", "Tlag", "ka",  "k", "Vm", "Km",
                    "k12", "k21", "k13", "k31",
                    "V", "V1", "V2", "V3",
                    "Cl", "Q", "Q2", "Q3",
                    "Mtt", "Ktr")

  structural_keywords <- c("compartment", "iv", "oral", "peripheral",
                           "elimination", "pkmodel")

  ## Create pattern: keywords followed by ( - matches actual macro calls like
  ## compartment(). This ensures we only match function calls, not just the
  ## word appearing in text.
  macro_pattern <- paste0("\\b(", paste(structural_keywords, collapse = "|"), ")\\s*\\(")

  extract_matched_params <- function(lines) {
    paren_contents <- regmatches(lines, gregexpr("\\(([^)]+)\\)", lines))
    all_paren_text <- gsub("^\\(|\\)$", "", unlist(paren_contents))
    tokens <- trimws(unlist(strsplit(all_paren_text, ",")))

    extracted_names <- unlist(lapply(tokens, function(t) {
      if (grepl("=", t)) {
        trimws(strsplit(t, "=")[[1]])
      } else {
        trimws(t)
      }
    }))

    sort(unique(valid_params[valid_params %in% extracted_names]))
  }

  parse_params <- function(param_string) {
    params <- c()
    remaining <- param_string

    while (nchar(remaining) > 0) {
      matched <- FALSE
      for (param in valid_params[order(-nchar(valid_params))]) {
        if (startsWith(remaining, param)) {
          params <- c(params, param)
          remaining <- substring(remaining, nchar(param) + 1)
          matched <- TRUE
          break
        }
      }
      if (!matched) {
        warning(paste("Could not parse parameter string:", param_string))
        return(NULL)
      }
    }
    return(params)
  }

  ## Transform iv(), oral(), absorption() macros to depot() calls.
  ## Example: oral(adm = 2, cmt = 1, Tlag, ka) -> depot(adm = 2, target = Ac, Tlag, ka)
  transform_to_depot <- function(lines) {
    depot_lines <- character()

    for (line in lines) {
      if (grepl("\\b(iv|oral|absorption)\\s*\\(", line)) {
        transformed <- str_replace(line, "\\biv\\s*\\(", "depot(")
        transformed <- str_replace(transformed, "\\boral\\s*\\(", "depot(")
        transformed <- str_replace(transformed, "\\babsorption\\s*\\(", "depot(")

        transformed <- str_replace_all(transformed, "\\bcmt\\s*=\\s*[a-zA-Z0-9_]+", "target = Ac")

        depot_lines <- c(depot_lines, transformed)
      }
    }

    return(depot_lines)
  }

  if (grepl("^lib:", structural_model_path)) {
    readLines(resolve_lib_model_path(structural_model_path))
  } else {
    model <- readLines(structural_model_path)
    model <- strip_comments(model)
    longitudinal <- extract_longitudinal(model)
    has_pkmodel <- any(grepl(macro_pattern, longitudinal))

    if (has_pkmodel) {
      structural_lines <- model[grepl(macro_pattern, model)]

      user_params <- if (length(structural_lines) > 0) {
        extract_matched_params(structural_lines)
      } else {
        NULL
      }

      if (is.null(user_params) || length(user_params) == 0) {
        input_line <- model[grepl("input\\s*=", model, ignore.case = TRUE)]
        if (length(input_line) > 0) {
          input_params <- gsub(".*\\{\\s*(.*)\\s*\\}.*", "\\1", input_line[1])
          user_params <- sort(trimws(strsplit(input_params, ",")[[1]]))
        }
      }

      if (is.null(user_params) || length(user_params) == 0) {
        warning("Structural keywords found but no parameters could be extracted")
        return(model)
      }

      # monolix_library_dir() errors when nothing resolves.
      # So the match below simply found nothing and the function
      # returned the structural model unmodified, with the macros never
      # expanded, behind nothing louder than a warning.
      all_models <- list.files(monolix_library_dir(),
                               pattern = "\\.txt$", full.names = TRUE)

      matched_file <- NULL
      for (lib_file in all_models) {
        filename_no_ext <- gsub("\\.txt$", "", basename(lib_file))
        parts <- strsplit(filename_no_ext, "_")[[1]]
        param_string <- parts[length(parts)]

        lib_params <- parse_params(param_string)

        if (!is.null(lib_params)) {
          if (length(user_params) == length(sort(lib_params)) &&
              all(user_params == sort(lib_params))) {
            matched_file <- lib_file
            break
          }
        }
      }

      if (!is.null(matched_file)) {
        structural_line_idx <- which(grepl(macro_pattern, model))

        structural_block_lines <- model[structural_line_idx]
        depot_lines <- transform_to_depot(structural_block_lines)

        lib_model <- readLines(matched_file)

        has_ka_in_depot <- any(grepl("\\bka\\b", depot_lines))

        if (has_ka_in_depot) {
          lib_model <- lib_model[!grepl("^\\s*ddt_Ad\\s*=|^\\s*dxdt_Ad\\s*=", lib_model)]

          lib_model <- gsub("\\s*[-+]\\s*ka\\s*\\*\\s*Ad\\b|\\bka\\s*\\*\\s*Ad\\b", "", lib_model)
        }

        pk_line_idx <- which(grepl("^\\s*PK\\s*:\\s*$", model))

        if (length(pk_line_idx) > 0) {
          before_pk <- model[1:pk_line_idx[1]]
          after_pk <- model[(pk_line_idx[1] + 1):length(model)]

          after_pk_filtered <- after_pk[!grepl(macro_pattern, after_pk)]

          new_model <- c(before_pk,
                         depot_lines,
                         lib_model,
                         after_pk_filtered)
        } else {
          new_model <- c(model[1:(min(structural_line_idx) - 1)],
                         lib_model,
                         model[(max(structural_line_idx) + 1):length(model)])
        }

        return(new_model)
      } else {
        # Previously a warning() followed by `return(model)`. That returned
        # the structural model with its iv()/oral()/absorption() macros
        # intact and unexpanded, i.e. a silently wrong translation that
        # still looked like a success. Fail instead.
        stop("No Monolix library model matches the parameter set: ",
             paste(user_params, collapse = ", "), ".\n",
             "  Searched ", length(all_models), " model(s) in ",
             monolix_library_dir(), ".\n",
             "  The structural model uses iv()/oral()/absorption() macros ",
             "that cannot be expanded without a match, so translation ",
             "cannot continue.", call. = FALSE)
      }

    } else {
      return(model)
    }
  }
}
