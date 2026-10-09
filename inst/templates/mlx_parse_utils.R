# R/mlx_parse_utils.R
# Shared Monolix structural-model (.mlxtran) text parsers.
#
# These are pure text-parsing helpers over cleaned [LONGITUDINAL] block lines
# (opts$mlxtran_lines). They are used by two independent features:
#   - R/translation_utils.R: F_/D_/ALAG_ bioavailability line generation
#     during Monolix -> mrgsolve translation (via extract_bioavailability(),
#     which wraps extract_depot_info()).
#   - R/vpc_utils_monolix.R: prepare_monolix_dataset()'s ADM->CMT mapping
#     (via extract_adm_to_target()) and Tk0/D_CMT RATE=-2 dosing detection.
# Kept in their own file (rather than living in either of the above) so
# neither feature has to depend on the other's file.

#' Return the lines of a Monolix model file starting at [LONGITUDINAL]
#'
#' @param lines Character vector. Full contents of a Monolix model file.
#'
#' @return Character vector of lines from `[LONGITUDINAL]` onward, or the
#'   whole input if `[LONGITUDINAL]` is not present.
#'
#' @importFrom stringr str_detect str_match str_trim
#'
#' @export
extract_longitudinal <- function(lines) {
  i <- which(str_detect(lines, "^\\s*\\[LONGITUDINAL\\]\\s*$"))
  if (!length(i)) return(lines)
  start <- i + 1
  lines[start:length(lines)]
}

#' Extract all lines belonging to one or more named blocks
#'
#' Find every occurrence of a `label:` header (e.g. `EQUATION:`,
#' `DEFINITION:`, `OUTPUT:`, `PK:`) and concatenate the lines under each,
#' stopping at the next block boundary. Library-model concatenation can
#' produce multiple headers with the same label; all are merged in file
#' order.
#'
#' @param lines Character vector. Model text.
#' @param label Character. Block label to search for (case-sensitive, no
#'   trailing colon).
#'
#' @return Character vector containing the merged block content, or
#'   `character(0)` if no such block is present.
#'
#' @export
get_block <- function(lines, label) {
  block_starts <- which(str_detect(lines, paste0("^\\s*", label, "\\s*:\\s*$")))
  if (!length(block_starts)) return(character())

  block_boundary_pattern <- "^\\s*(EQUATION|DEFINITION|OUTPUT|PK)\\s*:\\s*$"
  all_boundaries <- which(str_detect(lines, block_boundary_pattern))

  all_content <- character()

  for (block_start in block_starts) {
    start_line <- block_start + 1

    next_boundaries <- all_boundaries[all_boundaries > block_start]
    end_line <- if (length(next_boundaries)) min(next_boundaries) - 1 else length(lines)

    if (start_line <= end_line) {
      all_content <- c(all_content, lines[start_line:end_line])
    }
  }

  all_content
}

#' Extract a named parameter from a Monolix depot() call
#'
#' Return the value for `param_name` in a `depot(...)` call, supporting both
#' the `name = value` and bare-keyword forms (e.g. `depot(adm=5, ka)` where
#' the bare `ka` implies `ka = ka`).
#'
#' @param line Character. A single line containing a `depot(...)` call.
#' @param param_name Character. Parameter name to look up (e.g. `"ka"`,
#'   `"Tk0"`, `"Tlag"`, `"target"`).
#'
#' @return Character. The parameter value on success, the bare parameter
#'   name when supplied bare, or `NA_character_` when not present.
#'
#' @export
extract_depot_param <- function(line, param_name) {
  param_pattern <- paste0(param_name, "\\s*=\\s*")
  param_match <- str_match(line, param_pattern)

  if (!is.na(param_match[1, 1])) {
    param_start <- attr(regexpr(param_pattern, line), "match.length") +
                   regexpr(param_pattern, line)
    remaining <- substr(line, param_start, nchar(line))
    paren_depth <- 0
    param_end <- 0

    for (i in 1:nchar(remaining)) {
      char <- substr(remaining, i, i)
      if (char == "(") {
        paren_depth <- paren_depth + 1
      } else if (char == ")") {
        if (paren_depth == 0) {
          param_end <- i - 1
          break
        }
        paren_depth <- paren_depth - 1
      } else if (char == "," && paren_depth == 0) {
        param_end <- i - 1
        break
      }
    }

    if (param_end == 0) param_end <- nchar(remaining)
    param_val <- str_trim(substr(remaining, 1, param_end))
    return(param_val)
  }

  bare_pattern <- paste0("(?<=[,(])\\s*\\b", param_name, "\\b\\s*(?=[,)])")
  bare_match <- str_detect(line, bare_pattern)

  if (bare_match) {
    return(param_name)
  }

  return(NA)
}

#' Parse depot() dosing information from a Monolix PK block
#'
#' Walk every `depot(...)` call in the model's `PK:` block and produce (a)
#' the bioavailability/duration/lag lines that belong in mrgsolve's `$MAIN`,
#' (b) any synthetic depot compartment names, (c) the corresponding
#' first-order depot ODEs, and (d) the absorption terms that must be added
#' to each target compartment's ODE.
#'
#' @param clean_lines Character vector. Comment-stripped Monolix model text.
#'
#' @return A list with `bioavail_lines`, `depot_cpts`, `depot_odes`, and
#'   `target_absorption` (a named list of "absorption term" strings keyed by
#'   target compartment).
#'
#' @export
extract_depot_info <- function(clean_lines) {
  long <- extract_longitudinal(clean_lines)
  pk_block <- get_block(long, "PK")

  empty_result <- list(
    bioavail_lines = character(),
    depot_cpts = character(),
    depot_odes = character(),
    target_absorption = list()
  )

  if (!length(pk_block)) return(empty_result)

  depot_lines <- pk_block[str_detect(pk_block, "^\\s*depot\\s*\\(")]
  if (!length(depot_lines)) return(empty_result)

  bioavail_lines <- c()
  depot_cpts <- c()
  depot_odes <- c()
  target_absorption <- list()

  target_counts <- list()

  for (line in depot_lines) {
    target_match <- str_match(line, "target\\s*=\\s*([a-zA-Z0-9_]+)")
    target <- if (!is.na(target_match[1, 2])) target_match[1, 2] else NA
    if (is.na(target)) next

    ka_val  <- extract_depot_param(line, "ka")
    p_val   <- extract_depot_param(line, "p")
    tk0_val <- extract_depot_param(line, "Tk0")
    tlag_val <- extract_depot_param(line, "Tlag")

    if (is.na(p_val)) p_val <- "1"

    has_ka <- !is.na(ka_val)

    if (has_ka) {
      ## First-order absorption: need explicit depot compartment

      if (is.null(target_counts[[target]])) {
        target_counts[[target]] <- 1
        depot_name <- paste0(target, "_depot")
      } else {
        target_counts[[target]] <- target_counts[[target]] + 1
        depot_name <- paste0(target, "_depot", target_counts[[target]])
      }

      depot_cpts <- c(depot_cpts, depot_name)

      bioavail_lines <- c(bioavail_lines, sprintf("F_%s = %s;", depot_name, p_val))

      if (!is.na(tk0_val)) {
        bioavail_lines <- c(bioavail_lines, sprintf("D_%s = %s;", depot_name, tk0_val))
      }

      if (!is.na(tlag_val)) {
        bioavail_lines <- c(bioavail_lines, sprintf("ALAG_%s = %s;", depot_name, tlag_val))
      }

      depot_odes <- c(depot_odes, sprintf("dxdt_%s = -%s * %s;", depot_name, depot_name, ka_val))

      absorption_term <- sprintf("%s * %s", depot_name, ka_val)
      if (is.null(target_absorption[[target]])) {
        target_absorption[[target]] <- absorption_term
      } else {
        ## Multiple depots feeding same target: combine terms
        target_absorption[[target]] <- paste(target_absorption[[target]], "+", absorption_term)
      }

    } else {
      ## No ka: direct dosing into target (bolus or zero-order infusion).
      ## No implicit depot needed; dose goes directly to target compartment.
      bioavail_lines <- c(bioavail_lines, sprintf("F_%s = %s;", target, p_val))

      if (!is.na(tk0_val)) {
        bioavail_lines <- c(bioavail_lines, sprintf("D_%s = %s;", target, tk0_val))
      }

      if (!is.na(tlag_val)) {
        bioavail_lines <- c(bioavail_lines, sprintf("ALAG_%s = %s;", target, tlag_val))
      }
    }
  }

  list(
    bioavail_lines = bioavail_lines,
    depot_cpts = depot_cpts,
    depot_odes = depot_odes,
    target_absorption = target_absorption
  )
}
