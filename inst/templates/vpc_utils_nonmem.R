# R/vpc_utils_nonmem.R
# NONMEM-specific $DATA IGNORE= parsing utilities for the VPC tab.
# Standalone: no dependency on R/vpc_utils_common.R or the Monolix files.

#' Parse IGNORE statements from a NONMEM $DATA block
#'
#' Extract all `IGNORE=` directives from the `$DATA` block of a NONMEM
#' control stream. Inline comments (after `;`) are stripped and fully
#' commented lines are skipped before parsing. Comma-separated
#' `IGNORE=(cond1, cond2)` forms yield one condition per element.
#'
#' @param ctl_lines Character vector. Lines of the NONMEM control file.
#'
#' @return List with three elements: `conditions` (character vector of
#'   negated R filter expressions such as `"!(CMT > 2)"` applied row-wise
#'   after the data is parsed; wrapped in `dplyr::coalesce(..., FALSE)` for
#'   NA-safety), `ignore_chars` (character vector of leading-character
#'   comment markers to drop before parsing), and `ignore_nonnumeric`
#'   (logical; `TRUE` when `IGNORE=@` is present, meaning drop any row whose
#'   first non-blank character is not numeric/sign/decimal).
#'
#' @export
parse_nonmem_ignore <- function(ctl_lines) {
  nm_ops <- c("\\.EQ\\." = " == ", "\\.NE\\." = " != ",
               "\\.GE\\." = " >= ", "\\.LE\\." = " <= ",
               "\\.GT\\." = " > ",  "\\.LT\\." = " < ")

  # Strip inline comments; blank out fully commented lines
  ctl_lines <- sub(";.*$", "", ctl_lines)

  # Locate $DATA block
  data_start <- grep("^\\s*\\$DATA\\b", ctl_lines, ignore.case = TRUE)
  if (length(data_start) == 0)
    return(list(conditions = character(0), ignore_chars = character(0),
                ignore_nonnumeric = FALSE))

  # End at next $BLOCK
  block_starts <- grep("^\\s*\\$[A-Z]", ctl_lines, ignore.case = TRUE)
  next_block   <- block_starts[block_starts > data_start[1]][1]
  data_end     <- if (!is.na(next_block)) next_block - 1L else length(ctl_lines)

  data_lines <- ctl_lines[data_start[1]:data_end]

  conditions        <- character(0)
  ignore_chars      <- character(0)
  ignore_nonnumeric <- FALSE

  for (line in data_lines) {
    # ── Character-based IGNORE: IGNORE=@ or IGNORE=C (single char, not paren)
    char_m <- regmatches(line,
                         regexpr("(?i)IGNORE\\s*=\\s*([^(\\s])", line,
                                 perl = TRUE))
    if (length(char_m) > 0 && nchar(char_m) > 0) {
      ch <- sub("(?i)IGNORE\\s*=\\s*", "", char_m, perl = TRUE)
      if (ch == "@") {
        # @ is a wildcard: ignore any row not starting with a numeric character
        ignore_nonnumeric <- TRUE
      } else {
        ignore_chars <- unique(c(ignore_chars, ch))
      }
    }

    # ── Condition-based IGNORE: IGNORE=(cond1,cond2,...)
    cond_matches <- regmatches(
      line,
      gregexpr("(?i)IGNORE\\s*=\\s*\\([^)]+\\)", line, perl = TRUE)
    )[[1]]
    for (m in cond_matches) {
      inner <- sub("(?i)IGNORE\\s*=\\s*\\((.+)\\)", "\\1", m, perl = TRUE)
      for (cond in trimws(strsplit(inner, ",")[[1]])) {
        r_cond <- cond
        for (pat in names(nm_ops))
          r_cond <- gsub(pat, nm_ops[[pat]], r_cond, ignore.case = TRUE)
        conditions <- c(conditions, paste0("!dplyr::coalesce(", trimws(r_cond), ", FALSE)"))
      }
    }
  }

  list(conditions = conditions, ignore_chars = ignore_chars,
       ignore_nonnumeric = ignore_nonnumeric)
}


#' Apply NONMEM IGNORE character rules to a raw data body
#'
#' Drop `$DATA IGNORE=<char>` and `IGNORE=@` comment rows from a character
#' vector of raw (unparsed) data-file body lines. This is the
#' character-based half of [parse_nonmem_ignore()]'s output, applied before
#' the file is parsed into a data frame; the condition-based IGNORE=(cond)
#' rules are instead applied afterwards as dplyr filters on the parsed
#' frame.
#'
#' @param body Character vector of data-file lines with no header.
#' @param ignore_chars Character vector of leading-character comment markers
#'   whose rows should be dropped (from `IGNORE=<char>`, e.g. `IGNORE=C`).
#' @param ignore_nonnumeric Logical. When `TRUE`, drop any row whose first
#'   non-blank character is not numeric/sign/decimal (mirrors NONMEM's
#'   `$DATA IGNORE=@`).
#'
#' @return `body` with matching rows removed. Blank lines are always kept.
#'
#' @export
apply_nonmem_ignore_rules <- function(body, ignore_chars = character(0),
                                       ignore_nonnumeric = FALSE) {
  if (isTRUE(ignore_nonnumeric))
    body <- body[grepl("^\\s*[-+.0-9]", body) | nchar(trimws(body)) == 0]
  if (length(ignore_chars) > 0) {
    first_char <- substr(trimws(body, "left"), 1, 1)
    body <- body[!first_char %in% ignore_chars]
  }
  body
}


#' Extract the dataset file path from a NONMEM $DATA block
#'
#' Return the file token that immediately follows `$DATA`, resolved against
#' `ctl_dir` when the token is a relative path.
#'
#' @param ctl_lines Character vector. Lines of the NONMEM control file.
#' @param ctl_dir Character or `NULL`. Directory of the control file, used
#'   to resolve relative paths. `NULL` returns relative paths as-is.
#'
#' @return Character. Normalised (possibly non-existent) path string, or
#'   `NULL` when no `$DATA` block is present.
#'
#' @export
parse_nonmem_datafile <- function(ctl_lines, ctl_dir = NULL) {
  ctl_lines  <- sub(";.*$", "", ctl_lines)
  data_start <- grep("^\\s*\\$DATA\\b", ctl_lines, ignore.case = TRUE)
  if (length(data_start) == 0) return(NULL)

  data_line  <- ctl_lines[data_start[1]]
  file_token <- sub("(?i)^\\s*\\$DATA\\s+(\\S+).*$", "\\1", data_line, perl = TRUE)
  if (!nchar(file_token) || file_token == data_line) return(NULL)

  if (!is.null(ctl_dir) && !startsWith(file_token, "/"))
    file_token <- file.path(ctl_dir, file_token)

  normalizePath(file_token, mustWork = FALSE)
}
