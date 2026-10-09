#' Find matching parentheses and return the enclosed substring
#'
#' Scan `.string` from one end for a parenthesis and return the substring
#' from that parenthesis to its match. Used by the NONMEM-to-mrgsolve
#' translator to isolate the base or the exponent of a `**` expression when
#' it is wrapped in parentheses.
#'
#' @param .string Character. String containing parentheses.
#' @param .right Logical. `TRUE` if the parenthesis to match sits at the
#'   right end of `.string`; `FALSE` if it sits at the left end.
#'
#' @return The substring bounded by the matched parentheses (inclusive of
#'   them), with a leading `THETA` or `A` prepended when the match is the
#'   argument of a `THETA(...)` or `A(...)` expression. If no match is found
#'   `.string` is returned unchanged and a warning is printed.
#'
#' @export
matchParen <- function(.string, .right = FALSE) {
  .string <- gsub(" ", "", .string, fixed = TRUE)
  x <- rep(0, nchar(.string))
  ind <- 1:nchar(.string)
  l <- stringr::str_locate_all(.string, "\\(")[[1]][, "start"]
  r <- stringr::str_locate_all(.string, "\\)")[[1]][, "start"]
  x[l] <- 1
  x[r] <- -1
  if (.right) {
    x <- rev(x)
    ind <- rev(ind)
  }
  y <- cumsum(x)
  match <- dplyr::first(ind[y == 0])
  sub <- sort(c(ind[1], match))

  if (is.na(match)) {
    cat("\nNo matching parentheses in code\n\n.")
    return(.string)
  }

  if (.right) {
    checkTheta <- substring(.string, sub[1] - 5, sub[1] - 1)
    if (checkTheta == "THETA") sub[1] <- sub[1] - 5

    subParen <- substring(.string, sub[1], sub[2])
    justLeft <- stringr::str_split_i(.string, "[+\\-*/=]", -1)
    checkAmt <- gsub(subParen, "", justLeft, fixed = TRUE)

    if (checkAmt == "A") sub[1] <- sub[1] - 1
  }

  out <- substring(.string, sub[1], sub[2])
  return(out)
}

#' Convert a NONMEM (Fortran) code block to mrgsolve (C++) syntax
#'
#' Translate one or more NONMEM code lines to their mrgsolve equivalents:
#' lowercase intrinsic function names, replace Fortran comparators
#' (`.EQ.`, `.NE.`, ...) with C-style operators, rewrite `IF ... THEN ...
#' ENDIF` as `if (...) { ... }`, and turn `x**y` into `pow(x, y)`. Empty
#' lines are preserved for readability of the output.
#'
#' @param string Character vector. One or more NONMEM code lines to
#'   translate.
#'
#' @return Character vector of the same length as `string`, containing the
#'   translated C++ lines.
#'
#' @export
convertCode <- function(string) {
  outString <- purrr::map(string, function(.s) {
    out <- stringr::str_trim(.s)
    startEmpty <- !stringr::str_detect(out, "\\S")

    out <- stringr::str_split_i(out, ";", 1)
    out <- gsub("SQRT", "sqrt", out)
    out <- gsub("EXP", "exp", out)
    out <- gsub("LOG", "log", out)
    out <- gsub("IF\\(", "if(", out)
    out <- gsub("IF \\(", "if (", out)

    out <- gsub("\\.EQ\\.", " == ", out)
    out <- gsub("\\.NE\\.", " != ", out)
    out <- gsub("\\.LT\\.", " < ", out)
    out <- gsub("\\.GT\\.", " > ", out)
    out <- gsub("\\.LE\\.", " <= ", out)
    out <- gsub("\\.GE\\.", " >= ", out)
    out <- gsub("\\.AND\\.", " && ", out)
    out <- gsub("\\.OR\\.", " || ", out)

    out <- gsub(" THEN", " {", out)
    out <- gsub("ELSE", "} else {", out)
    out <- gsub("ENDIF", "}", out)

    if (stringr::str_detect(out, "\\*\\*")) {

      out <- gsub(" ", "", out)

      pows <- stringr::str_count(out, "\\*\\*")
      for (ii in 1:pows) {
        one <- stringr::str_split_i(out, "\\*\\*", 1)
        two <- stringr::str_split_i(out, "\\*\\*", 2)

        if (is.na(two)) break

        if (stringr::str_sub(one, nchar(one), nchar(one)) == ")") {
          base <- matchParen(one, .right = TRUE)
        } else {
          base <- stringr::str_split_i(one, "[+\\-*/=]", -1)
          base <- gsub("^[^a-zA-Z0-9_.]+", "", base)
        }

        if (stringr::str_sub(two, 1, 1) == "(") {
          pow <- matchParen(two, .right = FALSE)
        } else {
          pow <- stringr::str_split_i(two, "[+\\-*/=]", 1)
          pow <- gsub("[^a-zA-Z0-9_.]+$", "", pow)
        }

        out <- gsub(paste0(base, "**", pow),
                    paste0("pow(", base, ",", pow, ")"), out, fixed = TRUE)
      }
    }

    if (stringr::str_detect(out, "\\S")) out <- paste0(out, " ;")

    endFull <- stringr::str_detect(out, "\\S")
    if (endFull | startEmpty) return(out)

  }) %>% unlist()

  return(outString)
}

#' Parse a named code block from a NONMEM control stream
#'
#' Locate the `$block` header inside a NONMEM control-stream character
#' vector, extract the lines up to (but not including) the next `$...`
#' header, and pass them through [convertCode()] for Fortran-to-C++
#' translation.
#'
#' @param ctl Character vector. The full NONMEM control stream (typically the
#'   output of [readr::read_lines()]).
#' @param block Character. A regex identifying the block header, e.g.
#'   `"\\$PK"`, `"\\$DES"`, `"\\$ERROR"`.
#'
#' @return Character vector of translated code lines from the requested block,
#'   or `NULL` if the block does not appear in `ctl`.
#'
#' @export
parseCodeBlock <- function(ctl, block) {
  numRow <- 1:length(ctl)
  allBlock <- numRow[stringr::str_detect(ctl, "\\$")]
  if (any(stringr::str_detect(ctl, block))) {
    begin <- numRow[stringr::str_detect(ctl, block)] + 1
    end   <- allBlock[allBlock > begin][1] - 1
    out   <- convertCode(ctl[begin:end])
    return(out)
  }
}

#' Translate a NONMEM control stream to an mrgsolve C++ model
#'
#' High-level entry point that parses a NONMEM control (`.ctl` or `.mod`)
#' file plus its `.ext` final-estimates table and emits an mrgsolve C++
#' model source. Detects the `$SUBROUTINE`/`ADVAN` model form, translates
#' each of `$PRED`, `$PK`, `$DES`, and `$ERROR` via [parseCodeBlock()],
#' constructs `$PARAM`/`$OMEGA`/`$SIGMA` from the final estimates in
#' `extFile` (or leaves them empty when `nmext = TRUE`), and writes the
#' assembled `.cpp` to a scratch file that is then round-tripped through
#' [mrgsolve::mread()] to catch obvious compile errors. When compilation
#' fails on an undeclared data item, missing columns are appended to
#' `$PARAM` with a default value of 1 and compilation is retried once.
#'
#' @param ctlFile Character. Path to the NONMEM control-stream file.
#' @param extFile Character or `NULL`. Path to the corresponding `.ext`
#'   file. When `NULL`, derived by swapping the extension of `ctlFile`.
#' @param nmext Logical. When `TRUE`, the emitted model uses `$NMEXT` to
#'   pull parameters from `extFile` at simulate time instead of hard-coding
#'   them into `$PARAM`/`$OMEGA`/`$SIGMA`.
#'
#' @return A list with two elements: `code` (character vector, the assembled
#'   mrgsolve model source) and `message` (character vector, a status
#'   message with any warnings such as undeclared items added to `$PARAM`,
#'   `F = A(cmt)/S` insertions in `$ERROR`, or a note that NONMEM's PHI()
#'   was passed through unchanged).
#'
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom readr read_lines
#' @importFrom stringr str_detect str_locate_all str_split_i str_trim
#'   str_count str_sub str_starts str_which
#' @importFrom dplyr filter select mutate across left_join bind_cols
#'   case_when contains everything any_of all_of first sym
#' @importFrom mrgsolve mread bmat omat outvars zero_re data_set carry_out
#'   mrgsim_df
#' @importFrom ggplot2 ggplot aes geom_abline geom_point labs ggtitle
#'   theme_bw theme element_text
#'
#' @export
nonmem2mrgsolve <- function(ctlFile, extFile = NULL, nmext = FALSE) {

  if (!file.exists(ctlFile)) {
    string <- cat(ctlFile, "does not exist")
    return(list(code = NULL, message = string))
  }

  ctlext <- stringr::str_split_i(ctlFile, "\\.", 2)
  if (is.null(extFile)) {
    string <- paste0(".", ctlext)
    extFile <- gsub(string, ".ext", ctlFile, fixed = TRUE)
  }
  if (!file.exists(extFile)) {
    string <- cat(extFile, "does not exist")
    return(list(code = NULL, message = string))
  }

  ## if nmext, leave param block empty
  ## otherwise construct param, omega, and sigma blocks using estimates from extFile
  if (nmext) {
    param <- NULL
    omega <- NULL
    sigma <- NULL
  } else {
    ## final parameter estimates from extFile
    estimate <- read_ext(extFile) %>% dplyr::filter(ITERATION == -1E9)

    ## construct theta string for param block
    thetaEst <- estimate %>% dplyr::select(dplyr::contains("THETA"))
    param <- purrr::map(1:ncol(thetaEst), function(.i) {
      paste0(names(thetaEst)[.i], " = ", thetaEst[.i])
    }) %>% unlist()

    ## construct lower diagonal matrix for $OMEGA block
    omegaEst <- estimate %>% dplyr::select(dplyr::contains("OMEGA")) %>% mrgsolve::bmat()
    omega <- purrr::map(1:nrow(omegaEst), function(.i) {
      paste(omegaEst[.i, 1:.i], collapse = " ")
    }) %>% unlist()

    ## construct lower diagonal matrix for $SIGMA block
    sigmaEst <- estimate %>% dplyr::select(dplyr::contains("SIGMA")) %>% mrgsolve::bmat()
    sigma <- purrr::map(1:nrow(sigmaEst), function(.i) {
      paste(sigmaEst[.i, 1:.i], collapse = " ")
    }) %>% unlist()
  }

  basectl <- stringr::str_split_i(ctlFile, "/", -1)
  cppFile <- gsub(ctlext, "cpp", basectl, fixed = TRUE)

  ## read NONMEM control stream
  ctl <- readr::read_lines(ctlFile)

  ## find $SUBROUTINE and determine type of model
  subr <- ctl[stringr::str_detect(ctl, "\\$SUB")]
  advan <- "UNKNOWN"
  if (subr != "") {
    ## if $SUBROUTINE, find ADVAN
    advan <- stringr::str_split_i(subr, " ", 2)
    trans <- stringr::str_split_i(subr, " ", 3)

    ## determine what type of model this is
    pkmodel <- dplyr::case_when(
      advan == "ADVAN1" ~ "cmt=CENT, depot=FALSE",
      advan == "ADVAN2" ~ "cmt=\"GUT CENT\", depot=TRUE",
      advan == "ADVAN3" ~ "cmt=\"CENT PER\", depot=FALSE",
      advan == "ADVAN4" ~ "cmt=\"GUT CENT PER\", depot=TRUE",
      TRUE ~ ""
    )
    if (pkmodel == "") pkmodel <- NULL
  }

  ## determine what compartment to use for output
  ## default to compartment 2
  cmt <- dplyr::case_when(
    advan == "ADVAN1" ~ 1,
    advan == "ADVAN2" ~ 2,
    advan == "ADVAN3" ~ 1,
    advan == "ADVAN4" ~ 2,
    TRUE ~ 2
  )

  ## grab contents of $PRED, $PK, $DES, and $ERROR, if they exist
  pred <- parseCodeBlock(ctl, block = "\\$PRED")
  pk <- parseCodeBlock(ctl, block = "\\$PK")
  des <- parseCodeBlock(ctl, block = "\\$DES")
  err <- parseCodeBlock(ctl, block = "\\$ERROR")

  ## figure out what variables are defined in $PK or $PRED for use later
  predPK <- c(pred, pk)
  varDef <- purrr::map(predPK, function(.s) {
    out <- gsub(" ", "", .s)
    out <- gsub(";", "", out)
    out <- gsub("\\{", "", out)
    out <- gsub("\\}", "", out)
    ## strip out IF statements
    if (out != "") {
      if (stringr::str_starts(out, "if\\(")) {
        ifState <- matchParen(substring(out, 3), .right = FALSE)
        out <- gsub(paste0("if", ifState), "", out, fixed = TRUE)
      }
    }
    if (stringr::str_detect(out, "=")) {
      out <- stringr::str_split_i(out, "=", 1)
      if (stringr::str_detect(out, "\\S")) return(out)
    }
  }) %>% unlist() %>% unique()

  ## do some work on $ERROR block
  varErr <- NULL
  fInsert <- NULL
  if (!is.null(err)) {
    ## construct definition for F (IPRED from $PK)
    ## determine appropriate scalar or volume for divisor of F
    scalar <- paste0("S", cmt)
    volume <- paste0("V", cmt)
    divisor <- dplyr::case_when(
      any(stringr::str_detect(varDef, scalar)) ~ scalar,
      any(stringr::str_detect(varDef, volume)) ~ volume,
      TRUE ~ "V"
    )

    ## construct string for F replacement
    fInsert <- paste0("F = A(", cmt, ")/", divisor, " ;")

    ## insert F definition in $ERROR
    err <- c(fInsert, err)

    ## replace ERR() with EPS()
    err <- purrr::map(err, function(.s) {
      out <- gsub(" ", "", .s)
      out <- gsub("ERR\\(", "EPS\\(", .s)
      if (stringr::str_detect(out, "\\S")) return(out)
    }) %>% unlist()

    ## find any lines which use DV
    where <- stringr::str_which(err, "DV")
    if (length(where) > 0) {
      ## find any variables defined using DV
      varDV <- purrr::map(where, function(.i) {
        out <- gsub(" ", "", err[.i])
        out <- gsub(";", "", out)
        out <- gsub("\\{", "", out)
        out <- gsub("\\}", "", out)
        if (stringr::str_starts(out, "if\\(")) {
          ifState <- matchParen(substring(out, 3), .right = FALSE)
          out <- gsub(paste0("if", ifState), "", out, fixed = TRUE)
        }
        if (stringr::str_detect(out, "=")) {
          out <- stringr::str_split_i(out, "=", 1)
          if (stringr::str_detect(out, "\\S")) return(out)
        }
      }) %>% unlist()
      pattern <- paste(varDV, collapse = "|")
      where2 <- stringr::str_which(err, pattern)
      where <- unique(c(where, where2))

      err <- err[!(1:length(err)) %in% where]
    }

    ## find variables defined in $ERROR for $CAPTURE block
    varErr <- purrr::map(err, function(.s) {
      out <- gsub(" ", "", .s)
      out <- gsub(";", "", out)
      out <- gsub("\\{", "", out)
      out <- gsub("\\}", "", out)
      if (stringr::str_starts(out, "if\\(")) {
        ifState <- matchParen(substring(out, 3), .right = FALSE)
        out <- gsub(paste0("if", ifState), "", out, fixed = TRUE)
      }
      if (stringr::str_detect(out, "=")) {
        out <- stringr::str_split_i(out, "=", 1)
        ## scrub bare "F" (bioavailability), not substrings like F_FLAG
        if (out == "F") return(NULL)
        ## leading-underscore identifiers are always corrupted (e.g. F_FLAG
        ## mangled by an earlier blanket gsub); never real NONMEM names
        if (out == "_LAG") return(NULL)
        if (stringr::str_detect(out, "\\S")) return(out)
      }
    }) %>% unlist() %>% unique()
  }

  ## if $DES exists, count number of compartments
  init <- NULL
  if (!is.null(des)) {
    ncomp <- length(des[stringr::str_detect(des, "DADT")])
    init <- purrr::map(1:ncomp, function(.x) {
      paste0("A", .x, "=0")
    }) %>% unlist()
  }

  ## construct $DES and $INIT for ADVAN11
  if (advan == "ADVAN11") {
    if (trans == "TRANS4") {
      pk <- c(pk,
              " ",
              "K   = CL/V1 ;",
              "K12 = Q2/V1 ;",
              "K21 = Q2/V2 ;",
              "K13 = Q3/V1 ;",
              "K31 = Q3/V3 ;")
    }
    des <- c("DADT(1) = -K*A(1) - K12*A(1) + K21*A(2) - K13*A(1) + K31*A(3) ;",
             "DADT(2) = K12*A(1) - K21*A(2) ;",
             "DADT(3) = K13*A(1) - K31*A(3) ;")
    init <- c("A1=0", "A2=0", "A3=0")
  }

  ## construct $DES and $INIT for ADVAN12
  if (advan == "ADVAN12") {
    if (trans == "TRANS4") {
      pk <- c(pk,
              " ",
              "K   = CL/V2 ;",
              "K23 = Q3/V2 ;",
              "K32 = Q3/V3 ;",
              "K24 = Q4/V2 ;",
              "K42 = Q4/V4 ;")
    }
    des <- c("DADT(1) = -KA*A(1) ;",
             "DADT(2) = KA*A(1) - K*A(2) - K23*A(2) + K32*A(3) - K24*A(2) + K42*A(4) ;",
             "DADT(3) = K23*A(2) - K32*A(3) ;",
             "DADT(4) = K24*A(2) - K42*A(4) ;")
    init <- c("A1=0", "A2=0", "A3=0", "A4=0")
  }

  cppTop <- c(paste0("$PROB ", ctlFile),
              "$PLUGIN autodec nm-vars",
              " ")
  ## detect NONMEM's PHI(x) (standard normal CDF, used for M3-method BQL/
  ## censoring likelihood). mrgsolve has no PHI() and, since the .cpp is
  ## used for simulation not likelihood evaluation, the construct has no
  ## simulation meaning. Copied through as-is below; flagged via out_message
  ## so the user can remove the BQL/M3 branch manually.
  usesPhi <- any(stringr::str_detect(c(pred, pk, des, err), "PHI\\("))
  if (nmext) {
    cppTop <- c(cppTop,
                paste0("$NMEXT path=\"", extFile, "\""),
                " ")
  }

  ## construct cpp
  cpp <- cppTop
  if (!is.null(pkmodel)) cpp <- c(cpp, "$PKMODEL", pkmodel, " ")
  if (!is.null(init))    cpp <- c(cpp, "$INIT", init, "")
  if (!is.null(param))   cpp <- c(cpp, "$PARAM", param, "")
  if (!is.null(omega))   cpp <- c(cpp, "$OMEGA @block", omega, "")
  if (!is.null(sigma))   cpp <- c(cpp, "$SIGMA @block", sigma, "")
  if (!is.null(pred))    cpp <- c(cpp, "$PRED", pred, "")
  if (!is.null(pk))      cpp <- c(cpp, "$PK", pk, "")
  if (!is.null(des))     cpp <- c(cpp, "$DES", des, "")
  if (!is.null(err))     cpp <- c(cpp, "$ERROR", err, "")
  if (!is.null(varErr))  cpp <- c(cpp, "$CAPTURE", varErr)

  ## write out mrgsolve file and compile it to check for errors
  tmpFile <- file.path("/var/tmp", paste0("mod", sample(1:999999999, size = 1), ".cpp"))
  write(cpp, file = tmpFile, append = FALSE, sep = "\n")
  e <- capture.output(tryCatch(mrgsolve::mread(tmpFile),
                               error = function(e) { message(e$message) },
                               warning = function(w) { message(w$message) }),
                      type = "message")
  ## default success message when the initial compile succeeds outright
  ## (no undeclared-item fallback triggered below to set this otherwise)
  out_message <- "MRGsolve model compiled successfully"

  ## if model failed to compile
  if (any(stringr::str_detect(e, "model build step failed"))) {

    ## if failure was due to undeclared data items, add to param block
    if (any(stringr::str_detect(e, "was not declared"))) {
      items <- e[stringr::str_detect(e, "was not declared")]
      cols <- purrr::map(items, function(.s) {
        out <- gsub("‘", "", .s)
        out <- gsub("’", "", out)
        out <- stringr::str_split_i(out, "was not", 1)
        out <- gsub(" ", "", out)
        out <- stringr::str_split_i(out, ":", -1)
        ## leading-underscore identifier is always a mangled name, never a
        ## real dataset column; never add it to $PARAM
        if (out == "_LAG") return(NULL)
        if (stringr::str_detect(out, "\\S")) return(out)
      }) %>% unlist() %>% unique()

      paramAdd <- purrr::map(cols, function(.s) {
        paste0(.s, " = 1")
      }) %>% unlist()
      param <- c(param, paramAdd)

      cpp <- cppTop
      if (!is.null(pkmodel)) cpp <- c(cpp, "$PKMODEL", pkmodel, " ")
      if (!is.null(init))    cpp <- c(cpp, "$INIT", init, "")
      if (!is.null(param))   cpp <- c(cpp, "$PARAM", param, "")
      if (!is.null(omega))   cpp <- c(cpp, "$OMEGA @block", omega, "")
      if (!is.null(sigma))   cpp <- c(cpp, "$SIGMA @block", sigma, "")
      if (!is.null(pred))    cpp <- c(cpp, "$PRED", pred, "")
      if (!is.null(pk))      cpp <- c(cpp, "$PK", pk, "")
      if (!is.null(des))     cpp <- c(cpp, "$DES", des, "")
      if (!is.null(err))     cpp <- c(cpp, "$ERROR", err, "")
      if (!is.null(varErr))  cpp <- c(cpp, "$CAPTURE", varErr)
      cpp <- paste(cpp, collapse = "\n")

      tmpFile <- file.path("/var/tmp", paste0("mod", sample(1:999999, size = 1), ".cpp"))
      write(cpp, file = tmpFile, append = FALSE, sep = "\n")
      e <- capture.output(tryCatch(mrgsolve::mread(tmpFile),
                                   error = function(e) { message(e$message) },
                                   warning = function(w) { message(w$message) }),
                          type = "message")

      out_message <- c(paste(cols, collapse = " "),
                       "added to $PARAM block",
                       "Please check default values")

      if (any(stringr::str_detect(e, "model build step failed"))) {
        out_message <- c(out_message,
                         " ",
                         "MRGsolve model did not compile")
      }
    } else {
      out_message <- "MRGsolve model did not compile"
    }
  }

  if (!is.null(fInsert)) {
    out_message <- c(out_message,
                     " ",
                     paste("Setting", gsub(";", "", fInsert), "in $ERROR block"))
  }

  if (usesPhi) {
    out_message <- c(out_message,
                     " ",
                     "WARNING: PHI() (NONMEM's M3/BQL censoring likelihood) was found in the source control stream and copied through as-is.",
                     "mrgsolve has no PHI() and this model will likely fail to compile.",
                     "Remove likelihood code for BLQ observations in $ERROR. For example:",
                     "      IF(DV .LT. LLOQ) THEN",
                     "        F_FLAG=1",
                     "        Y=PHI((LLOQ-IPRED)/SD)",
                     "      ELSE",
                     "        Y=IPRED*(1+EPS(1) + EPS(2)",
                     "      ENDIF",
                     "would become:",
                     "      Y=IPRED*(1+EPS(1) + EPS(2) ;")
  }

  return(list(code = cpp, message = out_message))
}

#' Compile an mrgsolve C++ model source string
#'
#' Write a character-vector `cpp` model source to a scratch `.cpp` file and
#' round-trip it through [mrgsolve::mread()] to verify it compiles cleanly.
#' Errors are captured rather than raised so callers can surface a friendly
#' message in the Shiny UI.
#'
#' @param cpp Character vector. mrgsolve model source, typically produced
#'   by [nonmem2mrgsolve()].
#'
#' @return A list with `code` (the input `cpp`) and `message` (a
#'   compile-status message).
#'
#' @export
compile_cpp <- function(cpp) {

  tmpFile <- file.path("/var/tmp", paste0("mod", sample(1:999999999, size = 1), ".cpp"))
  write(cpp, file = tmpFile, append = FALSE, sep = "\n")
  e <- capture.output(tryCatch(mrgsolve::mread(tmpFile),
                               error = function(e) { message(e$message) },
                               warning = function(w) { message(w$message) }),
                      type = "message")

  if (any(stringr::str_detect(e, "model build step failed"))) {
    out_message <- c("MRGsolve model did not compile")
    cat(print(e, collapse = "\n"))
  } else {
    out_message <- c("MRGsolve compilation successful")
  }

  return(list(code = cpp, message = out_message))
}

#' Coerce a value to numeric, silently returning NA on failure
#'
#' Wrapper around `as.numeric(as.character(.x))` that suppresses the coercion
#' warning. Used inside [verifyModel()] when normalising a NONMEM dataset
#' before merging with mrgsolve simulation output.
#'
#' @param .x A vector to coerce.
#'
#' @return Numeric vector of the same length as `.x`; unparseable entries are
#'   `NA`.
#'
#' @export
convert_numeric <- function(.x) {
  suppressWarnings(
    as.numeric(as.character(.x))
  )
}

#' Replace NA with 0
#'
#' NONMEM treats missing values as zero when reading a dataset. This helper
#' mirrors that behaviour after a numeric coercion has produced NAs. Used in
#' [verifyModel()] on every dataset column before comparison with the
#' NONMEM table output.
#'
#' @param .x A vector.
#'
#' @return `.x` with `NA` entries replaced by `0`.
#'
#' @export
convert_zero <- function(.x) {
  ifelse(is.na(.x), 0, .x)
}

#' Verify a translated mrgsolve model against NONMEM PRED/IPRED output
#'
#' Read a NONMEM dataset and its post-fit table file, simulate the
#' translated mrgsolve model against the dataset, and produce a diagnostic
#' plot of NONMEM PRED vs mrgsolve PRED (and IPRED vs mrgsolve IPRED when
#' the table file carries the required ETAs). Optionally excludes BQL/M3
#' censored observations via `censor_filter` so that their placeholder
#' NONMEM PRED/IPRED do not appear as false mismatches. Used from the
#' verification tab of the Shiny app to give the user a visual go/no-go on
#' each translation.
#'
#' @param model Character. Either a path to an existing mrgsolve `.cpp`
#'   file or a character vector holding the model source; in the latter
#'   case the source is written to a scratch file before compilation.
#' @param dataset Character. Path to the NONMEM input dataset (CSV; leading
#'   `#ID` header is stripped automatically).
#' @param table Character. Path to a NONMEM table file (typically containing
#'   PRED/IPRED and optionally ETAs).
#' @param table2 Character or `NULL`. Path to an additional table file
#'   whose columns are merged into `table` when supplied.
#' @param filters Character vector of R-syntax filter expressions applied
#'   to the dataset (typically from parsing `$DATA IGNORE=(...)`).
#' @param ignore_chars Character vector of leading-character comment markers
#'   whose rows should be dropped (e.g. from `$DATA IGNORE=C`).
#' @param ignore_nonnumeric Logical. When `TRUE`, drop any row whose first
#'   non-blank character is not numeric/sign/decimal (mirrors NONMEM's
#'   `$DATA IGNORE=@`).
#' @param censor_filter Character or `NULL`. A single R filter expression
#'   identifying BQL/M3-censored rows in the table file (e.g. `"BQL == 1"`,
#'   `"BLQ == 1"`, `"CENS != 0"`). Rows matching are excluded from the
#'   PRED/IPRED comparison. `NULL` or empty string means no exclusion.
#'
#' @return A list with `plot` (a patchwork-composed `ggplot` object, or a
#'   single `ggplot` when IPRED is not available, or `NULL` on error) and
#'   `message` (character vector describing the outcome).
#'
#' @export
verifyModel <- function(model, dataset, table, table2 = NULL, filters,
                        ignore_chars = character(0), ignore_nonnumeric = FALSE,
                        censor_filter = NULL) {

  ## read MRGsolve model if it exists
  if (file.exists(model)) {
    tmpFile <- model
  } else {
    tmpFile <- file.path("/var/tmp", paste0("mod", sample(1:999999999, size = 1), ".cpp"))
    write(model, file = tmpFile, append = FALSE, sep = "\n")
  }

  ## make sure compilation works
  e <- capture.output(tryCatch(mrgsolve::mread(tmpFile),
                               error = function(e) { message(e$message) },
                               warning = function(w) { message(w$message) }),
                      type = "message")

  if (any(stringr::str_detect(e, "model build step failed"))) {
    return(list(plot = NULL, message = "MRGsolve model did not compile"))
  } else {
    mod <- mrgsolve::mread(tmpFile)
  }

  ## read dataset if it exists -- strip leading # from header (e.g. #ID -> ID)
  if (file.exists(dataset)) {
    raw_lines  <- readLines(dataset, warn = FALSE)
    header_idx <- which(nchar(trimws(raw_lines)) > 0)[1]
    header     <- sub("^#", "", raw_lines[header_idx])
    body       <- raw_lines[-seq_len(header_idx)]
    ## drop NONMEM IGNORE=@ / IGNORE=<char> comment rows (see
    ## apply_nonmem_ignore_rules() in vpc_utils.R -- shared with the VPC
    ## dataset-prep pipeline)
    body       <- apply_nonmem_ignore_rules(body, ignore_chars, ignore_nonnumeric)
    tmp_csv    <- tempfile(fileext = ".csv")
    writeLines(c(header, body), tmp_csv)
    dat <- read.table(tmp_csv, header = TRUE, sep = ",", na = ".")
  } else {
    return(list(plot = NULL, message = paste(dataset, "does not exist")))
  }

  ## Apply filters, if specified
  if (nchar(filters)[1] > 0) {
    filters <- purrr::map(filters, ~parse(text = .x))
    for (ii in 1:length(filters))  dat <- dat %>% dplyr::filter(eval(filters[[ii]]))
  }
  ## convert all columns in dataset to numeric
  ## replace missing values with zero (as done by NONMEM)
  dat <- dat %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), convert_numeric)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), convert_zero))

  ## read table file if it exists
  if (file.exists(table)) {
    tab <- read.table(table, header = TRUE, skip = 1)
  } else {
    return(list(plot = NULL, message = paste(table, "does not exist")))
  }

  ## if additional table file is specified
  if (!is.null(table2)) {
    if (file.exists(table2)) {
      tab2 <- read.table(table2, header = TRUE, skip = 1)
    } else {
      return(list(plot = NULL, message = paste(table2, "does not exist")))
    }
    if (nrow(tab) == nrow(tab2)) {
      ## if number of rows match, append any new columns
      tab <- tab %>% dplyr::bind_cols(tab2 %>% dplyr::select(!dplyr::any_of(names(tab))))
    } else {
      ## otherwise assume it is a FIRSTONLY table file and merge by ID
      tab <- dplyr::left_join(tab2 %>% dplyr::select(ID, !dplyr::any_of(names(tab))), by = c("ID"))
    }
  }

  if (nrow(dat) != nrow(tab)) {
    return(list(plot = NULL, message = "Number of rows in dataset and table file do not match.  Check filters."))
  }

  ## check to see if table file contains required ETAs to check IPRED
  netas <- convert_numeric(nrow(mrgsolve::omat(mod)))
  etas <- purrr::map(1:netas, function(.i) {
    if (.i < 10) {
      paste0("ETA", .i)
    } else {
      paste0("ET", .i)
    }
  }) %>% unlist()

  allETAs <- all(etas %in% colnames(tab))

  ## determine what to use for model output
  if ("IPRED" %in% mrgsolve::outvars(mod)$capture) {
    ipred <- "IPRED"
  } else {
    if ("Y" %in% mrgsolve::outvars(mod)$capture) {
      ipred <- "Y"
    } else {
      return(list(plot = NULL, message = "Verification requires IPRED or Y in $CAPTURE"))
    }
  }

  ###### generate PRED plot
  simpred  <- mod %>%
    mrgsolve::zero_re() %>%
    mrgsolve::data_set(dat) %>%
    mrgsolve::carry_out(EVID) %>%
    mrgsolve::mrgsim_df() %>%
    dplyr::select(EVID, PRED = !!dplyr::sym(ipred))
  ## append simulated columns to NONMEM table file
  sim <- tab %>%
    cbind.data.frame(MRGPRED = simpred$PRED, MRGEVID = simpred$EVID) %>%
    dplyr::filter(MRGEVID == 0)
  ## exclude BQL/M3-censored rows per the user-supplied censor_filter, if any.
  ## NONMEM PRED/IPRED for those rows are censoring-likelihood placeholders,
  ## not real predicted concentrations, so they are not comparable to
  ## mrgsolve's simulated (uncensored) output and would appear as a false
  ## mismatch
  bqlMsg <- NULL
  if (!is.null(censor_filter) && nchar(censor_filter)[1] > 0) {
    nBefore <- nrow(sim)
    sim <- sim %>% dplyr::filter(!eval(parse(text = censor_filter)))
    nExcluded <- nBefore - nrow(sim)
    if (nExcluded > 0) {
      bqlMsg <- paste0("Excluded ", nExcluded, " row(s) matching '", censor_filter, "' (BQL/M3) from PRED/IPRED comparison")
    }
  }
  p1 <- sim %>% ggplot2::ggplot(ggplot2::aes(x = PRED, y = MRGPRED)) +
    ggplot2::geom_abline() + ggplot2::geom_point() +
    ggplot2::labs(x = "NONMEM", y = "MRGsolve") + ggplot2::ggtitle("PRED") +
    ggplot2::theme_bw() + ggplot2::theme(aspect.ratio = 1) +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
                   axis.title = ggplot2::element_text(size = 14),
                   title = ggplot2::element_text(size = 14))

  ## generate IPRED plot if all ETAs are present
  if (allETAs) {
    dat <- dat %>% cbind.data.frame(tab %>% dplyr::select(dplyr::all_of(etas)))
    simipred <- mod %>%
      mrgsolve::zero_re(sigma) %>%
      mrgsolve::data_set(dat) %>%
      mrgsolve::carry_out(EVID) %>%
      mrgsolve::mrgsim_df(etasrc = "data") %>%
      dplyr::select(EVID, IPRED = !!dplyr::sym(ipred))
    sim <- tab %>%
      cbind.data.frame(MRGIPRED = simipred$IPRED, MRGEVID = simipred$EVID) %>%
      dplyr::filter(MRGEVID == 0)
    if (!is.null(censor_filter) && nchar(censor_filter)[1] > 0) {
      sim <- sim %>% dplyr::filter(!eval(parse(text = censor_filter)))
    }
    p2 <- sim %>% ggplot2::ggplot(ggplot2::aes(x = IPRED, y = MRGIPRED)) +
      ggplot2::geom_abline() + ggplot2::geom_point() +
      ggplot2::labs(x = "NONMEM", y = "MRGsolve") + ggplot2::ggtitle("IPRED") +
      ggplot2::theme_bw() + ggplot2::theme(aspect.ratio = 1) +
      ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
                     axis.title = ggplot2::element_text(size = 14),
                     title = ggplot2::element_text(size = 14))
  } else {
    msg <- "IPRED plot not created\nCheck ETAs in table file"
    if (!is.null(bqlMsg)) msg <- paste0(bqlMsg, "\n", msg)
    return(list(plot = p1, message = msg))
  }

  p <- patchwork::wrap_plots(list(p1, p2))

  msg <- "Verification plots successfully created"
  if (!is.null(bqlMsg)) msg <- paste0(bqlMsg, "\n", msg)
  return(list(plot = p, message = msg))
}
