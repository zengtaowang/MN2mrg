# R/vpc_utils_monolix.R
# Monolix-specific VPC data-prep utilities.
# Depends on R/mlx_parse_utils.R (extract_depot_param, extract_depot_info,
# extract_longitudinal, get_block) and R/vpc_utils_common.R (%||%).
# In package context, all live in the same NAMESPACE.

#' Build an ADM number to dosing-compartment mapping from a Monolix PK block
#'
#' Parse the `PK:` block of cleaned mlxtran lines to produce a mapping from
#' `adm=` numbers to the actual dosing-compartment name used by the
#' translated mrgsolve model. When a `depot()` call specifies `ka=`
#' (first-order absorption), the translated model doses into a synthetic
#' `<target>_depot` compartment (see [extract_depot_info()]'s
#' `target_counts` naming), not into the target itself. Must mirror
#' [extract_depot_info()]'s naming or ADM->CMT lookups built from this map
#' will route ka-based doses into the wrong compartment.
#'
#' @param clean_lines Character vector. Comment-stripped Monolix model text.
#'
#' @return Named list mapping `adm` numbers (as strings) to compartment
#'   names, e.g. `list("1" = "Ac_depot", "2" = "Ad")`.
#'
#' @export
extract_adm_to_target <- function(clean_lines) {
  long_idx <- which(stringr::str_detect(clean_lines, "^\\s*\\[LONGITUDINAL\\]\\s*$"))
  if (!length(long_idx)) return(list())

  long <- clean_lines[long_idx:length(clean_lines)]
  pk_start <- which(stringr::str_detect(long, "^\\s*PK\\s*:\\s*$"))
  if (!length(pk_start)) return(list())

  pk_end_candidates <- which(stringr::str_detect(long, "^\\s*(EQUATION|DEFINITION|OUTPUT)\\s*:\\s*$"))
  pk_end_candidates <- pk_end_candidates[pk_end_candidates > pk_start[1]]
  pk_end <- if (length(pk_end_candidates)) min(pk_end_candidates) - 1 else length(long)

  pk_block    <- long[(pk_start[1] + 1):pk_end]
  depot_lines <- pk_block[stringr::str_detect(pk_block, "^\\s*depot\\s*\\(")]
  if (!length(depot_lines)) return(list())

  adm_to_target <- list()
  target_counts <- list()
  for (line in depot_lines) {
    adm_match    <- stringr::str_match(line, "adm\\s*=\\s*([0-9]+)")
    target_match <- stringr::str_match(line, "target\\s*=\\s*([a-zA-Z0-9_]+)")
    if (is.na(adm_match[1, 2]) || is.na(target_match[1, 2])) next
    target <- target_match[1, 2]

    ka_val <- extract_depot_param(line, "ka")
    if (!is.na(ka_val)) {
      if (is.null(target_counts[[target]])) {
        target_counts[[target]] <- 1
        cmt_name <- paste0(target, "_depot")
      } else {
        target_counts[[target]] <- target_counts[[target]] + 1
        cmt_name <- paste0(target, "_depot", target_counts[[target]])
      }
    } else {
      cmt_name <- target
    }

    adm_to_target[[adm_match[1, 2]]] <- cmt_name
  }
  adm_to_target
}


#' Apply Monolix [FILTER] block conditions to a raw dataset
#'
#' Apply the R filter expressions returned by
#' [parse_mlx_dataset_filters()] to a raw Monolix dataset (raw csv column
#' names, before any renaming). Shared by [prepare_monolix_dataset()]'s
#' Step 0 and by the app.R ETA-lookup code, both of which must filter the
#' same rows before deriving any `as.factor()`-based numeric ID encoding
#' from the data (the encoding depends on the set of unique values present).
#'
#' @param data Data frame. Raw dataset (pre-rename).
#' @param conditions Character vector of R filter expressions (typically
#'   from [parse_mlx_dataset_filters()]$conditions).
#'
#' @return `data` with matching-row filters applied. Conditions that fail
#'   to parse or reference missing columns are silently skipped.
#'
#' @export
apply_mlx_filter_conditions <- function(data, conditions) {
  if (is.null(conditions) || length(conditions) == 0) return(data)
  for (expr_str in conditions) {
    parsed <- tryCatch(parse(text = expr_str), error = function(e) NULL)
    if (is.null(parsed)) next
    used_cols <- tryCatch(all.vars(parsed), error = function(e) character(0))
    if (!all(used_cols %in% names(data))) next
    data <- tryCatch(dplyr::filter(data, !!parsed[[1]]), error = function(e) data)
  }
  data
}

#' Standardise a raw Monolix dataset for mrgsolve simulation
#'
#' Unified helper shared by [prep_vpc_dataset()] and the app.R
#' `prepare_mlx_dataset()` inline block. Applies (in order): Monolix
#' `[FILTER]` conditions, drop of `ignore` columns, headerType-driven column
#' renaming (ID/TIME/DV/AMT/EVID/DVID/RATE/II/ADDL/BQL), numeric coercion,
#' EVID derivation, ADM->CMT routing via [extract_adm_to_target()],
#' zero-order-infusion `RATE = -2` flagging via [extract_depot_info()],
#' transformed-categorical covariate synthesis, raw-categorical encoding,
#' MDV filtering, DVID normalisation, and a final sort by ID/TIME.
#'
#' @param data Data frame. Raw dataset as loaded from the Monolix csv.
#' @param opts Named list. Fields consumed:
#'   `data_info` (from `lixoftConnectors::getData()`),
#'   `covariate_info` (from `getCovariateInformation()`),
#'   `mlxtran_lines` (cleaned structural model text for ADM->CMT parsing),
#'   `dvid_map` (data frame with `dvid_identifier` column),
#'   `has_dvid` (logical; TRUE when the project uses an obsid/DVID column),
#'   `filter_mdv` (logical; TRUE removes `MDV==1` obs rows, set TRUE for
#'   VPC), `mlx_filter_conditions` (character vector of R filter expressions
#'   from [parse_mlx_dataset_filters()], referencing RAW csv column names,
#'   applied before renaming), `bql_col` (fallback name when no `cens`
#'   headerType is set), and `tad_col` (user-mapped TAD source column to
#'   preserve through the ignore-column drop).
#'
#' @return A list with `data` (the prepared data frame sorted by ID and
#'   TIME) and `cat_covariate_all_categories` (a named list used to decode
#'   numeric categoricals back to their original labels in downstream
#'   plotting).
#'
#' @export
prepare_monolix_dataset <- function(data, opts = list()) {

  data_info      <- opts$data_info
  covariate_info <- opts$covariate_info
  mlxtran_lines  <- opts$mlxtran_lines
  dvid_map       <- opts$dvid_map
  has_dvid       <- isTRUE(opts$has_dvid)
  filter_mdv     <- isTRUE(opts$filter_mdv)

  cat_covariate_all_categories <- list()

  # -- Step 0: Apply Monolix [FILTER] block conditions (raw csv column names) --
  data <- apply_mlx_filter_conditions(data, opts$mlx_filter_conditions)

  # -- Step 1: Drop ignored columns --
  if (!is.null(data_info)) {
    ignore_cols <- data_info$header[data_info$headerTypes == "ignore"]
    # Never drop a column the caller explicitly wants preserved under its raw
    # name (e.g. VPC's user-mapped TAD source column -- Monolix's headerTypes
    # has no dedicated "tad" type, so an unused TAD column is normally tagged
    # "ignore" and would otherwise vanish before the TAD rename step runs).
    if (!is.null(opts$tad_col) && nchar(opts$tad_col) > 0)
      ignore_cols <- setdiff(ignore_cols, opts$tad_col)
    ignore_cols <- ignore_cols[ignore_cols %in% names(data)]
    if (length(ignore_cols) > 0)
      data <- dplyr::select(data, -dplyr::all_of(ignore_cols))
  }

  # -- Step 2: Column remapping via headerTypes --
  if (!is.null(data_info)) {
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
    type_to_std <- c(id = "ID", time = "TIME", observation = "DV",
                     amount = "AMT", evid = "EVID", obsid = "DVID",
                     rate = "RATE", interdoseinterval = "II", addl = "ADDL",
                     cens = "BQL")
    for (type in names(type_to_std)) {
      col <- data_info$header[data_info$headerTypes == type]
      if (length(col) == 1)
        data <- rename_col(data, col, type_to_std[[type]])
    }

    # The occasion column (headerType "occ", e.g. "dose_cumsum") is NOT
    # renamed -- mrgsolve's $PARAM occasion-indicator regressor is declared
    # under that exact original name (it's what the model's $MAIN cascade
    # reads to select the active IOV omega slot), so it must survive under
    # its original name in the data/idata passed to mrgsolve. Just make sure
    # it's numeric.
    occ_col <- data_info$header[data_info$headerTypes == "occ"]
    if (length(occ_col) == 1 && occ_col %in% names(data) && !is.numeric(data[[occ_col]]))
      data <- dplyr::mutate(data, !!rlang::sym(occ_col) := as.numeric(.data[[occ_col]]))
  }

  # -- Step 3: Numeric coercion --
  for (col in c("TIME", "DV", "AMT", "EVID", "RATE", "II", "ADDL")) {
    if (col %in% names(data) && !is.numeric(data[[col]]))
      data <- dplyr::mutate(data, !!rlang::sym(col) := as.numeric(.data[[col]]))
  }
  if ("ID" %in% names(data) && !is.numeric(data[["ID"]]))
    data <- dplyr::mutate(data, ID = as.numeric(as.factor(ID)))

  # -- Step 4: EVID derivation if absent --
  if (!"EVID" %in% names(data)) {
    if ("AMT" %in% names(data)) {
      data <- dplyr::mutate(data, EVID = dplyr::if_else(is.na(AMT), 0, 1))
    } else {
      data <- dplyr::mutate(data, EVID = 0L)
    }
  }

  # -- Step 5: CMT assignment --
  if ("ADM" %in% toupper(names(data)) && !is.null(mlxtran_lines)) {
    adm_col        <- names(data)[toupper(names(data)) == "ADM"]
    adm_cmt_lookup <- unlist(extract_adm_to_target(mlxtran_lines))
    data <- data %>%
      dplyr::mutate(CMT = adm_cmt_lookup[as.character(.data[[adm_col]])])

    # Dosing rows (EVID==1) whose ADM value has no depot() target in THIS
    # structural model represent administration of a compound/route the
    # model doesn't include at all (e.g. a co-administered drug dosed under
    # a different ADM code, never referenced in the PK: block) -- drop them
    # rather than routing them into a bogus CMT. mrgsolve rejects a dosing
    # record on an unmatched compartment name outright ("event record cmt
    # must be between 1 and N"), it doesn't silently ignore it.
    unmapped_dose <- is.na(data$CMT) & data$EVID == 1
    if (any(unmapped_dose, na.rm = TRUE))
      data <- data[!unmapped_dose, ]

    data <- data %>%
      dplyr::mutate(CMT = dplyr::if_else(is.na(CMT), "OBS", CMT))
  }
  if (!"CMT" %in% names(data))
    data <- dplyr::mutate(data, CMT = dplyr::if_else(EVID == 1, 1, 0))

  # -- Step 5b: Flag modeled-duration dosing rows with RATE = -2 --
  # A Monolix depot() PK macro with Tk0=<expr> is a zero-order infusion of
  # duration Tk0 into the target compartment -- the translator (see
  # extract_depot_info() in translation_utils.r) emits a matching
  # "D_<cmt> = <expr>;" $MAIN line for that compartment. mrgsolve only
  # honors that modeled duration when the dosing record's RATE is exactly
  # -2; otherwise it doses as an instantaneous bolus (silently ignoring
  # D_<cmt>, with only a warning) which drastically distorts the
  # early-timepoint concentration profile relative to Monolix's actual Tk0
  # absorption. Derived from the same mlxtran_lines already used for CMT
  # assignment (Step 5) rather than passed in separately, so every caller
  # of prepare_monolix_dataset() gets this automatically. Never overwrite
  # an already-observed real RATE value from the dataset.
  if (!is.null(mlxtran_lines)) {
    duration_lines <- grep("^D_[A-Za-z0-9_]+\\s*=", extract_depot_info(mlxtran_lines)$bioavail_lines, value = TRUE)
    duration_cmts  <- sub("^D_([A-Za-z0-9_]+)\\s*=.*$", "\\1", duration_lines)
    if (length(duration_cmts) > 0) {
      if (!"RATE" %in% names(data)) data$RATE <- 0
      dur_dose <- data$EVID == 1 & as.character(data$CMT) %in% duration_cmts &
        (is.na(data$RATE) | data$RATE == 0)
      data$RATE[dur_dose] <- -2
    }
  }

  # -- Step 6: Create transformed categorical covariate columns --
  # Monolix [COVARIATE] DEFINITION block can define derived covariates like:
  #   tSPECIES = { transform=SPECIES, categories={'G_NHP'={'NHP'}, 'G_HUMAN'={'HUMAN'}}, reference='G_NHP' }
  # These are never in the raw dataset; we derive them here from the source column.
  # This must run BEFORE step 7 so the source column (e.g. SPECIES) is still raw string.
  # Encoding: reference → 0, non-reference in names(f$transformed) order → 1, 2, ...
  # This matches convert_categorical_covariate() in translation_utils.r which assigns
  # 1-based indices to non-reference category occurrences in structural model formulas.
  if (!is.null(covariate_info) && !is.null(covariate_info$formula)) {
    for (trf_name in names(covariate_info$formula)) {
      f <- covariate_info$formula[[trf_name]]
      if (!is.list(f) || is.null(f$reference) || is.null(f$from) || is.null(f$transformed))
        next
      source_col <- f$from
      if (!source_col %in% names(data)) next

      all_groups <- names(f$transformed)
      ref_group  <- f$reference
      non_ref    <- setdiff(all_groups, ref_group)
      ordered    <- c(ref_group, non_ref)
      grp_to_int <- stats::setNames(seq_along(ordered) - 1L, ordered)

      src_to_int <- integer(0)
      for (grp in all_groups) {
        src_vals <- as.character(unlist(f$transformed[[grp]]))
        src_to_int[src_vals] <- grp_to_int[[grp]]
      }

      data[[trf_name]] <- src_to_int[as.character(data[[source_col]])]
      cat_covariate_all_categories[[trf_name]] <- ordered
    }
  }

  # -- Step 7: Encode raw categorical covariates --
  if (!is.null(data_info) && !is.null(covariate_info)) {
    cat_cov_idx   <- which(data_info$headerTypes == "catcov")
    cat_cov_names <- data_info$header[cat_cov_idx]
    for (cat_cov in cat_cov_names) {
      if (!cat_cov %in% names(data)) next
      all_cats <- if (!is.null(covariate_info$categories) &&
                      cat_cov %in% names(covariate_info$categories))
        covariate_info$categories[[cat_cov]]
      else
        sort(unique(as.character(data[[cat_cov]])))
      if (length(all_cats) == 0) next
      cat_encoding <- stats::setNames(seq_along(all_cats) - 1L, all_cats)
      data <- dplyr::mutate(data,
        !!rlang::sym(cat_cov) := cat_encoding[as.character(.data[[cat_cov]])])
      cat_covariate_all_categories[[cat_cov]] <- all_cats
    }
  }

  # -- Step 8: MDV filtering (VPC path only) --
  # BQL/M3 rows have EVID==0 & MDV==1 with DV set to a placeholder (0 or the
  # LLOQ); they are genuine observations and must be kept (as censored) for
  # vpc::vpc_cens() to compute the below-LLOQ fraction correctly. BQL is
  # auto-detected from headerTypes "cens" in Step 2 above; opts$bql_col
  # (manual UI mapping) is only a fallback for datasets without that type.
  if (filter_mdv) {
    mdv_col <- names(data)[toupper(names(data)) == "MDV"]
    bql_col <- if ("BQL" %in% names(data)) {
      "BQL"
    } else if (!is.null(opts$bql_col) && nchar(opts$bql_col) > 0) {
      names(data)[toupper(names(data)) == toupper(opts$bql_col)]
    } else character(0)
    if (length(mdv_col) > 0) {
      if (length(bql_col) == 1) {
        data <- dplyr::filter(data, !(.data[[mdv_col]] == 1 & EVID == 0 & .data[[bql_col]] != 1))
      } else {
        data <- dplyr::filter(data, !(.data[[mdv_col]] == 1 & EVID == 0))
      }
    }
  }

  # -- Step 9: DVID handling --
  if (!"DVID" %in% names(data)) {
    data <- dplyr::mutate(data, DVID = dplyr::if_else(EVID == 1, 0, 1))
  } else {
    if (is.character(data$DVID) && has_dvid && !is.null(dvid_map)) {
      dvid_ids        <- dvid_map$dvid_identifier
      all_numeric_ids <- !any(is.na(suppressWarnings(as.numeric(dvid_ids))))
      if (all_numeric_ids) {
        data <- dplyr::mutate(data,
          DVID = suppressWarnings(as.integer(as.character(DVID))))
      } else {
        data <- dplyr::mutate(data,
          DVID = match(as.character(DVID), dvid_ids))
      }
    }
    data <- dplyr::mutate(data,
      DVID = dplyr::if_else(is.na(DVID), 0, as.numeric(DVID)))
  }

  # -- Step 10: Sort by ID/TIME --
  data <- dplyr::arrange(data, ID, TIME)

  list(data = data, cat_covariate_all_categories = cat_covariate_all_categories)
}


#' Parse the active dataset row filter from a Monolix .mlxtran project
#'
#' Read the `[FILTER]` and `[APPLICATION]` sections of a Monolix `.mlxtran`
#' project file and return the active row-filter conditions in
#' R-evaluable form. Monolix's Data tab filter dialog writes these blocks
#' with `removeLines` / `removeIds` (drop matching rows), `selectLines` /
#' `selectIds` (keep matching rows), and an `[APPLICATION] computation=`
#' entry that names which named filter is active (`originaldata` = none).
#' Bareword-RHS string comparisons like `POPD==Obese` are auto-quoted so
#' they parse as R strings; a `ID==...` token is remapped to the actual
#' subject-identifying column via `id_col`.
#'
#' Defensive by design: missing file/sections, no active filter, or an
#' unparseable `[FILTER]` block all return empty vectors rather than
#' erroring.
#'
#' @param mlxtran_path Character. Path to the `.mlxtran` project file.
#' @param id_col Character. Name of the raw csv header column that has
#'   Monolix headerType `"id"`. Defaults to `"ID"`; used to rewrite the
#'   `ID==...` tokens that Monolix always writes generically into a filter
#'   condition regardless of the actual column name.
#'
#' @return List with `conditions` (character vector of R filter expressions
#'   to AND together, referencing raw csv column names) and `description`
#'   (character vector of human-readable descriptions for display).
#'
#' @export
parse_mlx_dataset_filters <- function(mlxtran_path, id_col = "ID") {
  empty <- list(conditions = character(0), description = character(0))
  if (is.null(mlxtran_path) || !nchar(mlxtran_path) || !file.exists(mlxtran_path))
    return(empty)

  lines <- tryCatch(readLines(mlxtran_path, warn = FALSE), error = function(e) NULL)
  if (is.null(lines)) return(empty)

  section_starts <- grep("^\\s*[\\[<][A-Za-z_]+[\\]>]\\s*$", lines)
  get_section <- function(tag_pattern) {
    idx <- grep(tag_pattern, lines, ignore.case = TRUE, perl = TRUE)
    if (!length(idx)) return(character(0))
    start <- idx[1]
    later <- section_starts[section_starts > start]
    end   <- if (length(later)) later[1] - 1L else length(lines)
    if (end < start + 1L) return(character(0))
    lines[(start + 1L):end]
  }

  app_lines <- get_section("^\\s*\\[APPLICATION\\]\\s*$")
  if (!length(app_lines)) return(empty)

  comp_line <- grep("^\\s*computation\\s*=", app_lines, perl = TRUE, value = TRUE)
  if (!length(comp_line)) return(empty)
  computation <- trimws(sub("^\\s*computation\\s*=\\s*", "", comp_line[1], perl = TRUE))
  if (!nchar(computation) || tolower(computation) == "originaldata")
    return(empty)

  filter_lines <- get_section("^\\s*\\[FILTER\\]\\s*$")
  if (!length(filter_lines)) return(empty)
  blob <- paste(filter_lines, collapse = " ")

  m <- regexpr(paste0("\\b", computation, "\\s*=\\s*\\{"), blob, perl = TRUE)
  if (m[1] == -1L) return(empty)

  # Find the closing brace matching the filter block's opening "{" by depth.
  start_brace <- m[1] + attr(m, "match.length") - 1L
  chars <- strsplit(blob, "", fixed = TRUE)[[1]]
  depth <- 1L
  end_brace <- NA_integer_
  for (i in seq(start_brace + 1L, length(chars))) {
    if (chars[i] == "{") depth <- depth + 1L
    else if (chars[i] == "}") {
      depth <- depth - 1L
      if (depth == 0L) { end_brace <- i; break }
    }
  }
  if (is.na(end_brace)) return(empty)

  block <- substring(blob, start_brace, end_brace)

  # A given key appears either unbraced ("key='expr'") or, for removeIds,
  # braced as an OR'd list ("key={'expr1','expr2'}") -- extract either shape.
  extract_key_values <- function(block, key) {
    m_brace <- regexpr(paste0(key, "\\s*=\\s*\\{([^}]*)\\}"), block, perl = TRUE)
    if (m_brace[1] != -1L) {
      whole <- regmatches(block, m_brace)
      inner <- sub(paste0(".*", key, "\\s*=\\s*\\{([^}]*)\\}.*"), "\\1", whole, perl = TRUE)
      vals  <- regmatches(inner, gregexpr("'([^']*)'", inner, perl = TRUE))[[1]]
      return(trimws(gsub("'", "", vals)))
    }
    m_single <- gregexpr(paste0(key, "\\s*=\\s*'([^']*)'"), block, perl = TRUE)
    whole    <- regmatches(block, m_single)[[1]]
    if (!length(whole)) return(character(0))
    trimws(sub(paste0(".*", key, "\\s*=\\s*'([^']*)'.*"), "\\1", whole, perl = TRUE))
  }

  # Monolix writes categorical comparisons with an unquoted RHS
  # (POPD==Obese, ID==subj-42) -- valid Monolix DSL, but not valid R (bare
  # multi-word text, or hyphens misread as subtraction). Auto-quote the RHS
  # of a simple "col OP rest" comparison unless it's already numeric or
  # quoted, so it parses as an R string comparison instead.
  quote_bareword_rhs <- function(cond) {
    m <- regmatches(cond, regexec("^\\s*([A-Za-z_][A-Za-z0-9_.]*)\\s*(==|!=)\\s*(.+?)\\s*$",
                                   cond, perl = TRUE))[[1]]
    if (length(m) != 4) return(cond)
    col <- m[2]; op <- m[3]; rhs <- m[4]
    if (grepl("^-?[0-9]+(\\.[0-9]+)?$", rhs) || grepl("^(['\"]).*\\1$", rhs)) return(cond)
    paste0(col, " ", op, " \"", gsub('"', '\\\\"', rhs), "\"")
  }

  # Monolix's filter dialog always refers to the subject-identifying column
  # as "ID", regardless of what that column is actually named in the raw
  # csv header -- it means whichever column has headerType "id" (Monolix's
  # <FIT>/getData() sense, which is the [CONTENT] "identifier" column when
  # the project declares one). A raw column literally named "ID" need not
  # be that column at all (it's commonly left un-typed/"ignore" when the
  # project uses a separate USUBJID as its true id column) -- id_col carries
  # the actual header name so "ID==..." conditions target the right column.
  remap_id_token <- function(cond) {
    if (is.null(id_col) || !nchar(id_col) || id_col == "ID") return(cond)
    sub("^(\\s*)ID(\\s*(==|!=))", paste0("\\1", id_col, "\\2"), cond, perl = TRUE)
  }

  quote_all <- function(vals) {
    if (!length(vals)) return(vals)
    vapply(vals, function(v) quote_bareword_rhs(remap_id_token(v)), character(1))
  }

  remove_lines_vals <- quote_all(extract_key_values(block, "removeLines"))
  select_ids_vals   <- quote_all(extract_key_values(block, "selectIds"))
  select_lines_vals <- quote_all(extract_key_values(block, "selectLines"))
  remove_ids_vals   <- quote_all(extract_key_values(block, "removeIds"))

  conditions  <- character(0)
  description <- character(0)

  if (length(remove_lines_vals)) {
    conditions  <- c(conditions, paste0("!(", remove_lines_vals, ")"))
    description <- c(description, paste0("removeLines: ", remove_lines_vals))
  }
  if (length(select_ids_vals)) {
    conditions  <- c(conditions, paste0("(", select_ids_vals, ")"))
    description <- c(description, paste0("selectIds: ", select_ids_vals))
  }
  if (length(select_lines_vals)) {
    conditions  <- c(conditions, paste0("(", select_lines_vals, ")"))
    description <- c(description, paste0("selectLines: ", select_lines_vals))
  }
  if (length(remove_ids_vals)) {
    conditions  <- c(conditions, paste0("!(", remove_ids_vals, ")"))
    description <- c(description, paste0("removeIds: ", remove_ids_vals))
  }

  if (!length(conditions)) return(empty)
  list(conditions = conditions, description = description)
}
