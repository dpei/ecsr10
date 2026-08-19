#' Apply comorbidity analysis to patient diagnosis data
#'
#' This function processes patient diagnosis data to identify comorbidities based on ICD-10-CM 
#' diagnosis codes using the Elixhauser Comorbidity Software Refined (CMR) methodology.
#' It handles Present on Admission (POA) logic, combination codes, and hierarchical exclusions.
#'
#' @param patient_data Data frame containing patient diagnosis data
#' @param dx_cols Character vector of column names containing diagnosis codes
#' @param poa_cols Character vector of column names containing POA indicators (optional)
#' @param year_col Character, name of column containing year information (default "year")
#' @param quarter_col Character, name of column containing quarter information (default "quarter")
#' @param release Character, which AHRQ CMR release to score with (default
#'   \code{cmr_version()}, the newest available). See \code{\link{cmr_releases}}
#'   for the supported set. The release selects the diagnosis-code table, the
#'   newest ICD-10-CM version reachable from year/quarter, and - via
#'   \code{\link{cmr_index}} - the index weights. It is recorded on the result as
#'   the \code{cmr_release} attribute. An explicit \code{comfmt} overrides only
#'   the code table; the release's ICD-version cap still applies.
#' @param comfmt Data frame with comorbidity format containing "target" and "pattern" columns.
#'   If NULL (default), uses the built-in table for \code{release}.
#' @param poa_exempt Named list of POA exempt codes by version. If NULL (default),
#'   uses the built-in poaxmpt_codes_long data when use_poa is TRUE.
#' @param use_poa Logical, whether to apply POA logic (default TRUE)
#' @param wildcard_mode Character, pattern matching mode ("wildcard" or "regex")
#' @param ncores Integer, number of forked workers the encounters are split across
#'   (default 1, i.e. serial). The input is divided into \code{ncores} contiguous
#'   blocks of rows and the whole pipeline runs on each block independently.
#'   Values above \code{parallel::detectCores()} are clamped with a warning.
#'   Forking is unavailable on Windows, where any value above 1 falls back to
#'   serial with a warning.
#'
#'   Results are identical for every \code{ncores} value. Every stage is
#'   per-encounter, and overlapping lookup patterns are resolved by the pattern's
#'   index in \code{comfmt}, which is the same in every block - so a block's
#'   code-to-target map is an exact restriction of the whole-dataset one.
#'
#'   Peak memory grows with \code{ncores} - each worker materializes its own
#'   long-format intermediate - so memory, not core count, is usually the
#'   binding constraint on large inputs.
#' @return Data frame with original patient diagnosis data plus 38 CMR comorbidity flags
#' @details
#' The function identifies 38 comorbidity categories:
#' \itemize{
#'   \item CMR_AIDS: HIV/AIDS
#'   \item CMR_ALCOHOL: Alcohol abuse
#'   \item CMR_ANEMDEF: Deficiency anemia
#'   \item CMR_AUTOIMMUNE: Autoimmune conditions
#'   \item CMR_BLDLOSS: Blood loss
#'   \item CMR_CANCER_LEUK: Leukemia
#'   \item CMR_CANCER_LYMPH: Lymphoma
#'   \item CMR_CANCER_METS: Metastatic cancer
#'   \item CMR_CANCER_NSITU: Non-situ cancer
#'   \item CMR_CANCER_SOLID: Solid tumor cancer
#'   \item CMR_CBVD: Cerebrovascular disease
#'   \item CMR_HF: Heart failure
#'   \item CMR_COAG: Coagulopathy
#'   \item CMR_DEMENTIA: Dementia
#'   \item CMR_DEPRESS: Depression
#'   \item CMR_DIAB_CX: Diabetes with complications
#'   \item CMR_DIAB_UNCX: Diabetes without complications
#'   \item CMR_DRUG_ABUSE: Drug abuse
#'   \item CMR_HTN_CX: Hypertension with complications
#'   \item CMR_HTN_UNCX: Hypertension without complications
#'   \item CMR_LIVER_MLD: Mild liver disease
#'   \item CMR_LIVER_SEV: Severe liver disease
#'   \item CMR_LUNG_CHRONIC: Chronic lung disease
#'   \item CMR_NEURO_MOVT: Neurological movement disorders
#'   \item CMR_NEURO_OTH: Other neurological disorders
#'   \item CMR_NEURO_SEIZ: Seizure disorders
#'   \item CMR_OBESE: Obesity
#'   \item CMR_PARALYSIS: Paralysis
#'   \item CMR_PERIVASC: Peripheral vascular disease
#'   \item CMR_PSYCHOSES: Psychoses
#'   \item CMR_PULMCIRC: Pulmonary circulation disorders
#'   \item CMR_RENLFL_MOD: Moderate renal failure
#'   \item CMR_RENLFL_SEV: Severe renal failure
#'   \item CMR_THYROID_HYPO: Hypothyroid disorders
#'   \item CMR_THYROID_OTH: Other thyroid disorders
#'   \item CMR_ULCER_PEPTIC: Peptic ulcer disease
#'   \item CMR_VALVE: Valvular disease
#'   \item CMR_WGHTLOSS: Weight loss
#' }
#' @examples
#' \dontrun{
#' # Simple workflow - uses built-in lookup data automatically
#' result <- comorbidity(patient_data, 
#'                       dx_cols = c("dx2", "dx3", "dx4"),
#'                       poa_cols = c("poa2", "poa3", "poa4"))
#' 
#' # Calculate risk indices
#' result_with_indices <- cmr_index(result)
#' 
#' # Advanced workflow with custom lookup data
#' custom_comfmt <- build_comfmt_from_csv("path/to/custom_lookup.csv")
#' result <- comorbidity(patient_data,
#'                       dx_cols = c("dx2", "dx3", "dx4"),
#'                       comfmt = custom_comfmt)
#'
#' # Split the encounters across 4 forked workers
#' result <- comorbidity(patient_data,
#'                       dx_cols = c("dx2", "dx3", "dx4"),
#'                       poa_cols = c("poa2", "poa3", "poa4"),
#'                       ncores = 4)
#' }
#' @export
comorbidity <- function(patient_data,
                        dx_cols,
                        poa_cols = NULL,
                        year_col = "year",
                        quarter_col = "quarter",
                        release = cmr_version(),
                        comfmt = NULL,
                        poa_exempt = NULL,
                        use_poa = TRUE,
                        wildcard_mode = "wildcard",
                        ncores = 1) {

  # Input validation
  stopifnot(all(dx_cols %in% names(patient_data)))
  if (!is.null(poa_cols)) stopifnot(length(poa_cols) == length(dx_cols))
  release <- .resolve_release(release)
  ncores <- .resolve_ncores(ncores)

  # Use default lookup data if not provided.
  #
  # Built here in the parent, BEFORE any fork, so that a chunked run builds the
  # tables once rather than once per worker; the workers inherit the finished
  # tables through the fork. They are also memoised per release, so looping over
  # releases - or calling comorbidity() repeatedly - reparses nothing.
  #
  # The cache must be populated here and the tables passed down as arguments. A
  # fork child's writes are discarded when it exits, so a worker that reached
  # into the cache itself would silently rebuild the tables once per worker.
  if (is.null(comfmt)) {
    comfmt <- .comfmt_for_release(release)
  }

  # Fails loudly on a target the pipeline would otherwise drop in silence.
  .validate_comfmt_targets(comfmt, release)

  if (use_poa && is.null(poa_exempt)) {
    poa_exempt <- .poa_exempt_default()
  }

  if (use_poa && !is.null(poa_exempt)) {
    if (!year_col %in% names(patient_data)) {
      stop(paste("Year column", year_col, "not found in patient diagnosis data"))
    }
    if (!quarter_col %in% names(patient_data)) {
      stop(paste("Quarter column", quarter_col, "not found in patient diagnosis data"))
    }
  }
  
  n_rows <- nrow(patient_data)

  # The release's ICD-10-CM ceiling. Encounters dated past the release's coverage
  # clamp to it, exactly as the release's own SAS ladder does.
  max_icd_version <- .release_max_icd_version(release)

  # Chunks cannot outnumber rows. This is a validity guard, not a performance
  # threshold - ncores is honoured as requested on frames of any size.
  chunks <- min(ncores, max(n_rows, 1L))

  if (chunks > 1L) {
    row_blocks <- split(seq_len(n_rows), cut(seq_len(n_rows), chunks, labels = FALSE))
    pieces <- parallel::mclapply(row_blocks, function(rows) {
      .comorbidity_flags(patient_data[rows, , drop = FALSE], dx_cols, poa_cols,
                         year_col, quarter_col, comfmt, poa_exempt, use_poa,
                         wildcard_mode, max_icd_version)
    }, mc.cores = chunks)

    # mclapply reports worker errors as try-error elements instead of raising them.
    # Without this check a failed worker would silently contribute a block of 0
    # flags for its encounters rather than an error.
    failed <- vapply(pieces, inherits, logical(1), "try-error")
    if (any(failed)) {
      stop("parallel chunk processing failed on ", sum(failed), " of ", length(pieces),
           " chunk(s): ",
           conditionMessage(attr(pieces[[which(failed)[1]]], "condition")), call. = FALSE)
    }

    result_matrix <- do.call(rbind, pieces)
  } else {
    result_matrix <- .comorbidity_flags(patient_data, dx_cols, poa_cols, year_col,
                                        quarter_col, comfmt, poa_exempt, use_poa,
                                        wildcard_mode, max_icd_version)
  }

  # Convert matrix to data frame and bind to original data
  out <- dplyr::bind_cols(patient_data, as.data.frame(result_matrix))

  # Record the release as an attribute rather than a column, so the result schema
  # is unchanged and readr::write_csv() ignores it. cmr_index() reads it to warn
  # about a release mismatch; it never uses it as a default, since `[`-subsetting
  # a data frame drops attributes and silent action at a distance would be worse
  # than the mismatch it was meant to catch.
  attr(out, "cmr_release") <- release
  out
}

#' Run the comorbidity pipeline over one block of encounters
#'
#' Internal. Takes lookup tables that are already built and returns only the
#' 38-column CMR flag matrix, one row per row of \code{patient_data} - not the
#' input columns bound back on, which would make every worker ship the caller's
#' whole frame back through a pipe.
#'
#' Every stage below is per-encounter: \code{row_id} is a row number over the
#' frame passed in and \code{n_rows} is its row count, so both are local to the
#' block. Running this on a contiguous block therefore produces exactly the rows
#' the whole-dataset call would have produced, which is what makes the chunked
#' path in \code{comorbidity()} identical to the serial one rather than merely
#' similar.
#' @keywords internal
.comorbidity_flags <- function(patient_data, dx_cols, poa_cols, year_col,
                               quarter_col, comfmt, poa_exempt, use_poa,
                               wildcard_mode, max_icd_version = 43L) {

  n_rows <- nrow(patient_data)

  # Pre-compile regex patterns (major optimization)
  if (wildcard_mode == "wildcard") {
    comfmt_compiled <- comfmt %>%
      dplyr::mutate(.regex = paste0("^", stringr::str_replace_all(pattern, "%", ".*"), "$"))
  } else {
    comfmt_compiled <- comfmt %>% dplyr::mutate(.regex = pattern)
  }
  
  # Convert regex patterns to compiled form for faster matching
  regex_list <- purrr::map(comfmt_compiled$.regex, ~ stringr::regex(.x, ignore_case = TRUE))
  
  # Add row identifiers and determine ICD versions efficiently
  claims_prep <- patient_data %>%
    dplyr::mutate(row_id = dplyr::row_number())
  
  if (use_poa && !is.null(poa_exempt)) {
    claims_prep <- claims_prep %>%
      dplyr::mutate(
        icd_version = determine_icd_version(
          as.integer(.data[[year_col]]),
          as.integer(.data[[quarter_col]]),
          max_icd_version
        )
      )

    # Every ICD version reached must have a POA-exempt list, or the left_join
    # below yields is_exempt = NA -> FALSE for its rows. That failure is silent
    # and moves flags in BOTH directions: POA-dependent targets lose their
    # exempt-code assignments, and .handle_special_cases() starts firing
    # CBVD_NPOA on rows it never should, which suppresses CMR_CBVD. The built-in
    # data covers v33-v43, so only a user-supplied poa_exempt can trip this.
    have <- as.integer(stringr::str_extract(names(poa_exempt), "\\d+"))
    missing_v <- setdiff(unique(claims_prep$icd_version), have)
    if (length(missing_v)) {
      stop("poa_exempt has no code list for ICD-10-CM version(s) ",
           paste(sort(missing_v), collapse = ", "),
           "; it covers ", paste(sort(have), collapse = ", "), call. = FALSE)
    }
  }
  
  # OPTIMIZATION 1: Reshape to long format once (instead of processing row by row)
  dx_long <- claims_prep %>%
    dplyr::select(row_id, dplyr::all_of(dx_cols), 
                  if(use_poa && !is.null(poa_exempt)) "icd_version" else NULL) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(dx_cols),
      names_to = "dx_position",
      values_to = "dx_code",
      values_drop_na = TRUE
    ) %>%
    dplyr::filter(dx_code != "") %>%
    dplyr::mutate(
      dx_code = normalize_icd10(as.character(dx_code)),
      dx_position_num = as.integer(stringr::str_extract(dx_position, "\\d+"))
    )
  
  # Add POA information if available
  if (!is.null(poa_cols)) {
    poa_long <- claims_prep %>%
      dplyr::select(row_id, dplyr::all_of(poa_cols)) %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(poa_cols),
        names_to = "poa_position", 
        values_to = "poa_code",
        values_drop_na = TRUE
      ) %>%
      dplyr::mutate(
        poa_position_num = as.integer(stringr::str_extract(poa_position, "\\d+")),
        poa_code = toupper(as.character(poa_code))
      ) %>%
      dplyr::select(row_id, poa_position_num, poa_code)

    # dx and POA columns are paired by the digits in their NAMES, not by
    # position in the vectors, and str_extract() takes the FIRST run of digits.
    # So HCUP's own naming - I10_DX2 paired with DXPOA2 - yields positions 10 and
    # 2, which join to nothing: every POA lookup misses, coalesces to "", and
    # every POA-dependent target silently stops flagging on non-exempt codes.
    # An empty intersection is never legitimate, so fail loudly instead.
    dx_pos  <- unique(stats::na.omit(dx_long$dx_position_num))
    poa_pos <- unique(stats::na.omit(poa_long$poa_position_num))
    if (length(dx_pos) && length(poa_pos) && !length(intersect(dx_pos, poa_pos))) {
      stop("no dx/POA column pairs: dx columns give position(s) ",
           paste(sort(dx_pos), collapse = ", "), " but POA columns give ",
           paste(sort(poa_pos), collapse = ", "), ".\n",
           "  Columns are paired by the first run of digits in their names, so ",
           "names like \"I10_DX2\" resolve to 10, not 2.\n",
           "  Rename to a form whose only digits are the position, e.g. dx2/poa2.",
           call. = FALSE)
    }

    dx_long <- dx_long %>%
      dplyr::left_join(poa_long, by = c("row_id", "dx_position_num" = "poa_position_num")) %>%
      dplyr::mutate(poa_code = dplyr::coalesce(poa_code, ""))
  } else {
    dx_long$poa_code <- ""
  }
  
  # OPTIMIZATION 2: Vectorized pattern matching
  unique_codes <- unique(dx_long$dx_code)

  # Create mapping of codes to targets efficiently.
  #
  # `priority` is the pattern's index in comfmt, which is what resolves overlapping
  # patterns below. Because that index is a property of the lookup table rather than
  # of the data, every chunk resolves an overlap the same way, and a chunk's map is
  # an exact restriction of the whole-dataset map to the codes the chunk contains.
  # That is what makes the ncores > 1 result identical to the serial one rather than
  # merely similar.
  match_block <- function(idx) {
    purrr::map_dfr(idx, function(i) {
      matching_codes <- unique_codes[stringr::str_detect(unique_codes, regex_list[[i]])]
      if (length(matching_codes) == 0L) return(NULL)
      dplyr::tibble(
        dx_code = matching_codes,
        target = comfmt_compiled$target[i],
        priority = i  # for handling overlapping patterns
      )
    })
  }

  n_patterns <- nrow(comfmt_compiled)

  code_target_map <- match_block(seq_len(n_patterns))

  # A code set that matches nothing yields a 0-column frame, which the arrange() below
  # cannot resolve columns against. Give it the schema explicitly.
  if (nrow(code_target_map) == 0L) {
    code_target_map <- dplyr::tibble(
      dx_code = character(), target = character(), priority = integer()
    )
  }

  # Handle overlapping patterns by keeping first match per code
  code_target_map <- code_target_map %>%
    dplyr::arrange(dx_code, priority) %>%
    dplyr::group_by(dx_code) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-priority)
  
  # Join targets back to diagnosis data
  dx_with_targets <- dx_long %>%
    dplyr::inner_join(code_target_map, by = "dx_code")
  
  # OPTIMIZATION 3: Efficient POA exempt lookup
  if (use_poa && !is.null(poa_exempt)) {
    poa_exempt_lookup <- purrr::map_dfr(names(poa_exempt), function(v) {
      version_num <- as.integer(stringr::str_extract(v, "\\d+"))
      dplyr::tibble(
        icd_version = version_num,
        dx_code = poa_exempt[[v]],
        is_exempt = TRUE
      )
    })
    
    dx_with_targets <- dx_with_targets %>%
      dplyr::left_join(poa_exempt_lookup, by = c("icd_version", "dx_code")) %>%
      dplyr::mutate(is_exempt = dplyr::coalesce(is_exempt, FALSE))
  } else {
    dx_with_targets$is_exempt <- FALSE
  }
  
  # Apply business rules and build the flag matrix for this block
  .apply_comorbidity_rules(dx_with_targets, n_rows, use_poa)
}

#' Validate and clamp a requested core count
#'
#' Internal helper. Returns a usable positive integer core count, warning rather
#' than erroring when the request is merely unsatisfiable (too many cores, or
#' Windows, where \code{parallel::mclapply} cannot fork).
#' @keywords internal
.resolve_ncores <- function(ncores) {
  ncores <- suppressWarnings(as.integer(ncores))
  if (length(ncores) != 1L || is.na(ncores) || ncores < 1L) {
    stop("ncores must be a single positive integer", call. = FALSE)
  }

  available <- parallel::detectCores(logical = TRUE)
  if (!is.na(available) && ncores > available) {
    warning("ncores = ", ncores, " exceeds the ", available,
            " available core(s); using ", available, call. = FALSE)
    ncores <- as.integer(available)
  }

  if (ncores > 1L && .Platform$OS.type == "windows") {
    warning("parallel::mclapply cannot fork on Windows; running serially (ncores = 1)",
            call. = FALSE)
    ncores <- 1L
  }

  ncores
}

#' Internal function to apply comorbidity business rules
#' @keywords internal
.apply_comorbidity_rules <- function(dx_with_targets, n_rows, use_poa) {
  
  # Comorbidity categories - see R/releases.R for the vectors and why they are
  # deliberately not release-aware.
  poa_neutral   <- CMR_POA_NEUTRAL
  poa_dependent <- CMR_POA_DEPENDENT


  # Apply assignment rules
  valid_assignments <- dx_with_targets %>%
    dplyr::mutate(
      should_assign = dplyr::case_when(
        target %in% poa_neutral ~ TRUE,
        !use_poa & target %in% poa_dependent ~ TRUE,
        use_poa & target %in% poa_dependent & (is_exempt | poa_code %in% c("Y", "W")) ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    dplyr::filter(should_assign)
  
  # Handle special cases and combination codes
  all_assignments <- .handle_special_cases(valid_assignments, dx_with_targets, use_poa)
  
  # Build and return result matrix
  .build_result_matrix(all_assignments, n_rows, use_poa)
}

#' Handle special cases and combination codes
#' @keywords internal
.handle_special_cases <- function(valid_assignments, dx_with_targets, use_poa) {
  
  special_assignments <- dplyr::tibble()
  
  if (use_poa && nrow(dx_with_targets) > 0) {
    # CBVD_NPOA case
    cbvd_npoa <- dx_with_targets %>%
      dplyr::filter(target == "CBVD_POA", !is_exempt, poa_code %in% c("N", "U")) %>%
      dplyr::mutate(target = "CBVD_NPOA", should_assign = TRUE) %>%
      dplyr::select(names(valid_assignments))
    
    special_assignments <- dplyr::bind_rows(special_assignments, cbvd_npoa)
  }
  
  # Handle combination codes
  combination_assignments <- .handle_combination_codes(dx_with_targets, use_poa)
  
  # Combine all valid assignments
  dplyr::bind_rows(valid_assignments, special_assignments, combination_assignments)
}

#' Handle combination codes logic
#' @keywords internal
.handle_combination_codes <- function(dx_with_targets, use_poa) {
  
  combination_assignments <- dplyr::tibble()
  
  if (nrow(dx_with_targets) == 0) return(combination_assignments)
  
  combo_codes <- CMR_COMBO_TARGETS


  combo_data <- dx_with_targets %>% dplyr::filter(target %in% combo_codes)
  
  if (nrow(combo_data) == 0) return(combination_assignments)
  
  for (i in seq_len(nrow(combo_data))) {
    row_data <- combo_data[i, ]
    target <- row_data$target
    
    # Apply combination logic based on target
    if (target == "DRUG_ABUSEPSYCHOSES") {
      combination_assignments <- dplyr::bind_rows(combination_assignments,
        row_data %>% dplyr::mutate(target = "DRUG_ABUSE", should_assign = TRUE))
      if (use_poa && (row_data$is_exempt | row_data$poa_code %in% c("Y", "W"))) {
        combination_assignments <- dplyr::bind_rows(combination_assignments,
          row_data %>% dplyr::mutate(target = "PSYCHOSES", should_assign = TRUE))
      }
    }
    
    if (target == "HFHTN_CX") {
      combination_assignments <- dplyr::bind_rows(combination_assignments,
        row_data %>% dplyr::mutate(target = "HTN_CX", should_assign = TRUE))
      if (use_poa && (row_data$is_exempt | row_data$poa_code %in% c("Y", "W"))) {
        combination_assignments <- dplyr::bind_rows(combination_assignments,
          row_data %>% dplyr::mutate(target = "HF", should_assign = TRUE))
      }
    }
    
    if (target == "HTN_CXRENLFL_SEV") {
      combination_assignments <- dplyr::bind_rows(combination_assignments,
        row_data %>% dplyr::mutate(target = "HTN_CX", should_assign = TRUE))
      if (use_poa && (row_data$is_exempt | row_data$poa_code %in% c("Y", "W"))) {
        combination_assignments <- dplyr::bind_rows(combination_assignments,
          row_data %>% dplyr::mutate(target = "RENLFL_SEV", should_assign = TRUE))
      }
    }
    
    if (target == "HFHTN_CXRENLFL_SEV") {
      combination_assignments <- dplyr::bind_rows(combination_assignments,
        row_data %>% dplyr::mutate(target = "HTN_CX", should_assign = TRUE))
      if (use_poa && (row_data$is_exempt | row_data$poa_code %in% c("Y", "W"))) {
        combination_assignments <- dplyr::bind_rows(combination_assignments,
          row_data %>% dplyr::mutate(target = "HF", should_assign = TRUE),
          row_data %>% dplyr::mutate(target = "RENLFL_SEV", should_assign = TRUE))
      }
    }
    
    if (target == "ALCOHOLLIVER_MLD") {
      combination_assignments <- dplyr::bind_rows(combination_assignments,
        row_data %>% dplyr::mutate(target = "ALCOHOL", should_assign = TRUE))
      if (use_poa && (row_data$is_exempt | row_data$poa_code %in% c("Y", "W"))) {
        combination_assignments <- dplyr::bind_rows(combination_assignments,
          row_data %>% dplyr::mutate(target = "LIVER_MLD", should_assign = TRUE))
      }
    }
    
    if (target == "VALVE_AUTOIMMUNE") {
      combination_assignments <- dplyr::bind_rows(combination_assignments,
        row_data %>% dplyr::mutate(target = "AUTOIMMUNE", should_assign = TRUE))
      if (use_poa && (row_data$is_exempt | row_data$poa_code %in% c("Y", "W"))) {
        combination_assignments <- dplyr::bind_rows(combination_assignments,
          row_data %>% dplyr::mutate(target = "VALVE", should_assign = TRUE))
      }
    }
    
    if (target == "CBVD_SQLAPARALYSIS") {
      if (use_poa && (row_data$is_exempt | row_data$poa_code %in% c("Y", "W"))) {
        combination_assignments <- dplyr::bind_rows(combination_assignments,
          row_data %>% dplyr::mutate(target = "PARALYSIS", should_assign = TRUE),
          row_data %>% dplyr::mutate(target = "CBVD_SQLA", should_assign = TRUE))
      }
    }
    
    if (target == "LIVER_MLD_NEURO") {
      if (use_poa && (row_data$is_exempt | row_data$poa_code %in% c("Y", "W"))) {
        combination_assignments <- dplyr::bind_rows(combination_assignments,
          row_data %>% dplyr::mutate(target = "LIVER_MLD", should_assign = TRUE),
          row_data %>% dplyr::mutate(target = "NEURO_OTH", should_assign = TRUE))
      }
    }
    
    if (target == "NEURO_OTH_SEIZ") {
      if (use_poa && (row_data$is_exempt | row_data$poa_code %in% c("Y", "W"))) {
        combination_assignments <- dplyr::bind_rows(combination_assignments,
          row_data %>% dplyr::mutate(target = "NEURO_OTH", should_assign = TRUE),
          row_data %>% dplyr::mutate(target = "NEURO_SEIZ", should_assign = TRUE))
      }
    }

    if (target == "LIVER_MLD_PULMCIRC") {
      if (use_poa && (row_data$is_exempt | row_data$poa_code %in% c("Y", "W"))) {
        combination_assignments <- dplyr::bind_rows(combination_assignments,
          row_data %>% dplyr::mutate(target = "LIVER_MLD", should_assign = TRUE),
          row_data %>% dplyr::mutate(target = "PULMCIRC", should_assign = TRUE))
      }
    }
  }
  
  combination_assignments
}

#' Build the final result matrix with comorbidity flags
#' @keywords internal
.build_result_matrix <- function(all_assignments, n_rows, use_poa) {
  
  final_targets <- c(
    "CMR_AIDS", "CMR_ALCOHOL", "CMR_ANEMDEF", "CMR_AUTOIMMUNE", "CMR_BLDLOSS",
    "CMR_CANCER_LEUK", "CMR_CANCER_LYMPH", "CMR_CANCER_METS", "CMR_CANCER_NSITU", 
    "CMR_CANCER_SOLID", "CMR_CBVD", "CMR_HF", "CMR_COAG", "CMR_DEMENTIA", 
    "CMR_DEPRESS", "CMR_DIAB_CX", "CMR_DIAB_UNCX", "CMR_DRUG_ABUSE", 
    "CMR_HTN_CX", "CMR_HTN_UNCX", "CMR_LIVER_MLD", "CMR_LIVER_SEV", 
    "CMR_LUNG_CHRONIC", "CMR_NEURO_MOVT", "CMR_NEURO_OTH", "CMR_NEURO_SEIZ", 
    "CMR_OBESE", "CMR_PARALYSIS", "CMR_PERIVASC", "CMR_PSYCHOSES", 
    "CMR_PULMCIRC", "CMR_RENLFL_MOD", "CMR_RENLFL_SEV", "CMR_THYROID_HYPO", 
    "CMR_THYROID_OTH", "CMR_ULCER_PEPTIC", "CMR_VALVE", "CMR_WGHTLOSS"
  )
  
  # Include intermediate targets for CBVD derivation
  intermediate_targets <- c("CMR_CBVD_POA", "CMR_CBVD_NPOA", "CMR_CBVD_SQLA")
  working_targets <- c(final_targets, intermediate_targets)
  
  # Initialize result matrix
  result_matrix <- matrix(0L, nrow = n_rows, ncol = length(working_targets))
  colnames(result_matrix) <- working_targets
  
  # Populate matrix
  if (nrow(all_assignments) > 0) {
    row_targets <- all_assignments %>%
      dplyr::mutate(cmr_target = paste0("CMR_", target)) %>%
      dplyr::filter(cmr_target %in% working_targets) %>%
      dplyr::group_by(row_id, cmr_target) %>%
      dplyr::summarise(.groups = "drop") %>%
      dplyr::mutate(
        row_idx = match(row_id, seq_len(n_rows)),
        col_idx = match(cmr_target, working_targets)
      ) %>%
      dplyr::filter(!is.na(row_idx), !is.na(col_idx))
    
    if (nrow(row_targets) > 0) {
      result_matrix[cbind(row_targets$row_idx, row_targets$col_idx)] <- 1L
    }
  }
  
  # Apply exclusion rules and derive final flags
  result_matrix <- .apply_exclusion_rules(result_matrix, use_poa, n_rows)
  
  # Return only final targets
  result_matrix[, final_targets, drop = FALSE]
}

#' Apply hierarchical exclusion rules
#' @keywords internal
.apply_exclusion_rules <- function(result_matrix, use_poa, n_rows) {
  
  # Basic exclusions
  result_matrix[, "CMR_DIAB_UNCX"] <- ifelse(result_matrix[, "CMR_DIAB_CX"] == 1, 0L, result_matrix[, "CMR_DIAB_UNCX"])
  result_matrix[, "CMR_HTN_UNCX"] <- ifelse(result_matrix[, "CMR_HTN_CX"] == 1, 0L, result_matrix[, "CMR_HTN_UNCX"])
  
  # Cancer hierarchy
  mets_mask <- result_matrix[, "CMR_CANCER_METS"] == 1
  result_matrix[mets_mask, "CMR_CANCER_SOLID"] <- 0L
  result_matrix[mets_mask, "CMR_CANCER_NSITU"] <- 0L
  
  solid_mask <- result_matrix[, "CMR_CANCER_SOLID"] == 1
  result_matrix[solid_mask, "CMR_CANCER_NSITU"] <- 0L
  
  if (use_poa) {
    result_matrix[, "CMR_LIVER_MLD"] <- ifelse(result_matrix[, "CMR_LIVER_SEV"] == 1, 0L, result_matrix[, "CMR_LIVER_MLD"])
    result_matrix[, "CMR_RENLFL_MOD"] <- ifelse(result_matrix[, "CMR_RENLFL_SEV"] == 1, 0L, result_matrix[, "CMR_RENLFL_MOD"])
    
    # Handle CBVD derivation
    if ("CMR_CBVD_POA" %in% colnames(result_matrix) && "CMR_CBVD_SQLA" %in% colnames(result_matrix)) {
      cbvd_poa_mask <- result_matrix[, "CMR_CBVD_POA"] == 1
      cbvd_npoa_mask <- if ("CMR_CBVD_NPOA" %in% colnames(result_matrix)) {
        result_matrix[, "CMR_CBVD_NPOA"] == 1
      } else {
        rep(FALSE, n_rows)
      }
      cbvd_sqla_mask <- result_matrix[, "CMR_CBVD_SQLA"] == 1
      
      result_matrix[, "CMR_CBVD"] <- as.integer(cbvd_poa_mask | (!cbvd_poa_mask & !cbvd_npoa_mask & cbvd_sqla_mask))
    }
  }
  
  result_matrix
}