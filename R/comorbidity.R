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
#' @param comfmt Data frame with comorbidity format containing "target" and "pattern" columns.
#'   If NULL (default), uses the built-in comfmt_lookup data.
#' @param poa_exempt Named list of POA exempt codes by version. If NULL (default),
#'   uses the built-in poaxmpt_codes_long data when use_poa is TRUE.
#' @param use_poa Logical, whether to apply POA logic (default TRUE)
#' @param wildcard_mode Character, pattern matching mode ("wildcard" or "regex")
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
#' }
#' @export
comorbidity <- function(patient_data,
                        dx_cols,
                        poa_cols = NULL,
                        year_col = "year",
                        quarter_col = "quarter",
                        comfmt = NULL,
                        poa_exempt = NULL,
                        use_poa = TRUE,
                        wildcard_mode = "wildcard") {
  
  # Input validation
  stopifnot(all(dx_cols %in% names(patient_data)))
  if (!is.null(poa_cols)) stopifnot(length(poa_cols) == length(dx_cols))
  
  # Use default lookup data if not provided
  if (is.null(comfmt)) {
    # Load built-in comorbidity lookup data
    data("comfmt_lookup", package = "ecsr10", envir = environment())
    # Create temporary file for processing
    temp_comfmt_file <- tempfile(fileext = ".csv")
    readr::write_csv(comfmt_lookup, temp_comfmt_file)
    comfmt <- build_comfmt_from_csv(temp_comfmt_file, mode = wildcard_mode)
    unlink(temp_comfmt_file)
  }
  
  if (use_poa && is.null(poa_exempt)) {
    # Load built-in POA exempt data
    data("poaxmpt_codes_long", package = "ecsr10", envir = environment())
    # Create temporary file for processing  
    temp_poa_file <- tempfile(fileext = ".csv")
    readr::write_csv(poaxmpt_codes_long, temp_poa_file)
    poa_exempt <- build_poa_exempt_formats(temp_poa_file)
    unlink(temp_poa_file)
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
          as.integer(.data[[quarter_col]])
        )
      )
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
    
    dx_long <- dx_long %>%
      dplyr::left_join(poa_long, by = c("row_id", "dx_position_num" = "poa_position_num")) %>%
      dplyr::mutate(poa_code = dplyr::coalesce(poa_code, ""))
  } else {
    dx_long$poa_code <- ""
  }
  
  # OPTIMIZATION 2: Vectorized pattern matching
  unique_codes <- unique(dx_long$dx_code)
  
  # Create mapping of codes to targets efficiently
  code_target_map <- dplyr::tibble()
  
  for (i in seq_len(nrow(comfmt_compiled))) {
    matching_codes <- unique_codes[stringr::str_detect(unique_codes, regex_list[[i]])]
    
    if (length(matching_codes) > 0) {
      code_target_map <- dplyr::bind_rows(
        code_target_map,
        dplyr::tibble(
          dx_code = matching_codes,
          target = comfmt_compiled$target[i],
          priority = i  # for handling overlapping patterns
        )
      )
    }
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
  
  # Apply business rules and build result matrix
  result_matrix <- .apply_comorbidity_rules(dx_with_targets, n_rows, use_poa)
  
  # Convert matrix to data frame and bind to original data
  result_df <- as.data.frame(result_matrix)
  dplyr::bind_cols(patient_data, result_df)
}

#' Internal function to apply comorbidity business rules
#' @keywords internal
.apply_comorbidity_rules <- function(dx_with_targets, n_rows, use_poa) {
  
  # Define comorbidity categories
  poa_neutral <- c("AIDS", "ALCOHOL", "AUTOIMMUNE", "LUNG_CHRONIC", "DEMENTIA", 
                   "DEPRESS", "DIAB_UNCX", "DIAB_CX", "DRUG_ABUSE", "HTN_UNCX", 
                   "HTN_CX", "THYROID_HYPO", "THYROID_OTH", "CANCER_LYMPH", 
                   "CANCER_LEUK", "CANCER_METS", "OBESE", "PERIVASC", 
                   "CANCER_SOLID", "CANCER_NSITU")
  
  poa_dependent <- c("ANEMDEF", "BLDLOSS", "HF", "COAG", "LIVER_MLD", "LIVER_SEV",
                     "NEURO_MOVT", "NEURO_SEIZ", "NEURO_OTH", "PARALYSIS", "PSYCHOSES",
                     "PULMCIRC", "RENLFL_MOD", "RENLFL_SEV", "ULCER_PEPTIC", "WGHTLOSS",
                     "CBVD_POA", "CBVD_SQLA", "VALVE")
  
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
  
  combo_codes <- c("DRUG_ABUSEPSYCHOSES", "HFHTN_CX", "HTN_CXRENLFL_SEV", 
                   "HFHTN_CXRENLFL_SEV", "ALCOHOLLIVER_MLD", "VALVE_AUTOIMMUNE", 
                   "CBVD_SQLAPARALYSIS", "LIVER_MLD_NEURO", "NEURO_OTH_SEIZ")
  
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