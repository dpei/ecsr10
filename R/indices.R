#' Calculate CMR mortality and readmission indices
#'
#' Calculates weighted risk scores for hospital readmission and mortality based on 
#' the 38 CMR comorbidity flags. Uses established weights from the CMR methodology.
#'
#' @param core_data Data frame with comorbidity flags (output from comorbidity function)
#' @return Data frame with two additional columns: CMR_Index_Readmission and CMR_Index_Mortality
#' @details
#' The function applies validated weights to each comorbidity category:
#' \itemize{
#'   \item Readmission weights range from -2 (obesity) to 11 (metastatic cancer)
#'   \item Mortality weights range from -9 (psychoses) to 22 (metastatic cancer, neurological disorders)
#'   \item Missing comorbidity flags are treated as 0
#'   \item Final scores are unweighted sums of individual comorbidity contributions
#' }
#' @examples
#' \dontrun{
#' # After running comorbidity analysis
#' result_with_comorbidities <- comorbidity(patient_data, ...)
#' result_with_indices <- cmr_index(result_with_comorbidities)
#' 
#' # View index distributions
#' summary(result_with_indices$CMR_Index_Readmission)
#' summary(result_with_indices$CMR_Index_Mortality)
#' }
#' @export
cmr_index <- function(core_data) {
  
  result_data <- core_data
  
  # Define readmission weights
  rw <- list(
    CMR_AIDS = 5, CMR_ALCOHOL = 3, CMR_ANEMDEF = 5, CMR_AUTOIMMUNE = 2,
    CMR_BLDLOSS = 2, CMR_CANCER_LEUK = 10, CMR_CANCER_LYMPH = 7,
    CMR_CANCER_METS = 11, CMR_CANCER_NSITU = 0, CMR_CANCER_SOLID = 7,
    CMR_CBVD = 0, CMR_HF = 7, CMR_COAG = 3, CMR_DEMENTIA = 1,
    CMR_DEPRESS = 2, CMR_DIAB_CX = 4, CMR_DIAB_UNCX = 0,
    CMR_DRUG_ABUSE = 6, CMR_HTN_CX = 0, CMR_HTN_UNCX = 0,
    CMR_LIVER_MLD = 3, CMR_LIVER_SEV = 10, CMR_LUNG_CHRONIC = 4,
    CMR_NEURO_MOVT = 1, CMR_NEURO_OTH = 2, CMR_NEURO_SEIZ = 5,
    CMR_OBESE = -2, CMR_PARALYSIS = 3, CMR_PERIVASC = 1,
    CMR_PSYCHOSES = 6, CMR_PULMCIRC = 3, CMR_RENLFL_MOD = 4,
    CMR_RENLFL_SEV = 8, CMR_THYROID_HYPO = 0, CMR_THYROID_OTH = 0,
    CMR_ULCER_PEPTIC = 2, CMR_VALVE = 0, CMR_WGHTLOSS = 6
  )
  
  # Define mortality weights
  mw <- list(
    CMR_AIDS = -4, CMR_ALCOHOL = -1, CMR_ANEMDEF = -3, CMR_AUTOIMMUNE = 0,
    CMR_BLDLOSS = -4, CMR_CANCER_LEUK = 9, CMR_CANCER_LYMPH = 5,
    CMR_CANCER_METS = 22, CMR_CANCER_NSITU = 0, CMR_CANCER_SOLID = 10,
    CMR_CBVD = 5, CMR_HF = 14, CMR_COAG = 14, CMR_DEMENTIA = 5,
    CMR_DEPRESS = -8, CMR_DIAB_CX = -2, CMR_DIAB_UNCX = 0,
    CMR_DRUG_ABUSE = -7, CMR_HTN_CX = 1, CMR_HTN_UNCX = 0,
    CMR_LIVER_MLD = 2, CMR_LIVER_SEV = 16, CMR_LUNG_CHRONIC = 2,
    CMR_NEURO_MOVT = -1, CMR_NEURO_OTH = 22, CMR_NEURO_SEIZ = 2,
    CMR_OBESE = -7, CMR_PARALYSIS = 4, CMR_PERIVASC = 3,
    CMR_PSYCHOSES = -9, CMR_PULMCIRC = 4, CMR_RENLFL_MOD = 3,
    CMR_RENLFL_SEV = 7, CMR_THYROID_HYPO = -3, CMR_THYROID_OTH = -8,
    CMR_ULCER_PEPTIC = 0, CMR_VALVE = 0, CMR_WGHTLOSS = 13
  )
  
  # Calculate indices for each record
  result_data$CMR_Index_Readmission <- 0
  result_data$CMR_Index_Mortality <- 0
  
  for (i in 1:nrow(result_data)) {
    readmit_score <- 0
    mort_score <- 0
    
    for (cmr_var in names(rw)) {
      if (cmr_var %in% names(result_data)) {
        cmr_value <- result_data[[cmr_var]][i]
        if (!is.na(cmr_value)) {
          readmit_score <- readmit_score + (cmr_value * rw[[cmr_var]])
          mort_score <- mort_score + (cmr_value * mw[[cmr_var]])
        }
      }
    }
    
    result_data$CMR_Index_Readmission[i] <- readmit_score
    result_data$CMR_Index_Mortality[i] <- mort_score
  }
  
  return(result_data)
}

#' Print summary statistics for CMR indices
#'
#' Internal function that displays descriptive statistics for the calculated CMR 
#' readmission and mortality indices. Users can access summary statistics through 
#' other means in the package workflow.
#'
#' @param data Data frame containing CMR indices (output from cmr_index function)
#' @return None (prints to console)
#' @keywords internal
print_cmr_summary <- function(data) {
  cat("\n=== CMR Index Summary Statistics ===\n")
  
  indices <- c("CMR_Index_Readmission", "CMR_Index_Mortality")
  
  for (idx in indices) {
    if (idx %in% names(data)) {
      values <- data[[idx]]
      cat(sprintf("\n%s:\n", idx))
      cat(sprintf("  N: %d\n", sum(!is.na(values))))
      cat(sprintf("  Missing: %d\n", sum(is.na(values))))
      cat(sprintf("  Mean: %.2f\n", mean(values, na.rm = TRUE)))
      cat(sprintf("  SD: %.2f\n", sd(values, na.rm = TRUE)))
      cat(sprintf("  Min: %.0f\n", min(values, na.rm = TRUE)))
      cat(sprintf("  Max: %.0f\n", max(values, na.rm = TRUE)))
      
      # Additional percentile information
      percentiles <- stats::quantile(values, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
      cat(sprintf("  25th percentile: %.1f\n", percentiles[[1]]))
      cat(sprintf("  50th percentile (median): %.1f\n", percentiles[[2]]))
      cat(sprintf("  75th percentile: %.1f\n", percentiles[[3]]))
    } else {
      cat(sprintf("\nWARNING: %s not found in data\n", idx))
    }
  }
  cat("\n")
}