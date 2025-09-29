#' @keywords internal
"_PACKAGE"

#' ecsr10: Elixhauser Comorbidity Software Refined for ICD-10-CM
#'
#' The ecsr10 package provides an R implementation of the Elixhauser Comorbidity 
#' Software Refined (CMR) workflow for analyzing ICD-10-CM diagnosis codes and 
#' calculating comorbidity indices. It processes patient diagnosis data to identify 
#' 38 specific comorbidity categories and calculate mortality/readmission risk indices.
#'
#' @section Main functions:
#' \itemize{
#'   \item \code{\link{comorbidity}}: Apply comorbidity analysis to patient diagnosis data
#'   \item \code{\link{cmr_index}}: Calculate mortality and readmission risk indices
#' }
#'
#' @section Key features:
#' \itemize{
#'   \item Compatible with original SAS-based CMR methodology
#'   \item Handles Present on Admission (POA) logic and exemptions
#'   \item Processes combination codes and hierarchical exclusions
#'   \item Optimized for performance with vectorized operations
#'   \item Includes validated lookup data for ICD-10-CM mappings
#' }
#'
#' @section Workflow:
#' 1. Load and prepare patient diagnosis data with diagnosis codes (DX) and POA indicators
#' 2. Apply comorbidity analysis using \code{comorbidity()}
#' 3. Calculate risk indices using \code{cmr_index()}
#' 4. Analyze results and summary statistics
#' 
#' The package automatically uses built-in lookup data, making the workflow 
#' simple and straightforward for most users.
#'
#' @section Data requirements:
#' Patient diagnosis data should be in wide format with:
#' \itemize{
#'   \item Diagnosis code columns (DX1, DX2, ..., DXn)  
#'   \item POA indicator columns (POA1, POA2, ..., POAn)
#'   \item Year and quarter for ICD version determination
#'   \item Patient/encounter identifiers
#' }
#'
#' @section Output:
#' The package generates 38 binary comorbidity flags plus 2 risk indices:
#' \itemize{
#'   \item 38 CMR comorbidity flags (0/1 indicators)
#'   \item CMR_Index_Readmission (weighted readmission risk score)
#'   \item CMR_Index_Mortality (weighted mortality risk score)
#' }
#'
#' @aliases ecsr10-package ecsr10
#' @importFrom dplyr mutate select filter arrange group_by ungroup summarise
#'   bind_rows bind_cols left_join inner_join slice row_number all_of coalesce
#'   case_when distinct transmute tibble
#' @importFrom readr read_csv
#' @importFrom stringr str_replace_all str_detect str_squish str_extract regex
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom purrr map map_dfr discard
#' @importFrom janitor clean_names
#' @importFrom stats quantile sd
#' @importFrom utils data
#' @importFrom magrittr "%>%"
#' @name ecsr10-package
NULL

# Suppress R CMD check NOTEs about global variable bindings in tidy evaluation
#' Global variables
#' @name ecsr10-globals
#' @keywords internal
NULL

utils::globalVariables(c(
  ".data", "cmr_target", "code", "col_idx", "dx_code", "dx_position", 
  "flag", "is_exempt", "pattern", "poa_code", "poa_position", 
  "poa_position_num", "priority", "row_id", "row_idx", "should_assign", 
  "target", "version_max", "version_min", "comfmt_lookup", "poaxmpt_codes_long"
))