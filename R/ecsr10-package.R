#' @keywords internal
"_PACKAGE"

#' ecsr10: Elixhauser Comorbidity Software for ICD-10-CM
#'
#' An R implementation of AHRQ/HCUP's Elixhauser Comorbidity Software for
#' ICD-10-CM diagnosis codes, covering both families the agency has published.
#' Select between them with \code{comorbidity()}'s \code{variant} argument.
#'
#' \describe{
#'   \item{\code{"refined"} (default)}{The Elixhauser Comorbidity Software
#'     \strong{Refined} for ICD-10-CM, v2021.1 - v2026.1. 38 comorbidity
#'     measures, 18 of them identified using Present on Admission indicators,
#'     plus the readmission and mortality indices from v2022.1 on.}
#'   \item{\code{"beta"}}{The superseded \strong{beta} software,
#'     v2016.2 - v2020.1. 30 measures plus a derived \code{CMRB_HTN_C}, no POA
#'     at all, and an MS-DRG exclusion screen in its place. No indices.}
#' }
#'
#' Together the two cover ICD-10-CM diagnosis codes from October 2015 through
#' September 2026 (ICD-10-CM versions 33-43). Call \code{\link{cmr_releases}}
#' and \code{\link{cmr_version}} to query the supported set at runtime rather
#' than hardcoding it.
#'
#' @section Main functions:
#' \itemize{
#'   \item \code{\link{comorbidity}}: Apply comorbidity analysis to patient diagnosis data
#'   \item \code{\link{cmr_index}}: Calculate mortality and readmission risk indices
#'   \item \code{\link{cmr_releases}}: The AHRQ releases this package can score with
#'   \item \code{\link{cmr_version}}: The default release of either family
#' }
#'
#' @section Key features:
#' \itemize{
#'   \item Value-level parity with AHRQ's own SAS output at every supported release
#'   \item Handles Present on Admission (POA) logic and exemptions
#'   \item Handles the beta family's MS-DRG exclusion screen
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
#' @importFrom parallel mclapply detectCores
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
  "target", "version_max", "version_min", "comfmt_lookup", "poaxmpt_codes_long",
  "comfmt_releases"
))