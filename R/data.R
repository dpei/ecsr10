#' ICD-10-CM to Comorbidity Mapping Data
#'
#' A dataset containing the mapping from ICD-10-CM diagnosis codes to the 38 Elixhauser 
#' comorbidity categories used in the CMR (Comorbidity Software Refined) methodology.
#' This dataset is used by the comorbidity analysis functions to identify which 
#' diagnosis codes correspond to specific comorbidity conditions.
#'
#' @format A data frame with 2 columns and 4567 rows:
#' \describe{
#'   \item{code}{Character. ICD-10-CM diagnosis code patterns using SQL-style wildcards
#'             for pattern matching. Examples: "E11\%", "I10", "N18\%"}
#'   \item{comorbidity}{Character. The target comorbidity category name. One of 49
#'                     lookup targets - the 38 output categories plus the combination
#'                     targets (e.g. HFHTN_CX, LIVER_MLD_PULMCIRC) that fan out into
#'                     several of them.}
#' }
#' @details
#' The mapping follows the Elixhauser Comorbidity Software Refined methodology and includes:
#' \itemize{
#'   \item 38 distinct comorbidity categories
#'   \item Wildcard patterns using % for flexible code matching
#'   \item Updated codes reflecting current ICD-10-CM versions
#'   \item Both primary and secondary diagnosis code patterns
#'   \item Special combination codes for complex conditions
#' }
#'
#' This is the default release's view of \code{\link{comfmt_releases}}, kept as a
#' separate object for backwards compatibility. It is exactly the
#' \code{release == cmr_version()} slice, and both objects are written by the same
#' parse so they cannot drift. To score with a different AHRQ release, pass
#' \code{release =} to \code{\link{comorbidity}} rather than subsetting this.
#'
#' Derived from AHRQ CMR v2026.1 - see \code{\link{cmr_version}}.
#'
#' Key comorbidity categories include:
#' \itemize{
#'   \item Chronic conditions: diabetes, hypertension, heart failure, cancer
#'   \item Acute conditions: blood loss, coagulopathy, anemia
#'   \item Mental health: depression, psychoses, drug/alcohol abuse
#'   \item Neurological: paralysis, seizures, movement disorders
#'   \item Other: obesity, weight loss, liver disease, renal failure
#' }
#' @source 
#' Based on the Healthcare Cost and Utilization Project (HCUP) Elixhauser 
#' Comorbidity Software Refined for ICD-10-CM. Originally developed by AHRQ.
#' @examples
#' data(comfmt_lookup)
#' head(comfmt_lookup)
#' 
#' # View available comorbidity categories
#' unique(comfmt_lookup$comorbidity)
#' 
#' # Find diabetes-related codes
#' diabetes_codes <- comfmt_lookup[grepl("DIAB", comfmt_lookup$comorbidity), ]
#' head(diabetes_codes)
"comfmt_lookup"

#' ICD-10-CM to Comorbidity Mapping, by AHRQ Release
#'
#' The diagnosis-code-to-comorbidity mapping for every AHRQ CMR release this
#' package supports, in long form. \code{\link{comorbidity}} selects a slice of
#' this via its \code{release} argument.
#'
#' @format A data frame with 3 columns and 22323 rows:
#' \describe{
#'   \item{release}{Character. AHRQ CMR release label, one of
#'                  \code{\link{cmr_releases}()}: "2022.1" ... "2026.1".}
#'   \item{code}{Character. ICD-10-CM diagnosis code. Codes are exact; the matcher
#'               also supports \code{\%} wildcards, but no shipped release uses them.}
#'   \item{comorbidity}{Character. Target comorbidity category, including the
#'                      combination targets that fan out into several output flags.}
#' }
#' @details
#' Rows per release, reflecting AHRQ's annual additions:
#' \tabular{lrrr}{
#'   \strong{release} \tab \strong{codes} \tab \strong{targets} \tab \strong{newest ICD-10-CM version} \cr
#'   2022.1 \tab 4319 \tab 46 \tab 39 \cr
#'   2023.1 \tab 4432 \tab 47 \tab 40 \cr
#'   2024.1 \tab 4463 \tab 47 \tab 41 \cr
#'   2025.1 \tab 4542 \tab 48 \tab 42 \cr
#'   2026.1 \tab 4567 \tab 49 \tab 43 \cr
#' }
#'
#' Target sets are purely additive across these releases - AHRQ has never retired
#' one - which is why the combination rules and category vectors in the scoring
#' pipeline are release-blind. Code sets are \emph{not} nested, however: v2023.1
#' dropped O9081, O9902 and O9903, and v2026.1 dropped R939. Do not assume an
#' older release's codes are a subset of a newer one's.
#'
#' The number of output columns does not vary: every release yields the same 38
#' \code{CMR_*} flags, with categories a release does not define left at zero.
#'
#' Each release slice is sorted by code, and codes are unique within a release.
#'
#' The POA-exempt lists are deliberately \emph{not} part of this table - they are
#' keyed by ICD-10-CM version rather than AHRQ release, and live in
#' \code{\link{poaxmpt_codes_long}}.
#' @seealso \code{\link{cmr_releases}}, \code{\link{comorbidity}},
#'   \code{\link{comfmt_lookup}}
#' @source
#' Parsed from the \code{$COMFMT} block of each release's
#' \code{CMR_Format_Program_*.sas}, distributed by AHRQ/HCUP as part of the
#' Elixhauser Comorbidity Software Refined for ICD-10-CM.
#' @examples
#' data(comfmt_releases)
#'
#' # Codes per release
#' table(comfmt_releases$release)
#'
#' # What v2026.1 added over v2025.1
#' a <- comfmt_releases$code[comfmt_releases$release == "2025.1"]
#' b <- comfmt_releases$code[comfmt_releases$release == "2026.1"]
#' setdiff(b, a)
"comfmt_releases"

#' Present on Admission (POA) Exempt Codes
#'
#' A dataset containing ICD-10-CM diagnosis codes that are exempt from Present on 
#' Admission (POA) indicator requirements. These codes represent conditions that are 
#' always considered present on admission regardless of POA indicator values.
#'
#' @format A data frame with 2 columns and 409843 rows:
#' \describe{
#'   \item{version}{Integer. ICD-10-CM version number (33-43) indicating the
#'                 ICD-10-CM annual update version}
#'   \item{code}{Character. Normalized ICD-10-CM diagnosis code without dots or spaces,
#'              in uppercase format (e.g., "E119", "I10")}
#' }
#' @details
#' POA exempt codes include conditions that:
#' \itemize{
#'   \item Are always considered present on admission by clinical definition
#'   \item Cannot be acquired during a hospital stay  
#'   \item Are chronic conditions that existed before admission
#'   \item Are congenital conditions or birth defects
#'   \item Are external cause codes (accidents, injuries)
#' }
#' 
#' Version mapping follows CMS guidelines:
#' \itemize{
#'   \item Version 33: 2015 Q4 - 2016 Q3
#'   \item Version 34: 2016 Q4 - 2017 Q3
#'   \item ...continuing through...
#'   \item Version 42: 2024 Q4 - 2025 Q3
#'   \item Version 43: 2025 Q4 - 2026 Q3
#' }
#'
#' Derived from AHRQ CMR v2026.1 - see \code{\link{cmr_version}}.
#' @source 
#' Based on CMS ICD-10-CM POA exempt code lists published annually with 
#' ICD-10-CM updates. Codes are version-specific to account for annual changes.
#' @examples
#' data(poaxmpt_codes_long)
#' head(poaxmpt_codes_long)
#' 
#' # View codes by version
#' table(poaxmpt_codes_long$version)
#' 
#' # Get POA exempt codes for version 43 (current)
#' v43_codes <- poaxmpt_codes_long[poaxmpt_codes_long$version == 43, ]
#' length(unique(v43_codes$code))
"poaxmpt_codes_long"