#' ICD-10-CM to Comorbidity Mapping Data
#'
#' A dataset containing the mapping from ICD-10-CM diagnosis codes to the 38 Elixhauser 
#' comorbidity categories used in the CMR (Comorbidity Software Refined) methodology.
#' This dataset is used by the comorbidity analysis functions to identify which 
#' diagnosis codes correspond to specific comorbidity conditions.
#'
#' @format A data frame with 2 columns and 4542 rows:
#' \describe{
#'   \item{code}{Character. ICD-10-CM diagnosis code patterns using SQL-style wildcards 
#'             for pattern matching. Examples: "E11\%", "I10", "N18\%"}
#'   \item{comorbidity}{Character. The target comorbidity category name. One of 38 categories
#'                     including AIDS, ALCOHOL, ANEMDEF, AUTOIMMUNE, BLDLOSS, etc.}
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

#' Present on Admission (POA) Exempt Codes
#'
#' A dataset containing ICD-10-CM diagnosis codes that are exempt from Present on 
#' Admission (POA) indicator requirements. These codes represent conditions that are 
#' always considered present on admission regardless of POA indicator values.
#'
#' @format A data frame with 2 columns and 371493 rows:
#' \describe{
#'   \item{version}{Integer. ICD-10-CM version number (33-42) indicating the 
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
#' }
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
#' # Get POA exempt codes for version 42 (current)
#' v42_codes <- poaxmpt_codes_long[poaxmpt_codes_long$version == 42, ]
#' length(unique(v42_codes$code))
"poaxmpt_codes_long"