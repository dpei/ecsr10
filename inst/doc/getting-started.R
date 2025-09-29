## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# # install.packages("devtools")
# devtools::install_github("dpei/ecsr10")

## ----setup--------------------------------------------------------------------
library(ecsr10)
library(dplyr)

## -----------------------------------------------------------------------------
# Example patient diagnosis data structure
example_patient_data <- data.frame(
  encounter_id = 1:3,
  test_type = "example",
  dx1 = c("E119", "I10", "Z5111"),
  dx2 = c("I10", "E119", "N183"),
  dx3 = c(NA, "N183", NA),
  poa1 = c("Y", "Y", "Y"),
  poa2 = c("N", "Y", "W"),
  poa3 = c(NA, "Y", NA),
  year = 2025,
  quarter = 1,
  stringsAsFactors = FALSE
)

print(example_patient_data)

## -----------------------------------------------------------------------------
# Simple workflow - uses built-in lookup data automatically
result <- comorbidity(example_patient_data, 
                      dx_cols = c("dx1", "dx2", "dx3"),
                      poa_cols = c("poa1", "poa2", "poa3"))

# Calculate risk indices
result_with_indices <- cmr_index(result)

# View summary
head(result_with_indices[,c("encounter_id", "CMR_Index_Readmission", "CMR_Index_Mortality")])

## -----------------------------------------------------------------------------
# Examine the comorbidity flags in more detail
result_summary <- result_with_indices %>%
  select(encounter_id, test_type, starts_with("CMR_")) %>%
  select(1:10)  # Show first 10 columns for brevity

print(result_summary)

## -----------------------------------------------------------------------------
# View the calculated indices
indices_summary <- result_with_indices %>%
  select(encounter_id, CMR_Index_Readmission, CMR_Index_Mortality)

print(indices_summary)

# Basic summary statistics
summary(result_with_indices[c("CMR_Index_Readmission", "CMR_Index_Mortality")])

## ----eval=FALSE---------------------------------------------------------------
# # Advanced workflow with custom lookup data
# # Note: These functions are internal but can still be used if needed
# custom_comfmt <- build_comfmt_from_csv("path/to/your/lookup.csv")
# custom_poa <- build_poa_exempt_formats("path/to/your/poa_exempt.csv")
# 
# result <- comorbidity(patient_data,
#                       dx_cols = c("dx1", "dx2", "dx3"),
#                       comfmt = custom_comfmt,
#                       poa_exempt = custom_poa)

## ----eval=FALSE---------------------------------------------------------------
# # Use custom lookup file
# custom_comfmt <- build_comfmt_from_csv("path/to/your/lookup.csv")
# 
# # Use custom POA exempt list
# custom_poa <- build_poa_exempt_formats("path/to/your/poa_exempt.csv")

## ----include=FALSE------------------------------------------------------------
# No cleanup needed - package handles temporary files automatically

