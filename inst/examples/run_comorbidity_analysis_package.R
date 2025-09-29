# run_comorbidity_analysis_package.R
# Example usage of the ecsr10 package for comorbidity analysis
# This demonstrates the simple workflow using built-in lookup data

suppressPackageStartupMessages({
  library(ecsr10)
  library(readr)
  library(dplyr)
  library(janitor)
})

# ---- User-configurable paths ----
input_path  <- "simulated_icd10_comorbidity_data.csv"   # patient diagnosis CSV
output_path <- "ecsr10_como_ret.csv"                    # output CSV

# ---- Load data ----
patient_data <- readr::read_csv(input_path, show_col_types = FALSE) %>% janitor::clean_names()

# Infer dx and poa columns (excludes dx1/poa1, processes dx2..dxN and poa2..poaN). Adjust as needed.
dx_cols  <- names(patient_data) %>% grep("^dx\\d+$", ., value = TRUE) %>% setdiff("dx1")
poa_cols <- names(patient_data) %>% grep("^poa\\d+$", ., value = TRUE) %>% setdiff("poa1") 
poa_cols <- if (length(poa_cols) == length(dx_cols)) poa_cols else NULL

# ---- Simple workflow using built-in data ----
# No need to manually build formats - the package handles this automatically!

# Add year and quarter for ICD version determination
patient_data$year <- 2025
patient_data$qt <- 1

# ---- Apply comorbidity analysis using simplified workflow ----
# The package automatically uses built-in lookup data
res <- comorbidity(patient_data, 
                   dx_cols = dx_cols, 
                   poa_cols = poa_cols,
                   year_col = "year",
                   quarter_col = "qt",
                   use_poa = !is.null(poa_cols))

# ---- Calculate risk indices using package function ----
res <- cmr_index(res)

# ---- Display summary statistics ----
# View basic summary (since print_cmr_summary is now internal)
cat("\n=== CMR Results Summary ===\n")
cat("Total encounters:", nrow(res), "\n")
cat("CMR flags (first 5):")
print(names(res)[grep("^CMR_", names(res))][1:5])
cat("Readmission index range:", 
    paste(range(res$CMR_Index_Readmission, na.rm = TRUE), collapse = " to "), "\n")
cat("Mortality index range:", 
    paste(range(res$CMR_Index_Mortality, na.rm = TRUE), collapse = " to "), "\n")

# ---- Save results ----
# Select columns with CMR_ prefix plus the specified columns
filtered_data <- res %>%
  select(test_type, encounter_id, starts_with("CMR_"))

readr::write_csv(filtered_data, output_path)
message("Wrote: ", output_path)

# ---- No cleanup needed ----
# Package handles temporary files automatically

# ---- Optional: Compare with original implementation ----
if (file.exists("R_como_ret.csv")) {
  message("\n=== Comparison with original implementation ===")
  original <- readr::read_csv("R_como_ret.csv", show_col_types = FALSE)
  package_result <- readr::read_csv(output_path, show_col_types = FALSE)
  
  # Basic dimension check
  cat("Original dimensions:", paste(dim(original), collapse = " x "), "\n")
  cat("Package dimensions:", paste(dim(package_result), collapse = " x "), "\n")
  
  # Column comparison
  orig_cols <- sort(names(original))
  pkg_cols <- sort(names(package_result))
  
  if (identical(orig_cols, pkg_cols)) {
    cat("✓ Column names match\n")
  } else {
    cat("✗ Column names differ\n")
    cat("Missing in package:", setdiff(orig_cols, pkg_cols), "\n")
    cat("Extra in package:", setdiff(pkg_cols, orig_cols), "\n")
  }
  
  # If same dimensions, check for differences
  if (identical(dim(original), dim(package_result)) && identical(orig_cols, pkg_cols)) {
    # Reorder columns to match
    package_result <- package_result[, orig_cols]
    
    # Check for numeric differences (allowing for small floating point differences)
    differences <- 0
    for (col in names(original)) {
      if (is.numeric(original[[col]]) && is.numeric(package_result[[col]])) {
        if (!all.equal(original[[col]], package_result[[col]], tolerance = 1e-10)) {
          differences <- differences + sum(original[[col]] != package_result[[col]], na.rm = TRUE)
        }
      } else {
        differences <- differences + sum(original[[col]] != package_result[[col]], na.rm = TRUE)
      }
    }
    
    if (differences == 0) {
      cat("✓ Results are identical\n")
    } else {
      cat("✗ Found", differences, "differences\n")
    }
  }
} else {
  message("Note: R_como_ret.csv not found for comparison")
}

cat("\n=== Package Analysis Complete ===\n")