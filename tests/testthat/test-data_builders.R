test_that("determine_icd_version works correctly", {
  # Test known mappings
  expect_equal(determine_icd_version(2015, 4), 33)
  expect_equal(determine_icd_version(2016, 1), 33)
  expect_equal(determine_icd_version(2016, 4), 34)
  expect_equal(determine_icd_version(2024, 4), 42)
  expect_equal(determine_icd_version(2025, 1), 42)
  
  # Test vectors
  years <- c(2015, 2016, 2024, 2025)
  quarters <- c(4, 4, 4, 1)
  expected <- c(33, 34, 42, 42)
  expect_equal(determine_icd_version(years, quarters), expected)
  
  # Test default case (future dates)
  expect_equal(determine_icd_version(2030, 1), 42)
  expect_equal(determine_icd_version(1990, 1), 42)
})

test_that("build_comfmt_from_csv handles different schemas", {
  skip_if_not_installed("readr")
  
  # Create temporary CSV files for testing
  temp_dir <- tempdir()
  
  # Schema A: code, comorbidity
  csv_a <- file.path(temp_dir, "format_a.csv")
  cat("code,comorbidity\nE11%,DIAB_CX\nI10,HTN_UNCX\n", file = csv_a)
  
  result_a <- build_comfmt_from_csv(csv_a)
  expect_true("target" %in% names(result_a))
  expect_true("pattern" %in% names(result_a))
  expect_equal(nrow(result_a), 2)
  expect_equal(result_a$target, c("DIAB_CX", "HTN_UNCX"))
  expect_equal(result_a$pattern, c("E11%", "I10"))
  
  # Schema B: target, pattern  
  csv_b <- file.path(temp_dir, "format_b.csv")
  cat("target,pattern\nDIAB_CX,E11%\nHTN_UNCX,I10\n", file = csv_b)
  
  result_b <- build_comfmt_from_csv(csv_b)
  expect_equal(result_a, result_b)
  
  # Test invalid schema
  csv_invalid <- file.path(temp_dir, "format_invalid.csv")
  cat("invalid,columns\ntest,data\n", file = csv_invalid)
  
  expect_error(build_comfmt_from_csv(csv_invalid), 
               "Lookup CSV must have columns")
  
  # Clean up
  unlink(c(csv_a, csv_b, csv_invalid))
})

test_that("build_poa_exempt_formats processes correctly", {
  skip_if_not_installed("readr")
  
  # Create temporary CSV for testing
  temp_dir <- tempdir()
  csv_file <- file.path(temp_dir, "poa_test.csv")
  
  cat("version,code\n33,E119\n33,I10\n34,E119\n34,Z511\n", file = csv_file)
  
  result <- build_poa_exempt_formats(csv_file)
  
  expect_true(is.list(result))
  expect_true("v33" %in% names(result))
  expect_true("v34" %in% names(result))
  expect_equal(length(result$v33), 2)
  expect_equal(length(result$v34), 2)
  expect_true("E119" %in% result$v33)
  expect_true("Z511" %in% result$v34)
  
  # Clean up
  unlink(csv_file)
})