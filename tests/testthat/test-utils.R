test_that("normalize_icd10 works correctly", {
  # Test basic normalization
  expect_equal(normalize_icd10("e11.9"), "E119")
  expect_equal(normalize_icd10("I10"), "I10") 
  expect_equal(normalize_icd10("  z51.11  "), "Z5111")
  
  # Test vector input
  codes <- c("E11.9", "I10", "  z51.11  ")
  expected <- c("E119", "I10", "Z5111")
  expect_equal(normalize_icd10(codes), expected)
  
  # Test edge cases
  expect_equal(normalize_icd10(""), "")
  expect_equal(normalize_icd10(NA_character_), NA_character_)
})

test_that("match_codes_to_patterns works with wildcards", {
  # Create test patterns
  patterns_df <- data.frame(
    pattern = c("E11%", "I10", "Z51%"),
    target = c("diabetes", "hypertension", "encounter"),
    stringsAsFactors = FALSE
  )
  
  # Test matching
  codes <- c("E119", "I10", "Z5111", "M79")
  result <- match_codes_to_patterns(codes, patterns_df, mode = "wildcard")
  expected <- c("diabetes", "hypertension", "encounter", NA)
  expect_equal(result, expected)
  
  # Test empty patterns
  empty_patterns <- data.frame(pattern = character(0), target = character(0))
  result_empty <- match_codes_to_patterns(codes, empty_patterns)
  expect_equal(result_empty, rep(NA_character_, length(codes)))
  
  # Test first match priority
  overlap_patterns <- data.frame(
    pattern = c("E11%", "E119"),  
    target = c("diabetes_broad", "diabetes_specific"),
    stringsAsFactors = FALSE
  )
  result_priority <- match_codes_to_patterns("E119", overlap_patterns)
  expect_equal(result_priority, "diabetes_broad")
})

test_that("match_codes_to_patterns works with regex mode", {
  patterns_df <- data.frame(
    pattern = c("^E11.*", "^I10$"),
    target = c("diabetes", "hypertension"),
    stringsAsFactors = FALSE
  )
  
  codes <- c("E119", "I10", "I109")
  result <- match_codes_to_patterns(codes, patterns_df, mode = "regex")
  expected <- c("diabetes", "hypertension", NA)
  expect_equal(result, expected)
})