test_that("cmr_index calculates correctly", {
  # Create test data with known comorbidities
  test_data <- data.frame(
    patient_id = 1:3,
    CMR_AIDS = c(1, 0, 0),
    CMR_ALCOHOL = c(0, 1, 0),
    CMR_CANCER_METS = c(0, 0, 1),
    CMR_OBESE = c(1, 1, 0),
    stringsAsFactors = FALSE
  )
  
  result <- cmr_index(test_data)
  
  # Check that index columns were added
  expect_true("CMR_Index_Readmission" %in% names(result))
  expect_true("CMR_Index_Mortality" %in% names(result))
  
  # Check known calculations
  # Patient 1: AIDS (readmit=5, mort=-4) + OBESE (readmit=-2, mort=-7) = 3, -11
  expect_equal(result$CMR_Index_Readmission[1], 3)
  expect_equal(result$CMR_Index_Mortality[1], -11)
  
  # Patient 2: ALCOHOL (readmit=3, mort=-1) + OBESE (readmit=-2, mort=-7) = 1, -8
  expect_equal(result$CMR_Index_Readmission[2], 1)
  expect_equal(result$CMR_Index_Mortality[2], -8)
  
  # Patient 3: CANCER_METS (readmit=11, mort=22) = 11, 22
  expect_equal(result$CMR_Index_Readmission[3], 11)
  expect_equal(result$CMR_Index_Mortality[3], 22)
})

test_that("cmr_index handles missing data", {
  # Test with missing values
  test_data <- data.frame(
    patient_id = 1:2,
    CMR_AIDS = c(NA, 1),
    CMR_ALCOHOL = c(1, NA),
    stringsAsFactors = FALSE
  )
  
  result <- cmr_index(test_data)
  
  # Should handle NA values gracefully
  expect_equal(result$CMR_Index_Readmission[1], 3)  # Only ALCOHOL weight
  expect_equal(result$CMR_Index_Mortality[1], -1)   # Only ALCOHOL weight
  expect_equal(result$CMR_Index_Readmission[2], 5)  # Only AIDS weight
  expect_equal(result$CMR_Index_Mortality[2], -4)   # Only AIDS weight
})

test_that("cmr_index handles empty data", {
  # Test with no comorbidities
  test_data <- data.frame(
    patient_id = 1:2,
    other_column = c("a", "b"),
    stringsAsFactors = FALSE
  )
  
  result <- cmr_index(test_data)
  
  expect_equal(result$CMR_Index_Readmission, c(0, 0))
  expect_equal(result$CMR_Index_Mortality, c(0, 0))
})

test_that("print_cmr_summary works without error", {
  # Create test data
  test_data <- data.frame(
    CMR_Index_Readmission = c(0, 5, -2, 10),
    CMR_Index_Mortality = c(-5, 0, 22, 15)
  )
  
  # Should not error
  expect_output(print_cmr_summary(test_data), "CMR Index Summary Statistics")
  expect_output(print_cmr_summary(test_data), "CMR_Index_Readmission")
  expect_output(print_cmr_summary(test_data), "CMR_Index_Mortality")
  expect_output(print_cmr_summary(test_data), "Mean:")
  expect_output(print_cmr_summary(test_data), "25th percentile")
})

test_that("print_cmr_summary handles missing columns", {
  # Test with data missing index columns
  test_data <- data.frame(
    patient_id = 1:2,
    other_column = c("a", "b")
  )
  
  expect_output(print_cmr_summary(test_data), "WARNING.*not found")
})