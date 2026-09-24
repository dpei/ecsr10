test_that("determine_icd_version works correctly", {
  # Test known mappings
  expect_equal(determine_icd_version(2015, 4), 33)
  expect_equal(determine_icd_version(2016, 1), 33)
  expect_equal(determine_icd_version(2016, 4), 34)
  expect_equal(determine_icd_version(2024, 4), 42)
  expect_equal(determine_icd_version(2025, 1), 42)
  expect_equal(determine_icd_version(2025, 4), 43)
  expect_equal(determine_icd_version(2026, 1), 43)

  # Test vectors
  years <- c(2015, 2016, 2024, 2025, 2026)
  quarters <- c(4, 4, 4, 1, 3)
  expected <- c(33, 34, 42, 42, 43)
  expect_equal(determine_icd_version(years, quarters), expected)

  # Test default case (unmapped dates fall back to the newest version)
  expect_equal(determine_icd_version(2030, 1), 43)
  expect_equal(determine_icd_version(1990, 1), 43)
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
# ------------------------------------------------- version filtering ----
#
# `version` is an ICD-10-CM VERSION axis (33-43), NOT an AHRQ RELEASE axis
# (2021.1-2026.1). The two are orthogonal, and conflating them is a real bug
# class - one competing implementation ships exactly that defect. Release
# selection happens in comorbidity(), never here.
#
# The filter was documented but inert until it was hoisted above the schema
# reduction: transmute()/select() dropped version_min/version_max before the
# `%in% names(df)` test could see them, so the branch was unreachable for BOTH
# accepted schemas. These tests pin that it runs.

version_df <- function() {
  data.frame(
    target      = c("EARLY", "LATE", "WINDOW", "OPEN"),
    pattern     = c("E119",  "N183", "I10",    "K7031"),
    version_min = c(33L,     40L,    35L,      NA),
    version_max = c(43L,     43L,    38L,      NA),
    stringsAsFactors = FALSE
  )
}

test_that("version filtering runs for the (target, pattern) schema", {
  df <- version_df()

  # LATE starts at 40, WINDOW ends at 38, OPEN is unbounded on both sides.
  expect_setequal(ecsr10:::.comfmt_from_df(df, version = 33L)$target,
                  c("EARLY", "OPEN"))
  expect_setequal(ecsr10:::.comfmt_from_df(df, version = 36L)$target,
                  c("EARLY", "WINDOW", "OPEN"))
  expect_setequal(ecsr10:::.comfmt_from_df(df, version = 41L)$target,
                  c("EARLY", "LATE", "OPEN"))
})

test_that("version filtering runs for the (code, comorbidity) schema", {
  df <- version_df()
  names(df)[names(df) == "target"]  <- "comorbidity"
  names(df)[names(df) == "pattern"] <- "code"

  expect_setequal(ecsr10:::.comfmt_from_df(df, version = 33L)$target,
                  c("EARLY", "OPEN"))
  expect_setequal(ecsr10:::.comfmt_from_df(df, version = 41L)$target,
                  c("EARLY", "LATE", "OPEN"))
})

test_that("version bounds are inclusive at both endpoints", {
  df <- version_df()

  # WINDOW spans [35, 38]. Both endpoints in, both neighbours out.
  expect_true("WINDOW" %in% ecsr10:::.comfmt_from_df(df, version = 35L)$target)
  expect_true("WINDOW" %in% ecsr10:::.comfmt_from_df(df, version = 38L)$target)
  expect_false("WINDOW" %in% ecsr10:::.comfmt_from_df(df, version = 34L)$target)
  expect_false("WINDOW" %in% ecsr10:::.comfmt_from_df(df, version = 39L)$target)
})

test_that("a missing bound is open-ended on that side", {
  df <- version_df()

  # OPEN has NA on both limits, so it survives every version.
  for (v in c(33L, 38L, 43L)) {
    expect_true("OPEN" %in% ecsr10:::.comfmt_from_df(df, version = v)$target)
  }
})

test_that("version = NULL keeps every row", {
  df <- version_df()
  expect_setequal(ecsr10:::.comfmt_from_df(df)$target,
                  c("EARLY", "LATE", "WINDOW", "OPEN"))
})

test_that("version filtering is a no-op without version_min/version_max", {
  # Which is the built-in case: comfmt_releases carries neither column, so
  # release selection is entirely comorbidity()'s job and this filter must not
  # touch the shipped tables.
  df <- data.frame(target = c("A", "B"), pattern = c("E119", "N183"),
                   stringsAsFactors = FALSE)

  expect_equal(nrow(ecsr10:::.comfmt_from_df(df, version = 33L)), 2L)
  expect_equal(ecsr10:::.comfmt_from_df(df, version = 33L),
               ecsr10:::.comfmt_from_df(df))
})

test_that("the built-in release tables are unaffected by a version argument", {
  built_in <- ecsr10:::.comfmt_for_release("2026.1")
  expect_false(any(c("version_min", "version_max") %in% names(built_in)))
  expect_equal(nrow(built_in), nrow(ecsr10:::.comfmt_from_df(built_in, version = 33L)))
})

test_that("build_comfmt_from_csv passes version through to the filter", {
  skip_if_not_installed("readr")

  csv <- tempfile(fileext = ".csv")
  on.exit(unlink(csv), add = TRUE)
  utils::write.csv(version_df(), csv, row.names = FALSE, na = "")

  expect_setequal(build_comfmt_from_csv(csv, version = 33L)$target,
                  c("EARLY", "OPEN"))
  expect_setequal(build_comfmt_from_csv(csv)$target,
                  c("EARLY", "LATE", "WINDOW", "OPEN"))
})
