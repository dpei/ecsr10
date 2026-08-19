# The chunked path splits encounters across forked workers and recombines the
# per-chunk flag matrices. These tests pin the invariant that makes that legal:
# every stage is per-encounter, so a chunk's rows are exactly the rows the
# whole-dataset call would have produced.
#
# On Windows .resolve_ncores() falls back to serial with a warning, so these
# compare a serial result against itself there rather than failing.
#
# No test here asks for more than 2 cores. R CMD check sets _R_CHECK_LIMIT_CORES_,
# under which parallel:::.check_ncores() turns any fork of 3+ workers into an error
# -- so a higher count here fails the check rather than testing anything. The
# invariant is per-encounter and independent of how many chunks it is cut into, so
# 2 chunks exercises it as well as 8 does; what matters is the shape of the split
# (even, uneven, more chunks than rows), which each test below varies instead.

make_test_data <- function(n) {
  base <- data.frame(
    encounter_id = character(0), dx2 = character(0), dx3 = character(0),
    poa2 = character(0), poa3 = character(0),
    stringsAsFactors = FALSE
  )
  # Codes chosen to exercise POA-neutral (E119 diabetes), POA-dependent
  # (I5022 heart failure), and no-match rows.
  codes <- data.frame(
    dx2  = c("E119", "I5022", "F209",  "Z9981", "I5022"),
    dx3  = c("I110", "E119",  "K7031", NA,      "G20"),
    poa2 = c("Y",    "N",     "Y",     "U",     "W"),
    poa3 = c("N",    "Y",     "Y",     NA,      "Y"),
    stringsAsFactors = FALSE
  )
  idx <- rep_len(seq_len(nrow(codes)), n)
  out <- codes[idx, , drop = FALSE]
  out$encounter_id <- sprintf("ENC%05d", seq_len(n))
  out$year <- 2025
  out$quarter <- 1
  rownames(out) <- NULL
  out[, c("encounter_id", "dx2", "dx3", "poa2", "poa3", "year", "quarter")]
}

test_that("chunked and serial runs give identical results", {
  test_data <- make_test_data(200)

  serial <- comorbidity(test_data, dx_cols = c("dx2", "dx3"),
                        poa_cols = c("poa2", "poa3"), ncores = 1)
  chunked <- comorbidity(test_data, dx_cols = c("dx2", "dx3"),
                         poa_cols = c("poa2", "poa3"), ncores = 2)

  expect_identical(names(serial), names(chunked))
  expect_equal(nrow(chunked), nrow(test_data))
  expect_identical(serial, chunked)
})

test_that("chunking holds when rows do not divide evenly across chunks", {
  test_data <- make_test_data(97)   # odd, so the 2 chunks are 49 and 48 rows

  serial <- comorbidity(test_data, dx_cols = c("dx2", "dx3"),
                        poa_cols = c("poa2", "poa3"), ncores = 1)
  chunked <- comorbidity(test_data, dx_cols = c("dx2", "dx3"),
                         poa_cols = c("poa2", "poa3"), ncores = 2)

  expect_identical(serial, chunked)
})

test_that("chunking holds when there are fewer rows than chunks", {
  # 1 row across 2 chunks leaves a chunk empty, which is the case this pins.
  test_data <- make_test_data(1)

  serial <- comorbidity(test_data, dx_cols = c("dx2", "dx3"),
                        poa_cols = c("poa2", "poa3"), ncores = 1)
  chunked <- comorbidity(test_data, dx_cols = c("dx2", "dx3"),
                         poa_cols = c("poa2", "poa3"), ncores = 2)

  expect_equal(nrow(chunked), 1)
  expect_identical(serial, chunked)
})

test_that("chunking holds with use_poa = FALSE", {
  test_data <- make_test_data(50)

  serial <- comorbidity(test_data, dx_cols = c("dx2", "dx3"),
                        use_poa = FALSE, ncores = 1)
  chunked <- comorbidity(test_data, dx_cols = c("dx2", "dx3"),
                         use_poa = FALSE, ncores = 2)

  expect_identical(serial, chunked)
  # The CBVD derivation lives inside the use_poa branch, so it stays 0 here.
  expect_true(all(chunked$CMR_CBVD == 0))
})

test_that("ncores is validated", {
  test_data <- make_test_data(10)

  expect_error(
    comorbidity(test_data, dx_cols = c("dx2", "dx3"), ncores = 0),
    "positive integer"
  )
  expect_error(
    comorbidity(test_data, dx_cols = c("dx2", "dx3"), ncores = c(1, 2)),
    "positive integer"
  )
})
