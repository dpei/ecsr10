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
  # CBVD is one of the 18 measures AHRQ's SAS leaves missing with POA off, so it
  # is NA rather than 0 - and NA per chunk, which is what makes rbind-ing the
  # blocks back together still match a serial run.
  expect_true(all(is.na(chunked$CMR_CBVD)))
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

# ---------------------------------------------------------------------------
# The beta variant forks through a separate function, .comorbidity_beta(), so
# the invariant has to be pinned there too. Its extra hazard over the refined
# path is `drg`: the DRG vector is subset alongside the rows, and getting that
# subscript wrong misaligns every encounter's screen without changing the row
# count - a corruption that is invisible to a shape check.

make_beta_data <- function(n) {
  codes <- data.frame(
    dx1 = "Z0000",
    # I5022 -> CHF, I1310 -> a detailed hypertension label, E119 -> DM,
    # C7800 -> METS, and one row that maps to nothing.
    dx2 = c("I5022", "I1310", "E119",  "C7800",  "Z9981", "I5022"),
    dx3 = c("I119",  "I5022", "E1165", "C50911", NA,      "I119"),
    # 291 is a CARDDRG (suppresses CHF/VALVE), 638 a DIABDRG, 3 is in no screen.
    # The last row repeats the first with an unscreened DRG, so CHF is present in
    # the output as well as suppressed in it - a fixture where every CHF is
    # screened away would let a broken screen pass.
    drg = c(291L, 291L, 638L, 3L, 3L, 3L),
    stringsAsFactors = FALSE
  )
  out <- codes[rep_len(seq_len(nrow(codes)), n), , drop = FALSE]
  out$encounter_id <- sprintf("BENC%05d", seq_len(n))
  rownames(out) <- NULL
  out[, c("encounter_id", "drg", "dx1", "dx2", "dx3")]
}

test_that("chunked and serial beta runs give identical results", {
  d <- make_beta_data(97)   # uneven split: 49 / 48

  serial <- comorbidity(d, dx_cols = c("dx2", "dx3"), variant = "beta",
                        release = "2020.1", drg_col = "drg", ncores = 1)
  chunked <- comorbidity(d, dx_cols = c("dx2", "dx3"), variant = "beta",
                         release = "2020.1", drg_col = "drg", ncores = 2)

  expect_equal(serial, chunked)
  expect_identical(attr(chunked, "cmr_variant"), "beta")

  # The screen must actually be biting on this fixture, or the test proves
  # nothing about the drg subsetting it is here to check.
  expect_true(any(serial$CMRB_CHF[serial$drg == 291L] == 0L))
  expect_true(any(serial$CMRB_CHF == 1L))
})

test_that("beta chunking survives more chunks than rows", {
  d <- make_beta_data(1)
  serial <- comorbidity(d, dx_cols = c("dx2", "dx3"), variant = "beta",
                        release = "2020.1", drg_col = "drg", ncores = 1)
  chunked <- comorbidity(d, dx_cols = c("dx2", "dx3"), variant = "beta",
                         release = "2020.1", drg_col = "drg", ncores = 2)
  expect_equal(serial, chunked)
  expect_equal(nrow(chunked), 1L)
})

test_that("beta chunking keeps drg aligned with its rows", {
  # The direct test of the hazard: two encounters with the same diagnoses but
  # different DRGs must keep their own answers no matter where the split lands.
  d <- make_beta_data(40)
  d$drg <- rep(c(291L, 3L), length.out = nrow(d))   # alternate screened/not
  d$dx2 <- "I5022"; d$dx3 <- NA_character_

  serial <- comorbidity(d, dx_cols = c("dx2", "dx3"), variant = "beta",
                        release = "2020.1", drg_col = "drg", ncores = 1)
  chunked <- comorbidity(d, dx_cols = c("dx2", "dx3"), variant = "beta",
                         release = "2020.1", drg_col = "drg", ncores = 2)

  expect_equal(serial, chunked)
  expect_true(all(chunked$CMRB_CHF[chunked$drg == 291L] == 0L))
  expect_true(all(chunked$CMRB_CHF[chunked$drg == 3L] == 1L))
})
