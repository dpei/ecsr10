# The dx/POA column-pairing contract, and the principal-diagnosis guard.
#
# Two hazards live here, and both used to be silent.
#
# 1. dx and POA columns are joined on the FIRST run of digits in their NAMES,
#    never on position within the two vectors. Until this file existed the only
#    guard was a data-derived check for a FULLY disjoint position set, so a
#    PARTIAL mismatch - dx2/dx3 paired with poa2/poa4 - was accepted: position 3
#    joined to nothing, coalesced to "", and every POA-dependent measure stopped
#    firing on that diagnosis with no error and no warning.
#
# 2. The AHRQ software scores secondary diagnoses only - `DO I = 2 TO
#    MIN(&MAXNDX,&NUMDX)` in the refined mapping program, `DO I = 2 TO
#    MIN(NDX,&NUMDX)` in the beta one. ecsr10 scores every column in dx_cols, so
#    passing dx1 departs from SAS by changing flags rather than by erroring.

make_pair_data <- function(n = 4) {
  out <- data.frame(
    encounter_id = sprintf("ENC%03d", seq_len(n)),
    # I5030 is heart failure, POA-dependent and NOT POA-exempt, so its flag
    # turns entirely on whether its POA column is found.
    dx1  = "I5030",
    dx2  = "Z9989",
    dx3  = "I5030",
    poa1 = "Y",
    poa2 = "Y",
    poa3 = "Y",
    poa4 = "Y",
    year = 2025,
    quarter = 1,
    stringsAsFactors = FALSE
  )
  out
}

# ---------------------------------------------------------------- pairing ----

test_that("matched dx/POA position sets are accepted and pair correctly", {
  d <- make_pair_data()
  res <- comorbidity(d, dx_cols = c("dx2", "dx3"), poa_cols = c("poa2", "poa3"))

  # dx3 = I5030 with poa3 = "Y" must reach CMR_HF.
  expect_true(all(res$CMR_HF == 1L))
})

test_that("pairing is by name, not by position in the vectors", {
  d <- make_pair_data()

  ordered  <- comorbidity(d, dx_cols = c("dx2", "dx3"), poa_cols = c("poa2", "poa3"))
  reversed <- comorbidity(d, dx_cols = c("dx2", "dx3"), poa_cols = c("poa3", "poa2"))

  # Same position SET, different vector order: the join is on the name digits, so
  # the two must agree. This is the documented contract, not an accident.
  expect_identical(ordered$CMR_HF, reversed$CMR_HF)
})

test_that("a partial dx/POA mismatch errors instead of silently blanking POA", {
  d <- make_pair_data()

  expect_error(
    comorbidity(d, dx_cols = c("dx2", "dx3"), poa_cols = c("poa2", "poa4")),
    "do not pair one-to-one"
  )

  # The specific hazard is named, so the message says what would have gone wrong
  # rather than only that something is unequal.
  expect_error(
    comorbidity(d, dx_cols = c("dx2", "dx3"), poa_cols = c("poa2", "poa4")),
    "blank POA value"
  )
})

test_that("the partial mismatch would otherwise have changed the answer", {
  # Pins the reason the check exists. Scoring the mismatched pair through the
  # engine directly - bypassing the validator - must differ from the matched one;
  # if it ever stops differing, this file is guarding nothing.
  d <- make_pair_data()
  comfmt <- ecsr10:::.comfmt_for_release("2026.1")
  poa_exempt <- ecsr10:::.poa_exempt_default()

  good <- ecsr10:::.comorbidity_flags(
    d, c("dx2", "dx3"), c("poa2", "poa3"), "year", "quarter",
    comfmt, poa_exempt, TRUE, "wildcard", 43L
  )
  bad <- ecsr10:::.comorbidity_flags(
    d, c("dx2", "dx3"), c("poa2", "poa4"), "year", "quarter",
    comfmt, poa_exempt, TRUE, "wildcard", 43L
  )

  hf <- which(ecsr10:::CMR_FINAL_TARGETS == "CMR_HF")
  expect_true(all(good[, hf] == 1L))
  expect_true(all(bad[, hf] == 0L))
})

test_that("a fully disjoint position set is rejected", {
  d <- make_pair_data()
  names(d)[names(d) == "dx2"]  <- "dx7"
  names(d)[names(d) == "dx3"]  <- "dx8"

  expect_error(
    comorbidity(d, dx_cols = c("dx7", "dx8"), poa_cols = c("poa2", "poa3")),
    "do not pair one-to-one"
  )
})

test_that("duplicate extracted positions are rejected", {
  # HCUP's own naming: I10_DX2 and I10_DX3 both yield 10, because the FIRST digit
  # run wins. Without this check one diagnosis inherits the other's POA and the
  # join fans out.
  d <- make_pair_data()
  names(d)[names(d) == "dx2"] <- "I10_DX2"
  names(d)[names(d) == "dx3"] <- "I10_DX3"

  expect_error(
    comorbidity(d, dx_cols = c("I10_DX2", "I10_DX3"), poa_cols = c("poa2", "poa3")),
    "duplicate diagnosis position"
  )

  # The message must carry the naming hint - that is the actual fix for this case.
  expect_error(
    comorbidity(d, dx_cols = c("I10_DX2", "I10_DX3"), poa_cols = c("poa2", "poa3")),
    "resolve to 10, not 2"
  )
})

test_that("duplicate positions among the POA columns are rejected too", {
  d <- make_pair_data()
  names(d)[names(d) == "poa2"] <- "DXPOA9_A"
  names(d)[names(d) == "poa3"] <- "DXPOA9_B"

  expect_error(
    comorbidity(d, dx_cols = c("dx2", "dx3"), poa_cols = c("DXPOA9_A", "DXPOA9_B")),
    "duplicate diagnosis position"
  )
})

test_that("column names carrying no digits are rejected", {
  d <- make_pair_data()
  names(d)[names(d) == "dx2"]  <- "dx_alpha"
  names(d)[names(d) == "poa2"] <- "poa_alpha"

  expect_error(
    comorbidity(d, dx_cols = c("dx_alpha", "dx3"), poa_cols = c("poa_alpha", "poa3")),
    "no diagnosis position can be extracted"
  )
})

test_that("poa_cols = NULL bypasses pairing validation entirely", {
  d <- make_pair_data()
  names(d)[names(d) == "dx2"] <- "dx_alpha"

  # No POA columns means nothing to pair; the digit-less name is then harmless.
  expect_no_error(
    comorbidity(d, dx_cols = c("dx_alpha", "dx3"), use_poa = FALSE)
  )
})

test_that("validation happens in the parent, before any fork", {
  # A worker error surfaces as "parallel chunk processing failed"; this must not.
  d <- make_pair_data(n = 8)

  expect_error(
    comorbidity(d, dx_cols = c("dx2", "dx3"), poa_cols = c("poa2", "poa4"),
                ncores = 2),
    "do not pair one-to-one"
  )
})

test_that("an all-NA diagnosis column is still validated", {
  # The old check read the reshaped frame, where values_drop_na had already
  # removed this column's position - so the mismatch was invisible. The name-based
  # check sees it.
  d <- make_pair_data()
  d$dx3 <- NA_character_

  expect_error(
    comorbidity(d, dx_cols = c("dx2", "dx3"), poa_cols = c("poa2", "poa4")),
    "do not pair one-to-one"
  )
})

# ------------------------------------------------------- principal dx ----

test_that("dx_cols containing position 1 warns", {
  d <- make_pair_data()

  expect_warning(
    comorbidity(d, dx_cols = c("dx1", "dx2", "dx3"),
                poa_cols = c("poa1", "poa2", "poa3")),
    "diagnosis position 1"
  )
  expect_warning(
    comorbidity(d, dx_cols = c("dx1", "dx2", "dx3"),
                poa_cols = c("poa1", "poa2", "poa3")),
    "SECONDARY diagnoses only"
  )
})

test_that("secondary-only dx_cols warn about nothing", {
  d <- make_pair_data()

  expect_no_warning(
    comorbidity(d, dx_cols = c("dx2", "dx3"), poa_cols = c("poa2", "poa3"))
  )
})

test_that("the principal-diagnosis warning is not fatal", {
  # expect_warning() in 3e returns the CONDITION, not the value - taking the
  # result from it would bind a condition object and make every later comparison
  # pass vacuously. Assert the warning separately, take the value from
  # suppressWarnings().
  d <- make_pair_data()
  res <- suppressWarnings(
    comorbidity(d, dx_cols = c("dx1", "dx2", "dx3"),
                poa_cols = c("poa1", "poa2", "poa3"))
  )

  expect_s3_class(res, "data.frame")
  expect_true("CMR_HF" %in% names(res))
  expect_equal(nrow(res), nrow(d))
})

test_that("the principal-diagnosis warning also fires on variant = beta", {
  # The beta program applies the same loop bound, `DO I = 2 TO MIN(NDX,&NUMDX)`,
  # which is why the check precedes the beta dispatch.
  d <- make_pair_data()
  d$drg <- 100L

  expect_warning(
    comorbidity(d, dx_cols = c("dx1", "dx2", "dx3"), variant = "beta",
                release = "2020.1", drg_col = "drg"),
    "diagnosis position 1"
  )
})

test_that("beta still rejects poa_cols before pairing is considered", {
  # Pairing validation sits AFTER the beta dispatch on purpose: "poa_cols is not
  # used by beta" is the more useful message when a beta call supplies them, even
  # when they are also mispaired.
  d <- make_pair_data()
  d$drg <- 100L

  expect_error(
    comorbidity(d, dx_cols = c("dx2", "dx3"), poa_cols = c("poa2", "poa4"),
                variant = "beta", release = "2020.1", drg_col = "drg"),
    "not used by variant"
  )
})
