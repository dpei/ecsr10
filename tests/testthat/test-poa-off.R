# The use_poa = FALSE contract, pinned against AHRQ's SAS %LET POA = 0 branch.
#
# The SAS initializes the 18 POA-dependent measures only inside `%if &POA.=1`, so
# with the switch off they reach the output dataset MISSING - while its LENGTH,
# ARRAY and LABEL statements stay unconditional, so the columns are still there.
# ecsr10 emitted 0/1 there until 0.4.1, which was the single configuration where
# it diverged from the SAS: 221,618 of 478,160 cells over the multi_release
# fixture. simulation/multi_release/ is the end-to-end gate; these are the unit
# tests that keep the pieces from drifting back.

make_poa_data <- function(n = 20) {
  codes <- data.frame(
    dx2  = c("E119",  "I5022", "F209",   "Z9981", "I5022"),
    dx3  = c("I110",  "E119",  "K7031",  NA,      "G20"),
    poa2 = c("Y",     "N",     "Y",      "U",     "W"),
    poa3 = c("N",     "Y",     "Y",      NA,      "Y"),
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

neutral_columns <- function() {
  setdiff(ecsr10:::CMR_FINAL_TARGETS, ecsr10:::CMR_POA_DEPENDENT_COLUMNS)
}

test_that("the POA-dependent column set is the 18 SAS leaves missing", {
  dep <- ecsr10:::CMR_POA_DEPENDENT_COLUMNS

  expect_length(dep, 18L)
  expect_true(all(dep %in% ecsr10:::CMR_FINAL_TARGETS))
  expect_length(neutral_columns(), 20L)

  # SAS's ARRAY COMPOA (19) plus the derived CMR_CBVD, minus the three CBVD
  # internals it DROPs. Spelled out here so a change to CMR_POA_DEPENDENT that
  # silently moves a measure between the two halves fails loudly.
  expect_setequal(dep, c(
    "CMR_ANEMDEF", "CMR_BLDLOSS", "CMR_CBVD", "CMR_COAG", "CMR_HF",
    "CMR_LIVER_MLD", "CMR_LIVER_SEV", "CMR_NEURO_MOVT", "CMR_NEURO_OTH",
    "CMR_NEURO_SEIZ", "CMR_PARALYSIS", "CMR_PSYCHOSES", "CMR_PULMCIRC",
    "CMR_RENLFL_MOD", "CMR_RENLFL_SEV", "CMR_ULCER_PEPTIC", "CMR_VALVE",
    "CMR_WGHTLOSS"
  ))
})

test_that("use_poa = FALSE returns NA for exactly the 18 gated measures", {
  res <- comorbidity(make_poa_data(), dx_cols = c("dx2", "dx3"), use_poa = FALSE)
  dep <- ecsr10:::CMR_POA_DEPENDENT_COLUMNS

  # Every cell of all 18, every row - SAS never assigns them at all.
  for (col in dep) {
    expect_true(all(is.na(res[[col]])), info = col)
  }
  # And the other 20 stay strictly 0/1: the POA-neutral half is untouched by the
  # switch, which is why it already agreed with SAS on every cell.
  for (col in neutral_columns()) {
    expect_false(anyNA(res[[col]]), info = col)
    expect_true(all(res[[col]] %in% c(0L, 1L)), info = col)
  }
  # NA_integer_, not NA logical: the column type must not change with the mode.
  expect_identical(storage.mode(res$CMR_CBVD), "integer")
})

test_that("the columns stay in the output, so the schema does not move", {
  dat <- make_poa_data()
  on  <- comorbidity(dat, dx_cols = c("dx2", "dx3"),
                     poa_cols = c("poa2", "poa3"), use_poa = TRUE)
  off <- comorbidity(dat, dx_cols = c("dx2", "dx3"), use_poa = FALSE)

  # SAS's LENGTH/ARRAY/LABEL statements are outside its %if, and its %else writes
  # `.` specifically to force the two CBVD variables into the PDV. Dropping the
  # columns here would be a different, incompatible answer.
  expect_identical(names(on), names(off))
  expect_identical(nrow(on), nrow(off))
})

test_that("use_poa = TRUE is unaffected - no NA anywhere", {
  res <- comorbidity(make_poa_data(), dx_cols = c("dx2", "dx3"),
                     poa_cols = c("poa2", "poa3"), use_poa = TRUE)

  for (col in ecsr10:::CMR_FINAL_TARGETS) {
    expect_false(anyNA(res[[col]]), info = col)
  }
})


test_that("use_poa is validated up front", {
  dat <- make_poa_data(5)

  # Unchecked, these failed in whichever of the `&&` / `&` contexts they reached
  # first, with a message naming neither the argument nor the caller.
  expect_error(comorbidity(dat, dx_cols = "dx2", use_poa = NA), "use_poa")
  expect_error(comorbidity(dat, dx_cols = "dx2", use_poa = "FALSE"), "use_poa")
  expect_error(comorbidity(dat, dx_cols = "dx2", use_poa = c(TRUE, FALSE)), "use_poa")
})

test_that("the result records whether POA was applied", {
  dat <- make_poa_data(5)

  expect_true(attr(comorbidity(dat, dx_cols = c("dx2", "dx3"),
                               poa_cols = c("poa2", "poa3")), "cmr_use_poa"))
  expect_false(attr(comorbidity(dat, dx_cols = c("dx2", "dx3"),
                                use_poa = FALSE), "cmr_use_poa"))
})

test_that("cmr_index scores an NA-bearing frame the way SAS SUM(OF ...) does", {
  res <- comorbidity(make_poa_data(), dx_cols = c("dx2", "dx3"), use_poa = FALSE)
  dep <- ecsr10:::CMR_POA_DEPENDENT_COLUMNS

  scored <- cmr_index(res)

  # SAS's index program has no POA switch at all - one flat 38-element array, and
  # SUM() ignores missing arguments. So a missing term contributes 0, exactly as
  # zeroing the column would. This equivalence is what closed the last 6,446
  # differing cells; do not let a "cleanup" turn it into NA propagation.
  zeroed <- res
  zeroed[dep] <- 0L
  scored_zeroed <- cmr_index(zeroed)

  expect_identical(scored$CMR_Index_Readmission, scored_zeroed$CMR_Index_Readmission)
  expect_identical(scored$CMR_Index_Mortality, scored_zeroed$CMR_Index_Mortality)

  # Well-formed numbers, not NA - the quiet part: a POA-off index looks exactly
  # like a POA-on one and is silently scored over 20 of 38 measures.
  expect_false(anyNA(scored$CMR_Index_Readmission))
  expect_false(anyNA(scored$CMR_Index_Mortality))
})

test_that("a POA-off index equals one built from the 20 neutral measures alone", {
  res <- comorbidity(make_poa_data(), dx_cols = c("dx2", "dx3"), use_poa = FALSE)

  full <- cmr_index(res)
  # Dropping the all-NA columns entirely must give the same answer: cmr_index()
  # skips absent columns, and SUM() skips missing terms, for the same reason.
  partial <- cmr_index(res[c("encounter_id", neutral_columns())])

  expect_identical(full$CMR_Index_Readmission, partial$CMR_Index_Readmission)
  expect_identical(full$CMR_Index_Mortality, partial$CMR_Index_Mortality)
})
