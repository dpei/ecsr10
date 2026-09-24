# test-vs-beta.R
# Value-level parity against AHRQ's own SAS output for the beta family,
# v2016.2 - v2020.1.
#
# `expected-sas-beta.rds` is each beta version's SAS program run against
# data/simulation/beta_releases/fixture/beta_core.csv on SAS OnDemand for Academics,
# reduced by simulation/make_test_references.R. It is this repository's own SAS
# run, not a vendored third-party artifact - no third party implements the beta
# software. Regenerate with simulation/beta_releases/run_all.sh --with-sas.
#
# The beta software is not the refined software with older tables. What this file
# gates that nothing else can:
#
#   * the MS-DRG exclusion screen, which has no analogue in the refined family -
#     24 numeric screens suppressing comorbidities related to the principal
#     diagnosis. The DRG_SCREEN stratum sits every screen's range boundaries and
#     the two DRGs just outside them, so an off-by-one in the interval test is a
#     red gate rather than a silent pass.
#   * the 10 detailed hypertension labels, which fan out into HTNCX/CHF/RENLFAIL
#     and then decide which screens may take those flags away again, with real
#     asymmetries between the branches (HHRWCHF_ lets RENALFLG zero HTNCX but not
#     RENLFAIL; HHRWRF_ groups HTNCXFLG with CARDFLG rather than testing them
#     separately). That is the highest-risk transcription in R/beta.R.
#   * statement ORDER. HTN_C is derived AFTER the screen, so a screened-away
#     HTNCX must not feed it.
#
# AHRQ's program DROPs HTN and HTNCX, keeping only their union HTN_C, so the
# reference carries 29 measures where comorbidity() emits 31 columns. The two
# extras are checked for consistency below rather than being ignored.

beta <- readRDS(test_path("expected-sas-beta.rds"))

BETA_FLAGS <- grep("^CMRB_", names(beta), value = TRUE)
BETA_ARMS  <- sort(unique(beta$release))

beta_input <- function(ref) {
  dx <- grep("^dx[0-9]+$", names(ref), value = TRUE)
  dx <- dx[order(as.integer(sub("dx", "", dx)))]
  cbind(ref[c("encounter_id", "drg")], ref[dx])
}

score_beta <- function(ref, rel) {
  inp <- beta_input(ref)
  dx  <- grep("^dx[0-9]+$", names(inp), value = TRUE)
  dx  <- dx[order(as.integer(sub("dx", "", dx)))]
  # The beta SAS excludes the primary diagnosis in its loop bound
  # (`DO I = 2 TO MIN(NDX, &NUMDX)`), so dropping position 1 here is what makes
  # the two agree.
  comorbidity(inp, dx_cols = dx[-1], variant = "beta", release = rel,
              drg_col = "drg")
}

REFB    <- lapply(BETA_ARMS, function(r) beta[beta$release == r, ])
SCOREDB <- Map(score_beta, REFB, BETA_ARMS)
names(REFB) <- names(SCOREDB) <- BETA_ARMS

test_that("the beta reference has the shape the tests assume", {
  expect_setequal(BETA_ARMS, cmr_releases("beta"))
  # 30 measures minus HTN and HTNCX, which the SAS drops, plus HTN_C.
  expect_length(BETA_FLAGS, 29L)
  expect_true("CMRB_HTN_C" %in% BETA_FLAGS)
  expect_false(any(c("CMRB_HTN", "CMRB_HTNCX") %in% BETA_FLAGS))
  expect_setequal(BETA_FLAGS,
                  setdiff(ecsr10:::BETA_FINAL_TARGETS, c("CMRB_HTN", "CMRB_HTNCX")))
  expect_true(all(vapply(beta[BETA_FLAGS], is.integer, logical(1))))
  # The DRG axis has to be varied, or the screen is untested by construction.
  expect_gt(length(unique(beta$drg)), 100L)
  expect_true(all(c("DRG_SCREEN", "HTN_PSEUDO", "HIERARCHY", "POSITION",
                    "CODE_SWEEP", "RELEASE_DELTA") %in% beta$test_type))
})

test_that("comorbidity(variant = 'beta') reproduces AHRQ SAS flags, every version", {
  for (rel in BETA_ARMS) {
    ref <- REFB[[rel]]
    res <- SCOREDB[[rel]]

    expect_equal(nrow(res), nrow(ref), info = rel)
    expect_identical(attr(res, "cmr_release"), rel)
    expect_identical(attr(res, "cmr_variant"), "beta")
    expect_setequal(grep("^CMRB_", names(res), value = TRUE),
                    ecsr10:::BETA_FINAL_TARGETS)

    for (fl in BETA_FLAGS) {
      expect_identical(as.integer(res[[fl]]), ref[[fl]], info = paste(rel, fl))
    }
  }
})

test_that("CMRB_HTN_C is the union of the two components AHRQ drops", {
  # The two extra columns ecsr10 keeps are not free-floating: HTN_C is derived
  # from them, so their consistency with it is checkable even though the SAS
  # emits none of the three relationships directly.
  for (rel in BETA_ARMS) {
    res <- SCOREDB[[rel]]
    expect_identical(as.integer(res$CMRB_HTN | res$CMRB_HTNCX),
                     as.integer(res$CMRB_HTN_C), info = rel)
    # The severity hierarchy: complicated hypertension beats uncomplicated.
    expect_true(all(res$CMRB_HTN[res$CMRB_HTNCX == 1L] == 0L), info = rel)
  }
})

test_that("the MS-DRG screen actually changes answers", {
  # Without this the whole beta arm could be passing because the screen never
  # fires - which is exactly what a wrong interval test would look like.
  rel <- "2020.1"
  inp <- beta_input(REFB[[rel]])
  dx  <- grep("^dx[0-9]+$", names(inp), value = TRUE)
  dx  <- dx[order(as.integer(sub("dx", "", dx)))]

  screened <- SCOREDB[[rel]]
  # The warning is asserted separately from the value on purpose: in testthat's
  # 3rd edition expect_warning() returns the CONDITION, not the expression's
  # value, so `x <- expect_warning(f())` silently binds a condition object and
  # every later `x[[col]]` is NULL - which makes the comparisons below vacuously
  # pass rather than fail.
  expect_warning(
    comorbidity(inp, dx_cols = dx[-1], variant = "beta", release = rel),
    "MS-DRG exclusion screen is not applied")
  unscreened <- suppressWarnings(
    comorbidity(inp, dx_cols = dx[-1], variant = "beta", release = rel))

  fl <- ecsr10:::BETA_FINAL_TARGETS
  differing <- sum(vapply(fl, function(f)
    sum(screened[[f]] != unscreened[[f]]), integer(1)))
  expect_gt(differing, 0L)
  # Suppression only ever removes flags; the screen can never add one.
  expect_true(all(vapply(fl, function(f)
    all(screened[[f]] <= unscreened[[f]]), logical(1))))
})

test_that("the beta versions are not interchangeable", {
  # Guards against the release argument being ignored, which would make every
  # arm pass for the wrong reason.
  a <- SCOREDB[["2016.2"]]; b <- SCOREDB[["2020.1"]]
  # The reference is reduced PER VERSION - a signature that is redundant in one
  # version may be the only witness of a behaviour in another - so the two arms
  # do not carry the same rows. Compare on the encounters they share.
  ids <- intersect(a$encounter_id, b$encounter_id)
  expect_gt(length(ids), 100L)
  ia <- match(ids, a$encounter_id); ib <- match(ids, b$encounter_id)
  differing <- sum(vapply(BETA_FLAGS, function(f)
    sum(a[[f]][ia] != b[[f]][ib]), integer(1)))
  expect_gt(differing, 0L)
})

test_that("beta rejects arguments belonging to the other family", {
  inp <- beta_input(REFB[["2020.1"]])[1:5, ]
  dx  <- grep("^dx[0-9]+$", names(inp), value = TRUE)

  expect_error(
    comorbidity(inp, dx_cols = dx[-1], variant = "beta", release = "2026.1",
                drg_col = "drg"),
    "beta release")
  expect_error(
    comorbidity(inp, dx_cols = dx[-1], variant = "beta", release = "2020.1",
                drg_col = "drg", poa_cols = dx[-1]),
    "not used by variant")
  expect_error(
    comorbidity(inp, dx_cols = dx[-1], release = "2026.1", drg_col = "drg",
                year_col = "drg", quarter_col = "drg"),
    "applies only to variant")
  expect_error(
    comorbidity(inp, dx_cols = dx[-1], variant = "beta", release = "2020.1",
                drg_col = "nope"),
    "must name a single column")
  # cmr_index() refuses a beta result even when handed a release that has weights
  expect_error(cmr_index(SCOREDB[["2020.1"]], release = "2026.1"),
               "no comorbidity indices")
})
