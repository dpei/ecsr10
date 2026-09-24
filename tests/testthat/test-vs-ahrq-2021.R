# test-vs-ahrq-2021.R
# Value-level parity against AHRQ's own SAS output for v2021.1.
#
# `expected-sas-2021_1.rds` is v2021.1's SAS program run against
# data/simulation/multi_release/fixture/multirelease_core.csv on SAS OnDemand for
# Academics, reduced by simulation/make_test_references.R. Unlike
# expected-ahrq-results.rds it is NOT vendored from a third party - no third
# party implements v2021.1 - it is this repository's own SAS run, and the
# harness that regenerates it is simulation/multi_release/run_all.sh --with-sas.
#
# v2021.1 is the arm that pins two things nothing else can:
#
#   1. The output SCHEMA varies by release. v2022.1 renamed ARTH -> AUTOIMMUNE
#      and CHF -> HF, so a v2021.1 result carries CMR_ARTH and CMR_CHF. Getting
#      that wrong is silent: .build_result_matrix() filters on a name list, and a
#      column it cannot find simply stays zero.
#   2. AHRQ ships no index program before v2022.1, so cmr_index() must refuse the
#      release rather than apply some other release's weights.
#
# The reference stores the flags CMR_-prefixed; v2021.1's SAS emits them bare,
# since AHRQ only added the prefix at v2022.1.

ahrq21 <- readRDS(test_path("expected-sas-2021_1.rds"))

FLAGS21 <- grep("^CMR_", names(ahrq21), value = TRUE)

ahrq21_input <- function(ref) {
  dx  <- grep("^dx[0-9]+$",  names(ref), value = TRUE)
  poa <- grep("^poa[0-9]+$", names(ref), value = TRUE)
  cbind(ref[c("encounter_id", "year", "dqtr")], ref[dx], ref[poa])
}

test_that("the v2021.1 reference has the shape the tests assume", {
  expect_equal(nrow(ahrq21), 6648L)
  expect_length(FLAGS21, 38L)
  # The two renamed measures, and only those two.
  expect_true(all(c("CMR_ARTH", "CMR_CHF") %in% FLAGS21))
  expect_false(any(c("CMR_AUTOIMMUNE", "CMR_HF") %in% FLAGS21))
  expect_setequal(setdiff(FLAGS21, c("CMR_ARTH", "CMR_CHF")),
                  setdiff(ecsr10:::CMR_FINAL_TARGETS, c("CMR_AUTOIMMUNE", "CMR_HF")))
  # Dates vary, which is what makes the ICD-version cap reachable at all.
  expect_gt(length(unique(paste(ahrq21$year, ahrq21$dqtr))), 10L)
  expect_true(all(vapply(ahrq21[FLAGS21], is.integer, logical(1))))
})

test_that("comorbidity() reproduces AHRQ SAS flags at v2021.1", {
  inp <- ahrq21_input(ahrq21)
  dx  <- grep("^dx[0-9]+$",  names(inp), value = TRUE)
  poa <- grep("^poa[0-9]+$", names(inp), value = TRUE)
  dx  <- dx[order(as.integer(sub("dx",  "", dx)))]
  poa <- poa[order(as.integer(sub("poa", "", poa)))]

  # Position 1 is the primary diagnosis; the CMR method scores secondaries only,
  # which the SAS does with `DO I = 2 TO ...`.
  res <- comorbidity(inp, dx_cols = dx[-1], poa_cols = poa[-1],
                     year_col = "year", quarter_col = "dqtr",
                     release = "2021.1")

  expect_equal(nrow(res), nrow(ahrq21))
  expect_identical(attr(res, "cmr_release"), "2021.1")
  expect_identical(attr(res, "cmr_variant"), "refined")
  expect_setequal(grep("^CMR_", names(res), value = TRUE), FLAGS21)

  for (fl in FLAGS21) {
    expect_identical(as.integer(res[[fl]]), ahrq21[[fl]], info = fl)
  }
})

test_that("the v2021.1 arm actually differs from v2026.1", {
  # Guards the gate against degrading into a no-op: if scoring the same rows
  # under the newest release gave the same answers, the arm would prove nothing.
  inp <- ahrq21_input(ahrq21)
  dx  <- grep("^dx[0-9]+$",  names(inp), value = TRUE)
  poa <- grep("^poa[0-9]+$", names(inp), value = TRUE)
  dx  <- dx[order(as.integer(sub("dx",  "", dx)))]
  poa <- poa[order(as.integer(sub("poa", "", poa)))]

  a <- comorbidity(inp, dx_cols = dx[-1], poa_cols = poa[-1],
                   year_col = "year", quarter_col = "dqtr", release = "2021.1")
  b <- comorbidity(inp, dx_cols = dx[-1], poa_cols = poa[-1],
                   year_col = "year", quarter_col = "dqtr", release = "2026.1")

  # The shared 36 measures, i.e. everything but the two AHRQ renamed.
  shared <- setdiff(FLAGS21, c("CMR_ARTH", "CMR_CHF"))
  differing <- sum(vapply(shared, function(fl) sum(a[[fl]] != b[[fl]]), integer(1)))
  expect_gt(differing, 0L)
})

test_that("cmr_index() refuses releases AHRQ ships no index program for", {
  df <- data.frame(CMR_AIDS = 1L, CMR_ALCOHOL = 0L)
  expect_error(cmr_index(df, release = "2021.1"), "no comorbidity index program")
  for (rel in cmr_releases("beta")) {
    expect_error(cmr_index(df, release = rel), "no comorbidity index program")
  }
  # ... and still scores the releases that do have one.
  expect_s3_class(cmr_index(df, release = "2022.1"), "data.frame")
})
