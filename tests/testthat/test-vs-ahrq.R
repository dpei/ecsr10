# test-vs-ahrq.R
# Value-level parity against AHRQ's own SAS output, per release.
#
# `expected-ahrq-results.rds` is the AHRQ CMR SAS program's output over a
# synthetic dataset, produced once per release v2022.1-v2026.1 and reduced to one
# representative row per distinct outcome signature (5,870 rows over 82 columns).
# It is vendored from the CRAN package `medicalcoder`, which built it and uses it
# for the same purpose:
#
#   source:    https://github.com/dewittpe/medicalcoder  tests/expected-ahrq-results.rds
#   commit:    a54e2493aeb392e4f1096e6c34d1f44e6869dcad
#   license:   BSD 3-clause, (c) 2026 Peter E. DeWitt, Seth Russell, Meg Rebull,
#              Tell Bennett, Vincent Rubinetti, The Regents of the University of
#              Colorado / University of Colorado Anschutz
#   generator: tests/build-expected-ahrq-results/ in that repository
#
# Why this file matters: it is the only test that checks comorbidity()'s output
# VALUES, and the only one that can tell the releases apart. The
# simulation/sas_parity/ fixture cannot - all five releases score it identically.
#
# It is also a genuine gate on the release-aware ICD-version ladder. Every row is
# dated Q4 of its own release year, which is past that release's truncated
# ladder, so each lands on the release's final ELSE: ICDVER 39/40/41/42/43. A
# release-blind ladder returns 40/41/42/43/43 instead and four of the five arms
# fail.
#
# Columns are renamed from HCUP's I10_DX*/DXPOA* to dx*/poa* on purpose: dx and
# POA columns are paired by the FIRST run of digits in their names, so "I10_DX2"
# resolves to position 10, not 2. comorbidity() now errors on that rather than
# silently dropping POA, and the last test here pins that behaviour.

ahrq <- readRDS(test_path("expected-ahrq-results.rds"))

# CMR_VERSION is stored as a double (2022.1). Never compare it with `==` against
# a literal; format once and key on the string.
ahrq$release <- sprintf("%.1f", ahrq$CMR_VERSION)

CMR_FLAGS <- grep("^CMR_", names(ahrq), value = TRUE)
CMR_FLAGS <- setdiff(CMR_FLAGS, c("CMR_VERSION", "CMR_Index_Readmission",
                                  "CMR_Index_Mortality"))

# Only DX1-DX3 carry data, and DX1 is the placeholder "acode" - never a real
# code - which matches the CMR rule that the primary diagnosis is excluded.
ahrq_input <- function(ref) {
  data.frame(
    PATID = ref$PATID,
    year  = ref$YEAR,
    qtr   = ref$DQTR,
    dx2   = ref$I10_DX2,  poa2 = ref$DXPOA2,
    dx3   = ref$I10_DX3,  poa3 = ref$DXPOA3,
    stringsAsFactors = FALSE
  )
}

score <- function(ref, rel) {
  comorbidity(ahrq_input(ref),
              dx_cols     = c("dx2", "dx3"),
              poa_cols    = c("poa2", "poa3"),
              year_col    = "year",
              quarter_col = "qtr",
              release     = rel)
}

# Score each release once and share it between the flag and index tests - the
# pipeline is the expensive part and running it twice doubles the suite for
# nothing.
REF    <- lapply(cmr_releases(), function(r) ahrq[ahrq$release == r, ])
SCORED <- Map(score, REF, cmr_releases())
names(REF) <- names(SCORED) <- cmr_releases()

test_that("the vendored AHRQ corpus has the shape the tests assume", {
  expect_equal(nrow(ahrq), 5870L)
  expect_setequal(unique(ahrq$release), cmr_releases())
  expect_length(CMR_FLAGS, 38L)
  # Every row sits at Q4 of its own release year, one ICD version per release.
  per <- unique(ahrq[, c("release", "YEAR", "DQTR", "ICDVER")])
  expect_equal(nrow(per), length(cmr_releases()))
  expect_true(all(per$DQTR == 4L))
  expect_equal(per$YEAR, as.integer(substr(per$release, 1, 4)))
  # PATID repeats across releases, so anything joining must key on both.
  expect_lt(length(unique(ahrq$PATID)), nrow(ahrq))
})

test_that("determine_icd_version reproduces the SAS ICDVER, per release", {
  for (rel in cmr_releases()) {
    ref <- ahrq[ahrq$release == rel, ]
    got <- determine_icd_version(ref$YEAR, ref$DQTR,
                                 .release_max_icd_version(rel))
    expect_equal(got, ref$ICDVER, info = rel)
  }
})

test_that("comorbidity() reproduces AHRQ SAS flags for every release", {
  for (rel in cmr_releases()) {
    ref <- REF[[rel]]
    res <- SCORED[[rel]]

    expect_equal(nrow(res), nrow(ref), info = rel)
    expect_identical(attr(res, "cmr_release"), rel)
    # Output schema is a stable 38 columns at every release.
    expect_setequal(grep("^CMR_", names(res), value = TRUE), CMR_FLAGS)

    for (fl in CMR_FLAGS) {
      expect_identical(as.integer(res[[fl]]), ref[[fl]],
                       info = paste(rel, fl))
    }
  }
})

test_that("cmr_index() reproduces AHRQ SAS index scores for every release", {
  for (rel in cmr_releases()) {
    ref <- REF[[rel]]
    res <- cmr_index(SCORED[[rel]], release = rel)

    # The reference stores these as integer; cmr_index() accumulates in double.
    expect_equal(res$CMR_Index_Mortality, as.numeric(ref$CMR_Index_Mortality),
                 info = rel)
    expect_equal(res$CMR_Index_Readmission, as.numeric(ref$CMR_Index_Readmission),
                 info = rel)
  }
})

test_that("the corpus actually discriminates releases", {
  # Guards against the gate silently degrading into a no-op. If scoring every
  # release with the newest one's rules still passed, the tests above would prove
  # nothing about the release argument.
  ref   <- REF[["2022.1"]]
  wrong <- score(ref, "2026.1")
  differing <- sum(vapply(CMR_FLAGS, function(fl)
    sum(as.integer(wrong[[fl]]) != ref[[fl]]), integer(1)))
  expect_gt(differing, 0L)

  # And the mortality weights really do differ at 2022.1.
  flags <- SCORED[["2022.1"]]
  expect_false(identical(
    suppressWarnings(cmr_index(flags, release = "2022.1")$CMR_Index_Mortality),
    suppressWarnings(cmr_index(flags, release = "2026.1")$CMR_Index_Mortality)))
})

test_that("HCUP-style dx column names are rejected rather than silently unpaired", {
  ref <- ahrq[ahrq$release == "2026.1", ][1:20, ]
  hcup <- data.frame(
    year = ref$YEAR, qtr = ref$DQTR,
    I10_DX2 = ref$I10_DX2, DXPOA2 = ref$DXPOA2,
    I10_DX3 = ref$I10_DX3, DXPOA3 = ref$DXPOA3,
    stringsAsFactors = FALSE
  )
  expect_error(
    comorbidity(hcup, dx_cols = c("I10_DX2", "I10_DX3"),
                poa_cols = c("DXPOA2", "DXPOA3"),
                year_col = "year", quarter_col = "qtr"),
    "no dx/POA column pairs"
  )
})
