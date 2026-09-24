#!/usr/bin/env Rscript
# checks.R
# Assertions over the generated datasets and the AHRQ source they came from.
#
#   cd ecsr10 && Rscript data-raw/checks.R
#
# Exit status: 0 = all checks pass, 1 = at least one failed.
#
# These run against SAS_software/, which is not part of the package, so they
# cannot live in tests/testthat/. Run them after data-raw/build_datasets.R and
# whenever a new AHRQ release is added.

here <- function(...) {
  a <- commandArgs(FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  file.path(if (length(f)) dirname(normalizePath(f[[1]])) else ".", ...)
}

source(here("parse_ahrq_sas.R"))

SAS_DIR  <- here("..", "..", "SAS_software")
DATA_DIR <- here("..", "data")

failures <- 0L
ok <- function(label, cond, detail = "") {
  cond <- isTRUE(cond)
  if (!cond) failures <<- failures + 1L
  # Detail is diagnostic, so it is shown only when the check fails - otherwise a
  # passing run reads as a wall of irrelevant values.
  cat(if (cond) "  ok   " else "  FAIL ", label,
      if (!cond && nzchar(detail)) paste0(" [got: ", detail, "]") else "",
      "\n", sep = "")
}

load(file.path(DATA_DIR, "comfmt_releases.rda"))
load(file.path(DATA_DIR, "comfmt_lookup.rda"))
load(file.path(DATA_DIR, "poaxmpt_codes_long.rda"))
load(file.path(DATA_DIR, "beta_comfmt.rda"))
load(file.path(DATA_DIR, "beta_drg_screens.rda"))

# What AHRQ documents for each release. Independent of the parser: these come
# from the release notes and the CMR-ChangeLog-*.xlsx files, not from our output.
EXPECTED <- data.frame(
  release = c("2021.1", "2022.1", "2023.1", "2024.1", "2025.1", "2026.1"),
  codes   = c(4495L, 4319L, 4432L, 4463L, 4542L, 4567L),
  targets = c(45L, 46L, 47L, 47L, 48L, 49L),
  stringsAsFactors = FALSE
)

# Beta counts are AFTER dropping the NONE catch-all rows (see build_datasets.R).
# Target count is 30 measures + 10 hypertension labels, identical in all five.
EXPECTED_BETA <- data.frame(
  release = c("2016.2", "2017.2", "2018.1", "2019.2", "2020.1"),
  codes   = c(3166L, 3448L, 3479L, 3493L, 3495L),
  targets = c(40L, 40L, 40L, 40L, 40L),
  stringsAsFactors = FALSE
)

# The ONE place a refined measure has ever been renamed. Sourced from
# SAS_software/2022.1/CMR-ChangeLog-v20211-v20221.xlsx, sheet
# `Change_to_Comorbidity` - whose sibling sheets confirm no measure was added,
# redefined or discontinued at that boundary. Names are v2021.1 -> v2022.1.
RENAMED_2021_2022 <- c(ARTH = "AUTOIMMUNE", CHF = "HF")

# The v2021.1 combination targets whose names embed a renamed measure, plus
# VALVE_AUTOIMMUNE, which v2021.1 simply does not have (it is new at v2022.1).
# This is why v2021.1 carries six combination targets against v2022.1's seven.
COMBO_2021_ONLY  <- c("CHFHTN_CX", "CHFHTN_CXRENLFL_SEV")
COMBO_2022_ONLY  <- c("HFHTN_CX", "HFHTN_CXRENLFL_SEV", "VALVE_AUTOIMMUNE")

# Codes AHRQ has dropped from the mapping, across every release boundary.
#
# The 202 at 2021.1 -> 2022.1 are the fallout of ARTH (Rheumatoid arthritis /
# collagen vascular diseases) being redefined as AUTOIMMUNE (Autoimmune
# conditions); the other four are one-offs from later years. Transcribed from
# each release's own CMR-ChangeLog-*.xlsx `Removed_Diagnosis_Codes` sheet, which
# makes this an independent cross-check of the parse rather than a restatement
# of it: the counts match at 202 and 4.
REMOVED_2021_2022 <- strsplit(trimws("
  I998 I999 K31811 K31819 K558 K559 M01X0 M01X11 M01X12 M01X19 M01X21 M01X22
  M01X29 M01X31 M01X32 M01X39 M01X41 M01X42 M01X49 M01X51 M01X52 M01X59
  M01X61 M01X62 M01X69 M01X71 M01X72 M01X79 M01X8 M01X9 M0200 M02011 M02012
  M02019 M02021 M02022 M02029 M02031 M02032 M02039 M02041 M02042 M02049
  M02051 M02052 M02059 M02061 M02062 M02069 M02071 M02072 M02079 M0208 M0209
  M0210 M02111 M02112 M02119 M02121 M02122 M02129 M02131 M02132 M02139
  M02141 M02142 M02149 M02151 M02152 M02159 M02161 M02162 M02169 M02171
  M02172 M02179 M0218 M0219 M0220 M02211 M02212 M02219 M02221 M02222 M02229
  M02231 M02232 M02239 M02241 M02242 M02249 M02251 M02252 M02259 M02261
  M02262 M02269 M02271 M02272 M02279 M0228 M0229 M0230 M02311 M02312 M02319
  M02321 M02322 M02329 M02331 M02332 M02339 M02341 M02342 M02349 M02351
  M02352 M02359 M02361 M02362 M02369 M02371 M02372 M02379 M0238 M0239 M0280
  M02811 M02812 M02819 M02821 M02822 M02829 M02831 M02832 M02839 M02841
  M02842 M02849 M02851 M02852 M02859 M02861 M02862 M02869 M02871 M02872
  M02879 M0288 M0289 M029 M4600 M4601 M4602 M4603 M4604 M4605 M4606 M4607
  M4608 M4609 M461 M4650 M4651 M4652 M4653 M4654 M4655 M4656 M4657 M4658
  M4659 M4680 M4681 M4682 M4683 M4684 M4685 M4686 M4687 M4688 M4689 M4690
  M4691 M4692 M4693 M4694 M4695 M4696 M4697 M4698 M4699 M4980 M4981 M4982
  M4983 M4984 M4985 M4986 M4987 M4988 M4989
"), "[[:space:]]+")[[1]]

KNOWN_REMOVED_CODES <- c(REMOVED_2021_2022,
                         "O9081", "O9902", "O9903", "R939")

cat("\n== parse each release ==\n")
parsed  <- list()
poa_all <- list()
for (r in AHRQ_RELEASES) {
  f <- format_program_path(r, SAS_DIR)
  parsed[[r]]  <- parse_comfmt(f)
  poa_all[[r]] <- parse_poaxmpt(f, other_sentinel = FALSE)
  e <- EXPECTED[EXPECTED$release == r, ]
  ok(sprintf("%s codes   == %d", r, e$codes),
     nrow(parsed[[r]]) == e$codes, nrow(parsed[[r]]))
  ok(sprintf("%s targets == %d", r, e$targets),
     length(unique(parsed[[r]]$comorbidity)) == e$targets,
     length(unique(parsed[[r]]$comorbidity)))
}

cat("\n== release -> max ICD-10-CM version ==\n")
# The invariant the release-aware ICDVER ladder rests on: a release labelled
# v<Y>.1 tops out at ICD version Y - 1983, which must equal the highest
# $POAXMPT_V<nn>FMT block the release's format program actually defines.
for (r in AHRQ_RELEASES) {
  ok(sprintf("%s max ICD version == %d", r, release_max_icd_version(r)),
     max(poa_all[[r]]$version) == release_max_icd_version(r),
     max(poa_all[[r]]$version))
}

cat("\n== comfmt_releases integrity ==\n")
ok("releases present and in order",
   identical(unique(comfmt_releases$release), AHRQ_RELEASES))
ok("row count == sum of per-release code counts",
   nrow(comfmt_releases) == sum(EXPECTED$codes), nrow(comfmt_releases))
ok("no NA anywhere", !anyNA(comfmt_releases))
ok("all columns character",
   all(vapply(comfmt_releases, is.character, logical(1))))
for (r in AHRQ_RELEASES) {
  sl <- comfmt_releases[comfmt_releases$release == r, c("code", "comorbidity")]
  rownames(sl) <- NULL
  ok(sprintf("%s slice matches its parse", r), identical(sl, parsed[[r]]))
  ok(sprintf("%s slice sorted by code, codes unique", r),
     !is.unsorted(sl$code) && !anyDuplicated(sl$code))
}

cat("\n== comfmt_lookup is the default-release view ==\n")
sl <- comfmt_releases[comfmt_releases$release == "2026.1", c("code", "comorbidity")]
rownames(sl) <- NULL
ok("comfmt_lookup == comfmt_releases['2026.1'] slice",
   identical(comfmt_lookup, sl))

cat("\n== target sets are additive, with one declared rename ==\n")
# The property that makes the hardcoded target vectors, combination rules and
# exclusion hierarchies in R/comorbidity.R safe to leave release-blind: nothing
# AHRQ has ever defined was later retired, so over-specifying is harmless.
#
# v2021.1 -> v2022.1 is the single exception, and it is a RENAME, not a
# retirement: ARTH became AUTOIMMUNE and CHF became HF, taking the two
# combination targets that embed CHF with them. Declared explicitly and pinned
# below rather than waived, because a genuine retirement would look identical
# from here and would silently break `.build_result_matrix()`.
for (i in seq.int(2L, length(AHRQ_RELEASES))) {
  r0 <- AHRQ_RELEASES[i - 1L]; r1 <- AHRQ_RELEASES[i]
  a <- unique(parsed[[r0]]$comorbidity)
  b <- unique(parsed[[r1]]$comorbidity)
  gone <- setdiff(a, b)
  if (r1 == "2022.1") {
    ok("2021.1 -> 2022.1 drops exactly ARTH, CHF and the two CHF combos",
       setequal(gone, c(names(RENAMED_2021_2022), COMBO_2021_ONLY)),
       paste(gone, collapse = ","))
    ok("2021.1 -> 2022.1 adds exactly AUTOIMMUNE, HF and the three new combos",
       setequal(setdiff(b, a),
                c(unname(RENAMED_2021_2022), COMBO_2022_ONLY)),
       paste(setdiff(b, a), collapse = ","))
    ok("the rename is name-only: same target count either side of it",
       length(a) + 1L == length(b), paste(length(a), length(b)))
  } else {
    ok(sprintf("%s -> %s retires no target", r0, r1),
       length(gone) == 0L, paste(gone, collapse = ","))
  }
}

cat("\n== code sets are NOT nested (removals exist) ==\n")
# Stated explicitly so no future test assumes older subset-of newer.
removed <- unlist(lapply(seq.int(2L, length(AHRQ_RELEASES)), function(i) {
  setdiff(parsed[[AHRQ_RELEASES[i - 1L]]]$code, parsed[[AHRQ_RELEASES[i]]]$code)
}))
ok("known removals observed",
   setequal(removed, KNOWN_REMOVED_CODES),
   sprintf("%d codes, %d unexpected", length(unique(removed)),
           length(setdiff(unique(removed), KNOWN_REMOVED_CODES))))
ok(sprintf("2021.1 -> 2022.1 removes exactly the %d codes its changelog lists",
           length(REMOVED_2021_2022)),
   setequal(setdiff(parsed[["2021.1"]]$code, parsed[["2022.1"]]$code),
            REMOVED_2021_2022))

cat("\n== POA-exempt lists are release-invariant ==\n")
# The assumption that lets poaxmpt_codes_long stay release-free. Compared as
# TOKEN SEQUENCES per shared ICD version, never as raw bytes: the final block of
# each file differs by trailing content (the next release appends a block after
# it), which makes a byte compare cry wolf.
for (i in seq.int(2L, length(AHRQ_RELEASES))) {
  r0 <- AHRQ_RELEASES[i - 1L]; r1 <- AHRQ_RELEASES[i]
  shared <- intersect(poa_all[[r0]]$version, poa_all[[r1]]$version)
  bad <- Filter(function(v) {
    !identical(poa_all[[r0]]$code[poa_all[[r0]]$version == v],
               poa_all[[r1]]$code[poa_all[[r1]]$version == v])
  }, sort(unique(shared)))
  ok(sprintf("%s -> %s: v%d-v%d unchanged", r0, r1, min(shared), max(shared)),
     length(bad) == 0L, paste0("differs at v", paste(bad, collapse = ",")))
}

cat("\n== shipped poaxmpt_codes_long still matches the source ==\n")
# Membership is all that matters downstream - build_poa_exempt_formats() splits
# this into per-version code sets and the pipeline only ever tests %in%. The
# shipped object's within-version ORDER is an artifact of its incremental build
# history (v41/v42 arrived sorted, v43 in file order), so sets are compared, not
# sequences. The dataset is not regenerated by build_datasets.R.
p26 <- poa_all[["2026.1"]]
ok("same set of ICD versions",
   setequal(unique(poaxmpt_codes_long$version), unique(p26$version)))
sentinels <- sum(poaxmpt_codes_long$code == "**OTHER**")
ok("one **OTHER** sentinel per version",
   sentinels == length(unique(poaxmpt_codes_long$version)), sentinels)
for (v in sort(unique(p26$version))) {
  shipped <- poaxmpt_codes_long$code[poaxmpt_codes_long$version == v]
  shipped <- shipped[shipped != "**OTHER**"]
  ok(sprintf("v%d code set matches source (%d codes)", v, length(shipped)),
     setequal(shipped, p26$code[p26$version == v]))
}

cat("\n== beta family: parse and dataset integrity ==\n")
beta_parsed <- list()
beta_drg    <- list()
for (r in AHRQ_BETA_RELEASES) {
  f <- format_program_path(r, SAS_DIR, "beta")
  raw <- parse_comfmt(f, AHRQ_LAYOUT$beta$block)
  beta_parsed[[r]] <- raw[!grepl("^none$", raw$comorbidity, ignore.case = TRUE), ,
                          drop = FALSE]
  rownames(beta_parsed[[r]]) <- NULL
  beta_drg[[r]] <- parse_drg_screens(f)

  e <- EXPECTED_BETA[EXPECTED_BETA$release == r, ]
  ok(sprintf("%s beta codes   == %d", r, e$codes),
     nrow(beta_parsed[[r]]) == e$codes, nrow(beta_parsed[[r]]))
  ok(sprintf("%s beta targets == %d", r, e$targets),
     length(unique(beta_parsed[[r]]$comorbidity)) == e$targets,
     length(unique(beta_parsed[[r]]$comorbidity)))

  # Dropping NONE rows must never swallow anything else, and exact matching is
  # what makes dropping them safe at all: a `%` wildcard in the table would let
  # a dropped code fall through to some other pattern.
  dropped <- setdiff(unique(raw$comorbidity), unique(beta_parsed[[r]]$comorbidity))
  ok(sprintf("%s only NONE-ish targets dropped", r),
     all(grepl("^none$", dropped, ignore.case = TRUE)),
     paste(dropped, collapse = ","))
  ok(sprintf("%s beta codes carry no %% wildcard", r),
     !any(grepl("%", beta_parsed[[r]]$code, fixed = TRUE)))
}

# 30 measures + 10 hypertension labels, and every beta version defines the same
# 40. This is what lets R/beta.R hold one release-blind measure vector.
BETA_EXPECTED_TARGETS <- sort(c(
  "CHF", "VALVE", "PULMCIRC", "PERIVASC", "HTN", "HTNCX", "PARA", "NEURO",
  "CHRNLUNG", "DM", "DMCX", "HYPOTHY", "RENLFAIL", "LIVER", "ULCER", "AIDS",
  "LYMPH", "METS", "TUMOR", "ARTH", "COAG", "OBESE", "WGHTLOSS", "LYTES",
  "BLDLOSS", "ANEMDEF", "ALCOHOL", "DRUG", "PSYCH", "DEPRESS",
  "HTNPREG", "HTNWOCHF", "HTNWCHF", "HRENWORF", "HRENWRF",
  "HHRWOHRF", "HHRWCHF", "HHRWRF", "HHRWHRF", "OHTNPREG"
))
for (r in AHRQ_BETA_RELEASES) {
  ok(sprintf("%s target set is the expected 40", r),
     identical(sort(unique(beta_parsed[[r]]$comorbidity)), BETA_EXPECTED_TARGETS),
     paste(setdiff(unique(beta_parsed[[r]]$comorbidity), BETA_EXPECTED_TARGETS),
           collapse = ","))
}

ok("beta_comfmt versions present and in order",
   identical(unique(beta_comfmt$release), AHRQ_BETA_RELEASES))
ok("beta_comfmt row count == sum of per-version code counts",
   nrow(beta_comfmt) == sum(EXPECTED_BETA$codes), nrow(beta_comfmt))
ok("beta_comfmt has no NA", !anyNA(beta_comfmt))
for (r in AHRQ_BETA_RELEASES) {
  sl <- beta_comfmt[beta_comfmt$release == r, c("code", "comorbidity")]
  rownames(sl) <- NULL
  ok(sprintf("%s beta slice matches its parse", r),
     identical(sl, beta_parsed[[r]]))
  ok(sprintf("%s beta slice sorted by code, codes unique", r),
     !is.unsorted(sl$code) && !anyDuplicated(sl$code))
}

cat("\n== beta MS-DRG screens ==\n")
for (r in AHRQ_BETA_RELEASES) {
  d <- beta_drg_screens[beta_drg_screens$release == r, ]
  ok(sprintf("%s defines all 24 screens", r),
     setequal(unique(d$screen), BETA_DRG_SCREEN_NAMES),
     paste(symdiff <- union(setdiff(unique(d$screen), BETA_DRG_SCREEN_NAMES),
                            setdiff(BETA_DRG_SCREEN_NAMES, unique(d$screen))),
           collapse = ","))
  ok(sprintf("%s screen ranges well-formed", r),
     all(d$drg_high >= d$drg_low) && all(d$drg_low >= 1L) && !anyNA(d))
  sl <- d[, c("screen", "drg_low", "drg_high")]
  rownames(sl) <- NULL
  ok(sprintf("%s screen slice matches its parse", r), identical(sl, beta_drg[[r]]))
}

# The MS-DRG grouper version moves with the fiscal year, so the screens are not
# expected to be constant - only to change in the direction AHRQ's headers say.
cat(sprintf("  note  screen ranges per version: %s\n",
            paste(sprintf("%s=%d", AHRQ_BETA_RELEASES,
                          vapply(AHRQ_BETA_RELEASES,
                                 function(r) nrow(beta_drg[[r]]), integer(1))),
                  collapse = " ")))

cat("\n== index weights ==\n")
# Re-derived from each release's CMR_Index_Program_*.sas, so R/indices.R's
# CMR_MORTALITY_OVERRIDES is pinned against the AHRQ source rather than trusted.
# Releases AHRQ ships no index program for. v2021.1 is the first Refined release
# and predates the Elixhauser Comorbidity Indices, which AHRQ states "are not
# available until v2022.1"; the beta family has no index program either. Skipped
# rather than erroring, so this loop stays a real check on the releases that do
# have one.
INDEXED_RELEASES <- setdiff(AHRQ_RELEASES, "2021.1")

index_weights <- function(release) {
  f <- list.files(file.path(SAS_DIR, release),
                  pattern = "^CMR_Index_Program_.*\\.sas$", full.names = TRUE)
  if (length(f) != 1L) stop("expected one index program in ", release)
  txt  <- strip_sas_comments(read_sas_text(f))
  pat  <- "\\b([rm]w)([A-Z_]+[A-Z])\\s*=\\s*(-?[0-9]+)\\s*;"
  hits <- regmatches(txt, gregexpr(pat, txt, perl = TRUE))[[1]]
  kind <- sub("^([rm]w).*", "\\1", hits)
  nm   <- sub("^[rm]w([A-Z_]+[A-Z])\\s*=.*", "\\1", hits)
  val  <- as.integer(sub(".*=\\s*(-?[0-9]+)\\s*;", "\\1", hits))
  list(rw = stats::setNames(val[kind == "rw"], nm[kind == "rw"]),
       mw = stats::setNames(val[kind == "mw"], nm[kind == "mw"]))
}

ok("no index program shipped for 2021.1",
   length(list.files(file.path(SAS_DIR, "2021.1"),
                     pattern = "^.*Index.*\\.sas$")) == 0L)

W <- lapply(INDEXED_RELEASES, index_weights)
names(W) <- INDEXED_RELEASES
for (r in INDEXED_RELEASES) {
  ok(sprintf("%s has 38 readmission + 38 mortality weights", r),
     length(W[[r]]$rw) == 38L && length(W[[r]]$mw) == 38L,
     paste(length(W[[r]]$rw), length(W[[r]]$mw)))
}

ok("readmission weights identical across all releases",
   all(vapply(INDEXED_RELEASES[-1], function(r)
     identical(W[[r]]$rw[order(names(W[[r]]$rw))],
               W[[1]]$rw[order(names(W[[1]]$rw))]), logical(1))))

# The only mortality change in the supported range, and the ten values
# R/indices.R::CMR_MORTALITY_OVERRIDES[["2022.1"]] must carry.
EXPECTED_MW_2022 <- c(AUTOIMMUNE = -1L, CANCER_LYMPH = 6L, CANCER_METS = 23L,
                      COAG = 15L, DEPRESS = -9L, HF = 15L, LIVER_SEV = 17L,
                      NEURO_OTH = 23L, RENLFL_SEV = 8L, WGHTLOSS = 14L)
for (i in seq.int(2L, length(INDEXED_RELEASES))) {
  a <- W[[INDEXED_RELEASES[i - 1L]]]$mw
  b <- W[[INDEXED_RELEASES[i]]]$mw
  d <- names(a)[a != b[names(a)]]
  if (INDEXED_RELEASES[i] == "2023.1") {
    ok("2022.1 -> 2023.1 mortality delta is the expected ten",
       setequal(d, names(EXPECTED_MW_2022)) &&
         identical(a[names(EXPECTED_MW_2022)], EXPECTED_MW_2022),
       paste(d, collapse = ","))
  } else {
    ok(sprintf("%s -> %s mortality weights identical",
               INDEXED_RELEASES[i - 1L], INDEXED_RELEASES[i]),
       length(d) == 0L, paste(d, collapse = ","))
  }
}

cat("\n", if (failures) sprintf("FAILED: %d check(s)\n", failures)
        else "All checks passed.\n", sep = "")
quit(save = "no", status = if (failures) 1L else 0L)
