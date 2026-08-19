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

# What AHRQ documents for each release. Independent of the parser: these come
# from the release notes and the CMR-ChangeLog-*.xlsx files, not from our output.
EXPECTED <- data.frame(
  release = c("2022.1", "2023.1", "2024.1", "2025.1", "2026.1"),
  codes   = c(4319L, 4432L, 4463L, 4542L, 4567L),
  targets = c(46L, 47L, 47L, 48L, 49L),
  stringsAsFactors = FALSE
)

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

cat("\n== target sets are purely additive across releases ==\n")
# The property that makes the hardcoded target vectors, combination rules and
# exclusion hierarchies in R/comorbidity.R safe to leave release-blind: nothing
# AHRQ has ever defined was later retired, so over-specifying is harmless.
for (i in seq.int(2L, length(AHRQ_RELEASES))) {
  a <- unique(parsed[[AHRQ_RELEASES[i - 1L]]]$comorbidity)
  b <- unique(parsed[[AHRQ_RELEASES[i]]]$comorbidity)
  ok(sprintf("%s -> %s retires no target",
             AHRQ_RELEASES[i - 1L], AHRQ_RELEASES[i]),
     length(setdiff(a, b)) == 0L, paste(setdiff(a, b), collapse = ","))
}

cat("\n== code sets are NOT nested (removals exist) ==\n")
# Stated explicitly so no future test assumes older subset-of newer.
removed <- unlist(lapply(seq.int(2L, length(AHRQ_RELEASES)), function(i) {
  setdiff(parsed[[AHRQ_RELEASES[i - 1L]]]$code, parsed[[AHRQ_RELEASES[i]]]$code)
}))
ok("known removals observed (O9081, O9902, O9903, R939)",
   setequal(removed, c("O9081", "O9902", "O9903", "R939")),
   paste(removed, collapse = ","))

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

cat("\n== index weights ==\n")
# Re-derived from each release's CMR_Index_Program_*.sas, so R/indices.R's
# CMR_MORTALITY_OVERRIDES is pinned against the AHRQ source rather than trusted.
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

W <- lapply(AHRQ_RELEASES, index_weights)
names(W) <- AHRQ_RELEASES
for (r in AHRQ_RELEASES) {
  ok(sprintf("%s has 38 readmission + 38 mortality weights", r),
     length(W[[r]]$rw) == 38L && length(W[[r]]$mw) == 38L,
     paste(length(W[[r]]$rw), length(W[[r]]$mw)))
}

ok("readmission weights identical across all releases",
   all(vapply(AHRQ_RELEASES[-1], function(r)
     identical(W[[r]]$rw[order(names(W[[r]]$rw))],
               W[[1]]$rw[order(names(W[[1]]$rw))]), logical(1))))

# The only mortality change in the supported range, and the ten values
# R/indices.R::CMR_MORTALITY_OVERRIDES[["2022.1"]] must carry.
EXPECTED_MW_2022 <- c(AUTOIMMUNE = -1L, CANCER_LYMPH = 6L, CANCER_METS = 23L,
                      COAG = 15L, DEPRESS = -9L, HF = 15L, LIVER_SEV = 17L,
                      NEURO_OTH = 23L, RENLFL_SEV = 8L, WGHTLOSS = 14L)
for (i in seq.int(2L, length(AHRQ_RELEASES))) {
  a <- W[[AHRQ_RELEASES[i - 1L]]]$mw
  b <- W[[AHRQ_RELEASES[i]]]$mw
  d <- names(a)[a != b[names(a)]]
  if (AHRQ_RELEASES[i] == "2023.1") {
    ok("2022.1 -> 2023.1 mortality delta is the expected ten",
       setequal(d, names(EXPECTED_MW_2022)) &&
         identical(a[names(EXPECTED_MW_2022)], EXPECTED_MW_2022),
       paste(d, collapse = ","))
  } else {
    ok(sprintf("%s -> %s mortality weights identical",
               AHRQ_RELEASES[i - 1L], AHRQ_RELEASES[i]),
       length(d) == 0L, paste(d, collapse = ","))
  }
}

cat("\n", if (failures) sprintf("FAILED: %d check(s)\n", failures)
        else "All checks passed.\n", sep = "")
quit(save = "no", status = if (failures) 1L else 0L)
