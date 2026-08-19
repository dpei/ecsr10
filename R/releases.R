# releases.R
# AHRQ release selection: the target vectors the pipeline is specified against,
# and the memoised per-release lookup tables comorbidity() scores with.

# ---- the target spec ----------------------------------------------------------
# Hoisted to package level from .apply_comorbidity_rules() and
# .handle_combination_codes() so .validate_comfmt_targets() can check a release's
# lookup table against them. The values are unchanged.
#
# These three vectors are deliberately NOT release-aware. AHRQ has never retired
# a target - the sets are purely additive 2022.1 -> 2026.1 (data-raw/checks.R
# asserts this) - so over-specifying is safe: a target absent from the selected
# release's table simply never appears in the data, leaving its branch
# unreachable and its output column all zeros. Under-specifying is what is
# dangerous, and .validate_comfmt_targets() is the guard against it.

#' Targets assigned regardless of POA
#' @keywords internal
CMR_POA_NEUTRAL <- c(
  "AIDS", "ALCOHOL", "AUTOIMMUNE", "LUNG_CHRONIC", "DEMENTIA",
  "DEPRESS", "DIAB_UNCX", "DIAB_CX", "DRUG_ABUSE", "HTN_UNCX",
  "HTN_CX", "THYROID_HYPO", "THYROID_OTH", "CANCER_LYMPH",
  "CANCER_LEUK", "CANCER_METS", "OBESE", "PERIVASC",
  "CANCER_SOLID", "CANCER_NSITU"
)

#' Targets assigned only when POA is absent, exempt, or "Y"/"W"
#' @keywords internal
CMR_POA_DEPENDENT <- c(
  "ANEMDEF", "BLDLOSS", "HF", "COAG", "LIVER_MLD", "LIVER_SEV",
  "NEURO_MOVT", "NEURO_SEIZ", "NEURO_OTH", "PARALYSIS", "PSYCHOSES",
  "PULMCIRC", "RENLFL_MOD", "RENLFL_SEV", "ULCER_PEPTIC", "WGHTLOSS",
  "CBVD_POA", "CBVD_SQLA", "VALVE"
)

#' Combination targets, which fan out into component flags
#' @keywords internal
CMR_COMBO_TARGETS <- c(
  "DRUG_ABUSEPSYCHOSES", "HFHTN_CX", "HTN_CXRENLFL_SEV",
  "HFHTN_CXRENLFL_SEV", "ALCOHOLLIVER_MLD", "VALVE_AUTOIMMUNE",
  "CBVD_SQLAPARALYSIS", "LIVER_MLD_NEURO", "NEURO_OTH_SEIZ",
  "LIVER_MLD_PULMCIRC"
)

#' Check that every target in a lookup table is one the pipeline handles
#'
#' Internal tripwire. A target present in \code{comfmt} but in none of the three
#' vectors above is silently discarded twice over: first by the
#' \code{case_when(..., TRUE ~ FALSE)} in \code{.apply_comorbidity_rules()}, and
#' again by the \code{working_targets} filter in \code{.build_result_matrix()}.
#' Neither drop warns, so the symptom is a comorbidity that simply never flags.
#'
#' This is the failure mode the annual-update checklist invites: regenerate the
#' tables for a release that added a measure, forget to add its fan-out rule, and
#' every result is quietly short one category.
#'
#' @param comfmt Data frame with a "target" column
#' @param release Character, the release being scored (for the error message)
#' @keywords internal
.validate_comfmt_targets <- function(comfmt, release = NULL) {
  known <- c(CMR_POA_NEUTRAL, CMR_POA_DEPENDENT, CMR_COMBO_TARGETS)
  unknown <- setdiff(unique(comfmt$target), known)
  if (length(unknown)) {
    stop("comorbidity lookup table",
         if (!is.null(release)) paste0(" for release ", release) else "",
         " contains ", length(unknown), " target(s) this version of ecsr10 does ",
         "not handle: ", paste(sort(unknown), collapse = ", "),
         ".\n  Each needs to be added to CMR_POA_NEUTRAL, CMR_POA_DEPENDENT or ",
         "CMR_COMBO_TARGETS (and, if a combination, given a fan-out rule in ",
         ".handle_combination_codes()); otherwise it would be silently dropped.",
         call. = FALSE)
  }
  invisible(TRUE)
}

# ---- memoised lookup tables ---------------------------------------------------
# Built once per release and reused. Populating this cache is deliberately the
# caller's job in the PARENT process, before comorbidity() forks: a fork child's
# writes die with it, so a worker that populated the cache itself would rebuild
# the tables once per worker instead of inheriting them.
.table_cache <- new.env(parent = emptyenv())

#' Compiled comorbidity lookup for one AHRQ release
#'
#' Internal. Returns the \code{(target, pattern)} table for \code{release},
#' memoised. Replaces the previous round-trip that wrote the built-in dataset to
#' a temporary CSV and read it straight back on every call.
#' @keywords internal
.comfmt_for_release <- function(release) {
  hit <- .table_cache[[release]]
  if (!is.null(hit)) return(hit)

  comfmt_releases <- NULL  # bound by data(); declared to satisfy R CMD check
  utils::data("comfmt_releases", package = "ecsr10", envir = environment())
  slice <- comfmt_releases[comfmt_releases$release == release,
                           c("code", "comorbidity"), drop = FALSE]
  if (!nrow(slice)) {
    stop("no lookup data for AHRQ release \"", release, "\"", call. = FALSE)
  }

  out <- .comfmt_from_df(slice)
  .table_cache[[release]] <- out
  out
}

#' Built-in POA-exempt code sets
#'
#' Internal. Memoised, and keyed on nothing: the POA-exempt lists are indexed by
#' ICD-10-CM version, not by AHRQ release, and every release ships identical
#' blocks for the versions it shares with its predecessor (verified in
#' data-raw/checks.R). What varies by release is only which versions are
#' reachable, and that is handled by the ICDVER cap.
#' @keywords internal
.poa_exempt_default <- function() {
  hit <- .table_cache[[".poa"]]
  if (!is.null(hit)) return(hit)

  poaxmpt_codes_long <- NULL  # bound by data()
  utils::data("poaxmpt_codes_long", package = "ecsr10", envir = environment())
  out <- .poa_exempt_from_df(poaxmpt_codes_long)
  .table_cache[[".poa"]] <- out
  out
}
