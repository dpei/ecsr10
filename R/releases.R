# releases.R
# AHRQ release selection: the target vectors the pipeline is specified against,
# and the memoised per-release lookup tables comorbidity() scores with.

# ---- the target spec ----------------------------------------------------------
# Hoisted to package level from .apply_comorbidity_rules() and
# .handle_combination_codes() so .validate_comfmt_targets() can check a release's
# lookup table against them. The values are unchanged.
#
# These three vectors are deliberately NOT release-aware. Over-specifying is
# safe: a target absent from the selected release's table simply never appears in
# the data, leaving its branch unreachable and its output column all zeros.
# Under-specifying is what is dangerous, and .validate_comfmt_targets() is the
# guard against it.
#
# The sets are purely additive from 2022.1 onward. v2021.1 -> v2022.1 is the one
# exception, and it is a RENAME rather than a retirement: ARTH became AUTOIMMUNE,
# CHF became HF, and the two combination targets embedding CHF moved with them
# (data-raw/checks.R pins this against AHRQ's own changelog). Both vocabularies
# are listed below and both are inert outside their own family - no 2022.1+ table
# contains ARTH, no 2021.1 table contains AUTOIMMUNE - so the union stays as safe
# as it was when there was only one vocabulary. What the rename DOES break is the
# claim that the output schema never varies by release; see .cmr_final_targets().

#' Targets assigned regardless of POA
#' @keywords internal
CMR_POA_NEUTRAL <- c(
  "AIDS", "ALCOHOL", "AUTOIMMUNE", "ARTH", "LUNG_CHRONIC", "DEMENTIA",
  "DEPRESS", "DIAB_UNCX", "DIAB_CX", "DRUG_ABUSE", "HTN_UNCX",
  "HTN_CX", "THYROID_HYPO", "THYROID_OTH", "CANCER_LYMPH",
  "CANCER_LEUK", "CANCER_METS", "OBESE", "PERIVASC",
  "CANCER_SOLID", "CANCER_NSITU"
)

#' Targets assigned only when POA is absent, exempt, or "Y"/"W"
#' @keywords internal
CMR_POA_DEPENDENT <- c(
  "ANEMDEF", "BLDLOSS", "HF", "CHF", "COAG", "LIVER_MLD", "LIVER_SEV",
  "NEURO_MOVT", "NEURO_SEIZ", "NEURO_OTH", "PARALYSIS", "PSYCHOSES",
  "PULMCIRC", "RENLFL_MOD", "RENLFL_SEV", "ULCER_PEPTIC", "WGHTLOSS",
  "CBVD_POA", "CBVD_SQLA", "VALVE"
)

#' The 18 output columns AHRQ's SAS leaves missing under \code{\%LET POA = 0}
#'
#' Its \code{ARRAY COMPOA (19)} members plus the derived \code{CMR_CBVD} and the
#' internal \code{CMR_CBVD_NPOA} - 21 in all - minus the three CBVD internals it
#' \code{DROP}s in both branches. Derived from \code{CMR_POA_DEPENDENT} rather
#' than re-listed, so the two cannot drift apart.
#'
#' \code{CHF} is v2021.1's spelling of \code{HF} and is inert after
#' \code{.cmr_normalize_targets()}; \code{CBVD_POA} and \code{CBVD_SQLA} are
#' intermediates dropped by \code{.build_result_matrix()}.
#' @keywords internal
CMR_POA_DEPENDENT_COLUMNS <- c(
  paste0("CMR_", setdiff(CMR_POA_DEPENDENT, c("CHF", "CBVD_POA", "CBVD_SQLA"))),
  "CMR_CBVD"
)

#' Combination targets, which fan out into component flags
#' @keywords internal
CMR_COMBO_TARGETS <- c(
  "DRUG_ABUSEPSYCHOSES", "HFHTN_CX", "HTN_CXRENLFL_SEV",
  "HFHTN_CXRENLFL_SEV", "ALCOHOLLIVER_MLD", "VALVE_AUTOIMMUNE",
  "CBVD_SQLAPARALYSIS", "LIVER_MLD_NEURO", "NEURO_OTH_SEIZ",
  "LIVER_MLD_PULMCIRC",
  # v2021.1 spellings of two of the above, under the pre-rename measure name.
  "CHFHTN_CX", "CHFHTN_CXRENLFL_SEV"
)

# ---- the output schema --------------------------------------------------------

#' The 38 output flag names, v2022.1 vocabulary
#'
#' Hoisted out of \code{.build_result_matrix()} so the schema has one definition
#' rather than living as a local inside the stage that happens to allocate the
#' matrix.
#' @keywords internal
CMR_FINAL_TARGETS <- c(
  "CMR_AIDS", "CMR_ALCOHOL", "CMR_ANEMDEF", "CMR_AUTOIMMUNE", "CMR_BLDLOSS",
  "CMR_CANCER_LEUK", "CMR_CANCER_LYMPH", "CMR_CANCER_METS", "CMR_CANCER_NSITU",
  "CMR_CANCER_SOLID", "CMR_CBVD", "CMR_HF", "CMR_COAG", "CMR_DEMENTIA",
  "CMR_DEPRESS", "CMR_DIAB_CX", "CMR_DIAB_UNCX", "CMR_DRUG_ABUSE",
  "CMR_HTN_CX", "CMR_HTN_UNCX", "CMR_LIVER_MLD", "CMR_LIVER_SEV",
  "CMR_LUNG_CHRONIC", "CMR_NEURO_MOVT", "CMR_NEURO_OTH", "CMR_NEURO_SEIZ",
  "CMR_OBESE", "CMR_PARALYSIS", "CMR_PERIVASC", "CMR_PSYCHOSES",
  "CMR_PULMCIRC", "CMR_RENLFL_MOD", "CMR_RENLFL_SEV", "CMR_THYROID_HYPO",
  "CMR_THYROID_OTH", "CMR_ULCER_PEPTIC", "CMR_VALVE", "CMR_WGHTLOSS"
)

#' Measures AHRQ renamed at v2021.1 -> v2022.1, keyed by their v2021.1 name
#'
#' Sourced from \code{CMR-ChangeLog-v20211-v20221.xlsx}, sheet
#' \code{Change_to_Comorbidity}; its sibling sheets confirm nothing was added,
#' redefined or discontinued at that boundary, so this is the complete delta.
#' @keywords internal
CMR_RENAMED_2021 <- c(AUTOIMMUNE = "ARTH", HF = "CHF")

#' The 38 output flag names for a release
#'
#' Internal. Every refined release emits 38 flags, but v2021.1 names two of them
#' the way its own SAS program does - \code{CMR_ARTH} and \code{CMR_CHF} rather
#' than \code{CMR_AUTOIMMUNE} and \code{CMR_HF} - so a v2021.1 result diffs
#' directly against AHRQ output instead of through a rename map.
#'
#' The column ORDER is fixed across releases; only those two names move.
#' @keywords internal
.cmr_final_targets <- function(release) {
  out <- CMR_FINAL_TARGETS
  if (identical(release, "2021.1")) {
    hit <- match(paste0("CMR_", names(CMR_RENAMED_2021)), out)
    out[hit] <- paste0("CMR_", unname(CMR_RENAMED_2021))
  }
  out
}

#' v2021.1 lookup targets, keyed by their v2022.1 equivalent
#'
#' The two renamed measures, plus the two combination targets whose names embed
#' one of them. \code{VALVE_AUTOIMMUNE} is absent on purpose: it is new at
#' v2022.1, not a rename, which is why v2021.1 carries six combination targets
#' against v2022.1's seven.
#' @keywords internal
CMR_TARGETS_2021 <- c(
  AUTOIMMUNE          = "ARTH",
  HF                  = "CHF",
  HFHTN_CX            = "CHFHTN_CX",
  HFHTN_CXRENLFL_SEV  = "CHFHTN_CXRENLFL_SEV"
)

#' Rewrite a v2021.1 lookup table into the v2022.1 target vocabulary
#'
#' Internal. The whole of v2021.1's difference from v2022.1 is that four target
#' names are spelled differently; the measures, their POA classes, their
#' combination fan-outs and their hierarchies are identical. Substituting the
#' vocabulary here lets the entire pipeline stay single-vocabulary, and
#' \code{comorbidity()} renames the two affected output columns back on the way
#' out - so a v2021.1 result still carries \code{CMR_ARTH} and \code{CMR_CHF},
#' matching AHRQ's own output.
#'
#' The alternative - threading \code{release} through
#' \code{.apply_comorbidity_rules()}, \code{.handle_combination_codes()} and
#' \code{.build_result_matrix()} and duplicating two fan-out rules - would put a
#' release branch inside three stages that are covered by five parity harnesses,
#' to express one substitution.
#'
#' Idempotent, and a no-op for every other release, so it is safe to apply to a
#' caller-supplied \code{comfmt} too: a table that already speaks the v2022.1
#' vocabulary has nothing to rewrite.
#' @keywords internal
.cmr_normalize_targets <- function(comfmt, release) {
  if (!identical(release, "2021.1")) return(comfmt)
  hit <- match(comfmt$target, CMR_TARGETS_2021)
  comfmt$target[!is.na(hit)] <- names(CMR_TARGETS_2021)[hit[!is.na(hit)]]
  comfmt
}

# ---- the beta family ----------------------------------------------------------
# The beta software's own vocabulary, in the order its ARRAY COM1 declares it.
# Deliberately kept apart from the refined vectors above: several names collide
# (CHF, VALVE, COAG, OBESE ...) while meaning something different, because a beta
# measure is screened by MS-DRG where the refined equivalent is screened by POA.
# The CMRB_ output prefix exists so the two can never be confused downstream.

#' The 30 beta comorbidity measures, in ARRAY COM1 order
#' @keywords internal
BETA_MEASURES <- c(
  "CHF", "VALVE", "PULMCIRC", "PERIVASC",
  "HTN", "HTNCX", "PARA", "NEURO", "CHRNLUNG",
  "DM", "DMCX", "HYPOTHY", "RENLFAIL", "LIVER",
  "ULCER", "AIDS", "LYMPH", "METS", "TUMOR",
  "ARTH", "COAG", "OBESE", "WGHTLOSS", "LYTES",
  "BLDLOSS", "ANEMDEF", "ALCOHOL", "DRUG", "PSYCH",
  "DEPRESS"
)

#' The 10 detailed hypertension labels
#'
#' Not measures. They are intermediates: the SAS \code{SELECT(DXVALUE)} block
#' turns each into a \code{_}-suffixed flag, which then sets HTNCX (and sometimes
#' CHF and RENLFAIL), and later drives which MS-DRG screens apply.
#' @keywords internal
BETA_HTN_PSEUDO <- c(
  "HTNPREG", "HTNWOCHF", "HTNWCHF", "HRENWORF", "HRENWRF",
  "HHRWOHRF", "HHRWCHF", "HHRWRF", "HHRWHRF", "OHTNPREG"
)

#' The 24 MS-DRG screens, in the order the beta analysis program resolves them
#'
#' Names are the SAS format names. \code{.beta_comorbidity_flags()} addresses
#' them by dropping the \code{DRG} suffix, so \code{CARDDRG} is \code{f("CARD")}.
#' @keywords internal
BETA_DRG_SCREEN_NAMES <- c(
  "CARDDRG", "PERIDRG", "CEREDRG", "NERVDRG", "PULMDRG", "DIABDRG",
  "HYPODRG", "RENALDRG", "RENFDRG", "LIVERDRG", "ULCEDRG", "HIVDRG",
  "LEUKDRG", "CANCDRG", "ARTHDRG", "NUTRDRG", "ANEMDRG", "ALCDRG",
  "HTNCXDRG", "HTNDRG", "COAGDRG", "PSYDRG", "OBESEDRG", "DEPRSDRG"
)

#' The 31 beta output columns
#'
#' The 30 measures plus HTN_C, which the SAS derives last, after the MS-DRG
#' screen has run.
#' @keywords internal
BETA_FINAL_TARGETS <- paste0("CMRB_", c(BETA_MEASURES, "HTN_C"))

#' Check that every target in a beta lookup table is one the pipeline handles
#'
#' Internal. The beta analogue of \code{.validate_comfmt_targets()}, and it
#' guards the same silent drop: a label the matcher produces but the flag matrix
#' has no column for vanishes without a warning.
#' @param comfmt Data frame with a "target" column
#' @param release Character, the version being scored (for the error message)
#' @keywords internal
.validate_beta_targets <- function(comfmt, release = NULL) {
  known <- c(BETA_MEASURES, BETA_HTN_PSEUDO)
  unknown <- setdiff(unique(comfmt$target), known)
  if (length(unknown)) {
    stop("beta comorbidity lookup table",
         if (!is.null(release)) paste0(" for version ", release) else "",
         " contains ", length(unknown), " target(s) this version of ecsr10 does ",
         "not handle: ", paste(sort(unknown), collapse = ", "),
         ".\n  Each needs to be added to BETA_MEASURES or BETA_HTN_PSEUDO ",
         "(R/releases.R) and given a rule in R/beta.R; otherwise it would be ",
         "silently dropped.", call. = FALSE)
  }
  invisible(TRUE)
}

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
  # CMR_POA_DEPENDENT_COLUMNS is derived, so this only fails if CMR_POA_DEPENDENT
  # or CMR_FINAL_TARGETS moved out from under it - which is exactly when a silent
  # NA-vs-0 divergence from the SAS %LET POA = 0 branch would reappear.
  stopifnot(
    length(CMR_POA_DEPENDENT_COLUMNS) == 18L,
    all(CMR_POA_DEPENDENT_COLUMNS %in% CMR_FINAL_TARGETS)
  )

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
  key <- paste0("refined:", release)
  hit <- .table_cache[[key]]
  if (!is.null(hit)) return(hit)

  comfmt_releases <- NULL  # bound by data(); declared to satisfy R CMD check
  utils::data("comfmt_releases", package = "ecsr10", envir = environment())
  slice <- comfmt_releases[comfmt_releases$release == release,
                           c("code", "comorbidity"), drop = FALSE]
  if (!nrow(slice)) {
    stop("no lookup data for AHRQ release \"", release, "\"", call. = FALSE)
  }

  out <- .comfmt_from_df(slice)
  .table_cache[[key]] <- out
  out
}

#' Compiled comorbidity lookup for one beta version
#'
#' Internal. The beta analogue of \code{.comfmt_for_release()}, memoised in the
#' same environment under a \code{beta:} key. Release labels cannot collide
#' across the two families, but the tables they key are different shapes, so the
#' prefix keeps a future refactor from crossing them.
#' @keywords internal
.beta_comfmt_for_release <- function(release) {
  key <- paste0("beta:", release)
  hit <- .table_cache[[key]]
  if (!is.null(hit)) return(hit)

  beta_comfmt <- NULL  # bound by data(); declared to satisfy R CMD check
  utils::data("beta_comfmt", package = "ecsr10", envir = environment())
  slice <- beta_comfmt[beta_comfmt$release == release,
                       c("code", "comorbidity"), drop = FALSE]
  if (!nrow(slice)) {
    stop("no lookup data for AHRQ beta version \"", release, "\"", call. = FALSE)
  }

  out <- .comfmt_from_df(slice)
  .table_cache[[key]] <- out
  out
}

#' MS-DRG exclusion screens for one beta version
#'
#' Internal. Returns a named list mapping screen name to a two-column integer
#' matrix of inclusive \code{(low, high)} MS-DRG bounds, memoised.
#'
#' A named list of matrices rather than the long data frame the dataset ships,
#' because the pipeline's only question is "is this DRG in this screen", asked 24
#' times per call; the reshape is done once here instead of per screen per call.
#' @keywords internal
.beta_drg_for_release <- function(release) {
  key <- paste0("betadrg:", release)
  hit <- .table_cache[[key]]
  if (!is.null(hit)) return(hit)

  beta_drg_screens <- NULL  # bound by data()
  utils::data("beta_drg_screens", package = "ecsr10", envir = environment())
  slice <- beta_drg_screens[beta_drg_screens$release == release, , drop = FALSE]
  if (!nrow(slice)) {
    stop("no MS-DRG screens for AHRQ beta version \"", release, "\"",
         call. = FALSE)
  }

  out <- lapply(split(slice, factor(slice$screen, levels = unique(slice$screen))),
                function(d) cbind(low = as.integer(d$drg_low),
                                  high = as.integer(d$drg_high)))
  .table_cache[[key]] <- out
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
