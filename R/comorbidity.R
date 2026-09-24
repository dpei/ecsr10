#' Apply comorbidity analysis to patient diagnosis data
#'
#' This function processes patient diagnosis data to identify comorbidities based on ICD-10-CM
#' diagnosis codes using AHRQ/HCUP's Elixhauser Comorbidity Software.
#'
#' Two families of that software are available, selected with \code{variant}:
#' \describe{
#'   \item{\code{"refined"} (default)}{The Elixhauser Comorbidity Software
#'     \strong{Refined} for ICD-10-CM, v2021.1 onward. 38 comorbidity measures,
#'     18 of them identified using Present on Admission (POA) indicators.
#'     Handles POA logic, combination codes, and hierarchical exclusions.}
#'   \item{\code{"beta"}}{The superseded \strong{beta} software, v2016.2-v2020.1.
#'     30 measures plus a derived \code{CMRB_HTN_C}, no POA at all. In its place
#'     it applies an MS-DRG screen: a comorbidity is suppressed when the
#'     encounter's MS-DRG is directly related to it, which is why this variant
#'     takes \code{drg_col} and why AHRQ states it applies to inpatient data only.
#'     Emits no comorbidity indices.}
#' }
#'
#' @param patient_data Data frame containing patient diagnosis data
#' @param dx_cols Character vector of column names containing diagnosis codes.
#'
#'   \strong{Secondary diagnoses only.} The AHRQ software opens its diagnosis
#'   loop at position 2 - \code{DO I = 2 TO MIN(&MAXNDX,&NUMDX)} in the refined
#'   mapping program, \code{DO I = 2 TO MIN(NDX,&NUMDX)} in the beta one - so the
#'   principal diagnosis is never examined. ecsr10 scores every column it is
#'   handed, which makes the exclusion the caller's job: pass \code{dx2},
#'   \code{dx3}, ... and not \code{dx1}. A column whose name resolves to position
#'   1 draws a warning, since including it changes flags rather than erroring.
#'
#'   The encounter-level diagnosis count is \emph{not} applied either. SAS caps
#'   its loop at \code{I10_NDX}, so a populated column past that count is
#'   ignored; ecsr10 scores every column in \code{dx_cols} regardless. Truncate
#'   \code{dx_cols} yourself if your data carries a meaningful \code{I10_NDX}.
#' @param poa_cols Character vector of column names containing POA indicators
#'   (optional). Must be the same length as \code{dx_cols}.
#'
#'   \strong{Paired with \code{dx_cols} by name, not by vector order.} Each
#'   column is assigned a diagnosis position from the \emph{first run of digits}
#'   in its name, and a diagnosis is matched to the POA column sharing its
#'   position - so \code{c("poa3", "poa2")} pairs exactly as \code{c("poa2",
#'   "poa3")} does. The two position sets must be identical; a partial mismatch
#'   such as \code{dx_cols = c("dx2", "dx3")} with \code{poa_cols = c("poa2",
#'   "poa4")} is an error, because position 3 would otherwise receive a blank POA
#'   value and silently stop flagging every POA-dependent measure. Names yielding
#'   no digits, or duplicate positions - HCUP's own \code{I10_DX2}/\code{I10_DX3}
#'   both resolve to 10 - are rejected for the same reason. Use names whose only
#'   digits are the position, e.g. \code{dx2}/\code{poa2}.
#' @param year_col Character, name of column containing year information (default "year")
#' @param quarter_col Character, name of column containing quarter information (default "quarter")
#' @param release Character, which AHRQ release to score with. \code{NULL}
#'   (default) means \code{cmr_version(variant)} - the newest release of the
#'   selected family. See \code{\link{cmr_releases}} for the supported set;
#'   labels do not overlap between families, so a release given under the wrong
#'   \code{variant} is rejected with a message saying which family it belongs to.
#'
#'   For \code{variant = "refined"} the release selects the diagnosis-code table,
#'   the newest ICD-10-CM version reachable from year/quarter, and - via
#'   \code{\link{cmr_index}} - the index weights. For \code{variant = "beta"} it
#'   selects the code table and the MS-DRG screens. It is recorded on the result
#'   as the \code{cmr_release} attribute, alongside \code{cmr_variant}. An
#'   explicit \code{comfmt} overrides only the code table; the release's
#'   ICD-version cap still applies.
#' @param comfmt Data frame with comorbidity format containing "target" and "pattern" columns.
#'   If NULL (default), uses the built-in table for \code{release}.
#' @param poa_exempt Named list of POA exempt codes by version. If NULL (default),
#'   uses the built-in poaxmpt_codes_long data when use_poa is TRUE.
#' @param use_poa Logical, whether to apply POA logic (default TRUE). \code{TRUE}
#'   requires \code{year_col} and \code{quarter_col}, which select the ICD-10-CM
#'   version and hence the POA-exemption list.
#'
#'   \code{FALSE} is the analogue of the SAS \code{\%LET POA = 0} switch, and it
#'   is not merely "POA ignored": 18 of the 38 measures are defined only in terms
#'   of POA, so they are not scoreable at all. They are returned as \code{NA}
#'   rather than 0, reproducing AHRQ's SAS (which initializes them only inside
#'   \code{\%if &POA.=1}, so with the switch off they reach the output dataset
#'   missing; the columns stay present, so the schema is unchanged), and the
#'   exclusions that depend on them (the liver and renal-failure hierarchies, and the whole \code{CMR_CBVD}
#'   derivation) do not run, matching the SAS program, which puts those rules
#'   inside its POA branch. \code{\link{cmr_index}} then scores only the 20
#'   POA-neutral measures.
#' @param wildcard_mode Character, pattern matching mode ("wildcard" or "regex")
#' @param ncores Integer, number of forked workers the encounters are split across
#'   (default 1, i.e. serial). The input is divided into \code{ncores} contiguous
#'   blocks of rows and the whole pipeline runs on each block independently.
#'   Values above \code{parallel::detectCores()} are clamped with a warning.
#'   Forking is unavailable on Windows, where any value above 1 falls back to
#'   serial with a warning.
#'
#'   Results are identical for every \code{ncores} value. Every stage is
#'   per-encounter, and overlapping lookup patterns are resolved by the pattern's
#'   index in \code{comfmt}, which is the same in every block - so a block's
#'   code-to-target map is an exact restriction of the whole-dataset one.
#'
#'   Peak memory grows with \code{ncores} - each worker materializes its own
#'   long-format intermediate - so memory, not core count, is usually the
#'   binding constraint on large inputs.
#' @param variant Character, which AHRQ software family to apply: \code{"refined"}
#'   (default) or \code{"beta"}. See the description above; the two produce
#'   different output columns and are not interchangeable.
#' @param drg_col Character, name of the column holding the encounter's MS-DRG.
#'   \code{variant = "beta"} only, where it drives the MS-DRG exclusion screen.
#'   \code{NULL} (default) runs with no screen and warns: that is faithful - the
#'   SAS program applies no suppression when \code{DRG} is missing, since
#'   \code{PUT(DRG, <screen>.)} cannot then return \code{"YES"} - but it is
#'   almost never what an inpatient analysis wants, so it is surfaced.
#' @return Data frame with the original patient diagnosis data plus, for
#'   \code{variant = "refined"}, 38 \code{CMR_*} flags, or for
#'   \code{variant = "beta"}, 31 \code{CMRB_*} flags. Flags are 0/1 integers -
#'   except the 18 POA-dependent measures under \code{use_poa = FALSE}, which are
#'   \code{NA}.
#'
#'   Every refined release emits the same 38 columns in the same order, but
#'   v2021.1 names two of them the way its own SAS program does - \code{CMR_ARTH}
#'   and \code{CMR_CHF}, which v2022.1 renamed to \code{CMR_AUTOIMMUNE} and
#'   \code{CMR_HF}. Code that hardcodes those two names must branch on the
#'   release, or read them from \code{cmr_releases()}-driven metadata.
#' @details
#' Under \code{variant = "refined"} the function identifies 38 comorbidity
#' categories (v2022.1 names shown; at v2021.1, \code{CMR_AUTOIMMUNE} is
#' \code{CMR_ARTH} and \code{CMR_HF} is \code{CMR_CHF}):
#' \itemize{
#'   \item CMR_AIDS: HIV/AIDS
#'   \item CMR_ALCOHOL: Alcohol abuse
#'   \item CMR_ANEMDEF: Deficiency anemia
#'   \item CMR_AUTOIMMUNE: Autoimmune conditions
#'   \item CMR_BLDLOSS: Blood loss
#'   \item CMR_CANCER_LEUK: Leukemia
#'   \item CMR_CANCER_LYMPH: Lymphoma
#'   \item CMR_CANCER_METS: Metastatic cancer
#'   \item CMR_CANCER_NSITU: Non-situ cancer
#'   \item CMR_CANCER_SOLID: Solid tumor cancer
#'   \item CMR_CBVD: Cerebrovascular disease
#'   \item CMR_HF: Heart failure
#'   \item CMR_COAG: Coagulopathy
#'   \item CMR_DEMENTIA: Dementia
#'   \item CMR_DEPRESS: Depression
#'   \item CMR_DIAB_CX: Diabetes with complications
#'   \item CMR_DIAB_UNCX: Diabetes without complications
#'   \item CMR_DRUG_ABUSE: Drug abuse
#'   \item CMR_HTN_CX: Hypertension with complications
#'   \item CMR_HTN_UNCX: Hypertension without complications
#'   \item CMR_LIVER_MLD: Mild liver disease
#'   \item CMR_LIVER_SEV: Severe liver disease
#'   \item CMR_LUNG_CHRONIC: Chronic lung disease
#'   \item CMR_NEURO_MOVT: Neurological movement disorders
#'   \item CMR_NEURO_OTH: Other neurological disorders
#'   \item CMR_NEURO_SEIZ: Seizure disorders
#'   \item CMR_OBESE: Obesity
#'   \item CMR_PARALYSIS: Paralysis
#'   \item CMR_PERIVASC: Peripheral vascular disease
#'   \item CMR_PSYCHOSES: Psychoses
#'   \item CMR_PULMCIRC: Pulmonary circulation disorders
#'   \item CMR_RENLFL_MOD: Moderate renal failure
#'   \item CMR_RENLFL_SEV: Severe renal failure
#'   \item CMR_THYROID_HYPO: Hypothyroid disorders
#'   \item CMR_THYROID_OTH: Other thyroid disorders
#'   \item CMR_ULCER_PEPTIC: Peptic ulcer disease
#'   \item CMR_VALVE: Valvular disease
#'   \item CMR_WGHTLOSS: Weight loss
#' }
#'
#' Under \code{variant = "beta"} it identifies 30 measures - \code{CMRB_AIDS},
#' \code{CMRB_ALCOHOL}, \code{CMRB_ANEMDEF}, \code{CMRB_ARTH},
#' \code{CMRB_BLDLOSS}, \code{CMRB_CHF}, \code{CMRB_CHRNLUNG},
#' \code{CMRB_COAG}, \code{CMRB_DEPRESS}, \code{CMRB_DM}, \code{CMRB_DMCX},
#' \code{CMRB_DRUG}, \code{CMRB_HTN}, \code{CMRB_HTNCX}, \code{CMRB_HYPOTHY},
#' \code{CMRB_LIVER}, \code{CMRB_LYMPH}, \code{CMRB_LYTES}, \code{CMRB_METS},
#' \code{CMRB_NEURO}, \code{CMRB_OBESE}, \code{CMRB_PARA},
#' \code{CMRB_PERIVASC}, \code{CMRB_PSYCH}, \code{CMRB_PULMCIRC},
#' \code{CMRB_RENLFAIL}, \code{CMRB_TUMOR}, \code{CMRB_ULCER},
#' \code{CMRB_VALVE}, \code{CMRB_WGHTLOSS} - plus \code{CMRB_HTN_C}, the union
#' of \code{CMRB_HTN} and \code{CMRB_HTNCX}, derived after the MS-DRG screen.
#'
#' The \code{CMRB_} prefix is deliberate rather than cosmetic. Several beta
#' measure names coincide with refined ones while meaning something different -
#' beta \code{CHF} and \code{VALVE} are screened by MS-DRG where refined
#' \code{CMR_HF} and \code{CMR_VALVE} are screened by POA - so the two families
#' are kept in separate namespaces and \code{\link{cmr_index}} refuses a beta
#' result outright.
#' @section Deliberate differences from the AHRQ SAS programs:
#' Scoring reproduces AHRQ's SAS output exactly on every supported release - see
#' the parity harnesses in \code{simulation/}. What follows is the short list of
#' places where the \emph{interface} is deliberately more forgiving, or where the
#' caller retains a responsibility SAS takes on. None of them changes a flag for
#' input SAS would also have accepted.
#' \describe{
#'   \item{Diagnosis codes are normalized}{\code{\link{normalize_icd10}} upcases,
#'     strips embedded decimal points and trims whitespace, so \code{"e11.9"} and
#'     \code{"E119"} score alike. The SAS program does none of this - it has no
#'     \code{UPCASE}, \code{COMPRESS} or \code{STRIP} on the diagnosis array -
#'     and expects uppercase, decimal-free codes. Anything SAS matches, ecsr10
#'     matches identically; ecsr10 additionally matches input SAS would have
#'     dropped.}
#'   \item{POA values are upcased}{SAS compares \code{DXPOA IN ("Y","W")}
#'     case-sensitively, so a lowercase \code{"y"} is treated as not-present-on-
#'     admission. ecsr10 upcases first.}
#'   \item{The encounter's diagnosis count is not applied}{SAS bounds its loop at
#'     \code{MIN(&MAXNDX,&NUMDX)}, ignoring a populated diagnosis column beyond
#'     the encounter's \code{I10_NDX}. ecsr10 has no \code{I10_NDX} analogue and
#'     scores every column in \code{dx_cols}; truncate it yourself if your data
#'     carries a meaningful count.}
#'   \item{The principal diagnosis is not excluded automatically}{See
#'     \code{dx_cols}. Passing a position-1 column warns rather than errors.}
#'   \item{Beta output is a superset}{\code{variant = "beta"} keeps
#'     \code{CMRB_HTN} and \code{CMRB_HTNCX}, which AHRQ's program \code{DROP}s in
#'     favour of their union \code{CMRB_HTN_C}, and carries a \code{CMRB_} prefix
#'     the SAS does not use.}
#'   \item{\code{cmr_index()} on an all-missing frame}{A hand-built frame in which
#'     every \code{CMR_*} input is \code{NA} scores 0, where SAS \code{SUM()} of
#'     all-missing terms returns missing. Not reachable from
#'     \code{comorbidity()} output, which always carries the 20 POA-neutral
#'     measures as 0/1 - and it is that same NA-as-zero treatment that makes
#'     \code{use_poa = FALSE} match SAS.}
#' }
#' @examples
#' \dontrun{
#' # Simple workflow - uses built-in lookup data automatically
#' result <- comorbidity(patient_data, 
#'                       dx_cols = c("dx2", "dx3", "dx4"),
#'                       poa_cols = c("poa2", "poa3", "poa4"))
#' 
#' # Calculate risk indices
#' result_with_indices <- cmr_index(result)
#' 
#' # Advanced workflow with a custom lookup table. `comfmt` takes a plain data
#' # frame of (target, pattern) - "%" is a wildcard under the default
#' # wildcard_mode - so no package internals are involved. Targets are the AHRQ
#' # measure names WITHOUT the CMR_ prefix the output columns carry.
#' custom_comfmt <- data.frame(
#'   target  = c("DIAB_UNCX", "HTN_UNCX"),
#'   pattern = c("E11%",      "I10")
#' )
#' result <- comorbidity(patient_data,
#'                       dx_cols = c("dx2", "dx3", "dx4"),
#'                       comfmt = custom_comfmt)
#'
#' # Split the encounters across 4 forked workers
#' result <- comorbidity(patient_data,
#'                       dx_cols = c("dx2", "dx3", "dx4"),
#'                       poa_cols = c("poa2", "poa3", "poa4"),
#'                       ncores = 4)
#'
#' # The superseded beta software, which screens on MS-DRG instead of POA
#' beta <- comorbidity(patient_data,
#'                     dx_cols = c("dx2", "dx3", "dx4"),
#'                     variant = "beta",
#'                     release = "2020.1",
#'                     drg_col = "drg")
#' }
#' @export
comorbidity <- function(patient_data,
                        dx_cols,
                        poa_cols = NULL,
                        year_col = "year",
                        quarter_col = "quarter",
                        release = NULL,
                        comfmt = NULL,
                        poa_exempt = NULL,
                        use_poa = TRUE,
                        wildcard_mode = "wildcard",
                        ncores = 1,
                        variant = c("refined", "beta"),
                        drg_col = NULL) {

  # `variant` and `drg_col` are appended rather than slotted in beside `release`
  # so that existing positional calls keep working. `release` defaults to NULL
  # and is resolved below rather than being written `cmr_version(variant)` in the
  # signature: that form does work, via R's lazy promises seeing the rebound
  # `variant`, but it depends on evaluation order in a way the next reader should
  # not have to reason about.
  variant <- .resolve_variant(variant)
  if (is.null(release)) release <- cmr_version(variant)
  release <- .resolve_release(release, variant)

  # Input validation
  stopifnot(all(dx_cols %in% names(patient_data)))
  if (!is.null(poa_cols)) stopifnot(length(poa_cols) == length(dx_cols))
  # use_poa reaches both `&&` (scalar) and `&` (vectorized, inside case_when)
  # contexts, so an unchecked NA fails in whichever it hits first rather than
  # here. Checked up front so the error names the argument.
  stopifnot(is.logical(use_poa), length(use_poa) == 1L, !is.na(use_poa))
  ncores <- .resolve_ncores(ncores)

  # Both families score secondary diagnoses only, so this precedes the beta
  # dispatch.
  .warn_primary_dx(dx_cols)

  if (identical(variant, "beta")) {
    return(.comorbidity_beta(patient_data, dx_cols, poa_cols, release, comfmt,
                             use_poa, wildcard_mode, ncores, drg_col))
  }

  # After the beta dispatch: beta rejects poa_cols outright, and that message is
  # the more useful one when a beta call supplies them.
  .validate_dx_poa_pairing(dx_cols, poa_cols)
  if (!is.null(drg_col)) {
    stop("`drg_col` applies only to variant = \"beta\". The refined software ",
         "dropped the MS-DRG exclusion screen at v2021.1 in favour of POA ",
         "indicators, so an MS-DRG has no role in it.", call. = FALSE)
  }

  # Use default lookup data if not provided.
  #
  # Built here in the parent, BEFORE any fork, so that a chunked run builds the
  # tables once rather than once per worker; the workers inherit the finished
  # tables through the fork. They are also memoised per release, so looping over
  # releases - or calling comorbidity() repeatedly - reparses nothing.
  #
  # The cache must be populated here and the tables passed down as arguments. A
  # fork child's writes are discarded when it exits, so a worker that reached
  # into the cache itself would silently rebuild the tables once per worker.
  if (is.null(comfmt)) {
    comfmt <- .comfmt_for_release(release)
  }

  # v2021.1 spells four of its targets differently (ARTH/CHF and the two
  # combination targets embedding CHF). Rewritten to the v2022.1 vocabulary so
  # the pipeline below stays single-vocabulary; the two affected output columns
  # are renamed back to AHRQ's spelling at the end. Applied to a caller-supplied
  # comfmt as well, since the silent-drop path is the same either way.
  comfmt <- .cmr_normalize_targets(comfmt, release)

  # Fails loudly on a target the pipeline would otherwise drop in silence.
  .validate_comfmt_targets(comfmt, release)

  if (use_poa && is.null(poa_exempt)) {
    poa_exempt <- .poa_exempt_default()
  }

  if (use_poa && !is.null(poa_exempt)) {
    if (!year_col %in% names(patient_data)) {
      stop(paste("Year column", year_col, "not found in patient diagnosis data"))
    }
    if (!quarter_col %in% names(patient_data)) {
      stop(paste("Quarter column", quarter_col, "not found in patient diagnosis data"))
    }
  }
  
  n_rows <- nrow(patient_data)

  # The release's ICD-10-CM ceiling. Encounters dated past the release's coverage
  # clamp to it, exactly as the release's own SAS ladder does.
  max_icd_version <- .release_max_icd_version(release)

  # Chunks cannot outnumber rows. This is a validity guard, not a performance
  # threshold - ncores is honoured as requested on frames of any size.
  chunks <- min(ncores, max(n_rows, 1L))

  if (chunks > 1L) {
    row_blocks <- split(seq_len(n_rows), cut(seq_len(n_rows), chunks, labels = FALSE))
    pieces <- parallel::mclapply(row_blocks, function(rows) {
      .comorbidity_flags(patient_data[rows, , drop = FALSE], dx_cols, poa_cols,
                         year_col, quarter_col, comfmt, poa_exempt, use_poa,
                         wildcard_mode, max_icd_version)
    }, mc.cores = chunks)

    # mclapply reports worker errors as try-error elements instead of raising them.
    # Without this check a failed worker would silently contribute a block of 0
    # flags for its encounters rather than an error.
    failed <- vapply(pieces, inherits, logical(1), "try-error")
    if (any(failed)) {
      stop("parallel chunk processing failed on ", sum(failed), " of ", length(pieces),
           " chunk(s): ",
           conditionMessage(attr(pieces[[which(failed)[1]]], "condition")), call. = FALSE)
    }

    result_matrix <- do.call(rbind, pieces)
  } else {
    result_matrix <- .comorbidity_flags(patient_data, dx_cols, poa_cols, year_col,
                                        quarter_col, comfmt, poa_exempt, use_poa,
                                        wildcard_mode, max_icd_version)
  }

  # Back to the release's own spelling. .build_result_matrix() always emits the
  # v2022.1 names; only v2021.1 moves, and only these two.
  colnames(result_matrix) <- .cmr_final_targets(release)

  # Convert matrix to data frame and bind to original data
  out <- dplyr::bind_cols(patient_data, as.data.frame(result_matrix))

  # Record the release as an attribute rather than a column, so the result schema
  # is unchanged and readr::write_csv() ignores it. cmr_index() reads it to warn
  # about a release mismatch; it never uses it as a default, since `[`-subsetting
  # a data frame drops attributes and silent action at a distance would be worse
  # than the mismatch it was meant to catch.
  attr(out, "cmr_release") <- release
  attr(out, "cmr_variant") <- "refined"
  # Whether the 18 POA-dependent measures were scoreable. Recorded so a consumer
  # can tell a POA-scored frame from a POA-off one; nothing in the package acts
  # on it, since attributes do not survive `[`-subsetting.
  attr(out, "cmr_use_poa") <- use_poa
  out
}

#' Score encounters with the AHRQ beta comorbidity software
#'
#' Internal. The \code{variant = "beta"} half of \code{comorbidity()}: validates
#' the beta-only arguments, builds the two lookup tables in the PARENT process
#' before any fork, then runs \code{.beta_comorbidity_flags()} serially or over
#' contiguous row blocks.
#'
#' Kept separate rather than folded into the refined path, which shares almost
#' nothing with it beyond the code matcher and is covered by five parity
#' harnesses that this change has no business disturbing.
#' @keywords internal
.comorbidity_beta <- function(patient_data, dx_cols, poa_cols, release, comfmt,
                              use_poa, wildcard_mode, ncores, drg_col) {

  # The beta software has no POA concept at all, so silently ignoring POA input
  # would let a caller believe POA was being honoured. use_poa is left at its
  # TRUE default by every caller who never thought about it, so only an explicit
  # poa_cols is an error; a TRUE use_poa with no poa_cols is just the default.
  if (!is.null(poa_cols)) {
    stop("`poa_cols` is not used by variant = \"beta\": the beta software ",
         "predates POA-based identification and screens on MS-DRG instead. ",
         "Pass `drg_col`, or use variant = \"refined\" for POA.", call. = FALSE)
  }

  drg <- NULL
  drg_screens <- NULL
  if (is.null(drg_col)) {
    warning("no `drg_col`: the MS-DRG exclusion screen is not applied, so ",
            "comorbidities related to the principal diagnosis are not ",
            "suppressed. This matches AHRQ's SAS run against data with a ",
            "missing DRG, but is rarely what an inpatient analysis wants.",
            call. = FALSE)
  } else {
    if (length(drg_col) != 1L || !drg_col %in% names(patient_data)) {
      stop("`drg_col` must name a single column present in `patient_data`; ",
           "\"", paste(drg_col, collapse = "\", \""), "\" is not.", call. = FALSE)
    }
    drg <- patient_data[[drg_col]]
    drg_screens <- .beta_drg_for_release(release)
  }

  # Built here in the parent, BEFORE any fork, for the same reason the refined
  # path does it: a fork child's cache writes die with it, so a worker reaching
  # into .table_cache itself would rebuild the tables once per worker.
  if (is.null(comfmt)) comfmt <- .beta_comfmt_for_release(release)
  .validate_beta_targets(comfmt, release)

  n_rows <- nrow(patient_data)
  chunks <- min(ncores, max(n_rows, 1L))

  if (chunks > 1L) {
    row_blocks <- split(seq_len(n_rows), cut(seq_len(n_rows), chunks, labels = FALSE))
    pieces <- parallel::mclapply(row_blocks, function(rows) {
      .beta_comorbidity_flags(patient_data[rows, , drop = FALSE], dx_cols,
                              if (is.null(drg)) NULL else drg[rows],
                              comfmt, drg_screens, wildcard_mode)
    }, mc.cores = chunks)

    failed <- vapply(pieces, inherits, logical(1), "try-error")
    if (any(failed)) {
      stop("parallel chunk processing failed on ", sum(failed), " of ", length(pieces),
           " chunk(s): ",
           conditionMessage(attr(pieces[[which(failed)[1]]], "condition")), call. = FALSE)
    }
    result_matrix <- do.call(rbind, pieces)
  } else {
    result_matrix <- .beta_comorbidity_flags(patient_data, dx_cols, drg, comfmt,
                                             drg_screens, wildcard_mode)
  }

  out <- dplyr::bind_cols(patient_data, as.data.frame(result_matrix))
  attr(out, "cmr_release") <- release
  attr(out, "cmr_variant") <- "beta"
  out
}

#' Run the comorbidity pipeline over one block of encounters
#'
#' Internal. Takes lookup tables that are already built and returns only the
#' 38-column CMR flag matrix, one row per row of \code{patient_data} - not the
#' input columns bound back on, which would make every worker ship the caller's
#' whole frame back through a pipe.
#'
#' Every stage below is per-encounter: \code{row_id} is a row number over the
#' frame passed in and \code{n_rows} is its row count, so both are local to the
#' block. Running this on a contiguous block therefore produces exactly the rows
#' the whole-dataset call would have produced, which is what makes the chunked
#' path in \code{comorbidity()} identical to the serial one rather than merely
#' similar.
#' @keywords internal
.comorbidity_flags <- function(patient_data, dx_cols, poa_cols, year_col,
                               quarter_col, comfmt, poa_exempt, use_poa,
                               wildcard_mode, max_icd_version = 43L) {

  n_rows <- nrow(patient_data)

  # Pre-compile regex patterns (major optimization)
  if (wildcard_mode == "wildcard") {
    comfmt_compiled <- comfmt %>%
      dplyr::mutate(.regex = paste0("^", stringr::str_replace_all(pattern, "%", ".*"), "$"))
  } else {
    comfmt_compiled <- comfmt %>% dplyr::mutate(.regex = pattern)
  }
  
  # Convert regex patterns to compiled form for faster matching
  regex_list <- purrr::map(comfmt_compiled$.regex, ~ stringr::regex(.x, ignore_case = TRUE))
  
  # Add row identifiers and determine ICD versions efficiently
  claims_prep <- patient_data %>%
    dplyr::mutate(row_id = dplyr::row_number())
  
  if (use_poa && !is.null(poa_exempt)) {
    claims_prep <- claims_prep %>%
      dplyr::mutate(
        icd_version = determine_icd_version(
          as.integer(.data[[year_col]]),
          as.integer(.data[[quarter_col]]),
          max_icd_version
        )
      )

    # Every ICD version reached must have a POA-exempt list, or the left_join
    # below yields is_exempt = NA -> FALSE for its rows. That failure is silent
    # and moves flags in BOTH directions: POA-dependent targets lose their
    # exempt-code assignments, and .handle_special_cases() starts firing
    # CBVD_NPOA on rows it never should, which suppresses CMR_CBVD. The built-in
    # data covers v33-v43, so only a user-supplied poa_exempt can trip this.
    have <- as.integer(stringr::str_extract(names(poa_exempt), "\\d+"))
    missing_v <- setdiff(unique(claims_prep$icd_version), have)
    if (length(missing_v)) {
      stop("poa_exempt has no code list for ICD-10-CM version(s) ",
           paste(sort(missing_v), collapse = ", "),
           "; it covers ", paste(sort(have), collapse = ", "), call. = FALSE)
    }
  }
  
  # OPTIMIZATION 1: Reshape to long format once (instead of processing row by row)
  dx_long <- claims_prep %>%
    dplyr::select(row_id, dplyr::all_of(dx_cols), 
                  if(use_poa && !is.null(poa_exempt)) "icd_version" else NULL) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(dx_cols),
      names_to = "dx_position",
      values_to = "dx_code",
      values_drop_na = TRUE
    ) %>%
    dplyr::filter(dx_code != "") %>%
    dplyr::mutate(
      dx_code = normalize_icd10(as.character(dx_code)),
      dx_position_num = as.integer(stringr::str_extract(dx_position, "\\d+"))
    )
  
  # Add POA information if available
  if (!is.null(poa_cols)) {
    poa_long <- claims_prep %>%
      dplyr::select(row_id, dplyr::all_of(poa_cols)) %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(poa_cols),
        names_to = "poa_position", 
        values_to = "poa_code",
        values_drop_na = TRUE
      ) %>%
      dplyr::mutate(
        poa_position_num = as.integer(stringr::str_extract(poa_position, "\\d+")),
        poa_code = toupper(as.character(poa_code))
      ) %>%
      dplyr::select(row_id, poa_position_num, poa_code)

    # dx and POA columns are paired by the digits in their NAMES, not by
    # position in the vectors, and str_extract() takes the FIRST run of digits.
    # The pairing is validated in the parent by .validate_dx_poa_pairing(), on
    # the column names rather than on this reshaped frame: an all-NA diagnosis
    # column never reaches here (values_drop_na drops it), and this function runs
    # once per forked block.
    dx_long <- dx_long %>%
      dplyr::left_join(poa_long, by = c("row_id", "dx_position_num" = "poa_position_num")) %>%
      dplyr::mutate(poa_code = dplyr::coalesce(poa_code, ""))
  } else {
    dx_long$poa_code <- ""
  }
  
  # OPTIMIZATION 2: Vectorized pattern matching
  unique_codes <- unique(dx_long$dx_code)

  # Create mapping of codes to targets efficiently.
  #
  # `priority` is the pattern's index in comfmt, which is what resolves overlapping
  # patterns below. Because that index is a property of the lookup table rather than
  # of the data, every chunk resolves an overlap the same way, and a chunk's map is
  # an exact restriction of the whole-dataset map to the codes the chunk contains.
  # That is what makes the ncores > 1 result identical to the serial one rather than
  # merely similar.
  match_block <- function(idx) {
    purrr::map_dfr(idx, function(i) {
      matching_codes <- unique_codes[stringr::str_detect(unique_codes, regex_list[[i]])]
      if (length(matching_codes) == 0L) return(NULL)
      dplyr::tibble(
        dx_code = matching_codes,
        target = comfmt_compiled$target[i],
        priority = i  # for handling overlapping patterns
      )
    })
  }

  n_patterns <- nrow(comfmt_compiled)

  code_target_map <- match_block(seq_len(n_patterns))

  # A code set that matches nothing yields a 0-column frame, which the arrange() below
  # cannot resolve columns against. Give it the schema explicitly.
  if (nrow(code_target_map) == 0L) {
    code_target_map <- dplyr::tibble(
      dx_code = character(), target = character(), priority = integer()
    )
  }

  # Handle overlapping patterns by keeping first match per code
  code_target_map <- code_target_map %>%
    dplyr::arrange(dx_code, priority) %>%
    dplyr::group_by(dx_code) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-priority)
  
  # Join targets back to diagnosis data
  dx_with_targets <- dx_long %>%
    dplyr::inner_join(code_target_map, by = "dx_code")
  
  # OPTIMIZATION 3: Efficient POA exempt lookup
  if (use_poa && !is.null(poa_exempt)) {
    poa_exempt_lookup <- purrr::map_dfr(names(poa_exempt), function(v) {
      version_num <- as.integer(stringr::str_extract(v, "\\d+"))
      dplyr::tibble(
        icd_version = version_num,
        dx_code = poa_exempt[[v]],
        is_exempt = TRUE
      )
    })
    
    dx_with_targets <- dx_with_targets %>%
      dplyr::left_join(poa_exempt_lookup, by = c("icd_version", "dx_code")) %>%
      dplyr::mutate(is_exempt = dplyr::coalesce(is_exempt, FALSE))
  } else {
    dx_with_targets$is_exempt <- FALSE
  }
  
  # Apply business rules and build the flag matrix for this block
  .apply_comorbidity_rules(dx_with_targets, n_rows, use_poa)
}

#' Extract the diagnosis position embedded in a column name
#'
#' Internal helper. Columns are paired by the FIRST run of digits in their name,
#' which is the contract \code{.comorbidity_flags()} implements via
#' \code{str_extract(name, "\\\\d+")}. Returned as \code{NA_integer_} for a name
#' carrying no digits at all.
#' @keywords internal
.dx_position_of <- function(cols) {
  suppressWarnings(as.integer(stringr::str_extract(cols, "\\d+")))
}

#' Warn when a diagnosis column looks like the primary diagnosis
#'
#' Internal helper. The AHRQ SAS programs open their diagnosis loop at position
#' 2 - \code{DO I = 2 TO MIN(&MAXNDX,&NUMDX)} in the refined mapping program,
#' \code{DO I = 2 TO MIN(NDX,&NUMDX)} in the beta one - so the principal
#' diagnosis never reaches the comorbidity logic. ecsr10 scores every column it
#' is handed, which makes the exclusion the caller's job; passing \code{dx1}
#' therefore departs from SAS silently, changing flags rather than erroring.
#'
#' A warning rather than an error, because a caller who has already dropped the
#' principal diagnosis may legitimately have named the first remaining column
#' \code{dx1}. Both variants are checked - the beta software applies the same
#' loop bound.
#' @keywords internal
.warn_primary_dx <- function(dx_cols) {
  pos <- .dx_position_of(dx_cols)
  hits <- dx_cols[!is.na(pos) & pos == 1L]
  if (!length(hits)) return(invisible(NULL))

  warning("dx_cols includes ", paste(sQuote(hits), collapse = ", "),
          ", which resolve(s) to diagnosis position 1.\n",
          "  The AHRQ software scores SECONDARY diagnoses only - its loop runs ",
          "`DO I = 2 TO ...`, so the principal diagnosis is never examined.\n",
          "  ecsr10 scores every column in dx_cols, so results will differ from ",
          "SAS unless you drop the principal diagnosis yourself, e.g. ",
          "dx_cols = setdiff(dx_cols, \"dx1\").\n",
          "  Ignore this if the column is already a secondary diagnosis that ",
          "merely happens to be named with a 1.",
          call. = FALSE)
  invisible(NULL)
}

#' Validate that dx and POA columns pair one-to-one
#'
#' Internal helper. dx and POA columns are joined on the FIRST run of digits in
#' their names, never on position within the two vectors, so \code{dx_cols} and
#' \code{poa_cols} must yield identical position sets. Anything less pairs some
#' positions and silently blanks the rest: an unpaired diagnosis coalesces to
#' \code{""}, which is neither \code{"Y"} nor \code{"W"}, so every POA-dependent
#' measure stops firing on that diagnosis with no error and no warning.
#'
#' Three distinct failures are rejected here:
#' \itemize{
#'   \item \strong{No digits.} A name like \code{"dx_a"} yields \code{NA} and
#'     joins to nothing.
#'   \item \strong{Duplicate positions.} \code{c("I10_DX2", "I10_DX3")} both
#'     yield 10 - the first digit run wins - so one diagnosis silently inherits
#'     the other's POA, and the join fans out.
#'   \item \strong{Unequal position sets.} The partial-overlap case; a fully
#'     disjoint set is just its extreme, and gets the HCUP-naming hint because
#'     that is nearly always the cause.
#' }
#'
#' Checked here in the parent, on the column NAMES, rather than downstream on
#' the reshaped data: a data-derived check misses a position whose column is
#' entirely NA (the pivot drops it), and would re-run once per forked block.
#' @keywords internal
.validate_dx_poa_pairing <- function(dx_cols, poa_cols) {
  if (is.null(poa_cols)) return(invisible(NULL))

  dx_pos  <- .dx_position_of(dx_cols)
  poa_pos <- .dx_position_of(poa_cols)

  naming_hint <- paste0(
    "  Columns are paired by the first run of digits in their names, so names ",
    "like \"I10_DX2\" resolve to 10, not 2.\n",
    "  Rename to a form whose only digits are the position, e.g. dx2/poa2."
  )

  bad_dx  <- dx_cols[is.na(dx_pos)]
  bad_poa <- poa_cols[is.na(poa_pos)]
  if (length(bad_dx) || length(bad_poa)) {
    stop("no diagnosis position can be extracted from ",
         paste(sQuote(c(bad_dx, bad_poa)), collapse = ", "), ".\n",
         naming_hint, call. = FALSE)
  }

  dup_dx  <- unique(dx_pos[duplicated(dx_pos)])
  dup_poa <- unique(poa_pos[duplicated(poa_pos)])
  if (length(dup_dx) || length(dup_poa)) {
    stop("duplicate diagnosis position(s) ",
         paste(sort(unique(c(dup_dx, dup_poa))), collapse = ", "),
         " extracted from ",
         if (length(dup_dx)) "dx_cols" else "poa_cols",
         if (length(dup_dx) && length(dup_poa)) " and poa_cols" else "",
         ".\n  Each dx column must map to exactly one POA column.\n",
         naming_hint, call. = FALSE)
  }

  if (!setequal(dx_pos, poa_pos)) {
    unpaired_dx  <- setdiff(dx_pos, poa_pos)
    unpaired_poa <- setdiff(poa_pos, dx_pos)
    stop("dx and POA columns do not pair one-to-one: dx columns give ",
         "position(s) ", paste(sort(dx_pos), collapse = ", "),
         " but POA columns give ", paste(sort(poa_pos), collapse = ", "), ".\n",
         if (length(unpaired_dx))
           paste0("  Diagnosis position(s) ",
                  paste(sort(unpaired_dx), collapse = ", "),
                  " would receive a blank POA value, silently suppressing every ",
                  "POA-dependent measure on them.\n"),
         if (length(unpaired_poa))
           paste0("  POA position(s) ",
                  paste(sort(unpaired_poa), collapse = ", "),
                  " match no diagnosis column and would be ignored.\n"),
         naming_hint, call. = FALSE)
  }

  invisible(NULL)
}

#' Validate and clamp a requested core count
#'
#' Internal helper. Returns a usable positive integer core count, warning rather
#' than erroring when the request is merely unsatisfiable (too many cores, or
#' Windows, where \code{parallel::mclapply} cannot fork).
#' @keywords internal
.resolve_ncores <- function(ncores) {
  ncores <- suppressWarnings(as.integer(ncores))
  if (length(ncores) != 1L || is.na(ncores) || ncores < 1L) {
    stop("ncores must be a single positive integer", call. = FALSE)
  }

  available <- parallel::detectCores(logical = TRUE)
  if (!is.na(available) && ncores > available) {
    warning("ncores = ", ncores, " exceeds the ", available,
            " available core(s); using ", available, call. = FALSE)
    ncores <- as.integer(available)
  }

  if (ncores > 1L && .Platform$OS.type == "windows") {
    warning("parallel::mclapply cannot fork on Windows; running serially (ncores = 1)",
            call. = FALSE)
    ncores <- 1L
  }

  ncores
}

#' Internal function to apply comorbidity business rules
#' @keywords internal
.apply_comorbidity_rules <- function(dx_with_targets, n_rows, use_poa) {
  
  # Comorbidity categories - see R/releases.R for the vectors and why they are
  # deliberately not release-aware.
  poa_neutral   <- CMR_POA_NEUTRAL
  poa_dependent <- CMR_POA_DEPENDENT

  # With POA off, AHRQ's SAS never initializes the POA-dependent measures - they
  # are assigned only inside `%if &POA.=1`, so they leave the DATA step missing.
  # .build_result_matrix() writes that NA, so no gated target is assigned here.

  # Apply assignment rules
  valid_assignments <- dx_with_targets %>%
    dplyr::mutate(
      should_assign = dplyr::case_when(
        target %in% poa_neutral ~ TRUE,
        use_poa & target %in% poa_dependent & (is_exempt | poa_code %in% c("Y", "W")) ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    dplyr::filter(should_assign)
  
  # Handle special cases and combination codes
  all_assignments <- .handle_special_cases(valid_assignments, dx_with_targets, use_poa)
  
  # Build and return result matrix
  .build_result_matrix(all_assignments, n_rows, use_poa)
}

#' Handle special cases and combination codes
#' @keywords internal
.handle_special_cases <- function(valid_assignments, dx_with_targets, use_poa) {
  
  special_assignments <- dplyr::tibble()
  
  if (use_poa && nrow(dx_with_targets) > 0) {
    # CBVD_NPOA case
    cbvd_npoa <- dx_with_targets %>%
      dplyr::filter(target == "CBVD_POA", !is_exempt, poa_code %in% c("N", "U")) %>%
      dplyr::mutate(target = "CBVD_NPOA", should_assign = TRUE) %>%
      dplyr::select(names(valid_assignments))
    
    special_assignments <- dplyr::bind_rows(special_assignments, cbvd_npoa)
  }
  
  # Handle combination codes
  combination_assignments <- .handle_combination_codes(dx_with_targets, use_poa)
  
  # Combine all valid assignments
  dplyr::bind_rows(valid_assignments, special_assignments, combination_assignments)
}

#' Handle combination codes logic
#' @keywords internal
.handle_combination_codes <- function(dx_with_targets, use_poa) {
  
  combination_assignments <- dplyr::tibble()
  
  if (nrow(dx_with_targets) == 0) return(combination_assignments)
  
  combo_codes <- CMR_COMBO_TARGETS


  combo_data <- dx_with_targets %>% dplyr::filter(target %in% combo_codes)
  
  if (nrow(combo_data) == 0) return(combination_assignments)
  
  for (i in seq_len(nrow(combo_data))) {
    row_data <- combo_data[i, ]
    target <- row_data$target
    
    # Apply combination logic based on target
    if (target == "DRUG_ABUSEPSYCHOSES") {
      combination_assignments <- dplyr::bind_rows(combination_assignments,
        row_data %>% dplyr::mutate(target = "DRUG_ABUSE", should_assign = TRUE))
      if (use_poa && (row_data$is_exempt | row_data$poa_code %in% c("Y", "W"))) {
        combination_assignments <- dplyr::bind_rows(combination_assignments,
          row_data %>% dplyr::mutate(target = "PSYCHOSES", should_assign = TRUE))
      }
    }
    
    if (target == "HFHTN_CX") {
      combination_assignments <- dplyr::bind_rows(combination_assignments,
        row_data %>% dplyr::mutate(target = "HTN_CX", should_assign = TRUE))
      if (use_poa && (row_data$is_exempt | row_data$poa_code %in% c("Y", "W"))) {
        combination_assignments <- dplyr::bind_rows(combination_assignments,
          row_data %>% dplyr::mutate(target = "HF", should_assign = TRUE))
      }
    }
    
    if (target == "HTN_CXRENLFL_SEV") {
      combination_assignments <- dplyr::bind_rows(combination_assignments,
        row_data %>% dplyr::mutate(target = "HTN_CX", should_assign = TRUE))
      if (use_poa && (row_data$is_exempt | row_data$poa_code %in% c("Y", "W"))) {
        combination_assignments <- dplyr::bind_rows(combination_assignments,
          row_data %>% dplyr::mutate(target = "RENLFL_SEV", should_assign = TRUE))
      }
    }
    
    if (target == "HFHTN_CXRENLFL_SEV") {
      combination_assignments <- dplyr::bind_rows(combination_assignments,
        row_data %>% dplyr::mutate(target = "HTN_CX", should_assign = TRUE))
      if (use_poa && (row_data$is_exempt | row_data$poa_code %in% c("Y", "W"))) {
        combination_assignments <- dplyr::bind_rows(combination_assignments,
          row_data %>% dplyr::mutate(target = "HF", should_assign = TRUE),
          row_data %>% dplyr::mutate(target = "RENLFL_SEV", should_assign = TRUE))
      }
    }
    
    if (target == "ALCOHOLLIVER_MLD") {
      combination_assignments <- dplyr::bind_rows(combination_assignments,
        row_data %>% dplyr::mutate(target = "ALCOHOL", should_assign = TRUE))
      if (use_poa && (row_data$is_exempt | row_data$poa_code %in% c("Y", "W"))) {
        combination_assignments <- dplyr::bind_rows(combination_assignments,
          row_data %>% dplyr::mutate(target = "LIVER_MLD", should_assign = TRUE))
      }
    }
    
    if (target == "VALVE_AUTOIMMUNE") {
      combination_assignments <- dplyr::bind_rows(combination_assignments,
        row_data %>% dplyr::mutate(target = "AUTOIMMUNE", should_assign = TRUE))
      if (use_poa && (row_data$is_exempt | row_data$poa_code %in% c("Y", "W"))) {
        combination_assignments <- dplyr::bind_rows(combination_assignments,
          row_data %>% dplyr::mutate(target = "VALVE", should_assign = TRUE))
      }
    }
    
    if (target == "CBVD_SQLAPARALYSIS") {
      if (use_poa && (row_data$is_exempt | row_data$poa_code %in% c("Y", "W"))) {
        combination_assignments <- dplyr::bind_rows(combination_assignments,
          row_data %>% dplyr::mutate(target = "PARALYSIS", should_assign = TRUE),
          row_data %>% dplyr::mutate(target = "CBVD_SQLA", should_assign = TRUE))
      }
    }
    
    if (target == "LIVER_MLD_NEURO") {
      if (use_poa && (row_data$is_exempt | row_data$poa_code %in% c("Y", "W"))) {
        combination_assignments <- dplyr::bind_rows(combination_assignments,
          row_data %>% dplyr::mutate(target = "LIVER_MLD", should_assign = TRUE),
          row_data %>% dplyr::mutate(target = "NEURO_OTH", should_assign = TRUE))
      }
    }
    
    if (target == "NEURO_OTH_SEIZ") {
      if (use_poa && (row_data$is_exempt | row_data$poa_code %in% c("Y", "W"))) {
        combination_assignments <- dplyr::bind_rows(combination_assignments,
          row_data %>% dplyr::mutate(target = "NEURO_OTH", should_assign = TRUE),
          row_data %>% dplyr::mutate(target = "NEURO_SEIZ", should_assign = TRUE))
      }
    }

    if (target == "LIVER_MLD_PULMCIRC") {
      if (use_poa && (row_data$is_exempt | row_data$poa_code %in% c("Y", "W"))) {
        combination_assignments <- dplyr::bind_rows(combination_assignments,
          row_data %>% dplyr::mutate(target = "LIVER_MLD", should_assign = TRUE),
          row_data %>% dplyr::mutate(target = "PULMCIRC", should_assign = TRUE))
      }
    }
  }
  
  combination_assignments
}

#' Build the final result matrix with comorbidity flags
#' @keywords internal
.build_result_matrix <- function(all_assignments, n_rows, use_poa) {
  
  # Always the v2022.1 vocabulary. comorbidity() renames the two columns v2021.1
  # spells differently after this returns - see .cmr_final_targets().
  final_targets <- CMR_FINAL_TARGETS
  
  # Include intermediate targets for CBVD derivation
  intermediate_targets <- c("CMR_CBVD_POA", "CMR_CBVD_NPOA", "CMR_CBVD_SQLA")
  working_targets <- c(final_targets, intermediate_targets)
  
  # Initialize result matrix
  result_matrix <- matrix(0L, nrow = n_rows, ncol = length(working_targets))
  colnames(result_matrix) <- working_targets
  
  # Populate matrix
  if (nrow(all_assignments) > 0) {
    row_targets <- all_assignments %>%
      dplyr::mutate(cmr_target = paste0("CMR_", target)) %>%
      dplyr::filter(cmr_target %in% working_targets) %>%
      dplyr::group_by(row_id, cmr_target) %>%
      dplyr::summarise(.groups = "drop") %>%
      dplyr::mutate(
        row_idx = match(row_id, seq_len(n_rows)),
        col_idx = match(cmr_target, working_targets)
      ) %>%
      dplyr::filter(!is.na(row_idx), !is.na(col_idx))
    
    if (nrow(row_targets) > 0) {
      result_matrix[cbind(row_targets$row_idx, row_targets$col_idx)] <- 1L
    }
  }
  
  # Apply exclusion rules and derive final flags
  result_matrix <- .apply_exclusion_rules(result_matrix, use_poa, n_rows)

  # POA off: the 18 gated measures are not scoreable, and AHRQ's SAS says so with
  # a missing value rather than a zero (CMR_Mapping_Program_*.sas, the %else of
  # the `%if &POA.=1` initialization block). The columns stay in the output -
  # SAS's LENGTH, ARRAY and LABEL statements are all unconditional - so a POA-off
  # result is schema-compatible with a POA-on one.
  #
  # Written AFTER the exclusions so every ifelse() above keeps testing plain 0/1
  # and none of them can start producing NA of their own.
  if (!use_poa) {
    result_matrix[, CMR_POA_DEPENDENT_COLUMNS] <- NA_integer_
  }

  # Return only final targets
  result_matrix[, final_targets, drop = FALSE]
}

#' Apply hierarchical exclusion rules
#' @keywords internal
.apply_exclusion_rules <- function(result_matrix, use_poa, n_rows) {
  
  # Basic exclusions
  result_matrix[, "CMR_DIAB_UNCX"] <- ifelse(result_matrix[, "CMR_DIAB_CX"] == 1, 0L, result_matrix[, "CMR_DIAB_UNCX"])
  result_matrix[, "CMR_HTN_UNCX"] <- ifelse(result_matrix[, "CMR_HTN_CX"] == 1, 0L, result_matrix[, "CMR_HTN_UNCX"])
  
  # Cancer hierarchy
  mets_mask <- result_matrix[, "CMR_CANCER_METS"] == 1
  result_matrix[mets_mask, "CMR_CANCER_SOLID"] <- 0L
  result_matrix[mets_mask, "CMR_CANCER_NSITU"] <- 0L
  
  solid_mask <- result_matrix[, "CMR_CANCER_SOLID"] == 1
  result_matrix[solid_mask, "CMR_CANCER_NSITU"] <- 0L
  
  if (use_poa) {
    result_matrix[, "CMR_LIVER_MLD"] <- ifelse(result_matrix[, "CMR_LIVER_SEV"] == 1, 0L, result_matrix[, "CMR_LIVER_MLD"])
    result_matrix[, "CMR_RENLFL_MOD"] <- ifelse(result_matrix[, "CMR_RENLFL_SEV"] == 1, 0L, result_matrix[, "CMR_RENLFL_MOD"])
    
    # Handle CBVD derivation
    if ("CMR_CBVD_POA" %in% colnames(result_matrix) && "CMR_CBVD_SQLA" %in% colnames(result_matrix)) {
      cbvd_poa_mask <- result_matrix[, "CMR_CBVD_POA"] == 1
      cbvd_npoa_mask <- if ("CMR_CBVD_NPOA" %in% colnames(result_matrix)) {
        result_matrix[, "CMR_CBVD_NPOA"] == 1
      } else {
        rep(FALSE, n_rows)
      }
      cbvd_sqla_mask <- result_matrix[, "CMR_CBVD_SQLA"] == 1
      
      result_matrix[, "CMR_CBVD"] <- as.integer(cbvd_poa_mask | (!cbvd_poa_mask & !cbvd_npoa_mask & cbvd_sqla_mask))
    }
  }
  
  result_matrix
}