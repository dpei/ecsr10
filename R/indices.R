# Mortality weights that differ from the v2023.1-onward baseline encoded in
# cmr_index(). Extracted from each release's CMR_Index_Program_*.sas: readmission
# weights are identical across all supported releases, and the only mortality
# change AHRQ has made in this range is v2022.1 -> v2023.1, on these ten
# categories. Storing the delta rather than five full 38-entry tables keeps the
# reviewable surface at ten numbers.
CMR_MORTALITY_OVERRIDES <- list(
  "2022.1" = c(
    CMR_AUTOIMMUNE = -1, CMR_CANCER_LYMPH = 6, CMR_CANCER_METS = 23,
    CMR_COAG = 15, CMR_DEPRESS = -9, CMR_HF = 15, CMR_LIVER_SEV = 17,
    CMR_NEURO_OTH = 23, CMR_RENLFL_SEV = 8, CMR_WGHTLOSS = 14
  )
)

#' Calculate CMR mortality and readmission indices
#'
#' Calculates weighted risk scores for hospital readmission and mortality based on 
#' the 38 CMR comorbidity flags. Uses established weights from the CMR methodology.
#'
#' @param core_data Data frame with comorbidity flags (output from comorbidity function)
#' @param release Character, which AHRQ CMR release's weights to apply (default
#'   \code{cmr_version()}). See \code{\link{cmr_releases}}. If \code{core_data}
#'   carries a \code{cmr_release} attribute - which \code{\link{comorbidity}}
#'   sets - and it differs from \code{release}, a warning is issued.
#' @return Data frame with two additional columns: CMR_Index_Readmission and CMR_Index_Mortality
#' @details
#' The function applies validated weights to each comorbidity category:
#' \itemize{
#'   \item Readmission weights range from -2 (obesity) to 11 (metastatic cancer),
#'     and are identical across every supported release
#'   \item Mortality weights range from -9 (psychoses) to 22 (metastatic cancer, neurological disorders)
#'   \item Mortality weights differ between v2022.1 and later releases on ten
#'     categories; all releases from v2023.1 onward agree
#'   \item Comorbidity columns absent from \code{core_data} are skipped rather than
#'     treated as an error, so a partially flagged frame yields a partial score
#'   \item \code{NA} flags contribute 0. This is what makes a
#'     \code{use_poa = FALSE} frame score the way AHRQ's SAS does: its index
#'     program is POA-blind - one flat 38-element array summed with
#'     \code{SUM(OF ...)}, which ignores missing terms - so with POA off both
#'     indices silently cover only the 20 POA-neutral measures. They come back as
#'     plausible small integers, not \code{NA}. Do not "fix" this to propagate
#'     \code{NA}; it would break parity with the SAS program
#'   \item A zero-row frame returns zero rows with both index columns present
#'   \item Final scores are unweighted sums of individual comorbidity contributions
#' }
#'
#' One residual difference from SAS, unreachable through \code{comorbidity()}:
#' \code{SUM()} returns missing when \emph{every} term is missing, whereas this
#' function returns 0. The 20 POA-neutral measures are always non-missing, so it
#' can only be reached with a hand-built all-\code{NA} frame.
#' @examples
#' \dontrun{
#' # After running comorbidity analysis
#' result_with_comorbidities <- comorbidity(patient_data, ...)
#' result_with_indices <- cmr_index(result_with_comorbidities)
#' 
#' # View index distributions
#' summary(result_with_indices$CMR_Index_Readmission)
#' summary(result_with_indices$CMR_Index_Mortality)
#' }
#' @export
cmr_index <- function(core_data, release = cmr_version()) {

  # A beta result has no index at any release, and its CMRB_ columns would not
  # be found by the weight lookup anyway - which would return a silent 0/0 rather
  # than an error. Checked before .resolve_release() so the message names the
  # real problem instead of complaining that "2020.1" is not a refined release.
  scored_variant <- attr(core_data, "cmr_variant")
  if (identical(scored_variant, "beta")) {
    stop("these flags were produced by the AHRQ beta software (variant = ",
         "\"beta\"), which has no comorbidity indices. AHRQ publishes the ",
         "Elixhauser Comorbidity Indices only from v2021.1 onward, and only for ",
         "the refined measures. Re-score with variant = \"refined\" to index.",
         call. = FALSE)
  }

  # No index program exists for these, so applying some other release's weights
  # would be inventing a result. AHRQ: the Elixhauser Comorbidity Indices Refined
  # for ICD-10-CM "are not available until v2022.1".
  #
  # Checked BEFORE .resolve_release(), which validates against the refined list
  # and would reject a beta label as an unknown release - technically true, and
  # useless here, since its suggestion to "pass variant = \"beta\"" names an
  # argument cmr_index() does not have.
  if (is.character(release) && length(release) == 1L && !is.na(release) &&
      release %in% CMR_NO_INDEX_RELEASES) {
    stop("AHRQ ships no comorbidity index program for release \"", release, "\". ",
         if (release %in% CMR_BETA_RELEASES) {
           paste0("v", release, " is beta-era software, which emits flags only; ",
                  "the Elixhauser Comorbidity Indices begin at v2022.1 and are ",
                  "defined only over the refined measures.")
         } else {
           paste0("The Elixhauser Comorbidity Indices are not available until ",
                  "v2022.1. Score the flags under \"", release, "\" if you need ",
                  "that release's mapping, then index them explicitly with a ",
                  "release that has weights, e.g. cmr_index(x, release = \"2022.1\").")
         }, call. = FALSE)
  }

  release <- .resolve_release(release)

  # Scoring flags from one release with another's weights is legal - the caller
  # may genuinely want it - but it is almost always a mistake, so it is surfaced.
  # Deliberately a warning and not a silent default: `[`-subsetting a data frame
  # drops attributes, so defaulting off the attribute would make behaviour depend
  # on whether the frame had been touched in between.
  scored_under <- attr(core_data, "cmr_release")
  if (!is.null(scored_under) && !identical(scored_under, release)) {
    warning("flags were scored under AHRQ release ", scored_under,
            " but are being indexed with ", release, " weights; ",
            "pass release = \"", scored_under, "\" to match", call. = FALSE)
  }

  result_data <- core_data

  # Define readmission weights
  rw <- list(
    CMR_AIDS = 5, CMR_ALCOHOL = 3, CMR_ANEMDEF = 5, CMR_AUTOIMMUNE = 2,
    CMR_BLDLOSS = 2, CMR_CANCER_LEUK = 10, CMR_CANCER_LYMPH = 7,
    CMR_CANCER_METS = 11, CMR_CANCER_NSITU = 0, CMR_CANCER_SOLID = 7,
    CMR_CBVD = 0, CMR_HF = 7, CMR_COAG = 3, CMR_DEMENTIA = 1,
    CMR_DEPRESS = 2, CMR_DIAB_CX = 4, CMR_DIAB_UNCX = 0,
    CMR_DRUG_ABUSE = 6, CMR_HTN_CX = 0, CMR_HTN_UNCX = 0,
    CMR_LIVER_MLD = 3, CMR_LIVER_SEV = 10, CMR_LUNG_CHRONIC = 4,
    CMR_NEURO_MOVT = 1, CMR_NEURO_OTH = 2, CMR_NEURO_SEIZ = 5,
    CMR_OBESE = -2, CMR_PARALYSIS = 3, CMR_PERIVASC = 1,
    CMR_PSYCHOSES = 6, CMR_PULMCIRC = 3, CMR_RENLFL_MOD = 4,
    CMR_RENLFL_SEV = 8, CMR_THYROID_HYPO = 0, CMR_THYROID_OTH = 0,
    CMR_ULCER_PEPTIC = 2, CMR_VALVE = 0, CMR_WGHTLOSS = 6
  )
  
  # Define mortality weights
  mw <- list(
    CMR_AIDS = -4, CMR_ALCOHOL = -1, CMR_ANEMDEF = -3, CMR_AUTOIMMUNE = 0,
    CMR_BLDLOSS = -4, CMR_CANCER_LEUK = 9, CMR_CANCER_LYMPH = 5,
    CMR_CANCER_METS = 22, CMR_CANCER_NSITU = 0, CMR_CANCER_SOLID = 10,
    CMR_CBVD = 5, CMR_HF = 14, CMR_COAG = 14, CMR_DEMENTIA = 5,
    CMR_DEPRESS = -8, CMR_DIAB_CX = -2, CMR_DIAB_UNCX = 0,
    CMR_DRUG_ABUSE = -7, CMR_HTN_CX = 1, CMR_HTN_UNCX = 0,
    CMR_LIVER_MLD = 2, CMR_LIVER_SEV = 16, CMR_LUNG_CHRONIC = 2,
    CMR_NEURO_MOVT = -1, CMR_NEURO_OTH = 22, CMR_NEURO_SEIZ = 2,
    CMR_OBESE = -7, CMR_PARALYSIS = 4, CMR_PERIVASC = 3,
    CMR_PSYCHOSES = -9, CMR_PULMCIRC = 4, CMR_RENLFL_MOD = 3,
    CMR_RENLFL_SEV = 7, CMR_THYROID_HYPO = -3, CMR_THYROID_OTH = -8,
    CMR_ULCER_PEPTIC = 0, CMR_VALVE = 0, CMR_WGHTLOSS = 13
  )

  # Per-release mortality overrides. The lists above are the v2023.1-onward
  # weights, which every release from 2023.1 to 2026.1 shares; only v2022.1
  # differs, on these ten categories. Readmission weights have never changed.
  # Both facts are re-derived from the AHRQ CMR_Index_Program_*.sas files by
  # data-raw/checks.R rather than taken on trust.
  if (release %in% names(CMR_MORTALITY_OVERRIDES)) {
    ov <- CMR_MORTALITY_OVERRIDES[[release]]
    mw[names(ov)] <- as.list(ov)
  }


  # Calculate indices for every record at once. Categories the frame does not carry
  # are dropped here rather than tested per row, which is what keeps a partially
  # flagged frame scoring instead of erroring.
  present <- intersect(names(rw), names(result_data))
  n <- nrow(result_data)

  readmit <- numeric(n)
  mort <- numeric(n)

  # One vectorised pass per category rather than a cell-by-cell loop over rows. Two
  # n-length accumulators is the whole footprint: building a matrix over the 38 flag
  # columns instead would allocate a second copy of the flags, which matters on the
  # large frames this is fast enough to be used on.
  for (cmr_var in present) {
    x <- as.numeric(result_data[[cmr_var]])
    x[is.na(x)] <- 0  # a missing flag contributes nothing to either score
    readmit <- readmit + (x * rw[[cmr_var]])
    mort <- mort + (x * mw[[cmr_var]])
  }

  result_data$CMR_Index_Readmission <- readmit
  result_data$CMR_Index_Mortality <- mort

  return(result_data)
}

#' Print summary statistics for CMR indices
#'
#' Internal function that displays descriptive statistics for the calculated CMR 
#' readmission and mortality indices. Users can access summary statistics through 
#' other means in the package workflow.
#'
#' @param data Data frame containing CMR indices (output from cmr_index function)
#' @return None (prints to console)
#' @keywords internal
print_cmr_summary <- function(data) {
  cat("\n=== CMR Index Summary Statistics ===\n")
  
  indices <- c("CMR_Index_Readmission", "CMR_Index_Mortality")
  
  for (idx in indices) {
    if (idx %in% names(data)) {
      values <- data[[idx]]
      cat(sprintf("\n%s:\n", idx))
      cat(sprintf("  N: %d\n", sum(!is.na(values))))
      cat(sprintf("  Missing: %d\n", sum(is.na(values))))
      cat(sprintf("  Mean: %.2f\n", mean(values, na.rm = TRUE)))
      cat(sprintf("  SD: %.2f\n", sd(values, na.rm = TRUE)))
      cat(sprintf("  Min: %.0f\n", min(values, na.rm = TRUE)))
      cat(sprintf("  Max: %.0f\n", max(values, na.rm = TRUE)))
      
      # Additional percentile information
      percentiles <- stats::quantile(values, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
      cat(sprintf("  25th percentile: %.1f\n", percentiles[[1]]))
      cat(sprintf("  50th percentile (median): %.1f\n", percentiles[[2]]))
      cat(sprintf("  75th percentile: %.1f\n", percentiles[[3]]))
    } else {
      cat(sprintf("\nWARNING: %s not found in data\n", idx))
    }
  }
  cat("\n")
}