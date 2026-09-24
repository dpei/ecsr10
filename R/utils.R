# The AHRQ CMR releases this package can score with, oldest first. The LAST
# element of each vector is that family's default, so a newly published release
# is APPENDED and an older one PREPENDED - see data-raw/README.md.
#
# Two families, because AHRQ shipped two different pieces of software:
#
#   refined  v2021.1 onward. 38 measures, 18 of them gated on POA. v2021.1 is
#            the first Refined release; v2022.1 renamed two of its measures
#            (ARTH -> AUTOIMMUNE, CHF -> HF) and added the comorbidity indices.
#   beta     v2016.2 - v2020.1. 30 measures plus a derived HTN_C, no POA at all,
#            and an MS-DRG screen that suppresses comorbidities related to the
#            principal diagnosis. Superseded, not merely older.
CMR_RELEASES      <- c("2021.1", "2022.1", "2023.1", "2024.1", "2025.1", "2026.1")
CMR_BETA_RELEASES <- c("2016.2", "2017.2", "2018.1", "2019.2", "2020.1")

# Releases AHRQ ships no index program for. The Elixhauser Comorbidity Indices
# Refined for ICD-10-CM "are not available until v2022.1" (AHRQ), and the beta
# software emits flags only. cmr_index() refuses these rather than quietly
# scoring them with some other release's weights.
CMR_NO_INDEX_RELEASES <- c("2021.1", CMR_BETA_RELEASES)

CMR_VARIANTS <- c("refined", "beta")

#' Internal. Releases belonging to one variant. `variant = "all"` concatenates.
#' @keywords internal
.releases_for_variant <- function(variant) {
  switch(variant,
         refined = CMR_RELEASES,
         beta    = CMR_BETA_RELEASES,
         all     = c(CMR_BETA_RELEASES, CMR_RELEASES),
         stop("`variant` must be one of: refined, beta, all", call. = FALSE))
}

#' AHRQ comorbidity software releases supported by this package
#'
#' The AHRQ/HCUP releases \code{\link{comorbidity}} can score with, oldest first.
#' Any one of these is valid as its \code{release} argument, given the matching
#' \code{variant}.
#'
#' @param variant Which family to list: \code{"refined"} (the default; the
#'   Elixhauser Comorbidity Software \emph{Refined} for ICD-10-CM, v2021.1
#'   onward), \code{"beta"} (the superseded beta software, v2016.2-v2020.1), or
#'   \code{"all"} for both, oldest first.
#' @return Character vector of release labels, e.g. \code{c("2021.1", ...)}.
#' @seealso \code{\link{cmr_version}} for the default of each family.
#' @examples
#' cmr_releases()
#' cmr_releases("beta")
#' @export
cmr_releases <- function(variant = c("refined", "beta", "all")) {
  .releases_for_variant(match.arg(variant))
}

#' Default AHRQ release
#'
#' Returns the release \code{\link{comorbidity}} and \code{\link{cmr_index}} use
#' when their \code{release} argument is not given - the newest one this package
#' ships tables for, within the requested family. Pass \code{release =}
#' explicitly to score under an older one; \code{\link{cmr_releases}} lists the
#' choices.
#'
#' This is the analogue of the \code{CMR_VERSION} macro variable the SAS program
#' stamps onto every output row. It is exposed as a function rather than an output
#' column, so \code{comorbidity()}'s result schema is unaffected. The release a
#' particular result was scored under is recorded on it as the \code{cmr_release}
#' attribute, and the family as \code{cmr_variant}.
#'
#' @param variant \code{"refined"} (default) or \code{"beta"}.
#' @return Character scalar, e.g. \code{"2026.1"}.
#' @seealso \code{\link{cmr_releases}}
#' @examples
#' cmr_version()
#' cmr_version("beta")
#' @export
cmr_version <- function(variant = c("refined", "beta")) {
  rel <- .releases_for_variant(match.arg(variant))
  rel[[length(rel)]]
}

#' Validate a variant argument
#'
#' Internal. Unlike \code{match.arg()}, this accepts an already-resolved scalar,
#' so callers can pass either the default vector or a user's single value.
#' @keywords internal
.resolve_variant <- function(variant) {
  if (identical(variant, CMR_VARIANTS)) return("refined")
  if (!is.character(variant) || length(variant) != 1L || is.na(variant) ||
      !variant %in% CMR_VARIANTS) {
    stop("`variant` must be one of: ", paste(CMR_VARIANTS, collapse = ", "),
         call. = FALSE)
  }
  variant
}

#' Validate an AHRQ release argument
#'
#' Internal. Returns the release unchanged, or errors naming the valid set. A
#' typo'd release must never fall through to the default, because scoring under
#' the wrong release fails silently - unmapped codes simply do not flag.
#'
#' Release labels do not overlap between the two families, so a release given
#' under the wrong variant is caught here; the error says which family the label
#' does belong to rather than just listing the valid set, because "2020.1 is not
#' a release" is a confusing thing to be told about a release that exists.
#' @keywords internal
.resolve_release <- function(release, variant = "refined") {
  valid <- .releases_for_variant(variant)
  if (!is.character(release) || length(release) != 1L || is.na(release)) {
    stop("`release` must be a single non-NA character string; one of: ",
         paste(valid, collapse = ", "), call. = FALSE)
  }
  if (!release %in% valid) {
    other <- setdiff(CMR_VARIANTS, variant)
    hint <- if (release %in% .releases_for_variant(other)) {
      paste0("\n  \"", release, "\" is a ", other,
             " release; pass variant = \"", other, "\" to use it.")
    } else ""
    stop("unsupported AHRQ ", variant, " release \"", release,
         "\"; must be one of: ", paste(valid, collapse = ", "), hint,
         call. = FALSE)
  }
  release
}

#' Newest ICD-10-CM version an AHRQ release covers
#'
#' Internal. A release labelled v\emph{Y}.1 covers codes through September of
#' fiscal year \emph{Y}, and ICD-10-CM version numbering makes that \emph{Y} minus
#' 1983 (v33 = FY2016 ... v43 = FY2026). Verified against every supported
#' release's SAS mapping program: each one's \code{ICDVER} ladder is truncated
#' here and its final \code{ELSE} assigns exactly this value.
#'
#' Refined releases only. The beta software has no POA-exempt lists and never
#' resolves an ICD-10-CM version, so the beta pipeline must not reach this.
#' @keywords internal
.release_max_icd_version <- function(release) {
  as.integer(substr(release, 1L, 4L)) - 1983L
}

#' Normalize ICD-10-CM codes
#'
#' Removes dots, trims spaces, and converts to uppercase for consistent ICD-10-CM code formatting.
#'
#' @param x Character vector of ICD-10-CM codes to normalize
#' @return Character vector of normalized ICD-10-CM codes
#' @examples
#' normalize_icd10(c("E11.9", "I10", "  z51.11  "))
#' @export
normalize_icd10 <- function(x) {
  x %>% toupper() %>% stringr::str_replace_all("\\.", "") %>% stringr::str_squish()
}

#' Match codes to patterns using wildcard or regex matching
#'
#' Given a vector of codes and a data frame of patterns, returns first matching row per code.
#' Supports SQL-style wildcard patterns (%) or regular expressions.
#'
#' @param codes Character vector of codes to match
#' @param patterns_df Data frame with columns "pattern" and "target"
#' @param mode Character, either "wildcard" (default) or "regex"
#' @return Character vector of matching targets, NA for unmatched codes
#' @examples
#' \dontrun{
#' patterns <- data.frame(
#'   pattern = c("E11%", "I10"),
#'   target = c("diabetes", "hypertension")
#' )
#' match_codes_to_patterns(c("E119", "I10", "Z511"), patterns)
#' }
#' @keywords internal
match_codes_to_patterns <- function(codes, patterns_df, mode = c("wildcard", "regex")) {
  mode <- match.arg(mode)
  out <- rep(NA_character_, length(codes))
  if (nrow(patterns_df) == 0) return(out)
  
  if (mode == "wildcard") {
    # Translate SQL-like % wildcard to regex
    patterns_df <- patterns_df %>%
      dplyr::mutate(.regex = paste0("^", stringr::str_replace_all(pattern, "%", ".*"), "$"))
  } else {
    patterns_df <- patterns_df %>% dplyr::mutate(.regex = pattern)
  }
  
  # Pre-compile regex patterns
  rex <- stringr::regex(patterns_df$.regex, ignore_case = TRUE)
  
  # For each pattern in order, fill matches that are NA
  for (i in seq_len(nrow(patterns_df))) {
    hits <- stringr::str_detect(codes, rex[i])
    out[is.na(out) & hits] <- patterns_df$target[i]
  }
  out
}

#' Convert pattern matches to wide logical flags
#'
#' Expands long table of pattern matches into wide logical flags per target.
#'
#' @param codes Character vector of codes from a single patient/encounter
#' @param patterns_df Data frame with "target" and "pattern" columns
#' @param targets Character vector of all possible target names
#' @param mode Character, pattern matching mode ("wildcard" or "regex")
#' @return Tibble with logical flags for each target
#' @keywords internal
flags_from_patterns <- function(codes, patterns_df, targets, mode = "wildcard") {
  present <- dplyr::tibble(
    target = match_codes_to_patterns(codes, patterns_df %>% dplyr::select(pattern, target), mode = mode) %>%
      purrr::discard(is.na) %>% unique()
  )
  
  dplyr::tibble(target = targets) %>% 
    dplyr::mutate(flag = target %in% present$target) %>% 
    tidyr::pivot_wider(names_from = target, values_from = flag)
}