# The AHRQ CMR releases this package can score with, oldest first. The last
# element is the default. Adding next year's release means appending here and
# regenerating comfmt_releases - see data-raw/README.md.
#
# v2021.1 is deliberately absent: it is structurally different software (two SAS
# programs rather than three, measures named ARTH/CHF rather than AUTOIMMUNE/HF,
# six combination targets, no CMR_ output prefix, and no index program at all),
# and no independent reference exists to validate a translation of it against.
CMR_RELEASES <- c("2022.1", "2023.1", "2024.1", "2025.1", "2026.1")

#' AHRQ CMR releases supported by this package
#'
#' The AHRQ/HCUP "Elixhauser Comorbidity Software Refined for ICD-10-CM" releases
#' that \code{\link{comorbidity}} and \code{\link{cmr_index}} can score with,
#' oldest first. Any one of these is valid as their \code{release} argument.
#'
#' @return Character vector of release labels, e.g. \code{c("2022.1", ...)}.
#' @seealso \code{\link{cmr_version}} for the default.
#' @examples
#' cmr_releases()
#' @export
cmr_releases <- function() {
  CMR_RELEASES
}

#' Default AHRQ CMR release
#'
#' Returns the release \code{\link{comorbidity}} and \code{\link{cmr_index}} use
#' when their \code{release} argument is not given - the newest one this package
#' ships tables for. Pass \code{release =} explicitly to score under an older one;
#' \code{\link{cmr_releases}} lists the choices.
#'
#' This is the analogue of the \code{CMR_VERSION} macro variable the SAS program
#' stamps onto every output row. It is exposed as a function rather than an output
#' column, so \code{comorbidity()}'s result schema is unaffected. The release a
#' particular result was scored under is recorded on it as the \code{cmr_release}
#' attribute.
#'
#' @return Character scalar, e.g. \code{"2026.1"}.
#' @seealso \code{\link{cmr_releases}}
#' @examples
#' cmr_version()
#' @export
cmr_version <- function() {
  CMR_RELEASES[[length(CMR_RELEASES)]]
}

#' Validate an AHRQ release argument
#'
#' Internal. Returns the release unchanged, or errors naming the valid set. A
#' typo'd release must never fall through to the default, because scoring under
#' the wrong release fails silently - unmapped codes simply do not flag.
#' @keywords internal
.resolve_release <- function(release) {
  if (!is.character(release) || length(release) != 1L || is.na(release)) {
    stop("`release` must be a single non-NA character string; one of: ",
         paste(CMR_RELEASES, collapse = ", "), call. = FALSE)
  }
  if (!release %in% CMR_RELEASES) {
    stop("unsupported AHRQ release \"", release, "\"; must be one of: ",
         paste(CMR_RELEASES, collapse = ", "), call. = FALSE)
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