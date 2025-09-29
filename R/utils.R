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