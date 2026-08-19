#' Build comorbidity format from CSV file
#'
#' Internal function that creates a comorbidity lookup table from a CSV file with 
#' flexible schema support. Accepts either (code, comorbidity) or (target, pattern) 
#' column formats. This function is used internally by the comorbidity() function.
#'
#' @param format_csv_path Character, path to CSV file containing comorbidity mappings
#' @param version Integer, ICD-10-CM version for optional version filtering
#' @param mode Character, pattern matching mode ("wildcard" or "regex")
#' @return Data frame with "target" and "pattern" columns
#' @keywords internal
build_comfmt_from_csv <- function(format_csv_path, version = NULL, mode = "wildcard") {
  .comfmt_from_df(readr::read_csv(format_csv_path, show_col_types = FALSE),
                  version = version)
}

#' Normalize a comorbidity lookup table
#'
#' Internal. The schema-handling half of \code{build_comfmt_from_csv()}, split out
#' so the built-in datasets can be used directly instead of being round-tripped
#' through a temporary CSV on every call.
#'
#' @param df Data frame with either (code, comorbidity) or (target, pattern) columns
#' @param version Integer, ICD-10-CM version for optional version filtering
#' @return Data frame with "target" and "pattern" columns
#' @keywords internal
.comfmt_from_df <- function(df, version = NULL) {
  df <- janitor::clean_names(df)

  # Accept either: (code, comorbidity) or (target, pattern)
  if (all(c("code", "comorbidity") %in% names(df))) {
    df <- df %>% dplyr::transmute(target = .data$comorbidity, pattern = .data$code)
  } else if (all(c("target", "pattern") %in% names(df))) {
    df <- df %>% dplyr::select(target, pattern)
  } else {
    stop("Lookup CSV must have columns (code, comorbidity) or (target, pattern).")
  }

  # Optional version filtering if version_min/version_max exist.
  #
  # NOTE: this is an ICD-10-CM VERSION axis (33-43), not an AHRQ RELEASE axis
  # (2022.1-2026.1). The two are orthogonal, and conflating them is a real bug
  # class - one competing implementation ships exactly that defect. Release
  # selection happens in comorbidity(), not here.
  if (!is.null(version) && all(c("version_min", "version_max") %in% names(df))) {
    df <- df %>%
      dplyr::mutate(
        version_min = dplyr::coalesce(as.integer(version_min), -Inf),
        version_max = dplyr::coalesce(as.integer(version_max), Inf)
      ) %>%
      dplyr::filter(version >= version_min, version <= version_max)
  }

  df %>% dplyr::distinct()
}

#' Build POA-exempt code sets from CSV file
#'
#' Internal function that creates a named list of POA (Present on Admission) exempt 
#' codes organized by ICD version. This function is used internally by the 
#' comorbidity() function.
#'
#' @param poaxmpt_csv_path Character, path to CSV file with "version" and "code" columns
#' @return Named list with POA exempt codes by version (e.g., list(v33 = c(...), v34 = c(...)))
#' @keywords internal
build_poa_exempt_formats <- function(poaxmpt_csv_path) {
  .poa_exempt_from_df(readr::read_csv(poaxmpt_csv_path, show_col_types = FALSE))
}

#' Split a POA-exempt table into per-version code sets
#'
#' Internal. The body of \code{build_poa_exempt_formats()}, split out so the
#' built-in dataset can be used directly rather than round-tripped through a
#' temporary CSV. The \code{v<nn>} key format is a contract with
#' \code{.comorbidity_flags()}, which parses the version back out of it.
#'
#' @param df Data frame with "version" and "code" columns
#' @return Named list of code vectors, e.g. \code{list(v33 = c(...), v34 = c(...))}
#' @keywords internal
.poa_exempt_from_df <- function(df) {
  df <- janitor::clean_names(df)

  stopifnot(all(c("version", "code") %in% names(df)))

  df <- df %>%
    dplyr::mutate(
      version = as.integer(version),
      code = normalize_icd10(code)
    )

  split(df$code, paste0("v", df$version))
}

#' Determine ICD version based on year and quarter
#'
#' Internal function that maps year and quarter combinations to ICD-10-CM version
#' numbers following the standard CMS versioning timeline. This function is used
#' internally by the comorbidity() function.
#'
#' Versions roll over at the Q4 fiscal-year boundary, so version 43 covers
#' 2025 Q4 through 2026 Q3. Following the SAS program, anything outside the
#' mapped range - pre-2015 dates and any future year - falls back to the newest
#' version the selected AHRQ release covers.
#'
#' \code{max_version} makes the ladder release-aware. Each AHRQ release ships a
#' ladder truncated at the newest ICD-10-CM version it covers, with its final
#' \code{ELSE} assigning that version; v2022.1 tops out at 39, v2026.1 at 43.
#' Capping the full ladder with \code{pmin} reproduces that truncation exactly,
#' because a truncated ladder assigns the full ladder's value wherever it has a
#' branch and the maximum everywhere else - and everything it lacks a branch for
#' is either pre-2015 (full ladder gives 0) or beyond the release's coverage
#' (full ladder gives something larger than the cap).
#'
#' @param year Integer vector of years
#' @param quarter Integer vector of quarters (1-4)
#' @param max_version Integer, newest ICD-10-CM version the AHRQ release covers.
#'   Defaults to 43, the newest release's cap.
#' @return Integer vector of ICD-10-CM version numbers
#' @keywords internal
determine_icd_version <- function(year, quarter, max_version = 43L) {
  # Initialize version
  version <- rep(0, length(year))
  
  # Apply SAS logic for version determination
  version[year == 2015 & quarter == 4] <- 33
  version[year == 2016 & quarter %in% c(1,2,3)] <- 33
  version[year == 2016 & quarter == 4] <- 34
  version[year == 2017 & quarter %in% c(1,2,3)] <- 34
  version[year == 2017 & quarter == 4] <- 35
  version[year == 2018 & quarter %in% c(1,2,3)] <- 35
  version[year == 2018 & quarter == 4] <- 36
  version[year == 2019 & quarter %in% c(1,2,3)] <- 36
  version[year == 2019 & quarter == 4] <- 37
  version[year == 2020 & quarter %in% c(1,2,3)] <- 37
  version[year == 2020 & quarter == 4] <- 38
  version[year == 2021 & quarter %in% c(1,2,3)] <- 38
  version[year == 2021 & quarter == 4] <- 39
  version[year == 2022 & quarter %in% c(1,2,3)] <- 39
  version[year == 2022 & quarter == 4] <- 40
  version[year == 2023 & quarter %in% c(1,2,3)] <- 40
  version[year == 2023 & quarter == 4] <- 41
  version[year == 2024 & quarter %in% c(1,2,3)] <- 41
  version[year == 2024 & quarter == 4] <- 42
  version[year == 2025 & quarter %in% c(1,2,3)] <- 42
  version[year == 2025 & quarter == 4] <- 43
  version[year == 2026 & quarter %in% c(1,2,3)] <- 43

  # Anything the ladder has no branch for falls back to the release's newest
  # version, then the whole vector is capped there - see @details.
  max_version <- as.integer(max_version)
  version[version == 0] <- max_version
  version <- pmin(version, max_version)

  return(version)
}