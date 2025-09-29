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
  df <- readr::read_csv(format_csv_path, show_col_types = FALSE) %>%
    janitor::clean_names()
  
  # Accept either: (code, comorbidity) or (target, pattern)
  if (all(c("code", "comorbidity") %in% names(df))) {
    df <- df %>% dplyr::transmute(target = .data$comorbidity, pattern = .data$code)
  } else if (all(c("target", "pattern") %in% names(df))) {
    df <- df %>% dplyr::select(target, pattern)
  } else {
    stop("Lookup CSV must have columns (code, comorbidity) or (target, pattern).")
  }
  
  # Optional version filtering if version_min/version_max exist
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
  df <- readr::read_csv(poaxmpt_csv_path, show_col_types = FALSE) %>%
    janitor::clean_names()
  
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
#' @param year Integer vector of years
#' @param quarter Integer vector of quarters (1-4)
#' @return Integer vector of ICD-10-CM version numbers
#' @keywords internal
determine_icd_version <- function(year, quarter) {
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
  
  # Default to version 42 for any other cases
  version[version == 0] <- 42
  
  return(version)
}