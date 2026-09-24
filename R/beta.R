# beta.R
# The AHRQ beta Elixhauser Comorbidity Software for ICD-10-CM, v2016.2-v2020.1.
#
# A transcription of `comoanaly_icd10cm_<ver>.txt`, which is byte-identical
# across all five beta versions apart from its banner and the rename of its input
# variables from DX1/NDX to I10_DX1/I10_NDX at v2019.2. One engine, five format
# tables - so nothing below is version-aware except the two lookups it is handed.
#
# It shares almost nothing with the refined pipeline in comorbidity.R:
#
#   * 30 measures plus a derived HTN_C, not 38 flags
#   * no POA anywhere - the beta software predates POA-based identification
#   * an MS-DRG screen instead: 24 numeric formats that suppress a comorbidity
#     when the principal diagnosis's DRG is directly related to it
#   * no comorbidity indices
#
# What it does share is the code->target matcher, which is where the real
# matching logic lives: normalize_icd10() and the same compiled-regex pass over
# the UNIQUE code vector, resolving overlaps by lowest table row index.
#
# STATEMENT ORDER IS THE SPECIFICATION. The SAS DATA step runs, in this order:
#
#   1. initialise the 30 measures and the 10 hypertension flags to 0
#   2. read secondary diagnoses, setting measures and hypertension flags
#   3. hypertension fan-out   - flags set HTNCX, and sometimes CHF / RENLFAIL
#   4. severity hierarchy     - HTNCX beats HTN, METS beats TUMOR, DMCX beats DM
#   5. resolve the 24 MS-DRG screen flags from DRG
#   6. MS-DRG suppression     - including per-hypertension-flag conditionals
#   7. HTN_C = HTN | HTNCX
#
# Steps 4 and 6 are both "hierarchy", and swapping them changes answers: the
# hierarchy runs BEFORE the DRG screen, so a HTN suppressed at step 4 stays 0
# even where step 6 would have suppressed HTNCX. HTN_C is computed LAST, after
# suppression, so a screened-away HTNCX does not feed it.

#' Run the beta comorbidity pipeline over one block of encounters
#'
#' Internal. Returns only the 31-column CMRB flag matrix, one row per row of
#' \code{patient_data}. Like \code{.comorbidity_flags()}, every stage is
#' per-encounter, so a contiguous block yields exactly the rows the whole-dataset
#' call would have, which is what makes \code{ncores > 1} identical to serial.
#'
#' @param patient_data Data frame of encounters
#' @param dx_cols Secondary diagnosis columns
#' @param drg Integer vector of MS-DRGs, one per row, or NULL for no screen
#' @param comfmt Compiled \code{(target, pattern)} lookup for the version
#' @param drg_screens Named list of \code{(low, high)} matrices, or NULL
#' @param wildcard_mode "wildcard" or "regex"
#' @keywords internal
.beta_comorbidity_flags <- function(patient_data, dx_cols, drg, comfmt,
                                    drg_screens, wildcard_mode = "wildcard") {

  n_rows <- nrow(patient_data)

  if (wildcard_mode == "wildcard") {
    comfmt_compiled <- comfmt %>%
      dplyr::mutate(.regex = paste0("^", stringr::str_replace_all(pattern, "%", ".*"), "$"))
  } else {
    comfmt_compiled <- comfmt %>% dplyr::mutate(.regex = pattern)
  }
  regex_list <- purrr::map(comfmt_compiled$.regex,
                           ~ stringr::regex(.x, ignore_case = TRUE))

  # ---- 1/2. read the secondary diagnoses ------------------------------------
  dx_long <- patient_data %>%
    dplyr::mutate(row_id = dplyr::row_number()) %>%
    dplyr::select("row_id", dplyr::all_of(dx_cols)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(dx_cols),
      names_to = "dx_position",
      values_to = "dx_code",
      values_drop_na = TRUE
    ) %>%
    dplyr::filter(.data$dx_code != "") %>%
    dplyr::mutate(dx_code = normalize_icd10(as.character(.data$dx_code)))

  unique_codes <- unique(dx_long$dx_code)

  # `priority` is the pattern's row index in the lookup table, a property of the
  # table rather than of the data - so every chunk resolves an overlap the same
  # way and a chunk's map is an exact restriction of the whole-dataset map.
  code_target_map <- purrr::map_dfr(seq_len(nrow(comfmt_compiled)), function(i) {
    hits <- unique_codes[stringr::str_detect(unique_codes, regex_list[[i]])]
    if (!length(hits)) return(NULL)
    dplyr::tibble(dx_code = hits, target = comfmt_compiled$target[i], priority = i)
  })
  if (nrow(code_target_map) == 0L) {
    code_target_map <- dplyr::tibble(dx_code = character(), target = character(),
                                     priority = integer())
  }
  code_target_map <- code_target_map %>%
    dplyr::arrange(.data$dx_code, .data$priority) %>%
    dplyr::group_by(.data$dx_code) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"priority")

  hits <- dplyr::inner_join(dx_long, code_target_map, by = "dx_code")

  # One matrix over the 30 measures and the 10 hypertension flags together. The
  # SAS keeps them as separate variables, but they are set by the same loop over
  # the same DXVALUE, and holding them in one matrix means the assignment is a
  # single scatter rather than two passes.
  cols <- c(BETA_MEASURES, BETA_HTN_PSEUDO)
  m <- matrix(0L, nrow = n_rows, ncol = length(cols),
              dimnames = list(NULL, cols))
  if (nrow(hits)) {
    j <- match(hits$target, cols)
    keep <- !is.na(j)
    if (any(keep)) {
      m[cbind(hits$row_id[keep], j[keep])] <- 1L
    }
  }

  # Columns are addressed by name throughout what follows, so the code reads
  # against the SAS line for line. `g`/`s` keep that from becoming noise.
  g <- function(nm) m[, nm]
  s <- function(nm, v) m[, nm] <<- as.integer(v)

  # ---- 3. hypertension fan-out ----------------------------------------------
  # Each detailed label implies complicated hypertension, and the labels naming a
  # component condition imply that too. These are the SAS's `IF <flag>_ THEN DO`
  # blocks, in file order.
  htncx <- g("HTNCX") | g("HTNPREG") | g("HTNWOCHF") | g("HTNWCHF") |
           g("HRENWORF") | g("HRENWRF") | g("HHRWOHRF") | g("HHRWCHF") |
           g("HHRWRF") | g("HHRWHRF") | g("OHTNPREG")
  s("HTNCX", htncx)
  s("CHF",      g("CHF")      | g("HTNWCHF") | g("HHRWCHF") | g("HHRWHRF"))
  s("RENLFAIL", g("RENLFAIL") | g("HRENWRF") | g("HHRWRF")  | g("HHRWHRF"))

  # ---- 4. severity hierarchy ------------------------------------------------
  s("HTN",   g("HTN")   & !g("HTNCX"))
  s("TUMOR", g("TUMOR") & !g("METS"))
  s("DM",    g("DM")    & !g("DMCX"))

  # ---- 5. MS-DRG screen flags -----------------------------------------------
  # `drg = NULL` means every flag is FALSE, which is exactly what the SAS does
  # when DRG is missing: PUT(., <screen>.) cannot return 'YES', so no FLG is set
  # and no suppression fires.
  flg <- if (is.null(drg_screens)) {
    stats::setNames(rep(list(rep(FALSE, n_rows)), length(BETA_DRG_SCREEN_NAMES)),
                    BETA_DRG_SCREEN_NAMES)
  } else {
    d <- suppressWarnings(as.integer(drg))
    lapply(drg_screens[BETA_DRG_SCREEN_NAMES], function(rg) {
      hit <- rep(FALSE, n_rows)
      for (k in seq_len(nrow(rg))) {
        hit <- hit | (!is.na(d) & d >= rg[k, "low"] & d <= rg[k, "high"])
      }
      hit
    })
  }
  f <- function(nm) flg[[paste0(nm, "DRG")]]

  # ---- 6. MS-DRG suppression -------------------------------------------------
  # Sequential, exactly as written: CHF is zeroed here and the hypertension
  # blocks below may zero it again, which is a no-op but keeps the transcription
  # honest rather than "optimised" into a different program.
  s("CHF",      g("CHF")      & !f("CARD"))
  s("VALVE",    g("VALVE")    & !f("CARD"))
  s("PULMCIRC", g("PULMCIRC") & !(f("CARD") | f("PULM")))
  s("PERIVASC", g("PERIVASC") & !f("PERI"))
  s("HTN",      g("HTN")      & !f("HTN"))

  # The detailed hypertension flags decide WHICH screens can suppress HTNCX, and
  # whether CHF/RENLFAIL go with it. Transcribed one SAS statement per line; the
  # asymmetries are real (HHRWCHF_ lets RENALFLG zero HTNCX but not RENLFAIL;
  # HHRWRF_ groups HTNCXFLG with CARDFLG rather than testing them separately).
  # force(cond) before the read-modify-write: `s()` mutates `m` through `<<-`,
  # and leaving `cond` a promise would make the answer depend on R's argument
  # evaluation order inside an expression that also reads `m`.
  z <- function(cond) { force(cond); s("HTNCX", g("HTNCX") & !cond) }
  z(g("HTNCX")    & f("HTNCX"))
  z(g("HTNPREG")  & f("HTNCX"))
  z(g("HTNWOCHF") & (f("HTNCX") | f("CARD")))
  z(g("HTNWCHF")  & (f("HTNCX") | f("CARD")))
  s("CHF", g("CHF") & !(g("HTNWCHF") & f("CARD")))
  z(g("HRENWORF") & (f("HTNCX") | f("RENAL")))
  z(g("HRENWRF")  & (f("HTNCX") | f("RENAL")))
  s("RENLFAIL", g("RENLFAIL") & !(g("HRENWRF") & f("RENAL")))
  z(g("HHRWOHRF") & (f("HTNCX") | f("CARD") | f("RENAL")))
  z(g("HHRWCHF")  & (f("HTNCX") | f("CARD") | f("RENAL")))
  s("CHF", g("CHF") & !(g("HHRWCHF") & f("CARD")))
  z(g("HHRWRF")   & (f("HTNCX") | f("CARD") | f("RENAL")))
  s("RENLFAIL", g("RENLFAIL") & !(g("HHRWRF") & f("RENAL")))
  z(g("HHRWHRF")  & (f("HTNCX") | f("CARD") | f("RENAL")))
  s("CHF",      g("CHF")      & !(g("HHRWHRF") & f("CARD")))
  s("RENLFAIL", g("RENLFAIL") & !(g("HHRWHRF") & f("RENAL")))
  z(g("OHTNPREG") & (f("HTNCX") | f("CARD") | f("RENAL")))

  s("NEURO",    g("NEURO")    & !f("NERV"))
  s("CHRNLUNG", g("CHRNLUNG") & !f("PULM"))
  s("DM",       g("DM")       & !f("DIAB"))
  s("DMCX",     g("DMCX")     & !f("DIAB"))
  s("HYPOTHY",  g("HYPOTHY")  & !f("HYPO"))
  s("RENLFAIL", g("RENLFAIL") & !f("RENF"))
  s("LIVER",    g("LIVER")    & !f("LIVER"))
  s("ULCER",    g("ULCER")    & !f("ULCE"))
  s("AIDS",     g("AIDS")     & !f("HIV"))
  s("LYMPH",    g("LYMPH")    & !f("LEUK"))
  s("METS",     g("METS")     & !f("CANC"))
  s("TUMOR",    g("TUMOR")    & !f("CANC"))
  s("ARTH",     g("ARTH")     & !f("ARTH"))
  s("COAG",     g("COAG")     & !f("COAG"))
  s("OBESE",    g("OBESE")    & !(f("NUTR") | f("OBESE")))
  s("WGHTLOSS", g("WGHTLOSS") & !f("NUTR"))
  s("LYTES",    g("LYTES")    & !f("NUTR"))
  s("BLDLOSS",  g("BLDLOSS")  & !f("ANEM"))
  s("ANEMDEF",  g("ANEMDEF")  & !f("ANEM"))
  s("ALCOHOL",  g("ALCOHOL")  & !f("ALC"))
  s("DRUG",     g("DRUG")     & !f("ALC"))
  s("PSYCH",    g("PSYCH")    & !f("PSY"))
  s("DEPRESS",  g("DEPRESS")  & !f("DEPRS"))
  s("PARA",     g("PARA")     & !f("CERE"))

  # ---- 7. HTN_C, last ---------------------------------------------------------
  out <- matrix(0L, nrow = n_rows, ncol = length(BETA_FINAL_TARGETS),
                dimnames = list(NULL, BETA_FINAL_TARGETS))
  out[, paste0("CMRB_", BETA_MEASURES)] <- m[, BETA_MEASURES, drop = FALSE]
  out[, "CMRB_HTN_C"] <- as.integer(g("HTN") | g("HTNCX"))
  out
}
