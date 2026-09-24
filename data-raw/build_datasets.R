#!/usr/bin/env Rscript
# build_datasets.R
# Regenerate the package's lookup datasets from the AHRQ SAS format programs.
#
#   cd ecsr10 && Rscript data-raw/build_datasets.R
#
# Writes:
#   data/comfmt_releases.rda   code -> comorbidity for every supported release
#   data/comfmt_lookup.rda     the v2026.1 view, kept for backwards compatibility
#   data/beta_comfmt.rda       code -> measure for the five BETA versions
#   data/beta_drg_screens.rda  the beta MS-DRG exclusion screens, as integer ranges
#
# Does NOT write data/poaxmpt_codes_long.rda. The POA-exempt lists are keyed by
# ICD-10-CM version, not by AHRQ release, and every release ships byte-identical
# blocks for the versions it shares with its predecessor - so that dataset needs
# no release dimension and is left exactly as it is. `checks.R` verifies the
# invariance claim rather than assuming it.
#
# Run `Rscript data-raw/checks.R` afterwards; it asserts everything this script
# is supposed to have produced.

# Base R only: this script must run without the package installed or loaded.
here <- function(...) {
  a <- commandArgs(FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  file.path(if (length(f)) dirname(normalizePath(f[[1]])) else ".", ...)
}

source(here("parse_ahrq_sas.R"))

DEFAULT_RELEASE <- "2026.1"

SAS_DIR  <- here("..", "..", "SAS_software")
DATA_DIR <- here("..", "data")

message("Parsing ", length(AHRQ_RELEASES), " refined releases and ",
        length(AHRQ_BETA_RELEASES), " beta versions from ", normalizePath(SAS_DIR))

parsed <- lapply(AHRQ_RELEASES, function(r) {
  f  <- format_program_path(r, SAS_DIR)
  cf <- parse_comfmt(f)
  message(sprintf("  %-8s %-38s %5d codes, %2d targets, max ICD v%d",
                  r, basename(f), nrow(cf), length(unique(cf$comorbidity)),
                  release_max_icd_version(r)))
  cf
})
names(parsed) <- AHRQ_RELEASES

# ---- comfmt_releases ----------------------------------------------------------
# Long form: one row per (release, code). Deliberately NOT the wide
# one-indicator-column-per-release shape - that cannot represent a code whose
# comorbidity changed between releases. AHRQ has never done that, but the long
# form costs ~6 KB after xz and does not bake the assumption in.
#
# Each release slice is sorted by code, matching the convention comfmt_lookup
# already uses. Order is preserved on purpose: overlaps in the matcher are
# resolved by lowest `priority`, which is the row index in the table.
comfmt_releases <- do.call(rbind, lapply(AHRQ_RELEASES, function(r) {
  data.frame(release     = r,
             code        = parsed[[r]]$code,
             comorbidity = parsed[[r]]$comorbidity,
             stringsAsFactors = FALSE)
}))
rownames(comfmt_releases) <- NULL

# ---- beta_comfmt --------------------------------------------------------------
# Same long shape as comfmt_releases, from the beta family's $RCOMFMT block.
#
# Rows whose target is the NONE catch-all are dropped. That is a no-op, not a
# simplification: the beta analysis program compares DXVALUE against its 30
# measure names and then against its 10 hypertension labels, so a DXVALUE of
# "NONE" falls through both exactly the way an unmapped code does. Dropping them
# matters because v2016.2 does not rely on `other = " "` - it enumerates all
# 66,666 non-comorbidity codes explicitly (in two spellings, "None" and "NONE"),
# which would make the shipped table 20x larger for no behavioural difference.
# Safe only because no beta code carries a `%` wildcard, so matching is exact and
# a dropped row cannot expose a code to some other pattern; checks.R pins both
# that and the fact that NONE is the only target ever dropped.
beta_parsed <- lapply(AHRQ_BETA_RELEASES, function(r) {
  f  <- format_program_path(r, SAS_DIR, "beta")
  cf <- parse_comfmt(f, AHRQ_LAYOUT$beta$block)
  cf <- cf[!grepl("^none$", cf$comorbidity, ignore.case = TRUE), , drop = FALSE]
  rownames(cf) <- NULL
  dg <- parse_drg_screens(f)
  message(sprintf("  %-8s %-38s %5d codes, %2d targets, %2d DRG screens / %3d ranges",
                  r, basename(f), nrow(cf), length(unique(cf$comorbidity)),
                  length(unique(dg$screen)), nrow(dg)))
  list(comfmt = cf, drg = dg)
})
names(beta_parsed) <- AHRQ_BETA_RELEASES

beta_comfmt <- do.call(rbind, lapply(AHRQ_BETA_RELEASES, function(r) {
  data.frame(release     = r,
             code        = beta_parsed[[r]]$comfmt$code,
             comorbidity = beta_parsed[[r]]$comfmt$comorbidity,
             stringsAsFactors = FALSE)
}))
rownames(beta_comfmt) <- NULL

# ---- beta_drg_screens ---------------------------------------------------------
# One row per (version, screen, contiguous MS-DRG range). Ranges rather than
# expanded members: the source is written that way, the widest screen spans 100+
# DRGs, and membership is an interval test either way.
beta_drg_screens <- do.call(rbind, lapply(AHRQ_BETA_RELEASES, function(r) {
  d <- beta_parsed[[r]]$drg
  data.frame(release  = r,
             screen   = d$screen,
             drg_low  = as.integer(d$drg_low),
             drg_high = as.integer(d$drg_high),
             stringsAsFactors = FALSE)
}))
rownames(beta_drg_screens) <- NULL

# ---- comfmt_lookup ------------------------------------------------------------
# The default-release view, regenerated from the same parse so the two objects
# cannot drift. checks.R pins that it equals the corresponding slice.
comfmt_lookup <- parsed[[DEFAULT_RELEASE]]

message("\ncomfmt_releases:  ", nrow(comfmt_releases), " rows over ",
        length(AHRQ_RELEASES), " releases")
message("comfmt_lookup:    ", nrow(comfmt_lookup), " rows (", DEFAULT_RELEASE, ")")
message("beta_comfmt:      ", nrow(beta_comfmt), " rows over ",
        length(AHRQ_BETA_RELEASES), " beta versions")
message("beta_drg_screens: ", nrow(beta_drg_screens), " ranges")

save(comfmt_releases, file = file.path(DATA_DIR, "comfmt_releases.rda"),
     compress = "xz", version = 3)
save(comfmt_lookup, file = file.path(DATA_DIR, "comfmt_lookup.rda"),
     compress = "gzip", version = 3)
save(beta_comfmt, file = file.path(DATA_DIR, "beta_comfmt.rda"),
     compress = "xz", version = 3)
save(beta_drg_screens, file = file.path(DATA_DIR, "beta_drg_screens.rda"),
     compress = "gzip", version = 3)

message("\nWrote:")
for (f in c("comfmt_releases.rda", "comfmt_lookup.rda",
            "beta_comfmt.rda", "beta_drg_screens.rda")) {
  message(sprintf("  data/%-22s %6.1f KB", f,
                  file.size(file.path(DATA_DIR, f)) / 1024))
}
message("\nNext: Rscript data-raw/checks.R")
