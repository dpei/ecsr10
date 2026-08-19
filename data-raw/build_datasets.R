#!/usr/bin/env Rscript
# build_datasets.R
# Regenerate the package's lookup datasets from the AHRQ SAS format programs.
#
#   cd ecsr10 && Rscript data-raw/build_datasets.R
#
# Writes:
#   data/comfmt_releases.rda   code -> comorbidity for every supported release
#   data/comfmt_lookup.rda     the v2026.1 view, kept for backwards compatibility
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

message("Parsing ", length(AHRQ_RELEASES), " AHRQ releases from ", normalizePath(SAS_DIR))

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

# ---- comfmt_lookup ------------------------------------------------------------
# The default-release view, regenerated from the same parse so the two objects
# cannot drift. checks.R pins that it equals the corresponding slice.
comfmt_lookup <- parsed[[DEFAULT_RELEASE]]

message("\ncomfmt_releases: ", nrow(comfmt_releases), " rows over ",
        length(AHRQ_RELEASES), " releases")
message("comfmt_lookup:   ", nrow(comfmt_lookup), " rows (", DEFAULT_RELEASE, ")")

save(comfmt_releases, file = file.path(DATA_DIR, "comfmt_releases.rda"),
     compress = "xz", version = 3)
save(comfmt_lookup, file = file.path(DATA_DIR, "comfmt_lookup.rda"),
     compress = "gzip", version = 3)

message("\nWrote:")
for (f in c("comfmt_releases.rda", "comfmt_lookup.rda")) {
  message(sprintf("  data/%-22s %6.1f KB", f,
                  file.size(file.path(DATA_DIR, f)) / 1024))
}
message("\nNext: Rscript data-raw/checks.R")
