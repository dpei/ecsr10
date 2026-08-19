# data-raw/

Regenerates the package's lookup datasets from the AHRQ/HCUP SAS source. Not part of the built
package — `^data-raw$` is in `.Rbuildignore`.

```bash
cd ecsr10
Rscript data-raw/build_datasets.R   # writes data/comfmt_releases.rda + data/comfmt_lookup.rda
Rscript data-raw/checks.R           # asserts everything above; exits 1 on any failure
```

| file | role |
|---|---|
| `parse_ahrq_sas.R` | tokenizer for the SAS `Proc format` value blocks; also `AHRQ_RELEASES` and `release_max_icd_version()` |
| `build_datasets.R` | drives the parser over `../../SAS_software/<release>/` and writes the `.rda`s |
| `checks.R` | assertions over the generated data *and* the AHRQ source it came from |

## Source

`SAS_software/<release>/CMR_Format_Program_v<release>.sas`, one folder per AHRQ release. The
upstream index is <https://hcup-us.ahrq.gov/toolssoftware/comorbidityicd10/comorbidity_icd10.jsp>;
prior releases are at `comorbidity_icd10_archive.jsp`. The zips download from the flat path
`https://hcup-us.ahrq.gov/toolssoftware/comorbidityicd10/`:

| release | zip | released |
|---|---|---|
| 2022.1 | `CMR_v2022-1.zip` | Oct 2021 |
| 2023.1 | `CMR_v2023-1.zip` | Dec 2022 |
| 2024.1 | `CMR_v2024-1.zip` | Mar 2024 |
| 2025.1 | `CMR_v2025-1.zip` | Nov 2024 |
| 2026.1 | `CMR-v2026-1.zip` | Nov 2025 |

Unpack each into `SAS_software/<release>/` — all five zips carry the same six files (three SAS
programs, reference workbook, changelog, user guide).

**v2021.1 is deliberately not supported.** It is structurally different software, not merely older
tables: two SAS programs rather than three (`Comorb_ICD10CM_Format` / `Comorb_ICD10CM_Analy`),
measures named `ARTH`/`CHF` rather than `AUTOIMMUNE`/`HF`, six combination targets rather than ten,
no `CMR_` prefix on its output columns, and **no index program at all** — AHRQ states the Elixhauser
Comorbidity Indices Refined for ICD-10-CM "are not available until v2022.1." It also has no
independent reference to validate a translation against: the CRAN package `medicalcoder` has no
`elixhauser_ahrq2021` method, and its AHRQ SAS corpus has no 2021 arm.

## Parser gotchas

Four things bite anyone re-implementing this. All four are handled in `parse_ahrq_sas.R`, and each
one fails *silently* if missed:

1. **Encoding is per file, not per release.** 2026.1's Format and Index programs carry a UTF-8 BOM;
   its Mapping program does not, and 2022.1–2025.1 are plain ASCII. Probe for the BOM.
2. **`$POAXMPT_V41FMT` and `$POAXMPT_V43FMT` use single quotes**; every other block, `$COMFMT`
   included, uses double. A double-quote-only tokenizer yields zero codes for those two blocks — no
   error, just an empty POA-exempt list at ICD versions 41 and 43.
3. **Every block ends with `other = " "`.** Left in, it parses as a code named `" "` mapped to the
   preceding target. Blocks are truncated there.
4. **The file opens with a large `/* */` banner.** Comments are stripped before block detection so
   nothing in the prose can be mistaken for a value block.

CRLF needs no special handling — `\r` is whitespace outside quoted strings.

## What is and isn't regenerated

`build_datasets.R` writes `comfmt_releases.rda` (all five releases) and `comfmt_lookup.rda` (the
2026.1 view, kept for backwards compatibility) **from the same parse**, so the two cannot drift;
`checks.R` pins that `comfmt_lookup` equals the 2026.1 slice.

It does **not** write `poaxmpt_codes_long.rda`. The POA-exempt lists are keyed by ICD-10-CM version,
not by AHRQ release, and `checks.R` verifies that claim rather than assuming it: for every pair of
consecutive releases, the blocks for every shared ICD version are compared as token sequences and
must be identical. That currently holds for v33–v42 across all five releases — each release only
appends a block. Compare token sequences, never raw bytes: the last block of any file differs by
trailing content, because the next release appends after it, so a byte compare cries wolf.

The shipped `poaxmpt_codes_long` carries one `**OTHER**` sentinel row per version (11 total), an
artifact of the original parse having captured the `other = " "` catch-all. It is harmless —
`**OTHER**` can never match a normalised ICD code — and `parse_poaxmpt(other_sentinel = TRUE)`
reproduces it. Its within-version row *order* is an artifact of that dataset's incremental build
history (v41/v42 arrived sorted, v43 in file order), so `checks.R` compares code **sets**, which is
all the pipeline depends on: `build_poa_exempt_formats()` splits it into per-version vectors that are
only ever tested with `%in%`.

## Adding next year's release

1. Download the new zip, unpack into `SAS_software/<release>/`.
2. Add the release to `AHRQ_RELEASES` in `parse_ahrq_sas.R` and to `EXPECTED` in `checks.R` (code
   and target counts come from the release's `CMR-ChangeLog-*.xlsx`).
3. Add it to `CMR_RELEASES` in `R/utils.R`, and bump `cmr_default_release()`.
4. `Rscript data-raw/build_datasets.R && Rscript data-raw/checks.R`.
5. If the release adds a combination target, add its fan-out to `.handle_combination_codes()` —
   `.validate_comfmt_targets()` fails loudly if you forget.
6. Diff `CMR_Index_Program_*` against the prior release for weight changes; if any, add an override
   in `R/indices.R` the way `MW_2022` does.
7. Extend `tests/testthat/test-vs-ahrq.R` and `simulation/ahrq_releases/` with the new arm.
