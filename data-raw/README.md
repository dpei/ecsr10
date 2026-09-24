# data-raw/

Regenerates the package's lookup datasets from the AHRQ/HCUP SAS source. Not part of the built
package — `^data-raw$` is in `.Rbuildignore`.

```bash
cd ecsr10
Rscript data-raw/build_datasets.R   # writes the four .rda lookup datasets
Rscript data-raw/checks.R           # asserts everything above; exits 1 on any failure
```

| file | role |
|---|---|
| `parse_ahrq_sas.R` | tokenizer for the SAS `Proc format` value blocks; also `AHRQ_RELEASES`, `AHRQ_BETA_RELEASES`, `AHRQ_LAYOUT` and `release_max_icd_version()` |
| `build_datasets.R` | drives the parser over `../../SAS_software/` and writes the `.rda`s |
| `checks.R` | assertions over the generated data *and* the AHRQ source it came from |

## Source

`SAS_software/<release>/CMR_Format_Program_v<release>.sas`, one folder per AHRQ release. The
upstream index is <https://hcup-us.ahrq.gov/toolssoftware/comorbidityicd10/comorbidity_icd10.jsp>;
prior releases are at `comorbidity_icd10_archive.jsp`. The zips download from the flat path
`https://hcup-us.ahrq.gov/toolssoftware/comorbidityicd10/`:

| release | zip | released |
|---|---|---|
| 2021.1 | `ElixhauserComorbidity_v2021-1.zip` | Oct 2020 |
| 2022.1 | `CMR_v2022-1.zip` | Oct 2021 |
| 2023.1 | `CMR_v2023-1.zip` | Dec 2022 |
| 2024.1 | `CMR_v2024-1.zip` | Mar 2024 |
| 2025.1 | `CMR_v2025-1.zip` | Nov 2024 |
| 2026.1 | `CMR-v2026-1.zip` | Nov 2025 |

Unpack each into `SAS_software/<release>/`. The v2022.1+ zips carry the same six files (three SAS
programs, reference workbook, changelog, user guide); v2021.1 carries five, having no index program.

### Beta versions — `SAS_software/beta/<version>/`

Two plain `.txt` programs each, no zip, no reference workbook:

| version | format program | analysis program | MS-DRG grouper |
|---|---|---|---|
| 2016.2 | `comformat_icd10cm_2016_2.txt` | `comoanaly_icd10cm_2016.txt` | V34 |
| 2017.2 | `comformat_icd10cm_2017_2.txt` | `comoanaly_icd10cm_2017.txt` | V34 |
| 2018.1 | `comformat_icd10cm_2018.txt` | `comoanaly_icd10cm_2018.txt` | V35 |
| 2019.2 | `comformat_icd10cm_2019_2.txt` | `comoanaly_icd10cm_2019_2.txt` | V36 |
| 2020.1 | `comformat_icd10cm_2020_1.txt` | `comoanaly_icd10cm_2020_1.txt` | V37 |

Note the analysis-program naming is not uniform: v2016.2–v2018.1 drop the point release from the
file name, v2019.2 and v2020.1 keep it. Each version also has a `TableofChanges*.pdf` /
`Table-of-Changes-*.pdf`, which is the cross-check for the parse the way `CMR-ChangeLog-*.xlsx` is
for the refined releases.

**All five beta analysis programs are the same program.** Normalise whitespace and the
`DX`/`I10_DX` variable rename that landed at v2019.2, and they diff to zero lines. Only the format
program's code lists and MS-DRG ranges move, which is why `R/beta.R` is one release-blind engine.

**v2021.1 needs the parser to be flexible about file names.** It ships two SAS programs rather than
three — `Comorb_ICD10CM_Format` and `Comorb_ICD10CM_Analy`, with no index program, because AHRQ
states the Elixhauser Comorbidity Indices "are not available until v2022.1". `AHRQ_LAYOUT` in
`parse_ahrq_sas.R` is what lets one parser find every naming. Its measures are the same 38 as
v2022.1's, with two spelled `ARTH`/`CHF` rather than `AUTOIMMUNE`/`HF`; the package emits v2021.1's
own spelling, and `checks.R` pins the rename against AHRQ's own changelog.

## Parser gotchas

Five things bite anyone re-implementing this. All are handled in `parse_ahrq_sas.R`, and each one
fails *silently* if missed:

1. **Encoding is per file, not per release.** 2026.1's Format and Index programs carry a UTF-8 BOM;
   its Mapping program does not, and 2022.1–2025.1 are plain ASCII. Probe for the BOM.
2. **`$POAXMPT_V41FMT` and `$POAXMPT_V43FMT` use single quotes**; every other block, `$COMFMT`
   included, uses double. A double-quote-only tokenizer yields zero codes for those two blocks — no
   error, just an empty POA-exempt list at ICD versions 41 and 43.
3. **Every block ends with `other = " "`.** Left in, it parses as a code named `" "` mapped to the
   preceding target. Blocks are truncated there.
4. **The file opens with a large `/* */` banner.** Comments are stripped before block detection so
   nothing in the prose can be mistaken for a value block.

5. **The beta MS-DRG screens are NUMERIC value blocks.** `Value $NAME` matches only character
   formats, so `sas_value_blocks()` cannot see `VALUE CARDDRG 001-002, ... = "YES";` at all — it
   returns the code table and nothing else, with no error. `sas_drg_blocks()` handles those. Two
   traps of their own: the DRGs are zero-padded in the source (`054` is DRG 54) while `DRG` is
   numeric in the DATA step, and these blocks carry **no** `other =` clause, so nothing truncates
   them but the next `VALUE` header.

CRLF needs no special handling — `\r` is whitespace outside quoted strings.

## The NONE catch-all, and why beta rows are dropped

`build_datasets.R` drops every beta row whose target is AHRQ's `NONE` catch-all. That is a no-op,
not a simplification: the analysis program tests `DXVALUE` against its 30 measure names and then its
10 hypertension labels, so `"NONE"` falls through both exactly the way an unmapped code does. It
matters because **v2016.2 does not rely on `other = " "`** — it enumerates all 66,666
non-comorbidity codes explicitly, in two spellings (`None` and `NONE`), which would make the shipped
table twenty times larger for no behavioural difference. The drop is only safe because no beta code
carries a `%` wildcard, so matching is exact and a dropped row cannot expose a code to some other
pattern; `checks.R` pins both that and the fact that `NONE` is the only target ever dropped.

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
3. Add it to `CMR_RELEASES` in `R/utils.R`. **Append** — `cmr_version()` returns the last element,
   so appending moves the default; an *older* release must be prepended instead.
4. `Rscript data-raw/build_datasets.R && Rscript data-raw/checks.R`.
5. If the release adds a combination target, add its fan-out to `.handle_combination_codes()` —
   `.validate_comfmt_targets()` fails loudly if you forget.
6. Diff `CMR_Index_Program_*` against the prior release for weight changes; if any, add an override
   in `R/indices.R` the way `MW_2022` does.
7. Extend `tests/testthat/test-vs-ahrq.R` and `simulation/ahrq_releases/` with the new arm.
