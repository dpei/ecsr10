# ecsr10

<!-- badges: start -->
<!-- badges: end -->

The **ecsr10** package provides an R implementation of the Elixhauser Comorbidity Software Refined (CMR) workflow for analyzing ICD-10-CM diagnosis codes and calculating comorbidity indices. This package is an R adaptation of the original SAS-based software developed by the Agency for Healthcare Research and Quality (AHRQ) as part of the Healthcare Cost and Utilization Project (HCUP).

## Overview

This package processes patient diagnosis data to:

- Identify 38 specific comorbidity categories from secondary diagnosis
- Calculate mortality and readmission risk indices
- Handle Present on Admission (POA) logic
- Apply hierarchical exclusion rules

The implementation translates SAS-based comorbidity logic into efficient R code, maintaining compatibility with the original CMR workflow.

## Attribution and Source

This package is based on the **Elixhauser Comorbidity Software Refined for ICD-10-CM Diagnoses** developed by:

- **AHRQ** - Agency for Healthcare Research and Quality
- **HCUP** - Healthcare Cost and Utilization Project (a Federal-State-Industry partnership sponsored by AHRQ)

**Original SAS Program**: The official SAS software and documentation can be found at:
[https://hcup-us.ahrq.gov/toolssoftware/comorbidityicd10/comorbidity_icd10.jsp](https://hcup-us.ahrq.gov/toolssoftware/comorbidityicd10/comorbidity_icd10.jsp)

**Methodology**: This R package faithfully adapts the Present on Admission (POA) logic and 38 comorbidity conditions as specified in the AHRQ documentation, translating the SAS workflow into R while maintaining the same clinical logic and hierarchical exclusion rules. Compared with the original Elixhauser comorbidity, the number of comorbidity measures in this implementation increases from 29 to 38, with three measures added, five measures modified to create 12 more specific measures, and one measure discontinued. This program uses POA indicators for 18 of the 38 comorbidity measures. Running without them (`use_poa = FALSE`) returns those 18 as `NA`, which is what the SAS program emits under `%LET POA = 0`; the remaining 20 and both indices are scored as usual.

**Implemented releases**: both families AHRQ has published, selectable per call.

| variant | releases | measures | screened on | indices |
|---|---|---|---|---|
| `"refined"` (default) | v2021.1 - v2026.1 | 38 | POA, for 18 of them | v2022.1 on |
| `"beta"` | v2016.2 - v2020.1 | 30 + `CMRB_HTN_C` | MS-DRG | none |

The default is **refined v2026.1** (released October 2025). Together the two families cover
ICD-10-CM diagnosis codes from October 2015 through September 2026 - ICD-10-CM versions 33 to 43.

``` r
cmr_releases()
#> [1] "2021.1" "2022.1" "2023.1" "2024.1" "2025.1" "2026.1"

cmr_releases("beta")
#> [1] "2016.2" "2017.2" "2018.1" "2019.2" "2020.1"

cmr_version()   # the default
#> [1] "2026.1"

# Score under an earlier release - e.g. to reproduce a published cohort
res <- comorbidity(patient_data, dx_cols, poa_cols, release = "2023.1")
res <- cmr_index(res, release = "2023.1")
```

The release selects the diagnosis-code table, the newest ICD-10-CM version reachable from
year/quarter, and the index weights. It is recorded on the result as the `cmr_release` attribute
(and the family as `cmr_variant`), and `cmr_index()` warns if you index flags from one release with
another's weights. Report the release alongside any published results — the same patient can flag
differently across releases.

Every supported release reproduces AHRQ's own SAS output exactly. See `simulation/ahrq_releases/`
(167,322 encounters, v2022.1-v2026.1), `simulation/multi_release/` (11,954 encounters over 67
discharge-date cells, v2021.1-v2026.1) and `simulation/beta_releases/` (6,018 encounters over 556
MS-DRGs, v2016.2-v2020.1) — zero differing cells in all of them.

### Two things that differ at v2021.1

v2021.1 is the first *Refined* release, and it predates two conventions the later ones share:

- **It names two measures differently.** `CMR_ARTH` and `CMR_CHF`, which v2022.1 renamed to
  `CMR_AUTOIMMUNE` and `CMR_HF`. `comorbidity()` emits v2021.1's own spelling, so code that
  hardcodes either pair must branch on the release. Nothing else about the 38 measures changed —
  AHRQ's changelog records no measure added, redefined or discontinued at that boundary.
- **It has no comorbidity indices.** AHRQ states they "are not available until v2022.1", so
  `cmr_index()` refuses the release rather than applying some other release's weights.

### The beta family

v2016.2 - v2020.1 are the *beta* Elixhauser Comorbidity Software — superseded software, not merely
older tables. They have no POA concept at all; in its place they apply an **MS-DRG exclusion
screen**, suppressing a comorbidity when the encounter's MS-DRG is directly related to the principal
diagnosis. Output columns carry a `CMRB_` prefix, because several beta measure names coincide with
refined ones while meaning something different (beta `CHF` is DRG-screened where refined `CMR_HF` is
POA-screened).

``` r
res <- comorbidity(patient_data, dx_cols,
                   variant = "beta", release = "2020.1", drg_col = "drg")
```

Use the refined software unless you are specifically reproducing a pre-2021 analysis; AHRQ's own
advice is to use the most recent version of the tool.

## Installation

You can install the development version of ecsr10 from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("dpei/ecsr10")
```

## Basic Usage

``` r
library(ecsr10)

# Load your patient diagnosis data
# patient_data <- read_csv("your_patient_data.csv")

# Apply comorbidity analysis.
#
# SECONDARY DIAGNOSES ONLY. The AHRQ software opens its diagnosis loop at
# position 2 (`DO I = 2 TO ...`), so the principal diagnosis is never examined.
# ecsr10 scores every column you hand it, so leave dx1/poa1 out of dx_cols and
# poa_cols - passing them changes flags rather than erroring, and draws a warning.
# result <- comorbidity(patient_data,
#                       dx_cols  = c("dx2", "dx3", "dx4"),
#                       poa_cols = c("poa2", "poa3", "poa4"),
#                       ...)

# Calculate risk indices
# result_with_indices <- cmr_index(result)
```

`dx_cols` and `poa_cols` are paired by the **first run of digits in each column
name**, not by position in the two vectors, and the two position sets must match
exactly. `dx3` paired with `poa4` is an error, not a silent blank POA; so are
duplicate positions, which is what HCUP's own `I10_DX2`/`I10_DX3` produce (both
resolve to 10). Use names whose only digits are the position.

ecsr10 does not apply the encounter's `I10_NDX` diagnosis count either — SAS caps
its loop there, so a populated column past that count is ignored. Truncate
`dx_cols` yourself if your data carries a meaningful count.

### Parallel execution

`comorbidity()` accepts `ncores` (default `1`), which splits the encounters into
that many contiguous blocks and runs the whole pipeline — reshape, pattern
matching, POA rules, matrix build — on each block in a forked worker. Results are
identical for every `ncores` value.

``` r
# result <- comorbidity(patient_data, dx_cols = ..., poa_cols = ..., ncores = 4)
```

Each worker materializes its own long-format intermediate, so peak memory grows
with `ncores`. On large inputs that, rather than core count, is usually what limits
how high you can set it.

Values above `parallel::detectCores()` are clamped with a warning. Forking is
unavailable on Windows, where any `ncores > 1` falls back to serial with a warning.

Speedup is machine- and data-specific, so measure it on your own hardware rather than
relying on a published figure, and give each core count a fresh R process — a grid walked
inside one process reports spuriously flat speedups for its later cells. This repository's
benchmark, over a real discharge cohort, is `application/code/benchmark_ncores.R`, which
`cd application && ./run_all.sh` runs as part of the pipeline (`--no-benchmark` skips it).

## Data Requirements

### Input Patient Diagnosis Data
- Wide format with DX columns (diagnosis codes)  
- POA columns (Present on Admission indicators)
- Year and quarter for ICD version determination

### Lookup Data
The package includes:
- `comfmt_lookup`: ICD-10-CM to comorbidity mappings
- `poaxmpt_codes_long`: POA exempt codes by ICD version

## Reference

When using this package, please cite the original AHRQ methodology:

> Agency for Healthcare Research and Quality (AHRQ). Elixhauser Comorbidity Software Refined for ICD-10-CM Diagnoses. Healthcare Cost and Utilization Project (HCUP). Rockville, MD: AHRQ. Available at: https://hcup-us.ahrq.gov/toolssoftware/comorbidityicd10/comorbidity_icd10.jsp

## Author

**Dong Pei**  
Email: peidong2009@gmail.com

## License

MIT License. See LICENSE.md for details.
