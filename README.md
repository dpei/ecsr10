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

**Methodology**: This R package faithfully adapts the Present on Admission (POA) logic and 38 comorbidity conditions as specified in the AHRQ documentation, translating the SAS workflow into R while maintaining the same clinical logic and hierarchical exclusion rules. Compared with the original Elixhauser comorbidity, the number of comorbidity measures in this implementation increases from 29 to 38, with three measures added, five measures modified to create 12 more specific measures, and one measure discontinued. This program uses POA indicators for 18 of the 38 comorbidity measures.

**Implemented releases**: **v2022.1 through v2026.1**, selectable per call. The default is
**v2026.1** (released October 2025), covering ICD-10-CM diagnosis codes from October 2015 through
September 2026 — ICD-10-CM versions 33 through 43.

``` r
cmr_releases()
#> [1] "2022.1" "2023.1" "2024.1" "2025.1" "2026.1"

cmr_version()   # the default
#> [1] "2026.1"

# Score under an earlier release - e.g. to reproduce a published cohort
res <- comorbidity(patient_data, dx_cols, poa_cols, release = "2023.1")
res <- cmr_index(res, release = "2023.1")
```

The release selects the diagnosis-code table, the newest ICD-10-CM version reachable from
year/quarter, and the index weights. It is recorded on the result as the `cmr_release` attribute,
and `cmr_index()` warns if you index flags from one release with another's weights. Report the
release alongside any published results — the same patient can flag differently across releases.

Every supported release reproduces AHRQ's own SAS output exactly: 167,322 encounters covering
81,212 distinct diagnosis codes, all 38 flags and both index scores, zero differing cells. See
`simulation/ahrq_releases/`.

v2021.1 is not supported. It is structurally different software — measures named `ARTH`/`CHF`
rather than `AUTOIMMUNE`/`HF`, no `CMR_` output prefix, and no index program at all (AHRQ: the
Indices "are not available until v2022.1").

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

# Apply comorbidity analysis
# The input should NOT contain the primary diagnosis. It should only contain secondary diagnosis.
# Alternatively, you may specify secondary diagnosis in dx_cols parameter in the comorbidity function.
# result <- comorbidity(patient_data, 
#                       dx_cols = c("dx1", "dx2", "dx3"),
#                       poa_cols = c("poa1", "poa2", "poa3"),
#                       ...)

# Calculate risk indices
# result_with_indices <- cmr_index(result)
```

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

To measure `ncores` on your own hardware and data, use
`simulation/large_data/benchmark_ncores.R`.

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
