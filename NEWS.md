# ecsr10 0.5.0

## `poa_missing` removed from `comorbidity()`

The `poa_missing` argument and its `"assume_present"` mode are gone. With
`use_poa = FALSE` the 18 POA-dependent measures are always `NA`, as AHRQ's SAS
program returns them. The removed mode had no SAS counterpart and was internally
inconsistent (`CMR_CBVD` was 0 for every row, and the four combination targets
with no POA-neutral component contributed nothing); the applied study in the
manuscript found it changed the comorbidity profile of 11.4% of admissions
relative to the reference scoring, twice the effect of overwriting POA to `Y`.
Callers who passed `poa_missing = "na"` should drop the argument; callers who
relied on `"assume_present"` should set every POA value to `Y` and score with
`use_poa = TRUE`, which is the direct representation of a file without POA.

# ecsr10 0.4.2

Input-validation and documentation release. No scoring rule, lookup table or
index weight changed: all five parity harnesses reproduce AHRQ's SAS output
exactly, as before.

## `comorbidity()` now rejects mispaired dx/POA columns

`dx_cols` and `poa_cols` are joined on the **first run of digits in each column
name**, never on position within the two vectors. Until now the only guard was a
check for a *fully* disjoint position set, so a **partial** mismatch was
accepted:

```r
comorbidity(d, dx_cols = c("dx2", "dx3"), poa_cols = c("poa2", "poa4"))
```

Position 2 paired; position 3 joined to nothing, coalesced to `""`, and — since
`""` is neither `"Y"` nor `"W"` — every POA-dependent measure silently stopped
firing on that diagnosis. On a frame where `dx3` carried heart failure with
`poa3 = "Y"`, `CMR_HF` went from 1 to 0 with no error and no warning.

Three cases are now errors, each naming what would have gone wrong:

- **Unequal position sets**, including the partial-overlap case above.
- **Duplicate extracted positions.** HCUP's own `I10_DX2` and `I10_DX3` both
  resolve to 10, because the first digit run wins.
- **Names carrying no digits**, which resolve to `NA` and pair with nothing.

Vector order is explicitly *not* part of the contract: `c("poa3", "poa2")` pairs
exactly as `c("poa2", "poa3")` does, and both are accepted.

The check moved from the reshaped data to the column **names**, and from
`.comorbidity_flags()` to the parent `comorbidity()`. Two consequences: a
diagnosis column that is entirely `NA` is now validated (the pivot used to drop
its position before the old check could see it), and validation happens once,
before any fork, rather than inside every worker.

The previous fully-disjoint error is subsumed. Code matching on its old message,
`"no dx/POA column pairs"`, should match `"do not pair one-to-one"` or
`"duplicate diagnosis position"` instead.

## `comorbidity()` warns when passed the principal diagnosis

The AHRQ software scores **secondary diagnoses only** — its loop runs
`DO I = 2 TO MIN(&MAXNDX,&NUMDX)` in the refined mapping program and
`DO I = 2 TO MIN(NDX,&NUMDX)` in the beta one, so the principal diagnosis is
never examined. `ecsr10` scores every column in `dx_cols`, which makes the
exclusion the caller's job — and getting it wrong changes flags rather than
erroring.

A `dx_cols` entry whose name resolves to position 1 now warns. It is a warning
rather than an error because a caller who has already dropped the principal
diagnosis may legitimately have named the first remaining column `dx1`. Both
variants are checked.

Neither the `README` example nor the vignette had been following this rule; both
now do.

## Bug fix: custom lookup version filtering never ran

`build_comfmt_from_csv(version =)` and `.comfmt_from_df(version =)` documented
optional filtering on `version_min`/`version_max`, but the schema was reduced to
`(target, pattern)` — dropping both columns — *before* the code tested whether
they existed. The branch was unreachable for both accepted input schemas, so
`version` was silently inert: a code first valid at ICD-10-CM version 40 stayed
in the table when version 33 was requested.

Filtering now runs before the reduction. Bounds are inclusive, and an absent
limit is open-ended on that side.

This affects **custom** lookup tables only. The built-in tables carry neither
column, so nothing about release selection changes — and `version` remains an
ICD-10-CM version axis (33–43), not an AHRQ release axis (2021.1–2026.1).

## Documentation

- `dx_cols` and `poa_cols` now document the secondary-diagnosis rule, the
  name-based pairing contract, and the fact that SAS's `I10_NDX` diagnosis-count
  cap is **not** applied — a populated column past the encounter's count is
  scored, where SAS ignores it.
- A new "Deliberate differences from the AHRQ SAS programs" section on
  `?comorbidity` collects the places where the interface is more forgiving than
  SAS (diagnosis-code and POA normalisation) or leaves a responsibility with the
  caller (principal diagnosis, `I10_NDX`).
- The vignette no longer calls the unexported `build_comfmt_from_csv()` and
  `build_poa_exempt_formats()`. Its "Custom Data Sources" section now uses the
  supported public workflow — passing plain data frames to `comfmt` and
  `poa_exempt` — and the chunk evaluates, so the vignette build would catch it
  breaking. A duplicated copy of that section was removed.
- `simulation/sas_parity/README.md` no longer claims the package test suite does
  not cover `comorbidity()`.

## Shiny front-end

`inspect_input()` compared dx and POA column *counts*, which the package's new
strict pairing would have turned into an error for an upload with equal-length
but mismatched sets (`dx2,dx3,dx4` against `poa2,poa3,poa5`). It now compares
position sets, so such a file still degrades to the "POA not used" notice rather
than raising.

# ecsr10 0.4.1

## Breaking change: `use_poa = FALSE` now matches the AHRQ SAS program

`comorbidity(use_poa = FALSE)` returns `NA` for the 18 POA-dependent measures
instead of 0/1. This was the one configuration in which `ecsr10` disagreed with
AHRQ's SAS software; it now agrees on every compared cell.

AHRQ's mapping program initialises those measures only inside `%if &POA.=1`, so
under `%LET POA = 0` they reach the output dataset **missing**. `ecsr10`
previously emitted 0/1, assigning each gated measure as though every diagnosis
were present on admission. Measured over the 11,954-encounter fixture in
`simulation/multi_release/` at v2026.1, that was **221,618 of 478,160 cells**;
it is now 0.

The affected columns are `CMR_ANEMDEF`, `CMR_BLDLOSS`, `CMR_CBVD`, `CMR_COAG`,
`CMR_HF`, `CMR_LIVER_MLD`, `CMR_LIVER_SEV`, `CMR_NEURO_MOVT`, `CMR_NEURO_OTH`,
`CMR_NEURO_SEIZ`, `CMR_PARALYSIS`, `CMR_PSYCHOSES`, `CMR_PULMCIRC`,
`CMR_RENLFL_MOD`, `CMR_RENLFL_SEV`, `CMR_ULCER_PEPTIC`, `CMR_VALVE` and
`CMR_WGHTLOSS`. They remain **present** in the output, matching SAS, whose
`LENGTH`, `ARRAY` and `LABEL` statements are outside the POA branch - so the
result schema is unchanged and a POA-off frame still binds against a POA-on one.

`use_poa = TRUE` (the default) and `variant = "beta"` are unaffected.

### What this means for `cmr_index()`

Nothing changed in the function, but its output moves with the flags. `NA`
contributes 0, which is arithmetically what SAS's `SUM(OF ...)` does when it
skips a missing term, so a POA-off frame now scores over the 20 POA-neutral
measures only - exactly as AHRQ's index program does, which has no POA switch at
all. Both indices still come back as ordinary numbers, not `NA`, so a POA-off
score is not comparable with a POA-on one and nothing in the output says so.

## Other changes

* `use_poa` is now validated up front. `NA`, a string, or a vector of length
  other than 1 raise an error naming the argument, rather than failing later in
  whichever scalar or vectorized context reached the value first.
* `comorbidity()` records `attr(result, "cmr_use_poa")` alongside the existing
  `cmr_release` and `cmr_variant` attributes. Informational; nothing in the
  package reads it, since `[`-subsetting drops attributes.
* New test file `tests/testthat/test-poa-off.R` pins the contract above,
  including the `cmr_index()` NA-equals-zero equivalence.
* `simulation/multi_release/run_all.sh --with-sas` now runs and **gates** the
  `%LET POA = 0` arm for every release. It previously produced the `ecsr10`
  side of that arm on every run and compared it against nothing, which is why
  the divergence went unmeasured.
