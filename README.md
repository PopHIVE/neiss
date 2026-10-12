# neiss

Ingests the U.S. Consumer Product Safety Commission (CPSC) **National Electronic Injury Surveillance System (NEISS)** public files into the [PopHIVE](https://github.com/PopHIVE) standard wide format.

NEISS is a national probability sample of about 100 U.S. hospital emergency departments, so all output is national (`geography = "00"`). Each record carries a survey weight; both raw sampled counts and weighted national estimates are produced. See `measure_info.json` for variable definitions and caveats.

This is a [Data Collection Framework](https://github.com/PopHIVE/dcf) (`dcf`) data source project. It is maintained as a standalone repository rather than directly inside [PopHIVE/Ingest](https://github.com/PopHIVE/Ingest); its `standard/` output is pulled into `Ingest/data/neiss` from here.

## Contents

- `ingest.R` — downloads the yearly NEISS workbooks from CPSC (2019 onward, latest year auto-detected) and builds the standard files
- `measure_info.json` — metadata for each output variable
- `raw/lookups.rds`, `raw/nc-est2023-agesex-res.csv` — diagnosis/sex/race code tables and the Census population estimates used for rates. `ingest.R` re-downloads them if absent.
- `raw/neiss<year>.xlsx` — yearly CPSC workbooks (up to ~50 MB each). Gitignored; `ingest.R` downloads them.
- `standard/` — standardized output, two age schemes by two injury breakdowns, each with a `_rate` variant:
  - `data_infant_diagnosis`, `data_infant_product` — children under 2, age in months
  - `data_agegroup_diagnosis`, `data_agegroup_product` — 12 age groups

## Consumed by

`PopHIVE/Ingest` (`data/neiss`), which feeds the `bundle_sex_disparities` and `bundle_youth_wellbeing` bundles.

## Usage

```R
dcf::dcf_process()
```
