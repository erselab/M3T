# Data provenance & regeneration (`_data_raw/`)

The Python package **ships** its reference datasets under `src/m3t/data/`
(parquet + JSON), converted directly from the R package's `.rda` files by
[`build_data.py`](build_data.py). That conversion is the source of the shipped
data — it guarantees the Python package uses values **identical** to R.

This directory holds the tooling to (a) rebuild the shipped data and (b)
optionally refresh a dataset from its upstream source.

## Files

| File | Purpose |
|---|---|
| [`export_rda_reference.R`](export_rda_reference.R) | R script: emit ground-truth metadata (`tests/golden/rda_reference.json`) + full content of the list/matrix datasets (`ghgi_lists/`). Run in the `M3T` conda env. |
| [`build_data.py`](build_data.py) | Convert `data/*.rda` → `src/m3t/data/*.parquet` \| `*.json`. |
| [`scrapers.py`](scrapers.py) | Python ports of the reproducible `data-raw` scrapers (refresh tooling). |
| `ghgi_lists/` | R-exported JSON for the datasets pyreadr can't read (2 GHGI lists + `Neighboring_states` matrix). |

## Rebuild the shipped data

```bash
conda run -n M3T Rscript python/_data_raw/export_rda_reference.R   # ground truth + list/matrix JSON
python python/_data_raw/build_data.py                              # -> src/m3t/data/
pytest tests/test_datasets.py                                      # validate vs R
```

## Catalog of the 15 R `data-raw` scripts → 19 datasets

Reproducibility:
**live** = fully reproducible from a public URL;
**local** = the R script reads a file from the original author's machine (not
reproducible here — the `.rda` is the only source);
**complex** = reproducible but non-trivial parsing (Excel annex tables).

| R script | Dataset(s) | Source | Repro | Python port |
|---|---|---|---|---|
| `GHGRP_wastewater.R` | GHGRP_wastewater | EPA dmapservice subpart II CSV | live | ✅ `scrapers.ghgrp_wastewater` (validated: sums match) |
| `GHGRP_combustion_emissions.R` | GHGRP_combustion_emissions | EPA dmapservice subpart C CSV | live | ✅ `scrapers.ghgrp_combustion_emissions` |
| `GHGRP_landfills.R` | GHGRP_landfills | EPA dmapservice subpart HH + gas-collection details | live | ✅ `scrapers.ghgrp_landfills` |
| `GHGRP_LDC.R` | GHGRP_LDC | EPA dmapservice subpart W (LDC) | live | ⬜ TODO (same dmap pattern; extra column derivations) |
| `EIA_SEDS.R` | EIA_SEDS | EIA SEDS bulk zip → JSON lines | live | ⬜ TODO (zip + series filter) |
| `Census_state_population_M3T.R` | Census_state_population_M3T | Census popest CSVs (2010-2020 + newest) | live | ⬜ TODO (two CSVs merged, CONUS subset) |
| `LMOP_data.R` | LMOP_data | EPA LMOP landfill xlsx | live | ⬜ TODO (Excel) |
| `NEI_all_years.R` | NEI_all_years | EPA NEI | live | ⬜ TODO |
| `Neighboring_states.R` | Neighboring_states | derived adjacency matrix | live | ⬜ TODO (pure derivation) |
| `Wastewater_1990_state_septic.R` | Wastewater_1990_state_septic | Census 1990 sewage table | live | ⬜ TODO |
| `ghgi_annex_36.R` | GHGI_landfill_total_M3T, GHGI_NG_distribution, GHGI_NG_transmission, GHGI_stationary_combustion | EPA GHGI Annex Excel tables | complex | ⬜ TODO (Excel annex parsing) |
| `CWNS_2012.R` | CWNS_2012 | EPA CWNS 2012 export | local | — (`.rda` only) |
| `CWNS_2022.R` | CWNS_2022 | EPA CWNS 2022 export | local | — (`.rda` only) |
| `EIA_NG_file.R` | EIA_NG_data | EIA NG (author-preprocessed file) | local | — (`.rda` only) |
| `HIFLD_NG_data.R` | HIFLD_NG_data | HIFLD compressor stations xlsx (author's drive) | local | — (`.rda` only) |
| `PHMSA_natural_gas_distribution.R` | PHMSA_natural_gas_distribution | PHMSA annual reports (author-preprocessed) | local | — (`.rda` only) |

The **local** scripts reference paths on the original author's machine
(`grep -l "input_directory <-"` in `data-raw/`), so their datasets can only come
from the shipped `.rda`. The remaining **live**/**complex** scrapers are worth
porting incrementally; the three GHGRP ports establish the shared
`dmapservice` + `make_consistent` pattern the rest follow.
