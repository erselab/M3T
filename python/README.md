# M3T (Python port)

Python port of the **Modular Methane Mapping Tool (M3T)** — gridded, sectoral
methane emission inventories for U.S. urban areas. See
[`../PYTHON_PORT_PLAN.md`](../PYTHON_PORT_PLAN.md) for the full plan and status.

> **Status: Phase 3 (sector ports, in progress).** Foundation complete (config,
> terra-parity geo shim, domain builder, 19 datasets, validation, orchestrator).
> Sectors run through `ch4_inventory_build` end-to-end, each golden-tested
> cell-for-cell against the corresponding R function.
>
> Ported sectors: **landfills** ✅, **natural gas transmission** ✅,
> **wastewater** ✅ (municipal CWNS/DMR × GHGI/Moore, industrial GHGRP subpart II,
> septic national/by-state, and all 8 sector-total variants — golden-tested against
> R and running through `ch4_inventory_build`).
> **Stationary combustion** ✅ golden-tested (SEDS → NEI → Vulcan/ACES
> disaggregation, 14 subsectors × 2 levels + totals) but **not yet registered**:
> the orchestrator can't supply county Tigerlines or a gridded CO₂ inventory yet,
> and the default `Use_Vulcan` needs a large Zenodo download.
> Stubs: NG distribution, wetlands, remaining-GEPA.
>
> State-resolved sectors need the Census Tigerlines: pass `tigerlines=` to
> `ch4_inventory_build` and the orchestrator derives `state_tigerlines` /
> `state_name_list` (territories dropped, clipped to the domain, sorted by STUSPS)
> onto the `RunContext`. Septic requires this.

## Companion data

The large inputs (NLCD land cover, DMR, Tigerlines, Vulcan, ...) are **not** shipped
with the code — download the M3T companion archive from Zenodo separately and point
the `Source_*` config options at it. Tests use small clipped fixtures committed
under `tests/golden/`, so the suite runs without it; the R oracle-capture scripts
do need it (`M3T_DATA=/path/to/M3T_Processed`).

## Install

The geospatial stack (rioxarray / rasterio / geopandas) pulls GDAL/PROJ/GEOS;
use conda rather than pip wheels.

```bash
conda env create -f environment-py.yaml   # creates env "m3t-py"
conda activate m3t-py
pip install -e .
```

## Test

```bash
pytest -q                       # unit + invariant tests
pytest -q -m "not network"      # skip anything needing internet
```

To (re)generate the terra oracle fixtures the geo tests compare against, run the
R capture script (requires the R package + terra installed):

```bash
Rscript tests/golden/capture_geo_oracle.R
```

## Layout

| Path | Purpose | R origin |
|---|---|---|
| `src/m3t/config.py` | `Config` dataclass, 78 options + `get`/`set` | `R/M3T_config.R` |
| `src/m3t/geo.py` | terra-parity raster/vector shim over rioxarray | `terra::*` calls |
| `src/m3t/domain.py` | build target-grid domain (box/CONUS/file/vector) | domain block of `CH4_inventory_build.R` |
| `src/m3t/download.py` | retrying downloader, Zenodo/Vulcan fetchers | `R/utils.R`, orchestrator |
| `src/m3t/datasets.py` | loader for the 19 packaged datasets | R `LazyData` |
| `src/m3t/validation.py` | config error-checking (accumulated) | error block of `CH4_inventory_build.R` |
| `src/m3t/context.py` | `RunContext` threaded to every sector | R sector arg lists |
| `src/m3t/inventory.py` | `ch4_inventory_build` orchestrator | `R/CH4_inventory_build.R` |
| `src/m3t/sectors/` | sector interface + registry (stubs now) | `R/<Sector>.R` (Phase 3) |
| `src/m3t/combine.py` | sum sectors into a total | `R/Combine_across_sectors.R` |
| `_data_raw/` | data conversion + scraper provenance | `data-raw/*.R` |
| `tests/` | unit, invariant, and golden/oracle tests | — |
