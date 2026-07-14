# M3T (Python port)

Python port of the **Modular Methane Mapping Tool (M3T)** — gridded, sectoral
methane emission inventories for U.S. urban areas. See
[`../PYTHON_PORT_PLAN.md`](../PYTHON_PORT_PLAN.md) for status, the terra-parity
notes, and the remaining work.

> **All seven sectors are ported**, each golden-tested cell-for-cell against the
> corresponding R function and running end-to-end through `ch4_inventory_build`.
> `Combine_across_sectors` is ported too (summary min/mean/max, individual
> combinations + key, thermogenic split), as is plotting — pass `verbose=True` to
> get a map per sector plus the summed inventory. 202 tests pass.
>
> The whole R package is now ported; only packaging (a PyPI/conda release) is left.
>
> Note `combine` is a **corrected** port — the R original mis-joins stationary
> combustion's two files, writes a key that doesn't match its rasters, and cannot
> run `Separate_thermo` + summary at all. See `PYTHON_PORT_PLAN.md` §5b.

## Sectors

| Sector | Notes |
|---|---|
| Landfills (municipal solid waste) | GHGRP subpart HH + C, three method variants, LMOP residual |
| Natural gas transmission | EIA pipelines × GHGI EF; HIFLD compressors overridden by GHGRP subpart W |
| Wastewater | Municipal CWNS/DMR × GHGI/Moore, industrial GHGRP subpart II, septic national/by-state, 8 sector-total variants |
| Stationary combustion | SEDS → NEI → Vulcan/ACES disaggregation; 14 subsectors × 2 levels + totals |
| Natural gas distribution | PHMSA/EIA/GHGRP activity → 6 subsectors × res/com × 2 levels (`by_LDC` not ported) |
| Wetlands & inland waters | Wetcharts ensemble + NWI/SOCCR1/SOCCR2 and freshwater |
| Remaining sectors (gridded EPA) | GEPA v2: industrial landfills, biogenic, thermogenic |

## Install

The geospatial stack (rioxarray / rasterio / geopandas) pulls GDAL/PROJ/GEOS;
use conda rather than pip wheels.

```bash
conda env create -f environment-py.yaml   # creates env "m3t-py"
conda activate m3t-py
pip install -e .
```

## Data you must supply

The large inputs (NLCD, DMR, Census Tigerlines, Vulcan, Wetcharts, GHGRP tables, ...)
are **not** shipped with the code. Download the M3T companion archive from Zenodo.

`Source_*="M3T"` — the default for most options — means *"read it from the run's
`in/` directory"*. So the normal way to run is to **stage the companion files into
`<run_directory>/in/`** (symlink or copy) and leave the defaults alone; if a needed
file is missing you get an error naming the exact path. Individual options can
instead be set to `"download"` or to a local file path.

`notebooks/nyc_demo.py` is the worked example — see its `prepare_run_dir()`, which
symlinks the companion files into each run's `in/` so the run exercises the default
`"M3T"` code paths.

Beyond that:

* **Stationary combustion** needs a gridded CO₂ proxy: an extracted `Vulcan_v4.0/`
  in `in/` (or `Source_Vulcan="download"` to fetch and unpack the four sector zips),
  or set `Use_ACES`.
* **State-resolved sectors** (including septic) need the Census Tigerlines: pass
  `tigerlines=` to `ch4_inventory_build`; the orchestrator derives `state_tigerlines`
  / `state_name_list` onto the `RunContext` (territories dropped, clipped to the
  domain, sorted by STUSPS).

Tests use small clipped fixtures committed under `tests/golden/`, so the suite runs
offline without the archive. The R oracle-capture scripts do need it
(`M3T_DATA=/path/to/M3T_Processed`).

## Quickstart

```python
import os
import geopandas as gpd
import m3t

DATA = os.environ["M3T_DATA"]   # the extracted Zenodo companion archive
# ./run/in/ has been staged from DATA (see notebooks/nyc_demo.py::prepare_run_dir)

states = gpd.read_file(f"{DATA}/combined_state_tigerlines.gpkg", layer="2019")

cfg = m3t.Config()
cfg.Process_wetlands_and_inland_waters = False   # config is a plain dataclass

ctx = m3t.ch4_inventory_build(
    run_directory="./run",
    inventory_year=2019,
    domain=(-74.3, 40.5, -73.6, 41.0),   # or "CONUS", ["CT","RI"], a file, a GeoDataFrame
    domain_res=0.02,
    domain_crs="epsg:4326",
    tigerlines=states,
    config=cfg,
)
# per-sector rasters -> ./run/out/<sector>.nc ; combined total -> ./run/out/M3T_total.nc
```

The config is copied per run, so a run can never mutate the object you passed in.

## Mass conservation

`Mass_conserving_regrid` is the one config option with no R equivalent. It defaults
to **`True`**, which conserves mass through disaggregation. The R code inflates
gridded totals by **~1.4%** (it treats Lambert-Conformal-Conic 1 km pixels as exactly
1 km², then area-*averages* onto the target grid). Setting it to `False` reproduces
R exactly and is what the golden tests use. See `PYTHON_PORT_PLAN.md` §5.

## Test

```bash
pytest -q                      # full suite
pytest -q -m "not network"     # skip anything needing internet
pytest -q -m "not golden"      # skip R-oracle comparisons
ruff check src tests _data_raw
```

Regenerate an R oracle (needs the R `M3T` package + terra installed):

```bash
conda run -n M3T Rscript tests/golden/capture_landfills_oracle.R
```

## Layout

| Path | Purpose | R origin |
|---|---|---|
| `src/m3t/config.py` | `Config` dataclass, 79 options + `get`/`set` | `M3T_config.R` |
| `src/m3t/geo.py` | terra-parity raster/vector shim over rioxarray | `terra::*` calls |
| `src/m3t/domain.py` | target-grid domain (box/CONUS/file/vector/state) | domain block of `CH4_inventory_build.R` |
| `src/m3t/shared_data.py` | cross-sector input loaders (GHGRP, GHGI, Vulcan/ACES, NLCD, ...) | data-prep blocks of `CH4_inventory_build.R` |
| `src/m3t/datasets.py` | loader for the 19 packaged datasets | R `LazyData` |
| `src/m3t/validation.py` | config error-checking (accumulated, fail-fast) | error block of `CH4_inventory_build.R` |
| `src/m3t/context.py` | `RunContext` threaded to every sector | R's long sector arg lists |
| `src/m3t/inventory.py` | `ch4_inventory_build` orchestrator | `CH4_inventory_build.R` |
| `src/m3t/disaggregation.py` | county → grid disaggregation | `Inventory_based_disaggregation.R` |
| `src/m3t/sectors/` | the seven sector ports + registry | `R/<Sector>.R` |
| `src/m3t/combine.py` | summary/individual combinations + thermo split (corrected) | `Combine_across_sectors.R` |
| `src/m3t/plotting.py` | sector maps (`log_plot` / `not_log_plot`) | `Plotting_individual_sectors.R` |
| `_data_raw/` | `.rda` → parquet conversion + scraper provenance | `data-raw/*.R` |
| `tests/golden/` | R oracle-capture scripts + committed fixtures | — |
| `notebooks/nyc_demo.py` | worked example | — |
