# M3T: R → Python Port Plan

## 1. Goal & scope

Port the **Modular Methane Mapping Tool (M3T)** from an R package to an installable
Python package (`m3t`) that reproduces `CH4_inventory_build()` output (gridded, sectoral
CH₄ emissions as NetCDF/GeoTIFF) within numerical tolerance.

Success criterion: for a fixed domain/year/config (e.g. `CONUS` and `c("CT","RI")` at 1°,
2019), the Python output rasters match the R output within a small relative tolerance
(target: max relative cell difference < 1e-4 on populated cells, exact match on grid
definition/CRS/extent).

## 2. Current architecture (what we're porting)

| Layer | R location | LOC | Notes |
|---|---|---|---|
| Orchestrator | `R/CH4_inventory_build.R` | ~1,460 | Directory setup, downloads, domain build, sector dispatch, combine |
| Config | `R/M3T_config.R`, `R/M3T_config_documentation.R` | ~770 | 80 options in a global env `M3T_config` + get/set |
| Domain | `R/Define_custom_domain.R` | 73 | Interactive/box/Census-based domain construction |
| Sectors | `R/Stationary_combustion.R`, `R/Natural_Gas_Distribution.R`, `R/Natural_Gas_Transmission.R`, `R/Wastewater.R`, `R/Municipal_solid_waste.R`, `R/SOCCR_Wetlands.R`, `R/Disaggregate_Wetcharts.R`, `R/NWI_Wetland_fraction.R`, `R/NLCD_fractions_by_state.R` | ~5,000 | The bulk of the science |
| Disaggregation helpers | `R/Inventory_based_disaggregation.R`, `R/Prepare_GEPA.R` | ~380 | Downscaling activity data to grid |
| Combine | `R/Combine_across_sectors.R` | 441 | Sum sectors onto common grid, write output |
| Plotting | `R/Plotting_individual_sectors.R` | 336 | `log_plot`, `not_log_plot`, per-sector visuals |
| Utils | `R/utils.R`, `R/utils-pipe.R` | ~140 | Downloader, NetCDF writer, GHGRP normalizer |
| Packaged data | `data/*.rda` (19) built by `data-raw/*.R` (15) | — | 3.4 MB of reference tables scraped from EPA/EIA/Census/PHMSA |

**Data flow:** config flags → download raw data (Zenodo companion + live EPA/EIA/Census/PHMSA/FWS) →
build target grid from domain → each enabled sector produces a raster on that grid →
`Combine_across_sectors` sums and writes NetCDF/GeoTIFF, plus optional plots.

External data hosts the code reaches: `zenodo.org`, `epa.gov`/`data.epa.gov`/`enviro.epa.gov`/`echo.epa.gov`,
`census.gov`/`www2.census.gov`, `eia.gov`, `phmsa.dot.gov`, `fws.gov`, `mrlc.gov`, `ornl.gov` (Wetcharts), arcgis services.

## 3. Dependency mapping (R → Python)

| R package | Usage | Python replacement |
|---|---|---|
| **`terra`** (SpatRaster/SpatVector) | 500+ calls — the core | **Rasters:** `rioxarray` + `xarray` + `rasterio` (closest to `terra`'s labeled multi-layer rasters). **Vectors:** `geopandas` + `shapely`. **Reproj/CRS:** `pyproj` / `rasterio.warp`. |
| `terra::project/crop/mask/extend/aggregate/disagg/resample` | raster algebra | `rioxarray.reproject`, `rio.clip`, `.rio.pad_box`, `rasterio` warp/`rasterstats` |
| `terra::extract/rasterize/global/cellSize/zonal` | raster↔vector | `rasterstats.zonal_stats`, `rasterio.features.rasterize`, `xarray` reductions, `.rio` area |
| `terra::vect / writeVector / relate / buffer / makeValid` | vector ops | `geopandas` / `shapely` |
| `ncdf4`, `terra::writeCDF` | NetCDF I/O | `xarray` (`to_netcdf`) + `netCDF4` |
| `readxl::read_xlsx/read_excel` | Excel input | `pandas.read_excel` (`openpyxl`) |
| `dplyr` (only `setdiff`) | minimal | `pandas` / stdlib sets |
| `jsonlite`, `curl` | JSON + downloads | `requests`, stdlib `json` |
| `magrittr`/`rlang` (`%>%`, env) | pipes + config env | native Python; config becomes a class/module |
| `.rda` packaged data | reference tables | regenerate as `.parquet`/`.csv`/`.gpkg` shipped in package data |

**Key risk:** `terra` and `rioxarray`/`rasterio` differ in default resampling, cell-registration,
NA handling, and how `global()`/`extract()` weight partial cells. These differences must be
pinned down per-operation with golden tests (see §6).

## 4. Target Python package layout

```
m3t/
  __init__.py
  config.py              # Config dataclass replacing M3T_config env (80 options + docs)
  inventory.py           # ch4_inventory_build()  (orchestrator)
  domain.py              # build domain grid from box/Census/file  (Define_custom_domain)
  download.py            # trycatch_downloader, Zenodo/Vulcan/GHGRP/GHGI fetchers (utils.R)
  geo.py                 # raster/vector helper layer over rioxarray/geopandas (terra shims)
  io_netcdf.py           # write_cdf equivalents
  sectors/
    stationary_combustion.py
    natural_gas_distribution.py
    natural_gas_transmission.py
    wastewater.py
    municipal_solid_waste.py
    wetlands_soccr.py
    wetcharts.py
    nwi_wetland_fraction.py
    nlcd_fractions_by_state.py
  disaggregation.py      # inventory_based_disaggregation, prepare_gepa
  combine.py             # combine_across_sectors
  plotting.py            # log_plot / not_log_plot (matplotlib)
  data/                  # regenerated reference datasets (parquet/gpkg)
  _data_raw/             # scripts that regenerate data/ (port of data-raw/)
tests/
  golden/                # reference outputs captured from the R package
pyproject.toml
```

Config becomes a `Config` dataclass (or a `dataclass` + module singleton) with the same 80
field names; `get_config`/`set_config` kept as thin helpers for parity. Global-env semantics
in R (config mutated mid-run then reset on exit) are replaced by passing an explicit,
possibly-copied `Config` object through the call chain — cleaner and thread-safe.

## 5. Phased execution plan

### Phase 0 — Foundation & harness (before any science)
1. Stand up `pyproject.toml`, package skeleton, `geopandas`/`rioxarray`/`rasterio`/`xarray`/`pyproj`/`pandas`/`requests` deps, CI, and a dev env (conda env mirroring `environment.yaml` for GDAL/PROJ).
2. Build **`geo.py`**, a thin abstraction over rioxarray/geopandas that mirrors the ~15 `terra` verbs actually used (`rast`, `project`, `crop`, `mask`, `extend`, `aggregate`, `disagg`, `global`, `extract`, `rasterize`, `cellSize`, `ext`, `res`, `crs`, `vect`). Nail down registration/resampling/NA conventions here **once** so every sector inherits identical behavior.
3. Port **`config.py`** (all 80 options + docs) and **`download.py`** (`trycatch_downloader`, Zenodo & Vulcan fetchers). These are low-risk and unblock everything.
4. Capture **golden fixtures**: run the R package on a small fixed domain/year for each sector with `verbose`, and save the intermediate + final rasters as the test oracle.

### Phase 1 — Data regeneration (`data-raw/`)
Port the 15 `data-raw/*.R` scripts to `_data_raw/` and regenerate the 19 packaged datasets as
parquet/gpkg. Validate each regenerated table against the values embedded in the `.rda` files
(read the `.rda`s once via R or `pyreadr` to extract ground truth). These are mostly
tabular scrapes (EIA SEDS, Census population, GHGRP, PHMSA, LMOP, CWNS, wastewater septic) —
straightforward `pandas` ports and a good warm-up for the team on the download/parse idioms.

### Phase 2 — Domain & orchestrator scaffold
Port `domain.py` (`Define_custom_domain` + the Census-Tigerline/box/CONUS/file logic in the
first ~700 lines of `CH4_inventory_build.R`) and the orchestrator control flow (directory
setup, run-settings dump, error-checking, sector dispatch) with sectors stubbed. At this point
`ch4_inventory_build()` runs end-to-end producing empty/zero sector rasters on a correct grid —
lets us verify grid/CRS/extent parity independently of the science.

### Phase 3 — Sectors, one at a time (each gated by its golden test)
Recommended order (simplest/most-isolated first):
1. **Municipal solid waste / landfills** (`Municipal_solid_waste.R`) — point-source (GHGRP/LMOP) rasterization; good first real sector.
2. **Natural gas transmission** (`Natural_Gas_Transmission.R`) — HIFLD pipeline + GHGRP.
3. **Wastewater** (`Wastewater.R`, 1,201 LOC) — CWNS/DMR + septic; heavier tabular logic.
4. **Stationary combustion** (`Stationary_combustion.R`, 1,170 LOC) — needs Vulcan/ACES downscaling + `disaggregation.py` + `prepare_gepa`.
5. **Natural gas distribution** (`Natural_Gas_Distribution.R`, 1,474 LOC) — most complex: LDC/state/domain variations, shares downscaling with stationary combustion.
6. **Wetlands & inland waters** (`SOCCR_Wetlands.R`, `Disaggregate_Wetcharts.R`, `NWI_Wetland_fraction.R`, `NLCD_fractions_by_state.R`) — the most raster-intensive (NLCD/NWI fractions, Wetcharts NetCDF).

Each sector is "done" only when its golden test passes within tolerance.

### Phase 4 — Combine, plotting, polish
Port `combine.py` (sum sectors onto common grid + NetCDF/GeoTIFF output with matching
metadata) and `plotting.py` (`log_plot`/`not_log_plot` in matplotlib). Then full end-to-end
golden test for `c("CT","RI")` and a CONUS smoke test.

### Phase 5 — Packaging & docs
`pip`/`conda` install, README/quickstart port (`README.Rmd` → docs), function docstrings from the
roxygen `man/*.Rd`, example run script. Optionally publish to PyPI/conda-forge.

## 6. Testing strategy (the backbone of a faithful port)

- **Golden/oracle tests:** the R package is the reference. Before touching each sector, run R
  on a fixed small domain/year and archive every intermediate raster + the final output. Python
  must reproduce them within tolerance. This is non-negotiable for a scientific port.
- **Grid-definition tests:** exact match of CRS, extent, resolution, cell registration, and NA
  mask — before comparing values.
- **Per-operation `geo.py` tests:** verify each `terra`→rioxarray shim against `terra` on toy
  inputs (especially `extract`/`global`/`aggregate`/`rasterize` partial-cell weighting).
- **Tabular parity:** regenerated `data/` tables vs. the original `.rda` values.
- **Tolerance policy:** define up front (e.g. relative < 1e-4, absolute floor for near-zero
  cells); log and investigate any sector that needs a looser bound.

## 7. Key risks & open questions

1. **`terra` vs `rioxarray` numerical divergence** — the top risk. Resampling defaults, cell
   registration, and area/zonal weighting differ. Mitigation: centralize in `geo.py`, test each
   op against `terra`, pin resampling methods explicitly.
2. **Global mutable config + `on.exit` resets** — R mutates `M3T_config` mid-run and restores it.
   Decide now: pass an explicit `Config` object (recommended) vs. replicate global state.
3. **Live external downloads** — many sources (EPA/EIA/Census/PHMSA/FWS/ORNL/arcgis) can change
   URLs or formats. Cache fixtures for tests; keep the Zenodo companion as the stable path.
4. **Interactive domain drawing** (`terra::draw`/`zoom` in `Define_custom_domain`) — needs a
   matplotlib/ipyleaflet equivalent or a non-interactive fallback (bbox input).
5. **Reading the `.rda` oracle** — need R or `pyreadr`/`rdata` to extract packaged-data ground
   truth for Phase 1 validation.
6. **Performance** — CONUS at high res is heavy; `terra` is C++-backed. `rioxarray`+`dask` may be
   needed for parity on large domains, or accept a first version that's slower.
7. **Numeric-vs-string config values** — several options accept either a number or a keyword
   ("GHGI", "M3T", "download"); Python typing/validation must mirror this leniency.

## 8. Rough effort estimate

- Phase 0: ~1–2 weeks (geo shim + config + downloader + harness).
- Phase 1: ~1–2 weeks (15 data scripts).
- Phase 2: ~1 week.
- Phase 3: ~1–2 weeks per major sector → ~6–10 weeks total.
- Phase 4–5: ~2 weeks.

**Order of attack recommendation:** build `geo.py` + golden-test harness first and prove
grid parity on a tiny domain before porting any science. Everything downstream depends on the
geospatial layer behaving identically to `terra`.
```
