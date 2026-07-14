# M3T: R → Python Port — Plan & Status

The Python port lives in [`python/`](python/). This document is the reference for
*why* it is built the way it is: what's done, what's left, the terra-parity
gotchas we had to pin down, and the places we deliberately diverge from R.

For install/test/layout, see [`python/README.md`](python/README.md).
For defects found in the R implementation, see
[`R_BUGS_FOUND.md`](R_BUGS_FOUND.md) — the port departs from R in exactly the
three places recorded there: mass conservation (**B5**), `Combine_across_sectors`
(**B8**–**B10**), and one plotting label (**B11**). Everywhere else it reproduces
R, bugs included, so parity stays testable.

---

## 1. Status at a glance

**All seven sectors are ported and golden-tested cell-for-cell against the R
functions.** The pipeline runs end-to-end through `ch4_inventory_build`.

| Area | State |
|---|---|
| Config (`config.py`) | ✅ 79 options — R's 78 plus `Mass_conserving_regrid` (see §5) |
| Geo shim (`geo.py`) | ✅ ~25 terra-parity verbs, oracle-tested against terra |
| Packaged data (19 datasets) | ✅ converted from `.rda`, validated against R-computed ground truth |
| Domain builder | ✅ box / CONUS / file / vector / state-Tigerline selection |
| Orchestrator + validation | ✅ dispatch, run-settings, accumulated config errors |
| **All 7 sectors** | ✅ each with an R oracle-capture script + golden test |
| **`Combine_across_sectors`** | ✅ ported — **corrected**; R's version is buggy (see §5b) |
| **Plotting** | ✅ ported (`log_plot` / `not_log_plot`, viridis, `verbose=True`) |
| Packaging / docs | ◐ installable + tested; no PyPI/conda release |

**Tests:** 202 passing (1 network-marked, deselected by default), ruff clean.
**Size:** 5,714 lines of Python `src/` against 10,282 lines of R.

### Sectors

| Sector | Module | R origin |
|---|---|---|
| Municipal solid waste (landfills) | `sectors/landfills.py` | `Municipal_solid_waste.R` |
| Natural gas transmission | `sectors/ng_transmission.py` | `Natural_Gas_Transmission.R` |
| Wastewater | `sectors/wastewater.py` | `Wastewater.R` |
| Stationary combustion | `sectors/stationary_combustion.py` | `Stationary_combustion.R` |
| Natural gas distribution | `sectors/ng_distribution.py` | `Natural_Gas_Distribution.R` |
| Wetlands & inland waters | `sectors/wetlands.py` | `SOCCR_Wetlands.R`, `Disaggregate_Wetcharts.R`, `NWI_*`, `NLCD_*` |
| Remaining sectors (gridded EPA) | `sectors/remaining_gepa.py` | `Prepare_GEPA.R` |

Sectors register over stub placeholders at import (`sectors/__init__.py`); the
registry currently contains **no stubs**.

---

## 2. Architecture (what we ported)

| Layer | R location | LOC | Python |
|---|---|---|---|
| Orchestrator | `R/CH4_inventory_build.R` | ~1,460 | `inventory.py`, `shared_data.py`, `validation.py`, `domain.py` |
| Config | `R/M3T_config.R` (+ docs) | ~770 | `config.py` |
| Sectors | 9 sector/helper files | ~5,000 | `sectors/*.py` |
| Disaggregation | `Inventory_based_disaggregation.R`, `Prepare_GEPA.R` | ~380 | `disaggregation.py` |
| Combine | `Combine_across_sectors.R` | 441 | `combine.py` (corrected — see §5b) |
| Plotting | `Plotting_individual_sectors.R` | 336 | `plotting.py` |
| Utils | `utils.R` | ~140 | `download.py`, `geo.py` |
| Packaged data | `data/*.rda` (19) | — | `m3t/data/*.parquet|json` + `datasets.py` |

**Data flow:** config → load shared inputs (`shared_data.py`) → build target grid
(`domain.py`) → each enabled sector writes a raster to `out/` → `combine.py` sums
them.

Two design changes from R, both deliberate:

* **No global mutable config.** R mutates a `M3T_config` environment mid-run and
  restores it via `on.exit`. Python passes an explicit `Config` dataclass, copied
  per run — a run can never mutate the caller's config (pinned by test).
* **No giant sector argument lists.** R's sector functions take ~30 positional
  args. Python threads a single `RunContext` (config, dirs, grid, domain, shared
  inputs) through every sector.

---

## 3. Dependency mapping (R → Python)

| R | Python |
|---|---|
| **`terra`** (SpatRaster/SpatVector) — 500+ calls, the core | **`geo.py`**: `rioxarray`/`xarray`/`rasterio` for rasters, `geopandas`/`shapely` for vectors, `pyproj` for CRS + geodesic measurement |
| `ncdf4`, `terra::writeCDF` | `xarray.to_netcdf` + `netCDF4` |
| `readxl` | `pandas.read_excel` (`openpyxl`) |
| `dplyr` (only `setdiff`) | `pandas` / stdlib |
| `jsonlite`, `curl` | `requests`, stdlib `json` |
| `magrittr`/`rlang` (pipe, env) | native Python; config is a dataclass |
| `.rda` packaged data | parquet (tables) + JSON (nested/matrix), via `datasets.py` |

---

## 4. terra parity: the gotchas we had to pin down

These are the findings that cost real debugging time. They are enforced by
oracle tests (`tests/test_geo_oracle.py`, `tests/test_domain_oracle.py`) that
compare against terra directly, so a regression fails loudly.

**1. `rast(x, resolution=r)` behaves differently depending on `x`.**
For a **SpatVector/SpatRaster** (what M3T actually uses to build
`domain_template`), terra **preserves the resolution** and re-fits the extent:
it anchors `(xmin, ymin)`, computes `ncol = round(width/xres)` — **`round`, not
`ceil`, so the grid need not cover the whole box** — and recomputes `xmax`/`ymax`.
A non-grid-aligned minimum is kept as-is. This is what `geo.make_grid`
implements. (For a bare **SpatExtent**, terra does the opposite: preserves the
extent and changes the resolution. M3T doesn't rely on that variant.)
This matters because real state-polygon domains have arbitrary float bounds.

**2. `cellSize` is geodesic, not planar — even for a projected CRS.**
terra defaults to `transform=TRUE`, so a 500 m UTM cell is **248,658.818 m²**,
not 250,000. Since M3T divides by cell area on every mass→flux conversion, a
planar area would bias every sector's flux by ~0.5%. `geo.cell_area` reproduces
terra exactly via `pyproj` geodesic polygon area.

**3. `rasterizeGeom(fun="length")` is geodesic too.** `geo.rasterize_line_length`
clips lines to each cell and sums geodesic lengths; it matches terra to a max
relative error of ~6e-8.

**4. `writeCDF` stores y ascending (CF convention).** On read-back terra reports
an unknown extent and yields a south-to-north array, while rioxarray is north-up.
Sums matched but cells were transposed vertically until the oracle exporters were
normalized to north-up. All `capture_*_oracle.R` scripts now flip explicitly.

**5. Point rasterization needs a sum reducer.** `terra::rasterize(pts, fun=sum)`
sums co-located facilities into a cell; rasterio has no equivalent, hence
`geo.rasterize_points_sum`.

---

## 5. Deliberate divergence #1: mass conservation

`Mass_conserving_regrid` is the only config option that does not exist in R. It
defaults to **`True`** (correct), and the R behaviour is retained behind
`False` so golden tests can still prove parity.

**The R disaggregation does not conserve mass — it inflates gridded totals by
~1.4% relative to the county totals that went in.** Two compounding errors
(documented in `stationary_combustion._to_flux`):

* it treats each 1 km Vulcan/ACES pixel as exactly 1 km² when converting
  "mol/s per pixel" to nmol/m²/s. Those grids are **Lambert Conformal Conic —
  conformal, not equal-area** — so a "1 km" pixel is really ~1.009 km² at
  CT/RI latitude (~0.9% over-statement);
* it then area-*averages* onto the domain grid and weights cells by their
  coverage of the domain's *bounding box*; neither step conserves mass.

`tests/test_mass_conservation.py` asserts both directions: the conserving path
reproduces the county totals exactly, and the R-parity path is **pinned at the
1.014 ratio** so the divergence can never drift silently.

## 5b. Deliberate divergence #2: `Combine_across_sectors` is a corrected port

`R/Combine_across_sectors.R` has three bugs. All were confirmed by running the
real R function on synthetic constant-valued rasters, where every output value is
decodable arithmetic (`tests/golden/capture_combine_oracle.R`). Faithfully
reproducing corrupt output would have no value, so `combine.py` implements the
intended behaviour and `tests/test_combine.py` **pins R's numbers next to ours**
so the divergence stays explicit.

**1. Individual combinations are corrupt.** Stationary combustion is the one
sector whose variation spans two files (fossil fuel + wood). R meant to join
them, but the guard reads `isa(stat_comb_options_filenames[1], "matrix")` — the
`[1]` grabs the first *element* (a string), so the test is always `FALSE` and the
join never happens. The 2×N matrix then flattens column-major inside
`expand.grid`, giving a filename grid with **twice as many rows as the key**; R
iterates over the key's row count and uses only the first half. Net effect: every
combination contains **only one of the two stationary-combustion files**, and
**the key CSV does not describe the raster it names** (combination 17's key says
`Vulcan_bystate`; the raster actually holds `wood_ACES`).

**2. The summary halves stationary combustion.** It takes min/mean/max across the
individual fossil/wood layers rather than across variation *totals* — mean
250,000 where the correct value is 500,000, on the synthetic fixture.

**3. `Separate_thermo` + summary cannot run.** `writeCDF` → `rast` drops the
extent and CRS, so a sector file reads back with extent `0..ncol` and no CRS. That
block seeds its accumulator from `domain_template` (which *has* an extent) and
then adds the read-back layers, so terra raises `[rast] extents do not match`. The
main summary path survives only because it seeds from `set_rast`, which is also
read back and therefore consistently wrong.

(That same `writeCDF` round-trip means **R's own combined outputs carry no
georeferencing**. Python writes via xarray, which preserves coords and CRS.)

---

## 6. What's left

**Packaging.** Installable and tested; no PyPI/conda-forge release, and the
roxygen `man/*.Rd` docs aren't carried over.

Everything else in the R package is ported.

### Known, documented non-ports

Each raises a clear error rather than failing quietly:

* **NG distribution `by_LDC`** — reads a semi-manual prep script that lives
  outside the package.
* **Wastewater CONUS/large-domain septic branch.**
* **Interactive `"custom"` domain drawing** (`terra::draw`/`zoom`) — needs a GUI;
  pass an explicit bbox or vector instead.
* **`Source_State_population_data="download"`** — use `"M3T"` or a CSV path.

Note the **companion archive is wired**, not missing: `Source_*="M3T"` reads from
the run's `in/` directory, which is where you stage the Zenodo companion files
(see `notebooks/nyc_demo.py::prepare_run_dir`). It raises only when a required
file is absent, naming the path.

---

## 7. Testing strategy

The R package is the reference; every sector is gated on a golden test.

* **Golden/oracle tests** — `tests/golden/capture_*_oracle.R` call the *real* R
  functions (including internals via `M3T:::`) on a small fixed domain and export
  grid metadata + full value arrays. Python must reproduce them.
* **Grid-definition first** — CRS, extent, resolution, registration and NA mask
  must match exactly before values are compared.
* **Per-operation geo tests** — each terra shim is checked against terra on toy
  inputs (`test_geo_oracle.py`), which is how the §4 gotchas were caught.
* **Tabular parity** — packaged datasets validated against sums/shapes R computed
  itself (non-circular).
* **Mass conservation** — asserted independently of R (§5).
* **Tolerance** — grid definition exact; cell values relative < 1e-4 (looser only
  where documented, e.g. 1e-3 for per-cell geodesic line length).

Fixtures are small clipped subsets committed under `tests/golden/`, so the suite
runs offline without the companion archive.

---

## 8. Open risks

1. **Performance** — terra is C++-backed; CONUS at fine resolution has not been
   benchmarked. `geo.cell_area` still loops per-cell for projected CRSs
   (row-invariant fast path only for geographic). May need vectorizing or `dask`.
2. **Live external sources** — EPA/EIA/Census/PHMSA/ArcGIS URLs and schemas drift;
   the `"download"` paths will rot. The companion archive is the stable path.
3. **Upstream data drift** — the packaged datasets were converted from the `.rda`
   rather than re-scraped, precisely so Python reproduces R exactly. Re-scraping
   could legitimately return different numbers.
