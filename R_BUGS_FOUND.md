# Bugs found in the M3T R package

Found while porting M3T to Python (`python/`). Each Python sector was validated
against an oracle captured from the *real* R function on a small domain, so these
surfaced as disagreements between the two, or as R failing outright.

Nothing here has been changed in the R. The Python port reproduces the R's
behaviour wherever it is merely *different* (so parity stays testable), and departs
from it only where noted — currently one place: mass conservation (**B5**), behind
`Config.Mass_conserving_regrid`, default on.

**Four of these are silent**: the run completes, the output looks plausible, and
the numbers are wrong (**B2** stationary combustion, **B3**, **B4**, **B5**). Those
are the ones worth acting on first. **B1** and **B6** fail loudly.

Environment used: R 4.x + terra, in the `M3T` conda env on macOS (arm64). **B1** and
**B2** depend on how terra/GDAL was built and may not reproduce everywhere — but
they cannot be ruled out by inspection, only by running the check given for each.

---

## B1 — ACES NetCDFs are not georeferenced; `Use_ACES = TRUE` fails

**Severity: medium (loud failure — the run stops).**
`R/CH4_inventory_build.R:317-320` (ACES load) → `R/Stationary_combustion.R:570`

`terra::rast("ACES_annual_Residential_2017.nc")` reads the **values** correctly
(they sum to 39,061,333, matching an independent xarray read to the last digit) but
carries **no georeferencing at all**: the extent comes back as pixel indices
(`0 4634 0 2908`) and the CRS is empty. terra warns `[rast] unknown extent`.

`CH4_inventory_build.R:317` uses that raster as-is, so the first thing that needs a
CRS blows up:

```
Stationary_combustion.R:570   all_merge_LCC_state <- terra::project(all_merge_state, aces_res)
Error: [project] output crs is not valid
```

So in an environment like this one, **ACES cannot be used at all** — Vulcan
(GeoTIFF) is unaffected. It is a loud failure, not a silent one.

Check:

```r
a <- terra::rast("ACES V2.0/ACES_annual_Residential_2017.nc")
terra::crs(a) == ""     # TRUE -> bug is present
terra::ext(a)           # 0 4634 0 2908 (pixel indices, not metres)
```

Root cause is the GDAL netCDF driver: it is an optional plugin, and it is absent
from this conda build (`rasterio` in the same stack cannot open the files at all).
`conda install -c conda-forge libgdal-netcdf` may be all that is needed.

**A trap if you hand-georeference it.** The obvious workaround — assign
`ext()`/`crs()` from the file's own `geotransform` and `proj4` attributes — is not
enough: terra also presents the rows **flipped** (south-first) relative to the file,
which stores northing descending. Georeferencing without flipping puts the data in
the wrong hemisphere; the CT/RI window comes back all-`NA`. It needs
`flip(r, direction = "vertical")` first. (This bit us: our first R oracle capture
did exactly that and produced an all-zero ACES, which silently disabled the CO₂
proxy in our own golden tests — see `Inventory_based_disaggregation.R:87`, which
falls back to spreading a county's methane evenly when it finds no CO₂. That was
our bug, not M3T's, but the flip is a real hazard for anyone patching around B1.)

Safest fix: convert the NetCDFs to GeoTIFF once and point `Source_ACES` at those
(`python/tests/golden/make_aces_fixtures.py` does this, and validates the result
against Vulcan — the two CO₂ inventories correlate at 0.986 over NYC). A cheap
safety net regardless: refuse to proceed if the chosen CO₂ inventory sums to zero.

---

## B2 — `writeCDF` output cannot be read back; sector totals are flipped or the run dies

**Severity: high (silent in one sector, fatal in another).**
`R/Stationary_combustion.R:843-848`, `R/SOCCR_Wetlands.R:446`

Both sectors build their sector totals by **re-reading NetCDFs they just wrote**.
In this environment terra cannot recover the extent it wrote — the run prints
`[rast] unknown extent`, and the files read back with `ext = [0, ncol, 0, nrow]`.

Two different failures follow:

* **Stationary combustion** — `sum(rast(list.files(...)))` still "works", so
  `Stationary_combustion_sector_{fossil_fuel,wood}_total_*.nc` are written
  **vertically flipped** relative to the subsector rasters they are made of.
  `writeCDF` stores y ascending, so the round trip hands the rows back south-first
  while terra treats row 1 as the top; writing the sum out again flips it a second
  time. The giveaway is that the totals have the **correct sum** but a mirrored
  spatial pattern — a rearrangement, not a miscalculation (comparing against a
  correctly-oriented total gives a median per-cell error of 0.86 with identical
  totals).

* **Wetlands** — `SOCCR_Wetlands` **crashes**:
  `Error ... in selecting a method for function 'writeCDF': [+] extents do not match`,
  because the re-read Wetcharts raster will not add to the in-memory freshwater
  raster. `Freshwater.nc`, `SOCCR1.nc` and `SOCCR2.nc` are written before this
  point; **no `Wetland_sector_total_*.nc` is produced at all.**

Check:

```r
r <- terra::rast("out/stationary_combustion/stat_comb_res_petr_bystate_aces.nc")
terra::ext(r)   # [0, 26, 0, 11] -> bug is present (should be the domain extent)
```

Suggested fix: sum the rasters **in memory** rather than round-tripping through
disk. (The Python port does, which is why it produces the wetland sector totals R
cannot.)

---

## B3 — `by_domain` means different things depending on where the NEI came from

**Severity: medium (wrong numbers, but only for `by_domain`).**
`R/Stationary_combustion.R:302` vs `:335`

* `Source_NEI_data = "M3T"` (line 302) loads `M3T::NEI_all_years` and filters by
  **year only** — all ~3,244 US counties are kept.
* `Source_NEI_data = "download"` (line 335) requests the API filtered to
  `st_abbrv/in/<state_name_list>` — only the run's states.

So `emiss_frac_domain` (a county's share of the sector's CO₂) is a share of the
**national** total in the packaged path and of the **domain** total in the download
path. Likewise `domain_total_ch4` (`:288`) sums SEDS over *all* states in the
packaged path. Same config, same domain, different `by_domain` answer depending on
the data source.

`by_state` is unaffected — within-state shares don't depend on the other states.

The Python port reproduces the **packaged** path (the one M3T ships) and documents
this in `sectors/stationary_combustion.prepare_nei`.

---

## B4 — Cells on an interior polygon border lose most of their emissions

**Severity: medium (small, systematic loss at internal boundaries).**
`R/SOCCR_Wetlands.R:398-399`, `R/Wastewater.R:656-657, 712-713`,
`R/CH4_inventory_build.R:1210`, `R/Disaggregate_Wetcharts.R:285, 298`,
`R/Natural_Gas_Distribution.R:903`, `R/Prepare_GEPA.R:161`

The idiom is

```r
cover <- terra::extract(x, domain, weights = TRUE, cells = TRUE)
x[cover[, 'cell']] <- x[cover[, 'cell']] * cover[, 'weight']
```

`extract` returns **one row per (polygon, cell)**. When `domain` has more than one
polygon — any multi-state run — a cell straddling the shared border comes back
**twice**, with partial weights that sum to ~1 (e.g. 0.53 and 0.18 for CT/RI).

R's `[<-` with duplicate indices keeps only the **last** write. So that cell is
scaled by one state's partial weight (0.18×) instead of 1×, and ~80% of its
emissions vanish. Verified on a CT+RI domain: 728 duplicated cells, each split
across the two states.

Suggested fix: aggregate the weights per cell before applying them, e.g.
`stats::aggregate(weight ~ cell, cover, sum)`.

---

## B5 — The Vulcan/ACES gridding does not conserve mass (~1–3%)

**Severity: medium (systematic bias, latitude-dependent).**
`R/Stationary_combustion.R:707-708`, `R/Natural_Gas_Distribution.R:913-914`

`Inventory_based_disaggregation` produces **mol/s per inventory pixel**. `save_data`
then does `input <- input * 1000`, commented "convert from mol/km2s to nmol/m2s" —
which quietly asserts that one 1 km Vulcan/ACES pixel *is* exactly 1 km².

It is not. Both grids are **Lambert Conformal Conic — conformal, not equal-area**.
A "1 km" pixel is **1.00909 km²** at CT/RI's latitude, so the flux is overstated
by ~0.9% before anything else. `save_data` then area-*averages* the flux onto the
run grid and weights cells by their coverage of the domain's **bounding box** —
neither of which conserves a total.

Measured against the county CH₄ totals that fed the grid:

| domain | gridded / county input |
|---|---|
| CT + RI | **+1.4%** |
| NYC | **+2.5%** (stationary combustion), **+1.1%** (NG distribution) |

The bias grows with the conformal scale factor, so it varies across CONUS and is
**not a constant you can divide out**.

This is the one place the Python port deliberately departs from the R: it regrids
the mass conservatively and divides by each cell's true geodesic area, so the
gridded total equals the county totals (verified to ~0.05%). Set
`Config.Mass_conserving_regrid = False` to reproduce the R exactly — the golden
tests do.

---

## B6 — `Wastewater` crashes when a domain has no industrial (subpart II) reporters

**Severity: low (loud failure, easy to hit).**
`R/Wastewater.R:785`

```r
csv_data <- csv_data[, c("facility_id", "facility_name.x", "state", "emiss",
                         "longitude", "latitude")]
# Error in `[.data.frame`: undefined columns selected
```

If no GHGRP subpart II (industrial wastewater) facility falls inside the domain,
`ghgrp_crop` is empty, `as.data.frame(cbind(ghgrp_crop, ghgrp_latlong))` has no
columns, and the column selection fails. Every other part of the sector had already
computed fine.

Reproduced on **CT + RI**, which has no subpart-II reporters at all — hardly an
exotic domain. (This is why the Python golden test uses IA + NE.)

Suggested fix: guard the CSV block with `if (nrow(ghgrp_crop) > 0)`.

---

## B7 — Freshwater loop labels layers with the wrong type vector

**Severity: cosmetic (names only; values unaffected).**
`R/SOCCR_Wetlands.R:289` and `:352`

Inside the **freshwater** loop:

```r
names(subset_data) <- SOCCR_wetland_types[i]     # should be Freshwater_wetland_types[i]
```

`SOCCR_wetland_types` has 4 entries (`M2`, `E2`, `PFO`, `PNF`) but the freshwater
loop runs to 6, so `i = 5, 6` index past the end and the layer is named `NA`. Only
the layer name is affected — the arithmetic uses `Freshwater_wetland_types[i]`
correctly — so no numbers change. Worth fixing before it misleads someone.

---

## Latent / worth knowing (not bugs today)

**Sector-total regex is a character class, not an alternation.**
`R/Stationary_combustion.R:844`:

```r
pattern = "stat_comb_[[:alnum:]]+_[coal|gas|petr]+_bystate_aces"
```

`[coal|gas|petr]` is the *character class* `{c,o,a,l,|,g,s,p,e,t,r}`, not the three
alternatives. It happens to work — `wood` contains `w`/`d`, which are not in the
class, so it is excluded, and coal/gas/petr all match. But any future fuel spelled
only from those letters (e.g. `peat`) would be silently swept into the fossil-fuel
total. Use `_(coal|gas|petr)_`.

**`GHGI_stationary_combustion` carries its year in `rownames`.**
Not an R bug — it works fine in R — but the year exists *only* as a row label, and
`CH4_inventory_build.R:865` selects on it. Any tool that round-trips the table
through a format without row labels (`pyreadr`, `write.csv(row.names = FALSE)`,
parquet, …) silently produces 12 indistinguishable `US_EPA` rows. It cost us a real
bug in the port. A `year` column would be safer.
