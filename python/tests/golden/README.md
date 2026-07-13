# Golden fixtures (R oracle)

The R package is the reference implementation. Every non-trivial Python port is
checked against outputs captured from R here.

## Two kinds of fixtures

1. **`geo` op oracles** — small, deterministic outputs of individual `terra`
   verbs (grid math, `global`, `aggregate`, `project`, `extract`, `rasterize`)
   on toy inputs. Regenerate with [`capture_geo_oracle.R`](capture_geo_oracle.R).
   These validate `m3t.geo` against `terra` cell-for-cell and are cheap to run.

2. **Sector / end-to-end oracles** — the intermediate and final rasters produced
   by `CH4_inventory_build()` for a small fixed domain/year/config. Captured per
   sector as Phase 3 progresses. Large; store as compressed GeoTIFF/NetCDF and
   keep out of git if they exceed a few MB (see `.gitignore`).

## Workflow

```
# from the repo root, with the R package installed
Rscript python/tests/golden/capture_geo_oracle.R
# -> writes python/tests/golden/geo_oracle.json (+ any .tif fixtures)
```

Python tests then load the JSON/rasters and assert parity within the tolerance
policy defined in `PYTHON_PORT_PLAN.md` §6 (relative < 1e-4 on populated cells,
exact match on grid definition/CRS/extent).

## Tolerance

- Grid definition (CRS, extent, resolution, shape, NA mask): **exact**.
- Cell values: **relative < 1e-4**, with an absolute floor for near-zero cells.
- Any op needing a looser bound must be documented with the reason.
