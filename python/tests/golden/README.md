# Golden fixtures (R oracle)

The R package is the reference implementation. Every sector port is gated on a
golden test that compares against output captured from R here.

## Two kinds of fixtures

1. **`geo`/domain op oracles** — small, deterministic outputs of individual
   `terra` verbs (grid construction, `global`, `aggregate`, `cellSize`,
   `rasterizeGeom`) on toy inputs. Regenerate with
   [`capture_geo_oracle.R`](capture_geo_oracle.R) and
   [`capture_domain_oracle.R`](capture_domain_oracle.R). These validate `m3t.geo`
   against terra cell-for-cell, and are how the terra-parity gotchas in
   `PYTHON_PORT_PLAN.md` §4 were found.

2. **Sector oracles** — one `capture_<sector>_oracle.R` per sector. Each calls the
   *real* R function (internals via `M3T:::`) on a small fixed domain/year and
   exports grid metadata plus full value arrays as JSON. The input fixtures
   (clipped GHGRP/EIA/NLCD subsets) are committed so the Python suite runs
   offline; the raw `.nc`/`.tif` outputs are regenerable and gitignored.

## Workflow

```bash
# from the repo root, with the R M3T package + terra installed
conda run -n M3T Rscript python/tests/golden/capture_geo_oracle.R
conda run -n M3T Rscript python/tests/golden/capture_landfills_oracle.R
# ...
```

Some sector capture scripts need the Zenodo companion archive:
`M3T_DATA=/path/to/M3T_Processed`.

## Orientation gotcha

`terra::writeCDF` stores y **ascending** (CF convention), and terra's read-back of
those files reports an unknown extent and yields a south-to-north array, whereas
rioxarray is north-up. Every capture script therefore flips explicitly so the
exported `values` are canonical **top-left-first (north-up)**. If a comparison
shows matching sums but transposed cells, this is why.

## Tolerance

- Grid definition (CRS, extent, resolution, shape, NA mask): **exact**.
- Cell values: **relative < 1e-4**, with an absolute floor for near-zero cells.
- Any op needing a looser bound must be documented with the reason — currently
  only per-cell geodesic line length (`rasterize_line_length`, 1e-3), though it
  actually agrees with terra to ~6e-8.
