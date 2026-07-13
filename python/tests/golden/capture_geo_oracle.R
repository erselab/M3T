#!/usr/bin/env Rscript
# Capture terra outputs for the toy geo ops that m3t.geo must reproduce.
# Run:  Rscript python/tests/golden/capture_geo_oracle.R
# Writes: python/tests/golden/geo_oracle.json
#
# The Python side (tests/test_geo_oracle.py, added in Phase 0/3) loads this JSON
# and asserts m3t.geo matches terra cell-for-cell / within tolerance.

suppressMessages({
  library(terra)
  library(jsonlite)
})

out <- list()

## 1. make_grid: rast(SpatVector, resolution) -- the rule M3T actually uses.
##    Build from a polygon (as domain_template does), NOT a bare SpatExtent,
##    because terra treats those two cases differently (see geo.make_grid docs).
p1 <- as.polygons(ext(-75, -72, 39, 42), crs = "epsg:4326")
g <- rast(p1, resolution = 1, crs = "epsg:4326")
out$make_grid_3x3 <- list(
  nrow = nrow(g), ncol = ncol(g),
  ext = as.vector(ext(g)),         # xmin, xmax, ymin, ymax
  res = as.vector(res(g))
)

# non-integer width -> resolution preserved, cell count rounded, min anchored
p2 <- as.polygons(ext(-75, -71.5, 39, 42), crs = "epsg:4326")
g2 <- rast(p2, resolution = 1, crs = "epsg:4326")
out$make_grid_snap <- list(
  nrow = nrow(g2), ncol = ncol(g2),
  ext = as.vector(ext(g2)), res = as.vector(res(g2))
)

## 2. global reductions ------------------------------------------------------
vals <- rast(ext(0, 2, 0, 2), resolution = 1, crs = "epsg:32618")
values(vals) <- c(1, 2, 3, NA)   # terra fills by row, top-left first
out$global <- list(
  sum = global(vals, "sum", na.rm = TRUE)[1, 1],
  max = global(vals, "max", na.rm = TRUE)[1, 1],
  notNA = global(vals, "notNA")[1, 1]
)

## 3. aggregate(sum) conserves total ----------------------------------------
big <- rast(ext(0, 4, 0, 4), resolution = 1, crs = "epsg:32618")
values(big) <- 1
agg <- aggregate(big, fact = 2, fun = "sum")
out$aggregate <- list(
  nrow = nrow(agg), ncol = ncol(agg),
  res = as.vector(res(agg)),
  total = global(agg, "sum", na.rm = TRUE)[1, 1]
)

## 4. cellSize (projected metres) -------------------------------------------
cs <- rast(ext(0, 1000, 0, 1000), resolution = 500, crs = "epsg:32618")
out$cell_area_m <- values(cellSize(cs, unit = "m"))[1, 1]

dir.create("python/tests/golden", showWarnings = FALSE, recursive = TRUE)
write_json(out, "python/tests/golden/geo_oracle.json",
           auto_unbox = TRUE, digits = 12, pretty = TRUE)
cat("wrote python/tests/golden/geo_oracle.json\n")
