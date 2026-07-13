#!/usr/bin/env Rscript
# Reproduce the R domain_template construction from CH4_inventory_build.R for the
# domain forms that don't need Census vectors, so the Python domain.build_domain
# can be checked for grid parity.
# Run:  conda run -n M3T Rscript python/tests/golden/capture_domain_oracle.R

suppressMessages({ library(terra); library(jsonlite) })

domain_crs <- "epsg:4326"

# Faithful copy of the R box/CONUS pipeline: build a rast with explicit extent,
# take its extent as a polygon, then rast(poly, resolution) -> template.
build_template <- function(xrange, yrange, res) {
  res <- rep(res, length.out = 2)
  d <- rast(nrows = diff(range(yrange)) / res[2],
            ncols = diff(range(xrange)) / res[1],
            xmin = min(xrange), xmax = max(xrange),
            ymin = min(yrange), ymax = max(yrange), vals = 1)
  poly <- as.polygons(ext(d), crs = domain_crs)
  tmpl <- rast(poly, resolution = res, crs = domain_crs, vals = NA)
  list(nrow = nrow(tmpl), ncol = ncol(tmpl),
       ext = as.vector(ext(tmpl)),        # xmin, xmax, ymin, ymax
       res = as.vector(res(tmpl)))
}

out <- list(
  box_int    = build_template(c(-75, -72), c(39, 42), 1),
  box_noninteger = build_template(c(-75, -71.5), c(39, 42), 1),
  conus_1deg = build_template(c(-130, -60), c(20, 55), 1),
  box_halfdeg = build_template(c(-83, -80), c(40, 42), 0.5),
  # discriminating cases: width 3.2 (ceil=4 vs round=3) and non-grid-aligned min
  box_width_3p2 = build_template(c(-75, -71.8), c(39, 42), 1),
  box_offset_min = build_template(c(-74.3, -71.1), c(39.2, 41.9), 1)
)

dir.create("python/tests/golden", showWarnings = FALSE, recursive = TRUE)
write_json(out, "python/tests/golden/domain_oracle.json",
           auto_unbox = TRUE, digits = 12, pretty = TRUE)
cat("wrote python/tests/golden/domain_oracle.json\n")
