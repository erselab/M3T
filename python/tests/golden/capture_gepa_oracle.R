#!/usr/bin/env Rscript
# Capture golden output for the remaining-sectors (gridded EPA) sector by calling
# the real M3T:::Prepare_GEPA on a CT+RI domain.
#
# Two domain resolutions on purpose, because Prepare_GEPA branches on which grid is
# coarser and the two branches are completely different code:
#   0.1 deg  -- same as GEPA, so it takes the nearest-neighbour branch
#   0.25 deg -- coarser than GEPA, so it takes the area-average branch (the one
#               place in all of M3T that asks for *exact* extract weights)
#
# Needs the GEPA v2 file; Source_GEPA="download" fetches it from Zenodo record
# 8367082 (3 MB). Point M3T_GEPA at a local copy to skip the download:
#
#   M3T_GEPA=/tmp/gepa_2019.nc \
#     conda run -n M3T Rscript python/tests/golden/capture_gepa_oracle.R

suppressMessages({ library(terra); library(jsonlite); library(M3T) })

gold_dir <- "python/tests/golden/gepa"
in_dir <- file.path(gold_dir, "in")
dir.create(in_dir, recursive = TRUE, showWarnings = FALSE)

gepa_src <- Sys.getenv("M3T_GEPA", "")
stopifnot(nzchar(gepa_src), file.exists(gepa_src))

inventory_year <- 2019
domain_crs <- "epsg:4326"
states <- c("CT", "RI")

# Reuse the CT+RI domain already committed by the wetlands capture -- it is the
# same states from the same Tigerlines vintage, so this needs no companion drive.
domain <- vect("python/tests/golden/wetlands/domain_ct_ri.geojson")
writeVector(domain, file.path(gold_dir, "domain_ct_ri.geojson"),
            filetype = "GeoJSON", overwrite = TRUE)

# The committed GEPA fixture (in/gepa_ctri.nc) is cropped from the full CONUS file
# in *Python*, not here: terra's writeCDF collapses the 28 named sector variables
# into a single 28-layer variable and loses their names, which is exactly what the
# sector selects on. Regenerate it with:
#
#   python -c "import xarray as xr, geopandas as gpd; \
#     d=xr.open_dataset('gepa_2019.nc'); \
#     b=gpd.read_file('python/tests/golden/gepa/domain_ct_ri.geojson').total_bounds; \
#     d.sel(lon=slice(b[0]-2,b[2]+2), lat=slice(b[1]-2,b[3]+2)) \
#      .to_netcdf('python/tests/golden/gepa/in/gepa_ctri.nc')"

rasters <- list()
params <- list()
for (dres in c(0.1, 0.25)) {
  domain_template <- rast(domain, resolution = dres, crs = domain_crs, vals = NA)
  out_dir <- file.path(gold_dir, "out", paste0("res", dres))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  M3T:::Prepare_GEPA(
    input_directory = in_dir,
    output_directory = out_dir,
    Source_GEPA = gepa_src,
    inventory_year = inventory_year,
    domain = domain,
    domain_template = domain_template,
    plot_directory = "",
    County_Tigerlines = NULL,
    State_CB = NULL,
    verbose = FALSE
  )

  for (f in list.files(out_dir, pattern = "[.]nc$", full.names = TRUE)) {
    r <- flip(rast(f), direction = "vertical")
    key <- paste0(sub("[.]nc$", "", basename(f)), "@", dres)
    rasters[[key]] <- list(
      nrow = nrow(r), ncol = ncol(r), res = dres,
      sum = as.numeric(global(r, "sum", na.rm = TRUE)[1, 1]),
      nonzero = as.integer(global(r, function(x) sum(x != 0, na.rm = TRUE))[1, 1]),
      values = as.numeric(values(r)))
  }
}

oracle <- list(
  params = list(inventory_year = inventory_year, domain_states = states,
                domain_ext = as.vector(ext(domain)), domain_crs = domain_crs,
                resolutions = c(0.1, 0.25)),
  rasters = rasters
)
write_json(oracle, file.path(gold_dir, "gepa_oracle.json"),
           auto_unbox = TRUE, digits = 12, na = "null")
cat("wrote oracle with", length(rasters), "rasters\n")
