#!/usr/bin/env Rscript
# Capture golden output for the stationary combustion sector by calling the real
# M3T:::Stationary_combustion on a CT+RI domain.
#
# Uses ACES (Use_ACES) rather than the config default Vulcan: ACES is on the
# companion drive already, whereas raw Vulcan is a large Zenodo download, and the
# two follow identical code paths (the inventory is only a spatial proxy).
# inventory_year 2017 = the newest year ACES, SEDS and the GHGI all cover.
#
#   M3T_DATA=/Volumes/Expansion/M3T_Processed \
#     conda run -n M3T Rscript python/tests/golden/capture_stationary_combustion_oracle.R
#
# Writes python/tests/golden/stationary_combustion/:
#   out/**.nc                          raw sector outputs
#   stat_comb_oracle.json              grid meta + values + sums (the oracle)
#   aces_<sector>_ctri.tif             clipped ACES fixtures (committed)
#   counties_ctri.geojson              county polygons (committed)

suppressMessages({ library(terra); library(jsonlite); library(M3T) })

data_dir <- Sys.getenv("M3T_DATA", "/Volumes/Expansion/M3T_Processed")
gold_dir <- "python/tests/golden/stationary_combustion"
out_dir <- file.path(gold_dir, "out")
dir.create(file.path(gold_dir, "in", "NEI"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(gold_dir, "in", "EIA"), recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
stopifnot(dir.exists(data_dir))

inventory_year <- 2017
GHGI_data_yr <- 2017
domain_res <- 0.1
domain_crs <- "epsg:4326"
states <- c("CT", "RI")

# --- domain + tigerlines, as the orchestrator derives them -------------------
State_Tigerlines <- vect(file.path(data_dir, "combined_state_tigerlines.gpkg"),
                         layer = as.character(inventory_year))
if (domain_crs != crs(State_Tigerlines)) State_Tigerlines <- project(State_Tigerlines, domain_crs)
State_Tigerlines <- State_Tigerlines[State_Tigerlines$STUSPS %in% states, ]
State_Tigerlines <- State_Tigerlines[order(State_Tigerlines$STUSPS), ]
state_name_list <- State_Tigerlines$STUSPS

County_Tigerlines <- vect(file.path(data_dir, "combined_county_tigerlines.gpkg"),
                          layer = as.character(inventory_year))
if (domain_crs != crs(County_Tigerlines)) County_Tigerlines <- project(County_Tigerlines, domain_crs)
County_Tigerlines <- County_Tigerlines[County_Tigerlines$STATEFP %in% State_Tigerlines$STATEFP, ]

domain <- State_Tigerlines
domain_template <- rast(domain, resolution = domain_res, crs = domain_crs, vals = NA)

writeVector(County_Tigerlines[, c("STATEFP", "COUNTYFP", "NAME")],
            file.path(gold_dir, "counties_ctri.geojson"),
            filetype = "GeoJSON", overwrite = TRUE)

# --- ACES: one raster per sector, cropped to the domain ----------------------
# The ACES NetCDFs put their georeferencing in a `crs` variable (proj4 +
# geotransform attributes) that this GDAL/terra build does not pick up -- a plain
# rast() yields extent 0..ncol and no CRS. Apply the file's own values explicitly;
# python/src/m3t/shared_data.py:load_aces does exactly the same.
ACES_PROJ <- "+proj=lcc +lat_0=40 +lon_0=-97 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
ACES_ORIGIN <- c(-2300000, 1300000)  # top-left x, y; 1000 m cells
aces_dir <- file.path(data_dir, "ACES V2.0")
load_aces <- function(sector) {
  r <- rast(file.path(aces_dir, paste0("ACES_annual_", sector, "_", inventory_year, ".nc")),
            subds = "flux_co2")
  ext(r) <- ext(ACES_ORIGIN[1], ACES_ORIGIN[1] + ncol(r) * 1000,
                ACES_ORIGIN[2] - nrow(r) * 1000, ACES_ORIGIN[2])
  crs(r) <- ACES_PROJ
  r
}
aces_res  <- load_aces("Residential")
aces_com  <- load_aces("Commercial")
aces_ind  <- load_aces("Industrial")
aces_elec <- load_aces("Elec")

# committed fixtures: clip to the domain (+ generous margin for the zero-buffer)
dom_aces <- project(domain, crs(aces_res))
clip_ext <- ext(dom_aces) + 60000
for (nm in c("res", "com", "ind", "elec")) {
  r <- get(paste0("aces_", nm))
  writeRaster(crop(r, clip_ext, snap = "out"),
              file.path(gold_dir, paste0("aces_", nm, "_ctri.tif")),
              overwrite = TRUE, datatype = "FLT8S")
}

M3T:::Stationary_combustion(
  input_directory = file.path(gold_dir, "in"),
  output_directory = out_dir,
  inventory_year = inventory_year,
  GHGI_data_yr = GHGI_data_yr,
  domain = domain,
  domain_template = domain_template,
  State_Tigerlines = State_Tigerlines,
  County_Tigerlines = County_Tigerlines,
  state_name_list = state_name_list,
  verbose = FALSE,
  Use_ACES = TRUE,
  Use_Vulcan = FALSE,
  aces_res = aces_res, aces_com = aces_com, aces_ind = aces_ind, aces_elec = aces_elec,
  vu_res = NULL, vu_com = NULL, vu_ind = NULL, vu_elec = NULL,
  stationary_combustion_GHGI_data =
    M3T::GHGI_stationary_combustion[rownames(M3T::GHGI_stationary_combustion) ==
                                      as.character(GHGI_data_yr), ],
  stationary_combustion_by_state = TRUE,
  stationary_combustion_by_domain = TRUE,
  stationary_combustion_emission_factors = M3T:::M3T_config$stationary_combustion_emission_factors,
  Source_EIA_SEDS_data = "M3T",
  Source_NEI_data = "M3T",
  plot_directory = "",
  State_CB = NULL
)

# --- export ------------------------------------------------------------------
export_rast <- function(path) {
  r <- flip(rast(path), direction = "vertical")
  list(
    file = basename(path),
    nrow = nrow(r), ncol = ncol(r),
    ext = as.vector(ext(r)), res = as.vector(res(r)),
    sum = as.numeric(global(r, "sum", na.rm = TRUE)[1, 1]),
    nonzero = as.integer(global(r, function(x) sum(x != 0, na.rm = TRUE))[1, 1]),
    values = as.numeric(values(r))
  )
}

outputs <- c(
  list.files(file.path(out_dir, "stationary_combustion"), pattern = "[.]nc$", full.names = TRUE),
  list.files(out_dir, pattern = "^Stationary_combustion_sector.*[.]nc$", full.names = TRUE)
)

oracle <- list(
  params = list(inventory_year = inventory_year, GHGI_data_yr = GHGI_data_yr,
                domain_states = states, domain_ext = as.vector(ext(domain)),
                domain_res = domain_res, domain_crs = domain_crs),
  rasters = lapply(outputs, export_rast)
)
names(oracle$rasters) <- basename(outputs)

write_json(oracle, file.path(gold_dir, "stat_comb_oracle.json"),
           auto_unbox = TRUE, digits = 12, na = "null")
cat("wrote oracle with", length(outputs), "rasters\n")
