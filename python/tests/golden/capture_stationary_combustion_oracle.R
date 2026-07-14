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

# --- ACES: read the committed GeoTIFF fixtures, NOT the NetCDFs ---------------
# terra reads the ACES .nc files as ALL ZEROS in this environment (right shape,
# every value 0). That silently disables the CO2 proxy: the disaggregation sees a
# county with no CO2 and falls back to spreading its methane evenly, so the oracle
# would agree with Python while neither had used ACES at all. The fixtures are built
# from the NetCDFs by tests/golden/make_aces_fixtures.py (Python/xarray reads them
# correctly) and are what both sides read.
load_aces <- function(sector) rast(file.path(gold_dir, paste0("aces_", sector, "_ctri.tif")))
aces_res  <- load_aces("res")
aces_com  <- load_aces("com")
aces_ind  <- load_aces("ind")
aces_elec <- load_aces("elec")
for (nm in c("res", "com", "ind", "elec")) {
  r <- get(paste0("aces_", nm))
  stopifnot(as.numeric(global(r, "sum", na.rm = TRUE)[1, 1]) > 0)
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
