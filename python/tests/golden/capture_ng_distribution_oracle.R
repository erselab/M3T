#!/usr/bin/env Rscript
# Capture golden output for the NG distribution sector by calling the real
# M3T:::Natural_Gas_Distribution on a CT+RI domain with ACES (same reasoning as
# the stationary combustion capture: ACES is on the companion drive, Vulcan is a
# large download, and the two follow identical code paths).
#
#   M3T_DATA=/Volumes/Expansion/M3T_Processed \
#     conda run -n M3T Rscript python/tests/golden/capture_ng_distribution_oracle.R
#
# by_LDC is not captured -- it reads the output of a semi-manual prep script that
# ships outside the package, and is off by default.

suppressMessages({ library(terra); library(jsonlite); library(M3T) })

data_dir <- Sys.getenv("M3T_DATA", "/Volumes/Expansion/M3T_Processed")
gold_dir <- "python/tests/golden/ng_distribution"
out_dir <- file.path(gold_dir, "out")
dir.create(file.path(gold_dir, "in", "GHGRP"), recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
stopifnot(dir.exists(data_dir))

inventory_year <- 2017
GHGI_data_yr <- 2017
domain_res <- 0.1
domain_crs <- "epsg:4326"
states <- c("CT", "RI")

State_Tigerlines <- vect(file.path(data_dir, "combined_state_tigerlines.gpkg"),
                         layer = as.character(inventory_year))
if (domain_crs != crs(State_Tigerlines)) State_Tigerlines <- project(State_Tigerlines, domain_crs)
State_Tigerlines <- State_Tigerlines[State_Tigerlines$STUSPS %in% states, ]
State_Tigerlines <- State_Tigerlines[order(State_Tigerlines$STUSPS), ]
state_name_list <- State_Tigerlines$STUSPS

domain <- State_Tigerlines
domain_template <- rast(domain, resolution = domain_res, crs = domain_crs, vals = NA)

# --- GHGRP facility + subpart W (committed fixtures, filtered to NG distribution)
facility_csv <- file.path(gold_dir, "facility_data_ngdist.csv")
subw_csv <- file.path(gold_dir, "subpartW_ngdist.csv")
if (!file.exists(subw_csv)) {
  w <- utils::read.csv(file.path(data_dir, "GHGRP", "Oil_and_gas_W.csv"))
  w <- w[w$industry_segment == "Natural gas distribution [98.230(a)(8)]", ]
  utils::write.csv(w, subw_csv, row.names = FALSE)
}
GHGRP_subpartW_emissions <- utils::read.csv(subw_csv)

if (!file.exists(facility_csv)) {
  fac <- utils::read.csv(file.path(data_dir, "GHGRP", "facility_data.csv"))
  keep <- fac$facility_id %in% GHGRP_subpartW_emissions$facility_id
  cols <- c("facility_id", "year", "facility_name", "latitude", "longitude",
            "state", "state_name")
  utils::write.csv(fac[keep, cols], facility_csv, row.names = FALSE)
}
GHGRP_facility_data <- utils::read.csv(facility_csv)

# --- ACES: read the committed GeoTIFF fixtures, NOT the NetCDFs ---------------
# terra reads the ACES .nc files as ALL ZEROS in this environment (right shape,
# every value 0). That silently disables the CO2 proxy: the disaggregation sees a
# county with no CO2 and falls back to spreading its methane evenly, so the oracle
# would agree with Python while neither had used ACES at all. The fixtures are built
# from the NetCDFs by tests/golden/make_aces_fixtures.py (Python/xarray reads them
# correctly) and are what both sides read.
load_aces <- function(sector) rast(file.path(gold_dir, paste0("aces_", sector, "_ctri.tif")))
aces_res <- load_aces("res")
aces_com <- load_aces("com")
stopifnot(as.numeric(global(aces_res, "sum", na.rm = TRUE)[1, 1]) > 0)

writeVector(State_Tigerlines[, c("STUSPS", "NAME", "STATEFP")],
            file.path(gold_dir, "domain_ct_ri.geojson"),
            filetype = "GeoJSON", overwrite = TRUE)

# --- the GHGI tables the orchestrator resolves from the "GHGI" keyword ---------
cfg <- M3T:::M3T_config
GHGI_MnR <- data.frame(
  "Type" = rownames(M3T::GHGI_NG_distribution$GHGI_MnR_Activity),
  "EF" = M3T::GHGI_NG_distribution$GHGI_MnR_EF[, as.character(GHGI_data_yr)],
  "Total_stations" = M3T::GHGI_NG_distribution$GHGI_MnR_Activity[, as.character(GHGI_data_yr)])
GHGI_maintenance <- data.frame(
  "Type" = rownames(M3T::GHGI_NG_distribution$GHGI_maintenance),
  "EF" = M3T::GHGI_NG_distribution$GHGI_maintenance[, as.character(GHGI_data_yr)])
GHGI_meters <- data.frame(
  "Type" = rownames(M3T::GHGI_NG_distribution$GHGI_meters),
  "EF" = M3T::GHGI_NG_distribution$GHGI_meters[, as.character(GHGI_data_yr)])
GHGI_services <- data.frame(
  "Type" = rownames(M3T::GHGI_NG_distribution$GHGI_services),
  "EF" = M3T::GHGI_NG_distribution$GHGI_services[, as.character(GHGI_data_yr)])

M3T:::Natural_Gas_Distribution(
  domain = domain,
  domain_template = domain_template,
  state_name_list = state_name_list,
  input_directory = file.path(gold_dir, "in"),
  output_directory = out_dir,
  inventory_year = inventory_year,
  GHGI_data_yr = GHGI_data_yr,
  verbose = FALSE,
  GHGRP_facility_data = GHGRP_facility_data,
  GHGRP_subpartW_emissions = GHGRP_subpartW_emissions,
  Source_EIA_NG_file = "M3T",
  Source_PHMSA_file = "M3T",
  Source_GHGRP_LDC = "M3T",
  GHGI_MnR = GHGI_MnR,
  GHGI_maintenance = GHGI_maintenance,
  GHGI_meters = GHGI_meters,
  GHGI_services = GHGI_services,
  State_Tigerlines = State_Tigerlines,
  NG_distribution_by_LDC = FALSE,
  NG_distribution_by_state = TRUE,
  NG_distribution_by_domain = TRUE,
  Source_byLDC_file = "",
  natural_gas_pipeline_emission_factors = cfg$natural_gas_pipeline_emission_factors,
  natural_gas_res_post_meter_emission_factor = cfg$natural_gas_res_post_meter_emission_factor,
  natural_gas_com_post_meter_emission_factor = cfg$natural_gas_com_post_meter_emission_factor,
  Use_ACES = TRUE,
  Use_Vulcan = FALSE,
  aces_res = aces_res, aces_com = aces_com,
  vu_res = NULL, vu_com = NULL,
  plot_directory = "",
  County_Tigerlines = NULL,
  State_CB = NULL
)

export_rast <- function(path) {
  r <- flip(rast(path), direction = "vertical")
  list(file = basename(path), nrow = nrow(r), ncol = ncol(r),
       ext = as.vector(ext(r)), res = as.vector(res(r)),
       sum = as.numeric(global(r, "sum", na.rm = TRUE)[1, 1]),
       nonzero = as.integer(global(r, function(x) sum(x != 0, na.rm = TRUE))[1, 1]),
       values = as.numeric(values(r)))
}

outputs <- list.files(file.path(out_dir, "NG_distribution"), pattern = "[.]nc$",
                      full.names = TRUE)
oracle <- list(
  params = list(inventory_year = inventory_year, GHGI_data_yr = GHGI_data_yr,
                domain_states = states, domain_ext = as.vector(ext(domain)),
                domain_res = domain_res, domain_crs = domain_crs),
  rasters = lapply(outputs, export_rast)
)
names(oracle$rasters) <- basename(outputs)
write_json(oracle, file.path(gold_dir, "ng_dist_oracle.json"),
           auto_unbox = TRUE, digits = 12, na = "null")
cat("wrote oracle with", length(outputs), "rasters\n")
