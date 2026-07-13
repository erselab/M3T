#!/usr/bin/env Rscript
# Capture golden output for the landfills sector by calling the *real* internal
# M3T::Municipal_solid_waste on a small box domain, bypassing the orchestrator
# (state_name_list / tigerlines are unused by this function; verbose=FALSE).
#
# Requires: the M3T R package installed, and a GHGRP facility file at
#   python/tests/golden/landfills/facility_data.csv
# (download once from https://data.epa.gov/dmapservice/ghg.pub_dim_facility/CSV).
#
# Run from the repo root:
#   conda run -n M3T Rscript python/tests/golden/capture_landfills_oracle.R
#
# Writes python/tests/golden/landfills/:
#   *.nc                 (raw sector outputs, gitignored/regenerable)
#   landfills_oracle.json (grid meta + full value arrays + sums; the committed oracle)

suppressMessages({ library(terra); library(jsonlite) })

gold_dir <- "python/tests/golden/landfills"
dir.create(gold_dir, recursive = TRUE, showWarnings = FALSE)
# Filtered to landfill facility_ids + used columns (identical results to the full
# pub_dim_facility table, since the sector inner-joins on landfill facilities).
facility_csv <- file.path(gold_dir, "facility_data_landfills.csv")
if (!file.exists(facility_csv)) {
  stop("missing ", facility_csv,
       " -- build it from ghg.pub_dim_facility CSV (see test_landfills_golden.py)")
}

# --- inputs exactly as the orchestrator prepares them -----------------------
inventory_year <- 2019
GHGI_file_yr <- max(as.numeric(M3T::GHGI_landfill_total_M3T$Year))
GHGI_data_yr <- if (inventory_year > GHGI_file_yr) GHGI_file_yr else inventory_year
GHGI_landfill_total <- M3T::GHGI_landfill_total_M3T$Emissions[
  M3T::GHGI_landfill_total_M3T$Year == GHGI_data_yr]

GHGRP_facility_data <- utils::read.csv(facility_csv)
GHGRP_facility_data$county_fips <- sprintf("%05d", GHGRP_facility_data$county_fips)
GHGRP_facility_data$zip <- sprintf("%05d", GHGRP_facility_data$zip)

# --- small domain (a box over PA/NJ, plenty of landfills) -------------------
domain_res <- 0.1
domain_crs <- "epsg:4326"
domain <- as.polygons(ext(-80.5, -74.7, 39.7, 42.3), crs = domain_crs)
domain_template <- rast(domain, resolution = domain_res, crs = domain_crs, vals = NA)

out_dir <- file.path(gold_dir, "out")
in_dir <- file.path(gold_dir, "in"); dir.create(file.path(in_dir, "GHGRP"), recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, showWarnings = FALSE)

M3T:::Municipal_solid_waste(
  input_directory = in_dir,
  domain = domain,
  domain_template = domain_template,
  state_name_list = NA,
  output_directory = out_dir,
  inventory_year = inventory_year,
  GHGI_data_yr = GHGI_data_yr,
  verbose = FALSE,
  GHGI_landfill_total = GHGI_landfill_total,
  GHGRP_facility_data = GHGRP_facility_data,
  GHGRP_combustion_emissions = M3T::GHGRP_combustion_emissions,
  Source_GHGRP_landfills = "M3T",
  Source_LMOP = "M3T",
  landfill_ghgrp_reported = TRUE,
  landfill_ghgrp_generation_first = TRUE,
  landfill_ghgrp_collection_first = TRUE,
  plot_directory = "",
  County_Tigerlines = NULL,
  State_CB = NULL
)

# --- export each output raster as grid meta + values ------------------------
export_rast <- function(path) {
  r <- rast(path)
  # writeCDF stores y ascending (CF convention); on read-back terra reports an
  # unknown extent and yields a south-to-north array. Flip to north-up so the
  # exported `values` are canonical top-left-first (matching rioxarray/Python).
  r <- flip(r, direction = "vertical")
  list(
    file = basename(path),
    nrow = nrow(r), ncol = ncol(r),
    ext = as.vector(ext(r)),           # xmin, xmax, ymin, ymax
    res = as.vector(res(r)),
    sum = as.numeric(global(r, "sum", na.rm = TRUE)[1, 1]),
    nonzero = as.integer(global(r, function(x) sum(x != 0, na.rm = TRUE))[1, 1]),
    values = as.numeric(values(r))     # row-major, top-left first
  )
}

outputs <- c(
  file.path(out_dir, "Landfills", "MSW_GHGRP_reported.nc"),
  file.path(out_dir, "Landfills", "MSW_GHGRP_generation_first.nc"),
  file.path(out_dir, "Landfills", "MSW_GHGRP_collection_first.nc"),
  file.path(out_dir, "Landfills", "MSW_LMOP.nc"),
  file.path(out_dir, "Landfill_sector_total_GHGRP_reported.nc"),
  file.path(out_dir, "Landfill_sector_total_GHGRP_generation_first.nc"),
  file.path(out_dir, "Landfill_sector_total_GHGRP_collection_first.nc")
)

oracle <- list(
  params = list(inventory_year = inventory_year, GHGI_data_yr = GHGI_data_yr,
                GHGI_landfill_total = GHGI_landfill_total,
                domain_ext = c(-80.5, -74.7, 39.7, 42.3), domain_res = domain_res,
                domain_crs = domain_crs),
  rasters = lapply(outputs, export_rast)
)
names(oracle$rasters) <- basename(outputs)

write_json(oracle, file.path(gold_dir, "landfills_oracle.json"),
           auto_unbox = TRUE, digits = 12, na = "null")
cat("wrote", file.path(gold_dir, "landfills_oracle.json"), "\n")
