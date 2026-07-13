#!/usr/bin/env Rscript
# Capture golden output for the NG transmission sector by calling the real
# internal M3T::Natural_Gas_Transmission on a small PA box, bypassing the
# orchestrator (state_name_list unused; verbose=FALSE).
#
# Fixtures (committed, in python/tests/golden/ng_transmission/):
#   subpartW_ngtrans.csv        GHGRP subpart W, 2019, transmission+processing
#   facility_data_ngtrans.csv   GHGRP facility locations for those facilities
#   eia_pipes_pa_all.geojson    EIA pipelines intersecting the domain bbox
#
# Run from repo root:
#   conda run -n M3T Rscript python/tests/golden/capture_ng_transmission_oracle.R

suppressMessages({ library(terra); library(jsonlite) })

gold <- "python/tests/golden/ng_transmission"
inv_year <- 2019
GHGI_file_yr <- max(as.numeric(M3T::GHGI_landfill_total_M3T$Year))
GHGI_data_yr <- if (inv_year > GHGI_file_yr) GHGI_file_yr else inv_year
yr <- as.character(GHGI_data_yr)

# GHGI EF frames exactly as the orchestrator builds them
GHGI_Pipeline <- data.frame(
  Type = rownames(M3T::GHGI_NG_transmission$GHGI_Pipeline_Activity),
  Emissions = M3T::GHGI_NG_transmission$GHGI_Pipeline_Emissions[, yr],
  Total_stations = M3T::GHGI_NG_transmission$GHGI_Pipeline_Activity[, yr])
GHGI_transmission_compressors <- data.frame(
  Type = rownames(M3T::GHGI_NG_transmission$GHGI_transmission_compressors_Activity),
  Emissions = M3T::GHGI_NG_transmission$GHGI_transmission_compressors_Emissions[, yr],
  Total_stations = M3T::GHGI_NG_transmission$GHGI_transmission_compressors_Activity[, yr])

GHGRP_facility_data <- utils::read.csv(file.path(gold, "facility_data_ngtrans.csv"))
GHGRP_facility_data$county_fips <- sprintf("%05d", GHGRP_facility_data$county_fips)
GHGRP_facility_data$zip <- sprintf("%05d", GHGRP_facility_data$zip)
GHGRP_subpartW_emissions <- utils::read.csv(file.path(gold, "subpartW_ngtrans.csv"))

domain_res <- 0.1
domain_crs <- "epsg:4326"
domain <- as.polygons(ext(-80.5, -74.7, 39.7, 42.3), crs = domain_crs)
domain_template <- rast(domain, resolution = domain_res, crs = domain_crs, vals = NA)

in_dir <- file.path(gold, "in"); dir.create(file.path(in_dir, "EIA"), recursive = TRUE, showWarnings = FALSE)
out_dir <- file.path(gold, "out"); dir.create(out_dir, showWarnings = FALSE)

M3T:::Natural_Gas_Transmission(
  input_directory = in_dir,
  GHGI_transmission_compressors = GHGI_transmission_compressors,
  GHGI_Pipeline = GHGI_Pipeline,
  Source_HIFLD_compressor_file = "M3T",
  Source_EIA_transmission_file = normalizePath(file.path(gold, "eia_pipes_pa_all.geojson")),
  domain = domain,
  domain_template = domain_template,
  GHGRP_facility_data = GHGRP_facility_data,
  GHGRP_subpartW_emissions = GHGRP_subpartW_emissions,
  GHGRP_combustion_emissions = M3T::GHGRP_combustion_emissions,
  state_name_list = NA,
  output_directory = out_dir,
  inventory_year = inv_year,
  GHGI_data_yr = GHGI_data_yr,
  verbose = FALSE,
  plot_directory = "",
  County_Tigerlines = NULL,
  State_CB = NULL
)

export_rast <- function(path) {
  r <- rast(path)
  r <- flip(r, direction = "vertical")  # CF y-ascending readback -> north-up
  list(file = basename(path), nrow = nrow(r), ncol = ncol(r),
       sum = as.numeric(global(r, "sum", na.rm = TRUE)[1, 1]),
       nonzero = as.integer(global(r, function(x) sum(x != 0, na.rm = TRUE))[1, 1]),
       values = as.numeric(values(r)))
}

outputs <- c(
  file.path(out_dir, "NG_transmission", "NG_trans_pipes.nc"),
  file.path(out_dir, "NG_transmission", "NG_trans_compressors.nc"),
  file.path(out_dir, "NG_transmission_sector_total.nc"))

# also record the key scalars for step-by-step validation
oracle <- list(
  params = list(inventory_year = inv_year, GHGI_data_yr = GHGI_data_yr,
                domain_ext = c(-80.5, -74.7, 39.7, 42.3), domain_res = domain_res),
  rasters = setNames(lapply(outputs, export_rast), basename(outputs))
)
write_json(oracle, file.path(gold, "ng_transmission_oracle.json"),
           auto_unbox = TRUE, digits = 12, na = "null")
cat("wrote", file.path(gold, "ng_transmission_oracle.json"), "\n")
