#!/usr/bin/env Rscript
# Capture golden output for the wastewater sector by calling the *real* internal
# M3T:::Wastewater on a CT+RI state domain (the port plan's reference domain).
#
# Unlike Municipal_solid_waste, this function cannot be run with its septic
# inputs stubbed out: it unconditionally loads the NLCD rasters and merges state
# population against State_Tigerlines. So the capture needs the M3T companion
# data (DMR_data.csv, combined_wastewater_NLCD.tif, the two septic-area CSVs,
# combined_state_tigerlines.gpkg) plus a GHGRP facility table.
#
# Point M3T_DATA at the companion directory (default is the external drive):
#   M3T_DATA=/Volumes/Expansion/M3T_Processed \
#     conda run -n M3T Rscript python/tests/golden/capture_wastewater_oracle.R
#
# All method variants are enabled so a single run yields the full oracle:
#   4 municipal (CWNS|DMR x GHGI|Moore), 1 industrial, 2 septic, 8 sector totals.
#
# Writes python/tests/golden/wastewater/:
#   out/*.nc                 (raw sector outputs)
#   wastewater_oracle.json   (grid meta + full value arrays + sums; the committed oracle)
#   dmr_ctri.csv, facility_data_wastewater.csv  (committed input fixtures)

suppressMessages({ library(terra); library(jsonlite); library(M3T) })

data_dir <- Sys.getenv("M3T_DATA", "/Volumes/Expansion/M3T_Processed")
gold_dir <- "python/tests/golden/wastewater"
dir.create(file.path(gold_dir, "in", "GHGRP"), recursive = TRUE, showWarnings = FALSE)
out_dir <- file.path(gold_dir, "out")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

stopifnot(dir.exists(data_dir))

inventory_year <- 2019
domain_res <- 0.1
domain_crs <- "epsg:4326"
# IA+NE rather than the plan's CT/RI: the industrial (GHGRP subpart II) stream
# has no reporters at all in CT/RI, and the R's CSV block errors on a zero-row
# facility set. IA (16) and NE (10) are the top two by 2019 subpart-II count and
# are adjacent, so two states also exercise the per-state septic path.
states <- c("IA", "NE")

# --- domain: CT + RI polygons, exactly as the orchestrator derives them ------
State_Tigerlines <- vect(file.path(data_dir, "combined_state_tigerlines.gpkg"),
                         layer = as.character(inventory_year))
if (domain_crs != crs(State_Tigerlines)) State_Tigerlines <- project(State_Tigerlines, domain_crs)
State_Tigerlines <- State_Tigerlines[State_Tigerlines$STUSPS %in% states, ]
State_Tigerlines <- State_Tigerlines[order(State_Tigerlines$STUSPS), ]
state_name_list <- State_Tigerlines$STUSPS

domain <- State_Tigerlines
domain_template <- rast(domain, resolution = domain_res, crs = domain_crs, vals = NA)

# commit the domain polygons so the Python test doesn't need the companion drive
writeVector(domain[, c("STUSPS", "NAME", "STATEFP")],
            file.path(gold_dir, "domain_ia_ne.geojson"),
            filetype = "GeoJSON", overwrite = TRUE)

# --- inputs the orchestrator would prepare -----------------------------------
GHGI_wastewater_data <- M3T:::M3T_config$GHGI_wastewater_data
GHGI_file_yr <- max(as.numeric(GHGI_wastewater_data$year))
GHGI_data_yr <- if (inventory_year > GHGI_file_yr) GHGI_file_yr else inventory_year

facility_csv <- file.path(gold_dir, "facility_data_wastewater.csv")
if (!file.exists(facility_csv)) {
  # subset the full GHGRP facility table to subpart-II (wastewater) reporters
  fac <- utils::read.csv(file.path(data_dir, "GHGRP", "facility_data.csv"))
  keep <- fac$facility_id %in% M3T::GHGRP_wastewater$facility_id
  cols <- c("facility_id", "year", "facility_name", "latitude", "longitude", "state")
  utils::write.csv(fac[keep, cols], facility_csv, row.names = FALSE)
}
GHGRP_facility_data <- utils::read.csv(facility_csv)

# DMR: the sector reads <input_directory>/DMR_data.csv and filters to the year.
# Commit only the rows the CT/RI domain can see (plus the national flow total is
# computed pre-crop, so ALL rows of the year are needed -- keep the full year).
in_dir <- file.path(gold_dir, "in")
dmr_fixture <- file.path(in_dir, "DMR_data.csv")
if (!file.exists(dmr_fixture)) {
  dmr <- utils::read.csv(file.path(data_dir, "DMR_data.csv"))
  DMR_yr <- (2010:2024)[which.min(abs(inventory_year - (2010:2024)))]
  utils::write.csv(dmr[dmr$year == DMR_yr, ], dmr_fixture, row.names = FALSE)
}

# NLCD + septic-area inputs are large; read them from the companion dir directly
# by pointing input_directory there, and copy DMR in alongside.
run_in <- file.path(tempdir(), "m3t_in")
dir.create(file.path(run_in, "GHGRP"), recursive = TRUE, showWarnings = FALSE)
for (f in c("combined_wastewater_NLCD.tif", "Total_national_septic_area.csv",
            "wastewater_state_septic_area.csv")) {
  file.symlink(file.path(data_dir, f), file.path(run_in, f))
}
file.copy(dmr_fixture, file.path(run_in, "DMR_data.csv"), overwrite = TRUE)

M3T:::Wastewater(
  input_directory  = run_in,
  output_directory = out_dir,
  Wastewater_use_CWNS = TRUE,
  Wastewater_use_DMR  = TRUE,
  Wastewater_Municipal_Method_Moore = TRUE,
  Wastewater_Municipal_Method_GHGI  = TRUE,
  Wastewater_national_septic = TRUE,
  Wastewater_state_septic    = TRUE,
  domain = domain,
  domain_template = domain_template,
  GHGRP_facility_data = GHGRP_facility_data,
  Source_GHGRP_wastewater = "M3T",
  Source_CWNS = "M3T",
  Source_DMR  = "M3T",
  Source_wastewater_NLCD = "M3T",
  Source_State_population_data = "M3T",
  inventory_year = inventory_year,
  National_wastewater_info       = M3T:::M3T_config$National_wastewater_info,
  Wastewater_reported_State_info = M3T:::M3T_config$Wastewater_reported_State_info,
  GHGI_wastewater_data           = GHGI_wastewater_data,
  GHGI_data_yr = GHGI_data_yr,
  Total_national_open_or_low_int_area = M3T:::M3T_config$Total_national_open_or_low_int_area,
  State_Tigerlines = State_Tigerlines,
  state_name_list = state_name_list,
  County_Tigerlines = NULL,
  plot_directory = "",
  State_CB = NULL,
  verbose = FALSE
)

# --- export each output raster as grid meta + values -------------------------
export_rast <- function(path) {
  r <- rast(path)
  # writeCDF stores y ascending (CF); flip to north-up so `values` are canonical
  # top-left-first, matching rioxarray/Python row-major order.
  r <- flip(r, direction = "vertical")
  list(
    file = basename(path),
    nrow = nrow(r), ncol = ncol(r),
    ext = as.vector(ext(r)), res = as.vector(res(r)),
    sum = as.numeric(global(r, "sum", na.rm = TRUE)[1, 1]),
    nonzero = as.integer(global(r, function(x) sum(x != 0, na.rm = TRUE))[1, 1]),
    values = as.numeric(values(r))
  )
}

ww <- file.path(out_dir, "Wastewater")
outputs <- c(
  file.path(ww, "Wastewater_CWNS_GHGI_dom_central.nc"),
  file.path(ww, "Wastewater_CWNS_Moore_dom_central.nc"),
  file.path(ww, "Wastewater_DMR_GHGI_dom_central.nc"),
  file.path(ww, "Wastewater_DMR_Moore_dom_central.nc"),
  file.path(ww, "Wastewater_dom_septic_national.nc"),
  file.path(ww, "Wastewater_dom_septic_bystate.nc"),
  file.path(ww, "Wastewater_ind.nc"),
  file.path(out_dir, "Wastewater_sector_total_CWNS_GHGI_national.nc"),
  file.path(out_dir, "Wastewater_sector_total_CWNS_GHGI_state.nc"),
  file.path(out_dir, "Wastewater_sector_total_CWNS_Moore_national.nc"),
  file.path(out_dir, "Wastewater_sector_total_CWNS_Moore_state.nc"),
  file.path(out_dir, "Wastewater_sector_total_DMR_GHGI_national.nc"),
  file.path(out_dir, "Wastewater_sector_total_DMR_GHGI_state.nc"),
  file.path(out_dir, "Wastewater_sector_total_DMR_Moore_national.nc"),
  file.path(out_dir, "Wastewater_sector_total_DMR_Moore_state.nc")
)
outputs <- outputs[file.exists(outputs)]

oracle <- list(
  params = list(
    inventory_year = inventory_year, GHGI_data_yr = GHGI_data_yr,
    domain_states = states, domain_ext = as.vector(ext(domain)),
    domain_res = domain_res, domain_crs = domain_crs,
    central_EPA_emiss = GHGI_wastewater_data$Nonseptic.Emissions[
      GHGI_wastewater_data$year == GHGI_data_yr] * 1e9 / (16.043 * 365 * 24 * 60 * 60)
  ),
  rasters = lapply(outputs, export_rast)
)
names(oracle$rasters) <- basename(outputs)

write_json(oracle, file.path(gold_dir, "wastewater_oracle.json"),
           auto_unbox = TRUE, digits = 12, na = "null")
cat("wrote", file.path(gold_dir, "wastewater_oracle.json"), "with",
    length(outputs), "rasters\n")
