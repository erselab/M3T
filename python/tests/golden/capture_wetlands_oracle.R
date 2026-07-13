#!/usr/bin/env Rscript
# Capture golden output for the wetlands sector on a CT+RI domain.
#
# Two pieces, matching the default ("M3T") data path:
#   1. the Wetcharts prep block of CH4_inventory_build (not a function -- it lives
#      inline in the orchestrator, so it is reproduced here verbatim);
#   2. M3T:::SOCCR_Wetlands, with SOCCR1 and SOCCR2 both enabled (the config
#      defaults leave them off, but they are supported options).
#
#   M3T_DATA=/Volumes/Expansion/M3T_Processed \
#     conda run -n M3T Rscript python/tests/golden/capture_wetlands_oracle.R

suppressMessages({ library(terra); library(jsonlite); library(M3T) })

data_dir <- Sys.getenv("M3T_DATA", "/Volumes/Expansion/M3T_Processed")
gold_dir <- "python/tests/golden/wetlands"
out_dir <- file.path(gold_dir, "out")
in_dir <- file.path(gold_dir, "in")
dir.create(file.path(in_dir, "processed_NWI_data"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_dir, "Wetlands"), recursive = TRUE, showWarnings = FALSE)
stopifnot(dir.exists(data_dir))

inventory_year <- 2019
domain_res <- 0.1
domain_crs <- "epsg:4326"
states <- c("CT", "RI")
cfg <- M3T:::M3T_config

State_Tigerlines <- vect(file.path(data_dir, "combined_state_tigerlines.gpkg"),
                         layer = as.character(inventory_year))
if (domain_crs != crs(State_Tigerlines)) State_Tigerlines <- project(State_Tigerlines, domain_crs)
State_Tigerlines <- State_Tigerlines[State_Tigerlines$STUSPS %in% states, ]
State_Tigerlines <- State_Tigerlines[order(State_Tigerlines$STUSPS), ]
state_name_list <- State_Tigerlines$STUSPS
domain <- State_Tigerlines
domain_template <- rast(domain, resolution = domain_res, crs = domain_crs, vals = NA)

writeVector(State_Tigerlines[, c("STUSPS", "NAME", "STATEFP")],
            file.path(gold_dir, "domain_ct_ri.geojson"),
            filetype = "GeoJSON", overwrite = TRUE)

# --- committed fixtures: the two states' NWI rasters + clipped watersheds ------
for (st in states) {
  f <- file.path(data_dir, "processed_NWI_data",
                 paste0(st, "_combined_NWI_wetland_landcover.tif"))
  r <- rast(f)
  keep <- crop(r, ext(project(domain, crs(r))) + 30000, snap = "out")
  writeRaster(keep, file.path(in_dir, "processed_NWI_data", basename(f)),
              overwrite = TRUE, datatype = "FLT8S")
}
ws <- vect(file.path(data_dir, "Watersheds.gpkg"))
writeVector(ws, file.path(in_dir, "Watersheds.gpkg"), overwrite = TRUE)

# --- Wetcharts prep, lifted from CH4_inventory_build.R -------------------------
wetcharts <- rast(file.path(data_dir, "combined_NLCD_downscaled_wetcharts.tif"))
wetcharts_years <- sapply(strsplit(names(wetcharts), split = "_"), "[[", 1)
nearest <- unique(wetcharts_years)[which.min(abs(as.numeric(unique(wetcharts_years)) - inventory_year))]
wetcharts <- wetcharts[[wetcharts_years %in% nearest]]
cat("wetcharts year:", nearest, " layers:", nlyr(wetcharts), "\n")

domain_trans <- project(domain_template, crs(wetcharts))
wetcharts <- crop(wetcharts, ext(domain_trans) * 1.1, snap = "out")
# commit the cropped ensemble as the fixture (small once cropped)
writeRaster(wetcharts, file.path(in_dir, "wetcharts_ctri.tif"),
            overwrite = TRUE, datatype = "FLT8S")

if (any(res(wetcharts) < res(domain_trans))) {
  wetcharts <- project(wetcharts, domain_template, method = "mean")
} else if (any(res(wetcharts) > res(domain_trans))) {
  wetcharts <- disagg(wetcharts, round(res(wetcharts) / res(domain_trans), 3), "near")
  wetcharts <- project(wetcharts, domain_template, method = "near")
}
wetcharts <- mask(wetcharts, domain)
cover <- extract(wetcharts[[1]], domain, weights = TRUE, cells = TRUE)
wetcharts[cover[, "cell"]] <- wetcharts[cover[, "cell"]] * cover[, "weight"]

wetcharts_models <- sapply(strsplit(names(wetcharts), split = "_"), "[[", 3)
Wetland_output_directory <- file.path(out_dir, "Wetlands")
for (A in seq_along(cfg$Wetcharts_model_subset)) {
  sub <- terra::mean(wetcharts[[wetcharts_models %in% cfg$Wetcharts_model_subset[[A]]]], na.rm = TRUE)
  M3T:::writeCDF_no_newline(sub,
    file.path(Wetland_output_directory, paste0("Wetcharts_NLCD_Downscaled_subset_", A, ".nc")),
    force_v4 = TRUE, varname = "methane_emissions", unit = "nmol/m2/s",
    longname = "wetcharts", missval = -9999, overwrite = TRUE)
}

# --- SOCCR_Wetlands, with both variants on ------------------------------------
# NOTE: this call dies at the very end, in the sector-total block, with
#   "[+] extents do not match"
# It gets there by re-reading the Wetcharts NetCDF it just wrote
# (rast(".../Wetcharts_NLCD_Downscaled_subset_1.nc")), and terra cannot recover the
# extent it wrote -- hence the "[rast] unknown extent" warnings -- so the re-read
# raster has extent 0..ncol and will not add to the in-memory Freshwater raster.
# Same root cause as the flipped sector totals in Stationary_combustion.R: writeCDF
# output is not readable back by this GDAL/terra build.
#
# Freshwater.nc, SOCCR1.nc and SOCCR2.nc are all written *before* that point, so we
# catch the error and export them. They are the reference; the sector totals are
# plain sums of them, which the Python test asserts structurally.
tryCatch(
  M3T:::SOCCR_Wetlands(
  input_directory = in_dir,
  output_directory = out_dir,
  plot_directory = "",
  state_name_list = state_name_list,
  domain = domain,
  domain_template = domain_template,
  Use_SOCCR1 = TRUE,
  Use_SOCCR2 = TRUE,
  Wetland_EFs = cfg$Wetland_EFs,
  verbose = FALSE,
  State_Tigerlines = State_Tigerlines,
  County_Tigerlines = NULL,
  State_CB = NULL,
  Source_Watershed_file = "M3T",
  Source_NWI = "M3T",
    Use_Wetcharts = TRUE,
    Wetcharts_model_subset = cfg$Wetcharts_model_subset
  ),
  error = function(e) {
    if (!grepl("extents do not match", conditionMessage(e))) stop(e)
    cat("NOTE: R's sector-total block failed as expected (", conditionMessage(e),
        ") -- component rasters were written first and are what we export.\n")
  }
)

export_rast <- function(path) {
  r <- flip(rast(path), direction = "vertical")
  list(file = basename(path), nrow = nrow(r), ncol = ncol(r),
       ext = as.vector(ext(r)), res = as.vector(res(r)),
       sum = as.numeric(global(r, "sum", na.rm = TRUE)[1, 1]),
       nonzero = as.integer(global(r, function(x) sum(x != 0, na.rm = TRUE))[1, 1]),
       values = as.numeric(values(r)))
}

outputs <- c(
  list.files(Wetland_output_directory, pattern = "[.]nc$", full.names = TRUE),
  list.files(out_dir, pattern = "^Wetland_sector_total.*[.]nc$", full.names = TRUE)
)
oracle <- list(
  params = list(inventory_year = inventory_year, wetcharts_year = as.numeric(nearest),
                domain_states = states, domain_ext = as.vector(ext(domain)),
                domain_res = domain_res, domain_crs = domain_crs),
  rasters = lapply(outputs, export_rast)
)
names(oracle$rasters) <- basename(outputs)
write_json(oracle, file.path(gold_dir, "wetlands_oracle.json"),
           auto_unbox = TRUE, digits = 12, na = "null")
cat("wrote oracle with", length(outputs), "rasters\n")
