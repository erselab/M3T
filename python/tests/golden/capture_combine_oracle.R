#!/usr/bin/env Rscript
# Capture golden output for Combine_across_sectors by calling the real internal
# M3T:::Combine_across_sectors on SYNTHETIC sector rasters.
#
# Each synthetic sector file is a constant-valued raster with a distinct value, so
# every output can be decoded arithmetically -- that pins the exact semantics
# (which files feed min/mean/max, how the two stationary-combustion files per
# variation are treated, how the thermo/non-thermo split falls out) without
# needing the companion data or a full inventory run.
#
# Run from repo root:
#   conda run -n M3T Rscript python/tests/golden/capture_combine_oracle.R

suppressMessages({ library(terra); library(jsonlite) })

gold <- "python/tests/golden/combine"
out_dir <- file.path(gold, "out")
unlink(out_dir, recursive = TRUE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

domain_crs <- "epsg:4326"
domain <- as.polygons(ext(-75, -74.7, 40, 40.2), crs = domain_crs)
domain_template <- rast(domain, resolution = 0.1, crs = domain_crs, vals = NA)

# distinct constant per file so outputs are decodable
files <- list(
  # non-varying ("set") sectors
  "GEPA_ind_landfill.nc" = 1,
  "GEPA_non_thermo.nc" = 2,
  "GEPA_thermo.nc" = 4,
  "NG_transmission_sector_total.nc" = 8,
  # landfills: 2 variations
  "Landfill_sector_total_GHGRP_reported.nc" = 10,
  "Landfill_sector_total_GHGRP_generation_first.nc" = 20,
  # wetlands: 2 variations
  "Wetland_sector_total_SOCCR1.nc" = 100,
  "Wetland_sector_total_Wetcharts_NLCD_subset_1.nc" = 200,
  # wastewater: 2 variations
  "Wastewater_sector_total_DMR_Moore_national.nc" = 1000,
  "Wastewater_sector_total_CWNS_GHGI_state.nc" = 2000,
  # NG distribution: 2 variations
  "NG_distribution_sector_total_Vulcan_bystate.nc" = 10000,
  "NG_distribution_sector_total_ACES_bystate.nc" = 20000,
  # stationary combustion: 2 variations, EACH made of 2 files (fossil + wood)
  "Stationary_combustion_sector_fossil_fuel_total_Vulcan_bystate.nc" = 100000,
  "Stationary_combustion_sector_wood_total_Vulcan_bystate.nc" = 200000,
  "Stationary_combustion_sector_fossil_fuel_total_ACES_bystate.nc" = 300000,
  "Stationary_combustion_sector_wood_total_ACES_bystate.nc" = 400000
)

for (nm in names(files)) {
  r <- domain_template
  values(r) <- files[[nm]]
  suppressWarnings(invisible(capture.output(
    writeCDF(r, file.path(out_dir, nm), varname = "methane_emissions",
             unit = "nmol/m2/s", missval = -9999, force_v4 = TRUE, overwrite = TRUE)
  )))
}

# NOTE: R's thermogenic *summary* path cannot run. `writeCDF` -> `rast` loses the
# extent and CRS (a sector file reads back with ext 0..ncol, no CRS), and that
# block builds its accumulator with `rast(domain_template, nlyrs=3)` -- the real
# extent -- then adds the read-back layers, so terra raises
# "[rast] extents do not match". The main summary path survives only because it
# seeds from `set_rast` (also read back), so its extents agree with each other.
# We therefore capture in two runs, avoiding that combination.

run_combine <- function(sep_thermo, summary_, individual) {
  M3T:::Combine_across_sectors(
    output_directory = out_dir,
    Separate_thermo = sep_thermo,
    Create_summary_combinations = summary_,
    Create_individual_combinations = individual,
    plot_directory = "",
    County_Tigerlines = NULL,
    State_CB = NULL,
    domain = domain,
    domain_template = domain_template,
    verbose = FALSE
  )
}

# Run A: main summary + individual combinations + key (no thermo split)
run_combine(sep_thermo = FALSE, summary_ = TRUE, individual = TRUE)
# Run B: thermogenic / non-thermogenic individual combinations (no summary)
run_combine(sep_thermo = TRUE, summary_ = FALSE, individual = TRUE)

# --- export ---------------------------------------------------------------- #
# constant rasters -> report one representative value per layer
layer_values <- function(path) {
  r <- rast(path)
  vals <- sapply(seq_len(nlyr(r)), function(i) {
    v <- values(r[[i]]); as.numeric(v[!is.na(v)][1])
  })
  list(file = basename(path), nlyr = nlyr(r), names = names(r), values = vals)
}

combined <- file.path(out_dir, "Combined_files")
summary_dir <- file.path(combined, "summary_combinations")

oracle <- list(inputs = files)

for (f in list.files(summary_dir, pattern = "[.]nc$", full.names = TRUE)) {
  oracle$summary[[basename(f)]] <- layer_values(f)
}

# individual combinations: value per combination file, plus the key
indiv <- sort(list.files(combined, pattern = "^Combined_inventory_combination_.*nc$",
                         full.names = TRUE))
oracle$individual <- lapply(indiv, layer_values)
names(oracle$individual) <- basename(indiv)

for (sub in c("thermogenic", "non_thermogenic")) {
  fs <- sort(list.files(file.path(combined, sub), pattern = "[.]nc$", full.names = TRUE))
  if (length(fs)) {
    lst <- lapply(fs, layer_values); names(lst) <- basename(fs)
    oracle[[sub]] <- lst
  }
}

key <- utils::read.csv(file.path(combined, "Combined_inventory_key.csv"),
                       check.names = FALSE, colClasses = "character")
oracle$key <- key
oracle$key_colnames <- colnames(key)

write_json(oracle, file.path(gold, "combine_oracle.json"),
           auto_unbox = TRUE, digits = 12, na = "null")
cat("wrote", file.path(gold, "combine_oracle.json"), "\n")
