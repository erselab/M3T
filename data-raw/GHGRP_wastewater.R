## code to prepare `GHGRP_wastewater` dataset goes here

################################################################################
#download
ghgrp_wastewater_file <- tempfile(fileext = ".csv")

data_URL <- "https://data.epa.gov/dmapservice/ghg.ii_subpart_level_information/csv"
utils::download.file(data_URL,ghgrp_wastewater_file,quiet=T)
################################################################################
#load in - no need to subset
GHGRP_wastewater <- read.csv(ghgrp_wastewater_file)

################################################################################
#save
usethis::use_data(GHGRP_wastewater, overwrite = TRUE)
