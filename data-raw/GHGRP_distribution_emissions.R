## code to prepare `ghgrp_w_only_emissions` dataset goes here

options("timeout"=60*20)

################################################################################
#Download the relevant ghgrp emissions data using the API
#(https://www.epa.gov/enviro/envirofacts-data-service-api) and combine the
#facility and emission data appropriately

ghgrp_oil_and_gas_file <-  tempfile(fileext = ".csv")

#download the relevant LDC-sector data
#(https://www.epa.gov/enviro/greenhouse-gas-model).  
data_URL <- "https://data.epa.gov/dmapservice/ghg.ef_w_emissions_source_ghg/csv"
utils::download.file(data_URL, destfile = ghgrp_oil_and_gas_file,quiet=T)

GHGRP_oil_and_gas_emissions <- read.csv(ghgrp_oil_and_gas_file)

unlink(ghgrp_oil_and_gas_file)

usethis::use_data(GHGRP_oil_and_gas_emissions, overwrite = TRUE)
