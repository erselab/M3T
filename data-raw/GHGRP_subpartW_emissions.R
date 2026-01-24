## code to prepare `ghgrp_w_only_emissions` dataset.

input_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Manuscript/All inventory data/Prepared inventory data/"

################################################################################
#Download the relevant ghgrp emissions data using the API
#(https://www.epa.gov/enviro/envirofacts-data-service-api) and combine the
#facility and emission data appropriately

options("timeout"=60*20)

ghgrp_oil_and_gas_file <-  tempfile(fileext = ".csv")

#download the relevant LDC-sector data
#(https://www.epa.gov/enviro/greenhouse-gas-model).  
data_URL <- "https://data.epa.gov/dmapservice/ghg.ef_w_emissions_source_ghg/csv"
utils::download.file(data_URL, destfile = ghgrp_oil_and_gas_file,quiet=T)

################################################################################
#Subset

GHGRP_oil_and_gas_emissions <- utils::read.csv(ghgrp_oil_and_gas_file)

unlink(ghgrp_oil_and_gas_file)

GHGRP_oil_and_gas_emissions <- GHGRP_oil_and_gas_emissions[,c("facility_id","facility_name","industry_segment","reporting_year","total_reported_ch4_emissions")]

################################################################################
#save

utils::write.csv(GHGRP_oil_and_gas_emissions,file.path(input_directory,"GHGRP_subpartW_emissions.csv"))

