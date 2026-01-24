## code to prepare `GHGRP_combustion` dataset goes here

#Download the relevant emissions data using the API
#(https://www.epa.gov/enviro/envirofacts-data-service-api)

#download the relevant landfill-sector data in MT CH4/yr
#(https://www.epa.gov/enviro/greenhouse-gas-model).  Must download the
#relevant data for each possible sector separately as emissions are split by
#sector (i.e., gas capture for electricity is subpart D, stationary combustion
#is C, and landfill emissions HH - all of which can occur at the same
#landfill).  Only C and D are included as "reported" municipal emissions
#exclude subpart C and D. Landfills have 2 options for reporting their
#emissions - equation HH-6 and HH-8.  HH-6 is based on a first order decay
#model, HH-8 is based on collection efficiency of a gas collection system.
#The ghgrp_landfill_detail_emissions include both as either can be the
#"reported" value.


################################################################################
#download, load in and combine the emission data appropriately

ghgrp_combustion_file <- tempfile(".csv")

data_URL <- "https://data.epa.gov/dmapservice/ghg.c_subpart_level_information/csv"
download.file(data_URL,ghgrp_combustion_file,quiet=T)

#load in the files
ghgrp_combustion_emissions <- read.csv(ghgrp_combustion_file)

#simple function to make sure gas names are limited to methane, and column
#names are consistent across GHG input data
make_consistent <- function(input){
  colnames(input) <- gsub("ghg_gas_name","ghg_name",colnames(input))
  colnames(input) <- gsub("reporting_year","year",colnames(input))
  input$ghg_name <- tolower(input$ghg_name)
  input$facility_name <- tolower(input$facility_name)
  input <- input[input$ghg_name=="methane",]
  return(input)
}

GHGRP_combustion <- make_consistent(ghgrp_combustion_emissions)

usethis::use_data(GHGRP_combustion, overwrite = TRUE)
