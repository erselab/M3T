## code to prepare `GHGRP_landfills` dataset goes here

################################################################################
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

ghgrp_landfill_file <- tempfile(fileext = ".csv")
ghgrp_landfill_system_details_file <- tempfile(fileext = ".csv")

data_URL <- "https://data.epa.gov/dmapservice/ghg.hh_subpart_level_information/csv"
utils::download.file(data_URL,ghgrp_landfill_file,quiet=T)

data_URL <- "https://data.epa.gov/dmapservice/ghg.hh_gas_collection_system_detls/csv"
utils::download.file(data_URL,ghgrp_landfill_system_details_file,quiet=T)

################################################################################
#load in and combine the emission data appropriately

#load in the files
ghgrp_landfill_only_emissions <- read.csv(ghgrp_landfill_file)
ghgrp_landfill_detail_emissions <- read.csv(ghgrp_landfill_system_details_file)

#simple function to make sure gas names are limited to methane, and column
#names are consistent
make_consistent <- function(input){
  colnames(input) <- gsub("ghg_gas_name","ghg_name",colnames(input))
  colnames(input) <- gsub("reporting_year","year",colnames(input))
  input$ghg_name <- tolower(input$ghg_name)
  input$facility_name <- tolower(input$facility_name)
  input <- input[input$ghg_name=="methane",]
  return(input)
}

ghgrp_landfill_only_emissions <- make_consistent(ghgrp_landfill_only_emissions)

#Now add the HH-6 and HH-8 emission rates to the dataframe too
GHGRP_landfills <- merge(ghgrp_landfill_only_emissions,
                         ghgrp_landfill_detail_emissions[,c("facility_id","reporting_year","equation_hh6_result","equation_hh8_result")],
                         by.x=c("facility_id","year"),by.y=c("facility_id","reporting_year"),all.x=T)
colnames(GHGRP_landfills) <- gsub("equation_hh6_result","HH_modeled",colnames(GHGRP_landfills))
colnames(GHGRP_landfills) <- gsub("equation_hh8_result","HH_collection_efficiency",colnames(GHGRP_landfills))

GHGRP_landfills <- GHGRP_landfills[GHGRP_landfills$ghg_name=="methane",]

usethis::use_data(GHGRP_landfills, overwrite = TRUE)
