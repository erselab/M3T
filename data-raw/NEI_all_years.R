## code to prepare `NEI_all_years` dataset goes here


library(jsonlite)
################################################################################
#download

#this data takes longer than the default timeout - increase
default_timeout <- options("timeout")
options("timeout"=60*20)

#set output location
NEI_filename <- tempfile(fileext = ".csv")

#download data
data_URL <- "https://data.epa.gov/dmapservice/nei.county_sector_summary/pollutant_code/equals/CO/json"
NEI_data_orig <- jsonlite::fromJSON(data_URL)

#download the sector code key
data_URL <- "https://data.epa.gov/dmapservice/nei.sectors/json"
NEI_sector_codes <- jsonlite::fromJSON(data_URL)

################################################################################
#cleanup

#Rewrite the sector codes from numeric to text descriptions.  Method from
#(https://stackoverflow.com/a/50898694)
NEI_data_orig$sector_code[NEI_data_orig$sector_code %in% NEI_sector_codes$sector_code] <- 
  NEI_sector_codes$ei_sector[match(NEI_data_orig$sector_code,NEI_sector_codes$sector_code,nomatch=0)]

#change the column names to match those of the xlsx downloaded equivalent, to
#be consistent with older versions of the code.  Just renaming a few columns.
colnames(NEI_data_orig) <- toupper(gsub("_"," ",
                                        gsub("st_abbrv","state",
                                             gsub("county_name","county",
                                                  gsub("sector_code","SECTOR",
                                                       gsub("uom","unit of measure",colnames(NEI_data_orig)))))))

#filter to only sectors used by this package
required_sectors <- c('Fuel Comb - Comm/Institutional - Biomass',
                      'Fuel Comb - Comm/Institutional - Coal',
                      'Fuel Comb - Comm/Institutional - Natural Gas',
                      'Fuel Comb - Comm/Institutional - Oil',
                      'Fuel Comb - Comm/Institutional - Other',
                      'Fuel Comb - Electric Generation - Biomass',
                      'Fuel Comb - Electric Generation - Coal',
                      'Fuel Comb - Electric Generation - Natural Gas',
                      'Fuel Comb - Electric Generation - Oil',
                      'Fuel Comb - Electric Generation - Other',
                      'Fuel Comb - Industrial Boilers, ICEs - Biomass',
                      'Fuel Comb - Industrial Boilers, ICEs - Coal',
                      'Fuel Comb - Industrial Boilers, ICEs - Natural Gas',
                      'Fuel Comb - Industrial Boilers, ICEs - Oil',
                      'Fuel Comb - Industrial Boilers, ICEs - Other',
                      'Fuel Comb - Residential - Natural Gas',
                      'Fuel Comb - Residential - Oil',
                      'Fuel Comb - Residential - Other',
                      'Fuel Comb - Residential - Wood')
NEI_data_orig <- NEI_data_orig[NEI_data_orig$SECTOR %in% required_sectors,]

NEI_all_years <- NEI_data_orig

options("timeout"=default_timeout)

################################################################################
#save

usethis::use_data(NEI_all_years, overwrite = TRUE)
