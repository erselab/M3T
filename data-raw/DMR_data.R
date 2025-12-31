## code to prepare `DMR_data` dataset.  Takes the folder containing the DMR data
## for all years at the time (2010 - 2024) from
## https://echo.epa.gov/trends/loading-tool/water-pollution-search.  Set
## Pollutant = wastewater flow and industry = Publicly Owned Treatment Works
## (POTWs)

input_directory <- "D:/MMMT STUFF/All inventory data/Not Automated/"
# input_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Manuscript/All inventory data/Not Automated"

library(terra)
################################################################################
#load, minor processing, and combine across years

setwd(input_directory)
Data_files <- list.files(pattern = "DMR")

indx=1
startline <- readLines(Data_files[indx],n = 10)
DMR_yr <- startline[grep(pattern = "Search Criteria",x = startline)]
digit <- attributes(regexpr("Search Criteria: Year = 20.{2}",DMR_yr))$match.length
DMR_yr <- substr(DMR_yr,digit-2,digit+1)

Combined_dataset <- read.csv(Data_files[indx],skip=grep(pattern = "Data Source",x = startline)-1)
Combined_dataset$year <- DMR_yr

for(indx in 2:length(Data_files)){
  startline <- readLines(Data_files[indx],n = 10)
  DMR_yr <- startline[grep(pattern = "Search Criteria",x = startline)]
  digit <- attributes(regexpr("Search Criteria: Year = 20.{2}",DMR_yr))$match.length
  DMR_yr <- substr(DMR_yr,digit-2,digit+1)

  dataset <- read.csv(Data_files[indx],skip=grep(pattern = "Data Source",x = startline)-1)
  dataset$year <- DMR_yr
  Combined_dataset <- rbind(Combined_dataset,dataset)
}

#filter to only the columns relevant for this work
Combined_dataset <- Combined_dataset[,c("Facility.Latitude","Facility.Longitude","Average.Daily.Flow..MGD.","year")]

#replace periods with underscores in naming for consistency/ease
colnames(Combined_dataset) <- gsub("\\.","\\_",colnames(Combined_dataset))

#remove those without location data and vect as lat/long assuming WGS
#(didn't see one explicitly mentioned, little impact on location)
DMR_data <- subset(Combined_dataset,!is.na(Facility_Latitude) & !is.na(Facility_Longitude))

################################################################################
#save output
usethis::use_data(DMR_data, overwrite = TRUE)
