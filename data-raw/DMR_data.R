## code to prepare `DMR_data` dataset.  Takes the folder containing the DMR data
## for all years at the time (2010 - 2024) from
## https://echo.epa.gov/trends/loading-tool/water-pollution-search.  Set
## Pollutant = wastewater flow and industry = Publicly Owned Treatment Works
## (POTWs). Downloaded as annual files.

input_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Manuscript/All inventory data/Prepared inventory data/DMR_raw/"
output_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Manuscript/All inventory data/Prepared inventory data/"

################################################################################
#load, minor processing, and combine across years

Data_files <- list.files(pattern = "DMR",input_directory,full.names = T)

#use regex to ID the first line with useful data and the year
indx=1
startline <- readLines(Data_files[indx],n = 10)
DMR_yr <- startline[grep(pattern = "Search Criteria",x = startline)]
txt_location <- attributes(regexpr("Search Criteria: Year = 20.{2}",DMR_yr))$match.length
DMR_yr <- substr(DMR_yr,txt_location-2,txt_location+1)

#create a single dataframe with all data, add a column with the year
Combined_dataset <- read.csv(Data_files[indx],skip=grep(pattern = "Data Source",x = startline)-1)
Combined_dataset$year <- DMR_yr

#repeat identically for all files
for(indx in 2:length(Data_files)){
  startline <- readLines(Data_files[indx],n = 10)
  DMR_yr <- startline[grep(pattern = "Search Criteria",x = startline)]
  txt_location <- attributes(regexpr("Search Criteria: Year = 20.{2}",DMR_yr))$match.length
  DMR_yr <- substr(DMR_yr,txt_location-2,txt_location+1)

  dataset <- read.csv(Data_files[indx],skip=grep(pattern = "Data Source",x = startline)-1)
  dataset$year <- DMR_yr
  Combined_dataset <- rbind(Combined_dataset,dataset)
  cat("\rProcessed DMR file",indx,"of",length(Data_files),"             ")
}

#filter to only the columns relevant for this work + name and state for
#reference
Combined_dataset <- Combined_dataset[,c("Facility.Name","State","Facility.Latitude","Facility.Longitude","Average.Daily.Flow..MGD.","year")]

#replace periods with underscores in naming for consistency/ease
colnames(Combined_dataset) <- gsub("\\.","\\_",colnames(Combined_dataset))

#remove those without location data
DMR_data <- subset(Combined_dataset,!is.na(Facility_Latitude) & !is.na(Facility_Longitude))

################################################################################
#save output

utils::write.csv(DMR_data,file.path(output_directory,"DMR_data.csv"))

