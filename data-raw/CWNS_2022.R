## code to prepare `CWNS_2022` dataset.  This takes the folder containing the
## 2022 Clean Watershed Needs report data from
## https://www.epa.gov/cwns/clean-watersheds-needs-survey-cwns-2022-report-and-data.
## There is a link to the data dashboard which has a data download tab.
## Download the data as CSV and set the working directory to this folder.

input_directory <- "D:/MMMT STUFF/All inventory data/Not Automated/2022_CWNS_NATIONAL_APR2024/"

################################################################################
#load in and preprocess

#load in the relevant tables from CWNS 2022
Location <- read.csv(file.path(input_directory,"PHYSICAL_LOCATION.csv"))
Flow <- read.csv(file.path(input_directory,"FLOW.csv"))
Facilities <- read.csv(file.path(input_directory,"FACILITIES.csv"))

#filter to only municipal facilities
Flow <- Flow[Flow$FLOW_TYPE=="Municipal Flow",]
Location <- Location[Location$CWNS_ID %in% Flow$CWNS_ID,]
Facilities <- Facilities[Facilities$CWNS_ID %in% Flow$CWNS_ID,]

#combine the relevant data from the 3 files (equivalent to merge by ID,
#then subsetting columns)
CWNS_2022 <- Location
CWNS_2022$EXIST_MUNICIPAL <- Flow$CURRENT_DESIGN_FLOW[match(CWNS_2022$CWNS_ID,Flow$CWNS_ID)]
CWNS_2022$facility_name <- Facilities$FACILITY_NAME[match(CWNS_2022$CWNS_ID,Facilities$CWNS_ID)]

#filter to only the columns relevant for this work
CWNS_2022 <- CWNS_2022[,c("LATITUDE","LONGITUDE","EXIST_MUNICIPAL")]
CWNS_2022 <- na.omit(CWNS_2022)
################################################################################
#save output

usethis::use_data(CWNS_2022, overwrite = TRUE)
