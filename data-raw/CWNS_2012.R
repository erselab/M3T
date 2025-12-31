## code to prepare `CWNS_2012` dataset.  This is to subset the data after
## converting from an access database as described in the R/data.R documentation

input_directory <- "D:/MMMT STUFF/All inventory data/Not Automated/"
output_directory <- "D:/MMMT STUFF/All inventory data/Not Automated/"

library(readxl)
################################################################################
#load and minor processing

CWNS_2012 <- read_xlsx(file.path(input_directory,"2012_CWNS.xlsx"))

#correct an oklahoma site that had the wrong hemisphere listed
CWNS_2012[CWNS_2012$FACILITY_ID=="1172804","LONGITUDE"] <- gsub("E","W",CWNS_2012[CWNS_2012$FACILITY_ID=="1172804","LONGITUDE"])

#subset to the relevant columns and convert to df to save space
CWNS_2012 <- CWNS_2012[,c("LATITUDE","LONGITUDE","EXIST_MUNICIPAL","HORIZONTAL_COORDINATE_DATUM")]
CWNS_2012 <- as.data.frame(CWNS_2012)

#some have no location or activity data - remove them
CWNS_2012 <- na.omit(CWNS_2012)

#ID any that are in the western or southern hemisphere (- coordinates)
Western_hemis <- grep("W",CWNS_2012$LONGITUDE)
Southern_hemis <- grep("S",CWNS_2012$LATITUDE)

#remove the hemisphere text so we can make numeric
CWNS_2012$LATITUDE <- gsub("N|S","",CWNS_2012$LATITUDE)
CWNS_2012$LONGITUDE <- gsub("W|E","",CWNS_2012$LONGITUDE)
CWNS_2012$LATITUDE <- as.numeric(CWNS_2012$LATITUDE)
CWNS_2012$LONGITUDE <- as.numeric(CWNS_2012$LONGITUDE)

#make those in the S or W hemispheres the appropriate negative coordinates
CWNS_2012$LATITUDE[Southern_hemis] <- CWNS_2012$LATITUDE[Southern_hemis]*-1
CWNS_2012$LONGITUDE[Western_hemis] <- CWNS_2012$LONGITUDE[Western_hemis]*-1

################################################################################
#save

usethis::use_data(CWNS_2012, overwrite = TRUE)
