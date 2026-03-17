## code to prepare `Wastewater_1990_state_septic`.  Data is a text file from the
#Historical Census of Housing Tables: Sewage Disposal:
#https://www.census.gov/data/tables/time-series/dec/coh-sewage.html

#edit filepath to the txt file if changed
Census_file <- "https://www2.census.gov/programs-surveys/decennial/tables/time-series/coh-sewage/sewage1990.txt"

################################################################################
#processing/filtering

#load in the file as text for headers, as table for the data (header formatted
#in odd manner)
header <- readLines(Census_file)
Wastewater_1990_state_septic <- read.table(Census_file,skip=grep(header,pattern="US")-1)

#grab the header information in a few ways
category <- unlist(strsplit(header[2],split = " {2,}"))
category <- c(category[1],rep(category[-1],each=2))
category[1] <- "State"
category[grep("Septic.*or",category)] <- paste0(category[grep("Septic.*or",category)]," Cesspool")

unit <- unlist(strsplit(header[5],split = " {2,}"))
unit[1] <- ""
unit <- gsub("Pe.*","Percent",unit)
unit[-1] <- paste0(" (",unit[-1],")")

#combine the 2 parts of the header
header <- paste0(category,unit)

#add the header
colnames(Wastewater_1990_state_septic) <- header

#filter to only the 2 columns needed
Wastewater_1990_state_septic <- Wastewater_1990_state_septic[,c("State","Septic tank or Cesspool (Percent)")]
#convert from e.g., 12.8% to 0.128
Wastewater_1990_state_septic$`Septic tank or Cesspool (Percent)` <- as.numeric(gsub("\\%","",Wastewater_1990_state_septic$`Septic tank or Cesspool (Percent)`))/100

#rename header for simplicity
colnames(Wastewater_1990_state_septic) <- gsub("Septic tank or Cesspool \\(Percent\\)","Septic_Fraction",colnames(Wastewater_1990_state_septic))
################################################################################
#save output to package

usethis::use_data(Wastewater_1990_state_septic, overwrite = TRUE)
