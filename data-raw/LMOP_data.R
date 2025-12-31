## code to prepare `LMOP_data` dataset goes here

library(readxl)
output_directory <- "D:/MMMT STUFF/All inventory data/Automated/"
# output_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Manuscript/All inventory data/Automated/"

################################################################################
#Download, load in, and prepare LMOP data

#download the webpage and load in the HTML
data_URL <- paste0("https://www.epa.gov/lmop/landfill-technical-data")
download_dest <- tempfile(fileext = ".html")
download.file(data_URL,download_dest,quiet = T)
HTML_data <- readChar(download_dest,file.info(download_dest)$size)

#Search for https:// - any 60 or fewer characters - landfilllmopdata.xlsx in
#the HTML_data.  The link had about 40 characters between https:// and
#landfilllmopdata in the current version, but this should identify any
#version if the format is reasonably consistent.  The data URL webpage must
#still be up to date though.  
Matchtext <- regexpr("https://.{1,60}landfilllmopdata.xlsx",HTML_data)
data_URL2 <- substring(HTML_data,Matchtext[1],Matchtext[1]+attr( Matchtext , "match.length")-1)

#Use regex to save the year of the dataset as part of the download for
#clarity
LMOP_yr <- substr(data_URL2,regexpr("20??",data_URL2)[1],regexpr("20??",data_URL2)[1]+3)
LMOP_file <- file.path(output_directory,paste0(LMOP_yr,"_LMOP_landfill_only.xlsx"))
download.file(data_URL2,LMOP_file,quiet=T,method="curl")
unlink(download_dest)

# Read in LMOP and remove those in GHGRP.  Note facilities that used to report
# to GHGRP and stopped with a valid reason are being considered LMOP
# facilities in this approach.  
LMOP <- readxl::read_xlsx(LMOP_file,sheet="LMOP Database",col_names = T)

#This has some nans in, remove those
LMOP_data <- subset(LMOP,!is.na(Latitude))
LMOP_data <- LMOP_data[,c("GHGRP ID","Latitude","Longitude","Landfill Name","Year Landfill Opened")]

usethis::use_data(LMOP_data, overwrite = TRUE)
