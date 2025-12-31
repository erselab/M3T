## code to prepare `GHGRP_facility_data` dataset goes here

filename <- tempfile(fileext = ".csv")

#download data and read in an R dataframe.  Cannot filter to year as
#previous year's data is used in some functions.  Cannot filter to state
#as distribution needs to correct some states as they list headquarters
#rather than area of operation.  See
#https://www.epa.gov/enviro/envirofacts-data-service-api
data_URL <- "https://data.epa.gov/dmapservice/ghg.pub_dim_facility/CSV"
utils::download.file(data_URL, destfile = filename,quiet=T)

#read in the data
GHGRP_facility_data <- utils::read.csv(filename)

relevant_cols <- c("facility_id","facility_name","latitude","longitude","year","state")
GHGRP_facility_data <- GHGRP_facility_data[,relevant_cols]

usethis::use_data(GHGRP_facility_data, overwrite = TRUE)
