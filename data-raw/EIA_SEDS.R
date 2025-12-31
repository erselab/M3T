## code to prepare `EIA_SEDS` dataset goes here

#API key to access SEDS data API.  The API is described at
#https://www.eia.gov/opendata/ and one can register for a key with a
#link on the right hand side of this page.
EIA_API_key <- "1kLep4UApTZKwdOrDkW6J8qlO0niiw8ej0JPliyc"
library(jsonlite)
################################################################################
#download SEDS data

#CONUS subset including DC
SEDS_state_name_list <- state.abb[!state.abb %in% c("AK","AS","MP","PR","HI","GU","VI")]
SEDS_state_name_list <- sort(c(SEDS_state_name_list,"DC"))

SEDS_filename <- tempfile(fileext = ".csv")

#go up to this year - years outside the available data will just be ignored
SEDS_years <- 2011:format(Sys.Date(),format = "%Y")

#see https://www.eia.gov/opendata/browser/seds.  Filtered to only sectors,
#states, and years of interest here.  All in billion BTU/yr units (last
#digit B instead of P - short tons for each series ID)
SEDS_URL <- paste0("https://api.eia.gov/v2/seds/data/?frequency=annual&data[0]=value&facets[seriesId][]=CLCCB",
                   "&facets[seriesId][]=CLEIB&facets[seriesId][]=CLICB&facets[seriesId][]=NGCCB&facets[seriesId][]=NGEIB&facets[seriesId][]=NGICB&facets[seriesId][]=PACCB&facets[seriesId][]=PAEIB&facets[seriesId][]=PAICB&facets[seriesId][]=PARCB&facets[seriesId][]=WDRCB&facets[seriesId][]=WWCCB&facets[seriesId][]=WWEIB&facets[seriesId][]=WWICB",
                   paste0("&facets[stateId][]=",SEDS_state_name_list,collapse = ""),
                   "&start=",SEDS_years[1],"&end=",tail(SEDS_years,1),
                   "&sort[0][column]=seriesId&sort[0][direction]=asc&offset=0&api_key=",EIA_API_key)

#download directly into R and keep only the data table
EIA_raw_data <- jsonlite::fromJSON(SEDS_URL)
EIA_SEDS <- EIA_raw_data$response$data
################################################################################
#save
usethis::use_data(EIA_SEDS, overwrite = TRUE)
