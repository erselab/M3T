## code to prepare `EIA_SEDS` dataset.

#API key to access SEDS data API.  The API is described at
#https://www.eia.gov/opendata/ and one can register for a key with a
#link on the right hand side of this page.
EIA_API_key <- "1kLep4UApTZKwdOrDkW6J8qlO0niiw8ej0JPliyc"
################################################################################
#download SEDS data

#CONUS subset including DC
SEDS_state_name_list <- datasets::state.abb[!datasets::state.abb %in% c("AK","AS","MP","PR","HI","GU","VI")]
SEDS_state_name_list <- c(sort(c(SEDS_state_name_list,"DC")),"US")

SEDS_filename <- tempfile(fileext = ".csv")

#go up to this year - years outside the available data will just be ignored
SEDS_years <- 2011:format(Sys.Date(),format = "%Y")

EIA_SEDS_combined <- data.frame()

#see https://www.eia.gov/opendata/browser/seds.  Filtered to only sectors,
#states, and years of interest here.  All in billion BTU/yr units (last digit B
#instead of P - short tons for each series ID). Download by year to avoid
#download file size limit of 5000
for(A in 1:length(SEDS_years)){
  SEDS_URL <- paste0("https://api.eia.gov/v2/seds/data/?frequency=annual&data[0]=value&facets[seriesId][]=CLCCB",
                     "&facets[seriesId][]=CLEIB&facets[seriesId][]=CLICB&facets[seriesId][]=NGCCB&facets[seriesId][]=NGEIB&facets[seriesId][]=NGICB&facets[seriesId][]=PACCB&facets[seriesId][]=PAEIB&facets[seriesId][]=PAICB&facets[seriesId][]=PARCB&facets[seriesId][]=WDRCB&facets[seriesId][]=WWCCB&facets[seriesId][]=WWEIB&facets[seriesId][]=WWICB",
                     paste0("&facets[stateId][]=",SEDS_state_name_list,collapse = ""),
                     "&start=",SEDS_years[A]-1,"&end=",SEDS_years[A]+1,
                     "&sort[0][column]=seriesId&sort[0][direction]=asc&offset=0&api_key=",EIA_API_key)

  #https://api.eia.gov/v2/seds/data/?frequency=annual&data[0]=value&start=2011&end=2011&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000

  #download directly into R and keep only the relevant info in the data table
  EIA_raw_data <- jsonlite::fromJSON(SEDS_URL)
  EIA_SEDS <- EIA_raw_data$response$data
  if(!is.null(nrow(EIA_SEDS))){
    if(nrow(EIA_SEDS)==5000){
      stop("Hit API limit - need to download piecemeal or update URL")
    }
  }else{
    break
  }

  #frustratingly, the API seems inconsistent. Sometimes 2022 - 2022 grabs
  #only 2022.  Other times doing that gives you no data, and you have to set
  #to 2021 - 2022 or 2022 - 2023 to get the data you want.  This is just to
  #handle these cases.  Download desired year +/-1, then filter to just the
  #year of interest.
  EIA_SEDS <- EIA_SEDS[EIA_SEDS$period==SEDS_years[A],]
  EIA_SEDS_combined <- rbind(EIA_SEDS_combined,EIA_SEDS)
  cat("\rFinished downloading year",A,"of",length(SEDS_years),"               ")
  
  #add a slight delay to avoid throttling of the API
  Sys.sleep(3)
}



if(!all(EIA_SEDS_combined$unit=="Billion Btu")){
  stop("Units not as expected - some are not billion btu")
}


#unneeded columns
EIA_SEDS_combined[,c("unit","stateDescription")] <- NULL
################################################################################
#save

EIA_SEDS <- EIA_SEDS_combined
usethis::use_data(EIA_SEDS, overwrite = TRUE)


