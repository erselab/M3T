## code to download census datasets.  Downloads every year from 2010 to 2025.


output_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Manuscript/All inventory data/Automated/"

for(inventory_year in 2010:2025){
  #Every 10 years the census updates the urban areas.  Just round down to the
  #nearest decade.  2020 and 21 use 2010 still, though 2020 has both and a
  #corrected 2020 census version.
  UAC_year <- floor(as.numeric(substring(as.character(inventory_year),3,4))/10)*10
  if(inventory_year %in% c(2020,2021)){
    UAC_year <- 10
  }
  
  #2010 is formatted differently
  if(inventory_year==2010){
    Census_filenames <- c(paste0(output_directory,"State_Tigerlines/tl_",inventory_year,"_us_state",UAC_year,".shp"),
                          paste0(output_directory,"Urban_Tigerlines/tl_",inventory_year,"_us_uac",UAC_year,".shp"),
                          paste0(output_directory,"County_Tigerlines/tl_",inventory_year,"_us_county",UAC_year,".shp"))
  }else{
    Census_filenames <- c(paste0(output_directory,"State_Tigerlines/tl_",inventory_year,"_us_state.shp"),
                          paste0(output_directory,"Urban_Tigerlines/tl_",inventory_year,"_us_uac",UAC_year,".shp"),
                          paste0(output_directory,"County_Tigerlines/tl_",inventory_year,"_us_county.shp"))
  }
  
  #2011 has no urban tigerlines
  if(inventory_year==2011){
    Census_filenames <- Census_filenames[c(1,3)]
  }
  #URLs for state, county, and urban shapefiles (2010 is formatted
  #differently)
  if(inventory_year==2010){
    Census_FTP_URLs <- c(paste0("https://www2.census.gov/geo/tiger/TIGER",inventory_year,"/STATE/",inventory_year,"/tl_",inventory_year,"_us_state",UAC_year,".zip"),
                         paste0("https://www2.census.gov/geo/tiger/TIGER",inventory_year,"/UA/",inventory_year,"/tl_",inventory_year,"_us_uac",UAC_year,".zip"),
                         paste0("https://www2.census.gov/geo/tiger/TIGER",inventory_year,"/COUNTY/",inventory_year,"/tl_",inventory_year,"_us_county",UAC_year,".zip"))
  }else{
    Census_FTP_URLs <- c(paste0("https://www2.census.gov/geo/tiger/TIGER",inventory_year,"/STATE/tl_",inventory_year,"_us_state.zip"),
                         paste0("https://www2.census.gov/geo/tiger/TIGER",inventory_year,"/UAC/tl_",inventory_year,"_us_uac",UAC_year,".zip"),
                         paste0("https://www2.census.gov/geo/tiger/TIGER",inventory_year,"/COUNTY/tl_",inventory_year,"_us_county.zip"))
  }
  if(inventory_year==2011){
    Census_FTP_URLs <- Census_FTP_URLs[c(1,3)]
  }
  download_location <- tempfile(fileext = ".zip")
  #download each to a temp file then unzip to the input directory
  for(A in 1:length(Census_FTP_URLs)){
    download.file(Census_FTP_URLs[A],download_location,method = "curl",quiet=T)
    utils::unzip(download_location,exdir=file.path(output_directory,c("State_Tigerlines","Urban_Tigerlines","County_Tigerlines")[A]))
    #delete the temp file
    unlink(download_location)
  }
}

download.file("https://www2.census.gov/geo/tiger/GENZ2024/gpkg/cb_2024_us_all_500k.zip",download_location,method = "curl")
utils::unzip(download_location,exdir=file.path(output_directory,"Cartographic_Boundary_500k"))
#delete the temp file
unlink(download_location)




















#instead rewrite the code to work with the bigger xl data - different format,
#same data.  Could be saved to Rdata potentially (check size).  if statement to
#auto-download the year needed (as currently coded), require user EIA API code,
#but default to this.

# SEDS_state_name_list <- c(state_name_list,"US")
# SEDS_filename <- file.path(output_directory,"EIA","SEDS.csv")
# 
# if(!file.exists(SEDS_filename)){
#   #see https://www.eia.gov/opendata/browser/seds.  Filtered to only sectors,
#   #states, and years of interest here.  All in billion BTU/yr units (last
#   #digit B instead of P - short tons)
#   SEDS_URL <- paste0("https://api.eia.gov/v2/seds/data/?frequency=annual&data[0]=value&facets[seriesId][]=CLCCB",
#                      "&facets[seriesId][]=CLEIB&facets[seriesId][]=CLICB&facets[seriesId][]=NGCCB&facets[seriesId][]=NGEIB&facets[seriesId][]=NGICB&facets[seriesId][]=PACCB&facets[seriesId][]=PAEIB&facets[seriesId][]=PAICB&facets[seriesId][]=PARCB&facets[seriesId][]=WDRCB&facets[seriesId][]=WWCCB&facets[seriesId][]=WWEIB&facets[seriesId][]=WWICB",
#                      paste0("&facets[stateId][]=",SEDS_state_name_list,collapse = ""),
#                      "&start=",inventory_year-1,"&end=",inventory_year,
#                      "&sort[0][column]=seriesId&sort[0][direction]=asc&offset=0&api_key=",EIA_API_key)
#   
#   #download directly into R and keep only the data table
#   EIA_raw_data <- Trycatch_downloader(SEDS_URL,output_location=NULL,method="JSON",
#                                       error_message=paste0("\nEIA State Energy Data System data could not be downloaded using API link: ",SEDS_URL,"\n\nmake sure you have an active EIA API key in the config!"))
#   EIA_raw_data <- EIA_raw_data$response$data
#   if(any(EIA_raw_data$period!=inventory_year)){
#     #frustratingly, the API seems to sometimes pull start - end assuming that if
#     #the same year, it should grab that 1 year.  Other times, however, doing
#     #that gives you no data, and you have to set to previous year - year desired
#     #to get the 1 year of data you want.  This if allows the code to handle
#     #either case.  Download previous year - year, but if that includes 2 years
#     #of data, redo the download for just the 1 yr of interest instead.
#     SEDS_URL <- paste0("https://api.eia.gov/v2/seds/data/?frequency=annual&data[0]=value&facets[seriesId][]=CLCCB",
#                        "&facets[seriesId][]=CLEIB&facets[seriesId][]=CLICB&facets[seriesId][]=NGCCB&facets[seriesId][]=NGEIB&facets[seriesId][]=NGICB&facets[seriesId][]=PACCB&facets[seriesId][]=PAEIB&facets[seriesId][]=PAICB&facets[seriesId][]=PARCB&facets[seriesId][]=WDRCB&facets[seriesId][]=WWCCB&facets[seriesId][]=WWEIB&facets[seriesId][]=WWICB",
#                        paste0("&facets[stateId][]=",SEDS_state_name_list,collapse = ""),
#                        "&start=",inventory_year,"&end=",inventory_year,
#                        "&sort[0][column]=seriesId&sort[0][direction]=asc&offset=0&api_key=",EIA_API_key)
#     EIA_raw_data <- Trycatch_downloader(SEDS_URL,output_location=NULL,method="JSON",
#                                         error_message=paste0("\nEIA State Energy Data System data could not be downloaded using API link: ",SEDS_URL,"\n\nmake sure you have an active EIA API key in the config!"))
#     EIA_raw_data <- EIA_raw_data$response$data
#   }
#   write.csv(file = SEDS_filename,x = EIA_raw_data,row.names = F)
# }











