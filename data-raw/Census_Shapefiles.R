## code to prepare census tigerlines (state, county, urban) and cartographic
## boundary files for all available years starting in 2010

output_directory <- "D:/MMMT STUFF/All inventory data/Automated/"
# output_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Manuscript/All inventory data/Automated/"

library(terra)
################################################################################
#First see what's available by searching the FTP as an html
download_location <- tempfile(fileext = ".html")
Trycatch_downloader("https://www2.census.gov/geo/tiger/",download_location,method = "save")
Census_file_list=readLines(download_location)
pattern <- "TIGER[[:digit:]]{4}/"
Census_years <- grep(pattern,Census_file_list,value=T)
Census_years <- as.numeric(substr(Census_years,regexpr(pattern,Census_years)+5,regexpr(pattern,Census_years)+8))

#keep only those relevant for the package (2011+)
Census_years <- Census_years[Census_years>2010]
invisible(file.remove(download_location))

################################################################################
#download data
for(inventory_year in Census_years){
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
  #newer files are formatted slightly differently
  if(inventory_year>2023){
    Census_FTP_URLs[2] <- paste0("https://www2.census.gov/geo/tiger/TIGER",inventory_year,"/UAC",UAC_year,"/tl_",inventory_year,"_us_uac",UAC_year,".zip")
  }
  
  download_location <- tempfile(fileext = ".zip")
  #download each to a temp file then unzip to the input directory
  for(A in 1:length(Census_FTP_URLs)){
    download.file(Census_FTP_URLs[A],download_location,quiet=T,overwrite=T)
    utils::unzip(download_location,exdir=file.path(output_directory,c("State_Tigerlines","Urban_Tigerlines","County_Tigerlines")[A]))
    #delete the temp file
    unlink(download_location)
  }
  cat("\rFinished downloading",inventory_year,"- there are",sum(Census_years>inventory_year),"years left               ")
}

#only need a single file here - purely for visualization
download.file("https://www2.census.gov/geo/tiger/GENZ2024/gpkg/cb_2024_us_all_500k.zip",download_location,quiet=T,overwrite=T)
utils::unzip(download_location,exdir=file.path(output_directory,"Cartographic_Boundary_500k"))
#delete the temp file
unlink(download_location)

################################################################################
#basic processing

#load in each
state_files <- list.files(file.path(output_directory,"State_Tigerlines"),pattern="*\\.shp$",full.names = T)
urban_files <- list.files(file.path(output_directory,"Urban_Tigerlines"),pattern="*\\.shp$",full.names = T)
county_files <- list.files(file.path(output_directory,"County_Tigerlines"),pattern="*\\.shp$",full.names = T)
CB_file <- file.path(output_directory,"Cartographic_Boundary_500k","cb_2024_us_all_500k.gpkg")

State_Tigerlines <- terra::vect(state_files[1])
Urban_Tigerlines <- terra::vect(urban_files[1])
County_Tigerlines <- terra::vect(county_files[1])
State_CB <- terra::vect(CB_file,layer="cb_2024_us_state_500k")

#subset to CONUS - urban is just for defining the domain so no need
State_Tigerlines <- State_Tigerlines[!State_Tigerlines$STUSPS %in% c("AK","AS","MP","PR","HI","GU","VI"),]
County_Tigerlines <- terra::mask(County_Tigerlines,State_Tigerlines)
State_CB <- terra::mask(State_CB,State_Tigerlines)

#output names
State_output <- file.path(output_directory,"combined_state_tigerlines.gpkg")
County_output <- file.path(output_directory,"combined_county_tigerlines.gpkg")
Urban_output <- file.path(output_directory,"combined_urban_tigerlines.gpkg")
CB_output <- file.path(output_directory,"Cropped_state_CB.gpkg")

#save as gpkg to add to
writeVector(State_Tigerlines,State_output,overwrite=T,layer='2011')
writeVector(County_Tigerlines,County_output,overwrite=T,layer='2012')
writeVector(Urban_Tigerlines,Urban_output,overwrite=T,layer='2011')
writeVector(State_CB,CB_output,overwrite=T,layer='2024')

#loop through each yr and add as a new layer in the gpkg
for(A in 2:length(state_files)){
  yr <- strsplit(basename(state_files[A]),"_")[[1]][2]
  
  State_Tigerlines <- terra::vect(state_files[A])
  Urban_Tigerlines <- terra::vect(urban_files[A])
  
  State_Tigerlines <- State_Tigerlines[!State_Tigerlines$STUSPS %in% c("AK","AS","MP","PR","HI","GU","VI"),]
  
  writeVector(State_Tigerlines,State_output,insert=T,overwrite=T,layer=yr)
  writeVector(Urban_Tigerlines,Urban_output,insert=T,overwrite=T,layer=yr)
  
  #no county data for 2011, so skip A that's 2012 for the others
  if(A>2){
    County_Tigerlines <- terra::vect(county_files[A-1])
    County_Tigerlines <- terra::mask(County_Tigerlines,State_Tigerlines)
    writeVector(County_Tigerlines,County_output,insert=T,overwrite=T,layer=yr)
  }
  cat("\rFinished cropping and combining",yr,"- there are",length(state_files)-A,"left                       ")
}

################################################################################
#delete raw files
unlink(dirname(state_files[1]),recursive = T)
unlink(dirname(urban_files[1]),recursive = T)
unlink(dirname(county_files[1]),recursive = T)
unlink(CB_file)

