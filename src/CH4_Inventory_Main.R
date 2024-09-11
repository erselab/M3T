#'@title Main function to calculate gridded methane emissions.  Runs sector by
#'  sector according to the config file.
#'
#'@description `CH4_inventory_build` runs multiple other functions to calculate
#'  the gridded methane emissions sector-by-sector using information provided in
#'  the config file and inputs.
#'
#'@details This function will source all other functions and the config, create
#'  the domain from the input data in the case that a SpatRaster was not
#'  provided, and download some files (Census tigerlines) that will be needed
#'  for multiple sectors.  It will then use the information in the config file
#'  and inputs to run every relevant sector.  As such, the inputs to this
#'  function and the config may require user editing.
#'
#'  See references
#'  \href{https://doi.org/10.1029/2020JD032974}{Vulcan} and
#'  \href{https://doi.org/10.1002/2017JD027359}{ACES}
#'@param focus_city Character or numeric or a vector of characters or numerics.
#'  Each character must exactly match the Census name, which can be found at
#'  \url{https://www.census.gov/programs-surveys/geography/guidance/geo-areas/urban-rural.html}
#'  for the 2020 census and at
#'  \url{https://www2.census.gov/geo/pdfs/maps-data/maps/reference/2010UAUC_List.pdf}
#'  for the 2010 census.  Years rely on the naming scheme of the most recent
#'  census, so e.g., 2018 would rely on the 2010 names.
#'
#'  Each numeric must exactly match the code given to the urban area, also
#'  detailed in the above documents.
#'
#'  This will be used to subset the urban area census outlines and used in any
#'  visualizations.
#'@param domain SpatRaster providing the desired output grid, including the
#'  desired resolution and coordinate reference system or a 2x2 data frame.  For
#'  the data frame, the first column would be x values, the second column y
#'  values, and the first row would be the minima, the second row the maxima.
#'  This is used to build a SpatRaster.
#'@param domain_res Numeric providing the resolution for the domain.  Can be
#'  length 1 or 2 (x,y).
#'@param domain_crs Character providing the projection of the domain
#'@param input_directory Character providing the full filepath to load/save
#'  input data
#'@param plot_directory Character providing the full filepath to save figures.
#'  Only relevant if verbose = TRUE.
#'@param code_directory Character providing the full filepath to source all
#'  other functions and the config.
#'@param ACES_directory Character providing the full filepath to a folder with
#'  sectoral Anthropogenic Carbon Emission System (ACES) inventory files that
#'  have been averaged into annual files.  ACES is available at
#'  \url{https://doi.org/10.3334/ORNLDAAC/1943}.
#'@param vulcan_directory Character providing the full filepath to a folder with
#'  annual, sectoral Vulcan inventory files.  Vulcan v3.0 is available at
#'  \url{https://doi.org/10.3334/ORNLDAAC/1741}.
#'@param inventory_year Numeric indicating the desired year of data to use.
#'@param verbose Logical indicating whether to save additional output.  This
#'  includes easier to read csv files of the data that has been gridded for
#'  multiple sectors as well as sector and subsector visuals.
#'@returns Nothing is returned from the function, but there will be frequent
#'  user updates as multiple other functions are run and output files created
#'  for the various sectors.
#'@examples
#' CH4_inventory_build(input_directory="G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw_data_rewrite/",
#'                     output_directory="G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_rewrite/",
#'                     code_directory="~/../../Kristian/Desktop/methane_inventory/src/",
#'                     plot_directory="G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Figures_rewrite/",
#'                     focus_city="Philadelphia, PA--NJ--DE--MD",
#'                     inventory_year=2019,
#'                     domain=as.data.frame(cbind(c(-76.65,-73.65),
#'                                                c(38.97,40.97))),
#'                     domain_res=0.01,
#'                     domain_crs="epsg:4326",
#'                     ACES_directory="G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Inventories/ACES_v2.0",
#'                     vulcan_directory="G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Inventories/Vulcan_v3.0",
#'                     verbose=TRUE)
#'@author Joe Pitt, \email{madeup@@wisc.edu}
#'@author Kris Hajny, \email{blank@@fake.edu}
#'@author Israel Lopez-Coto, \email{test@@test.edu}
#'@export


#Function to run all other functions as desired to build a CH4 inventory one
#sector at a time.  Requires the user set a config file to determine which
#variants for some sectors are run among other things.

#some defaults for a Philly centered domain with NAD83 crs
# CH4_inventory_build <- function(input_directory="G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw_data_rewrite/",
#                                 output_directory="G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_rewrite/",
#                                 code_directory="~/../../Kristian/Desktop/methane_inventory/src/",
#                                 plot_directory="G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Figures_rewrite/",
#                                 focus_city="Philadelphia, PA--NJ--DE--MD",
#                                 inventory_year=2019,
#                                 domain=as.data.frame(cbind(c(-76.65,-73.65),
#                                                            c(38.97,40.97))),
#                                 domain_res=0.01,
#                                 domain_crs="epsg:4326",
#                                 ACES_directory="G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Inventories/ACES_v2.0",
#                                 vulcan_directory="G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Inventories/Vulcan_v3.0",
#                                 verbose=TRUE){
code_directory="~/../../Kristian/Desktop/methane_inventory/src/"

input_directory="G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw_data_rewrite/"
output_directory="G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_rewrite/"
plot_directory="G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Figures_rewrite/"
#if desired.  Must either be UACE code entered as numeric or exact text match
#entered as character. Too many cities have similar/identical names otherwise.
#Can be > 1 city. These can be found here (see "List of 2020 Census Urban
#Areas")
#(https://www.census.gov/programs-surveys/geography/guidance/geo-areas/urban-rural.html)
#for 2020 - 2020 and here
#(https://www2.census.gov/geo/pdfs/maps-data/maps/reference/2010UAUC_List.pdf)
#for 2010 - 2019
focus_city="Philadelphia, PA--NJ--DE--MD"

inventory_year=2019
domain=as.data.frame(cbind(c(-76.65,-73.65),
                           c(38.97,40.97)))
domain_res=0.01
domain_crs="epsg:4326" #lat/long
ACES_directory="G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Inventories/ACES_v2.0"
vulcan_directory="G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Inventories/Vulcan_v3.0"
verbose=TRUE

#an easy way to switch to stress-testing code.  Changes year to 2016, resolution
#to 0.1 and reprojects to a cylindrical projection (or the Vulcan one) to make
#sure functions still run without failing.
testmode <- F
testmode_vulcan <- F
################################################################################
#User input

#Philly centered domain
# Domain_bounding_box <- cbind(c(-76.65,-73.65),
#                              c(38.97,40.97))

#ballpark domain focused on the Northeast Corridor (a little SW of the Richmond
#tower to a little NE of Boston Urban Outline)
# Domain_bounding_box <- cbind(c(-77.88,-70.39),
#                              c(37.31,43.15))

#do not save data for XESMF reprojection in Python - just reproject with
#Terra.  
XESMF <- F

################################################################################
#load all packages necessary throughout processsing

packagecheck <- c("terra", "ncdf4", "readxl","jsonlite","dplyr")
# 
# #install each package and library them.  Ensures all are up to date. 
# lapply(packagecheck,install.packages,dependencies=T,quiet=T)
# lapply(packagecheck,library,character.only=T)
# 
# rm(packagecheck)

#quick way to install only packages that are not already installed
i=1
while(i<=length(packagecheck)){
  if(length(find.package(packagecheck[i],quiet = TRUE))<1){
    install.packages(packagecheck[i],repos="https://repo.miserver.it.umich.edu/cran/")
  }
  i <- i+1
}

suppressPackageStartupMessages(invisible(lapply(packagecheck, library, character.only=TRUE)))
rm(packagecheck,i)

#terra = raster dataclasses and processing functions
#ncdf4 = .nc filetype functions
#readxl = enables loading in .xlsx or similar filetypes
#jsonlite = allows simple loading of JSON files, primarily for downloading input data via API


#may need, but may be able to avoid using
#dplyr = part of tidyverse for cleaner, sometimes more efficient code.
#        Landfill sector uses piping, group_by, and slice_max from this.


#shouldn't need anymore, may need to code out in some scripts still
#geosphere = some processing functions for spatial data
#fBasics = timpallete colorscale
#pracma = haversine function to calculate distances from lat/long points.  terra can do this
#rvest and httr = easier access to html data
#rgdal = dead package for spatial processing
#maps = basic maps, other datasets are better
#sf and sp = many spatial dataclasses
#raster = raster dataclasses and processing functions

################################################################################
#Create input/output directories

dir.create(input_directory,showWarnings = F)
dir.create(output_directory,showWarnings = F)
if(verbose){
  dir.create(plot_directory,showWarnings = F)
  dir.create(paste0(plot_directory,"Summed_Sectors"),showWarnings = F)
}

################################################################################
#Get the years for ACES and Vulcan based on the input year.

ACES_year <- (2012:2017)[which.min(abs(2012:2017 - inventory_year))]
#year of ACES data, will be part of the filename

vulcan_band <- which.min(abs(2010:2015 - inventory_year))
#year of Vulcan data.  Assuming Vulcan v3.0, 1 - 6 corresponding to years 2010 -
#2015

################################################################################
#load in the many relevant functions and the config file

#Load in a function to disaggregate total emissions using ACES/Vulcan or both
#within sub-domains (state, entire domain)
source(paste0(code_directory,"Inventory_based_disaggregation.R"))

#Load in a few functions for consistent, basic plotting
source(paste0(code_directory,"Plotting_individual_sectors.R"))

#Load in the config file full of emission factors and other details needed for
#processing some sectors
source(paste0(code_directory,"CH4_inventory_config.R"))

#load in the functions for each sector (only run later if config set
#accordingly)
source(paste0(code_directory,"Landfill_emissions_r1.R"))
source(paste0(code_directory,"stationary_combustion_r4.R"))
source(paste0(code_directory,"NLCD_fractions_by_state.R"))
source(paste0(code_directory,"WWTP_emissions_r3.R"))
source(paste0(code_directory,"NG_transmission_emissions_r1.R"))
source(paste0(code_directory,"NG_distribution_emissions_r4.R"))
source(paste0(code_directory,"Prepare_GEPA.R"))
source(paste0(code_directory,"WETCHARTS_downscaling.R"))
source(paste0(code_directory,"Wetland_fraction_r2_WIP.R"))
source(paste0(code_directory,"Wetland_emissions_r2.R"))

################################################################################
#some early error checking, mostly looking at config.  Are the options
#acceptable, properly formatted, etc.?

#run the config and pull all user-set variables from it
main_config()

error_text <- ""
error_found <- FALSE

#each of the below is just a combination of config options that is unusable
#(e.g., all activity data choices for a sector set to F).  Add to error text so
#all config errors can be presented at once.
if((!Use_ACES & !Use_Vulcan) & (Process_stationary_combustion | Process_natural_gas_distribution)){
  error_found <- TRUE
  error_text <- paste0(error_text,"\n\nMust set both Process_stationary_combustion and Process_natural_gas_distribution to FALSE or set either Use_ACES or Use_Vulcan to TRUE to disaggregate stationary combustion and natural gas distribution data")
}

if(Process_stationary_combustion & (!stationary_combustion_by_state & !stationary_combustion_by_domain)){
  error_found <- TRUE
  error_text <- paste0(error_text,"\n\nMust set Process_stationary_combustion to FALSE or set either stationary_combustion_by_state or stationary_combustion_by_domain to TRUE to disaggregate stationary combustion data")
}

if(Process_stationary_combustion & (!stationary_combustion_by_state & !stationary_combustion_by_domain)){
  error_found <- TRUE
  error_text <- paste0(error_text,"\n\nMust set Process_stationary_combustion to FALSE or set either stationary_combustion_by_state or stationary_combustion_by_domain to TRUE to disaggregate stationary combustion data")
}

if(Process_natural_gas_distribution & (!NG_distribution_by_LDC & !NG_distribution_by_state & !NG_distribution_by_domain)){
  error_found <- TRUE
  error_text <- paste0(error_text,"\n\nMust set Process_natural_gas_distribution to FALSE or set either NG_distribution_by_LDC or NG_distribution_by_state or NG_distribution_by_domain to TRUE to disaggregate natural gas distribution data")
}

if(Process_natural_gas_distribution & (!NG_distribution_by_LDC & !NG_distribution_by_state & !NG_distribution_by_domain)){
  error_found <- TRUE
  error_text <- paste0(error_text,"\n\nMust set Process_natural_gas_distribution to FALSE or set either NG_distribution_by_LDC or NG_distribution_by_state or NG_distribution_by_domain to TRUE to disaggregate natural gas distribution data")
}

if(Process_wastewater & (!Wastewater_use_CWNS & !Wastewater_use_DMR)){
  error_found <- TRUE
  error_text <- paste0(error_text,"\n\nMust set Process_wastewater to FALSE or set either Wastewater_use_CWNS or Wastewater_use_DMR to TRUE as these are the only options available for input data")
}

if(Process_wastewater & (!Wastewater_Municipal_Method_Moore_linear & !Wastewater_Municipal_Method_Moore_EF & !Wastewater_Municipal_Method_GHGI)){
  error_found <- TRUE
  error_text <- paste0(error_text,"\n\nMust set Process_wastewater to FALSE or set either Wastewater_Municipal_Method_Moore_linear or Wastewater_Municipal_Method_Moore_EF or Wastewater_Municipal_Method_GHGI to TRUE to convert activity data to emissions")
}

if(Process_wastewater & (!Wastewater_Municipal_Method_Moore_linear & !Wastewater_Municipal_Method_Moore_EF & !Wastewater_Municipal_Method_GHGI)){
  error_found <- TRUE
  error_text <- paste0(error_text,"\n\nMust set Process_wastewater to FALSE or set either Wastewater_Municipal_Method_Moore_linear or Wastewater_Municipal_Method_Moore_EF or Wastewater_Municipal_Method_GHGI to TRUE to convert activity data to emissions")
}

if(Process_wastewater & (!Wastewater_national_septic & !Wastewater_state_septic)){
  error_found <- TRUE
  error_text <- paste0(error_text,"\n\nMust set Process_wastewater to FALSE or set either Wastewater_national_septic or Wastewater_state_septic to TRUE as these are the only methods available to calculate septic emissions")
}

if(Process_wetlands_and_inland_waters & (!Use_SOCCR1 & !Use_SOCCR2 & !Use_Wetcharts)){
  error_found <- TRUE
  error_text <- paste0(error_text,"\n\nMust set Process_wetlands_and_inland_waters to FALSE or set either Use_SOCCR1 or Use_SOCCR2 or Use_Wetcharts to TRUE as these are the only methods available to calculate wetland emissions")
}

if(Process_wetlands_and_inland_waters & Use_Wetcharts & (!Use_NLCD & !Use_NALCMS)){
  error_found <- TRUE
  error_text <- paste0(error_text,"\n\nMust set Process_wetlands_and_inland_waters or Use_Wetcharts to FALSE or set either Use_NLCD or Use_NALCMS to TRUE to disaggregate wetcharts")
}

if(Process_wastewater & sum(c("scaled","reported")!=unique(Wastewater_State_info[,4]))){
  error_found <- TRUE
  error_text <- paste0(error_text,"\n\nMust set Process_wastewater to FALSE or set Wastewater_State_info method values to either scaled or reported for all entries")
}
if(error_found){
  stop(error_text)
}

#check with Israel, need to add checks for data types throughout too

################################################################################
#create the domain and set it to all NaN
if(testmode){
  domain_res <- domain_res*10
  inventory_year <- 2016
}
if(length(domain_res)==1){
  domain_res <- rep(domain_res,2)
}

if(class(domain)=="SpatRaster"){
  values(domain) <- NaN
}else if(class(domain)=="data.frame"){
  domain <- rast(nrows=diff(range(domain[,2]))/domain_res[2], 
                 ncols=diff(range(domain[,1]))/domain_res[1],
                 xmin=min(domain[,1]), xmax=max(domain[,1]),
                 ymin=min(domain[,2]), ymax=max(domain[,2]), 
                 crs=domain_crs)
  rm(domain_res,domain_crs)
}
if(testmode_vulcan){
  domain <- project(domain,"+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") #Lambert Conic Conformal, same as Vulcan/ACES
}else if(testmode){
  domain <- project(domain,"epsg:4087") #Equidistant Cylindrical - nothing we use has this, significantly different spatially, good test option
}
################################################################################
#load in Census tigerlines necessary for several functions

#Every 10 years the census updates the urban areas
if(inventory_year>=2010 & inventory_year<2020){
  UAC_year <- 10
}else if(inventory_year>=2020 & Inventory_year<2030){
  UAC_year <- 20
}

Census_filenames <- c(paste0(input_directory,"State_Tigerlines/tl_",inventory_year,"_us_state.shp"),
                      paste0(input_directory,"Urban_Tigerlines/tl_",inventory_year,"_us_uac",UAC_year,".shp"),
                      paste0(input_directory,"County_Tigerlines/tl_",inventory_year,"_us_county.shp"))

if(!all(file.exists(Census_filenames))){
  #URLs for state, county, and urban shapefiles
  Census_FTP_URLs <- c(paste0("https://www2.census.gov/geo/tiger/TIGER",inventory_year,"/STATE/tl_",inventory_year,"_us_state.zip"),
                       paste0("https://www2.census.gov/geo/tiger/TIGER",inventory_year,"/UAC/tl_",inventory_year,"_us_uac",UAC_year,".zip"),
                       paste0("https://www2.census.gov/geo/tiger/TIGER",inventory_year,"/COUNTY/tl_",inventory_year,"_us_county.zip"))
  download_location <- tempfile(fileext = ".zip")
  #download each to a temp file then unzip to the input directory
  for(A in 1:length(Census_FTP_URLs)){
    download.file(Census_FTP_URLs[A],destfile = download_location,quiet = T)
    unzip(download_location,exdir=file.path(input_directory,c("State_Tigerlines","Urban_Tigerlines","County_Tigerlines")[A]))
  }
  #delete the temp file
  unlink(download_location)
  rm(Census_FTP_URLs,download_location,A)
}

#load them in
State_Tigerlines <- vect(Census_filenames[1])
Urban_Tigerlines <- vect(Census_filenames[2])
County_Tigerlines <- vect(Census_filenames[3])

#project to match the domain (crs)
State_Tigerlines <- project(State_Tigerlines,domain)
Urban_Tigerlines <- project(Urban_Tigerlines,domain)
County_Tigerlines <- project(County_Tigerlines,domain)

#subset to just those relevant for the domain (speedier).  For state it's any
#state that touches the domain at all.  For county, it's only those within the
#states (i.e., not just touching the states, crop vs mask for vectors).
State_Tigerlines <- mask(State_Tigerlines,mask=as.polygons(domain))
Urban_Tigerlines <- mask(Urban_Tigerlines,mask=State_Tigerlines)
County_Tigerlines <- crop(County_Tigerlines,State_Tigerlines)

#sort by state abbreviation
State_Tigerlines <- State_Tigerlines[order(State_Tigerlines$STUSPS),]

#save the states in the domain for use in some functions
state_name_list <- State_Tigerlines$STUSPS

#grab the urban area tigerlines for just the focus city
if(class(focus_city)=="numeric"){
  #can't use $ for urban tigerlines as column name is UACE10 for 2010 Census,
  #UACE20 for 2020 Census
  focus_city_tigerlines <- terra::subset(Urban_Tigerlines,as.numeric(unlist(Urban_Tigerlines[[1]])) %in% focus_city)
}else if(class(focus_city)=="character"){
  focus_city_tigerlines <- terra::subset(Urban_Tigerlines,Urban_Tigerlines$NAME10 %in% focus_city)
}else{
  focus_city_tigerlines <- "none"
}

rm(UAC_year,Census_filenames,focus_city)
################################################################################
#Actually run the functions now, based on the config file

if(Process_landfills){
  # rm(list=setdiff(ls(),c("input_directory","domain","output_directory",
  #                        "inventory_year","verbose","clear","state_name_list",
  #                        "GHGI_landfill_total","code_directory",
  #                        "plot_directory","County_Tigerlines",
  #                        "State_Tigerlines","focus_city_tigerlines")))
  # source(paste0(code_directory,"CH4_inventory_config.R"))
  # source(paste0(code_directory,"Landfill_emissions_r1.R"))
  # source(paste0(code_directory,"Plotting_individual_sectors.R"))
  # main_config()
  # rm(code_directory)
  Municipal_solid_waste(LMOP_file=file.path(input_directory,
                                            "lmopdata(Mar_24)_landfill_only.xlsx"),
                        domain=domain,
                        state_name_list=state_name_list,
                        output_directory=output_directory,
                        inventory_year=inventory_year,
                        verbose=verbose,
                        GHGI_landfill_total=GHGI_landfill_total,
                        plot_directory=plot_directory,
                        County_Tigerlines=County_Tigerlines,
                        State_Tigerlines=State_Tigerlines,
                        focus_city_tigerlines=focus_city_tigerlines)
}
if(Process_natural_gas_distribution){
  # rm(list=setdiff(ls(),c("input_directory","domain","output_directory",
  #                        "inventory_year","verbose","clear","state_name_list",
  #                        "code_directory",
  #                        "plot_directory","County_Tigerlines",
  #                        "State_Tigerlines","focus_city_tigerlines",
  #                        "ACES_directory","vulcan_directory","ACES_year",
  #                        "vulcan_band")))
  # source(paste0(code_directory,"CH4_inventory_config.R"))
  # source(paste0(code_directory,"NG_distribution_emissions_r4.R"))
  # source(paste0(code_directory,"Plotting_individual_sectors.R"))
  # source(paste0(code_directory,"Inventory_based_disaggregation.R"))
  # main_config()
  # rm(code_directory)
  # filter_vulcan()
  NG_distribution(domain=domain,
                  state_name_list=state_name_list,
                  output_directory=output_directory,
                  inventory_year=inventory_year,
                  verbose=verbose,
                  EIA_file = file.path(input_directory,"176 Type of Operations and Sector Items.xlsx"),
                  PHMSA_file = file.path(input_directory,"annual_gas_distribution_2010_present/annual_gas_distribution_2019.xlsx"),
                  GHGI_file = file.path(input_directory,"2022_ghgi_natural_gas_systems_annex36_tables.xlsx"),
                  GHGI_EF_sheet = "3.6-2",
                  GHGI_Activity_sheet = "3.6-7",
                  State_Tigerlines=State_Tigerlines,
                  NG_distribution_by_LDC = NG_distribution_by_LDC,
                  NG_distribution_by_state = NG_distribution_by_state,
                  NG_distribution_by_domain = NG_distribution_by_domain,
                  GHGI_natural_gas_pipeline_emission_factors=GHGI_natural_gas_pipeline_emission_factors,
                  natural_gas_post_meter_emission_factor=natural_gas_post_meter_emission_factor,
                  Use_ACES=Use_ACES,
                  Use_Vulcan=Use_Vulcan,
                  ACES_directory=ACES_directory,
                  vulcan_directory=vulcan_directory,
                  ACES_year=ACES_year,
                  vulcan_band=vulcan_band,
                  plot_directory=plot_directory,
                  County_Tigerlines=County_Tigerlines,
                  focus_city_tigerlines=focus_city_tigerlines)
  # filter_vulcan
}
if(Process_natural_gas_transmission){
  # rm(list=setdiff(ls(),c("input_directory","domain","output_directory",
  #                        "inventory_year","verbose","clear","state_name_list",
  #                        "code_directory",
  #                        "plot_directory","County_Tigerlines",
  #                        "State_Tigerlines","focus_city_tigerlines")))
  # source(paste0(code_directory,"CH4_inventory_config.R"))
  # source(paste0(code_directory,"NG_transmission_emissions_r1.R"))
  # source(paste0(code_directory,"Plotting_individual_sectors.R"))
  # main_config()
  # rm(code_directory)
  Transmission(GHGI_file=file.path(input_directory,"2022_ghgi_natural_gas_systems_annex36_tables.xlsx"),
               GHGI_Emissions_sheet="3.6-1",
               GHGI_Activity_sheet="3.6-7",
               domain=domain,
               state_name_list=state_name_list,
               output_directory=output_directory,
               inventory_year=inventory_year,
               verbose=verbose,
               plot_directory=plot_directory,
               County_Tigerlines=County_Tigerlines,
               State_Tigerlines=State_Tigerlines,
               focus_city_tigerlines=focus_city_tigerlines)
}
if(Process_stationary_combustion){
  # rm(list=setdiff(ls(),c("input_directory","domain","output_directory",
  #                        "inventory_year","verbose","clear","state_name_list",
  #                        "GHGI_landfill_total","code_directory",
  #                        "plot_directory","County_Tigerlines",
  #                        "State_Tigerlines","focus_city_tigerlines",
  #                        "ACES_directory","vulcan_directory","ACES_year",
  #                        "vulcan_band")))
  # source(paste0(code_directory,"CH4_inventory_config.R"))
  # source(paste0(code_directory,"stationary_combustion_r4.R"))
  # source(paste0(code_directory,"Plotting_individual_sectors.R"))
  # source(paste0(code_directory,"Inventory_based_disaggregation.R"))
  # main_config()
  # rm(code_directory)
  Stationary_combustion(NEI_file=file.path(input_directory,"NEI_2017.xlsx"),
                        domain=domain,
                        state_name_list=state_name_list,
                        output_directory=output_directory,
                        inventory_year=inventory_year,
                        verbose=verbose,
                        County_Tigerlines=County_Tigerlines,
                        Use_ACES=Use_ACES,
                        Use_Vulcan=Use_Vulcan,
                        ACES_directory=ACES_directory,
                        vulcan_directory=vulcan_directory,
                        ACES_year=ACES_year,
                        vulcan_band=vulcan_band,
                        stationary_combustion_by_state=stationary_combustion_by_state,
                        stationary_combustion_by_domain=stationary_combustion_by_domain,
                        stationary_combustion_GHGI_data=stationary_combustion_GHGI_data,
                        stationary_combustion_emission_factors=stationary_combustion_emission_factors,
                        EIA_API_key=EIA_API_key,
                        plot_directory=plot_directory,
                        State_Tigerlines=State_Tigerlines,
                        focus_city_tigerlines=focus_city_tigerlines)
}
if(Process_wastewater){
  # rm(list=setdiff(ls(),c("domain","output_directory",
  #                        "clear","state_name_list","code_directory","State_Tigerlines")))
  # source(paste0(code_directory,"CH4_inventory_config.R"))
  # source(paste0(code_directory,"NLCD_fractions_by_state.R"))
  # main_config()
  # rm(code_directory)
  NLCD_open_and_low_int(NLCD_file=file.path("G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Shapefiles/nlcd_2019_land_cover_l48_20210604/nlcd_2019_land_cover_l48_20210604.img"),
                        domain=domain,
                        state_name_list=state_name_list,
                        State_Tigerlines=State_Tigerlines,
                        output_directory=output_directory)

  # rm(list=setdiff(ls(),c("input_directory","domain","output_directory",
  #                        "inventory_year","verbose","clear","state_name_list",
  #                        "code_directory",
  #                        "plot_directory","County_Tigerlines",
  #                        "State_Tigerlines","focus_city_tigerlines")))
  # source(paste0(code_directory,"CH4_inventory_config.R"))
  # source(paste0(code_directory,"WWTP_emissions_r3.R"))
  # source(paste0(code_directory,"Plotting_individual_sectors.R"))
  # main_config()
  # rm(code_directory)
  Wastewater(DMR_file=file.path(input_directory,'DMR_2022_from_8_10_2023.csv'),
             CWNS_file=file.path(input_directory,'CWNS_merged_data_2012_KH.xlsx'),
             output_directory=output_directory,
             Wastewater_Municipal_method=Wastewater_Municipal_method,
             Wastewater_Municipal_file=Wastewater_Municipal_file,
             domain=domain,
             state_name_list=state_name_list,
             inventory_year=inventory_year,
             National_wastewater_info=National_wastewater_info,
             Wastewater_State_info=Wastewater_State_info,
             GHGI_national_wastewater_nonseptic=GHGI_national_wastewater_nonseptic,
             GHGI_national_wastewater_septic=GHGI_national_wastewater_septic,
             GHGI_septic_EF=GHGI_septic_EF,
             Total_national_open_or_low_int_area=Total_national_open_or_low_int_area,
             State_Tigerlines=State_Tigerlines,
             County_Tigerlines=County_Tigerlines,
             focus_city_tigerlines=focus_city_tigerlines,
             verbose=verbose,
             plot_directory=plot_directory)
}
if(Process_wetlands_and_inland_waters){
  if(Use_Wetcharts){
    # rm(list=setdiff(ls(),c("input_directory","domain","output_directory",
    #                        "inventory_year","verbose","clear",
    #                        "code_directory",
    #                        "plot_directory","County_Tigerlines",
    #                        "State_Tigerlines","focus_city_tigerlines")))
    # source(paste0(code_directory,"CH4_inventory_config.R"))
    # source(paste0(code_directory,"WETCHARTS_downscaling.R"))
    # source(paste0(code_directory,"Plotting_individual_sectors.R"))
    # main_config()
    # rm(code_directory)
    Disaggregate_Wetcharts(input_directory=input_directory,
                           output_directory=output_directory,
                           domain=domain,
                           verbose=verbose,
                           inventory_year=inventory_year,
                           plot_directory=plot_directory,
                           State_Tigerlines=State_Tigerlines,
                           County_Tigerlines=County_Tigerlines,
                           focus_city_tigerlines=focus_city_tigerlines,
                           Use_NLCD=Use_NLCD,
                           Use_NALCMS=Use_NALCMS,
                           NLCD_file=file.path("G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Shapefiles/nlcd_2019_land_cover_l48_20210604/nlcd_2019_land_cover_l48_20210604.img"),
                           NALCMS_file=file.path("G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Shapefiles/NALCMS_2020_land_cover/NA_NALCMS_landcover_2020_30m/data/NA_NALCMS_landcover_2020_30m.tif"),
                           Wetcharts_model_subset)
  }
  # rm(list=setdiff(ls(),c("input_directory","domain","output_directory",
  #                        "state_name_list","code_directory")))
  # source(paste0(code_directory,"CH4_inventory_config.R"))
  # source(paste0(code_directory,"Wetland_fraction_r2_WIP.R"))
  # main_config()
  # rm(code_directory)
  NWI_Wetland_fraction(input_directory=input_directory,
                       output_directory=output_directory,
                       domain=domain,
                       state_name_list=state_name_list,
                       Use_SOCCR1=Use_SOCCR1,
                       Use_SOCCR2=Use_SOCCR2,
                       Include_freshwater=Include_freshwater)
  # rm(list=setdiff(ls(),c("plot_directory","domain","output_directory",
  #                        "code_directory","verbose","County_Tigerlines",
  #                        "State_Tigerlines","focus_city_tigerlines")))
  # source(paste0(code_directory,"CH4_inventory_config.R"))
  # source(paste0(code_directory,"Wetland_emissions_r2.R"))
  # source(paste0(code_directory,"Plotting_individual_sectors.R"))
  # main_config()
  # rm(code_directory)
  SOCCR_Wetlands(output_directory=output_directory,
                 plot_directory=plot_directory,
                 domain=domain,
                 Use_SOCCR1=Use_SOCCR1,
                 Use_SOCCR2=Use_SOCCR2,
                 Include_freshwater=Include_freshwater,
                 Wetland_EFs=Wetland_EFs,
                 verbose=verbose,
                 County_Tigerlines=County_Tigerlines,
                 State_Tigerlines=State_Tigerlines,
                 focus_city_tigerlines=focus_city_tigerlines,
                 watershed_shapefile=file.path(input_directory,"watersheds_shapefile/watershed_p_v2.shp"))
}
if(Incorporate_remaining_sectors_from_gridded_EPA){
  # rm(list=setdiff(ls(),c("input_directory","domain","output_directory",
  #                        "inventory_year","clear","code_directory")))
  # source(paste0(code_directory,"Prepare_GEPA.R"))
  # rm(code_directory)
  Prepare_GEPA(inventory_year=inventory_year,
               input_directory=input_directory,
               output_directory=output_directory,
               domain=domain)
}
if(Combine_sectors){
  
}

# }

#example quick plots
##sf chloropleth
# plot(all_merge_sf_LCC_state["res_wood_ER"])
##terra chloropleth, same colorscale
# plot(all_merge_LCC_state,"res_wood_ER",col=sf.colors(13),breaks=13)



