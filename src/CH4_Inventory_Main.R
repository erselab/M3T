#'@title Main function to calculate gridded methane emissions.  Runs sector by
#'  sector according to the config file.
#'
#'@description `CH4_inventory_build` runs multiple other functions to calculate
#'  the gridded methane emissions sector-by-sector using information provided in
#'  the config file and inputs.
#'
#'@details This function will source all other functions and the config, create
#'  the domain from the input data, and download some files (Census tigerlines, 
#'  ghgrp location/name data) that will be needed for multiple sectors.  It will
#'  then use the information in the config file and inputs to run every relevant
#'  sector.  As such, the inputs to this function and the config may require 
#'  user editing.
#'
#'  See references \href{https://doi.org/10.1029/2020JD032974}{Vulcan} and
#'  \href{https://doi.org/10.1002/2017JD027359}{ACES}
#'@param domain data.frame or character.  If data.frame, provides the corner
#'  coordinates of the desired region to process.  The first column would be x
#'  values, the second column y values, and the first row would be the minima,
#'  the second row the maxima.  These must be provided in the appropriate units
#'  for the domain_crs parameter (e.g., decimal degrees for a lat/long
#'  projection).  If character, it indicates the desired region using census
#'  Tigerlines.  It can be a state abbreviation, state name, state FIPS code (as
#'  a character), Urban Area name, or Urban Area Census Code.  For example:
#'  "DE", "Delaware", and "10" are all equivalent and "Long Neck, DE" and
#'  "51202" are equivalent.  Names must match exactly and FIPS or urban area
#'  codes must include leading 0's.  Lists of the names and codes for all urban
#'  areas for the most recent census are available at
#'  \url{https://www.census.gov/programs-surveys/geography/guidance/geo-areas/urban-rural.html}.
#'  Links to previous census urban areas are on the same page.  A list with
#'  state fips codes is available here
#'  \url{https://www.census.gov/library/reference/code-lists/ansi.html}.
#'@param domain_res Numeric providing the resolution for the domain.  Can be
#'  length 1 for equal x and y resolution or length 2 (x, y).
#'@param domain_crs Character providing the projection of the domain in PROJ
#'  string (\url{https://proj.org/en/stable/operations/projections/index.html})
#'  or EPSG codes (\url{https://epsg.io/}) or WKT
#'  (\url{https://docs.ogc.org/is/12-063r5/12-063r5.html}) formats.
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
#'@param inventory_year Numeric indicating the desired year of data to use.  The
#'  closest available will be used for datasets that are not provided annually.
#'@param verbose Logical indicating whether to save additional output.  This
#'  includes easier to read csv files of the data that has been gridded for
#'  multiple sectors as well as sector and subsector visuals.
#'@param EIA_file Character providing the full filepath to the EIA form 176 data
#'  in xlsx format available at
#'  \url{https://www.eia.gov/naturalgas/ngqs/#?report=RP4&year1=2020&year2=2020&company=Name}.
#'@param GHGI_file Character providing the full filepath to the GHGI natural gas
#'  systems annex file in xlsx format available at
#'  \url{https://www.epa.gov/ghgemissions/natural-gas-and-petroleum-systems-ghg-inventory-additional-information-1990-2022-ghg}.
#'@param HIFLD_compressor_file Character providing the full filepath to the
#'  HIFLD natural gas transmission compressor data in csv format.  This data has
#'  been deprecated and is no longer available from HIFLD.
#'@param NLCD_file Character providing the full filepath to the NLCD landcover
#'  data in img format available at
#'  \url{https://www.mrlc.gov/data?f%5B0%5D=category%3ALand%20Cover&f%5B1%5D=project_tax_term_term_parents_tax_term_name%3AAnnual%20NLCD}.
#'@param NALCMS_file Character providing the full filepath to the NALCMS
#'  landcover data in tif format available at
#'  \url{http://www.cec.org/north-american-land-change-monitoring-system/}.
#'@param DMR_file Character providing the full filepath to the DMR wastewater
#'  treatment plant data in csv format available at
#'  \url{https://echo.epa.gov/trends/loading-tool/water-pollution-search}.
#'@param CWNS_file Character providing the full filepath to the CWNS wastewater
#'  treatment plant data.  For 2012 this should be in xlsx format and for 2022
#'  this should be a folder with multiple csv's. The 2012 CWNS is available at
#'  \url{https://ordspub.epa.gov/ords/cwns2012/f?p=cwns2012:25:} and the 2022
#'  CWNS is available at
#'  \url{https://sdwis.epa.gov/ords/sfdw_pub/r/sfdw/cwns_pub/data-download?session=8491118346746}.
#'@param watershed_shapefile Character providing the full filepath to the
#'  watershed polygons in shapefile format available at
#'  \url{http://www.cec.org/north-american-environmental-atlas/watersheds/}.
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
CH4_inventory_build <- function(input_directory,
                                output_directory,
                                code_directory,
                                plot_directory,
                                inventory_year,
                                domain=as.data.frame(cbind(c(-76.65,-73.65),
                                                           c(38.97,40.97))),
                                domain_res=0.01,
                                domain_crs="epsg:4326",
                                ACES_directory,
                                vulcan_directory,
                                verbose,
                                EIA_file,
                                GHGI_file,
                                HIFLD_compressor_file,
                                NLCD_file,
                                NALCMS_file,
                                DMR_file,
                                CWNS_file,
                                watershed_shapefile){

  ################################################################################
  #do not save data for XESMF reprojection in Python - just reproject with
  #Terra.
  XESMF <- F
  ################################################################################
  #load all packages necessary throughout processsing
  
  packagecheck <- c("terra", "ncdf4", "readxl","jsonlite","dplyr")

  #quick way to install only packages that are not already installed and make
  #sure they're up to date
  i=1
  while(i<=length(packagecheck)){
    if(length(find.package(packagecheck[i],quiet = TRUE))<1){
      install.packages(packagecheck[i])
    }else{
      update.packages(oldPkgs = packagecheck[i])
    }
    i <- i+1
  }
  
  #Library all packages.  Ignore startup messages.
  suppressPackageStartupMessages(invisible(lapply(packagecheck, library, character.only=TRUE)))
  rm(packagecheck,i)
  
  #terra = raster dataclasses and processing functions
  #ncdf4 = .nc filetype functions
  #readxl = enables loading in .xlsx or similar filetypes
  #jsonlite = allows simple loading of JSON files, primarily for downloading input data via API
  
  #may need, but may be able to avoid using
  #dplyr = part of tidyverse for cleaner, sometimes more efficient code.
  #        Landfill sector uses piping, group_by, and slice_max from this.
  #        NG dist and stationary comb use piping too
  
  ################################################################################
  #Create input/output directories
  
  dir.create(input_directory,showWarnings = F)
  dir.create(output_directory,showWarnings = F)
  if(verbose){
    dir.create(plot_directory,showWarnings = F)
    dir.create(paste0(plot_directory,"Summed_Sectors"),showWarnings = F)
  }
  dir.create(file.path(input_directory,"GHGRP"),showWarnings = F)
  dir.create(file.path(input_directory,"EIA"),showWarnings = F)
  dir.create(file.path(input_directory,"NEI"),showWarnings = F)
  
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
  source(paste0(code_directory,"Prepare_ACES_Vulcan.R"))
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
    error_text <- paste0(error_text,"\n\nMust set both Process_stationary_combustion and Process_natural_gas_distribution to FALSE or set Use_ACES and/or Use_Vulcan to TRUE to disaggregate stationary combustion and natural gas distribution data")
  }
  
  if(Process_stationary_combustion & (!stationary_combustion_by_state & !stationary_combustion_by_domain)){
    error_found <- TRUE
    error_text <- paste0(error_text,"\n\nMust set Process_stationary_combustion to FALSE or set stationary_combustion_by_state and/or stationary_combustion_by_domain to TRUE to disaggregate stationary combustion data")
  }
  
  if(Process_natural_gas_distribution & (!NG_distribution_by_LDC & !NG_distribution_by_state & !NG_distribution_by_domain)){
    error_found <- TRUE
    error_text <- paste0(error_text,"\n\nMust set Process_natural_gas_distribution to FALSE or set NG_distribution_by_LDC and/or NG_distribution_by_state and/or NG_distribution_by_domain to TRUE to disaggregate natural gas distribution data")
  }
  
  if(Process_natural_gas_distribution & NG_distribution_by_LDC & !file.exists(file.path(input_directory,"/byLDC_merged/byLDC_merged.shp"))){
    error_found <- TRUE
    error_text <- paste0(error_text,"\n\nNG_distribution_by_LDC must be set to FALSE or the NG_distribution_byLDC_prep.R script must be manually adjusted, run, and the output checked before running main")
  }
  
  #if any of the NG distribution GHGI EFs were not clearly specified, default to
  #use the GHGI data.
  NG_dist_GHGI_EFs <- c("GHGI_MnR","GHGI_maintenance","GHGI_meters","GHGI_services")
  if(Process_natural_gas_distribution & any(!NG_dist_GHGI_EFs %in% ls())){
    NG_dist_GHGI_EFs <- NG_dist_GHGI_EFs[!NG_dist_GHGI_EFs %in% ls()]
    for(A in 1:length(NG_dist_GHGI_EFs)){
      assign(NG_dist_GHGI_EFs[A],"GHGI")
    }
    rm(A)
  }
  rm(NG_dist_GHGI_EFs)
  
  #if any of the transmission GHGI EFs were not clearly specified, default to use
  #the GHGI data.
  transmission_GHGI_EFs <- c("GHGI_transmission_compressors","GHGI_Pipeline")
  if(Process_natural_gas_transmission & any(!transmission_GHGI_EFs %in% ls())){
    transmission_GHGI_EFs <- transmission_GHGI_EFs[!transmission_GHGI_EFs %in% ls()]
    for(A in 1:length(transmission_GHGI_EFs)){
      assign(transmission_GHGI_EFs[A],"GHGI")
    }
    rm(A)
  }
  rm(transmission_GHGI_EFs)
  
  
  if(Process_wastewater & (!Wastewater_use_CWNS & !Wastewater_use_DMR)){
    error_found <- TRUE
    error_text <- paste0(error_text,"\n\nMust set Process_wastewater to FALSE or set Wastewater_use_CWNS and/or Wastewater_use_DMR to TRUE as these are the only options available for input data")
  }
  
  if(Process_wastewater & (!Wastewater_Municipal_Method_Moore_linear & !Wastewater_Municipal_Method_Moore_EF & !Wastewater_Municipal_Method_GHGI)){
    error_found <- TRUE
    error_text <- paste0(error_text,"\n\nMust set Process_wastewater to FALSE or set Wastewater_Municipal_Method_Moore_linear and/or Wastewater_Municipal_Method_Moore_EF and/or Wastewater_Municipal_Method_GHGI to TRUE to convert activity data to emissions")
  }
  
  if(Process_wastewater & (!Wastewater_national_septic & !Wastewater_state_septic)){
    error_found <- TRUE
    error_text <- paste0(error_text,"\n\nMust set Process_wastewater to FALSE or set Wastewater_national_septic and/or Wastewater_state_septic to TRUE as these are the only methods available to calculate septic emissions")
  }
  
  if(Process_wetlands_and_inland_waters & (!Use_SOCCR1 & !Use_SOCCR2 & !Use_Wetcharts & !Include_freshwater)){
    error_found <- TRUE
    error_text <- paste0(error_text,"\n\nMust set Process_wetlands_and_inland_waters to FALSE or set Use_SOCCR1 and/or Use_SOCCR2 and/or Use_Wetcharts and/or Include_freshwater to TRUE as these are the only methods available to calculate wetland/inland water emissions")
  }
  
  if(Process_wetlands_and_inland_waters & Use_Wetcharts & (!Use_NLCD & !Use_NALCMS)){
    error_found <- TRUE
    error_text <- paste0(error_text,"\n\nMust set Process_wetlands_and_inland_waters or Use_Wetcharts to FALSE or set Use_NLCD and/or Use_NALCMS to TRUE to disaggregate wetcharts")
  }
  
  if(Process_wastewater & sum(!unique(Wastewater_State_info[,4]) %in% c("scaled","reported"))){
    error_found <- TRUE
    error_text <- paste0(error_text,"\n\nMust set Process_wastewater to FALSE or set Wastewater_State_info method values to either scaled or reported for all entries")
  }
  
  if(error_found){
    stop(error_text)
  }
  
  #check with Israel, need to add checks for data types throughout too
  
  rm(error_found,error_text)
  ################################################################################
  #Get the years for ACES and Vulcan based on the input year.
  
  if(Use_ACES & (Process_stationary_combustion | Process_natural_gas_distribution)){
    #year of ACES data, will be part of the filename
    ACES_year <- (2012:2017)[which.min(abs(2012:2017 - inventory_year))]
    if(inventory_year!=ACES_year){
      cat("ACES does not include",inventory_year,"using",ACES_year,"as the nearest data available\n")
    }
  }
  
  if(Use_Vulcan & (Process_stationary_combustion | Process_natural_gas_distribution)){
    #year of Vulcan data.  Assuming Vulcan v3.0, 1 - 6 corresponding to years
    #2010 - 2015
    vulcan_band <- which.min(abs(2010:2015 - inventory_year))
    if(inventory_year!=(2010:2015)[vulcan_band]){
      cat("Vulcan does not include",inventory_year,"using",(2010:2015)[vulcan_band],"as the nearest data available\n")
    }
  }
  
  ################################################################################
  #create the directories if needed
  
  dir.create(input_directory,showWarnings = F,recursive = T)
  dir.create(output_directory,showWarnings = F,recursive = T)
  dir.create(plot_directory,showWarnings = F,recursive = T)
  ################################################################################
  #save the config and input data from this point to a text file for reference
  #with the output

  sink(file = file.path(output_directory,"Config_settings.txt"),type='output')

  #loop through all objects in the environment, except functions, and save them to
  #the file.
  for(object in setdiff(ls(envir = environment()),ls.str(mode = "function"))){
    cat(object,"=\n")
    temp_data <- get(object)
    #only include row names if they exist, not just row numbers (confusing to
    #read).  Use print for tables/lists (outputs ~formatted) and cat for
    #number/text.
    if(class(temp_data)=="data.frame"){
      if(rownames(temp_data)[1]=="1"){
        print(temp_data,quote = FALSE,row.names=F,width=300)
      }else{
        print(temp_data,quote = FALSE,width=300)
      }
    }else if(class(temp_data)=="list"){
      print(temp_data,quote = FALSE,width=300)
    }else{
      cat(temp_data)
    }
    #add some blank lines between entries for easier reading
    cat("\n")
    cat("\n")
  }
  closeAllConnections()

  ################################################################################
  #writing a function to simplify the code a bit.  Just a trycatch for
  #downloading data that pauses for a second if the download fails, then tries
  #again (5x before giving up).  Includes user update and can handle a file URL
  #or a JSON or can directly vect in a file from online.
  
  #try to download the url, and retry up to 5x with 2s between tries. Based on
  #https://stackoverflow.com/a/60880960
  
  Trycatch_downloader <- function(URL,output_location=NULL,method,error_message){
    counter = 0
    repeat{
      counter=counter+1
      if(counter>1){
        cat("\n",URL,"Download failed, retrying up to 5x")
      }
      info=tryCatch(
        #save to file
        if(method=="save"){
          download.file(URL,destfile=output_location,quiet = T,method="curl")
        #load in as JSON
        }else if(method=="JSON"){
          fromJSON(URL)
        #load in as SpatVector
        }else if(method=="vect"){
          vect(URL)
        },
        warning = function(w) {
          Sys.sleep(2)
          NA
        },
        error = function(e) {
          Sys.sleep(2)
          NA
        }
      )
      #download was successful
      if(!all(is.na(info)) | length(info)>1){
        if(method!="save"){
          return(info)
        }
        break
      }
      #download failed repeatedly
      if(counter>=5){
        stop(error_message)
      }
    }
  }
  assign("Trycatch_downloader",Trycatch_downloader,envir = .GlobalEnv)
  ################################################################################
  #overwrite this function with an identical form, but capture output (just
  #avoids a newline print)
  writeCDF <- function(...){
    invisible(capture.output(terra::writeCDF(...)))
  }
  assign("writeCDF",writeCDF,envir = .GlobalEnv)
  
  cat("Finished loading config and error checking it\n")
  ################################################################################
  #load in Census tigerlines necessary for several functions
  
  #Every 10 years the census updates the urban areas.  Just round down to the
  #nearest decade.
  UAC_year <- floor(as.numeric(substring(as.character(inventory_year),3,4))/10)*10
  
  Census_filenames <- c(paste0(input_directory,"State_Tigerlines/tl_",inventory_year,"_us_state.shp"),
                        paste0(input_directory,"Urban_Tigerlines/tl_",inventory_year,"_us_uac",UAC_year,".shp"),
                        paste0(input_directory,"County_Tigerlines/tl_",inventory_year,"_us_county.shp"))
  
  #only redownload if not already available
  if(!all(file.exists(Census_filenames))){
    #URLs for state, county, and urban shapefiles
    Census_FTP_URLs <- c(paste0("https://www2.census.gov/geo/tiger/TIGER",inventory_year,"/STATE/tl_",inventory_year,"_us_state.zip"),
                         paste0("https://www2.census.gov/geo/tiger/TIGER",inventory_year,"/UAC/tl_",inventory_year,"_us_uac",UAC_year,".zip"),
                         paste0("https://www2.census.gov/geo/tiger/TIGER",inventory_year,"/COUNTY/tl_",inventory_year,"_us_county.zip"))
    download_location <- tempfile(fileext = ".zip")
    #download each to a temp file then unzip to the input directory
    for(A in 1:length(Census_FTP_URLs)){
      Trycatch_downloader(URL = Census_FTP_URLs[A],output_location = download_location,method = "save",
                          error_message = paste("Census tigerlines could not be downloaded using link:",Census_FTP_URLs[A]))
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
  State_Tigerlines <- project(State_Tigerlines,domain_crs)
  Urban_Tigerlines <- project(Urban_Tigerlines,domain_crs)
  County_Tigerlines <- project(County_Tigerlines,domain_crs)
  
  ################################################################################
  #create the domain
  
  if(length(domain_res)==1){
    domain_res <- rep(domain_res,2)
  }
  
  if(class(domain)=="data.frame"){
    # domain=data.frame("lon"=c(-76.65,-73.65),
    #                   "lat"=c(38.97,40.97))
    
    domain <- rast(nrows=diff(range(domain[,2]))/domain_res[2], 
                   ncols=diff(range(domain[,1]))/domain_res[1],
                   xmin=min(domain[,1]), xmax=max(domain[,1]),
                   ymin=min(domain[,2]), ymax=max(domain[,2]),
                   vals=1)
    domain <- as.polygons(ext(domain),crs=domain_crs)
  }else if(class(domain)=="character"){
    # domain <- "DE"
    # domain <- "Delaware"
    # domain <- "10"
    # domain <- "Long Neck, DE"
    # domain <- "51202"
    
    #text is actually text
    if(is.na(suppressWarnings(as.numeric(domain)))){
      if(nchar(domain)==2){
        #2 letters = state abbreviation
        State_Tigerlines <- State_Tigerlines[State_Tigerlines$STUSPS==domain,]
        domain <- State_Tigerlines[State_Tigerlines$STUSPS==domain,]
      }else if(domain %in% State_Tigerlines$NAME){
        #full name of state
        State_Tigerlines <- State_Tigerlines[State_Tigerlines$NAME==domain,]
        domain <- State_Tigerlines[State_Tigerlines$NAME==domain,]
      }else if(domain %in% unlist(values(Urban_Tigerlines[,3]))){
        #full name of an urban area
        domain <- Urban_Tigerlines[unlist(values(Urban_Tigerlines[,3]))==domain,]
      }
    #text is actually a number
    }else{
      if(nchar(domain)==2){
        #State FIPS
        State_Tigerlines <- State_Tigerlines[State_Tigerlines$STATEFP==domain,]
        domain <- State_Tigerlines[State_Tigerlines$STATEFP==domain,]
      }else if(nchar(domain)==5){
        #urban area code
        domain <- Urban_Tigerlines[Urban_Tigerlines$UACE10==domain,]
      }
    }
  }
  
  if(class(domain)!="SpatVector"){
    stop("domain is not set to a state abbreviation, state full name, state FIPS code, urban area abbreviation, urban area full name, urban area census code, data.frame with the corners of a box, or a SpatVector file with the desired polygon.")
  }

  domain_template <- rast(domain,resolution=domain_res,crs=domain_crs,vals=NA)

  ################################################################################
  # Now crop/mask the tigerlines to the domain
  
  #subset to just those relevant for the domain.  For state it's any state that
  #touches the domain at all.  For county, it's only those within the states
  #(i.e., not those touching but outside the states, crop vs mask for vectors).
  State_Tigerlines <- mask(State_Tigerlines,mask=domain)
  Urban_Tigerlines <- mask(Urban_Tigerlines,mask=domain)
  County_Tigerlines <- crop(County_Tigerlines,State_Tigerlines)
  
  #sort by state abbreviation
  State_Tigerlines <- State_Tigerlines[order(State_Tigerlines$STUSPS),]
  
  #save the states in the domain for use in some functions
  state_name_list <- State_Tigerlines$STUSPS
  
  #if there's only 1 state, there's no point in processing by domain and by state
  #as they're identical.
  if(length(state_name_list)==1){
    stationary_combustion_by_domain <- FALSE
    NG_distribution_by_domain <- FALSE
  }
  
  cat("Finished loading in census tigerline shapefiles\n")
  ################################################################################
  #Download the facility details (e.g., location) for GHGRP facilities using the
  #API.  Will be needed for several sectors.

  if(Process_landfills | Process_natural_gas_distribution | Process_natural_gas_transmission | Process_wastewater){
    GHGRP_facility_info_file <- file.path(input_directory,"GHGRP","facility_info.csv")
    
    if(!file.exists(GHGRP_facility_info_file)){
      #download data and read in an R dataframe.  Cannot filter to year as
      #previous year's data is used in some functions.  Cannot filter to state
      #as distribution needs to correct some states as they list headquarters
      #rather than area of operation.  See
      #https://www.epa.gov/enviro/envirofacts-data-service-api
      data_URL <- "https://data.epa.gov/dmapservice/ghg.pub_dim_facility/CSV"
      Trycatch_downloader(URL = data_URL,method = "save",output_location = GHGRP_facility_info_file,
                                                 error_message = paste0("Greenhouse Gas Reporting Program data could not be downloaded using API link: ",data_URL))
    }
    
    #read in the data and update the county fips and zip with leading zeroes
    #where needed.  Neither is used, but this is good practice.
    ghgrp_facility_info <- read.csv(GHGRP_facility_info_file)
    ghgrp_facility_info$county_fips <- sprintf("%05d",ghgrp_facility_info$county_fips)
    ghgrp_facility_info$zip <- sprintf("%05d",ghgrp_facility_info$zip)
  }
  
  cat("Finished loading in GHGRP facility location data.  Running individual sectors now\n\n")
  ################################################################################
  #one check now that we have the state list - ensure that the septic input
  #census data is for the states in the domain and only them.
  if(Process_wastewater & !(all(Wastewater_State_info$State %in% state_name_list) & all(state_name_list %in% Wastewater_State_info$State))){
    stop("\nMust set Wastewater_State_info in the config for the states in the domain, and only the states in the domain - which are:\n",paste(state_name_list,collapse=", "))
  }
  
  ################################################################################
  #Actually run the functions now, based on the config file
  
  #we need to prepare ACES and/or Vulcan for both of these sectors
  # if(Process_natural_gas_distribution | Process_stationary_combustion){
  #   # rm(list=setdiff(ls(),c("input_directory","Use_ACES","Use_Vulcan",
  #   #                        "ACES_year","vulcan_band","State_Tigerlines",
  #   #                        "code_directory")))
  #   # # source(paste0(code_directory,"CH4_inventory_config.R"))
  #   # source(paste0(code_directory,"Prepare_ACES_Vulcan.R"))
  #   # # main_config()
  #   # rm(code_directory)
  #   Prepare_ACES_Vulcan(input_directory,
  #                       Use_ACES,
  #                       Use_Vulcan,
  #                       ACES_year,
  #                       vulcan_band,
  #                       State_Tigerlines)
  # filter_vulcan()
  # }
  if(Process_landfills){
    Municipal_solid_waste(input_directory=input_directory,
                          domain=domain,
                          domain_template=domain_template,
                          state_name_list=state_name_list,
                          output_directory=output_directory,
                          inventory_year=inventory_year,
                          verbose=verbose,
                          GHGI_landfill_total=GHGI_landfill_total,
                          ghgrp_facility_info=ghgrp_facility_info,
                          landfill_ghgrp_reported,
                          landfill_ghgrp_modeled,
                          landfill_ghgrp_collection_efficiency,
                          plot_directory=plot_directory,
                          County_Tigerlines=County_Tigerlines,
                          State_Tigerlines=State_Tigerlines)
  }
  if(Process_natural_gas_distribution){
    NG_distribution(domain=domain,
                    domain_template=domain_template,
                    state_name_list=state_name_list,
                    input_directory=input_directory,
                    output_directory=output_directory,
                    inventory_year=inventory_year,
                    verbose=verbose,
                    ghgrp_facility_info=ghgrp_facility_info,
                    EIA_file = EIA_file,
                    PHMSA_file = PHMSA_file,
                    GHGI_file = GHGI_file,
                    GHGI_MnR = GHGI_MnR,
                    GHGI_maintenance = GHGI_maintenance,
                    GHGI_meters = GHGI_meters,
                    GHGI_services = GHGI_services,
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
                    County_Tigerlines=County_Tigerlines)
  }
  if(Process_natural_gas_transmission){
    Transmission(input_directory=input_directory,
                 GHGI_file=GHGI_file,
                 GHGI_transmission_compressors=GHGI_transmission_compressors,
                 GHGI_Pipeline=GHGI_Pipeline,
                 HIFLD_compressor_file=HIFLD_compressor_file,
                 domain=domain,
                 domain_template=domain_template,
                 ghgrp_facility_info=ghgrp_facility_info,
                 state_name_list=state_name_list,
                 output_directory=output_directory,
                 inventory_year=inventory_year,
                 verbose=verbose,
                 plot_directory=plot_directory,
                 County_Tigerlines=County_Tigerlines,
                 State_Tigerlines=State_Tigerlines)
  }
  if(Process_stationary_combustion){
    Stationary_combustion(input_directory=input_directory,
                          domain=domain,
                          domain_template=domain_template,
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
                          State_Tigerlines=State_Tigerlines)
  }
  if(Process_wastewater){
    NLCD_open_and_low_int(NLCD_file=NLCD_file,
                          domain=domain,
                          domain_template=domain_template,
                          state_name_list=state_name_list,
                          State_Tigerlines=State_Tigerlines,
                          output_directory=output_directory)

    Wastewater(input_directory=input_directory,
               DMR_file=DMR_file,
               CWNS_file=CWNS_file,
               output_directory=output_directory,
               Wastewater_use_CWNS=Wastewater_use_CWNS,
               Wastewater_use_DMR=Wastewater_use_DMR,
               Wastewater_Municipal_Method_Moore_linear=Wastewater_Municipal_Method_Moore_linear,
               Wastewater_Municipal_Method_Moore_EF=Wastewater_Municipal_Method_Moore_EF,
               Wastewater_Municipal_Method_GHGI=Wastewater_Municipal_Method_GHGI,
               domain=domain,
               domain_template=domain_template,
               ghgrp_facility_info=ghgrp_facility_info,
               inventory_year=inventory_year,
               National_wastewater_info=National_wastewater_info,
               Wastewater_State_info=Wastewater_State_info,
               GHGI_national_wastewater_nonseptic=GHGI_national_wastewater_nonseptic,
               GHGI_national_wastewater_septic=GHGI_national_wastewater_septic,
               GHGI_septic_EF=GHGI_septic_EF,
               Total_national_open_or_low_int_area=Total_national_open_or_low_int_area,
               State_Tigerlines=State_Tigerlines,
               County_Tigerlines=County_Tigerlines,
               verbose=verbose,
               plot_directory=plot_directory)
  }
  if(Process_wetlands_and_inland_waters){
    if(Use_Wetcharts){
      Disaggregate_Wetcharts(input_directory=input_directory,
                             output_directory=output_directory,
                             domain=domain,
                             domain_template=domain_template,
                             verbose=verbose,
                             inventory_year=inventory_year,
                             plot_directory=plot_directory,
                             State_Tigerlines=State_Tigerlines,
                             County_Tigerlines=County_Tigerlines,
                             Use_NLCD=Use_NLCD,
                             Use_NALCMS=Use_NALCMS,
                             NLCD_file=NLCD_file,
                             NALCMS_file=NALCMS_file,
                             Wetcharts_model_subset)
    }
    NWI_Wetland_fraction(input_directory=input_directory,
                         output_directory=output_directory,
                         domain=domain,
                         domain_template=domain_template,
                         state_name_list=state_name_list,
                         Use_SOCCR1=Use_SOCCR1,
                         Use_SOCCR2=Use_SOCCR2,
                         Include_freshwater=Include_freshwater)
    
    SOCCR_Wetlands(output_directory=output_directory,
                   plot_directory=plot_directory,
                   domain=domain,
                   domain_template=domain_template,
                   Use_SOCCR1=Use_SOCCR1,
                   Use_SOCCR2=Use_SOCCR2,
                   Include_freshwater=Include_freshwater,
                   Wetland_EFs=Wetland_EFs,
                   verbose=verbose,
                   County_Tigerlines=County_Tigerlines,
                   State_Tigerlines=State_Tigerlines,
                   watershed_shapefile=watershed_shapefile)
  }
  if(Incorporate_remaining_sectors_from_gridded_EPA){
    Prepare_GEPA(inventory_year=inventory_year,
                 input_directory=input_directory,
                 output_directory=output_directory,
                 domain=domain,
                 plot_directory=plot_directory,
                 County_Tigerlines=County_Tigerlines,
                 State_Tigerlines=State_Tigerlines,
                 domain_template=domain_template,
                 verbose=verbose)
  }
  if(Combine_sectors){
    stop("Combine Sectors hasn't been coded yet.  Do not set to TRUE in config")
  }
}

#example quick plots
##sf chloropleth
# plot(all_merge_sf_LCC_state["res_wood_ER"])
##terra chloropleth, same colorscale
# plot(all_merge_LCC_state,"res_wood_ER",col=sf.colors(13),breaks=13)



