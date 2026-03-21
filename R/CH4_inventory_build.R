#'@title Main function to calculate gridded methane emissions.  Runs sector by
#'  sector according to the config file.
#'
#'@description `CH4_inventory_build` runs multiple other functions to calculate
#'  the gridded methane emissions sector-by-sector using information provided in
#'  the config file and inputs.
#'
#'@details This function will call multiple internal functions and use
#'  \code{\link{M3T_config}} to create gridded methane inventories. Internet
#'  access is required so that the necessary datasets can be downloaded either
#'  directly from the source or from a companion Zenodo contiaining
#'  pre-processed data unless all "Source_" variables in
#'  \code{\link{M3T_config}} are set to filepaths that point to local copies of
#'  the needed data.
#'@param domain data.frame or character.  If data.frame, provides the corner
#'  coordinates of the desired region to process.  The first column would be x
#'  values, the second column y values, and the first row would be the minima,
#'  the second row the maxima.  These must be provided in the appropriate units
#'  for the domain_crs parameter (e.g., decimal degrees for a lat/long
#'  projection).
#'
#'  If character, it can be defined based on
#'  \href{https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html}{U.S.
#'  Census Bureau Tigerlines}.  It can be a state abbreviation, state name,
#'  state FIPS code (as a character), Urban Area name, or Urban Area Census
#'  Code.  For example: "DE", "Delaware", and "10" are all equivalent and "Long
#'  Neck, DE" and "51202" are equivalent. It can also be "CONUS" to run for the
#'  entire continental United States or "custom" for a domain created by the
#'  user interactively. Lastly, it can be a filepath pointing to a polygon file
#'  that can be interpreted by the terra package. Names must match exactly and
#'  FIPS or urban area codes must include leading 0's.  Lists of the names and
#'  codes for all urban areas for the most recent census are available
#'  \href{https://www.census.gov/programs-surveys/geography/guidance/geo-areas/urban-rural.html}{here}.
#'  Links to previous census urban areas are on the same page.  A list with
#'  state fips codes is available
#'  \href{https://www.census.gov/library/reference/code-lists/ansi.html}{here}.
#'@param domain_res Numeric providing the resolution for the domain.  Can be
#'  length 1 for equal x and y resolution or length 2 (x, y).
#'@param domain_crs Character providing the projection of the domain in
#'  \href{https://proj.org/en/stable/operations/projections/index.html}{PROJ
#'  string} or \href{https://epsg.io/}{EPSG codes} or
#'  \href{https://docs.ogc.org/is/12-063r5/12-063r5.html}{WKT} formats. Commonly
#'  desired crs include "epsg:4326" for lat/long and "+proj=lcc +lat_0=40
#'  +lon_0=-97 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
#'  for a m grid in the lambert conformal conical projection used by some CO2
#'  inventories for the continental US
#'@param run_directory Character providing the full filepath to load/save input
#'  and output data.  Subfolders will be created.
#'@param inventory_year Numeric indicating the desired year of data to use.  The
#'  closest available will be used if unavailable with a user update.
#'@param verbose Logical indicating whether to save sector and subsector
#'  visuals.
#'@return Nothing is returned from the function, but there will be frequent user
#'  updates as multiple other functions are run and output files created for the
#'  various sectors.
#'@export
#'@examplesIf interactive() && curl::has_internet()
#' CH4_inventory_build(run_directory=tempdir(),
#'                     inventory_year=2019,
#'                     domain="RI",
#'                     domain_res=1,
#'                     domain_crs="epsg:4326",
#'                     verbose=F)
#'@seealso [M3T_config] Generates the config function with user-editable
#'  settings used throughout processing.


CH4_inventory_build <- function(run_directory,
                                inventory_year,
                                domain,
                                domain_res,
                                domain_crs="epsg:4326",
                                verbose=FALSE){
  ################################################################################
  #quick internet check given much of the package requires it if any M3T_config
  #values are M3T or download
  if(!curl::has_internet() & any(M3T_get_config() %in% c("M3T","download"))){
    stop("Internet is required to access the datasets needed to do these analyses. Either connect to the internet or use M3T_set_config() to instead point to locally available files for each of the below. See '?M3T_config' for details.\n",
         paste0(names(M3T_get_config())[(M3T_get_config() %in% c("M3T","download"))],collapse="\n"))
  }
  ################################################################################
  #Create input/output directories
  
  #very minor, but saw issues running vector layers with ~/.. type paths.
  #Convert here just in case.
  run_directory <- normalizePath(run_directory)
  
  input_directory <- file.path(run_directory,"in")
  output_directory <- file.path(run_directory,"out")
  dir.create(input_directory,showWarnings = F,recursive = T)
  dir.create(output_directory,showWarnings = F)
  if(verbose){
    plot_directory <- file.path(run_directory,"plots")
    dir.create(plot_directory,showWarnings = F)
    dir.create(file.path(plot_directory,"Summed_Sectors"),showWarnings = F)
  }else{
    plot_directory <- ""
  }
  dir.create(file.path(input_directory,"GHGRP"),showWarnings = F)
  dir.create(file.path(input_directory,"EIA"),showWarnings = F)
  dir.create(file.path(input_directory,"NEI"),showWarnings = F)
  ################################################################################
  #save run settings to a text file for reference with the output
  
  sink(file = file.path(input_directory,"Run_settings.txt"),type='output')
  
  cat("Run Date: ",as.character(Sys.Date()),
      "\nM3T package version:",as.character(utils::packageVersion("M3T")),"\n\n")
  
  #loop through all objects in the environment, except functions, and save them to
  #the file.
  for(object in dplyr::setdiff(ls(envir = environment()),utils::ls.str(mode = "function"))){
    cat(object,"=\n")
    temp_data <- get(object)
    #only include row names if they exist, not just row numbers (confusing to
    #read).  Use print for tables/lists (outputs ~formatted) and cat for
    #number/text.
    if(isa(temp_data,"data.frame")){
      if(rownames(temp_data)[1]=="1"){
        print(temp_data,quote = FALSE,row.names=F,width=300)
      }else{
        print(temp_data,quote = FALSE,width=300)
      }
    }else if(isa(temp_data,"list")){
      print(temp_data,quote = FALSE,width=300)
    }else{
      cat(temp_data)
    }
    #add some blank lines between entries for easier reading
    cat("\n")
    cat("\n")
  }
  
  cat(rep("\n",4),"\nConfig settings:\n")
  #repeat almost exactly for config
  for(object in names(M3T_config)){
    cat(object,"=\n")
    temp_data <- M3T_get_config(object)
    #only include row names if they exist, not just row numbers (confusing to
    #read).  Use print for tables/lists (outputs ~formatted) and cat for
    #number/text.
    if(isa(temp_data,"data.frame")){
      if(rownames(temp_data)[1]=="1"){
        print(temp_data,quote = FALSE,row.names=F,width=300)
      }else{
        print(temp_data,quote = FALSE,width=300)
      }
    }else if(isa(temp_data,"list")){
      for(A in 1:length(temp_data)){
        if(A!=1){cat("\n")}
        cat("\tsubset",A,":\n\t",temp_data[[A]])
        }
    }else{
      cat(temp_data)
    }
    #add some blank lines between entries for easier reading
    cat("\n")
    cat("\n")
  }
  
  closeAllConnections()
  ################################################################################
  #change options for use within this function
  
  #change the datatype to higher res and turn off progress bars
  M3T_terra <- terra::terraOptions(print=F)
  terra::terraOptions(datatype=M3T_config$Terra_datatype,
                      progress=M3T_config$Terra_progress)
  
  #increase the timeout, particularly important if using downloads
  M3T_timeout <- options("timeout")
  options("timeout"=M3T_config$Base_timeout)
  
  #Reset these options to M3Ts once finished
  on.exit(options("timeout"=M3T_timeout), add = TRUE)
  on.exit(terra::terraOptions(datatype=M3T_terra$datatype,
                              progress=M3T_terra$progress), add = TRUE)
  
  ################################################################################
  #Download necessary data from Zenodo
  
  if(any(M3T_get_config()=="M3T")){
    #UPDATE TO ZENODO
    
    invisible(file.copy(list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Manuscript/All inventory data/Prepared inventory data/M3T_Zenodo_data/Processed/",full.names = T),
                        input_directory,recursive=T))
    
  }
  ################################################################################
  #function to download vulcan v4.0 files if M3T_config$Source_Vulcan="download"
  
  Download_vulcan <- function(){
    #Zenodo API
    #https://zenodo.org/records/15446748
    
    #the only sectors needed
    File_list <- c("v4.res.co2.usa.1km.lcc.mn.allyrs.zip","v4.com.co2.usa.1km.lcc.mn.allyrs.zip",
                   "v4.ind.co2.usa.1km.lcc.mn.allyrs.zip","v4.elc.co2.usa.1km.lcc.mn.allyrs.zip")
    
    Vulcan_URL <- paste0("https://zenodo.org/api/records/15446748/files/",File_list,"/content")
    zip_file <- tempfile(fileext = ".zip")
    
    #unzip each to the appropriate folder and delete zip file
    for(A in 1:length(File_list)){
      Trycatch_downloader(URL=Vulcan_URL[A],output_location=zip_file,method="save")
      utils::unzip(zip_file,exdir=vulcan_directory,overwrite = T)
      unlink(zip_file)
    }
    
    #also include the readme
    Vulcan_URL <- paste0("https://zenodo.org/api/records/15446748/files/readme.Vulcan.1km.V4.0.May.20.2025.pdf/content")
    Trycatch_downloader(URL=Vulcan_URL,
                        output_location=file.path(vulcan_directory,"readme_Vulcan_1km_V4.0_May_20_2025.pdf"),
                        method="save")
  }
  ################################################################################
  #Get the years for ACES and Vulcan based on the input year.
  
  if(M3T_config$Use_ACES & (M3T_config$Process_stationary_combustion | M3T_config$Process_natural_gas_distribution)){
    #year of ACES data, will be part of the filename
    ACES_year <- (2012:2017)[which.min(abs(2012:2017 - inventory_year))]
    # ACES_year <- 2017
    if(inventory_year!=ACES_year){
      cat("ACES does not include",inventory_year,"using",ACES_year,"as the nearest data available\n")
    }
    
    ACES_directory <- file.path(input_directory,"ACES V2.0")
    if(M3T_config$Source_ACES=="M3T"){
      #UPDATE TO ZENODO
      aces_res <- terra::rast(file.path(ACES_directory,paste0('ACES_annual_Residential_',ACES_year,'.nc')))
      aces_com <- terra::rast(file.path(ACES_directory,paste0('ACES_annual_Commercial_',ACES_year,'.nc')))
      aces_ind <- terra::rast(file.path(ACES_directory,paste0('ACES_annual_Industrial_',ACES_year,'.nc')))
      aces_elec <- terra::rast(file.path(ACES_directory,paste0('ACES_annual_Elec_',ACES_year,'.nc')))
    }else{
      dir.create(ACES_directory,showWarnings = F)
      invisible(file.copy(list.files(M3T_config$Source_ACES,full.names = T),
                          ACES_directory,overwrite=T,recursive=T))
      
      aces_res <- terra::rast(file.path(ACES_directory,paste0('ACES_annual_Residential_',ACES_year,'.nc')))
      aces_com <- terra::rast(file.path(ACES_directory,paste0('ACES_annual_Commercial_',ACES_year,'.nc')))
      aces_ind <- terra::rast(file.path(ACES_directory,paste0('ACES_annual_Industrial_',ACES_year,'.nc')))
      aces_elec <- terra::rast(file.path(ACES_directory,paste0('ACES_annual_Elec_',ACES_year,'.nc')))
    }
  }
  
  if(M3T_config$Use_Vulcan & (M3T_config$Process_stationary_combustion | M3T_config$Process_natural_gas_distribution)){
    #year of Vulcan v4.0 data.
    vulcan_year <- (2010:2021)[which.min(abs(2010:2021 - inventory_year))]
    if(inventory_year!=vulcan_year){
      cat("Vulcan does not include",inventory_year,"using",(2010:2015)[vulcan_year],"as the nearest data available\n")
    }
    
    vulcan_directory <- file.path(input_directory,"Vulcan_v4.0")
    dir.create(vulcan_directory,showWarnings = F)
    #only download if files not already available
    if(length(list.files(vulcan_directory)) < 4){
      if(M3T_config$Source_Vulcan=="download"){
        cat("Downloading sectoral Vulcan v4.0 CO2 emissions maps now.\n\n")
        Download_vulcan()
      }else{
        invisible(file.copy(list.files(M3T_config$Source_Vulcan,full.names = T),
                            vulcan_directory,overwrite=T,recursive=T))
      }
    }
    vu_res <- terra::rast(file.path(vulcan_directory,paste0("v4.res.co2.usa.1km.lcc.mn.",vulcan_year,".tif")))
    vu_com <- terra::rast(file.path(vulcan_directory,paste0("v4.com.co2.usa.1km.lcc.mn.",vulcan_year,".tif")))
    vu_ind <- terra::rast(file.path(vulcan_directory,paste0("v4.ind.co2.usa.1km.lcc.mn.",vulcan_year,".tif")))
    vu_elec <- terra::rast(file.path(vulcan_directory,paste0("v4.elc.co2.usa.1km.lcc.mn.",vulcan_year,".tif")))
  }
  ################################################################################
  #some early error checking, mostly looking at config.  Are the options
  #acceptable, properly formatted, etc.?
  
  error_text <- "The below errors were found based on the config data:\n"
  error_found <- FALSE
  
  #each of the below is just a combination of config options that is unusable
  #(e.g., all activity data choices for a sector set to F).  Add to error text so
  #all config errors can be presented at once.
  
  #ID any source options that are 0 characters or not text
  problem_source_entries <- names(M3T_config)[grepl(names(M3T_config),pattern="Source_.*")]
  problem_source_entries <- problem_source_entries[sapply(problem_source_entries,M3T_get_config)==0 | 
                                                     sapply(problem_source_entries,function(x){!isa(M3T_get_config(x),"character")})]
  if(length(problem_source_entries)>0){
    error_found <- TRUE
    problem_source_entries <- paste(problem_source_entries,collapse=", ")
    error_text <- paste0(error_text,"\n\nMust set all data sources to \"M3T\", \"download\", or a file path. ",problem_source_entries," are set incorrectly (either not text in \"\" or empty).")
  }
  
  if(M3T_config$Process_landfills & !(isa(M3T_config$GHGI_landfill_total,"numeric") | M3T_config$GHGI_landfill_total=="GHGI")){
    error_found <- TRUE
    error_text <- paste0(error_text,"\n\nMust set M3T_config$Process_landfills to FALSE or set M3T_config$GHGI_landfill_total in config to a number or \"GHGI\"")
  }
  
  if(M3T_config$Process_landfills & (!M3T_config$landfill_ghgrp_reported & !M3T_config$landfill_ghgrp_modeled & !M3T_config$landfill_ghgrp_collection_efficiency)){
    error_found <- TRUE
    error_text <- paste0(error_text,"\n\nMust set M3T_config$Process_landfills to FALSE or set M3T_config$landfill_ghgrp_reported and/or M3T_config$landfill_ghgrp_modeled and/or M3T_config$landfill_ghgrp_collection_efficiency to TRUE to calculate municipal landfill emissions")
  }
  
  if((!M3T_config$Use_ACES & !M3T_config$Use_Vulcan) & (M3T_config$Process_stationary_combustion | M3T_config$Process_natural_gas_distribution)){
    error_found <- TRUE
    error_text <- paste0(error_text,"\n\nMust set both M3T_config$Process_stationary_combustion and M3T_config$Process_natural_gas_distribution to FALSE or set M3T_config$Use_ACES and/or M3T_config$Use_Vulcan to TRUE to disaggregate stationary combustion and natural gas distribution data")
  }
  
  if(M3T_config$Process_stationary_combustion & (!M3T_config$stationary_combustion_by_state & !M3T_config$stationary_combustion_by_domain)){
    error_found <- TRUE
    error_text <- paste0(error_text,"\n\nMust set M3T_config$Process_stationary_combustion to FALSE or set M3T_config$stationary_combustion_by_state and/or M3T_config$stationary_combustion_by_domain to TRUE to disaggregate stationary combustion data")
  }
  
  if(M3T_config$Process_natural_gas_distribution & (!M3T_config$NG_distribution_by_LDC & !M3T_config$NG_distribution_by_state & !M3T_config$NG_distribution_by_domain)){
    error_found <- TRUE
    error_text <- paste0(error_text,"\n\nMust set M3T_config$Process_natural_gas_distribution to FALSE or set M3T_config$NG_distribution_by_LDC and/or M3T_config$NG_distribution_by_state and/or M3T_config$NG_distribution_by_domain to TRUE to disaggregate natural gas distribution data")
  }
  
  if(M3T_config$Process_natural_gas_distribution & M3T_config$NG_distribution_by_LDC & !file.exists(file.path(input_directory,"/byLDC_merged/byLDC_merged.shp"))){
    error_found <- TRUE
    error_text <- paste0(error_text,"\n\nM3T_config$NG_distribution_by_LDC must be set to FALSE or the NG_distribution_byLDC_prep.R script must be manually adjusted, run, and the output checked before running main")
  }
  
  if(M3T_config$Process_wastewater & (!M3T_config$Wastewater_use_CWNS & !M3T_config$Wastewater_use_DMR)){
    error_found <- TRUE
    error_text <- paste0(error_text,"\n\nMust set M3T_config$Process_wastewater to FALSE or set M3T_config$Wastewater_use_CWNS and/or M3T_config$Wastewater_use_DMR to TRUE as these are the only options available in the package for input data")
  }
  
  if(M3T_config$Process_wastewater & (!M3T_config$Wastewater_Municipal_Method_Moore_EF & !M3T_config$Wastewater_Municipal_Method_GHGI)){
    error_found <- TRUE
    error_text <- paste0(error_text,"\n\nMust set M3T_config$Process_wastewater to FALSE or set M3T_config$Wastewater_Municipal_Method_Moore_EF and/or M3T_config$Wastewater_Municipal_Method_GHGI to TRUE to convert activity data to emissions")
  }
  
  if(M3T_config$Process_wastewater & (!M3T_config$Wastewater_national_septic & !M3T_config$Wastewater_state_septic)){
    error_found <- TRUE
    error_text <- paste0(error_text,"\n\nMust set M3T_config$Process_wastewater to FALSE or set M3T_config$Wastewater_national_septic and/or M3T_config$Wastewater_state_septic to TRUE as these are the only methods available in the package to calculate septic emissions")
  }
  
  if(M3T_config$Process_wetlands_and_inland_waters & !(M3T_config$Use_SOCCR1 | M3T_config$Use_SOCCR2) & !M3T_config$Use_Wetcharts){
    error_found <- TRUE
    error_text <- paste0(error_text,"\n\nMust set M3T_config$Process_wetlands_and_inland_waters to FALSE or set M3T_config$Use_SOCCR1 and/or M3T_config$Use_SOCCR2 and/or M3T_config$Use_Wetcharts to TRUE as these are the only methods available in the package to calculate wetland/inland water emissions")
  }
  
  if(M3T_config$Process_wetlands_and_inland_waters & M3T_config$Use_Wetcharts & M3T_config$Source_wetland_NLCD!="M3T" & !file.exists(M3T_config$Source_wetland_NLCD)){
    error_found <- TRUE
    error_text <- paste0(error_text,"\n\nMust set M3T_config$Process_wetlands_and_inland_waters or M3T_config$Use_Wetcharts to FALSE or set M3T_config$Source_wetland_NLCD to \"M3T\" to use preprocessed wetcharts or set M3T_config$Source_wetland_NLCD to the filepath for the raw wetcharts data to use")
  }
  
  if(M3T_config$Combine_sectors & !(M3T_config$Create_summary_combinations | M3T_config$Create_individual_combinations)){
    error_found <- TRUE
    error_text <- paste0(error_text,"\n\nMust set M3T_config$Combine_sectors to FALSE or set either M3T_config$Create_summary_combinations or M3T_config$Create_individual_combinations to TRUE")
  }
  
  if(error_found){
    stop(error_text)
  }
  
  ################################################################################
  #load in Census tigerlines necessary for several functions
  
  #Every 10 years the census updates the urban areas.  Just round down to the
  #nearest decade.  2020 and 21 use 2010 still, though 2020 has both and a
  #corrected 2020 census version.
  UAC_year <- floor(as.numeric(substring(as.character(inventory_year),3,4))/10)*10
  if(inventory_year %in% c(2020,2021)){
    UAC_year <- 10
  }
  
  #2010 is formatted differently
  if(inventory_year==2010){
    Census_filenames <- c(file.path(input_directory,"County_Tigerlines",paste0("tl_",inventory_year,"_us_county",UAC_year,".shp")),
                          file.path(input_directory,"State_Tigerlines",paste0("tl_",inventory_year,"_us_state",UAC_year,".shp")),
                          file.path(input_directory,"Urban_Tigerlines",paste0("tl_",inventory_year,"_us_uac",UAC_year,".shp")))
  }else{
    Census_filenames <- c(file.path(input_directory,"County_Tigerlines",paste0("/tl_",inventory_year,"_us_county.shp")),
                          file.path(input_directory,"State_Tigerlines",paste0("/tl_",inventory_year,"_us_state.shp")),
                          file.path(input_directory,"Urban_Tigerlines",paste0("/tl_",inventory_year,"_us_uac",UAC_year,".shp")))
  }
  
  #2011 has no urban tigerlines, so use 2012 instead + user update
  if(inventory_year==2011){
    cat("2011 has no urban area census tigerlines - using 2012 instead (only relevant if defining domain using urban area)\n")
    Census_filenames[3] <- file.path(input_directory,"Urban_Tigerlines",paste0("tl_2012_us_uac",UAC_year,".shp"))
  }
  
  #only download if set to do so
  if(M3T_config$Source_Tigerlines_data=="download"){
    #First see what's available by searching the FTP as an html
    download_location <- tempfile(fileext = ".html")
    Trycatch_downloader("https://www2.census.gov/geo/tiger/",download_location,method = "save")
    Census_file_list=readLines(download_location)
    pattern <- "TIGER[[:digit:]]{4}/"
    Census_years <- grep(pattern,Census_file_list,value=T)
    Census_years <- as.numeric(substr(Census_years,regexpr(pattern,Census_years)+5,regexpr(pattern,Census_years)+8))
    
    #define the closest and alert user if not = inventory year
    Census_match_yr <- Census_years[which.min(abs(Census_years - inventory_year))]
    if(inventory_year!=Census_match_yr){
      #update filenames too
      Census_filenames <- gsub(inventory_year,Census_match_yr,Census_filenames)
      cat("Census data does not include",inventory_year,"using",Census_match_yr,"for Tigerline data as the nearest data available\n")
    }
    invisible(file.remove(download_location))
    
    
    #URLs for state, county, and urban shapefiles (2010 is formatted
    #differently)
    if(Census_match_yr==2010){
      Census_FTP_URLs <- c(paste0("https://www2.census.gov/geo/tiger/TIGER",Census_match_yr,"/COUNTY/",Census_match_yr,"/tl_",Census_match_yr,"_us_county",UAC_year,".zip"),
                           paste0("https://www2.census.gov/geo/tiger/TIGER",Census_match_yr,"/STATE/",Census_match_yr,"/tl_",Census_match_yr,"_us_state",UAC_year,".zip"),
                           paste0("https://www2.census.gov/geo/tiger/TIGER",Census_match_yr,"/UA/",Census_match_yr,"/tl_",Census_match_yr,"_us_uac",UAC_year,".zip"))
    }else{
      Census_FTP_URLs <- c(paste0("https://www2.census.gov/geo/tiger/TIGER",Census_match_yr,"/COUNTY/tl_",Census_match_yr,"_us_county.zip"),
                           paste0("https://www2.census.gov/geo/tiger/TIGER",Census_match_yr,"/STATE/tl_",Census_match_yr,"_us_state.zip"),
                           paste0("https://www2.census.gov/geo/tiger/TIGER",Census_match_yr,"/UAC/tl_",Census_match_yr,"_us_uac",UAC_year,".zip"))
    }
    if(Census_match_yr==2011){
      Census_FTP_URLs[3] <- paste0("https://www2.census.gov/geo/tiger/TIGER2012/UAC/tl_2012_us_uac",UAC_year,".zip")
    }
    if(Census_match_yr>2023){
      Census_FTP_URLs[3] <- paste0("https://www2.census.gov/geo/tiger/TIGER",Census_match_yr,"/UAC",UAC_year,"/tl_",Census_match_yr,"_us_uac",UAC_year,".zip")
    }
    download_location <- tempfile(fileext = ".zip")
    #download each to a temp file then unzip to the input directory
    if(!all(file.exists(Census_filenames))){
      for(A in 1:length(Census_FTP_URLs)){
        Trycatch_downloader(URL = Census_FTP_URLs[A],output_location = download_location,method = "save",
                            error_message = paste("Census tigerlines could not be downloaded using link:",Census_FTP_URLs[A]))
        utils::unzip(download_location,exdir=file.path(input_directory,c("County_Tigerlines","State_Tigerlines","Urban_Tigerlines")[A]))
      }
      #delete the temp file
      unlink(download_location)
    }
    #load them in
    County_Tigerlines <- terra::vect(Census_filenames[1])
    State_Tigerlines <- terra::vect(Census_filenames[2])
    Urban_Tigerlines <- terra::vect(Census_filenames[3])
  }else if(M3T_config$Source_Tigerlines_data=="M3T"){
    #UPDATE TO ZENODO
    
    State_yrs <- as.numeric(terra::vector_layers(file.path(input_directory,"combined_state_tigerlines.gpkg")))
    
    #define the closest and alert user if not = inventory year
    Census_match_yr <- as.character(State_yrs[which.min(abs(State_yrs - inventory_year))])
    if(inventory_year!=Census_match_yr){
      cat("M3T census data does not include",inventory_year,"using",Census_match_yr,"for Tigerline data as the nearest data available\n")
    }
    
    #load them in
    County_Tigerlines <- terra::vect(file.path(input_directory,"combined_county_tigerlines.gpkg"),layer=Census_match_yr)
    State_Tigerlines <- terra::vect(file.path(input_directory,"combined_state_tigerlines.gpkg"),layer=Census_match_yr)
    if(Census_match_yr=="2011"){
      Urban_Tigerlines <- terra::vect(file.path(input_directory,"combined_urban_tigerlines.gpkg"),layer="2012")
    }else{
      Urban_Tigerlines <- terra::vect(file.path(input_directory,"combined_urban_tigerlines.gpkg"),layer=Census_match_yr)
    }
  }else{
    invisible(file.copy(sort(M3T_config$Source_Tigerlines_data),Census_filenames,overwrite=T))
    #load them in
    County_Tigerlines <- terra::vect(Census_filenames[1])
    State_Tigerlines <- terra::vect(Census_filenames[2])
    Urban_Tigerlines <- terra::vect(Census_filenames[3])
    
  }
  
  if(domain_crs!=terra::crs(State_Tigerlines)){
    #project to match the domain (crs)
    State_Tigerlines <- terra::project(State_Tigerlines,domain_crs)
    County_Tigerlines <- terra::project(County_Tigerlines,domain_crs)
    Urban_Tigerlines <- terra::project(Urban_Tigerlines,domain_crs)
  }
  
  #remove UAC_year from the end of each urban tigerlines name
  names(Urban_Tigerlines) <- sapply(names(Urban_Tigerlines),FUN=function(x){substr(x,1,nchar(x)-2)})
  
  #names for these are also formatted differently, just in that they include
  #UAC_year.
  if(Census_match_yr==2010){
    names(State_Tigerlines) <- sapply(names(State_Tigerlines),FUN=function(x){substr(x,1,nchar(x)-2)})
    names(County_Tigerlines) <- sapply(names(County_Tigerlines),FUN=function(x){substr(x,1,nchar(x)-2)})
  }
  ################################################################################
  #load in cartographic boundary files for visualization if needed.  Always uses
  #2024 given its solely for visualization at the state level, which doesn't
  #change significantly over time.
  
  if(all(tolower(domain)=="custom") | verbose==T){
    cb_file <- file.path(input_directory,"Cartographic_Boundary_500k","cb_2024_us_all_500k.gpkg")
    
    if(!file.exists(cb_file)){
      if(M3T_config$Source_Cartographic_Boundaries_data=="download"){
        #first download cb file - same as tigerlines, but excluding water boundaries
        download_location <- tempfile(fileext = ".zip")
        Trycatch_downloader("https://www2.census.gov/geo/tiger/GENZ2024/gpkg/cb_2024_us_all_500k.zip",download_location,"save",
                            "Failed to download cartographic boundary files at: https://www2.census.gov/geo/tiger/GENZ2024/gpkg/cb_2024_us_all_500k.zip")
        utils::unzip(download_location,exdir=file.path(input_directory,"Cartographic_Boundary_500k"))
        #delete the temp file
        unlink(download_location)
      }else{
        invisible(file.copy(M3T_config$Source_Cartographic_Boundaries_data,cb_file,overwrite=T))
      }
    }
    #load in the appropriate layer of map data
    State_CB <- terra::vect(cb_file,layer="cb_2024_us_state_500k")
    State_CB <- terra::project(State_CB,domain_crs)
  }else{
    State_CB <- ""
  }
  
  ################################################################################
  #create the domain
  
  if(length(domain_res)==1){
    domain_res <- rep(domain_res,2)
  }
  
  if(isa(domain,"data.frame")){
    domain <- terra::rast(nrows=diff(range(domain[,2]))/domain_res[2],
                          ncols=diff(range(domain[,1]))/domain_res[1],
                          xmin=min(domain[,1]), xmax=max(domain[,1]),
                          ymin=min(domain[,2]), ymax=max(domain[,2]),
                          vals=1)
    domain <- terra::as.polygons(terra::ext(domain),crs=domain_crs)
  }else if(isa(domain,"character")){
    #if multiple entries, focus on just 1 for simpler comparisons (must all be
    #the same type anyway)
    test_domain <- domain[1]
    
    #text is actually text
    if(is.na(suppressWarnings(as.numeric(test_domain)))){
      if(test_domain=="CONUS"){
        domain <- as.data.frame(cbind(c(-130,-60),
                                      c(20,55)))
        domain <- terra::rast(nrows=diff(range(domain[,2]))/domain_res[2],
                              ncols=diff(range(domain[,1]))/domain_res[1],
                              xmin=min(domain[,1]), xmax=max(domain[,1]),
                              ymin=min(domain[,2]), ymax=max(domain[,2]),
                              vals=1)
        domain <- terra::as.polygons(terra::ext(domain),crs=domain_crs)
      }else if(tolower(test_domain)=="custom"){
        domain <- define_custom_domain(input_directory,State_CB)
        domain <- terra::project(domain,domain_crs)
      }else if(test_domain %in% State_Tigerlines$STUSPS){
        #2 letters = state abbreviation
        State_Tigerlines <- State_Tigerlines[State_Tigerlines$STUSPS %in% domain,]
        domain <- State_Tigerlines[State_Tigerlines$STUSPS %in% domain,]
      }else if(test_domain %in% State_Tigerlines$NAME){
        #full name of state
        State_Tigerlines <- State_Tigerlines[State_Tigerlines$NAME %in% domain,]
        domain <- State_Tigerlines[State_Tigerlines$NAME %in% domain,]
      }else if(test_domain %in% unlist(terra::values(Urban_Tigerlines[,3]))){
        #full name of an urban area
        domain <- Urban_Tigerlines[unlist(terra::values(Urban_Tigerlines[,3])) %in% domain,]
      }else{
        #assume it's a filepath - otherwise it's been incorrectly supplied
        domain <- terra::vect(domain)
      }
      #text is actually a number
    }else{
      if(test_domain %in% State_Tigerlines$STATEFP){
        #State FIPS
        State_Tigerlines <- State_Tigerlines[State_Tigerlines$STATEFP %in% domain,]
        domain <- State_Tigerlines[State_Tigerlines$STATEFP %in% domain,]
      }else if(test_domain %in% Urban_Tigerlines$UACE){
        #urban area code
        domain <- Urban_Tigerlines[Urban_Tigerlines$UACE %in% domain,]
      }
    }
  }
  
  domain_template <- terra::rast(domain,resolution=domain_res,crs=domain_crs,vals=NA)
  
  ################################################################################
  # Now crop/mask the tigerlines to the domain
  
  #first remove states outside of CONUS (Alaska, Samoa, Puerto Rico, Hawaii,
  #Mariana Islands, Guam, virgin islands)
  State_Tigerlines <- State_Tigerlines[!State_Tigerlines$STUSPS %in% c("AK","AS","PR","HI","MP","GU","VI"),]
  County_Tigerlines <- County_Tigerlines[(County_Tigerlines$STATEFP %in% State_Tigerlines$STATEFP),]

  #only bother if at least some states are outside the domain (i.e., domain !=
  #CONUS).  If this given the time required to crop county tigerlines
  if(!all(terra::relate(State_Tigerlines,domain,"within"))){
    #subset to just those relevant for the domain.  For state it's any state that
    #touches the domain at all.  For county, it's only those within the states
    #(i.e., not those touching but outside the states, crop vs mask for vectors).
    #Urban are no longer needed
    State_Tigerlines <- terra::mask(State_Tigerlines,mask=domain)
    County_Tigerlines <- terra::crop(County_Tigerlines,State_Tigerlines)
  }
  
  #sort by state abbreviation
  State_Tigerlines <- State_Tigerlines[order(State_Tigerlines$STUSPS),]
  
  #save the states in the domain for use in some functions
  state_name_list <- State_Tigerlines$STUSPS
  
  if(verbose==T & !all(terra::relate(State_Tigerlines,domain,"within"))){
    State_CB <- terra::crop(State_CB,State_Tigerlines)
  }
  
  #if there's only 1 state, there's no point in processing by domain and by state
  #as they're identical.
  if(length(state_name_list)==1 & M3T_config$stationary_combustion_by_domain & M3T_config$stationary_combustion_by_state){
    M3T_config$stationary_combustion_by_domain <- FALSE
    cat("setting bydomain to FALSE for stationary combustion and NG distribution as there is only 1 state in the domain\n")
  }
  if(length(state_name_list)==1 & M3T_config$NG_distribution_by_domain & M3T_config$NG_distribution_by_state){
    M3T_config$NG_distribution_by_domain <- FALSE
  }
  
  cat("Finished loading in census tigerline shapefiles\n")
  ################################################################################
  #Download and load in GHGRP data that will be needed for several sectors using
  #the Envirofacts API.
  
  if(M3T_config$Process_landfills | M3T_config$Process_natural_gas_distribution | M3T_config$Process_natural_gas_transmission | M3T_config$Process_wastewater){
    GHGRP_facility_data_file <- file.path(input_directory,"GHGRP","facility_data.csv")
    
    #source = M3T means the zenodo file will be used and the file already exists
    if(!file.exists(GHGRP_facility_data_file)){
      if(M3T_config$Source_GHGRP_facility_data=="download"){
        #download data and read in an R dataframe.  Cannot filter to year as
        #previous year's data is used in some functions.  Cannot filter to state
        #as distribution needs to correct some states as they list headquarters
        #rather than area of operation.  See
        #https://www.epa.gov/enviro/envirofacts-data-service-api
        data_URL <- "https://data.epa.gov/dmapservice/ghg.pub_dim_facility/CSV"
        Trycatch_downloader(URL = data_URL,method = "save",output_location = GHGRP_facility_data_file,
                            error_message = paste0("Greenhouse Gas Reporting Program data could not be downloaded using API link: ",data_URL))
      }else{
        invisible(file.copy(M3T_config$Source_GHGRP_facility_data,GHGRP_facility_data_file,overwrite=T))
      }
    }
    #read in the data and update the county fips and zip with leading zeroes
    #where needed.  Neither is used, but this is good practice.
    GHGRP_facility_data <- utils::read.csv(GHGRP_facility_data_file)
    GHGRP_facility_data$county_fips <- sprintf("%05d",GHGRP_facility_data$county_fips)
    GHGRP_facility_data$zip <- sprintf("%05d",GHGRP_facility_data$zip)
    
    
    
    
    
    #NATURAL GAS SYSTEMS
    ghgrp_oil_and_gas_file <- file.path(input_directory,"/GHGRP/Oil_and_gas_W.csv")
    
    #source = M3T means the zenodo file will be used and the file already exists
    if(!file.exists(ghgrp_oil_and_gas_file)){
      if(M3T_config$Source_GHGRP_NG=="download"){
        #download the relevant LDC-sector data
        #(https://www.epa.gov/enviro/greenhouse-gas-model).  
        data_URL <- "https://data.epa.gov/dmapservice/ghg.ef_w_emissions_source_ghg/csv"
        Trycatch_downloader(URL = data_URL,method = "save",output_location = ghgrp_oil_and_gas_file,
                            error_message = paste0("Greenhouse Gas Reporting Program data could not be downloaded using API link: ",data_URL))
      }else{
        invisible(file.copy(M3T_config$Source_GHGRP_NG,ghgrp_oil_and_gas_file,overwrite = T))
      }
    }
    GHGRP_subpartW_emissions <- utils::read.csv(ghgrp_oil_and_gas_file)
    
    
    
    
    #COMBUSTION
    #source = M3T means the data is already in the package
    if(M3T_config$Source_GHGRP_combustion!="M3T"){
      ghgrp_combustion_file <- file.path(input_directory,"GHGRP","combustion_C.csv")
      
      if(!file.exists(ghgrp_combustion_file)){
        if(M3T_config$Source_GHGRP_combustion=="download"){
          data_URL <- "https://data.epa.gov/dmapservice/ghg.c_subpart_level_information/csv"
          Trycatch_downloader(URL = data_URL,method = "save",output_location = ghgrp_combustion_file,
                              error_message = paste0("Greenhouse Gas Reporting Program data could not be downloaded using API link: ",data_URL))
        }else{
          ghgrp_combustion_file <- file.path(input_directory,"GHGRP","User_supplied_combustion_file.csv")
          invisible(file.copy(M3T_config$Source_GHGRP_combustion,file.path(input_directory,"GHGRP",ghgrp_combustion_file),overwrite = T))
        }
      }
      GHGRP_combustion_emissions <- utils::read.csv(ghgrp_combustion_file)
      GHGRP_combustion_emissions <- make_consistent(GHGRP_combustion_emissions)
    }else{
      GHGRP_combustion_emissions <- M3T::GHGRP_combustion_emissions
    }
    
    cat("Finished loading in GHGRP data needed across sectors\n")
  }
  ################################################################################
  #Download, load in, and prepare GHGI data if/as needed.  Download the most
  #recent available as previous years are updated with each new GHGI.
  
  if((M3T_config$Process_landfills | M3T_config$Process_natural_gas_distribution | M3T_config$Process_natural_gas_transmission | M3T_config$Process_stationary_combustion | M3T_config$Process_wastewater) &
     any(c(M3T_config$GHGI_landfill_total,
           M3T_config$GHGI_MnR,M3T_config$GHGI_maintenance,
           M3T_config$GHGI_meters,M3T_config$GHGI_services,
           M3T_config$GHGI_Pipeline,M3T_config$GHGI_transmission_compressors,
           M3T_config$stationary_combustion_GHGI_data)=="GHGI")){
    
    if(M3T_config$Source_GHGI=="M3T"){
      #UPDATE TO ZENODO
      
      #use data for the inventory yr, based on the most recent GHGI file
      #(previous yrs are updated).  Exception if inventory yr > GHGI file (I.e.,
      #no data for inventory year).
      GHGI_file_yr <- max(as.numeric(M3T::GHGI_landfill_total_M3T$Year))
      if(inventory_year>GHGI_file_yr){
        #update user
        cat("GHGI/GHGRP not available for",inventory_year,"using",GHGI_file_yr,"for GHGI data as the nearest data available\n")
        GHGI_data_yr <- GHGI_file_yr
      }else{
        GHGI_data_yr <- inventory_year
      }
      
      #pull appropriate year from built in data
      if(M3T_config$GHGI_landfill_total=="GHGI"){
        M3T_config$GHGI_landfill_total <- M3T::GHGI_landfill_total_M3T$Emissions[M3T::GHGI_landfill_total_M3T$Year==GHGI_data_yr]
      }
      if(M3T_config$GHGI_MnR=="GHGI"){
        M3T_config$GHGI_MnR <- data.frame("Type"=rownames(M3T::GHGI_NG_distribution$GHGI_MnR_Activity),
                                          "EF"=M3T::GHGI_NG_distribution$GHGI_MnR_EF[,as.character(GHGI_data_yr)],
                                          "Total_stations"=M3T::GHGI_NG_distribution$GHGI_MnR_Activity[,as.character(GHGI_data_yr)])
      }
      if(M3T_config$GHGI_maintenance=="GHGI"){
        M3T_config$GHGI_maintenance <- data.frame("Type"=rownames(M3T::GHGI_NG_distribution$GHGI_maintenance),
                                                  "EF"=M3T::GHGI_NG_distribution$GHGI_maintenance[,as.character(GHGI_data_yr)])
      }
      if(M3T_config$GHGI_meters=="GHGI"){
        M3T_config$GHGI_meters <- data.frame("Type"=rownames(M3T::GHGI_NG_distribution$GHGI_meters),
                                             "EF"=M3T::GHGI_NG_distribution$GHGI_meters[,as.character(GHGI_data_yr)])
      }
      if(M3T_config$GHGI_services=="GHGI"){
        M3T_config$GHGI_services <- data.frame("Type"=rownames(M3T::GHGI_NG_distribution$GHGI_services),
                                               "EF"=M3T::GHGI_NG_distribution$GHGI_services[,as.character(GHGI_data_yr)])
      }
      if(M3T_config$GHGI_Pipeline=="GHGI"){
        M3T_config$GHGI_Pipeline <- data.frame("Type"=rownames(M3T::GHGI_NG_transmission$GHGI_Pipeline_Activity),
                                               "Emissions"=M3T::GHGI_NG_transmission$GHGI_Pipeline_Emissions[,as.character(GHGI_data_yr)],
                                               "Total_stations"=M3T::GHGI_NG_transmission$GHGI_Pipeline_Activity[,as.character(GHGI_data_yr)])
      }
      if(M3T_config$GHGI_transmission_compressors=="GHGI"){
        M3T_config$GHGI_transmission_compressors <- data.frame("Type"=rownames(M3T::GHGI_NG_transmission$GHGI_transmission_compressors_Activity),
                                                               "Emissions"=M3T::GHGI_NG_transmission$GHGI_transmission_compressors_Emissions[,as.character(GHGI_data_yr)],
                                                               "Total_stations"=M3T::GHGI_NG_transmission$GHGI_transmission_compressors_Activity[,as.character(GHGI_data_yr)])
      }
      if(M3T_config$stationary_combustion_GHGI_data=="GHGI"){
        M3T_config$stationary_combustion_GHGI_data <- M3T::GHGI_stationary_combustion[rownames(M3T::GHGI_stationary_combustion) == as.character(GHGI_data_yr),]
      }
    }else{
      
      #have to download the files and pull the data from each file instead
      if(M3T_config$Source_GHGI=="download"){
        #Iteratively test GHGI webpages down to 2022 (most recent available when code
        #was written) to find the newest one available that ALSO has GHGRP data.
        for(GHGI_file_yr in 2029:2022){
          data_URL <- paste0("https://www.epa.gov/ghgemissions/inventory-us-greenhouse-gas-emissions-and-sinks-1990-",GHGI_file_yr)
          test_url <- suppressWarnings(try(utils::download.file(data_URL,tempfile(".zip"),quiet = T),silent = T))
          
          #SEDS too - if processing stationary combustion.  See
          #https://www.eia.gov/opendata/browser/seds. URL is described in that
          #sector's code.
          if(M3T_config$Process_stationary_combustion){
            SEDS_data_URL <- paste0("https://api.eia.gov/v2/seds/data/?frequency=annual&data[0]=value&facets[seriesId][]=CLCCB",
                                    "&facets[seriesId][]=CLEIB&facets[seriesId][]=CLICB&facets[seriesId][]=NGCCB&facets[seriesId][]=NGEIB&facets[seriesId][]=NGICB&facets[seriesId][]=PACCB&facets[seriesId][]=PAEIB&facets[seriesId][]=PAICB&facets[seriesId][]=PARCB&facets[seriesId][]=WDRCB&facets[seriesId][]=WWCCB&facets[seriesId][]=WWEIB&facets[seriesId][]=WWICB",
                                    "&facets[stateId][]=AL",
                                    "&start=",GHGI_file_yr-1,"&end=",GHGI_file_yr+1,
                                    "&sort[0][column]=seriesId&sort[0][direction]=asc&offset=0&api_key=",M3T_config$EIA_API_key)
            SEDS_test_url <- jsonlite::fromJSON(SEDS_data_URL)
          }else{
            SEDS_test_url <- list()
            SEDS_test_url$response$data$period=GHGI_file_yr
          }
          
          if(test_url==0 & (GHGI_file_yr %in% unique(GHGRP_subpartW_emissions$reporting_year)) & (GHGI_file_yr %in% SEDS_test_url$response$data$period)){
            break
          }
        }
        
        #use data for the inventory yr, based on the most recent GHGI file
        #(previous yrs are updated).  Exception if inventory yr > GHGI file (I.e.,
        #no data for inventory year).
        if(inventory_year>GHGI_file_yr){
          #update user
          cat("GHGI/GHGRP not available for",inventory_year,"using",GHGI_file_yr,"for GHGI data as the nearest data available\n")
          GHGI_data_yr <- GHGI_file_yr
        }else{
          GHGI_data_yr <- inventory_year
        }
        
        #download the webpage and load in the HTML
        download_dest <- tempfile(fileext = ".html")
        Trycatch_downloader(URL = data_URL,method = "save",output_location = download_dest,
                            error_message = paste0("GHGI data could not be webscraped from webpage: ",data_URL))
        HTML_data <- readChar(download_dest,file.info(download_dest)$size)
        
        #Search for https:// - any 60 or fewer characters - main - any
        #characters.zip in the HTML_data. This should identify the file if it's
        #named similarly to any other in the 2020's. The HTML_data webpage must
        #still be up to date though.
        Matchtext <- regexpr("https://www.epa.gov/.{1,60}main.{0,60}.zip",HTML_data,ignore.case = T)
        Matchtext_annex <- regexpr("https://www.epa.gov/.{1,60}annex.{0,60}.zip",HTML_data,ignore.case = T)
        data_URL2 <- substring(HTML_data,Matchtext[1],Matchtext[1]+attr( Matchtext , "match.length")-1)
        data_URL_annex <- substring(HTML_data,Matchtext_annex[1],Matchtext_annex[1]+attr( Matchtext_annex , "match.length")-1)
        
        #Use regex to save the year of the dataset as part of the download for
        #clarity.  Download and unzip.
        GHGI_yr <- substr(data_URL2,regexpr("20.{2}",data_URL2)[1],regexpr("20.{2}",data_URL2)[1]+3)
        GHGI_file <- file.path(input_directory,paste0(GHGI_yr,"_GHGI_tables.zip"))
        if(!dir.exists(gsub("\\.zip","",GHGI_file))){
          Trycatch_downloader(URL = data_URL2,method = "save",output_location = GHGI_file,
                              error_message = paste0("GHGI data could not be downloaded from webpage:\n",data_URL2))
          utils::unzip(GHGI_file,exdir = file.path(input_directory,paste0(GHGI_yr,"_GHGI_tables")),overwrite=T)
          
          #annex too
          Trycatch_downloader(URL = data_URL_annex,method = "save",output_location = GHGI_file,
                              error_message = paste0("GHGI data could not be downloaded from webpage:\n",data_URL_annex))
          utils::unzip(GHGI_file,exdir = file.path(input_directory,paste0(GHGI_yr,"_GHGI_tables")),overwrite=T)
          
          #delete zip files
          unlink(GHGI_file)
          GHGI_file <- gsub("\\.zip","",GHGI_file)
          
          #ID zipped subfolders
          sub_zips <- list.files(file.path(input_directory,paste0(GHGI_yr,"_GHGI_tables")),pattern = "*.zip",full.names = T)
          sub_folders <- gsub(".zip","",sub_zips)
          
          #just a duplicate of the GHGI folder - delete
          unlink(sub_zips[grep("Main Text",sub_zips)])
          sub_zips <- sub_zips[-grep("Main Text",sub_zips)]
          
          #unzip subfolders
          for(A in 1:length(sub_zips)){
            utils::unzip(zipfile = sub_zips[A],exdir = sub_folders[A],overwrite = T)
            unlink(sub_zips[A])
          }
          
          
          
          
          
          
          #now repeat for the petroleum and NG annex tables
          NG_annex <- paste0(gsub("inventory-us-greenhouse-gas-emissions-and-sinks",
                                  "natural-gas-and-petroleum-systems-ghg-inventory-additional-information",
                                  data_URL),"-ghg")
          
          Trycatch_downloader(URL = NG_annex,method = "save",output_location = download_dest,
                              error_message = paste0("GHGI data could not be webscraped from webpage: ",data_URL))
          HTML_data <- readChar(download_dest,file.info(download_dest)$size)
          
          Matchtext <- regexpr("https://www.epa.gov/.{1,100}ghgi_natural_gas_systems.{0,60}.xlsx",HTML_data,ignore.case = T)
          data_URL2 <- substring(HTML_data,Matchtext[1],Matchtext[1]+attr( Matchtext , "match.length")-1)
          
          NG_annex_file <- file.path(GHGI_file,paste0(GHGI_yr,"_ghgi_natural_gas_systems_annex36_tables.xlsx"))
          Trycatch_downloader(URL = data_URL2,method = "save",output_location = NG_annex_file,
                              error_message = paste0("GHGI data could not be downloaded from webpage:\n",data_URL2))
          unlink(download_dest)
        }else{
          GHGI_file <- gsub("\\.zip","",GHGI_file)
          NG_annex_file <- file.path(GHGI_file,paste0(GHGI_yr,"_ghgi_natural_gas_systems_annex36_tables.xlsx"))
        }
      }else{
        GHGI_file <- file.path(input_directory,"User_supplied_GHGI_tables")
        dir.create(GHGI_file)
        invisible(file.copy(M3T_config$Source_GHGI,GHGI_file,recursive = T,overwrite=T))
        NG_annex_file <- list.files(GHGI_file,"_ghgi_natural_gas_systems_annex36_tables.xlsx",full.names = T)
        GHGI_data_yr <- inventory_year
      }
      
      
      
      
      
      
      #grab landfill data
      if(M3T_config$GHGI_landfill_total=="GHGI"){
        #find the relevant folder and file using regex of folder names and file headers
        Waste_folder <- list.files(GHGI_file,pattern="*Waste*",full.names = T)
        Waste_files <- list.files(Waste_folder,full.names=T)
        M3T_config$GHGI_landfill_total <- sapply(Waste_files,readLines,n=1)
        M3T_config$GHGI_landfill_total <- Waste_files[grep("*CH4 Emissions from Landfills \\(kt CH4\\)*",M3T_config$GHGI_landfill_total)]
        M3T_config$GHGI_landfill_total <- utils::read.csv(M3T_config$GHGI_landfill_total,skip = 1)
        #get the required data
        M3T_config$GHGI_landfill_total <- sapply(M3T_config$GHGI_landfill_total[M3T_config$GHGI_landfill_total$Activity=="MSW net CH4 Emissions",-1],FUN = function(x){as.numeric(gsub(",","",x))})
        M3T_config$GHGI_landfill_total <- as.data.frame(t(M3T_config$GHGI_landfill_total))
        M3T_config$GHGI_landfill_total <- M3T_config$GHGI_landfill_total[,paste0("X",GHGI_data_yr)]
      }
      
      
      
      
      
      
      #grab the NG distribution data
      if(any(c(M3T_config$GHGI_MnR,M3T_config$GHGI_maintenance,M3T_config$GHGI_meters,M3T_config$GHGI_services)=="GHGI")){
        #use grep and the index page of the annex file to identify the pages we want
        GHGI_index <- readxl::read_excel(NG_annex_file,sheet = "Index",.name_repair = "minimal")
        
        GHGI_Activity_sheet <- gsub("Table ","",
                                    GHGI_index[sapply(GHGI_index[,2],FUN=function(x){grep(pattern="Activity Data for Natural Gas Systems Sources",x)}),1])
        GHGI_Emission_Factor_sheet <- gsub("Table ","",
                                           GHGI_index[sapply(GHGI_index[,2],FUN=function(x){grep(pattern="Average CH4 Emission Factors \\(kg/unit activity\\) for Natural Gas Systems Sources",x)}),1])
        
        #Columns = year, rows = various types of sources.  First row is just to
        #identify the first row of the tables as there is also header information that
        #we want to exclude
        first_row <- which(readxl::read_xlsx(NG_annex_file,sheet = GHGI_Activity_sheet,.name_repair = "minimal")[,1]=="Segment/Source")
        GHGI_Activity <- readxl::read_xlsx(NG_annex_file,sheet = GHGI_Activity_sheet,skip=first_row,col_names = T)
        
        first_row <- which(readxl::read_xlsx(NG_annex_file,sheet = GHGI_Emission_Factor_sheet,.name_repair = "minimal")[,1]=="Segment/Source")
        GHGI_Emission_Factors <- readxl::read_xlsx(NG_annex_file,sheet = GHGI_Emission_Factor_sheet,skip=first_row,col_names = T)
        
        if(M3T_config$GHGI_MnR=="GHGI"){
          #all the sources we're looking for, written exactly as in the GHGI file
          Data_list <- c("M&R >300","M&R 100-300","M&R <100","Reg >300","R-Vault >300",
                         "Reg 100-300","R-Vault 100-300","Reg 40-100","R-Vault 40-100",
                         "Reg <40")
          
          #use sapply to find the row using data list, specify the column as the year and
          #grab the relevant EF and activity data into a dataframe.
          
          #Metering and regulating stations in mol/s/station
          M3T_config$GHGI_MnR <- data.frame("Type"=Data_list,
                                            "EF"=as.numeric(unlist(sapply(Data_list,FUN=function(x){GHGI_Emission_Factors[GHGI_Emission_Factors[,1]==x,as.character(GHGI_data_yr)]})))*
                                              1000/(16.043*60*60*24*365),#convert from kg/yr to mol/s
                                            "Total_stations"=as.numeric(unlist(sapply(Data_list,FUN=function(x){GHGI_Activity[GHGI_Activity[,1]==x,as.character(GHGI_data_yr)]}))),
                                            row.names = NULL)
        }
        
        #repeat for several other source types
        if(M3T_config$GHGI_services=="GHGI"){
          Data_list <- c("Services - Unprotected steel",
                         "Services Protected steel",
                         "Services - Plastic",
                         "Services - Copper")
          
          #Service emissions in mol/s/event
          M3T_config$GHGI_services <- data.frame("Type"=Data_list,
                                                 "EF"=as.numeric(unlist(sapply(Data_list,FUN=function(x){GHGI_Emission_Factors[GHGI_Emission_Factors[,1]==x,as.character(GHGI_data_yr)]})))*
                                                   1000/(16.043*60*60*24*365),#convert from kg/yr to mol/s
                                                 row.names = NULL)
        }
        
        if(M3T_config$GHGI_meters=="GHGI"){
          Data_list <- c("Residential",
                         "Commercial",
                         "Industrial")
          
          #meter emissions in mol/s/meter
          M3T_config$GHGI_meters <- data.frame("Type"=Data_list,
                                               "EF"=as.numeric(unlist(sapply(Data_list,FUN=function(x){GHGI_Emission_Factors[which(GHGI_Emission_Factors[,1]==x)[1],as.character(GHGI_data_yr)]})))*
                                                 1000/(16.043*60*60*24*365),#convert from kg/yr to mol/s
                                               row.names = NULL)
        }
        
        if(all(M3T_config$GHGI_maintenance=="GHGI")){
          Data_list <- c("Pressure Relief Valve Releases",
                         "Pipeline Blowdown",
                         "Mishaps (Dig-ins)")
          
          M3T_config$GHGI_maintenance <- data.frame("Type"=Data_list,
                                                    "EF"=as.numeric(unlist(sapply(Data_list,FUN=function(x){GHGI_Emission_Factors[GHGI_Emission_Factors[,1]==x,as.character(GHGI_data_yr)]})))*
                                                      1000/(16.043*60*60*24*365),#convert from kg/yr to mol/s
                                                    row.names = NULL)
          
          #Event emissions in mol/s/mile of pipeline
        }
      }
      
      
      
      
      
      
      #grab the NG transmission data
      if(any(c(M3T_config$GHGI_Pipeline,M3T_config$GHGI_transmission_compressors)=="GHGI")){
        #identical to NG distribution one, but emissions and activity instead of
        #emission factors and activity
        GHGI_index <- readxl::read_excel(NG_annex_file,sheet = "Index",.name_repair = "minimal")
        GHGI_Activity_sheet <- gsub("Table ","",
                                    GHGI_index[sapply(GHGI_index[,2],FUN=function(x){grep(pattern="Activity Data for Natural Gas Systems Sources",x)}),1])
        GHGI_Emission_sheet <- gsub("Table ","",
                                    GHGI_index[sapply(GHGI_index[,2],FUN=function(x){grep(pattern="CH4 Emissions \\(kt\\) for Natural Gas Systems",x)}),1])
        first_row <- which(readxl::read_xlsx(NG_annex_file,sheet = GHGI_Activity_sheet,.name_repair = "minimal")[,1]=="Segment/Source")
        GHGI_Activity <- readxl::read_xlsx(NG_annex_file,sheet = GHGI_Activity_sheet,skip=first_row,col_names = T)
        first_row <- which(readxl::read_xlsx(NG_annex_file,sheet = GHGI_Emission_sheet,.name_repair = "minimal")[,1]=="Segment/Source")
        GHGI_Emissions <- readxl::read_xlsx(NG_annex_file,sheet = GHGI_Emission_sheet,skip=first_row,col_names = T)
        
        if(M3T_config$GHGI_Pipeline=="GHGI"){
          #all the sources we're looking for, written exactly as in the GHGI file
          Data_list <- c("Pipeline Leaks","M&R (Trans. Co. Interconnect)","M&R (Farm Taps + Direct Sales)",
                         "Pipeline venting")
          
          #use sapply to find the row using data list, specify the column as the year
          #and grab the relevant emissions and activity data into a dataframe.
          M3T_config$GHGI_Pipeline <- data.frame("Type"=Data_list,
                                                 "Emissions"=as.numeric(unlist(
                                                   sapply(Data_list,FUN=function(x){GHGI_Emissions[which(GHGI_Emissions[,1]==x)[1],as.character(GHGI_data_yr)]})))*
                                                   1E9/(16.043*60*60*24*365),#convert from kt/yr to mol/s
                                                 "Total_stations"=as.numeric(unlist(
                                                   sapply(Data_list,FUN=function(x){GHGI_Activity[which(GHGI_Activity[,1]==x)[1],as.character(GHGI_data_yr)]})))*
                                                   1609.344,#convert from miles to meters
                                                 row.names = NULL)
        }
        
        if(M3T_config$GHGI_transmission_compressors=="GHGI"){
          #transmission station total + emissions during operations (vents, flaring,
          #leaks, exhaust, etc.)
          Data_list <- c("Station Total Emissions","Dehydrator vents (Transmission)",
                         "Flaring (Transmission)","Engines (Transmission)",
                         "Turbines (Transmission)","Engines (Storage)",
                         "Turbines (Storage)","Generators (Engines)",
                         "Generators (Turbines)","Pneumatic Devices Transmission",
                         "Station Venting Transmission")
          
          #use sapply to find the row using data list, specify the column as the year
          #and grab the relevant emissions and activity data into a dataframe.
          M3T_config$GHGI_transmission_compressors <- data.frame("Type"=Data_list,
                                                                 "Emissions"=as.numeric(unlist(sapply(Data_list,FUN=function(x){GHGI_Emissions[which(GHGI_Emissions[,1]==x)[1],as.character(GHGI_data_yr)]})))*
                                                                   1E9/(16.043*60*60*24*365),#convert from kt/yr to mol/s
                                                                 "Total_stations"=as.numeric(unlist(sapply(Data_list,FUN=function(x){GHGI_Activity[which(GHGI_Activity[,1]==x)[1],as.character(GHGI_data_yr)]}))),
                                                                 row.names = NULL)
        }
      }
      
      
      
      
      
      
      #grab the stationary combustion data
      if(M3T_config$stationary_combustion_GHGI_data=="GHGI"){
        #find the relevant folder and file using regex of folder names and file headers
        stationary_combustion_folder <- file.path(GHGI_file,"2024 Annex 3 Tables")
        stationary_combustion_files <- list.files(stationary_combustion_folder,full.names=T)
        M3T_config$stationary_combustion_GHGI_data <- sapply(stationary_combustion_files,readLines,n=1)
        M3T_config$stationary_combustion_GHGI_data <- stationary_combustion_files[suppressWarnings(grep("*Fuel Consumption by Stationary Combustion.*\\(TBtu\\)*",M3T_config$stationary_combustion_GHGI_data))]
        M3T_config$stationary_combustion_GHGI_data <- utils::read.csv(M3T_config$stationary_combustion_GHGI_data,skip = 1)
        
        #reformat to match SEDS format
        rownames(M3T_config$stationary_combustion_GHGI_data) <- paste0(rep(M3T_config$stationary_combustion_GHGI_data[seq(1,26,6),1],each=6) , " ",
                                                                       M3T_config$stationary_combustion_GHGI_data[,1])[1:nrow(M3T_config$stationary_combustion_GHGI_data)]
        M3T_config$stationary_combustion_GHGI_data <- t(M3T_config$stationary_combustion_GHGI_data)
        M3T_config$stationary_combustion_GHGI_data <- as.data.frame(matrix(M3T_config$stationary_combustion_GHGI_data[paste0("X",GHGI_data_yr),c("Coal Commercial","Coal Industrial","Coal Electric Power",
                                                                                                                                                 "Petroleum Residential","Petroleum Commercial","Petroleum Industrial","Petroleum Electric Power",
                                                                                                                                                 "Natural Gas Commercial","Natural Gas Industrial","Natural Gas Electric Power",
                                                                                                                                                 "Wood Residential","Wood Commercial","Wood Industrial","Wood Electric Power")],nrow=1))
        M3T_config$stationary_combustion_GHGI_data <- cbind("US_EPA",M3T_config$stationary_combustion_GHGI_data)
        colnames(M3T_config$stationary_combustion_GHGI_data) <- c("State",
                                                                  "com_coal","ind_coal","elec_coal",
                                                                  "res_petr","com_petr","ind_petr","elec_petr",
                                                                  "com_gas","ind_gas","elec_gas",
                                                                  "res_wood","com_wood","ind_wood","elec_wood")
        #make numeric rather than text
        M3T_config$stationary_combustion_GHGI_data[,-1] <- apply(M3T_config$stationary_combustion_GHGI_data[,-1], 2, FUN=function(x){as.numeric(gsub(",","",x))})
      }
    }
  }
  ################################################################################
  #the 2012 clean watershed needs survey has no SC data - alert users
  if(M3T_config$Process_wastewater & M3T_config$Wastewater_use_CWNS & inventory_year<2017){
    cat("WARNING - AK & SC did not report to the 2012 clean watershed needs survey. We recommend using the DMR dataset instead - especially if SC is within the domain.\nIf downscaling GHGI data and using the CWNS data, AK & SC wastewater emissions will be apportioned to other states.\n")
    Sys.sleep(3)
  }
  ################################################################################
  #do some basic processing to prep NLCD data for the domain if needed
  
  if(M3T_config$Process_wetlands_and_inland_waters & M3T_config$Use_Wetcharts & M3T_config$Source_wetcharts=="M3T"){
    Wetland_output_directory <- file.path(output_directory,"Wetlands")
    dir.create(Wetland_output_directory,showWarnings = F)
    
    wetcharts <- terra::rast(file.path(input_directory,'combined_NLCD_downscaled_wetcharts.tif'))
    
    wetcharts_years <- sapply(strsplit(names(wetcharts),split = "_"),"[[",1)
    wetcharts_nearest_year <- unique(wetcharts_years)[which.min(abs(as.numeric(unique(wetcharts_years)) - inventory_year))]
    wetcharts <- wetcharts[[wetcharts_years %in% wetcharts_nearest_year]]
    if(inventory_year!=wetcharts_nearest_year){
      cat("Prepared wetcharts does not include",inventory_year,"using",wetcharts_nearest_year,"as the nearest data available\n")
    }
    
    if(any(terra::ext(domain)/terra::ext(State_Tigerlines) > 1.1)){
      domain_trans <- terra::as.polygons(terra::ext(domain)/terra::ext(State_Tigerlines) * terra::ext(terra::project(State_Tigerlines,wetcharts)))
      
      domain_trans <- terra::rast(domain_trans,crs=terra::crs(wetcharts),res=terra::res(terra::project(domain_template,terra::crs(wetcharts))))
    }else{
      domain_trans <- terra::project(domain_template,terra::crs(wetcharts))
    }
    
    #crop to the domain + buffer first to speed up process
    wetcharts <- terra::crop(wetcharts,terra::ext(domain_trans)*1.1,snap="out")
    if(any(terra::res(wetcharts) < terra::res(domain_trans))){
      wetcharts <- terra::project(wetcharts,domain_template,method="mean")
    }else if(any(terra::res(wetcharts) > terra::res(domain_trans))){
      wetcharts <- terra::disagg(wetcharts,
                                 round(terra::res(wetcharts)/terra::res(domain_trans),3),
                                 "near")
      #reproject to exact domain now.  Here using nearest neighbor to prevent
      #only 1 row/column of higher res pixels on the border of each coarser
      #pixel from being interpolated.
      wetcharts <- terra::project(wetcharts,domain_template,method="near")
    }
    
    wetcharts <- terra::mask(wetcharts,domain)
    
    if(!any(terra::ext(domain)/terra::ext(State_Tigerlines) > 1.1)){
      #account for pixels partially within the domain
      cover <- terra::extract(wetcharts[[1]],domain,weights=T,cells=T)
      wetcharts[cover[,'cell']] <- wetcharts[cover[,'cell']]*cover[,'weight']
    }
    
    wetcharts_models <- sapply(strsplit(names(wetcharts),split = "_"),"[[",3)

    for(A in 1:length(M3T_config$Wetcharts_model_subset)){
      wetcharts_subset <- terra::mean(wetcharts[[wetcharts_models %in% M3T_config$Wetcharts_model_subset[[A]]]],na.rm=T)
      
      writeCDF_no_newline(wetcharts_subset,
                          file.path(Wetland_output_directory,paste0('Wetcharts_NLCD_Downscaled_subset_',A,'.nc')),
                          force_v4=TRUE,
                          varname='methane_emissions',
                          unit='nmol/m2/s',
                          longname=paste0('Methane emissions from Wetlands from the Wetcharts models, subset to models ',paste(M3T_config$Wetcharts_model_subset[[A]],collapse = ", ")),
                          missval=-9999,
                          overwrite=TRUE)
    }
  }

  cat("Finished loading config and error checking it. Running individual sectors now\n\n")
  ################################################################################
  #Actually run the functions now, based on the config file
  
  if(M3T_config$Process_landfills){
    Municipal_solid_waste(input_directory=input_directory,
                          domain=domain,
                          domain_template=domain_template,
                          state_name_list=state_name_list,
                          output_directory=output_directory,
                          inventory_year=inventory_year,
                          GHGI_data_yr=GHGI_data_yr,
                          verbose=verbose,
                          GHGI_landfill_total=M3T_config$GHGI_landfill_total,
                          GHGRP_facility_data=GHGRP_facility_data,
                          GHGRP_combustion_emissions=GHGRP_combustion_emissions,
                          Source_GHGRP_landfills=M3T_config$Source_GHGRP_landfills,
                          Source_LMOP=M3T_config$Source_LMOP,
                          landfill_ghgrp_reported=M3T_config$landfill_ghgrp_reported,
                          landfill_ghgrp_modeled=M3T_config$landfill_ghgrp_modeled,
                          landfill_ghgrp_collection_efficiency=M3T_config$landfill_ghgrp_collection_efficiency,
                          plot_directory=plot_directory,
                          County_Tigerlines=County_Tigerlines,
                          State_CB=State_CB)
  }
  invisible(gc())
  if(M3T_config$Process_natural_gas_distribution){
    NG_distribution(domain=domain,
                    domain_template=domain_template,
                    state_name_list=state_name_list,
                    input_directory=input_directory,
                    output_directory=output_directory,
                    inventory_year=inventory_year,
                    GHGI_data_yr=GHGI_data_yr,
                    verbose=verbose,
                    GHGRP_facility_data=GHGRP_facility_data,
                    GHGRP_subpartW_emissions=GHGRP_subpartW_emissions,
                    Source_EIA_NG_file = M3T_config$Source_EIA_NG_file,
                    Source_PHMSA_file = M3T_config$Source_PHMSA_file,
                    Source_GHGRP_LDC = M3T_config$Source_GHGRP_LDC,
                    GHGI_MnR = M3T_config$GHGI_MnR,
                    GHGI_maintenance = M3T_config$GHGI_maintenance,
                    GHGI_meters = M3T_config$GHGI_meters,
                    GHGI_services = M3T_config$GHGI_services,
                    State_Tigerlines=State_Tigerlines,
                    NG_distribution_by_LDC = M3T_config$NG_distribution_by_LDC,
                    NG_distribution_by_state = M3T_config$NG_distribution_by_state,
                    NG_distribution_by_domain = M3T_config$NG_distribution_by_domain,
                    natural_gas_pipeline_emission_factors=M3T_config$natural_gas_pipeline_emission_factors,
                    natural_gas_res_post_meter_emission_factor=M3T_config$natural_gas_res_post_meter_emission_factor,
                    natural_gas_com_post_meter_emission_factor=M3T_config$natural_gas_com_post_meter_emission_factor,
                    Use_ACES=M3T_config$Use_ACES,
                    Use_Vulcan=M3T_config$Use_Vulcan,
                    aces_res=aces_res,
                    aces_com=aces_com,
                    vu_res=vu_res,
                    vu_com=vu_com,
                    plot_directory=plot_directory,
                    County_Tigerlines=County_Tigerlines,
                    State_CB=State_CB)
  }
  invisible(gc())
  if(M3T_config$Process_natural_gas_transmission){
    Transmission(input_directory=input_directory,
                 GHGI_transmission_compressors=M3T_config$GHGI_transmission_compressors,
                 GHGI_Pipeline=M3T_config$GHGI_Pipeline,
                 Source_HIFLD_compressor_file=M3T_config$Source_HIFLD_compressor_file,
                 Source_EIA_transmission_file=M3T_config$Source_EIA_transmission_file,
                 domain=domain,
                 domain_template=domain_template,
                 GHGRP_facility_data=GHGRP_facility_data,
                 GHGRP_subpartW_emissions=GHGRP_subpartW_emissions,
                 GHGRP_combustion_emissions=GHGRP_combustion_emissions,
                 state_name_list=state_name_list,
                 output_directory=output_directory,
                 inventory_year=inventory_year,
                 GHGI_data_yr=GHGI_data_yr,
                 verbose=verbose,
                 plot_directory=plot_directory,
                 County_Tigerlines=County_Tigerlines,
                 State_CB=State_CB)
  }
  invisible(gc())
  if(M3T_config$Process_stationary_combustion){
    Stationary_combustion(input_directory=input_directory,
                          domain=domain,
                          domain_template=domain_template,
                          state_name_list=state_name_list,
                          output_directory=output_directory,
                          inventory_year=inventory_year,
                          GHGI_data_yr=GHGI_data_yr,
                          State_Tigerlines=State_Tigerlines,
                          verbose=verbose,
                          County_Tigerlines=County_Tigerlines,
                          Use_ACES=M3T_config$Use_ACES,
                          Use_Vulcan=M3T_config$Use_Vulcan,
                          aces_res=aces_res,
                          aces_com=aces_com,
                          aces_ind=aces_ind,
                          aces_elec=aces_elec,
                          vu_res=vu_res,
                          vu_com=vu_com,
                          vu_ind=vu_ind,
                          vu_elec=vu_elec,
                          stationary_combustion_by_state=M3T_config$stationary_combustion_by_state,
                          stationary_combustion_by_domain=M3T_config$stationary_combustion_by_domain,
                          stationary_combustion_GHGI_data=M3T_config$stationary_combustion_GHGI_data,
                          stationary_combustion_emission_factors=M3T_config$stationary_combustion_emission_factors,
                          Source_EIA_SEDS_data=M3T_config$Source_EIA_SEDS_data,
                          Source_NEI_data=M3T_config$Source_NEI_data,
                          EIA_API_key=M3T_config$EIA_API_key,
                          plot_directory=plot_directory,
                          State_CB=State_CB)
  }
  invisible(gc())
  if(M3T_config$Process_wastewater){
    #this if is outside the function as the entire function is to process
    #wetcharts and the processed version is pulled in M3T
    if(M3T_config$Source_wastewater_NLCD!="M3T"){
      #UPDATE TO ZENODO
      NLCD_open_and_low_int(input_directory=input_directory,
                            Source_wastewater_NLCD=M3T_config$Source_wastewater_NLCD,
                            domain=domain,
                            domain_template=domain_template,
                            state_name_list=state_name_list,
                            State_Tigerlines=State_Tigerlines,
                            output_directory=output_directory)
    }
    Wastewater(input_directory=input_directory,
               output_directory=output_directory,
               Wastewater_use_CWNS=M3T_config$Wastewater_use_CWNS,
               Wastewater_use_DMR=M3T_config$Wastewater_use_DMR,
               Wastewater_Municipal_Method_Moore_EF=M3T_config$Wastewater_Municipal_Method_Moore_EF,
               Wastewater_Municipal_Method_GHGI=M3T_config$Wastewater_Municipal_Method_GHGI,
               Wastewater_national_septic=M3T_config$Wastewater_national_septic,
               Wastewater_state_septic=M3T_config$Wastewater_state_septic,
               domain=domain,
               domain_template=domain_template,
               GHGRP_facility_data=GHGRP_facility_data,
               Source_GHGRP_wastewater=M3T_config$Source_GHGRP_wastewater,
               Source_CWNS=M3T_config$Source_CWNS,
               Source_DMR=M3T_config$Source_DMR,
               Source_wastewater_NLCD=M3T_config$Source_wastewater_NLCD,
               Source_State_population_data=M3T_config$Source_State_population_data,
               inventory_year=inventory_year,
               National_wastewater_info=M3T_config$National_wastewater_info,
               Wastewater_reported_State_info=M3T_config$Wastewater_reported_State_info,
               GHGI_data_yr=GHGI_data_yr,
               GHGI_wastewater_data=M3T_config$GHGI_wastewater_data,
               Total_national_open_or_low_int_area=M3T_config$Total_national_open_or_low_int_area,
               State_Tigerlines=State_Tigerlines,
               state_name_list=state_name_list,
               County_Tigerlines=County_Tigerlines,
               verbose=verbose,
               State_CB=State_CB,
               plot_directory=plot_directory)
  }
  invisible(gc())
  if(M3T_config$Process_wetlands_and_inland_waters){
    if(M3T_config$Use_Wetcharts & M3T_config$Source_wetland_NLCD!="M3T"){
      #this source if is outside the function as the entire function is to process
      #wetcharts and the processed version is pulled in M3T

      #UPDATE TO ZENODO
      Disaggregate_Wetcharts(input_directory=input_directory,
                             output_directory=output_directory,
                             domain=domain,
                             domain_template=domain_template,
                             verbose=verbose,
                             inventory_year=inventory_year,
                             plot_directory=plot_directory,
                             State_Tigerlines=State_Tigerlines,
                             State_CB=State_CB,
                             County_Tigerlines=County_Tigerlines,
                             Source_wetland_NLCD=M3T_config$Source_wetland_NLCD,
                             Source_wetcharts=M3T_config$Source_wetcharts,
                             Wetcharts_model_subset=M3T_config$Wetcharts_model_subset)
    }
    if(M3T_config$Source_NWI!="M3T"){
      NWI_Wetland_fraction(input_directory=input_directory,
                           output_directory=output_directory,
                           domain=domain,
                           domain_template=domain_template,
                           state_name_list=state_name_list)
    }
    SOCCR_Wetlands(input_directory=input_directory,
                   output_directory=output_directory,
                   plot_directory=plot_directory,
                   state_name_list=state_name_list,
                   domain=domain,
                   domain_template=domain_template,
                   Use_SOCCR1=M3T_config$Use_SOCCR1,
                   Use_SOCCR2=M3T_config$Use_SOCCR2,
                   Wetland_EFs=M3T_config$Wetland_EFs,
                   verbose=verbose,
                   State_Tigerlines=State_Tigerlines,
                   County_Tigerlines=County_Tigerlines,
                   State_CB=State_CB,
                   Source_Watershed_file=M3T_config$Source_Watershed_file,
                   Use_Wetcharts=M3T_config$Use_Wetcharts,
                   Wetcharts_model_subset=M3T_config$Wetcharts_model_subset)
  }
  invisible(gc())
  if(M3T_config$Process_remaining_sectors_from_gridded_EPA){
    Prepare_GEPA(inventory_year=inventory_year,
                 input_directory=input_directory,
                 output_directory=output_directory,
                 domain=domain,
                 plot_directory=plot_directory,
                 Source_GEPA=M3T_config$Source_GEPA,
                 County_Tigerlines=County_Tigerlines,
                 State_CB=State_CB,
                 domain_template=domain_template,
                 verbose=verbose)
  }
  invisible(gc())
  if(M3T_config$Combine_sectors){
    Combine_inventories(output_directory=output_directory,
                        Separate_thermo=M3T_config$Separate_thermo,
                        Create_summary_combinations=M3T_config$Create_summary_combinations,
                        Create_individual_combinations=M3T_config$Create_individual_combinations,
                        plot_directory=plot_directory,
                        County_Tigerlines=County_Tigerlines,
                        domain_template=domain_template,
                        domain=domain,
                        verbose=verbose,
                        State_CB=State_CB)
  }
}

