#'@title Create gridded municipal solid waste methane emissions maps
#'
#'@description \code{Municipal_solid_waste} is an internal function that we
#'  strongly recommend users do not use directly, instead using
#'  \code{\link{CH4_inventory_build}} and \code{\link{M3T_config}} which call
#'  this function. \code{Municipal_solid_waste} writes up to 4 main netcdf files
#'  and 3 sector total netcdf files of gridded methane emissions from municipal
#'  landfills, as well as optional visuals and 2 csv's with the data that is 
#'  gridded
#'
#'@details This function calculates and grids methane emissions from municipal
#'  landfills. It uses the \href{https://www.epa.gov/ghgreporting}{Environmental
#'  Protection Agency's (EPA) Greenhouse Gas Reporting Program (GHGRP)}
#'  emissions when available.  It then calculates the difference between
#'  national GHGRP emissions and the
#'  \href{https://www.epa.gov/ghgemissions/inventory-us-greenhouse-gas-emissions-and-sinks}{EPA
#'  national Greenhouse Gas Inventory (GHGI)} emissions (GHGI - GHGRP) and
#'  distributes this residual equally to all facilities in the
#'  \href{https://www.epa.gov/lmop/landfill-technical-data}{EPA Landfill Methane
#'  Outreach Program (LMOP)} that are not already accounted for by the GHGRP.
#'
#'  Landfills have 2 options for reporting their emissions to the GHGRP -
#'  equation HH-6 and HH-8.  HH-6 is based on a first order decay model, HH-8 is
#'  based on the collection efficiency of a gas collection system. Landfills can
#'  choose to treat either as the reported value, but both are provided to the
#'  GHGRP.  The generation first values (HH-6) or collection first (HH-8) can be
#'  used rather than the reported values when a gas collection system exists.
#'  Note the GHGRP total used for the LMOP calculation will be the reported
#'  values regardless, as these are what are used in the GHGI.
#'
#'  The GHGRP includes only facilities that emit at least 25,000 metric tons of
#'  carbon dioxide equivalent while the GHGI is intended to capture all national
#'  emissions and LMOP is a voluntary program with location and other details,
#'  but no emissions information.  GHGRP data is available starting in 2011 and
#'  generally is about 2 years behind present day, the GHGI is available
#'  starting in 1990 and is updated approximately in sync with the GHGRP, and
#'  the LMOP is updated more frequently, sometimes multiple times per year.  All
#'  three datasets are annual.  The GHGRP and LMOP are at the facility scale
#'  while the GHGI is national totals.
#'@inheritParams define_custom_domain
#'@inheritParams CH4_inventory_build
#'
#'@param domain SpatVector polygon outlining the desired output area as created
#'  in \code{\link{CH4_inventory_build}}.
#'@param domain_template SpatRaster providing the desired output grid, including
#'  the desired resolution and coordinate reference system as created in
#'  \code{\link{CH4_inventory_build}}.
#'@param state_name_list Character vector listing all states within the desired
#'  domain as created in \code{\link{CH4_inventory_build}}.
#'@param output_directory Character providing the full filepath to save
#'  processed data as created in \code{\link{CH4_inventory_build}}
#'@param GHGI_data_yr Integer providing the year of data to use for the
#'  \href{https://www.epa.gov/ghgreporting}{Environmental Protection Agency
#'  (EPA) Greenhouse Gas Reporting Program (GHGRP) data}.
#'@param verbose Logical indicating whether to save visuals. This includes up to
#'  5 plots of the gridded methane emissions on log scales, one for LMOP
#'  facilities, one for each variation of the GHGRP facilities, and one with the
#'  sum of LMOP and GHGRP emissions. The GHGRP data is the first available in
#'  the order of reported, generation first, then collection first.
#'@param GHGI_landfill_total Numeric.  Pulled from \code{\link{M3T_config}}.
#'@param GHGRP_facility_data Data.frame with the GHGRP location data for all
#'  years and states as prepared in \code{\link{CH4_inventory_build}} using the
#'  \code{Source_GHGRP_facility_data} provided in \code{\link{M3T_config}}.
#'@param GHGRP_combustion_emissions Data.frame with the GHGRP combustion
#'  emissions data for all years and states as prepared in
#'  \code{\link{CH4_inventory_build}} using
#'  \code{Source_GHGRP_combustion_emissions} provided in
#'  \code{\link{M3T_config}}.
#'@param Source_GHGRP_landfills Character.  Pulled from
#'  \code{\link{M3T_config}}.
#'@param Source_LMOP Character.  Pulled from \code{\link{M3T_config}}.
#'@param landfill_ghgrp_reported Logical.  Pulled from \code{\link{M3T_config}}.
#'@param landfill_ghgrp_generation_first Logical.  \code{\link{M3T_config}}.
#'@param landfill_ghgrp_collection_first Logical. \code{\link{M3T_config}}.
#'@param plot_directory Character. \strong{Optional}. Provides the full filepath
#'  to save figures. Only relevant if \code{verbose} = TRUE.
#'@param County_Tigerlines SpatVector. \strong{Optional}.
#'  \href{https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html}{US
#'  Census Tigerlines} files for visualization. Only relevant if \code{verbose} = TRUE.
#'@param State_CB SpatVector. \strong{Optional}.
#'  \href{https://www.census.gov/geographies/mapping-files/tme-series/geo/cartographic-boundary.html}{US
#'  Census Cartographic Boundary} files for visualization. Only relevant if
#'  \code{verbose} = TRUE.
#'@returns Nothing is returned from the function, but the main outputs are up to
#'  4 main netcdf files and 3 sector total netcdf files of gridded methane
#'  emissions from municipal landfills. The main files are titled
#'  "MSW_GHGRP_reported.nc", "MSW_GHGRP_generation_first.nc",
#'  "MSW_GHGRP_collection_first.nc", and "MSW_LMOP.nc". The sector total
#'  netcdf files are titled "Landfill_sector_total_GHGRP_reported.nc",
#'  "Landfill_sector_total_GHGRP_generation_first.nc", and
#'  "Landfill_sector_total_GHGRP_collection_first.nc". 2 csv's are also saved 
#'  with the filtered, processed input data titled "GHGRP_MSW_Landfills.csv" 
#'  and "LMOP_MSW_Landfills.csv".
#'@seealso [CH4_inventory_build()] Calculates methane inventory using settings
#'  provided in config.
#'
#'  [M3T_config] Generates the config function with user-editable settings used
#'  throughout processing.
#'@keywords internal


Municipal_solid_waste <- function(input_directory,
                                  domain,
                                  domain_template,
                                  state_name_list,
                                  output_directory,
                                  inventory_year,
                                  GHGI_data_yr,
                                  verbose,
                                  GHGI_landfill_total,
                                  GHGRP_facility_data,
                                  GHGRP_combustion_emissions,
                                  Source_GHGRP_landfills,
                                  Source_LMOP,
                                  landfill_ghgrp_reported,
                                  landfill_ghgrp_generation_first,
                                  landfill_ghgrp_collection_first,
                                  plot_directory,
                                  County_Tigerlines,
                                  State_CB){
  
  starttime <- Sys.time()
  cat("Starting landfill sector: Municipal_solid_waste\n")
  
  Landfill_output_directory <- file.path(output_directory,"Landfills")
  dir.create(Landfill_output_directory,showWarnings = F)
  ################################################################################
  #Get the GHGRP landfill data
  
  #Source = M3T means the data is already available
  if(Source_GHGRP_landfills!="M3T"){
    
    #otherwise, download and prep data
    if(Source_GHGRP_landfills=="download"){
      #Download the relevant emissions data using the API
      #(https://www.epa.gov/enviro/envirofacts-data-service-api)
      
      #download the relevant landfill-sector data in MT CH4/yr
      #(https://www.epa.gov/enviro/greenhouse-gas-model).  Must download the
      #relevant data for each possible sector separately as emissions are split by
      #sector (i.e., gas capture for electricity is subpart D, stationary combustion
      #is C, and landfill emissions HH - all of which can occur at the same
      #landfill).  Only C and HH are included as "reported" municipal emissions
      #exclude subpart C and D. Landfills have 2 options for reporting their
      #emissions - equation HH-6 and HH-8.  HH-6 is based on a first order decay
      #model, HH-8 is based on collection efficiency of a gas collection system.
      #The ghgrp_landfill_detail_emissions include both as either can be the
      #"reported" value.
      
      ghgrp_landfill_file <- file.path(input_directory,"GHGRP","landfill_HH.csv")
      ghgrp_landfill_system_details_file <- file.path(input_directory,"GHGRP","landfill_HH_details.csv")
      
      data_URL <- "https://data.epa.gov/dmapservice/ghg.hh_subpart_level_information/csv"
      Trycatch_downloader(URL = data_URL,method = "save",output_location = ghgrp_landfill_file,
                          error_message = paste0("Greenhouse Gas Reporting Program data could not be downloaded using API link: ",data_URL))
      
      data_URL <- "https://data.epa.gov/dmapservice/ghg.hh_gas_collection_system_detls/csv"
      Trycatch_downloader(URL = data_URL,method = "save",output_location = ghgrp_landfill_system_details_file,
                          error_message = paste0("Greenhouse Gas Reporting Program data could not be downloaded using API link: ",data_URL))
    }else{
      ghgrp_landfill_file <- file.path(input_directory,"GHGRP","User_supplied_landfill_file.csv")
      ghgrp_landfill_system_details_file <- file.path(input_directory,"GHGRP","User_supplied_landfill_detail_file.csv")
      
      invisible(file.copy(Source_GHGRP_landfills,file.path(input_directory,"GHGRP",ghgrp_landfill_file,overwrite = T)))
      invisible(file.copy(Source_GHGRP_landfills,file.path(input_directory,"GHGRP",ghgrp_landfill_system_details_file,overwrite = T)))
    }
    ################################################################################
    #load in and combine the emission data appropriately
    
    #load in the files
    ghgrp_landfill_only_emissions <- utils::read.csv(ghgrp_landfill_file)
    ghgrp_landfill_detail_emissions <- utils::read.csv(ghgrp_landfill_system_details_file)
    
    ghgrp_landfill_only_emissions <- make_consistent(ghgrp_landfill_only_emissions)
    
    #Now add the HH-6 and HH-8 emission rates to the dataframe too
    GHGRP_landfills <- merge(ghgrp_landfill_only_emissions,
                             ghgrp_landfill_detail_emissions[,c("facility_id","reporting_year","equation_hh6_result","equation_hh8_result")],
                             by.x=c("facility_id","year"),by.y=c("facility_id","reporting_year"),all.x=T)
    colnames(GHGRP_landfills) <- gsub("equation_hh6_result","generation_first_HH6",colnames(GHGRP_landfills))
    colnames(GHGRP_landfills) <- gsub("equation_hh8_result","collection_first_HH8",colnames(GHGRP_landfills))
  }else{
    GHGRP_landfills <- M3T::GHGRP_landfills
  }
  ################################################################################
  #Use yr determined in CH4 inventory build - closest to inventory year with
  #both GHGI and GHGRP
  
  GHGRP_year <- GHGI_data_yr
  
  ################################################################################
  #Combine the combustion emissions data and put them all together
  colnames(GHGRP_combustion_emissions) <- gsub("ghg_quantity","C_emissions",colnames(GHGRP_combustion_emissions))
  colnames(GHGRP_landfills) <- gsub("ghg_quantity","HH_emissions",colnames(GHGRP_landfills))
  
  #combine combustion and landfills into 1 dataframe - using landfill emissions
  #as the base to get ID/year matches from
  ghgrp_landfill_emissions=merge(GHGRP_landfills,GHGRP_combustion_emissions,by=c("facility_id","year","facility_name","ghg_name"),all.x=T)
  
  #convert the relevant columns to numeric class
  ghgrp_landfill_emissions[,c("HH_emissions","C_emissions","generation_first_HH6","collection_first_HH8")] <- apply(ghgrp_landfill_emissions[,c("HH_emissions","C_emissions","generation_first_HH6","collection_first_HH8")],
                                                                                                                    2,FUN = function(x){as.numeric(x)})
  
  #calculate the sum of combustion (C) and reported landfill (HH) CH4 emissions.  
  ghgrp_landfill_emissions$ghg_quantity <- rowSums(ghgrp_landfill_emissions[,c("HH_emissions","C_emissions")],na.rm=T)
  
  #Calculate national total in the GHGRP for the year of interest
  #MT CH4/yr to Gg CH4/yr
  ghgrp_national <- sum(as.numeric(ghgrp_landfill_emissions$ghg_quantity[ghgrp_landfill_emissions$year==GHGRP_year]))/1000
  
  ################################################################################  
  # Now calculate national totals
  
  EPA_total <- GHGI_landfill_total
  non_ghgrp_total <- EPA_total - ghgrp_national
  
  ################################################################################
  #Merge with location-like data and account for facilities that stopped
  #reporting without a valid reason
  
  #combine the datasets by ID, and year
  ghgrp_all_data <- merge(GHGRP_facility_data,ghgrp_landfill_emissions,
                          by=c("facility_id","year"))
  
  #keep only data for the year of interest
  ghgrp <- ghgrp_all_data[ghgrp_all_data$year==GHGRP_year,]
  
  #identify facilities that stopped reporting without a valid reason, then
  #subset to only landfill facilities and only those that we don't have data for
  #(e.g., it stopped reporting back one year, but started up again before
  #inventory_year)
  nonreporting_facilities <- unique(GHGRP_facility_data$facility_id[GHGRP_facility_data$reporting_status=="STOPPED_REPORTING_UNKNOWN_REASON" & GHGRP_facility_data$year<=GHGRP_year])
  nonreporting_landfills <- nonreporting_facilities[which(nonreporting_facilities %in% unique(ghgrp_landfill_emissions$facility_id))]
  nonreporting_landfills <- nonreporting_landfills[!(nonreporting_landfills %in% unique(ghgrp$facility_id))]
  
  ################################################################################
  #handle those that stopped reporting without a valid reason
  
  # find the closest data available for those that stopped reporting (this can
  # be after the inventory year in some cases)
  nonreporting_landfill_data <- ghgrp_all_data[ghgrp_all_data$facility_id %in% nonreporting_landfills,]
  nonreporting_landfill_data=tapply(nonreporting_landfill_data,
                                    INDEX=nonreporting_landfill_data$facility_id,
                                    FUN=function(x){x[which.min(abs(x$year-GHGRP_year)),]})
  if(length(nonreporting_landfill_data)>0){
    nonreporting_landfill_data=do.call(rbind, nonreporting_landfill_data)
  }
  
  #add this most recent data to the GHGRP dataset
  ghgrp_updated <- rbind(nonreporting_landfill_data,ghgrp)
  
  #convert the relevant columns to numeric class
  ghgrp_updated[,c("latitude","longitude","ghg_quantity","HH_emissions",
                   "C_emissions","collection_first_HH8","generation_first_HH6")] <- apply(ghgrp_updated[,c("latitude","longitude","ghg_quantity","HH_emissions",
                                                                                                           "C_emissions","collection_first_HH8","generation_first_HH6")],
                                                                                          2,FUN = function(x){as.numeric(x)})
  ##############################################################################
  #convert and subset data for all 3 methods
  
  #MT CH4/yr to mol/s of CH4
  ghgrp_updated[,c("HH_emissions","generation_first_HH6",
                   "collection_first_HH8","C_emissions")] <- ghgrp_updated[,c("HH_emissions","generation_first_HH6",
                                                                              "collection_first_HH8","C_emissions")]*1e6/(16.043*365*24*60*60)
  
  ghgrp_updated$reported_method <- rowSums(ghgrp_updated[,c("HH_emissions","C_emissions")],na.rm=T)
  ghgrp_updated$collection_first_method <- rowSums(ghgrp_updated[,c("collection_first_HH8","C_emissions")],na.rm=T)
  ghgrp_updated$generation_first_method <- rowSums(ghgrp_updated[,c("generation_first_HH6","C_emissions")],na.rm=T)
  
  #replace those with no data with the reported values (these are landfills
  #without gas capture systems)
  ghgrp_updated$generation_first_method[is.na(ghgrp_updated$generation_first_HH6)] <- ghgrp_updated$reported_method[is.na(ghgrp_updated$generation_first_HH6)]
  ghgrp_updated$collection_first_method[is.na(ghgrp_updated$collection_first_HH8)] <- ghgrp_updated$reported_method[is.na(ghgrp_updated$collection_first_HH8)]
  
  ghgrp_updated <- ghgrp_updated[,c("facility_id","year","facility_name.x",
                                    "latitude","longitude","state",
                                    "reported_method",
                                    "generation_first_method","collection_first_method")]
  colnames(ghgrp_updated) <- c("GHGRP_ID","year","facility_name",
                               "latitude","longitude","state",
                               "reported_method",
                               "generation_first_method","collection_first_method")
  ##############################################################################
  #Now convert to spatial.
  
  #convert to a spatial object, crop to domain
  ghgrp_crop <- terra::vect(ghgrp_updated,geom=c("longitude","latitude"))
  terra::crs(ghgrp_crop) <- "epsg:4326"
  ghgrp_crop <- terra::project(ghgrp_crop,terra::crs(domain))
  ghgrp_crop <- terra::crop(ghgrp_crop, domain)
  ghgrp_crop <- terra::mask(ghgrp_crop,domain)
  
  ##############################################################################
  #save a csv for easy understanding of the filtered input data
  
  #prep the data for a csv too
  ghgrp_latlong <- terra::crds(terra::project(ghgrp_crop,"epsg:4326"))
  colnames(ghgrp_latlong) <- c("longitude","latitude")
  csv_data <- as.data.frame(cbind(ghgrp_crop,ghgrp_latlong))
  
  colnames(csv_data) <- c("GHGRP_ID","year","facility_name",
                          "state",
                          "reported_method_mol_per_s",
                          "generation_first_method_mol_per_s",
                          "collection_first_method_mol_per_s",
                          "longitude","latitude")
  
  csv_data <- csv_data[order(csv_data$year,csv_data$facility_name),]
  
  utils::write.csv(csv_data,file.path(Landfill_output_directory,"GHGRP_MSW_Landfills.csv"),
                   row.names = F)
  ##############################################################################
  # Now rasterise and save
  
  landfill_rasterize <- function(emission_var,outname,longname){
    ghgrp_rast <- terra::rasterize(ghgrp_crop, domain_template, emission_var, fun=sum)
    # Calculate flux, mol/s to nmol/m2/s
    ghgrp_flux <- ghgrp_rast*1e9/(terra::cellSize(ghgrp_rast,unit="m"))  
    ghgrp_flux[is.na(ghgrp_flux)]<-0
    
    # Now write the raster as a netcdf files
    writeCDF_no_newline(ghgrp_flux,
                        file.path(Landfill_output_directory,outname),
                        force_v4=TRUE,
                        varname='methane_emissions',
                        unit='nmol/m2/s',
                        longname=longname,
                        missval=-9999,
                        overwrite=TRUE)
    return(ghgrp_flux)
  }
  
  if(landfill_ghgrp_reported){
    ghgrp_reported <- landfill_rasterize(emission_var="reported_method",outname="MSW_GHGRP_reported.nc",longname='Methane emissions from municipal solid waste landfills that report to GHGRP')
  }
  if(landfill_ghgrp_generation_first){
    ghgrp_generation_first <- landfill_rasterize(emission_var="generation_first_method",outname="MSW_GHGRP_generation_first.nc",longname='Methane emissions from municipal solid waste landfills that report to GHGRP - forcing the method to the first order decay method for facilities with gas collection systems')
  }
    if(landfill_ghgrp_collection_first){
    ghgrp_collection_first <- landfill_rasterize(emission_var="collection_first_method",outname="MSW_GHGRP_collection_first.nc",longname='Methane emissions from municipal solid waste landfills that report to GHGRP - forcing the method to the collection efficiency based method for facilities with gas collection systems')
  }  
  
  rm(ghgrp_all_data,nonreporting_facilities,nonreporting_landfills)
  ################################################################################
  #Download, and load in LMOP data
  
  #Source=M3T means the data is already available
  if(Source_LMOP!="M3T"){
    if(Source_LMOP=="download"){
      LMOP_file <- list.files(input_directory,pattern="*LMOP_landfill_only.xlsx",full.names = T)
      
      # if(identical(LMOP_file,character(0))){
      #download the webpage and load in the HTML
      data_URL <- paste0("https://www.epa.gov/lmop/landfill-technical-data")
      download_dest <- tempfile(fileext = ".html")
      Trycatch_downloader(URL = data_URL,method = "save",output_location = download_dest,
                          error_message = paste0("LMOP data could not be webscraped from webpage: ",data_URL))
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
      LMOP_yr <- substr(data_URL2,regexpr("20.{2}",data_URL2)[1],regexpr("20.{2}",data_URL2)[1]+3)
      LMOP_file <- file.path(input_directory,paste0(LMOP_yr,"_LMOP_landfill_only.xlsx"))
      Trycatch_downloader(URL = data_URL2,method = "save",output_location = LMOP_file,
                          error_message = paste0("LMOP data could not be downloaded from webpage:\n",data_URL2,"\nMake sure the main EPA page for it is still accurate:\n",data_URL))
      unlink(download_dest)
    }else{
      LMOP_file <- file.path(input_directory,"User_supplied_LMOP_file.xlsx")
      file.copy(Source_LMOP,LMOP_file,overwrite = T)
    }
    ################################################################################
    #Remove LMOP sites already in GHGRP.  Note facilities that used to report to
    #GHGRP and stopped with a valid reason are being retained as LMOP facilities
    #in this approach.
    LMOP <- readxl::read_xlsx(LMOP_file,sheet="LMOP Database",col_names = T)
    
    #This has some nans in, remove those
    LMOP <- subset(LMOP,!is.na(LMOP$Latitude))
  }else{
    LMOP <- M3T::LMOP_data
  }
  ################################################################################
  #Process LMOP
  
  LMOP_non_ghgrp <- LMOP[!(LMOP$`GHGRP ID` %in% ghgrp$facility_id),]
  
  #Make spatial
  LMOP_non_ghgrp <- terra::vect(LMOP_non_ghgrp,geom=c("Longitude","Latitude"))
  terra::crs(LMOP_non_ghgrp) <- "epsg:4326"
  LMOP_non_ghgrp <- terra::project(LMOP_non_ghgrp,terra::crs(domain))
  
  #Exclude those we already handled as they stopped reporting without a valid
  #reason.
  if(length(nonreporting_landfill_data)>0){
    LMOP_non_ghgrp <- LMOP_non_ghgrp[!(LMOP_non_ghgrp$`GHGRP ID` %in% nonreporting_landfill_data$facility_id),]
  }
  
  #Exclude those that opened after GHGRP_year.  This will retain those with
  #no open date provided
  indx <- LMOP_non_ghgrp$`Year Landfill Opened` <= GHGRP_year
  indx[is.na(indx)] <- TRUE
  LMOP_non_ghgrp <- LMOP_non_ghgrp[indx,]
  
  LMOP_crop <- terra::crop(LMOP_non_ghgrp, domain)
  LMOP_crop <- terra::mask(LMOP_crop,domain)
  
  # Find avg emission per non-GHGRP LMOP landfill (including the ones with no coordinates)
  avg_non_ghgrp <- non_ghgrp_total/nrow(LMOP_non_ghgrp)
  # For comparison, calculate avg ghgrp
  avg_ghgrp <- ghgrp_national/nrow(ghgrp)
  # Assign the avg emissions to LMOP landfills
  LMOP_crop$emiss <- avg_non_ghgrp*1e9/(16.043*365*24*60*60)   
  #Gg CH4/yr to mol/s of CH4
  
  ##############################################################################
  #save a csv for easy understanding of the filtered input data
  
  #prep the data for a csv too
  LMOP_latlong <- terra::crds(terra::project(LMOP_crop,"epsg:4326"))
  colnames(LMOP_latlong) <- c("longitude","latitude")
  csv_data <- as.data.frame(cbind(LMOP_crop,LMOP_latlong))
  
  colnames(csv_data) <- c("GHGRP_ID","Landfill_Name","Landfill_Opened",
                          "Emissions_mol_per_s","longitude","latitude")
  
  utils::write.csv(csv_data,file.path(Landfill_output_directory,"LMOP_MSW_Landfills.csv"),
                   row.names = F)
  ################################################################################
  # Now rasterise and save
  
  LMOP_rast <- terra::rasterize(LMOP_crop, domain_template, field="emiss", fun=sum)
  # Calculate flux, mol/s to nmol/m2/s
  LMOP_flux <- LMOP_rast*1e9/(terra::cellSize(LMOP_rast,unit="m"))
  LMOP_flux[is.na(LMOP_flux)]<-0
  
  writeCDF_no_newline(LMOP_flux,
                      file.path(Landfill_output_directory,'MSW_LMOP.nc'),
                      force_v4=TRUE,
                      varname='methane_emissions',
                      unit='nmol/m2/s',
                      longname='Methane emissions from municipal solid waste landfills that report to LMOP but not GHGRP',
                      missval=-9999,
                      overwrite=TRUE)
  
  
  ################################################################################
  #Create a sector total, 1 per variant
  
  if(landfill_ghgrp_reported){
    writeCDF_no_newline(LMOP_flux+ghgrp_reported,
                        file.path(output_directory,paste0('Landfill_sector_total_GHGRP_reported.nc')),
                        force_v4=TRUE,
                        varname='methane_emissions',
                        unit='nmol/m2/s',
                        longname='Methane emissions from municipal solid waste landfills',
                        missval=-9999,
                        overwrite=TRUE)
  }
  
  if(landfill_ghgrp_generation_first){
    writeCDF_no_newline(LMOP_flux+ghgrp_generation_first,
                        file.path(output_directory,paste0('Landfill_sector_total_GHGRP_generation_first.nc')),
                        force_v4=TRUE,
                        varname='methane_emissions',
                        unit='nmol/m2/s',
                        longname='Methane emissions from municipal solid waste landfills',
                        missval=-9999,
                        overwrite=TRUE)
  }
  
  if(landfill_ghgrp_collection_first){
    writeCDF_no_newline(LMOP_flux+ghgrp_collection_first,
                        file.path(output_directory,paste0('Landfill_sector_total_GHGRP_collection_first.nc')),
                        force_v4=TRUE,
                        varname='methane_emissions',
                        unit='nmol/m2/s',
                        longname='Methane emissions from municipal solid waste landfills',
                        missval=-9999,
                        overwrite=TRUE)
  }
  
  ################################################################################
  #Finally, load up some functions and plot up this output nicely
  
  if(verbose){
    zlim_min <- 5000
    zlim_max <- 0
    
    
    LMOP_flux[LMOP_flux==0] <- NA
    zlim_max <- max(terra::global(LMOP_flux,max,na.rm=T),zlim_max,na.rm=T)
    zlim_min <- min(terra::global(LMOP_flux,min,na.rm=T),zlim_min,na.rm=T)
    
    if(landfill_ghgrp_reported){
      ghgrp_reported[ghgrp_reported==0] <- NA
      zlim_max <- max(terra::global(ghgrp_reported,max,na.rm=T),zlim_max,na.rm=T)
      zlim_min <- min(terra::global(ghgrp_reported,min,na.rm=T),zlim_min,na.rm=T)
    }
    
    if(landfill_ghgrp_generation_first){
      ghgrp_generation_first[ghgrp_generation_first==0] <- NA
      zlim_max <- max(terra::global(ghgrp_generation_first,max,na.rm=T),zlim_max,na.rm=T)
      zlim_min <- min(terra::global(ghgrp_generation_first,min,na.rm=T),zlim_min,na.rm=T)
    }
    
    if(landfill_ghgrp_collection_first){
      ghgrp_collection_first[ghgrp_collection_first==0] <- NA
      zlim_max <- max(terra::global(ghgrp_collection_first,max,na.rm=T),zlim_max,na.rm=T)
      zlim_min <- min(terra::global(ghgrp_collection_first,min,na.rm=T),zlim_min,na.rm=T)
    }
    zlim_max <- log10(zlim_max)
    zlim_min <- log10(zlim_min)
    
    
    #now actually do the plotting
    if(landfill_ghgrp_reported){
      log_plot(ghgrp_reported,filename="MSW_GHGRP_reported",
               "Municipal Solid Waste -\n GHGRP reporters",
               zlim_min=zlim_min,zlim_max=zlim_max,plot_directory=plot_directory,
               domain=domain,County_Tigerlines=County_Tigerlines,
               State_CB=State_CB)
    }
    if(landfill_ghgrp_generation_first){
      log_plot(ghgrp_generation_first,filename="MSW_GHGRP_generation_first",
               "Municipal Solid Waste -\n GHGRP reporters - decay model based emissions",
               zlim_min=zlim_min,zlim_max=zlim_max,plot_directory=plot_directory,
               domain=domain,County_Tigerlines=County_Tigerlines,
               State_CB=State_CB)
    }
    if(landfill_ghgrp_collection_first){
      log_plot(ghgrp_collection_first,filename="MSW_GHGRP_collection_first",
               "Municipal Solid Waste -\n GHGRP reporters - collection efficiency based\nemissions",
               zlim_min=zlim_min,zlim_max=zlim_max,plot_directory=plot_directory,
               domain=domain,County_Tigerlines=County_Tigerlines,
               State_CB=State_CB)
    }
    log_plot(LMOP_flux,filename="MSW_LMOP",
             "Municipal Solid Waste -\n (GHGI - GHGRP) distributed using \nLandfill Methane Outreach Program",
             zlim_min=zlim_min,zlim_max=zlim_max,plot_directory=plot_directory,
             domain=domain,County_Tigerlines=County_Tigerlines,
             State_CB=State_CB)
    
    if(landfill_ghgrp_reported){
      Summed_solid_waste <- sum(c(ghgrp_reported,LMOP_flux),na.rm=T)
      title <- "Municipal Solid Waste -\n GHGRP reported emissions + (GHGI - GHGRP) distributed using \nLandfill Methane Outreach Program"
    }else if(landfill_ghgrp_generation_first){
      Summed_solid_waste <- sum(c(ghgrp_generation_first,LMOP_flux),na.rm=T)
      title <- "Municipal Solid Waste -\n GHGRP generation first emissions + (GHGI - GHGRP) distributed using \nLandfill Methane Outreach Program"
    }else if(landfill_ghgrp_collection_first){
      Summed_solid_waste <- sum(c(ghgrp_collection_first,LMOP_flux),na.rm=T)
      title <- "Municipal Solid Waste -\n GHGRP collection first emissions + (GHGI - GHGRP) distributed using \nLandfill Methane Outreach Program"
    }
    log_plot(Summed_solid_waste,title,
             plot_directory=plot_directory,
             domain=domain,County_Tigerlines=County_Tigerlines,
             State_CB=State_CB)
  }
  cat("Finished landfill sector: Municipal_solid_waste at",format(Sys.time(),"%H:%M"),"with a total runtime of",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes\n\n")
}

