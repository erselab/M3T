#'@title Create gridded municipal solid waste methane emissions maps
#'
#'@description `Municipal_solid_waste` writes up to 4 netcdf files of gridded
#'  methane emissions from municipal landfills, as well as optional visuals and
#'  more easily read csv files
#'
#'@details This function calculates and grids methane emissions from municipal
#'  landfills. It uses the Environmental Protection Agency's (EPA) Greenhouse
#'  Gas Reporting Program (GHGRP) emissions when available.  It then calculates
#'  the difference between national GHGRP emissions and the EPA national
#'  Greenhouse Gas Inventory (GHGI) emissions (GHGI - GHGRP) and distributes
#'  this residual equally to all facilities in the EPA Landfill Methane Outreach
#'  Program (LMOP) that are not already accounted for by the GHGRP.
#'
#'  The necessary GHGRP and LMOP data will be automatically downloaded.
#'
#'  Landfills have 2 options for reporting their emissions to the GHGRP -
#'  equation HH-6 and HH-8.  HH-6 is based on a first order decay model, HH-8 is
#'  based on the collection efficiency of a gas collection system. Landfills can
#'  choose to treat either as the reported value, but both are provided to the
#'  GHGRP.  If set in the config, the modeled values (HH-6) or collection
#'  efficiency (HH-8) can be used rather than the reported values when a gas
#'  collection system exists.  Note the GHGRP total used for the LMOP
#'  calculation will be the reported values regardless, as these are what are
#'  used in the GHGI.
#'
#'  The GHGRP includes only facilities that emit at least 25,000 metric tons of
#'  carbon dioxide equivalent while the GHGI is intended to capture all national
#'  emissions and LMOP is a voluntary program with location and other details,
#'  but no emissions information.  GHGRP data is available starting in 2010 and
#'  generally is about 2 years behind present day, the GHGI is available
#'  starting in 1990 and is updated approximately in sync with the GHGRP, and
#'  the LMOP is updated more frequently, sometimes multiple times per year.  All
#'  three datasets are annual.  The GHGRP and LMOP are at the facility scale
#'  while the GHGI is national totals.
#'
#'  The GHGI is available at
#'  \url{https://www.epa.gov/ghgemissions/inventory-us-greenhouse-gas-emissions-and-sinks}
#'  The GHGRP is available at \url{https://www.epa.gov/ghgreporting} LMOP is
#'  available at \url{https://www.epa.gov/lmop/landfill-technical-data}
#'
#'  LMOP and GHGRP gridded emissions maps are saved separately.
#'@inheritParams define_custom_domain 
#'@param domain SpatVector polygon outlining the desired output area
#'@param domain_template SpatRaster providing the desired output grid, including
#'  the desired resolution and coordinate reference system
#'@param state_name_list Character vector listing all states within the desired
#'  domain
#'@param output_directory Character providing the full filepath to save
#'  processed data
#'@param inventory_year Numeric indicating the desired year of data to use.
#'@param verbose Logical indicating whether to save additional output.  This
#'  includes 2 csv files providing the downloaded information used for all
#'  landfills within the domain, separated between LMOP and GHGRP.  It also
#'  includes up to 4 plots of the gridded methane emissions on log scales, one
#'  for LMOP facilities and one for each variation of the GHGRP facilities.
#'@param GHGI_landfill_total Numeric.  Pulled from config file.  The total
#'  national emissions from municipal solid waste from the GHGI for the
#'  inventory_year in kilotons per year, equivalent to gigagrams per year.  The
#'  GHGI is available at
#'  \url{https://www.epa.gov/ghgemissions/inventory-us-greenhouse-gas-emissions-and-sinks}
#'  and the value can be found in the table titled 'CH4 emissions from Landfills
#'  (kt)' as row 'MSW net CH4 Emissions'.
#'@param GHGRP_facility_data Data.frame with the GHGRP location data for all
#'  years and states.  See
#'  \url{https://www.epa.gov/enviro/envirofacts-data-service-api}
#'@param landfill_ghgrp_reported Logical.  Pulled from config file.  Whether or
#'  not to use reported GHGRP values.
#'@param landfill_ghgrp_modeled Logical.  Pulled from config file.  Whether or
#'  not to overwrite reported GHGRP values with the modeled emissions (HH-6) for
#'  landfills with gas collection systems.
#'@param landfill_ghgrp_collection_efficiency Logical.  Pulled from config file.
#'  Whether or not to overwrite reported GHGRP values with the collection
#'  efficiency based emissions (HH-8) for landfills with gas collection systems.
#'@param plot_directory Character. \strong{Optional}. Provides the full filepath to save figures.
#'  Only relevant if verbose = TRUE.
#'@param County_Tigerlines SpatVector. \strong{Optional}. United States Census Bureau county
#'  shapefile.  Available at
#'  \url{https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html}.
#'  Only relevant if verbose=TRUE.
#'@param State_CB SpatVector. \strong{Optional}. US Census Cartographic Boundary files for
#'  visualization
#'  \url{https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html}.
#'  Only relevant if verbose=T
#'@returns Nothing is returned from the function, but the main outputs are up to
#'  4 netcdf files of the methane emissions of municipal landfills.  They are
#'  titled "MSW_GHGRP_reported.nc", "MSW_GHGRP_modeled.nc",
#'  "MSW_GHGRP_collection_efficiency.nc", and "MSW_LMOP.nc".
#'
#'  If verbose is set to TRUE, then "MSW_GHGRP_all.csv" and "MSW_LMOP_all.csv"
#'  are also saved.  The csvs include all downloaded variables for landfills
#'  within the domain.
#'@examples
#'library(terra)
#'grid_bbox=cbind(c(-76.65,-73.65),c(38.97,40.97))
#'grid_res=0.01
#'grid_crs="epsg:4326"
#'grid <- rast(nrows=diff(range(grid_bbox[,2]))/grid_res,
#'             ncols=diff(range(grid_bbox[,1]))/grid_res, xmin=min(grid_bbox[,1]),
#'             xmax=max(grid_bbox[,1]), ymin=min(grid_bbox[,2]), ymax=max(grid_bbox[,2]),
#'             crs=grid_crs)
#'grid_polygon <- vect(ext(domain_template))
#'crs(grid_polygon) <- grid_crs
#'
#'GHGRP_facility_data <- read.csv("~/../Desktop/GHGRP/facility_info.csv")
#'GHGRP_facility_data$county_fips <- sprintf("%05d",GHGRP_facility_data$county_fips)
#'GHGRP_facility_data$zip <- sprintf("%05d",GHGRP_facility_data$zip)
#'
#'
#' Municipal_solid_waste(domain=grid_polygon,
#'                       domain_template=grid,
#'                       state_name_list=c("DE","MD","NJ","NY","PA"),
#'                       output_directory="~/../Desktop/out/",
#'                       input_directory="~/../Desktop/in/",
#'                       inventory_year=2018,
#'                       verbose=TRUE,
#'                       GHGI_landfill_total = 3943,
#'                       GHGRP_facility_data="~/../Desktop/in/GHGRP/facility_info.csv",
#'                       landfill_ghgrp_reported=TRUE,
#'                       landfill_ghgrp_modeled=TRUE,
#'                       landfill_ghgrp_collection_efficiency=TRUE,
#'                       plot_directory="~/../Desktop/plots/"
#'                       County_Tigerlines=vect("~/../Desktop/in/County_Tigerlines/tl_2018_us_county.shp"),
#'                       State_Tigerlines=vect("~/../Desktop/in/State_Tigerlines/tl_2018_us_state.shp"))
#'@inherit CH4_inventory_build author
#'@seealso [CH4_inventory_build()] Calculates methane inventory using settings provided in config.
#'@export





## Landfill_emissions_r1.R
## Developed: 2021-10-22 17:00
## Finalized: 2023-02-03
#
# Calculate emissions from landfills
# There are 2 sources of data:
# GHGRP - has emissions but only certain landfills
# LMOP - has more landfills than GHGRP, and some details about them, but not 
# emissions
#
# Note - to convert to fluxes this code uses the raster packages area function.
# This simplifies the area calculation of a lat/long box and is not appropriate
# near the poles
#
# The national EPA (GHGI) inventory total comes from taking the GHGRP emissions 
# and applying a scale factor to account for the non-reporting landfills.
# This scale factor is based on the LMOP database and the WBJ directory (which it appears you have to pay for: https://www.wasteinfo.com/diratlas.htm)
# Details are given on page 168 (A-457) of this report:
# https://www.epa.gov/sites/production/files/2021-04/documents/us-ghg-inventory-2021-annex-3-additional-source-or-sink-categories-part-b.pdf
# They identified 1544 landfills that accepted MSW between 1940 and 2016 and had never reported to the GHGRP
# For now, calculate total emissions for non-GHGRP-reporting landfills in LMOP as a residual between the GHGI and GHGRP totals
# Then allocate equally among non-GHGRP reporters at the national level (note that many landfills don't report waste in place)
# Note that we can only do MSW landfills like this, as industrial waste landfills don't report to LMOP
# There are far fewer GHGRP industrial waste landfills
# To get the non-reporters I could potentially trawl through the FRS data and work out what is an industrial landfill and what isn't
# But leave this out here and just use the GEPA industrial landfill emissions


Municipal_solid_waste <- function(input_directory,
                                  domain,
                                  domain_template,
                                  state_name_list,
                                  output_directory,
                                  inventory_year,
                                  verbose,
                                  GHGI_landfill_total,
                                  GHGRP_facility_data,
                                  GHGRP_combustion_emissions,
                                  Source_GHGRP_landfills,
                                  Source_LMOP,
                                  landfill_ghgrp_reported,
                                  landfill_ghgrp_modeled,
                                  landfill_ghgrp_collection_efficiency,
                                  plot_directory,
                                  County_Tigerlines,
                                  State_CB){
  
  starttime <- Sys.time()
  cat("Starting landfill sector: Municipal_solid_waste\n")
  
  Landfill_output_directory <- file.path(output_directory,"Landfills")
  dir.create(Landfill_output_directory,showWarnings = F)
  ################################################################################
  #Get the GHGRP landfill data
  
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
  }else if(Source_GHGRP_landfills=="default"){
    #UPDATE TO ZENODO
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
  colnames(GHGRP_landfills) <- gsub("equation_hh6_result","HH_modeled",colnames(GHGRP_landfills))
  colnames(GHGRP_landfills) <- gsub("equation_hh8_result","HH_collection_efficiency",colnames(GHGRP_landfills))
  ################################################################################
  #Determine nearest year available
  
  GHGRP_year <- unique(GHGRP_landfills$year)
  GHGRP_year <- GHGRP_year[which.min(abs(GHGRP_year - inventory_year))]
  if(inventory_year!=GHGRP_year){
    cat("GHGRP does not include",inventory_year,"using",GHGRP_year,"as the nearest data available\n")
  }
  
  ################################################################################
  #Combine the combustion emissions data and put them all together
  colnames(GHGRP_combustion_emissions) <- gsub("ghg_quantity","C_emissions",colnames(GHGRP_combustion_emissions))
  colnames(GHGRP_landfills) <- gsub("ghg_quantity","HH_emissions",colnames(GHGRP_landfills))
  
  #combine combustion and landfills into 1 dataframe - using landfill emissions
  #as the base to get ID/year matches from
  ghgrp_landfill_emissions=merge(GHGRP_landfills,GHGRP_combustion_emissions,by=c("facility_id","year","facility_name","ghg_name"),all.x=T)
  
  #convert the relevant columns to numeric class
  ghgrp_landfill_emissions[,c("HH_emissions","C_emissions","HH_modeled","HH_collection_efficiency")] <- apply(ghgrp_landfill_emissions[,c("HH_emissions","C_emissions","HH_modeled","HH_collection_efficiency")],
                                                                                                              2,FUN = function(x){as.numeric(x)})
  
  #calculate the sum of combustion (C) and landfill (HH) CH4 emissions.  
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
  #(e.g., it stopped reporting back in 2015, but reported again post 2018)
  nonreporting_facilities <- unique(GHGRP_facility_data$facility_id[GHGRP_facility_data$reporting_status=="STOPPED_REPORTING_UNKNOWN_REASON" & GHGRP_facility_data$year<=GHGRP_year])
  nonreporting_landfills <- nonreporting_facilities[which(nonreporting_facilities %in% unique(ghgrp_landfill_emissions$facility_id))]
  nonreporting_landfills <- nonreporting_landfills[!(nonreporting_landfills %in% unique(ghgrp$facility_id))]
  
  ################################################################################
  #write a function to take the data and process into raster
  
  Finalize_ghgrp <- function(outname,longname){
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
    ghgrp_updated[,c("latitude","longitude","ghg_quantity")] <- apply(ghgrp_updated[,c("latitude","longitude","ghg_quantity")],
                                                                      2,FUN = function(x){as.numeric(x)})
    ##############################################################################
    #Now convert to spatial.
    
    #convert to a spatial object, crop to domain, convert units
    ghgrp_crop <- terra::vect(ghgrp_updated,geom=c("longitude","latitude"))
    terra::crs(ghgrp_crop) <- "epsg:4326"
    ghgrp_crop <- terra::project(ghgrp_crop,terra::crs(domain))
    ghgrp_crop <- terra::crop(ghgrp_crop, domain)
    #MT CH4/yr to mol/s of CH4
    ghgrp_crop$emiss <- ghgrp_crop$ghg_quantity*1e6/(16.043*365*24*60*60)
    
    #save to the environment for use later
    assign("nonreporting_landfill_data",nonreporting_landfill_data,envir = parent.env(environment()))
    ################################################################################
    # Now rasterise and save
    
    ghgrp_rast <- terra::rasterize(ghgrp_crop, domain_template, "emiss", fun=sum)
    # Calculate flux, mol/s to nmol/m2/s
    ghgrp_flux <- ghgrp_rast*1e9/(terra::cellSize(ghgrp_rast,unit="m"))  
    ghgrp_flux[is.na(ghgrp_flux)]<-0
    ghgrp_flux <- terra::mask(ghgrp_flux,domain)
    
    if(verbose & outname=="MSW_GHGRP_reported.nc"){
      if(nrow(ghgrp_crop)>0){
        #sort both by name
        ghgrp_crop <- ghgrp_crop[order(ghgrp_crop$facility_name.x),]
        
        # Save point sources as csv files - first just the raw dataframe
        utils::write.csv(ghgrp_crop, file.path(Landfill_output_directory,'MSW_GHGRP_all.csv'))
      }
    }
    
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
    #recalculate the ghg quantity as landfill CH4 (HH) + stationary combustion
    #(C) - only necessary if rerunning line by line
    ghgrp_all_data$ghg_quantity <- rowSums(ghgrp_all_data[,c("HH_emissions","C_emissions")],na.rm=T)
    ghgrp$ghg_quantity <- rowSums(ghgrp[,c("HH_emissions","C_emissions")],na.rm=T)
    ghgrp_reported <- Finalize_ghgrp(outname="MSW_GHGRP_reported.nc",longname='Methane emissions from municipal solid waste landfills that report to GHGRP')
  }
  
  if(landfill_ghgrp_modeled){
    #overwrite ghg quantity using the HH modeled landfill CH4 rather than the
    #reported
    ghgrp_all_data$ghg_quantity[!is.na(ghgrp_all_data[,c("HH_modeled")])] <- rowSums(ghgrp_all_data[,c("HH_modeled","C_emissions")],na.rm=T)[!is.na(ghgrp_all_data[,c("HH_modeled")])]
    ghgrp$ghg_quantity[!is.na(ghgrp[,c("HH_modeled")])] <- rowSums(ghgrp[,c("HH_modeled","C_emissions")],na.rm=T)[!is.na(ghgrp[,c("HH_modeled")])]
    ghgrp_modeled <- Finalize_ghgrp(outname="MSW_GHGRP_modeled.nc",longname='Methane emissions from municipal solid waste landfills that report to GHGRP - forcing the method to the first order decay method for facilities with gas collection systems')
  }
  
  if(landfill_ghgrp_collection_efficiency){
    #overwrite ghg quantity using the HH collection efficiency landfill CH4
    #rather than the reported
    ghgrp_all_data$ghg_quantity[!is.na(ghgrp_all_data[,c("HH_collection_efficiency")])] <- rowSums(ghgrp_all_data[,c("HH_collection_efficiency","C_emissions")],na.rm=T)[!is.na(ghgrp_all_data[,c("HH_collection_efficiency")])]
    ghgrp$ghg_quantity[!is.na(ghgrp[,c("HH_collection_efficiency")])] <- rowSums(ghgrp[,c("HH_collection_efficiency","C_emissions")],na.rm=T)[!is.na(ghgrp[,c("HH_collection_efficiency")])]
    ghgrp_collection_efficiency <- Finalize_ghgrp(outname="MSW_GHGRP_collection_efficiency.nc",longname='Methane emissions from municipal solid waste landfills that report to GHGRP - forcing the method to the collection efficiency based method for facilities with gas collection systems')
  }
  
  rm(ghgrp_all_data,nonreporting_facilities,nonreporting_landfills)
  ################################################################################
  #Download, and load in LMOP data
  
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
    # }
    
  }else if(Source_LMOP=="default"){
    #UPDATE TO ZENODO
    LMOP <- LMOP_data
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
  LMOP <- subset(LMOP,!is.na(Latitude))
  
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
  
  # Find avg emission per non-GHGRP LMOP landfill (including the ones with no coordinates)
  avg_non_ghgrp <- non_ghgrp_total/nrow(LMOP_non_ghgrp)
  # For comparison, calculate avg ghgrp
  avg_ghgrp <- ghgrp_national/nrow(ghgrp)
  # Assign the avg emissions to LMOP landfills
  LMOP_crop$emiss <- avg_non_ghgrp*1e9/(16.043*365*24*60*60)   
  #Gg CH4/yr to mol/s of CH4
  ################################################################################
  # Now rasterise and save
  
  LMOP_rast <- terra::rasterize(LMOP_crop, domain_template, field="emiss", fun=sum)
  # Calculate flux, mol/s to nmol/m2/s
  LMOP_flux <- LMOP_rast*1e9/(terra::cellSize(LMOP_rast,unit="m"))
  LMOP_flux[is.na(LMOP_flux)]<-0
  LMOP_flux <- terra::mask(LMOP_flux,domain)
  
  if(verbose){
    if(nrow(LMOP_crop)>0){
      LMOP_crop <- LMOP_crop[order(LMOP_crop$`Landfill Name`),]
      
      utils::write.csv(LMOP_crop, file.path(Landfill_output_directory,"MSW_LMOP_all.csv"))
    }
  }
  
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
  
  if(landfill_ghgrp_modeled){
    writeCDF_no_newline(LMOP_flux+ghgrp_modeled,
                        file.path(output_directory,paste0('Landfill_sector_total_GHGRP_modeled.nc')),
                        force_v4=TRUE,
                        varname='methane_emissions',
                        unit='nmol/m2/s',
                        longname='Methane emissions from municipal solid waste landfills',
                        missval=-9999,
                        overwrite=TRUE)
  }
  
  if(landfill_ghgrp_collection_efficiency){
    writeCDF_no_newline(LMOP_flux+ghgrp_collection_efficiency,
                        file.path(output_directory,paste0('Landfill_sector_total_GHGRP_collection_efficiency.nc')),
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
    LMOP_flux[LMOP_flux==0] <- NA
    zlim_min <- terra::global(LMOP_flux,min,na.rm=T)
    zlim_max <- terra::global(LMOP_flux,max,na.rm=T)
    
    if(landfill_ghgrp_reported){
      ghgrp_reported[ghgrp_reported==0] <- NA
      zlim_max <- max(terra::global(ghgrp_reported,max,na.rm=T),zlim_max,na.rm=T)
      zlim_min <- min(terra::global(ghgrp_reported,min,na.rm=T),zlim_min,na.rm=T)
    }
    
    if(landfill_ghgrp_modeled){
      ghgrp_modeled[ghgrp_modeled==0] <- NA
      zlim_max <- max(terra::global(ghgrp_modeled,max,na.rm=T),zlim_max,na.rm=T)
      zlim_min <- min(terra::global(ghgrp_modeled,min,na.rm=T),zlim_min,na.rm=T)
    }
    
    if(landfill_ghgrp_collection_efficiency){
      ghgrp_collection_efficiency[ghgrp_collection_efficiency==0] <- NA
      zlim_max <- max(terra::global(ghgrp_collection_efficiency,max,na.rm=T),zlim_max,na.rm=T)
      zlim_min <- min(terra::global(ghgrp_collection_efficiency,min,na.rm=T),zlim_min,na.rm=T)
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
    if(landfill_ghgrp_modeled){
      log_plot(ghgrp_modeled,filename="MSW_GHGRP_modeled",
               "Municipal Solid Waste -\n GHGRP reporters - decay model based emissions",
               zlim_min=zlim_min,zlim_max=zlim_max,plot_directory=plot_directory,
               domain=domain,County_Tigerlines=County_Tigerlines,
               State_CB=State_CB)
    }
    if(landfill_ghgrp_collection_efficiency){
      log_plot(ghgrp_collection_efficiency,filename="MSW_GHGRP_collection_efficiency",
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
    }else if(landfill_ghgrp_modeled){
      Summed_solid_waste <- sum(c(ghgrp_modeled,LMOP_flux),na.rm=T)
    }else if(landfill_ghgrp_collection_efficiency){
      Summed_solid_waste <- sum(c(ghgrp_collection_efficiency,LMOP_flux),na.rm=T)
    }
    log_plot(Summed_solid_waste,
             "Municipal Solid Waste -\n GHGRP reporters + (GHGI - GHGRP) distributed using \nLandfill Methane Outreach Program",
             plot_directory=plot_directory,
             domain=domain,County_Tigerlines=County_Tigerlines,
             State_CB=State_CB)
  }
  cat("Finished landfill sector: Municipal_solid_waste in",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes\n\n")
}

