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
#'@param LMOP_file Character providing the full filepath to the landfill methane
#'  outreach program's landfill-level only data excel file available at
#'  \url{https://www.epa.gov/lmop/landfill-technical-data}.  Only the
#'  LMOP_database tab is used and only the variables GHGRP_ID, latitude, and
#'  longitude are used.  Columns represent separate variables and rows represent
#'  separate landfills.  There is an example file in the package's datasets
#'  folder that has been successfully used in this code available for reference.
#'@param domain SpatVector polygon outlining the desired output area
#'@param domain_template SpatRaster providing the desired output grid, including
#'  the desired resolution and coordinate reference system
#'@param state_name_list Character vector listing all states within the desired
#'  domain
#'@param input_directory Character providing the full filepath to save/load
#'  raw input data
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
#'@param ghgrp_facility_info Data.frame with the GHGRP location data for all
#'  years and states.  See
#'  https://www.epa.gov/enviro/envirofacts-data-service-api
#'@param landfill_ghgrp_reported Logical.  Pulled from config file.  Whether or
#'  not to use reported GHGRP values.
#'@param landfill_ghgrp_modeled Logical.  Pulled from config file.  Whether or
#'  not to overwrite reported GHGRP values with the modeled emissions (HH-6) for
#'  landfills with gas collection systems.
#'@param landfill_ghgrp_collection_efficiency Logical.  Pulled from config file.
#'  Whether or not to overwrite reported GHGRP values with the collection
#'  efficiency based emissions (HH-8) for landfills with gas collection systems.
#'@param plot_directory Character providing the full filepath to save figures.
#'  Only relevant if verbose = TRUE.
#'@param County_Tigerlines SpatVector.  United States Census Bureau county
#'  shapefile.  Available at
#'  \url{https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html}.
#'  Only relevant if verbose=TRUE.
#'@param State_Tigerlines SpatVector.  United States Census Bureau county
#'  shapefile.  Available at
#'  \url{https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html}.
#'  Only relevant if verbose=TRUE.
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
#'ghgrp_facility_info <- read.csv("~/../Desktop/GHGRP/facility_info.csv")
#'ghgrp_facility_info$county_fips <- sprintf("%05d",ghgrp_facility_info$county_fips)
#'ghgrp_facility_info$zip <- sprintf("%05d",ghgrp_facility_info$zip)
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
#'                       ghgrp_facility_info="~/../Desktop/in/GHGRP/facility_info.csv",
#'                       landfill_ghgrp_reported=TRUE,
#'                       landfill_ghgrp_modeled=TRUE,
#'                       landfill_ghgrp_collection_efficiency=TRUE,
#'                       plot_directory="~/../Desktop/plots/"
#'                       County_Tigerlines=vect("~/../Desktop/in/County_Tigerlines/tl_2018_us_county.shp"),
#'                       State_Tigerlines=vect("~/../Desktop/in/State_Tigerlines/tl_2018_us_state.shp"))
#'@author Joe Pitt, \email{madeup@@wisc.edu}
#'@author Kris Hajny, \email{blank@@fake.edu}
#'@author Israel Lopez-Coto, \email{test@@test.edu}
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
                                  ghgrp_facility_info,
                                  landfill_ghgrp_reported,
                                  landfill_ghgrp_modeled,
                                  landfill_ghgrp_collection_efficiency,
                                  plot_directory,
                                  County_Tigerlines,
                                  State_Tigerlines){
  
  starttime <- Sys.time()
  cat("Starting landfill sector: Municipal_solid_waste\n")
  ################################################################################
  #Download the relevant emissions data using the API
  #(https://www.epa.gov/enviro/envirofacts-data-service-api)
  
  #download the relevant landfill-sector data in MT CH4/yr
  #(https://www.epa.gov/enviro/greenhouse-gas-model).  Must download the
  #relevant data for each possible sector separately as emissions are split by
  #sector (i.e., gas capture for electricity is subpart D, stationary combustion
  #is C, and landfill emissions HH - all of which can occur at the same
  #landfill).  Only C and D are included as "reported" municipal emissions
  #exclude subpart C and D. Landfills have 2 options for reporting their
  #emissions - equation HH-6 and HH-8.  HH-6 is based on a first order decay
  #model, HH-8 is based on collection efficiency of a gas collection system.
  #The ghgrp_landfill_detail_emissions include both as either can be the
  #"reported" value.
  
  ghgrp_landfill_file <- file.path(input_directory,"GHGRP","landfill_HH.csv")
  ghgrp_combustion_file <- file.path(input_directory,"GHGRP","combustion_C.csv")
  ghgrp_landfill_system_details_file <- file.path(input_directory,"GHGRP","landfill_HH_details.csv")
  
  
  if(!file.exists(ghgrp_landfill_file)){
    data_URL <- "https://data.epa.gov/dmapservice/ghg.hh_subpart_level_information/csv"
    Trycatch_downloader(URL = data_URL,method = "save",output_location = ghgrp_landfill_file,
                        error_message = paste0("Greenhouse Gas Reporting Program data could not be downloaded using API link: ",data_URL))
  }
  
  if(!file.exists(ghgrp_combustion_file)){
    data_URL <- "https://data.epa.gov/dmapservice/ghg.c_subpart_level_information/csv"
    Trycatch_downloader(URL = data_URL,method = "save",output_location = ghgrp_combustion_file,
                        error_message = paste0("Greenhouse Gas Reporting Program data could not be downloaded using API link: ",data_URL))
  }
  
  if(!file.exists(ghgrp_landfill_system_details_file)){
    data_URL <- "https://data.epa.gov/dmapservice/ghg.hh_gas_collection_system_detls/csv"
    Trycatch_downloader(URL = data_URL,method = "save",output_location = ghgrp_landfill_system_details_file,
                        error_message = paste0("Greenhouse Gas Reporting Program data could not be downloaded using API link: ",data_URL))
  }
  ################################################################################
  #load in and combine the emission data appropriately
  
  #load in the files
  ghgrp_landfill_only_emissions <- read.csv(ghgrp_landfill_file)
  ghgrp_combustion_emissions <- read.csv(ghgrp_combustion_file)
  ghgrp_landfill_detail_emissions <- read.csv(ghgrp_landfill_system_details_file)
  
  #simple function to make sure gas names are limited to methane, and column
  #names are consistent
  make_consistent <- function(input){
    colnames(input) <- gsub("ghg_gas_name","ghg_name",colnames(input))
    colnames(input) <- gsub("reporting_year","year",colnames(input))
    input$ghg_name <- tolower(input$ghg_name)
    input$facility_name <- tolower(input$facility_name)
    input <- input[input$ghg_name=="methane",]
    return(input)
  }
  
  ghgrp_landfill_only_emissions <- make_consistent(ghgrp_landfill_only_emissions)
  ghgrp_combustion_emissions <- make_consistent(ghgrp_combustion_emissions)

  #rename so the columns are different
  colnames(ghgrp_landfill_only_emissions) <- gsub("ghg_quantity","HH_emissions",colnames(ghgrp_landfill_only_emissions))
  colnames(ghgrp_combustion_emissions) <- gsub("ghg_quantity","C_emissions",colnames(ghgrp_combustion_emissions))

  #combine them into 1 dataframe - using landfill emissions as the base to get
  #ID/year matches from
  ghgrp_landfill_emissions=Reduce(function(dtf1, dtf2){merge(dtf1, dtf2, by = c("facility_id","year","facility_name","ghg_name"), all.x = TRUE)},
                                  list(ghgrp_landfill_only_emissions,
                                       ghgrp_combustion_emissions))
  
  #Now add the HH-6 and HH-8 emission rates to the dataframe too
  ghgrp_landfill_emissions <- merge(ghgrp_landfill_emissions,
                                    ghgrp_landfill_detail_emissions[,c("facility_id","reporting_year","equation_hh6_result","equation_hh8_result")],
                                    by.x=c("facility_id","year"),by.y=c("facility_id","reporting_year"),all.x=T)
  colnames(ghgrp_landfill_emissions) <- gsub("equation_hh6_result","HH_modeled",colnames(ghgrp_landfill_emissions))
  colnames(ghgrp_landfill_emissions) <- gsub("equation_hh8_result","HH_collection_efficiency",colnames(ghgrp_landfill_emissions))
  
  # test <- vector(length=nrow(ghgrp_landfill_emissions))
  # for(A in 1:length(test)){
  #   temp <- c(ghgrp_landfill_emissions$HH_modeled[A] - ghgrp_landfill_emissions$HH_emissions[A],
  #             ghgrp_landfill_emissions$HH_collection_efficiency[A] - ghgrp_landfill_emissions$HH_emissions[A])
  #   temp[is.na(temp)] <- 5E50
  #   test[A] <- temp[which.min(abs(temp))]
  # }
  # test[test==5E50] <- NA
  # ghgrp_landfill_emissions <- ghgrp_landfill_emissions[order(abs(test),decreasing = T),]
  # test <- test[order(abs(test),decreasing=T)]
  # GHGRP_errors <- ghgrp_landfill_emissions[which(!is.na(test) & test!=0),]
  # GHGRP_errors$discrepancy <- abs(test[which(!is.na(test) & test!=0)])
  # GHGRP_errors$URL <- paste0("https://ghgdata.epa.gov/ghgp/service/facilityDetail/",GHGRP_errors$year,"?id=",GHGRP_errors$facility_id,"&ds=E&et=&popup=true")
  # GHGRP_errors$ghg_name <- NULL
  # GHGRP_errors$C_emissions <- NULL
  # colnames(GHGRP_errors) <- c("facility_id","year","facility_name","Reported_landfill_CH4","HH-6_CH4","HH-8_CH4","discrepancy","URL")
  # GHGRP_errors[,c("Reported_landfill_CH4","HH-6_CH4","HH-8_CH4","discrepancy")] <- GHGRP_errors[,c("Reported_landfill_CH4","HH-6_CH4","HH-8_CH4","discrepancy")]*25
  # big_GHGRP_errors <- GHGRP_errors[GHGRP_errors$discrepancy>25,]
  # GHGRP_errors <- GHGRP_errors[GHGRP_errors$discrepancy<=25,]
  # write.csv(big_GHGRP_errors,file = "GHGRP_landfill_errors.csv",row.names = F)
  # write.csv(GHGRP_errors,file = "GHGRP_landfill_minor_errors.csv",row.names = F)
  

  #convert the relevant columns to numeric class
  ghgrp_landfill_emissions[,c("HH_emissions","C_emissions","HH_modeled","HH_collection_efficiency")] <- apply(ghgrp_landfill_emissions[,c("HH_emissions","C_emissions","HH_modeled","HH_collection_efficiency")],
                                                                                                              2,FUN = function(x){as.numeric(x)})
  
  #calculate the sum of combustion (C) and landfill (HH) CH4 emissions.  
  ghgrp_landfill_emissions$ghg_quantity <- rowSums(ghgrp_landfill_emissions[,c("HH_emissions","C_emissions")],na.rm=T)
  
  #Calculate national total in the GHGRP for the year of interest
  #MT CH4/yr to Gg CH4/yr
  ghgrp_national <- sum(as.numeric(ghgrp_landfill_emissions$ghg_quantity[ghgrp_landfill_emissions$year==inventory_year]))/1000   
  
  rm(ghgrp_landfill_only_emissions,ghgrp_combustion_emissions,make_consistent)
  ################################################################################  
  # Now calculate national totals
  
  EPA_total <- GHGI_landfill_total 
  non_ghgrp_total <- EPA_total - ghgrp_national
  
  ################################################################################
  #Merge with location-like data and account for facilities that stopped
  #reporting without a valid reason
  
  #combine the datasets by ID, and year
  ghgrp_all_data <- merge(ghgrp_facility_info,ghgrp_landfill_emissions,
                          by=c("facility_id","year"), all=F)
  
  #keep only data for the year of interest
  ghgrp <- ghgrp_all_data[ghgrp_all_data$year==inventory_year,]
  
  #identify facilities that stopped reporting without a valid reason, then
  #subset to only landfill facilities and only those that we don't have data for
  #(e.g., it stopped reporting back in 2015, but reported again post 2018)
  nonreporting_facilities <- unique(ghgrp_facility_info$facility_id[ghgrp_facility_info$reporting_status=="STOPPED_REPORTING_UNKNOWN_REASON" & ghgrp_facility_info$year<=inventory_year])
  nonreporting_landfills <- nonreporting_facilities[which(nonreporting_facilities %in% unique(ghgrp_landfill_emissions$facility_id))]
  nonreporting_landfills <- nonreporting_landfills[!(nonreporting_landfills %in% unique(ghgrp$facility_id))]
  
  Finalize_ghgrp <- function(outname,longname){
    # find the closest data available for those that stopped reporting (this can
    # be after the inventory year in some cases)
    nonreporting_landfill_data <- ghgrp_all_data[ghgrp_all_data$facility_id %in% nonreporting_landfills,]
    nonreporting_landfill_data=tapply(nonreporting_landfill_data,
                                      INDEX=nonreporting_landfill_data$facility_id,
                                      FUN=function(x){x[which.min(abs(x$year-inventory_year)),]})
    if(length(nonreporting_landfill_data)>0){
      nonreporting_landfill_data=do.call(rbind, nonreporting_landfill_data)
    }
    
    #add this most recent data to the GHGRP dataset
    ghgrp <- rbind(nonreporting_landfill_data,ghgrp)
    
    #convert the relevant columns to numeric class
    ghgrp[,c("latitude","longitude","ghg_quantity")] <- apply(ghgrp[,c("latitude","longitude","ghg_quantity")],
                                                              2,FUN = function(x){as.numeric(x)})
    ##############################################################################
    #Now convert to spatial.
    
    #convert to a spatial object, crop to domain, convert units
    ghgrp <- vect(ghgrp,geom=c("longitude","latitude"))
    crs(ghgrp) <- "epsg:4326"
    ghgrp <- project(ghgrp,crs(domain))
    ghgrp_crop <- crop(ghgrp, domain)
    #MT CH4/yr to mol/s of CH4
    ghgrp_crop$emiss <- ghgrp_crop$ghg_quantity*1e6/(16.043*365*24*60*60)
    
    #save to the environment for use later
    assign("nonreporting_landfill_data",nonreporting_landfill_data,envir = parent.env(environment()))
    ################################################################################
    # Now rasterise and save
    
    ghgrp_rast <- rasterize(ghgrp_crop, domain_template, "emiss", fun=sum)
    # Calculate flux, mol/s to nmol/m2/s
    ghgrp_flux <- ghgrp_rast*1e9/(cellSize(ghgrp_rast,unit="m"))  
    ghgrp_flux[is.na(ghgrp_flux)]<-0
    ghgrp_flux <- mask(ghgrp_flux,domain)
    
    if(verbose & outname=="MSW_GHGRP_reported.nc"){
      if(nrow(ghgrp_crop)>0){
        #sort both by name
        ghgrp_crop <- ghgrp_crop[order(ghgrp_crop$facility_name.x),]
        
        # Save point sources as csv files - first just the raw dataframe
        write.csv(ghgrp_crop, file.path(output_directory,'MSW_GHGRP_all.csv'))
      }
    }
    
    # Now write the raster as a netcdf files
    writeCDF(ghgrp_flux,
             file.path(output_directory,outname),
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
  #Download, load in, and prepare LMOP data
  
  LMOP_file <- file.path(input_directory,"LMOP_landfill_only.xlsx")
  
  if(!file.exists(LMOP_file)){
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
    Trycatch_downloader(URL = data_URL2,method = "save",output_location = LMOP_file,
                        error_message = paste0("LMOP data could not be downloaded from webpage:\n",data_URL2,"\nMake sure the main EPA page for it is still accurate:\n",data_URL))
    unlink(download_dest)
  }
  
  # Read in LMOP and remove those in GHGRP.  Note facilities that used to report
  # to GHGRP and stopped with a valid reason are being considered LMOP
  # facilities in this approach.  
  LMOP <- read_xlsx(LMOP_file,sheet="LMOP Database",col_names = T)
  LMOP_non_ghgrp <- LMOP[!(LMOP$`GHGRP ID` %in% ghgrp$facility_id),]
  
  #This has some nans in, remove those and make spatial
  LMOP_filt <- subset(LMOP_non_ghgrp,!is.na(Latitude))
  LMOP_filt <- vect(LMOP_filt,geom=c("Longitude","Latitude"))
  crs(LMOP_filt) <- "epsg:4326"
  LMOP_filt <- project(LMOP_filt,crs(domain))
  LMOP_crop <- crop(LMOP_filt, domain)
  
  #Exclude those we already handled as they stopped reporting without a valid
  #reason.
  LMOP_crop <- LMOP_crop[!(LMOP_crop$`GHGRP ID` %in% nonreporting_landfill_data$facility_id),]
  
  # Find avg emission per non-GHGRP LMOP landfill (including the ones with no coordinates)
  avg_non_ghgrp <- non_ghgrp_total/nrow(LMOP_non_ghgrp)
  # For comparison, calculate avg ghgrp
  avg_ghgrp <- ghgrp_national/nrow(ghgrp)
  # Assign the avg emissions to LMOP landfills
  LMOP_crop$emiss <- avg_non_ghgrp*1e9/(16.043*365*24*60*60)   
  #Gg CH4/yr to mol/s of CH4
  ################################################################################
  # Now rasterise and save

  LMOP_rast <- rasterize(LMOP_crop, domain_template, field="emiss", fun=sum)
  # Calculate flux, mol/s to nmol/m2/s
  LMOP_flux <- LMOP_rast*1e9/(cellSize(LMOP_rast,unit="m"))
  LMOP_flux[is.na(LMOP_flux)]<-0
  LMOP_flux <- mask(LMOP_flux,domain)
  
  if(verbose){
    if(nrow(LMOP_crop)>0){
      LMOP_crop <- LMOP_crop[order(LMOP_crop$`Landfill Name`),]
      
      write.csv(LMOP_crop, file.path(output_directory,"MSW_LMOP_all.csv"))
    }
  }
  
  writeCDF(LMOP_flux,
           file.path(output_directory,'MSW_LMOP.nc'),
           force_v4=TRUE,
           varname='methane_emissions',
           unit='nmol/m2/s',
           longname='Methane emissions from municipal solid waste landfills that report to LMOP but not GHGRP',
           missval=-9999,
           overwrite=TRUE)
  
  
  ################################################################################
  #Finally, load up some functions and plot up this output nicely
  
  if(verbose){
    LMOP_flux[LMOP_flux==0] <- NA
    
    if(landfill_ghgrp_reported){
      ghgrp_reported[ghgrp_reported==0] <- NA
    }
    
    if(landfill_ghgrp_modeled){
      ghgrp_modeled[ghgrp_modeled==0] <- NA
    }
    
    if(landfill_ghgrp_collection_efficiency){
      ghgrp_collection_efficiency[ghgrp_collection_efficiency==0] <- NA
    }
    
    
    zlim_min <- log10(min(global(ghgrp_reported,min,na.rm=T),
                          global(ghgrp_modeled,min,na.rm=T),
                          global(ghgrp_collection_efficiency,min,na.rm=T),
                          global(LMOP_flux,min,na.rm=T)))
    zlim_max <- log10(max(global(ghgrp_reported,max,na.rm=T),
                          global(ghgrp_modeled,max,na.rm=T),
                          global(ghgrp_collection_efficiency,max,na.rm=T),
                          global(LMOP_flux,max,na.rm=T)))
    
    log_plot(ghgrp_reported,filename="MSW_GHGRP_reported",
             "Municipal Solid Waste -\n GHGRP reporters",
             zlim_min=zlim_min,zlim_max=zlim_max,plot_directory=plot_directory,
             domain=domain,County_Tigerlines=County_Tigerlines,
             State_Tigerlines=State_Tigerlines)
    log_plot(ghgrp_modeled,filename="MSW_GHGRP_modeled",
             "Municipal Solid Waste -\n GHGRP reporters - decay model based emissions",
             zlim_min=zlim_min,zlim_max=zlim_max,plot_directory=plot_directory,
             domain=domain,County_Tigerlines=County_Tigerlines,
             State_Tigerlines=State_Tigerlines)
    log_plot(ghgrp_collection_efficiency,filename="MSW_GHGRP_collection_efficiency",
             "Municipal Solid Waste -\n GHGRP reporters - collection efficiency based\nemissions",
             zlim_min=zlim_min,zlim_max=zlim_max,plot_directory=plot_directory,
             domain=domain,County_Tigerlines=County_Tigerlines,
             State_Tigerlines=State_Tigerlines)
    log_plot(LMOP_flux,filename="MSW_LMOP",
             "Municipal Solid Waste -\n (GHGI - GHGRP) distributed using \nLandfill Methane Outreach Program",
             zlim_min=zlim_min,zlim_max=zlim_max,plot_directory=plot_directory,
             domain=domain,County_Tigerlines=County_Tigerlines,
             State_Tigerlines=State_Tigerlines)
    
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
             State_Tigerlines=State_Tigerlines)
  }
  cat("Finished landfill sector: Municipal_solid_waste in",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes\n\n")
}

