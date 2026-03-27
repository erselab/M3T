#'@title Create gridded natural gas transmission methane emissions maps
#'
#'@description `Natural_Gas_Transmission` writes 2 netcdf files of gridded methane emissions
#'  from natural gas transmission, as well as optional visuals and 2 optional
#'  csv files.
#'
#'@details This function calculates and grids methane emissions from natural gas
#'  transmission systems. It uses the Homeland Infrastructure Foundation-Level
#'  Data (HIFLD), Environmental Protection Agency's (EPA) Greenhouse Gas
#'  Inventory (GHGI), EPA Greenhouse Gas Reporting Program (GHGRP) and Energy
#'  Information Administration (EIA) Energy Atlas data.  
#'  
#'  The necessary GHGRP, GHGI, and EIA data will be automatically downloaded.
#'
#'  For pipelines the GHGI data includes the emissions and activity data for
#'  pipeline leaks, meter and regulating stations, and venting.  An emission
#'  factor in mols of methane per meter of pipeline per second can then be
#'  calculated.  This emission factor is then applied to the EIA
#'  inter/intrastate pipeline location data.
#'
#'  For compressors the GHGI data includes the emissions and activity data for
#'  compressor station fugitive emissions, dehydrator vents, flaring, engines,
#'  turbines, generators, pneumatic devices, and venting.  For generators they
#'  are split into engine and turbine emissions so the ratio between
#'  transmission/storage emissions for engines and turbines are applied to the
#'  generator values to get the transmission component of generator emissions.
#'  An national average emission rate in mols of methane per station per second
#'  can then be calculated.  This emission rate is then assigned to all HIFLD
#'  compressors.  GHGRP data is then used to overwrite this default emission
#'  rate.  Note most compressor stations do not report their emissions to the
#'  GHGRP, so only a subset will be overwritten using GHGRP data.  The GHGRP
#'  compressor emissions are scaled so that the average emissions within the
#'  domain are equal to the national average calculated from the GHGI.  As there
#'  is not a common identifier between the GHGRP and HIFLD datasets, the nearest
#'  facility is considered the matching facility.
#'
#'  The GHGRP includes only facilities that emit at least 25,000 metric tons of
#'  carbon dioxide equivalent while the GHGI is intended to capture all national
#'  emissions.  GHGRP data is available starting in 2010 and generally is about
#'  2 years behind present day, the GHGI is available starting in 1990 and is
#'  updated approximately in sync with the GHGRP.  Both datasets are annual. The
#'  GHGRP is at the facility scale while the GHGI is national totals.
#'
#'  The HIFLD compressor data being used was last updated December 2022.  It has
#'  since been deprecated, but as there is no replacement available, it is still
#'  being used.
#'
#'  The GHGI is available at
#'  \url{https://www.epa.gov/ghgemissions/inventory-us-greenhouse-gas-emissions-and-sinks}.
#'  The necessary GHGI Annex data is available at
#'  \url{https://www.epa.gov/ghgemissions/natural-gas-and-petroleum-systems-ghg-inventory-additional-information-1990-2022-ghg}
#'  for the 2024 GHGI.  In the GHGI Annexes, available at
#'  \url{https://www.epa.gov/ghgemissions/inventory-us-greenhouse-gas-emissions-and-sinks-1990-2022},
#'  for 2024 there is a link to the file in Section 3.6: "Methodology for
#'  Estimating CH4, CO2, and N2O Emissions from Natural Gas Systems".  The excel
#'  file has multiple sheets, each of which has a separate layout.  The GHGRP is
#'  available at \url{https://ghgdata.epa.gov/ghgp/main.do}, and the EIA data is
#'  available at
#'  \url{https://atlas.eia.gov/datasets/eia::natural-gas-interstate-and-intrastate-pipelines/about}.
#'  
#'@inheritParams Municipal_solid_waste 
#'@param state_name_list Character vector listing all states within the desired
#'  domain
#'@param GHGI_transmission_compressors Character or data.frame.  Pulled from
#'  config file. Either GHGI to indicate the GHGI file should be used to pull
#'  emissions and activity data or a data frame providing the needed values.
#'@param GHGI_Pipeline Character or data.frame.  Pulled from
#'  config file. Either GHGI to indicate the GHGI file should be used to pull
#'  emissions and activity data or a data frame providing the needed values.
#'@param Source_HIFLD_compressor_file Character providing the full filepath to the
#'  HIFLD compressor data.  As this file is now deprecated and no replacement
#'  has been created, it currently must be provided as part of the package.
#'@param GHGRP_facility_data Data.frame with the GHGRP location data for all
#'  years and states.  See
#'  https://www.epa.gov/enviro/envirofacts-data-service-api
#'@param verbose Logical indicating whether to save additional output.  This
#'  includes plots of the gridded methane emissions for each
#'  fuel-sector-inventory-variation combination as well as 2 summed plots for
#'  each inventory-variation combination - one for wood and one for all other
#'  sectors.
#'@param County_Tigerlines SpatVector.  United States Census Bureau county
#'  shapefile downloaded in Main.
#'@param plot_directory Character providing the full filepath to save figures.
#'  Only relevant if verbose = TRUE.
#'@returns Nothing is returned from the function, but the main outputs are 2
#'  netcdf files of the methane emissions from natural gas transmission.  They
#'  are titled "NG_trans_compressors.nc" and "NG_trans_pipes.nc" where
#'  NG=natural gas and trans=transmission.   The first file is for transmission
#'  compressor emissions and the second is for transmission pipeline emissions.
#'
#'  2 csv files are also optionally saved.  These are "NG_trans_compressors.csv"
#'  and "NG_trans_compressors_all.csv".  The simpler csv includes only the name,
#'  location, and assigned emissions for compressors within the domain that were
#'  pulled from the GHGRP  The _all files include all variables that were in the
#'  corresponding input file for the same compressors.
#'@inherit Municipal_solid_waste seealso
#'@keywords internal

Natural_Gas_Transmission <- function(input_directory,
                         GHGI_transmission_compressors,
                         GHGI_Pipeline,
                         Source_HIFLD_compressor_file,
                         Source_EIA_transmission_file,
                         domain,
                         domain_template,
                         GHGRP_facility_data,
                         GHGRP_subpartW_emissions,
                         GHGRP_combustion_emissions,
                         state_name_list,
                         output_directory,
                         inventory_year,
                         GHGI_data_yr,
                         verbose,
                         plot_directory,
                         County_Tigerlines,
                         State_CB){
  
  starttime <- Sys.time()
  cat("Starting natural gas transmission sector: Natural_Gas_Transmission\n")
  
  Transmission_output_directory <- file.path(output_directory,"NG_transmission")
  dir.create(Transmission_output_directory,showWarnings = F)
  ################################################################################
  #Download the needed datasets
  
  #EIA inter and intrastate transmission pipeline map from the EIA atlas
  pipes_EIA_file <- file.path(input_directory,"EIA","EIA_transmission_pipeline_map.geojson")
  
  #if source = "M3T", the file already exists
  if(Source_EIA_transmission_file=="download"){
    #download via API, load directly in then save.  Saving with downloader
    #directly instead caused a memory issue so only a small amount of data was
    #downloaded.
    
    # data_URL <- "https://services7.arcgis.com/FGr1D95XCGALKXqM/arcgis/rest/services/NaturalGas_InterIntrastate_Pipelines_US_EIA/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=geojson"
    data_URL <- "https://services2.arcgis.com/FiaPA4ga0iQKduv3/arcgis/rest/services/Natural_Gas_Interstate_and_Intrastate_Pipelines_1/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"
    pipes_EIA <- Trycatch_downloader(data_URL,method="vect",error_message=paste0("Unable to download EIA pipeline data at: ",data_URL))
    terra::writeVector(pipes_EIA,pipes_EIA_file,overwrite=T)
  }else{
    invisible(file.copy(Source_EIA_transmission_file,pipes_EIA_file,overwrite = T))
  }
  pipes_EIA <- terra::vect(pipes_EIA_file)
  
  
  
  #Deprecated HIFLD transmission compressor locations manually merged with GHGRP
  #compressors from 2010 to 2025 to avoid double counting
  HIFLD_compressor_file <- file.path(input_directory,"HIFLD_Natural_Gas_Compressor_Stations_updated.xlsx")
  
  if(Source_HIFLD_compressor_file=="M3T"){
    #UPDATE TO ZENODO
    compressors_HIFLD <- M3T::HIFLD_NG_data
  }else{
    invisible(file.copy(Source_HIFLD_compressor_file,HIFLD_compressor_file,overwrite = T))
    compressors_HIFLD <- readxl::read_excel(HIFLD_compressor_file)
  }
  compressors_HIFLD <- terra::vect(compressors_HIFLD,geom=c("LONGITUDE", "LATITUDE"),crs="epsg:4326")
  # compressors_HIFLD=vect("https://services1.arcgis.com/Hp6G80Pky0om7QvQ/arcgis/rest/services/Natural_Gas_Compressor_Stations/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")
  
  ################################################################################
  #though it's later cropped to the domain, crop out facilities in Canada
  #immediately
  
  compressors_HIFLD <- terra::mask(compressors_HIFLD,terra::ext(-125,-95,49.0001,60),inverse=T)
  ################################################################################
  #Use yr determined in CH4 inventory build - closest to inventory year with
  #both GHGI and GHGRP
  
  GHGRP_year <- GHGI_data_yr
  
  ################################################################################
  #load in and combine the emission data appropriately
  
  #because we're getting sub-facility level information for transmission
  #compressor, first need to aggregate.  Subsetting to only the year of interest
  #now instead of later.
  GHGRP_subpartW_emissions <- GHGRP_subpartW_emissions[GHGRP_subpartW_emissions$reporting_year==GHGRP_year,]
  processing_CH4 <- stats::aggregate(GHGRP_subpartW_emissions$total_reported_ch4_emissions,
                              by=list(GHGRP_subpartW_emissions$facility_id,
                                      GHGRP_subpartW_emissions$reporting_year,
                                      GHGRP_subpartW_emissions$facility_name,
                                      GHGRP_subpartW_emissions$industry_segment),
                              sum,na.rm=T)
  colnames(processing_CH4) <- c("facility_id","reporting_year","facility_name","industry_segment","ghg_quantity")
  processing_CH4 <- processing_CH4[,c(1:3,5,4)]
  
  #then split into transmission/compression and gas processing (some are both)
  GHGRP_subpartW_emissions <- processing_CH4[processing_CH4$industry_segment=="Onshore natural gas transmission compression [98.230(a)(4)]",]
  processing_CH4 <- processing_CH4[processing_CH4$industry_segment=="Onshore natural gas processing [98.230(a)(3)]",]
  
  #add a column to match combustion and fit the "make_consistent" mold
  GHGRP_subpartW_emissions$ghg_gas_name <- "methane"
  GHGRP_subpartW_emissions <- make_consistent(GHGRP_subpartW_emissions)
  GHGRP_subpartW_emissions <- GHGRP_subpartW_emissions[,colnames(GHGRP_combustion_emissions)]
  
  #rename so the columns are different
  colnames(GHGRP_subpartW_emissions) <- gsub("ghg_quantity","W_emissions",colnames(GHGRP_subpartW_emissions))
  colnames(GHGRP_combustion_emissions) <- gsub("ghg_quantity","C_emissions",colnames(GHGRP_combustion_emissions))
  
  #combine both into 1 dataframe - using NG_system emissions as the base to get
  #ID/year matches from
  GHGRP_subpartW_emissions=merge(GHGRP_subpartW_emissions,GHGRP_combustion_emissions,
                                 by=c("facility_id","year","facility_name","ghg_name"),
                                 all.x=T)
  
  #convert the relevant columns to numeric class
  GHGRP_subpartW_emissions[,c("W_emissions","C_emissions")] <- apply(GHGRP_subpartW_emissions[,c("W_emissions","C_emissions")],
                                                                     2,FUN = function(x){as.numeric(x)})
  GHGRP_subpartW_emissions$ghg_quantity <- rowSums(GHGRP_subpartW_emissions[,c("W_emissions","C_emissions")],na.rm=T)
  
  #for those facilities that are involved in processing, the combustion
  #emissions are not considered part of the transmission/compression total, so
  #remove it here (very small number of facilities AND very small fraction of
  #emissions, at most 1.6% of subpart W in 2011)
  processing_facilities <- GHGRP_subpartW_emissions$facility_id %in% processing_CH4$facility_id
  GHGRP_subpartW_emissions$ghg_quantity[processing_facilities] <- GHGRP_subpartW_emissions$W_emissions[processing_facilities]
  
  #now filter out those without any emissions
  GHGRP_subpartW_emissions <- GHGRP_subpartW_emissions[GHGRP_subpartW_emissions$ghg_quantity>0,]
  ################################################################################
  #Merge with location-like data
  
  #combine the datasets by ID, and year
  ghgrp_compressors <- merge(GHGRP_facility_data,GHGRP_subpartW_emissions,
                             by=c("facility_id","year"), all=F)
  
  #convert the relevant columns to numeric class
  ghgrp_compressors[,c("latitude","longitude","ghg_quantity")] <- apply(ghgrp_compressors[,c("latitude","longitude","ghg_quantity")],
                                                                        2,FUN = function(x){as.numeric(x)})
  ################################################################################
  #process the transmission pipeline data
  
  #calculate the ratio between transmission and storage emissions from engines and
  #turbines
  Engine_transmission_fraction <- GHGI_transmission_compressors[4,2]/sum(GHGI_transmission_compressors[c(4,6),2])
  Turbine_transmission_fraction <- GHGI_transmission_compressors[5,2]/sum(GHGI_transmission_compressors[c(5,7),2])
  
  #apply those ratios to the Generators for engines or turbines since they're not
  #separated into transmission and storage
  GHGI_transmission_compressors[8,2] <- Engine_transmission_fraction*GHGI_transmission_compressors[8,2]
  GHGI_transmission_compressors[9,2] <- Turbine_transmission_fraction*GHGI_transmission_compressors[9,2]
  
  #remove the storage data
  GHGI_transmission_compressors <- GHGI_transmission_compressors[c(1:5,8:11),]
  #sum of emissions / N stations (activity data from flaring entry)
  compressor_avg_emissions <- sum(GHGI_transmission_compressors[,2])/GHGI_transmission_compressors[3,3] #mol/station/s
  
  
  
  #sum of emissions / miles of pipelines (activity data from leaks entry)
  pipeline_EF <- sum(GHGI_Pipeline[,2])/GHGI_Pipeline[1,3] #mol/m/s
  
  # suppressWarnings(rm(GHGI_transmission_compressors,GHGI_Pipeline,GHGI_Activity,GHGI_Emissions,first_row,Data_list,
  #                     Engine_transmission_fraction,Turbine_transmission_fraction))
  cat("Finished loading all input data at",format(Sys.time(),"%H:%M"),"\n")
  ################################################################################
  #process the transmission pipeline data
  
  # Crop to just larger than d03 - don't know if it's necessary to have this
  # buffer but it can't hurt
  e <- terra::ext(domain)*1.1
  pipes_crop_EIA <- terra::crop(terra::project(pipes_EIA,terra::crs(domain)),e)
  
  #Set values to the pipe length (in metres) in each cell
  if(nrow(pipes_crop_EIA)>0){
    pipes_by_cell_EIA=terra::rasterizeGeom(pipes_crop_EIA,domain_template,fun="length",unit="m")
  }else{
    pipes_by_cell_EIA=domain_template
  }
  
  #Now multiply by the effective emission factor in mol/m/s to get to mol/s
  pipes_rast_EIA <- pipes_by_cell_EIA*pipeline_EF
  #Calculate flux, mol/s to nmol/m2/s
  pipes_flux <- pipes_rast_EIA*1e9/(terra::cellSize(pipes_rast_EIA,unit="m"))  
  
  #Set NA values to 0 and mask to the exact domain
  pipes_flux[is.na(pipes_flux)]<-0
  pipes_flux <- terra::mask(pipes_flux,domain)
  ################################################################################
  # Now onto the transmission compressor stations
  
  compressors_crop_HIFLD <- terra::crop(terra::project(compressors_HIFLD,terra::crs(domain)), domain)
  #default for all is the national avg
  compressors_crop_HIFLD$emiss <- compressor_avg_emissions #mol/s
  
  #prepare GHGRP compressor data
  ghgrp_compressors <- terra::vect(ghgrp_compressors,geom=c("longitude","latitude"))
  terra::crs(ghgrp_compressors) <- "epsg:4326"
  compressors_ghgrp_crop <- terra::crop(terra::project(ghgrp_compressors,terra::crs(domain)),domain)
  compressors_ghgrp_crop$ghg_quantity <- compressors_ghgrp_crop$ghg_quantity*1e6/(16.043*365*24*60*60) #MT CH4/yr to mol/s
  
  #Use the GHGRP matching built into the updated HIFLD data to ID data in both
  #datasets or just GHGRP
  indx <- match(compressors_ghgrp_crop$facility_id,compressors_crop_HIFLD$`GHGRP ID`)
  GHGRP_in_HIFLD <- compressors_ghgrp_crop[!is.na(indx),]
  GHGRP_missing_from_HIFLD <- compressors_ghgrp_crop[is.na(indx),]
  indx <- indx[!is.na(indx)]
  
  if(nrow(compressors_ghgrp_crop)>0){
    #replace the HIFLD default with GHGRP values for those with a match, then
    #add in those without a match
    compressors_crop_HIFLD$emiss[indx] <- GHGRP_in_HIFLD$ghg_quantity
    names(GHGRP_missing_from_HIFLD) <- gsub("ghg_quantity","emiss",names(GHGRP_missing_from_HIFLD))
    compressors_crop_HIFLD <- rbind(compressors_crop_HIFLD,GHGRP_missing_from_HIFLD[,"emiss"])
  }
  
  #convert to raster and convert units
  compressor_rast <- terra::rasterize(compressors_crop_HIFLD, domain_template, "emiss", fun=sum) # in mol/s
  compressor_flux <- compressor_rast*1e9/(terra::cellSize(compressor_rast,unit="m"))  # Calculate flux in nmol/m2/s
  compressor_flux[is.na(compressor_flux)]<-0
  compressor_flux <- terra::mask(compressor_flux,domain)
  ################################################################################
  # And save the output
  
  if(verbose){
    if(nrow(compressors_crop_HIFLD)>0){
      # Save point sources as csv files - first just the raw dataframe
      utils::write.csv(compressors_crop_HIFLD, file.path(Transmission_output_directory,"NG_trans_compressors_all.csv"))
      
      # Now just the names, coordinates and emissions
      compressors_output <- data.frame(compressors_crop_HIFLD$NAME,terra::crds(compressors_crop_HIFLD),compressors_crop_HIFLD$emiss)
      names(compressors_output) <- c('Site_Name','Longitude','Latitude','Emission_mol_per_s')
      utils::write.csv(compressors_output,file.path(Transmission_output_directory,"NG_trans_compressors.csv"),row.names=FALSE)
    }
  }
  
  writeCDF_no_newline(pipes_flux,
                      file.path(Transmission_output_directory,"NG_trans_pipes.nc"),
                      force_v4=TRUE,
                      varname='methane_emissions',
                      unit='nmol/m2/s',
                      longname='Methane emissions from natural gas transmission pipelines (inc. leaks, transmission M&R stations, farm taps, direct sales and pipeline venting)',
                      missval=-9999,
                      overwrite=TRUE)
  
  writeCDF_no_newline(compressor_flux,
                      file.path(Transmission_output_directory,"NG_trans_compressors.nc"),
                      force_v4=TRUE,
                      varname='methane_emissions',
                      unit='nmol/m2/s',
                      longname='Methane emissions from natural gas transmission compressor stations',
                      missval=-9999,
                      overwrite=TRUE)
  
  ################################################################################
  #Create a sector total
  
  writeCDF_no_newline(pipes_flux+compressor_flux,
                      file.path(output_directory,paste0('NG_transmission_sector_total.nc')),
                      force_v4=TRUE,
                      varname='methane_emissions',
                      unit='nmol/m2/s',
                      longname='Methane emissions from natural gas transmission',
                      missval=-9999,
                      overwrite=TRUE)
  
  ################################################################################
  #Finally, load up some functions/datasets and plot up this output nicely
  
  if(verbose){
    log_plot(compressor_flux,filename="NG_trans_compressors",
             "NG transmission - compressors\n GHGRP reporters + average GHGI emissions distributed using Homeland\nInfrastructure Foundation-Level Database",
             plot_directory=plot_directory,
             domain=domain,County_Tigerlines=County_Tigerlines,
             State_CB=State_CB)
    
    not_log_plot(pipes_flux,filename="NG_trans_pipes",
                 "NG transmission - pipelines\n EIA pipeline data * GHGI EF",
                 plot_directory=plot_directory,
                 domain=domain,County_Tigerlines=County_Tigerlines,
                 State_CB=State_CB)
    
    dir.create("Summed_Sectors",showWarnings = F)
    
    Summed_NG_transmission = compressor_flux+pipes_flux
    log_plot(Summed_NG_transmission,
             "NG Transmission Sector\nEIA for pipelines + HFILD/GHGRP\nfor compressors",
             plot_directory=plot_directory,
             domain=domain,County_Tigerlines=County_Tigerlines,
             State_CB=State_CB)
  }
  cat("Finished natural gas transmission sector: Natural_Gas_Transmission at",format(Sys.time(),"%H:%M"),"with a total runtime of",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes\n\n")
}
