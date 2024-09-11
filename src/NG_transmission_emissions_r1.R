#'@title Create gridded natural gas transmission methane emissions maps
#'
#'@description `Transmission` writes 2 netcdf files of gridded methane emissions
#'  from natural gas transmission, as well as optional visuals and 2 optional
#'  csv files.
#'
#'@details This function calculates and grids methane emissions from natural gas
#'  transmission systems. It uses the Homeland Infrastructure Foundation-Level
#'  Data (HIFLD), Environmental Protection Agency's (EPA) Greenhouse Gas
#'  Inventory (GHGI), EPA Greenhouse Gas Reporting Program (GHGRP) and Energy
#'  Information Administration (EIA) Energy Atlas data.  First the relevant data
#'  is pulled from the GHGI Annex file.
#'
#'  For pipelines the GHGI data includes the emissions and activity data for
#'  pipeline leaks, meter and regulating stations, and venting.  An emission
#'  factor in mols of methane per meter of pipeline per second can then be
#'  calculated.  This emission factor is then applied to the EIA
#'  inter/intrastate pipeline data.
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
#'  domain are equal to the national average calculated from the GHGI.  As their
#'  is not a common identifier between the GHGRP and HIFLD datasets, the nearest
#'  facility is considered the matching facility, though an error is flagged if
#'  a GHGRP compressor has no matching HIFLD compressor within 1 km.
#'
#'  The necessary GHGRP compressor location/emissions data, EIA pipeline data
#'  and HIFLD compressor location data will be automatically downloaded.
#'
#'  The GHGRP includes only facilities that emit at least 25,000 metric tons of
#'  carbon dioxide equivalent while the GHGI is intended to capture all national
#'  emissions.  GHGRP data is available starting in 2010 and generally is about
#'  2 years behind present day, the GHGI is available starting in 1990 and is
#'  updated approximately in sync with the GHGRP.  Both datasets are annual. The
#'  GHGRP is at the facility scale while the GHGI is national totals.
#'
#'  The EIA pipeline data being used was last updated January 2020.  The HIFLD
#'  compressor data being used was last updated December 2022.
#'
#'  The GHGI is available at
#'  \url{https://www.epa.gov/ghgemissions/inventory-us-greenhouse-gas-emissions-and-sinks},
#'  the GHGRP is available at \url{https://ghgdata.epa.gov/ghgp/main.do}, the
#'  HIFLD data is available at
#'  \url{https://hifld-geoplatform.hub.arcgis.com/datasets/geoplatform::natural-gas-compressor-stations/about},
#'  and the EIA data is available at
#'  \url{https://atlas.eia.gov/datasets/eia::natural-gas-interstate-and-intrastate-pipelines/about}.
#'@param GHGI_file Character providing the full filepath to the GHGI Annex 3.6
#'  excel file. This data is available at
#'  \url{https://www.epa.gov/ghgemissions/natural-gas-and-petroleum-systems-ghg-inventory-additional-information-1990-2020-ghg}
#'  for the 2022 GHGI.  In the GHGI Annexes, available at
#'  \url{https://www.epa.gov/ghgemissions/inventory-us-greenhouse-gas-emissions-and-sinks-1990-2020},
#'  there is a link to the file in Section 3.6: "Methodology for Estimating CH4,
#'  CO2, and N2O Emissions from Natural Gas Systems".  The excel file has
#'  multiple sheets, each of which has a separate layout.  There is an example
#'  file in the package's datasets folder that has been successfully used in
#'  this code available for reference.
#'@param GHGI_Emissions_sheet Character providing the sheet name in "GHGI_file"
#'  that provides the "CH4 Emissions for Natural Gas Systems, by Segment and
#'  Source, for All Years".  The sheet name as of the 2022 GHGI is "3.6-1".
#'@param GHGI_Activity_sheet Character providing the sheet name in "GHGI_file"
#'  that provides the "Activity Data for Natural Gas Systems Sources, for All
#'  Years".  The sheet name as of the 2022 GHGI is "3.6-7".
#'@param domain SpatRaster providing the desired output grid, including the
#'  desired resolution and coordinate reference system
#'@param state_name_list Character vector listing all states within the desired
#'  domain
#'@param output_directory Character providing the full filepath to save
#'  processed data
#'@param inventory_year Character indicating the desired year of data to use.
#'@param verbose Logical indicating whether to save additional output.  This
#'  includes plots of the gridded methane emissions for each
#'  fuel-sector-inventory-variation combination as well as 2 summed plots for
#'  each inventory-variation combination - one for wood and one for all other
#'  sectors.
#'@param County_Tigerlines SpatVector.  United States Census Bureau county
#'  shapefile downloaded in Main.
#'@param plot_directory Character providing the full filepath to save figures.
#'  Only relevant if verbose = TRUE.
#'@param State_Tigerlines SpatVector.  United States Census Bureau county
#'  shapefile.  Available at
#'  \url{https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html}.
#'  Only relevant if verbose=TRUE.
#'@param focus_city_tigerlines SpatVector.  United States Census Bureau county
#'  shapefile.  Available at
#'  \url{https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html}.
#'  Only relevant if a focus city was set in main and verbose=TRUE.
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
#'@examples
#'library(terra)
#' grid_bbox=cbind(c(-76.65,-73.65),c(38.97,40.97))
#' grid_res=0.01
#' grid_crs="epsg:4326"
#' grid <- rast(nrows=diff(range(grid_bbox[,2]))/grid_res,
#'              ncols=diff(range(grid_bbox[,1]))/grid_res, xmin=min(grid_bbox[,1]),
#'              xmax=max(grid_bbox[,1]), ymin=min(grid_bbox[,2]), ymax=max(grid_bbox[,2]),
#'              crs=grid_crs)
#' Urban_Tigerlines <- vect("~/../Desktop/Urban_Tigerlines/tl_2018_us_uac10.shp")
#' focus_city <- terra::subset(Urban_Tigerlines,Urban_Tigerlines$NAME10 %in% "Philadelphia, PA--NJ--DE--MD")
#' Transmission(GHGI_file="~/../Desktop/2022_ghgi_natural_gas_systems_annex36_tables.xlsx",
#'                       GHGI_Emissions_sheet="3.6-1",
#'                       GHGI_Activity_sheet="3.6-7",
#'                       domain=grid,
#'                       state_name_list=c("DE","MD","NJ","NY","PA"),
#'                       output_directory="~/../Desktop/",
#'                       inventory_year=2018,
#'                       verbose=TRUE,
#'                       State_Tigerlines=vect("~/../Desktop/State_Tigerlines/tl_2018_us_state.shp"),
#'                       County_Tigerlines=vect("~/../Desktop/County_Tigerlines/tl_2018_us_county.shp"),
#'                       focus_city_tigerlines=focus_city,
#'                       plot_directory="~/../Desktop/plots/")
#'@author Joe Pitt, \email{madeup@@wisc.edu}
#'@author Kris Hajny, \email{blank@@fake.edu}
#'@author Israel Lopez-Coto, \email{test@@test.edu}
#'@export

## NG_transmission_emissions_r1.R
## In use: 2021-11-02 20:00
## Finalized: 2023-02-03
#
# Calculate NG transmission emissions for d03 domain

Transmission <- function(GHGI_file,
                         GHGI_Emissions_sheet,
                         GHGI_Activity_sheet,
                         domain,
                         state_name_list,
                         output_directory,
                         inventory_year,
                         verbose,
                         plot_directory,
                         County_Tigerlines,
                         State_Tigerlines,
                         focus_city_tigerlines){
  
  starttime <- Sys.time()
  cat("Starting natural gas transmission sector: Transmission\n")
  ################################################################################
  #checked and all input data matches the old equivalent
  
  #checked pipelines via plotting and distance (though they disagreed).  Some
  #midwestern pipes only exist in the old file, but this is due to an update in
  #the data as the website shows the same
  
  #Checked and the compressors perfectly match the old method (used
  #terra::distance and saw the min was always <1E-4 m - nrow was also the same)
  
  #Checked and GHGRP compressors now perfectly match the GHGRP download within
  #noise (<0.5, a few as much as 0.4 MT CH4 difference).
  
  pipes_EIA=vect("https://services7.arcgis.com/FGr1D95XCGALKXqM/arcgis/rest/services/NaturalGas_InterIntrastate_Pipelines_US_EIA/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")
  compressors_HIFLD=vect("https://services1.arcgis.com/Hp6G80Pky0om7QvQ/arcgis/rest/services/Natural_Gas_Compressor_Stations/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")
  ################################################################################
  #Download the relevant GHGRP emissions data using the API
  #(https://www.GHGI.gov/enviro/envirofacts-data-service-api) and combine the
  #facility and emission data appropriately
  
  #download the relevant naturalgas-sector data
  #(https://www.GHGI.gov/enviro/greenhouse-gas-model).  Must download the relevant
  #data for each possible sector sGHGIrately as emissions are split by sector. The
  #total is combustion + NG systems, dominated by NG systems.  
  ghgrp_transmission_compressor_emissions <- fromJSON("https://data.epa.gov/efservice/ef_w_emissions_source_ghg/json")
  ghgrp_combustion_emissions <- fromJSON("https://data.epa.gov/efservice/C_SUBPART_LEVEL_INFORMATION/json")
  
  #because we're getting sub-facility level information for transmission
  #compressor, first need to aggregate.  Subsetting to only the year of interest
  #now instead of later.
  ghgrp_transmission_compressor_emissions <- ghgrp_transmission_compressor_emissions[ghgrp_transmission_compressor_emissions$reporting_year==inventory_year,]
  processing_CH4 <- aggregate(ghgrp_transmission_compressor_emissions$total_reported_ch4_emissions,
                              by=list(ghgrp_transmission_compressor_emissions$facility_id,
                                      ghgrp_transmission_compressor_emissions$reporting_year,
                                      ghgrp_transmission_compressor_emissions$facility_name,
                                      ghgrp_transmission_compressor_emissions$industry_segment),
                              sum,na.rm=T)
  processing_CH4 <- processing_CH4[,c(1:3,5,4)]
  
  #then split into transmission/compression and gas processing (some are both)
  ghgrp_transmission_compressor_emissions <- processing_CH4[processing_CH4[,5]=="Onshore natural gas transmission compression [98.230(a)(4)]",]
  processing_CH4 <- processing_CH4[processing_CH4[,5]=="Onshore natural gas processing [98.230(a)(3)]",]
  
  #reorganize slightly to match combustion.  Below function won't work right as
  #it's a competely different table
  colnames(ghgrp_transmission_compressor_emissions) <- colnames(ghgrp_combustion_emissions)
  ghgrp_transmission_compressor_emissions$ghg_gas_name <- "methane"
  
  #simple function to make sure gas names are limited to methane, and column names
  #are consistent
  make_consistent <- function(input){
    colnames(input) <- gsub("ghg_gas_name","ghg_name",colnames(input))
    colnames(input) <- gsub("reporting_year","year",colnames(input))
    input$ghg_name <- tolower(input$ghg_name)
    input$facility_name <- tolower(input$facility_name)
    input <- input[input$ghg_name=="methane",]
    return(input)
  }
  
  ghgrp_transmission_compressor_emissions <- make_consistent(ghgrp_transmission_compressor_emissions)
  ghgrp_combustion_emissions <- make_consistent(ghgrp_combustion_emissions)
  
  #rename so the columns are different
  colnames(ghgrp_transmission_compressor_emissions) <- gsub("ghg_quantity","W_emissions",colnames(ghgrp_transmission_compressor_emissions))
  colnames(ghgrp_combustion_emissions) <- gsub("ghg_quantity","C_emissions",colnames(ghgrp_combustion_emissions))
  
  #combine both into 1 dataframe - using NG_system emissions as the base to get
  #ID/year matches from
  ghgrp_transmission_compressor_emissions=Reduce(function(dtf1, dtf2){merge(dtf1, dtf2, by = c("facility_id","year","facility_name","ghg_name"), all.x = TRUE)},
                                                 list(ghgrp_transmission_compressor_emissions,
                                                      ghgrp_combustion_emissions))
  
  #convert the relevant columns to numeric class
  ghgrp_transmission_compressor_emissions[,c("W_emissions","C_emissions")] <- apply(ghgrp_transmission_compressor_emissions[,c("W_emissions","C_emissions")],
                                                                                    2,FUN = function(x){as.numeric(x)})
  ghgrp_transmission_compressor_emissions$ghg_quantity <- rowSums(ghgrp_transmission_compressor_emissions[,c("W_emissions","C_emissions")],na.rm=T)
  
  #for those facilities that are involved in processing, the combustion emissions
  #are not considered part of the transmission/compression total, so remove it
  #here (very small number of facilities)
  processing_facilities <- ghgrp_transmission_compressor_emissions$facility_id %in% processing_CH4[,1]
  ghgrp_transmission_compressor_emissions$ghg_quantity[processing_facilities] <- ghgrp_transmission_compressor_emissions$W_emissions[processing_facilities]
  
  #now filter out those without any emissions
  ghgrp_transmission_compressor_emissions <- ghgrp_transmission_compressor_emissions[ghgrp_transmission_compressor_emissions$ghg_quantity>0,]
  
  rm(processing_facilities,processing_CH4,ghgrp_combustion_emissions,make_consistent)
  ################################################################################
  #Download the relevant facility (e.g., location) data using the API and merge
  
  #see https://www.GHGI.gov/enviro/envirofacts-data-service-api
  data_URLs <- paste0("https://data.epa.gov/efservice/PUB_DIM_FACILITY/STATE/=/",state_name_list,"/JSON")
  
  #initialize output
  ghgrp_facility_info <- data.frame()
  for(A in 1:length(state_name_list)){
    # download data and read/combine in an R dataframe
    ghgrp_facility_info <- rbind(ghgrp_facility_info,fromJSON(data_URLs[A]))
  }
  
  #combine the datasets by ID, and year
  ghgrp <- merge(ghgrp_facility_info,ghgrp_transmission_compressor_emissions,
                 by=c("facility_id","year"), all=F)
  
  #convert the relevant columns to numeric class
  ghgrp[,c("latitude","longitude","ghg_quantity")] <- apply(ghgrp[,c("latitude","longitude","ghg_quantity")],
                                                            2,FUN = function(x){as.numeric(x)})
  
  #delete all tempfiles and clean up working environment
  rm(A,ghgrp_facility_info,ghgrp_transmission_compressor_emissions)
  ################################################################################
  #process the transmission pipeline data
  
  first_row <- which(read_xlsx(GHGI_file,sheet = GHGI_Activity_sheet,.name_repair = "minimal")[,1]=="Segment/Source")
  GHGI_p1 <- read_xlsx(GHGI_file,sheet = GHGI_Activity_sheet,skip=first_row,col_names = T)
  
  first_row <- which(read_xlsx(GHGI_file,sheet = GHGI_Emissions_sheet,.name_repair = "minimal")[,1]=="Segment/Source")
  GHGI_p2 <- read_xlsx(GHGI_file,sheet = GHGI_Emissions_sheet,skip=first_row,col_names = T)
  #p2 = emissions, p1 = activity data.  Columns = year, rows = various types of
  #sources.  First col is just to identify the first column of useable data
  
  Data_list <- c("Pipeline Leaks","M&R (Trans. Co. Interconnect)","M&R (Farm Taps + Direct Sales)",
                 "Pipeline venting")
  #all the sources we're looking for, written exactly as in the GHGI file
  
  GHGI_Pipeline <- data.frame("Type"=Data_list,
                              "Emissions"=as.numeric(unlist(sapply(Data_list,FUN=function(x){GHGI_p2[which(GHGI_p2[,1]==x)[1],as.character(inventory_year)]})))*
                                1E9/(16.043*60*60*24*365),#convert from kt/yr to mol/s
                              "Total_stations"=as.numeric(unlist(sapply(Data_list,FUN=function(x){GHGI_p1[which(GHGI_p1[,1]==x)[1],as.character(inventory_year)]})))*
                                1609.344,#convert from miles to meters
                              row.names = NULL)
  #use sapply to find the row using data list, specify the column as the year and
  #grab the relevant EF and activity data into a dataframe.
  
  pipeline_EF <- sum(GHGI_Pipeline[,2])/GHGI_Pipeline[1,3] #mol/m/s
  #sum of emissions / miles of pipelines (activity data from leaks entry)
  
  
  Data_list <- c("Station Total Emissions","Dehydrator vents (Transmission)",
                 "Flaring (Transmission)","Engines (Transmission)",
                 "Turbines (Transmission)","Engines (Storage)",
                 "Turbines (Storage)","Generators (Engines)",
                 "Generators (Turbines)","Pneumatic Devices Transmission",
                 "Station Venting Transmission")
  #transmission station total + emissions during operations (vents, flaring,
  #leaks, exhaust, etc.)
  
  GHGI_transmission_compressors <- data.frame("Type"=Data_list,
                                              "Emissions"=as.numeric(unlist(sapply(Data_list,FUN=function(x){GHGI_p2[which(GHGI_p2[,1]==x)[1],as.character(inventory_year)]})))*
                                                1E9/(16.043*60*60*24*365),#convert from kt/yr to mol/s
                                              "Total_stations"=as.numeric(unlist(sapply(Data_list,FUN=function(x){GHGI_p1[which(GHGI_p1[,1]==x)[1],as.character(inventory_year)]}))),
                                              row.names = NULL)
  #use sapply to find the row using data list, specify the column as the year and
  #grab the relevant EF and activity data into a dataframe.
  
  Engine_transmission_fraction <- GHGI_transmission_compressors[4,2]/sum(GHGI_transmission_compressors[c(4,6),2])
  Turbine_transmission_fraction <- GHGI_transmission_compressors[5,2]/sum(GHGI_transmission_compressors[c(5,7),2])
  #calculate the ratio between transmission and storage emissions from engines and
  #turbines
  
  GHGI_transmission_compressors[8,2] <- Engine_transmission_fraction*GHGI_transmission_compressors[8,2]
  GHGI_transmission_compressors[9,2] <- Turbine_transmission_fraction*GHGI_transmission_compressors[9,2]
  #apply those ratios to the Generators for engines or turbines since they're not
  #separated into transmission and storage
  
  GHGI_transmission_compressors <- GHGI_transmission_compressors[c(1:5,8:11),]
  #remove the storage data
  compressor_avg_emissions <- sum(GHGI_transmission_compressors[,2])/GHGI_transmission_compressors[3,3] #mol/station/s
  #sum of emissions / N stations (activity data from flaring entry)
  
  rm(GHGI_transmission_compressors,GHGI_Pipeline,GHGI_p1,GHGI_p2,first_row,Data_list,
     Engine_transmission_fraction,Turbine_transmission_fraction)
  cat("Finished loading all input data at",difftime(Sys.time(),starttime,units = "min"),"minutes since start\n")
  ################################################################################
  #process the transmission pipeline data
  
  # Crop to just larger than d03 - don't know if it's necessary to have this buffer but it can't hurt
  e <- ext(domain)*1.1
  pipes_crop_EIA <- crop(project(pipes_EIA,crs(domain)),e)
  
  pipes_by_cell_EIA=rasterizeGeom(pipes_crop_EIA,domain,fun="length")
  pipes_rast_EIA <- pipes_by_cell_EIA*pipeline_EF   # Set values to the pipe length (in metres) in each cell, multiplied by the effective emission factor in mol/m/s
  pipes_flux <- pipes_rast_EIA*1e9/(cellSize(pipes_rast_EIA,unit="m"))  # Calculate flux, mol/s to nmol/m2/s
  pipes_flux[is.na(pipes_flux)]<-0
  
  ################################################################################
  # Now onto the transmission compressor stations
  compressors_crop_HIFLD <- crop(project(compressors_HIFLD,crs(domain)), domain)
  compressors_final <- compressors_crop_HIFLD
  #default for all are the national avg
  compressors_final$emiss <- compressor_avg_emissions

  compressors_ghgrp <- vect(ghgrp,geom=c("longitude","latitude"))
  crs(compressors_ghgrp) <- "epsg:4326"
  compressors_ghgrp_crop <- crop(project(compressors_ghgrp,crs(domain)),domain)
  ################################################################################
  #Now identify the best matching GHGRP facility to the HIFLD ones using location.
  #If > 1 km, flag an error.  Otherwise, overwrite avg national compressor
  #emissions with the GHGRP ones for the specific facility.
  
  location_matches=nearest(compressors_ghgrp_crop,compressors_crop_HIFLD)
  
  combined_data <- cbind(as.data.frame(compressors_ghgrp_crop),
                         as.data.frame(compressors_crop_HIFLD)[location_matches$to_id,],
                         round(location_matches$distance))
  
  combined_data <- combined_data[,c("facility_id","state","facility_name.x",
                                    "ghg_quantity","STATE","NAME",
                                    "round(location_matches$distance)")]
  colnames(combined_data) <- c("GHGRP_ID","GHGRP_state","GHGRP_name",
                               "GHGRP_emissions","HIFLD_state","HIFLD_name",
                               "distance_m")
  
  if(max(combined_data$distance)>1000){
    View(combined_data)
    plot(ext(domain))
    lines(State_Tigerlines)
    points(compressors_crop_HIFLD,cex=2)
    points(compressors_ghgrp_crop,col="red")
    add_legend("bottom",legend = c("HIFLD","GHGRP"),pt.cex = c(2,1),
               horiz=T,col=c("black","red"),pch=16,bty="n")
    stop("some GHGRP compressors didn't have a HIFLD compressor within 1 km")
  }
  
  #scale the GHGRP emissions so that the domain average is equal to the national
  #average
  GHGRP_scaling <- compressor_avg_emissions/mean(compressors_ghgrp_crop$ghg_quantity*1e6/(16.043*365*24*60*60))
  compressors_final$emiss[location_matches$to_id] <- compressors_ghgrp_crop$ghg_quantity*1e6/(16.043*365*24*60*60)*GHGRP_scaling
  
  compressor_rast <- rasterize(compressors_final, domain, "emiss", fun=sum) # in mol/s
  compressor_flux <- compressor_rast*1e9/(cellSize(compressor_rast,unit="m"))  # Calculate flux in nmol/m2/s
  compressor_flux[is.na(compressor_flux)]<-0
  ################################################################################
  # And save the output
  
  if(verbose){
    # Save point sources as csv files - first just the raw dataframe
    write.csv(compressors_final, file.path(output_directory,"NG_trans_compressors_all.csv"))
    
    # Now just the names, coordinates and emissions
    compressors_output <- data.frame(compressors_final$NAME,crds(compressors_final),compressors_final$emiss)
    names(compressors_output) <- c('Site_Name','Longitude','Latitude','Emission_mol_per_s')
    write.csv(compressors_output,file.path(output_directory,"NG_trans_compressors.csv"),row.names=FALSE)
  }
  
  writeCDF(pipes_flux,
           file.path(output_directory,"NG_trans_pipes.nc"),
           force_v4=TRUE,
           varname='methane_emissions',
           unit='nmol/m2/s',
           longname='Methane emissions from natural gas transmission pipelines (inc. leaks, transmission M&R stations, farm taps, direct sales and pipeline venting)',
           missval=-9999,
           overwrite=TRUE)
  
  writeCDF(compressor_flux,
           file.path(output_directory,"NG_trans_compressors.nc"),
           force_v4=TRUE,
           varname='methane_emissions',
           unit='nmol/m2/s',
           longname='Methane emissions from natural gas transmission compressor stations',
           missval=-9999,
           overwrite=TRUE)
  
  ################################################################################
  #Finally, load up some functions/datasets and plot up this output nicely
  
  if(verbose){
    log_plot(compressor_flux,filename="NG_trans_compressors",
             "NG transmission - compressors\n GHGRP reporters + (average GHGI emissions distributed using\n Homeland Infrastructure Foundation-Level Database)")
    
    not_log_plot(pipes_flux,filename="NG_trans_pipes",
                 "NG transmission - pipelines\n EIA pipeline data * GHGI EF")
    
    dir.create("Summed_Sectors",showWarnings = F)
    
    Summed_NG_transmission = compressor_flux+pipes_flux
    log_plot(Summed_NG_transmission,
             "NG Transmission Sector\nEIA for pipelines + HFILD/GHGRP for compressors")
  }
  cat("Finished natural gas transmission sector: Transmission in",difftime(Sys.time(),starttime,units = "min"),"minutes\n")
}
