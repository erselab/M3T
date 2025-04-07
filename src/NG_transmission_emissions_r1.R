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
#'@param domain SpatVector polygon outlining the desired output area
#'@param domain_template SpatRaster providing the desired output grid, including
#'  the desired resolution and coordinate reference system
#'@param state_name_list Character vector listing all states within the desired
#'  domain
#'@param GHGI_transmission_compressors Character or data.frame.  Pulled from
#'  config file. Either GHGI to indicate the GHGI file should be used to pull
#'  emissions and activity data or a data frame providing the needed values.
#'@param GHGI_Pipeline Character or data.frame.  Pulled from
#'  config file. Either GHGI to indicate the GHGI file should be used to pull
#'  emissions and activity data or a data frame providing the needed values.
#'@param HIFLD_compressor_file Character providing the full filepath to the
#'  HIFLD compressor data.  As this file is now deprecated and no replacement
#'  has been created, it currently must be provided as part of the package.
#'@param ghgrp_facility_info Data.frame with the GHGRP location data for all
#'  years and states.  See
#'  https://www.epa.gov/enviro/envirofacts-data-service-api
#'@param input_directory Character providing the full filepath to save/load
#'  raw input data
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
#' grid_vect <- as.polygons(ext(grid),crs=grid_crs)
#' Transmission(GHGI_transmission_compressors="GHGI",
#'                       GHGI_Pipeline="GHGI",
#'                       HIFLD_compressor_file="~/../Desktop/in/Natural_Gas_Compressor_Stations.csv",
#'                       ghgrp_facility_info="~/../Desktop/in/GHGRP/facility_info.csv",
#'                       domain=grid_vect,
#'                       domain_template=grid,
#'                       state_name_list=c("DE","MD","NJ","NY","PA"),
#'                       output_directory="~/../Desktop/out/",
#'                       input_directory="~/../Desktop/in/",
#'                       inventory_year=2018,
#'                       verbose=TRUE,
#'                       State_Tigerlines=vect("~/../Desktop/in/State_Tigerlines/tl_2018_us_state.shp"),
#'                       County_Tigerlines=vect("~/../Desktop/in/County_Tigerlines/tl_2018_us_county.shp"),
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

Transmission <- function(input_directory,
                         GHGI_transmission_compressors,
                         GHGI_Pipeline,
                         HIFLD_compressor_file,
                         domain,
                         domain_template,
                         ghgrp_facility_info,
                         state_name_list,
                         output_directory,
                         inventory_year,
                         verbose,
                         plot_directory,
                         County_Tigerlines,
                         State_Tigerlines){
  
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
  
  data_URL <- "https://services7.arcgis.com/FGr1D95XCGALKXqM/arcgis/rest/services/NaturalGas_InterIntrastate_Pipelines_US_EIA/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=geojson"
  pipes_EIA <- Trycatch_downloader(data_URL,method="vect",error_message=paste0("Unable to download EIA pipeline data at: ",data_URL))

  #need to use the downloaded file for the moment - as of 9/30/24 the API and
  #website with the HIFLD compressors has been removed
  compressors_HIFLD <- read.csv(HIFLD_compressor_file)
  compressors_HIFLD <- vect(compressors_HIFLD,geom=c("LONGITUDE", "LATITUDE"),crs="epsg:4326")
  # compressors_HIFLD=vect("https://services1.arcgis.com/Hp6G80Pky0om7QvQ/arcgis/rest/services/Natural_Gas_Compressor_Stations/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")

  ################################################################################
  #though it's later cropped to the domain, crop out facilities in Canada
  #immediately

  compressors_HIFLD <- mask(compressors_HIFLD,ext(-125,-95,49.0001,60),inverse=T)
  ################################################################################
  #troubleshooting
  
  # data_URL <- "https://services7.arcgis.com/FGr1D95XCGALKXqM/arcgis/rest/services/NaturalGas_InterIntrastate_Pipelines_US_EIA/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=geojson"
  # pipes_EIA <- Trycatch_downloader(data_URL,method="vect",error_message=paste0("Unable to download EIA pipeline data at: ",data_URL))
  # # writeVector(pipes_EIA,pipes_EIA_file)
  # # 
  # # pipes_EIA_file <- file.path(input_directory,"EIA","interintrastate_pipelines.geojson")
  # # if(!file.exists(pipes_EIA_file)){
  # #   data_URL <- "https://services7.arcgis.com/FGr1D95XCGALKXqM/arcgis/rest/services/NaturalGas_InterIntrastate_Pipelines_US_EIA/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"
  # #   Trycatch_downloader(URL = data_URL,method = "save",output_location = pipes_EIA_file,
  # #                       error_message = paste0("Unable to download EIA pipeline data at: ",data_URL))
  # # }
  # pipes_EIA_gson <- vect("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw_data_12_2024/EIA/NaturalGas_InterIntrastate_Pipelines_US_EIA_365726140959634023.geojson")
  # # pipes_EIA_shape <- vect("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw_data_12_2024/EIA/NaturalGas_InterIntrastate_Pipelines_US_EIA_-4478325912711341125/NaturalGas_InterIntrastate_Pipelines_US_EIA.shp")
  # # pipes_EIA_gpkg <- vect("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw_data_12_2024/EIA/NaturalGas_InterIntrastate_Pipelines_US_EIA_-5692899880001079636.gpkg")
  # # pipes_EIA_shape <- project(pipes_EIA_shape,pipes_EIA_gson)
  # # pipes_EIA_gpkg <- project(pipes_EIA_gpkg,pipes_EIA_gson)
  # 
  # # View(as.data.frame(pipes_EIA[which(!(pipes_EIA$FID%in%pipes_EIA_gson$FID)),]))
  # 
  # # pipes_EIA <- pipes_EIA[pipes_EIA$TYPEPIPE!="Gathering",]
  # #not a solution, but part of a problem - doesn't match online map - the old version.  New one is not automated.
  # 
  # 
  # # plot(pipes_EIA[which(!(pipes_EIA$FID%in%pipes_EIA_gson$FID)),])
  # # plot(pipes_EIA[pipes_EIA$TYPEPIPE=="Gathering",])
  # 
  # temp <- pipes_EIA[pipes_EIA$TYPEPIPE!="Gathering",]
  # 
  # test=compareGeom(temp,pipes_EIA_gson)
  # test2=diag(test)
  # 
  # png("Transmission_test1.png",width=480*2)
  # plot(State_Tigerlines[!(State_Tigerlines$STUSPS %in% c("AK","GU","AS","VI","MP","HI","PR")),],border="lightgrey",main="red over black")
  # lines(temp[which(test2==FALSE),])
  # lines(pipes_EIA_gson[which(test2==FALSE),],col="red")
  # add_legend("bottomleft",col=c("red","black"),
  #            legend = c("Web download","R download"),lty=1)
  # dev.off()
  # 
  # png("Transmission_test2.png",width=480*2)
  # plot(State_Tigerlines[!(State_Tigerlines$STUSPS %in% c("AK","GU","AS","VI","MP","HI","PR")),],border="lightgrey",main="black over red")
  # lines(pipes_EIA_gson[which(test2==FALSE),],col="red")
  # lines(temp[which(test2==FALSE),])
  # add_legend("bottomleft",col=c("red","black"),
  #            legend = c("Web download","R download"),lty=1)
  # dev.off()
  # 
  # png("Transmission_test3.png",width=480*2)
  # plot(State_Tigerlines[!(State_Tigerlines$STUSPS %in% c("AK","GU","AS","VI","MP","HI","PR")),],border="lightgrey",main="ID 31664 - 31668")
  # lines(shift(pipes_EIA_gson[pipes_EIA_gson$FID%in%c(31664:31668),],dy=0.5),col="red")
  # text(shift(pipes_EIA_gson[pipes_EIA_gson$FID%in%c(31664:31668),],dy=0.5),"FID",col="red")
  # lines(temp[temp$FID%in%c(31664:31668),])
  # text(temp[temp$FID%in%c(31664:31668),],"FID")
  # add_legend("bottomleft",col=c("red","black"),
  #            legend = c("Web download","R download"),lty=1)
  # dev.off()
  # 
  # png("Tranmsmission_test4.png",width=480*2)
  # plot(State_Tigerlines[!(State_Tigerlines$STUSPS %in% c("AK","GU","AS","VI","MP","HI","PR")),],border="lightgrey",main="Gathering Pipelines")
  # lines(pipes_EIA[pipes_EIA$TYPEPIPE=="Gathering",])
  # dev.off()
  # 
  # View(cbind(as.data.frame(temp[test2==FALSE,]),as.data.frame(pipes_EIA_gson[test2==F,])))
  ################################################################################
  #Download the relevant GHGRP emissions data using the API
  #(https://www.GHGI.gov/enviro/envirofacts-data-service-api) and combine the
  #facility and emission data appropriately
  
  #download the relevant naturalgas-sector data
  #(https://www.GHGI.gov/enviro/greenhouse-gas-model).  Must download the relevant
  #data for each possible sector separately as emissions are split by sector. The
  #total is combustion + NG systems, dominated by NG systems.  

  ghgrp_compressor_file <- file.path(input_directory,"GHGRP","Oil_and_gas_W.csv")
  ghgrp_combustion_file <- file.path(input_directory,"GHGRP","combustion_C.csv")

  if(!file.exists(ghgrp_compressor_file)){
    data_URL <- "https://data.epa.gov/dmapservice/ghg.ef_w_emissions_source_ghg/csv"
    Trycatch_downloader(URL = data_URL,method = "save",output_location = ghgrp_compressor_file,
                        error_message = paste0("Greenhouse Gas Reporting Program data could not be downloaded using API link: ",data_URL))
  }
  if(!file.exists(ghgrp_combustion_file)){
    data_URL <- "https://data.epa.gov/dmapservice/ghg.c_subpart_level_information/csv"
    Trycatch_downloader(URL = data_URL,method = "save",output_location = ghgrp_combustion_file,
                        error_message = paste0("Greenhouse Gas Reporting Program data could not be downloaded using API link: ",data_URL))
  }
  
  ################################################################################
  #load in and combine the emission data appropriately
  
  ghgrp_transmission_compressor_emissions <- read.csv(ghgrp_compressor_file)
  ghgrp_combustion_emissions <- read.csv(ghgrp_combustion_file)
  
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
  colnames(processing_CH4) <- c("facility_id","reporting_year","facility_name","industry_segment","ghg_quantity")
  processing_CH4 <- processing_CH4[,c(1:3,5,4)]
  
  #then split into transmission/compression and gas processing (some are both)
  ghgrp_transmission_compressor_emissions <- processing_CH4[processing_CH4$industry_segment=="Onshore natural gas transmission compression [98.230(a)(4)]",]
  processing_CH4 <- processing_CH4[processing_CH4$industry_segment=="Onshore natural gas processing [98.230(a)(3)]",]
  
  #reorganize slightly to match combustion.  Below function won't work right as
  #it's a completely different table
  ghgrp_transmission_compressor_emissions$ghg_gas_name <- "methane"
  ghgrp_transmission_compressor_emissions <- ghgrp_transmission_compressor_emissions[,colnames(ghgrp_combustion_emissions)]

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
  processing_facilities <- ghgrp_transmission_compressor_emissions$facility_id %in% processing_CH4$facility_id
  ghgrp_transmission_compressor_emissions$ghg_quantity[processing_facilities] <- ghgrp_transmission_compressor_emissions$W_emissions[processing_facilities]
  
  #now filter out those without any emissions
  ghgrp_transmission_compressor_emissions <- ghgrp_transmission_compressor_emissions[ghgrp_transmission_compressor_emissions$ghg_quantity>0,]
  
  rm(processing_facilities,processing_CH4,ghgrp_combustion_emissions,make_consistent)
  ################################################################################
  #Merge with location-like data
  
  #combine the datasets by ID, and year
  ghgrp <- merge(ghgrp_facility_info,ghgrp_transmission_compressor_emissions,
                 by=c("facility_id","year"), all=F)
  
  #convert the relevant columns to numeric class
  ghgrp[,c("latitude","longitude","ghg_quantity")] <- apply(ghgrp[,c("latitude","longitude","ghg_quantity")],
                                                            2,FUN = function(x){as.numeric(x)})
  ################################################################################
  #Download the GHGI annex file if not already downloaded
  
  GHGI_file <- list.files(input_directory,pattern="*GHGI_natural_gas_annex_tables.xlsx",full.names = T)
  
  if(identical(GHGI_file,character(0))){
    #download the webpage and load in the HTML.  The webpage is year specific, so
    #check ~2 years ago (the most recent based on current reporting times)
    #and go farther back as needed
    GHGI_year <- as.numeric(substring(Sys.Date(),1,4))-2
    repeat{
      data_URL <- paste0("https://www.epa.gov/ghgemissions/natural-gas-and-petroleum-systems-ghg-inventory-additional-information-1990-",GHGI_year,"-ghg")
      download_dest <- tempfile(fileext = ".html")
      Trycatch_downloader(URL = data_URL,method = "save",output_location = download_dest,
                          error_message = paste0("LMOP data could not be webscraped from webpage: ",data_URL))
      HTML_data <- readChar(download_dest,file.info(download_dest)$size)
      if(grepl("<title>Page Not Found",HTML_data)){
        GHGI_year <- GHGI_year-1
        Sys.sleep(1)
        next
      }else{
        break
      }
    }
    
    #Search for https:// - any 100 or fewer characters - landfilllmopdata.xlsx in
    #the HTML_data.  The link had about 50 characters between https:// and
    #landfilllmopdata at most across the past few versions, but this should
    #identify any version if the format is reasonably consistent.  
    Matchtext <- regexpr("https://.{1,100}ghgi_natural_gas_systems_annex36_tables.xlsx",HTML_data)
    data_URL2 <- substring(HTML_data,Matchtext[1],Matchtext[1]+attr( Matchtext , "match.length")-1)
    GHGI_file <- paste0(input_directory,"/",GHGI_year,"_GHGI_natural_gas_annex_tables.xlsx")
    Trycatch_downloader(URL = data_URL2,method = "save",output_location = GHGI_file,
                        error_message = paste0("GHGI annex data could not be downloaded from webpage:\n",data_URL2,"\nMake sure the main EPA page for it is accurate:\n",data_URL))
    unlink(download_dest)
  }
  
  ################################################################################
  #process the transmission pipeline data
  
  #use grep and the index page of the annex file to identify the pages we want
  GHGI_index <- read_xlsx(GHGI_file,.name_repair = "minimal")
  GHGI_Activity_sheet <- gsub("Table ","",
                              GHGI_index[sapply(GHGI_index[,2],FUN=function(x){grep(pattern="Activity Data for Natural Gas Systems Sources",x)}),1])
  GHGI_Emissions_sheet <- gsub("Table ","",
                              GHGI_index[sapply(GHGI_index[,2],FUN=function(x){grep(pattern="CH4 Emissions \\(kt\\) for Natural Gas Systems",x)}),1])

  #Columns = year, rows = various types of sources.  First row is just to
  #identify the first row of the tables as there is also header information that
  #we want to exclude
  first_row <- which(read_xlsx(GHGI_file,sheet = GHGI_Activity_sheet,.name_repair = "minimal")[,1]=="Segment/Source")
  GHGI_Activity <- read_xlsx(GHGI_file,sheet = GHGI_Activity_sheet,skip=first_row,col_names = T)
  
  first_row <- which(read_xlsx(GHGI_file,sheet = GHGI_Emissions_sheet,.name_repair = "minimal")[,1]=="Segment/Source")
  GHGI_Emissions <- read_xlsx(GHGI_file,sheet = GHGI_Emissions_sheet,skip=first_row,col_names = T)
  
  if(all(GHGI_Pipeline=="GHGI")){
    #all the sources we're looking for, written exactly as in the GHGI file
    Data_list <- c("Pipeline Leaks","M&R (Trans. Co. Interconnect)","M&R (Farm Taps + Direct Sales)",
                   "Pipeline venting")
    
    #use sapply to find the row using data list, specify the column as the year
    #and grab the relevant emissions and activity data into a dataframe.
    GHGI_Pipeline <- data.frame("Type"=Data_list,
                                "Emissions"=as.numeric(unlist(
                                  sapply(Data_list,FUN=function(x){GHGI_Emissions[which(GHGI_Emissions[,1]==x)[1],as.character(inventory_year)]})))*
                                  1E9/(16.043*60*60*24*365),#convert from kt/yr to mol/s
                                "Total_stations"=as.numeric(unlist(
                                  sapply(Data_list,FUN=function(x){GHGI_Activity[which(GHGI_Activity[,1]==x)[1],as.character(inventory_year)]})))*
                                  1609.344,#convert from miles to meters
                                row.names = NULL)
  }
  
  pipeline_EF <- sum(GHGI_Pipeline[,2])/GHGI_Pipeline[1,3] #mol/m/s
  #sum of emissions / miles of pipelines (activity data from leaks entry)
  
  if(all(GHGI_transmission_compressors=="GHGI")){
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
    GHGI_transmission_compressors <- data.frame("Type"=Data_list,
                                                "Emissions"=as.numeric(unlist(sapply(Data_list,FUN=function(x){GHGI_Emissions[which(GHGI_Emissions[,1]==x)[1],as.character(inventory_year)]})))*
                                                  1E9/(16.043*60*60*24*365),#convert from kt/yr to mol/s
                                                "Total_stations"=as.numeric(unlist(sapply(Data_list,FUN=function(x){GHGI_Activity[which(GHGI_Activity[,1]==x)[1],as.character(inventory_year)]}))),
                                                row.names = NULL)
  }
  
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
  
  suppressWarnings(rm(GHGI_transmission_compressors,GHGI_Pipeline,GHGI_Activity,GHGI_Emissions,first_row,Data_list,
                      Engine_transmission_fraction,Turbine_transmission_fraction))
  cat("Finished loading all input data at",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes since start\n")
  ################################################################################
  #process the transmission pipeline data
  
  # Crop to just larger than d03 - don't know if it's necessary to have this
  # buffer but it can't hurt
  e <- ext(domain)*1.1
  pipes_crop_EIA <- crop(project(pipes_EIA,crs(domain)),e)
  
  #Set values to the pipe length (in metres) in each cell
  if(nrow(pipes_crop_EIA)>0){
    pipes_by_cell_EIA=rasterizeGeom(pipes_crop_EIA,domain_template,fun="length")
  }else{
    pipes_by_cell_EIA=domain_template
  }
  #Now multiply by the effective emission factor in mol/m/s to get to mol/s
  pipes_rast_EIA <- pipes_by_cell_EIA*pipeline_EF
  #Calculate flux, mol/s to nmol/m2/s
  pipes_flux <- pipes_rast_EIA*1e9/(cellSize(pipes_rast_EIA,unit="m"))  
  
  #Set NA values to 0 and mask to the exact domain
  pipes_flux[is.na(pipes_flux)]<-0
  pipes_flux <- mask(pipes_flux,domain)
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
  
  if(nrow(compressors_ghgrp_crop)>0){
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
    
    # if(max(combined_data$distance)>1000){
    #   View(combined_data)
    #   plot(ext(domain))
    #   lines(State_Tigerlines)
    #   points(compressors_crop_HIFLD,cex=2)
    #   points(compressors_ghgrp_crop,col="red")
    #   add_legend("bottom",legend = c("HIFLD","GHGRP"),pt.cex = c(2,1),
    #              horiz=T,col=c("black","red"),pch=16,xpd=T)
    #   stop("some GHGRP compressors didn't have a HIFLD compressor within 1 km")
    # }
    
    #scale the GHGRP emissions so that the domain average is equal to the national
    #average and convert GHGRP from MT CH4/yr to mol/s
    GHGRP_scaling <- compressor_avg_emissions/mean(compressors_ghgrp_crop$ghg_quantity*1e6/(16.043*365*24*60*60))
    compressors_final$emiss[location_matches$to_id] <- compressors_ghgrp_crop$ghg_quantity*1e6/(16.043*365*24*60*60)*GHGRP_scaling
  }
  
  compressor_rast <- rasterize(compressors_final, domain_template, "emiss", fun=sum) # in mol/s
  compressor_flux <- compressor_rast*1e9/(cellSize(compressor_rast,unit="m"))  # Calculate flux in nmol/m2/s
  compressor_flux[is.na(compressor_flux)]<-0
  compressor_flux <- mask(compressor_flux,domain)
  ################################################################################
  # And save the output
  
  if(verbose){
    if(nrow(compressors_final)>0){
      # Save point sources as csv files - first just the raw dataframe
      write.csv(compressors_final, file.path(output_directory,"NG_trans_compressors_all.csv"))
      
      # Now just the names, coordinates and emissions
      compressors_output <- data.frame(compressors_final$NAME,crds(compressors_final),compressors_final$emiss)
      names(compressors_output) <- c('Site_Name','Longitude','Latitude','Emission_mol_per_s')
      write.csv(compressors_output,file.path(output_directory,"NG_trans_compressors.csv"),row.names=FALSE)
    }
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
             "NG transmission - compressors\n GHGRP reporters + average GHGI emissions distributed using Homeland\nInfrastructure Foundation-Level Database",
             plot_directory=plot_directory,
             domain=domain,County_Tigerlines=County_Tigerlines,
             State_Tigerlines=State_Tigerlines)
    
    not_log_plot(pipes_flux,filename="NG_trans_pipes",
                 "NG transmission - pipelines\n EIA pipeline data * GHGI EF",
                 plot_directory=plot_directory,
                 domain=domain,County_Tigerlines=County_Tigerlines,
                 State_Tigerlines=State_Tigerlines)
    
    dir.create("Summed_Sectors",showWarnings = F)
    
    Summed_NG_transmission = compressor_flux+pipes_flux
    log_plot(Summed_NG_transmission,
             "NG Transmission Sector\nEIA for pipelines + HFILD/GHGRP\nfor compressors",
             plot_directory=plot_directory,
             domain=domain,County_Tigerlines=County_Tigerlines,
             State_Tigerlines=State_Tigerlines)
  }
  cat("Finished natural gas transmission sector: Transmission in",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes\n\n")
}
