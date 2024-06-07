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


Municipal_solid_waste <- function(LMOP_file,domain,state_name_list,
                                  output_directory,inventory_year,verbose){
  
  ################################################################################
  #Download the relevant emissions data using the API
  #(https://www.epa.gov/enviro/envirofacts-data-service-api) and combine the
  #facility and emission data appropriately
  
  #download the relevant landfill-sector data
  #(https://www.epa.gov/enviro/greenhouse-gas-model).  Must download the relevant
  #data for each possible sector separately as emissions are split by sector
  #(i.e., gas capture for electricity is subpart D, flaring is C, and landfill
  #emissions HH - all of which can occur at the same landfill)
  ghgrp_landfill_only_emissions <- fromJSON("https://data.epa.gov/efservice/HH_SUBPART_LEVEL_INFORMATION/JSON")
  # ghgrp_landfill_emissions2 <- fromJSON("https://data.epa.gov/dmapservice/ghg.hh_subpart_level_information/json")
  ghgrp_combustion_emissions <- fromJSON("https://data.epa.gov/efservice/C_SUBPART_LEVEL_INFORMATION/json")
  # ghgrp_electricity_emissions <- fromJSON("https://data.epa.gov/efservice/D_SUBPART_LEVEL_INFORMATION/json")
  # ghgrp_industrial_landfill_emissions <- fromJSON("https://data.epa.gov/efservice/tt_subpart_ghg_info/json")
  
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
  
  ghgrp_landfill_only_emissions <- make_consistent(ghgrp_landfill_only_emissions)
  ghgrp_combustion_emissions <- make_consistent(ghgrp_combustion_emissions)
  # ghgrp_electricity_emissions <- make_consistent(ghgrp_electricity_emissions)
  # ghgrp_industrial_landfill_emissions <- make_consistent(ghgrp_industrial_landfill_emissions)
  
  #rename so the columns are different
  colnames(ghgrp_landfill_only_emissions) <- gsub("ghg_quantity","HH_emissions",colnames(ghgrp_landfill_only_emissions))
  colnames(ghgrp_combustion_emissions) <- gsub("ghg_quantity","C_emissions",colnames(ghgrp_combustion_emissions))
  # colnames(ghgrp_electricity_emissions) <- gsub("ghg_quantity","D_emissions",colnames(ghgrp_electricity_emissions))
  # colnames(ghgrp_industrial_landfill_emissions) <- gsub("ghg_quantity","TT_emissions",colnames(ghgrp_industrial_landfill_emissions))
  
  #combine all 4 into 1 dataframe - using landfill emissions as the base to get
  #ID/year matches from
  ghgrp_landfill_emissions=Reduce(function(dtf1, dtf2){merge(dtf1, dtf2, by = c("facility_id","year","facility_name","ghg_name"), all.x = TRUE)},
                                  list(ghgrp_landfill_only_emissions,
                                       ghgrp_combustion_emissions))#,
  # ghgrp_electricity_emissions,
  # ghgrp_industrial_landfill_emissions))
  
  #convert the relevant columns to numeric class
  ghgrp_landfill_emissions[,c("HH_emissions","C_emissions")] <- apply(ghgrp_landfill_emissions[,c("HH_emissions","C_emissions")],
                                                                      2,FUN = function(x){as.numeric(x)})
  ghgrp_landfill_emissions$ghg_quantity <- rowSums(ghgrp_landfill_emissions[,c("HH_emissions","C_emissions")],na.rm=T)
  # ghgrp_landfill_emissions[,c("HH_emissions","C_emissions","D_emissions","TT_emissions")] <- apply(ghgrp_landfill_emissions[,c("HH_emissions","C_emissions","D_emissions","TT_emissions")],
  #                                                                                                  2,FUN = function(x){as.numeric(x)})
  # ghgrp_landfill_emissions$ghg_quantity <- rowSums(ghgrp_landfill_emissions[,c("HH_emissions","C_emissions","D_emissions","TT_emissions")],na.rm=T)
  
  
  # subpart d is only 1 facility and is NOT included in GHGRP flight.  
  # subpart C is many and IS included in GHGRP flight
  # subpart TT is only 1 facility and is NOT included in GHGRP flight.
  
  #Calculate national total in the GHGRP for the year of interest
  ghgrp_national <- sum(as.numeric(ghgrp_landfill_emissions$ghg_quantity[ghgrp_landfill_emissions$year==inventory_year]))/1000   # MT CH4/yr to Gg CH4/yr
  
  rm(ghgrp_landfill_only_emissions,ghgrp_combustion_emissions,make_consistent)
  ################################################################################
  #Download the relevant facility (e.g., location) data using the API and merge
  
  #see https://www.epa.gov/enviro/envirofacts-data-service-api
  data_URLs <- paste0("https://data.epa.gov/efservice/PUB_DIM_FACILITY/STATE/=/",state_name_list,"/JSON")
  
  #initialize output
  ghgrp_facility_info <- data.frame()
  for(A in 1:length(state_name_list)){
    # download data and read/combine in an R dataframe
    ghgrp_facility_info <- rbind(ghgrp_facility_info,fromJSON(data_URLs[A]))
  }
  
  #combine the datasets by ID, and year
  ghgrp_all_data <- merge(ghgrp_facility_info,ghgrp_landfill_emissions,
                          by=c("facility_id","year"), all=F)
  
  #keep only data for the year of interest
  ghgrp <- ghgrp_all_data[ghgrp_all_data$year==inventory_year,]
  
  #identify facilities that stopped reporting without a valid reason, then subset
  #to only landfill facilities
  nonreporting_facilities <- unique(ghgrp_facility_info$facility_id[ghgrp_facility_info$reporting_status=="STOPPED_REPORTING_UNKNOWN_REASON" & ghgrp_facility_info$year<=inventory_year])
  nonreporting_landfills <- nonreporting_facilities[which(nonreporting_facilities %in% unique(ghgrp_landfill_emissions$facility_id))]
  
  # find the latest data available for those that stopped reporting
  nonreporting_landfill_data <- ghgrp_all_data[ghgrp_all_data$facility_id %in% nonreporting_landfills,] %>%
    group_by(facility_id) %>%
    slice_max(order_by = year) %>%
    as.data.frame()
  
  #add this most recent data to the GHGRP dataset
  ghgrp <- rbind(nonreporting_landfill_data,ghgrp)
  
  #convert the relevant columns to numeric class
  ghgrp[,c("latitude","longitude","ghg_quantity")] <- apply(ghgrp[,c("latitude","longitude","ghg_quantity")],
                                                            2,FUN = function(x){as.numeric(x)})
  
  #delete all tempfiles and clean up working environment
  rm(A,ghgrp_all_data,ghgrp_facility_info,
     nonreporting_landfill_data,nonreporting_facilities,nonreporting_landfills)
  ################################################################################
  #Now convert to spatial and load/convert LMOP.  Assign GHGI_national -
  #GHGRP_national to all LMOP facilities equally.
  
  #convert to a spatial object, crop to d03, convert units
  ghgrp <- vect(ghgrp,geom=c("longitude","latitude"))
  crs(ghgrp) <- "epsg:4326"
  ghgrp <- project(ghgrp,crs(domain))
  ghgrp_crop <- crop(ghgrp, domain)
  ghgrp_crop$emiss <- ghgrp_crop$ghg_quantity*1e6/(16.043*365*24*60*60)   # MT CH4/yr to mol/s of CH4
  
  # Now calculate national totals
  EPA_total <- GHGI_landfill_total 
  non_ghgrp_total <- EPA_total - ghgrp_national
  
  # Read in LMOP and remove those in GHGRP.  Note facilities that used to report
  # to GHGRP and stopped with a valid reason are being considered LMOP facilities
  # in this approach.
  LMOP <- read_xlsx(LMOP_file,sheet="LMOP Database",col_names = T)
  LMOP_non_ghgrp <- LMOP[!(LMOP$`GHGRP ID` %in% ghgrp_landfill_emissions$facility_id[ghgrp_landfill_emissions$year==inventory_year]),]
  
  #This has some nans in, remove those
  LMOP_filt <- subset(LMOP_non_ghgrp,!is.na(Latitude))
  LMOP_filt <- vect(LMOP_filt,geom=c("Longitude","Latitude"))
  crs(LMOP_filt) <- "epsg:4326"
  LMOP_filt <- project(LMOP_filt,crs(domain))
  LMOP_crop <- crop(LMOP_filt, domain)
  
  # Find avg emission per non-GHGRP LMOP landfill (including the ones with no coordinates)
  avg_non_ghgrp <- non_ghgrp_total/nrow(LMOP_non_ghgrp)
  # For comparison, calculate avg ghgrp
  avg_ghgrp <- ghgrp_national/nrow(ghgrp)
  # Assign the avg emissions to LMOP landfills
  LMOP_crop$emiss <- avg_non_ghgrp*1e9/(16.043*365*24*60*60)   #Gg CH4/yr to mol/s of CH4
  
  ################################################################################
  # Now rasterise and save
  
  ghgrp_rast <- rasterize(ghgrp_crop, domain, "emiss", fun=sum)
  ghgrp_flux <- ghgrp_rast*1e9/(cellSize(ghgrp_rast,unit="m"))  # Calculate flux, mol/s to nmol/m2/s
  ghgrp_flux[is.na(ghgrp_flux)]<-0
  
  LMOP_rast <- rasterize(LMOP_crop, domain, field="emiss", fun=sum)
  LMOP_flux <- LMOP_rast*1e9/(cellSize(LMOP_rast,unit="m"))  # Calculate flux, mol/s to nmol/m2/s
  LMOP_flux[is.na(LMOP_flux)]<-0
  
  if(verbose){
    #sort both by name
    ghgrp_crop <- ghgrp_crop[order(ghgrp_crop$facility_name.x),]
    LMOP_crop <- LMOP_crop[order(LMOP_crop$`Landfill Name`),]
    
    # Save point sources as csv files - first just the raw dataframe
    write.csv(ghgrp_crop, file.path(output_directory,'MSW_GHGRP_all.csv'))
    write.csv(LMOP_crop, file.path(output_directory,"MSW_LMOP_all.csv"))
    
    # Now just the names, coordinates and emissions
    ghgrp_crop_output <- data.frame(ghgrp_crop$facility_name.x,crds(ghgrp_crop),ghgrp_crop$emiss)
    names(ghgrp_crop_output) <- c('Site_Name','Longitude','Latitude','Emission_mol_per_s')
    write.csv(ghgrp_crop_output,file.path(output_directory,'MSW_GHGRP.csv'),row.names=FALSE)
    
    LMOP_crop_output <- data.frame(LMOP_crop$`Landfill Name`,crds(LMOP_crop),LMOP_crop$emiss)
    names(LMOP_crop_output) <- c('Site_Name','Longitude','Latitude','Emission_mol_per_s')
    write.csv(LMOP_crop_output,file.path(output_directory,'MSW_LMOP.csv'),row.names=FALSE)
  }
  
  # Now write the rasters as netcdf files
  writeCDF(ghgrp_flux,
           file.path(output_directory,'MSW_GHGRP.nc'),
           force_v4=TRUE,
           varname='methane_emissions',
           unit='nmol/m2/s',
           longname='Methane emissions from municipal solid waste landfills that report to GHGRP',
           missval=-9999,
           overwrite=TRUE)
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
    log_plot(ghgrp_flux,filename="MSW_GHGRP",
             "Municipal Solid Waste -\n GHGRP reporters")
    log_plot(LMOP_flux,filename="MSW_LMOP",
             "Municipal Solid Waste -\n (GHGI - GHGRP) distributed using \nLandfill Methane Outreach Program")
    
    dir.create("Summed_Sectors",showWarnings = F)

    Summed_landfill <- ghgrp_flux+LMOP_flux
    log_plot(Summed_landfill,
             "Landfill Sector\nGHGRP + LMOP for municipal")
  }
  
}

