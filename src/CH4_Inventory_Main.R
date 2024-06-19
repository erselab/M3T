#Script to run all other functions as desired to build a CH4 inventory one
#sector at a time.  Requires the user set a config file to determine which
#variants for some sectors are run among other things.

#some defaults for a Philly centered domain with NAD83 crs
# CH4_inventory_build <- function(Input_directory="G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw data/",
#                                 Output_directory="G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed/",
#                                 focus_city="Philadelphia",
#                                 Inventory_year=2019,
#                                 # domain,
#                                 domain_bbox=cbind(c(-76.65,-73.65),
#                                                   c(38.97,40.97)),
#                                 domain_res=0.01,
#                                 domain_crs="epsg:4326",
#                                 ACES_directory="G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Inventories/ACES_v2.0",
#                                 Vulcan_directory="G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Inventories/Vulcan_v3.0",
#                                 verbose=TRUE){
  
  
  #shouldn't be necessary in a package, all will be in the same folder, need to
  #update appropriately.
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
  domain_bbox=cbind(c(-76.65,-73.65),
                    c(38.97,40.97))
  domain_res=0.01
  domain_crs="epsg:4326"
  ACES_directory="G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Inventories/ACES_v2.0"
  vulcan_directory="G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Inventories/Vulcan_v3.0"
  verbose=TRUE
  
  #https://ccdsupport.com/confluence/display/help/Reporting+Latitude+and+Longitude
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
  
  packagecheck <- c("terra", "ncdf4", "readxl", "pracma", "jsonlite","dplyr")
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
  #pracma = haversine function to calculate distances from lat/long points
  #jsonlite = allows simple loading of JSON files, primarily for downloading input data via API
  
  
  #may need, but may be able to avoid using
  #sf and sp = many spatial dataclasses
  #raster = raster dataclasses and processing functions
  #geosphere = some processing functions for spatial data
  #dplyr = part of tidyverse for cleaner, sometimes more efficient code.
  #        Landfill sector uses piping, group_by, and slice_max from this.
  
  
  #shouldn't need anymore, may need to code out in some scripts still
  #fBasics = timpallete colorscale
  #rvest and httr = easier access to html data
  #rgdal = dead package for spatial processing
  #maps = basic maps, other datasets are better
  
  #could use, but don't have to by any means
  #could use entire tidyverse throughout if desired...
  
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
  
  ################################################################################
  #create the domain and set it to all NaN
  if(exists("domain")){
    values(domain) <- NaN
  }else if(exists("domain_bbox")){
    domain <- rast(nrows=diff(range(domain_bbox[,2]))/domain_res, 
                   ncols=diff(range(domain_bbox[,1]))/domain_res,
                   xmin=min(domain_bbox[,1]), xmax=max(domain_bbox[,1]),
                   ymin=min(domain_bbox[,2]), ymax=max(domain_bbox[,2]), 
                   crs=domain_crs)
    rm(domain_bbox,domain_res,domain_crs)
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
  
  #subset to just those relevant for the domain (speedier)
  State_Tigerlines <- mask(State_Tigerlines,mask=as.polygons(domain))
  Urban_Tigerlines <- mask(Urban_Tigerlines,mask=State_Tigerlines)
  County_Tigerlines <- mask(County_Tigerlines,mask=State_Tigerlines)
  
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
    rm(Process_landfills,Municipal_solid_waste,GHGI_landfill_total)
  }
  if(Process_natural_gas_distribution){
    NG_distribution()
    rm(Process_natural_gas_distribution,natural_gas_post_meter_emission_factor,
       NG_distribution_by_domain,NG_distribution_by_LDC,NG_distribution_by_state)
    #add function name
  }
  if(Process_natural_gas_transmission){
    Transmission()
    rm(Process_natural_gas_transmission,Transmission)
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
    rm(stationary_combustion_GHGI_data,stationary_combustion_emission_factors,
       stationary_combustion_by_state,stationary_combustion_by_domain,
       Process_stationary_combustion,Stationary_combustion)
  }
  if(Process_wastewater){
    # rm(list=setdiff(ls(),c("domain","output_directory",
    #                        "clear","state_name_list","code_directory","State_Tigerlines")))
    # source(paste0(code_directory,"CH4_inventory_config.R"))
    # source(paste0(code_directory,"NLCD_fractions_by_state.R"))
    # rm(code_directory)
    NLCD_open_and_low_int(nlcd_file <- file.path("G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Shapefiles",
                                                             "nlcd_2019_land_cover_l48_20210604/nlcd_2019_land_cover_l48_20210604.img"),
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
    rm(Wastewater_Municipal_file,Wastewater_Municipal_method,
       Wastewater_State_info,GHGI_national_wastewater_septic,
       GHGI_national_wastewater_nonseptic,GHGI_septic_EF,
       Total_national_open_or_low_int_area,National_wastewater_info,
       Process_wastewater,NLCD_open_and_low_int,Wastewater)
  }
  if(Process_wetlands_and_inland_waters){
    
    rm(Process_wetlands_and_inland_waters)
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
    rm(Incorporate_remaining_sectors_from_gridded_EPA,Prepare_GEPA)
  }
  if(Combine_sectors){
    
  }

# }
  
  #example quick plots
  ##sf chloropleth
  # plot(all_merge_sf_LCC_state["res_wood_ER"])
  ##terra chloropleth, same colorscale
  # plot(all_merge_LCC_state,"res_wood_ER",col=sf.colors(13),breaks=13)

  

  