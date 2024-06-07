#Script (should be upgraded to function?) to run all other functions as desired
#to build a CH4 inventory one sector at a time.  Requires the user set a config
#file to determine which variants for some sectors are run among other things.

#questions/work to be done

#what shapefiles do we want to use for both subsetting in some cases, and
#plotting?  TL or CB (excludes water areas) or other (Joe had a much higher res
#shapefile)?

#need to post simplistic case for terra extract to github still.  exactextractR
#(adds exact=T for raster::extract) agrees almost perfectly, but for the simple
#case it gave the expected answer (1/2) whereas terra was always a little off.

#likely need to update/clean up comments throughout several of the functions to
#more appropriately describe what's happening.

#Need to be careful with any sort of API-based filtering of GHGRP data.  Saw
#that compressors subset to the states of interest included a few on the W
#coast...  Possible that some weren't included in the same (erroneous) fashion.

#double check ghgrp downloads - transmission (part W) includes a lot of
#facilities that report to subpart W, but are not what we're interested in.
#Additionally, some have different names between subparts - so merging may need
#to be done differently...

#Is there a better way to install/library packages than either of the 2
#approaches I provided?  I believe it should look different in a package anyway,
#so this may not matter.

#stress test main - change domain to a completely different region (and res),
#change to use an input raster rather than a bbox, change crs to one none of the
#input uses (e.g., UTM EPSG:3372), change year

#Do we want to keep the option to use XESMF at all, or just always use Terra or
#GdalUtils?

#As the package files (config, other functions, etc.) are going to be part of
#the same fileset, sourcing them should be fine, but the filepaths will need to
#be changed.  Is there a good way to set this up?

#Is there a better way to do a config file than a .R file like I did?

#So far the ECHO API doesn't seem to provide similar NEI data, but only facility
#locations without the emissions data.  Same for wastewater data
#https://echo.epa.gov/tools/web-services/facility-search-water#/Facility%20Information/get_cwa_rest_services_get_download

#Difficulty using USGS API too.
#https://data.usgs.gov/datacatalog/api/docs#/Harvest/read_harvested_files_harvest_files_get.
#PID for USGS NLCD is USGS:649595e9d34ef77fcb01dca3.

#Do we need the urban tigerlines, or just the focus city ones?  

#which is better?  Source the function Inventory_based_disaggregation here and
#within the code save output to an input environment (current setup) or source
#it in the function it's needed (i.e., stationary_combustion) and have it assign
#output to the parent directory?  Does it matter?

#I should go through and double check that any potential cropping/masking is
#done carefully.  Ideally, always include a slight buffer to avoid edge issues.
#Given the input can be any CRS, this could impact more than it originally did.

#Should change it so that different variants are all possible in any
#combination.  E.g., like Vulcan or ACES (either or both can be true).  This
#applies to wastewater (Moore vs GHGI, scaled emissions for septic vs reported
#septic emissions - which can be varied by state).

#Noticed an issue with WWTP that has an impact on a few pixels.  Pixels that are
#on the border of 2 states get included in both.  This occurs when masking.  Can
#easily deal with it by calculating fractional coverage within the state and
#multiplying by this weighting to put these pixels partially within each state
#and then adding them in the final domain-scale output.  Of course this ignores
#the fact that pre-aggregation more of the landtype of interest could be in 1
#state than the other, not necessarily evenly distributed...

#currently I have built things to get ~identical output to the original to test
#for bugs.  Several things can be updated to newer data.  E.g., CWNS 2022, NLCD
#2021.

#Note the newer CWNS seems to have a rather different layout, though it appears
#the same information is available.

#I need to carefully go through and set the codes to identify the appropriate
#year to use for data that are not available annually (e.g., NEI, CWNS).
#Additionally, I need some error capability to use a previous year if the year
#chosen is too recent to have data (e.g., GHGRP).

#For wastewater do we want to update the NLCD km2 national coverage of open or
#low_int landcover?  It wouldn't be hard, and terra runs drastically faster.
#Still going to take quite some time I'm sure.  We are using an NLCD that is 1
#or 2 versions ahead of the publication listing the values, and I can easily
#compare to that version to make sure the calculation is comparable.

#for NLCD_fraction saw that the domain-sum of open
#(global(open*cellSize(open,unit="km"),sum,na.rm=T) was 5235.635 when projecting
#a vector outline of the domain and crop/masking.  When projecting the raster to
#the domain (necessary for other calculations) I get the below:
#bilinear = 5162.411
#near = 5175.685
#cubic = 5174.696
#cubicspline = 5177.335
#lanczos = 5195.922
#We previoously used nearest neighbor and that's what I currently use too.

#need to update landfill to save combustion/waste emissions separately.

# Industrial wastewater facilities sometimes report CH4 to subparts AA
# (pulp/paper), C (combustion), and TT (industrial landfill), but the GHGRP
# equivalent is only from industrial wastewater

#downloading GHGRP data is sometimes repetitive (facility location type data in
#particular is downloaded in full several times).  Talking with EPA about better
#use of the API may solve this issue, but downloading this data 1 x in the main
#would improve things as well.  I could save it to the input folder like I do
#with tigerlines so it only has to be downloaded once.

#do we want to keep septic and wastewater in the same function?  They are pretty
#much completely independent calculations.  Industrial wastewater too (straight
#from GHGRP).

#wastewater matched exactly when replacing projection and area functions with
#terra and making sure input data was the same.  However, the NLCD fractions did
#differ more significantly given it used to be XESMF.  Both spatial and
#state/domain totals differed, though the differences weren't huge.

#should double check conversions are exactly what we want (minor, mass is 16.043
#g/mol or 16 g/mol, year to day = 365 or 365.25).  Should at least make sure
#they're all consistent.

#by default terra has progress bars for steps that take a long time (e.g., NLCD
#calculations).  Do we want this behavior?  If so we should add clarification
#before each discussing WHAT is in progress (confusing otherwise).

#need to go through and properly set verbose if statements for some codes.

#Do we want verbose to dictate saving csv's as well?  What about the input data?

#Need to check for reused data (e.g., ACES/Vulcan, GHGRP facility data, GHGRP
#combustion data) and consider downloading here rather than in multiple scripts.

#Need to consider saving input data like GHGRP to be loaded instead of
#re-downloaded each time (like tigerlines) to speed up multiple runs.

#we can speed up downloading API resources by filtering to just the data we need
#as long as we ensure it's not different data (like GHGRP was).

#for now I have the compressor code flag an error and provide some
#visuals/tables if there's any GHGRP reporting compressor stations without a
#matching HIFLD facility within 1 km.  GHGRP values overwrite the national
#average applied to other HIFLD facilities.  We can instead add any that far
#away as new facilities or dig into other ideas.  Only about 8 in our domain.
#Can run nationally to investigate.

#Need to consider the potential of multiples (3 GHGRP facilities match to the
#same HIFLD one).  I believe the code would just use 1 of them and ignore the
#others.

#checked and compressors and pipelines ~perfectly matched if using the terra
#area functions in the old code

#add clear user update(s) for each Sector (e.g., cat("starting sector X\n") ...
#cat("processing Y for sector X\n"))

#for NG distribution - the EIA does have state level data available by API, but
#I could not find an api to access the forms that are per company

#for NG distribution - we discussed having a separate script to build the by-LDC
#shapefiles and just including those in the package?.  Right now it's just
#loading in the prepared files from what I did for Philly before.  I will need
#to spend some time recoding this and rebuilding a merged dataset for the entire
#NEC.  I believe some GHGRP data may be able to replace some of the EIA/PHMSA
#data (need to investigate) and emailed GHGRP on 6/3/2024 to find out if we can
#access the GHGRP shapefiles directly.

#the eia API (distribion and stationry combustion) seems to have changed since I
#ran stationary combustion.  Filtering by year used to work fine if setting
#start/end year to the desired year for annual data.  Now that returns nothing
#and I need a +/- 1 year (stranger still, I needed to start 1 yr earlier for 1
#script, and end 1 year later for the other to get the desired year).

#for NG distribution - the more detailed GHGRP data I have easy access to via
#Envirofacts now has gas volumes delivered by sector which we were getting from
#EIA.  We still need customer counts from EIA, but this does provide a means to
#automatically match those datasets.  Unfortunately, they did not perfectly
#match, so there is some question as to which we rely on.  Most do match exactly
#or quite close though.

#for NG distribution - there WAS an API-downloadable table that included a bunch
#of data I previously webscraped (some of which is also available in PHMSA).
#However, it only goes to 2015 and I can't find equivalent data in the post 2015
#data tables.  As such I'm still webscraping for it...
#https://enviro.epa.gov/envirofacts/metadata/table/ghg/w_local_dist_companies_details

#for NG distribution - the extract function takes a LONG time to run.  Doing it
#for each state individually is not too bad, but doing it for the domain as a
#whole is CONSIDERABLY slower.  I should investigate simply summing the state
#values pixel-by-pixel (should give identical output).
#aggregate(do.call(rbind.data.frame, cover_all_old),
#by=list(do.call(rbind.data.frame, cover_all_old)$cell), sum,na.rm=T)

#for NG distribution - need to compare to original version, fixing all
#reprojections and area calculations to terra.

#for NG distribution and stationary combustion - there is a separate script to
#convert units, reformat, and reproject the output from these scripts because
#Joe did this via XESMF.  No need to do this now, and this should be
#reincorporated into the individual scripts.

#GEPA - the new versions no longer include forest fire CH4.  From the 2012
#version's paper "National forest fire emissions from the GHGI are distributed
#on a daily basis at 0.1° × 0.1° resolution using the Quick Fire Emissions Data
#set (QFED v2.4) for 2012".  QFED is available via FTP
#(http://ftp.as.harvard.edu/gcgrid/data/ExtData/HEMCO/QFED/v2018-07/), but the
#Harvard host site suggests GFED4 may be preferred now (at least for GEOS-CHEM).
#That's available through here (https://www.globalfiredata.org/index.html).

#GEPA - do we want visuals of the industrial landfills, non-fossil GEPA sectors,
#and fossil GEPA sectors?

#for NG dist and stat_comb - towards the end when saving and plotting there's
#quite a bit of redundant code that's also harder to read than it needs to be.
#Should clean up.

#Better organize output.  I think xl files in 1 folder, subsector output in 1,
#and sector output in a third

#roxygen or similar package from links Israel sent to properly document code.  

#Make all inputs to the functions actual arguments, not pulled from global.

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
  }else{
    focus_city_tigerlines <- terra::subset(Urban_Tigerlines,Urban_Tigerlines$NAME10 %in% focus_city)
  }

  rm(UAC_year,Census_filenames)
  ################################################################################
  #Actually run the functions now, based on the config file
  
  if(Process_landfills){
    Municipal_solid_waste(LMOP_file=file.path(input_directory,
                                              "lmopdata(Mar_24)_landfill_only.xlsx"),
                          domain=domain,state_name_list=state_name_list,
                          output_directory=output_directory,
                          inventory_year=inventory_year,
                          verbose=verbose)
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
    Stationary_combustion()
    rm(stationary_combustion_GHGI_data,stationary_combustion_emission_factors,
       stationary_combustion_by_state,stationary_combustion_by_domain,
       Process_stationary_combustion,Stationary_combustion)
  }
  if(Process_wastewater){
    NLCD_open_and_low_int()
    Wastewater()
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
    Prepare_GEPA()
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

  

  