#'@title Create gridded methane emissions maps for wastewater treatment plants
#'  and septic systems
#'
#'@description \code{Wastewater} is an internal function that we strongly
#'  recommend users do not use directly, instead using
#'  \code{\link{CH4_inventory_build}} and \code{\link{M3T_config}} which call
#'  this function. \code{Wastewater} writes up to 15 netcdf files of gridded
#'  methane emissions - 1 - 4  for municipal wastewater treatment facilities, 1
#'  for industrial wastewater treatment facilities, 1 - 2 for septic systems,
#'  and then up to 8 possible combinations across these.
#'
#'@details This function calculates and grids methane emissions from wastewater
#'  treatment and septic systems. It takes the total flow through municipal
#'  facilities to either distribute national total emissions proportionally or
#'  to calculate emissions using a log-log linear relationship from the
#'  scientific literature.  For industrial facilities, the Environmental
#'  Protection Agency's (EPA) Greenhouse Gas Reporting Program (GHGRP) data is
#'  used. Lastly septic emissions are mapped to "developed, open space" and
#'  "developed, low intensity" land cover.  The emissions are either
#'  disaggregated from national septic emissions or estimated using
#'  state-specific values as available.
#'
#'  For municipal wastewater treatment facilities there are a few variations.
#'  Input data can be set to either the Environmental Protection Agency's (EPA)
#'  Clean Watershed Needs Survey (CWNS), available at
#'  \url{https://www.epa.gov/cwns/clean-watersheds-needs-survey-cwns-past-reports-and-data}
#'  or the National Pollutant Discharge Elimination System (NPDES) Discharge
#'  Monitoring Reports (DMR) available through the Enforcement and Compliance
#'  History Online (ECHO) tool at
#'  \url{https://echo.epa.gov/trends/loading-tool/water-pollution-search}.  The
#'  CWNS is produced infrequently while DMR data is regularly updated, though
#'  the facilities and their data can differ between these products.  Facility
#'  locations and total effluent flow is used from these files.  The calculation
#'  method can then either be set to disaggregate the EPA Greenhouse Gas
#'  Inventory (GHGI) national total emissions to these facilities, using flow as
#'  a proxy, or calculate emissions for each facility using a log-log linear
#'  relationship between flow and emissions (log10(CH4 emissions in g/s) =
#'  1.28*log10(flow in m3/s) + 0.93) based on Moore et al.  Moore et al.
#'  measured emission rates at 96 wastewater treatment facilities to develop
#'  this relationship, which suggests nearly 2x the emissions as the GHGI using
#'  DMR flow rates as of 2023.  Note the authors' first paper discussed this
#'  relatinoship and was updated using the complete dataset from a followup
#'  paper.
#'
#'  GHGRP data is used for any and all industrial wastewater treatment
#'  facilities.
#'
#'  For septic emissions the output from \code{\link{NLCD_fractions_by_state}}
#'  is used. This provides the fractional coverage of the 2 landcover types
#'  (developed, open space and developed, low intensity) for each state in the
#'  domain as well as gridded data for the whole domain. Their are 2 approaches
#'  to calculate methane emissions from these.
#'
#'  National option - The total landcover of these types across the continental
#'  US is calculated using the NLCD combined with estimates using the limited AK
#'  NLCD data available.  We use this national total land area in km2 and the
#'  GHGI national total septic emissions in mol/s to calculate an emission
#'  factor in mol/s/km2.  This emission factor can be combined with the gridded
#'  land cover data to get gridded septic emissions.  Note this ignores any of
#'  this land cover in other US territory outside of the continental US like
#'  Guam, Hawaii, or American Somoa.
#'
#'  State option - we can instead take state-reported data rather than using
#'  national  Some states report the fraction of people in the state using
#'  septic systems to the American Housing Survey in the Plumbing, Water, and
#'  Sewage Disposal survey, available here
#'  \url{https://www.census.gov/programs-surveys/ahs/data/interactive/ahstablecreator.html?s_areas=00000&s_year=2021&s_tablename=TABLE1&s_bygroup1=1&s_bygroup2=1&s_filtergroup1=1&s_filtergroup2=1}.
#'  Note this data is reported every other year, not annually. For states that
#'  don't, the 1990 census data on septic fraction is available here
#'  \url{https://www.census.gov/data/tables/time-series/dec/coh-sewage.html} and
#'  this will be scaled using the change in the national septic fraction from
#'  1990 (from census) to the desired year (from the housing survey).  This new
#'  fraction can be combined with up to date population data available from the
#'  census
#'  \url{https://www.census.gov/data/tables/time-series/demo/popest/2020s-state-total.html},
#'  the total amount of this land cover in the state, and the GHGI septic EF (g
#'  CH4 per person per day) to get emissions.
#'
#'  The GHGRP includes only facilities that emit at least 25,000 metric tons of
#'  carbon dioxide equivalent while the GHGI is intended to capture all national
#'  emissions.  GHGRP data is available starting in 2010 and generally is about
#'  2 years behind present day, the GHGI is available starting in 1990 and is
#'  updated approximately in sync with the GHGRP.  Census data comes out
#'  annually and is generally 1 year behind.  All three datasets are annual. The
#'  GHGRP is at the facility scale while the GHGI is national totals.  Census
#'  data can be obtained at many different scales, but is used here at the state
#'  and national scale.
#'
#'  The GHGI is available at
#'  \url{https://www.epa.gov/ghgemissions/inventory-us-greenhouse-gas-emissions-and-sinks}
#'  The GHGRP is available at \url{https://www.epa.gov/ghgreporting} The Annual
#'  NLCD is available at \url{https://doi.org/10.5066/P94UXNTS} The 2011 AK NLCD
#'  is available at \url{https://doi.org/10.5066/P97S2IID} The 2016 AK NLCD is
#'  available at \url{https://doi.org/10.5066/P96HHBIE}
#'
#'  See references \href{https://doi.org/10.1016/j.isprsjprs.2020.02.019}{Homer et
#'  al.} and \href{https://doi.org/10.1021/acs.est.2c05373}{Moore et al.}
#'
#'@inheritParams Municipal_solid_waste
#'
#'@param verbose Logical indicating whether to save additional output.  This
#'  includes up to 5 plots of the gridded methane emissions.  Up to 2 for
#'  municipal wastewater treatment facilities, up to 2 for septic, and 1 for
#'  industrial wastewater treatment facilities
#'@param Wastewater_use_CWNS Logical.  Pulled from \code{\link{M3T_config}}.
#'@param Wastewater_use_DMR Logical.  Pulled from \code{\link{M3T_config}}.
#'@param Wastewater_Municipal_Method_Moore Logical.  Pulled from
#'  \code{\link{M3T_config}}.
#'@param Wastewater_Municipal_Method_GHGI Logical.  Pulled from
#'  \code{\link{M3T_config}}.
#'@param Wastewater_national_septic Logical.  Pulled from
#'  \code{\link{M3T_config}}.
#'@param Wastewater_state_septic Logical.  Pulled from \code{\link{M3T_config}}.
#'@param Source_GHGRP_wastewater Character.  Pulled from
#'  \code{\link{M3T_config}}.
#'@param Source_CWNS Character.  Pulled from \code{\link{M3T_config}}.
#'@param Source_DMR Character.  Pulled from \code{\link{M3T_config}}.
#'@param Source_State_population_data Character.  Pulled from
#'  \code{\link{M3T_config}}.
#'@param National_wastewater_info Data.frame.  Pulled from
#'  \code{\link{M3T_config}}.
#'@param Wastewater_reported_State_info Data.frame.  Pulled from
#'  \code{\link{M3T_config}}.
#'@param GHGI_wastewater_data Data.frame.  Pulled from \code{\link{M3T_config}}.
#'@param Total_national_open_or_low_int_area Numeric.  Pulled from
#'  \code{\link{M3T_config}}.
#'@returns Nothing is returned from the function, but the main outputs are up to
#'  14 netcdf files of the methane emissions from wastewater.  They are titled
#'  "Wastewater_ind.nc" for industrial wastewater,
#'  "Wastewater_X_Y_dom_central.nc" for municipal wastewater,
#'  "Wastewater_dom_septic_Z.nc" for septic emissions.  X is the input data used
#'  for the wastewater treatment plants - CWNS or DMR, Y is the method of
#'  converting to emissions - GHGI for downscaling GHGI totals proportionally
#'  with flow or Moore for applying the moore et al. log-linear relationship
#'  between flow and emissions, and Z is the approach used for septic emissions
#'  - bystate or national.
#'  The 8 possible combinations are named similarly as
#'  "Wastewater_sector_total_X_Y_Z.nc".
#'@references \href{https://doi.org/10.1016/j.isprsjprs.2020.02.019}{Homer et
#'  al.}
#'@references \href{https://doi.org/10.1021/acs.est.2c05373}{Moore et al.}
#'@seealso [CH4_inventory_build()] Calculates methane inventory using settings
#'  provided in config.
#'
#'  [NLCD_fractions_by_state()] Calculates the fraction of low intensity urban
#'  land cover in each pixel.
#'
#'  [M3T_config] Generates the config function with user-editable settings used
#'  throughout processing.
#'@keywords internal


Wastewater <- function(input_directory,
                       output_directory,
                       Wastewater_use_CWNS,
                       Wastewater_use_DMR,
                       Wastewater_Municipal_Method_Moore,
                       Wastewater_Municipal_Method_GHGI,
                       Wastewater_national_septic,
                       Wastewater_state_septic,
                       domain,
                       domain_template,
                       GHGRP_facility_data,
                       Source_GHGRP_wastewater,
                       Source_CWNS,
                       Source_DMR,
                       Source_wastewater_NLCD,
                       Source_State_population_data,
                       inventory_year,
                       National_wastewater_info,
                       Wastewater_reported_State_info,
                       GHGI_wastewater_data,
                       GHGI_data_yr,
                       Total_national_open_or_low_int_area,
                       State_Tigerlines,
                       state_name_list,
                       County_Tigerlines,
                       plot_directory,
                       State_CB,
                       verbose){
  
  starttime <- Sys.time()
  cat("Starting wastewater sector: Wastewater\n")
  
  Wastewater_partial_output_directory <- file.path(output_directory,"Wastewater","processed_NLCD_data")
  
  Wastewater_output_directory <- file.path(output_directory,"Wastewater")
  dir.create(Wastewater_output_directory,showWarnings = F)
  ################################################################################
  #load Clean Watershed Needs Survey
  
  if(Source_CWNS=="M3T"){
    CWNS_yr <- which.min(abs(inventory_year - c(2012,2022)))
    if(CWNS_yr==1){
      CWNS <- M3T::CWNS_2012
    }else{
      CWNS <- M3T::CWNS_2022
    }
    CWNS_yr <- c(2012,2022)[CWNS_yr]
  }else{
    if(dir.exists(Source_CWNS)){
      CWNS_yr <- 2022
      CWNS_file <- file.path(input_directory,"User_provided_CWNS")
      dir.create(CWNS_file,showWarnings = F)
      invisible(file.copy(list.files(Source_CWNS,full.names = T),
                          CWNS_file,overwrite = T,recursive=T))
      
      #load in the relevant tables from CWNS 2022
      Location <- utils::read.csv(file.path(CWNS_file,"PHYSICAL_LOCATION.csv"))
      Flow <- utils::read.csv(file.path(CWNS_file,"FLOW.csv"))
      Facilities <- utils::read.csv(file.path(CWNS_file,"FACILITIES.csv"))
      
      #filter to only municipal facilities
      Flow <- Flow[Flow$FLOW_TYPE=="Municipal Flow",]
      Location <- Location[Location$CWNS_ID %in% Flow$CWNS_ID,]
      Facilities <- Facilities[Facilities$CWNS_ID %in% Flow$CWNS_ID,]
      
      #combine the relevant data from the 3 files (equivalent to merge by ID,
      #then subsetting columns)
      CWNS_2022 <- Location
      CWNS_2022$EXIST_MUNICIPAL <- Flow$CURRENT_DESIGN_FLOW[match(CWNS_2022$CWNS_ID,Flow$CWNS_ID)]
      CWNS_2022$facility_name <- Facilities$FACILITY_NAME[match(CWNS_2022$CWNS_ID,Facilities$CWNS_ID)]
    }else if(file.exists(Source_CWNS)){
      CWNS_yr <- 2012
      CWNS_file <- file.path(input_directory,"User_provided_CWNS.csv")
      invisible(file.copy(Source_CWNS,CWNS_file,overwrite = T))
      CWNS <- utils::read.csv(CWNS_file)
    }
  }
  
  ################################################################################
  #load Discharge Monitoring Reports
  
  DMR_yr <- which.min(abs(inventory_year - (2010:2024)))
  DMR_file <- file.path(input_directory,paste0("DMR_",2010:2024,".csv")[DMR_yr])
  DMR_yr <- (2010:2024)[DMR_yr]
  if(Source_DMR=="M3T"){
    DMR <- utils::read.csv(file.path(input_directory,"DMR_data.csv"))
    DMR <- DMR[DMR$year==DMR_yr,]
  }else{
    DMR_file <- file.path(input_directory,"User_provided_DMR.csv")
    invisible(file.copy(Source_DMR,DMR_file,overwrite = T))
    DMR <- readLines(DMR_file,n = 10)
    DMR <- utils::read.csv(DMR_file,skip=grep(pattern = "Data Source",x = DMR)-1)
    
    #replace periods with underscores in naming for consistency/ease
    colnames(DMR) <- gsub("\\.","\\_",colnames(DMR))
    
    #remove those without location data and vect as lat/long assuming WGS
    #(didn't see one explicitly mentioned, little impact on location)
    DMR <- subset(DMR,!is.na(DMR$Facility_Latitude) & !is.na(DMR$Facility_Longitude))
  }
  
  #vect as lat/long assuming WGS
  #(didn't see one explicitly mentioned, has little impact on location)
  DMR_Municipal_flow <- terra::vect(DMR,geom=c("Facility_Longitude","Facility_Latitude"))
  terra::crs(DMR_Municipal_flow) <- "EPSG:4326"
  
  ################################################################################
  # First load in and prep the flow data
  
  if(Wastewater_use_CWNS){
    if(CWNS_yr==2012){
      # Nearly all the entries are NAD83, but some aren't
      # Convert everything over to WGS84
      # Assume blank or unknown entries are NAD83
      CWNS_wgs84 <- subset(CWNS, CWNS$HORIZONTAL_COORDINATE_DATUM=="World Geodetic System of 1984")
      CWNS_nad27 <- subset(CWNS, CWNS$HORIZONTAL_COORDINATE_DATUM=="North American Datum of 1927")
      CWNS_nad83 <- subset(CWNS, CWNS$HORIZONTAL_COORDINATE_DATUM!="North American Datum of 1927" & CWNS$HORIZONTAL_COORDINATE_DATUM!="World Geodetic System of 1984")
      
      CWNS_wgs84 <- terra::vect(CWNS_wgs84,geom=c("LONGITUDE","LATITUDE"))
      CWNS_nad27 <- terra::vect(CWNS_nad27,geom=c("LONGITUDE","LATITUDE"))
      CWNS_nad83 <- terra::vect(CWNS_nad83,geom=c("LONGITUDE","LATITUDE"))
      
      terra::crs(CWNS_wgs84) <- "EPSG:4326"  # WGS84
      terra::crs(CWNS_nad27) <- "EPSG:4267"  # NAD27
      terra::crs(CWNS_nad83) <- "EPSG:4269"  # NAD83
      
      CWNS_nad27_trans <- terra::project(CWNS_nad27,terra::crs(CWNS_wgs84))
      CWNS_nad83_trans <- terra::project(CWNS_nad83,terra::crs(CWNS_wgs84))
      
      CWNS_Municipal_flow <- rbind(CWNS_wgs84,CWNS_nad27_trans,CWNS_nad83_trans)
      
      CWNS_tot_flow <- sum(CWNS_Municipal_flow$EXIST_MUNICIPAL, na.rm=T)
      
    }else if(CWNS_yr==2022){
      CWNS_Municipal_flow <- terra::vect(CWNS_2022,geom=c("LONGITUDE","LATITUDE"),crs="EPSG:4269") # NAD83
      
      CWNS_tot_flow <- sum(CWNS_Municipal_flow$EXIST_MUNICIPAL, na.rm=T)
    }
  }
  
  
  if(Wastewater_use_DMR){
    DMR_tot_flow <- sum(DMR_Municipal_flow$Average_Daily_Flow__MGD_, na.rm=T)
  }
  
  # Take total emissions from the EPA GHGI
  GHGI_national_wastewater_nonseptic <- GHGI_wastewater_data$Nonseptic.Emissions[GHGI_wastewater_data$year==GHGI_data_yr]
  central_EPA_emiss <- GHGI_national_wastewater_nonseptic*1e9/(16.043*365*24*60*60)   #kt/y to mol/s
  
  cat("Finished loading in municipal treatment plant data at",format(Sys.time(),"%H:%M"),"\n")
  
  ################################################################################
  #these are assigned by the below function, but R doesn't see them being
  #created explicitly, so do so here just to make usethis::check() happy for
  #package building.
  WWTP_CWNS_GHGI_municipal <- WWTP_DMR_GHGI_municipal <- 
    WWTP_CWNS_Moore_municipal <- WWTP_DMR_Moore_municipal <- NULL
  ################################################################################
  #write a small helper function.  Creates a raster of emissions, cropped to the
  #domain, converted to proper units, and saved as 2 csvs.
  rasterize_plus <- function(input,outputname){
    #project and crop to the domain, remove NAs
    input_crop <- terra::project(input,terra::crs(domain))
    input_crop <- terra::crop(input_crop,domain)
    input_crop <- terra::mask(input_crop,domain)
    input_crop_filt <- subset(input_crop,!is.na(input_crop$emiss))
    
    #if there's at least 1 facility, rasterize it (mol/s)
    if(nrow(input_crop_filt)>0){
      rast <- terra::rasterize(input_crop_filt, domain_template, "emiss", fun=sum)
    }else{
      rast <- domain_template
    }
    
    # Calculate flux in nmol/m2/s
    rast_flux <- rast*1e9/(terra::cellSize(rast,unit="m"))  
    rast_flux[is.na(rast_flux)]<-0
    
    #save the raster to the active R environment
    assign(x = outputname,rast_flux,envir = parent.env(environment()))
    
    #also return the cropped data - for csv saving
    return(input_crop_filt)
  }
  
  ################################################################################
  #distribute EPA Municipal Wastewater Treatment Plant emissions from the GHGI
  #using the CWNS or DMR municipal flow as the proxy
  
  if(Wastewater_Municipal_Method_GHGI){
    if(Wastewater_use_CWNS){
      CWNS_Municipal_flow$emiss <- central_EPA_emiss*CWNS_Municipal_flow$EXIST_MUNICIPAL/CWNS_tot_flow   # in mol/s
      CWNS_GHGI_csv_data <- rasterize_plus(CWNS_Municipal_flow,"WWTP_CWNS_GHGI_municipal")
    }
    if(Wastewater_use_DMR){
      DMR_Municipal_flow$emiss <- central_EPA_emiss*DMR_Municipal_flow$Average_Daily_Flow__MGD_/DMR_tot_flow   # in mol/s
      DMR_GHGI_csv_data <- rasterize_plus(DMR_Municipal_flow,"WWTP_DMR_GHGI_municipal")
    }
  }
  ################################################################################
  #Instead calculate Municipal Wastewater Treatment Plant emissions using the
  #moore et al. EF
  
  if(Wastewater_Municipal_Method_Moore){
    if(Wastewater_use_CWNS){
      #convert from million gallons/day to m3/s
      CWNS_Municipal_flow$EXIST_MUNICIPAL <- CWNS_Municipal_flow$EXIST_MUNICIPAL*3785.41178/(24*60*60)
      #Apply the log-log linear relationship from Figure 2A of Moore et al.,
      #updated with the data in the 2025 nature publication
      CWNS_Municipal_flow$emiss <- 1.279367*log10(CWNS_Municipal_flow$EXIST_MUNICIPAL)+0.9257305
      #convert from log10(g/s) to mol/s
      CWNS_Municipal_flow$emiss <- (10^(CWNS_Municipal_flow$emiss))/(12.011+1.008*4)
      #convert back
      CWNS_Municipal_flow$EXIST_MUNICIPAL <- CWNS_Municipal_flow$EXIST_MUNICIPAL/3785.41178*(24*60*60)
      CWNS_Moore_csv_data <- rasterize_plus(CWNS_Municipal_flow,"WWTP_CWNS_Moore_municipal")
    }
    if(Wastewater_use_DMR){
      #convert from million gallons/day to m3/s
      DMR_Municipal_flow$Average_Daily_Flow__MGD_ <- DMR_Municipal_flow$Average_Daily_Flow__MGD_*3785.41178/(24*60*60)
      #Apply the log-log linear relationship from Figure 2A of Moore et al.
      DMR_Municipal_flow$emiss <- 1.279367*log10(DMR_Municipal_flow$Average_Daily_Flow__MGD_)+0.9257305
      #convert from log10(g/s) to mol/s
      DMR_Municipal_flow$emiss <- (10^(DMR_Municipal_flow$emiss))/(12.011+1.008*4)
      #convert back
      DMR_Municipal_flow$Average_Daily_Flow__MGD_ <- DMR_Municipal_flow$Average_Daily_Flow__MGD_/3785.41178*(24*60*60)
      DMR_Moore_csv_data <- rasterize_plus(DMR_Municipal_flow,"WWTP_DMR_Moore_municipal")
    }
  }
  
  cat("Finished calculating municipal treatment plant emissions at",format(Sys.time(),"%H:%M"),"\n")
  ################################################################################
  #organize and save CSV data
  
  #find all csv ready data and grab some info on them
  csv_options <- ls(pattern="csv_data")
  sources <- sapply(strsplit(csv_options,"_"),"[[",1)
  methods <- sapply(strsplit(csv_options,"_"),"[[",2)
  
  #blank output
  CWNS_csv <- data.frame()
  DMR_csv <- data.frame()
  
  for(A in which(sources=="CWNS")){
    #grab the coordinates and filter/reorganize to only needed variables
    temp <- get(csv_options[A])
    temp_latlong <- terra::crds(terra::project(temp,"epsg:4326"))
    colnames(temp_latlong) <- c("longitude","latitude")
    temp_csv <- as.data.frame(cbind(temp,temp_latlong))
    temp_csv <- temp_csv[,c("FACILITY_NAME","EXIST_MUNICIPAL","emiss","longitude","latitude")]
    
    #store emissions in a properly named column and rename columns
    if(methods[A]=="Moore"){
      colnames(temp_csv) <- c("Facility_name","Million_gallons_per_day_flow",
                              "Moore_Emissions_mol_per_s","longitude","latitude")
      temp_csv$GHGI_emissions_mol_per_s <- NA
    }else{
      colnames(temp_csv) <- c("Facility_name","Million_gallons_per_day_flow",
                              "GHGI_emissions_mol_per_s","longitude","latitude")
      temp_csv$Moore_Emissions_mol_per_s <- NA
    }
    
    #reorder
    temp_csv <- temp_csv[,c("Facility_name","Million_gallons_per_day_flow",
                            "GHGI_emissions_mol_per_s","Moore_Emissions_mol_per_s",
                            "longitude","latitude")]
    
    #add source
    temp_csv$Source <- gsub("CWNS","Clean Watershed Needs Survey",sources[A])
    
    #update the column if both methods are being used instead of adding new rows
    if(nrow(CWNS_csv)>0){
      CWNS_csv$Moore_Emissions_mol_per_s <- temp_csv$Moore_Emissions_mol_per_s
    }else{
      CWNS_csv <- rbind(CWNS_csv,temp_csv)
    }
  }
  
  
  #equivalent for DMR
  for(A in which(sources=="DMR")){
    temp <- get(csv_options[A])
    temp_latlong <- terra::crds(terra::project(temp,"epsg:4326"))
    colnames(temp_latlong) <- c("longitude","latitude")
    temp_csv <- as.data.frame(cbind(temp,temp_latlong))
    temp_csv <- temp_csv[,c("Facility_Name","Average_Daily_Flow__MGD_","emiss","longitude","latitude")]
    
    if(methods[A]=="Moore"){
      colnames(temp_csv) <- c("Facility_name","Million_gallons_per_day_flow",
                              "Moore_Emissions_mol_per_s","longitude","latitude")
      temp_csv$GHGI_emissions_mol_per_s <- NA
    }else{
      colnames(temp_csv) <- c("Facility_name","Million_gallons_per_day_flow",
                              "GHGI_emissions_mol_per_s","longitude","latitude")
      temp_csv$Moore_Emissions_mol_per_s <- NA
    }
    
    temp_csv <- temp_csv[,c("Facility_name","Million_gallons_per_day_flow",
                            "GHGI_emissions_mol_per_s","Moore_Emissions_mol_per_s",
                            "longitude","latitude")]
    
    temp_csv$Source <- gsub("DMR","Discharge Monitoring Reports",sources[A])
    
    if(nrow(DMR_csv)>0){
      DMR_csv$Moore_Emissions_mol_per_s <- temp_csv$Moore_Emissions_mol_per_s
    }else{
      DMR_csv <- rbind(DMR_csv,temp_csv)
    }
  }
  
  #combine the 2 and sort by name
  out_csv <- rbind(CWNS_csv,DMR_csv)
  out_csv <- out_csv[order(out_csv$Facility_name),]
  
  #Remove the column if empty
  if(!Wastewater_Municipal_Method_Moore){
    out_csv$Moore_Emissions_mol_per_s=NULL
  }
  if(!Wastewater_Municipal_Method_GHGI){
    out_csv$GHGI_emissions_mol_per_s=NULL
  }
  
  utils::write.csv(out_csv,file.path(Wastewater_output_directory,"Municipal_watewater_treatment.csv"),
                   row.names = F)
  ################################################################################
  #Now septic systems.  First download state population data
  
  septic_EPA_emiss <- GHGI_wastewater_data$Septic.Emissions[GHGI_wastewater_data$year==GHGI_data_yr]*1e9/(16.043*365*24*60*60)   #kt/y to mol/s
  
  #load the census population data and estimates then filter to the domain
  #states and inventory year
  
  if(Source_State_population_data=="M3T"){
    Census_state_population <- M3T::Census_state_population_M3T
  }else{
    #first define the state population dataset filenames - new each decade as
    #new census' are done
    if(inventory_year < 2020){
      State_pop_file <- file.path(input_directory,"2010-2020_state_pop.csv")
    }else if(inventory_year >= 2020){
      State_pop_file <- file.path(input_directory,"2020-2024_state_pop.csv")
    }
    
    if(Source_State_population_data=="download"){
      #Download the state population dataset needed
      if(!file.exists(State_pop_file)){
        if(inventory_year < 2020){
          data_URL <- "https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/state/totals/nst-est2020-alldata.csv"
          Trycatch_downloader(URL = data_URL,method = "save",output_location = State_pop_file,
                              error_message = paste0("State population data could not be downloaded using FTP link: ",data_URL))
        }else if(inventory_year <= 2024){
          data_URL <- "https://www2.census.gov/programs-surveys/popest/datasets/2020-2024/state/totals/NST-EST2024-ALLDATA.csv"
          Trycatch_downloader(URL = data_URL,method = "save",output_location = State_pop_file,
                              error_message = paste0("State population data could not be downloaded using FTP link: ",data_URL))
        }else{
          #for every year after the most recent census they output a new file with
          #the most recent estimates.  Iteratively test from the user input year
          #going back to 2024 (year code was written) to find the newest one
          #available.
          #just check if the URL is valid
          for(A in inventory_year:2024){
            data_URL <- paste0("https://www2.census.gov/programs-surveys/popest/datasets/2020-",A,"/state/totals/NST-EST",A,"-ALLDATA.csv")
            test_url=suppressWarnings(try(utils::download.file(data_URL,tempfile(".csv"),quiet = T),silent = T))
            if(test_url==0){
              break
            }
          }
          
          State_pop_file <- file.path(input_directory,paste0("2020-",A,"_state_pop.csv"))
          
          #download the data from the URL
          Trycatch_downloader(URL = data_URL,method = "save",output_location = State_pop_file,
                              error_message = paste0("State population data could not be downloaded using FTP link: ",data_URL))
        }
      }
    }else{
      State_pop_file <- file.path(input_directory,"User_provided_state_pop.csv")
      invisible(file.copy(Source_State_population_data,State_pop_file,overwrite=T))
    }
    
    Census_state_population <- utils::read.csv(State_pop_file)
  }
  
  ################################################################################
  #Update the state population data from config using this data
  
  #ensure organized as per state_name_list (STUSPS)
  State_population <- merge(Census_state_population,State_Tigerlines,by="NAME")
  State_population <- State_population[order(State_population$STUSPS),paste0("POPESTIMATE",GHGI_data_yr)]
  
  Wastewater_State_info <- M3T::Wastewater_1990_state_septic
  Wastewater_State_info <- Wastewater_State_info[order(Wastewater_State_info$State),]
  Wastewater_State_info$Method <- "scaled"
  
  #filter the reported state info to only those within 1 year (it's biannual),
  #within the domain
  Wastewater_reported_State_info <- Wastewater_reported_State_info[(Wastewater_reported_State_info$Year %in% (inventory_year-1):(inventory_year+1)) & 
                                                                     (Wastewater_reported_State_info$State %in% State_Tigerlines$STUSPS),]
  
  if(nrow(Wastewater_reported_State_info)!=0){
    #average across years if both years around the inventory year are available
    Wastewater_reported_State_info <- suppressWarnings(stats::aggregate(Wastewater_reported_State_info,by=list(Wastewater_reported_State_info$State),mean)[,-2])
    colnames(Wastewater_reported_State_info)[1] <- "State"
    #update using the reported info
    Wastewater_State_info$Septic_Fraction[match(Wastewater_reported_State_info$State,Wastewater_State_info$State)] <- Wastewater_reported_State_info$Septic_Fraction
    Wastewater_State_info$Method[match(Wastewater_reported_State_info$State,Wastewater_State_info$State)] <- "reported"
  }
  
  #filter the final info to the domain and incorporate the population
  Wastewater_State_info <- Wastewater_State_info[Wastewater_State_info$State %in% State_Tigerlines$STUSPS,]
  Wastewater_State_info$Population <- State_population
  
  #Same for the national data
  if(max(National_wastewater_info$Year) < (inventory_year-1)){
    National_wastewater_info <- National_wastewater_info[(National_wastewater_info$Year %in% max(National_wastewater_info$Year)) | 
                                                           National_wastewater_info$Year==1990,]
    cat("No reported national septic data for",inventory_year,"or +/- 1 year, using",max(National_wastewater_info$Year),"as the most recent data instead\n")
  }else{
    National_wastewater_info <- National_wastewater_info[(National_wastewater_info$Year %in% (inventory_year-1):(inventory_year+1)) | National_wastewater_info$Year==1990,]
    if(nrow(National_wastewater_info)>2){
      National_wastewater_info <- rbind(National_wastewater_info[1,],
                                        colMeans(National_wastewater_info[-1,]))
    }
  }
  
  if(nrow(Wastewater_reported_State_info)!=0 & any(Wastewater_reported_State_info$Year!=inventory_year)){
    cat("Reported septic data for",
        paste0(Wastewater_reported_State_info$State[Wastewater_reported_State_info$Year!=inventory_year],collapse = ", "),"are from the nearest available years,",
        paste0(Wastewater_reported_State_info$Year[Wastewater_reported_State_info$Year!=inventory_year],collapse = ", "),"respectively\n")
  }
  ################################################################################
  #Now load in output from NLCD_fractions_by_state - processing differs since
  #M3T is national, download is state by state
  
  if(Total_national_open_or_low_int_area == "M3T"){
    Total_national_open_or_low_int_area <- utils::read.csv(file.path(input_directory,"Total_national_septic_area.csv"))
    Total_national_open_or_low_int_area <- Total_national_open_or_low_int_area[which.min(abs(inventory_year - Total_national_open_or_low_int_area$year)),]
    NLCD_yr <- Total_national_open_or_low_int_area$year
    Total_national_open_or_low_int_area <- Total_national_open_or_low_int_area$Total_national_open_or_low_int_area
    if(NLCD_yr!=inventory_year & Source_wastewater_NLCD != "M3T"){
      cat("National Land Cover Data used for septic does not include",inventory_year,"using",NLCD_yr,"as the nearest data available\n")
    }
  }
  
  if(Source_wastewater_NLCD=="M3T"){
    nlcd_state_total_areas <- utils::read.csv(file.path(input_directory,"wastewater_state_septic_area.csv"))
    colnames(nlcd_state_total_areas) <- gsub("X","",colnames(nlcd_state_total_areas))
    NLCD_yr <- names(nlcd_state_total_areas)[-1][which.min(abs(inventory_year - as.numeric(colnames(nlcd_state_total_areas)[-1])))]
    if(NLCD_yr!=inventory_year){
      cat("National Land Cover Data used for septic does not include",inventory_year,"using",NLCD_yr,"as the nearest data available\n")
    }
    nlcd_state_total_areas <- nlcd_state_total_areas[,c("state",NLCD_yr)]
    colnames(nlcd_state_total_areas) <- c("X","open_or_low_int_area")
    nlcd_state_total_areas <- nlcd_state_total_areas[nlcd_state_total_areas$X %in% state_name_list,]
    
    Suburbia <- terra::rast(file.path(input_directory,"combined_wastewater_NLCD.tif"))
    Suburbia <- Suburbia[[names(Suburbia)==NLCD_yr]]
  }else{
    #output from NLCD_fractions_by_state - rasters have been reprojected
    Suburbia <- terra::rast(file.path(Wastewater_partial_output_directory,"NLCD_suburban.tif"))
    nlcd_state_total_areas <- utils::read.table(file.path(Wastewater_partial_output_directory,"NLCD_state_total_areas.csv"),header=T,sep=",")
  }
  
  #quickly ensure that the state data is all in the same order, alphabetical
  Wastewater_State_info <- Wastewater_State_info[order(Wastewater_State_info$State),]
  nlcd_state_total_areas <- nlcd_state_total_areas[order(nlcd_state_total_areas$X),]
  state_name_list <- sort(state_name_list)
  
  ################################################################################
  #Calculate septic emissions using emission factors and the land cover data
  #from NLCD_fractions_by_state.R
  
  tot_nlcd_area <- Total_national_open_or_low_int_area  # all states in km2
  NLCD_Tigerlines <- terra::project(State_Tigerlines,Suburbia)
  
  #if CONUS or custom with a very large domain - reprojecting domain can be
  #problematic
  if(any(terra::ext(domain)/terra::ext(State_Tigerlines) > 1.1)){
    NLCD_domain <- terra::as.polygons(terra::ext(domain)/terra::ext(State_Tigerlines) * terra::ext(NLCD_Tigerlines))
  }else{
    NLCD_domain <- terra::project(domain,NLCD_Tigerlines)
  }
  NLCD_res <- terra::res(terra::project(domain_template,NLCD_Tigerlines))
  terra::crs(NLCD_domain) <- terra::crs(NLCD_Tigerlines)
  
  
  if(Wastewater_national_septic){
    #Calculate state-by-state totals by equally distributing GHGI totals
    #to developed, open and developed low intensity land cover nationally.
    septic_flux <- Suburbia*septic_EPA_emiss/tot_nlcd_area # in mol/s/km2 since suburbia is just fractional coverage (0 to 1)
    
    # Now finalize units
    septic_flux <- septic_flux*1e9*1E-6  # Convert from mol/km2/s to nmol/m2/s
    septic_flux[is.na(septic_flux)]<-0
    
    #prep to project to domain
    septic_flux=terra::crop(septic_flux,NLCD_domain,snap="out")
    septic_flux=terra::mask(septic_flux,NLCD_domain,touches=T,updatevalue=0)
    
    #add a few pixels worth of buffer (at the domain resolution) filled with 0's
    #so the average doesn't consider these NA values to ignore in calculations
    #(drastically impacts avg).  Then finally reproject via average.
    if(!any(terra::ext(domain)/terra::ext(State_Tigerlines) > 1.1)){
      #prep to project to domain
      cover <- terra::extract(septic_flux,NLCD_domain,weights=T,cells=T)
      septic_flux[cover[,'cell']] <- septic_flux[cover[,'cell']]*cover[,'weight']
      septic_flux=terra::extend(septic_flux,fill=0,
                                terra::ext(septic_flux)+(NLCD_res*5))
    }
    septic_flux=terra::project(septic_flux,domain_template,method="average")
    
    
    #this approach will not NA out areas outside polygon domains - do so now.
    #Pixels that are outside the domain.  It's possible for something to be very
    #slightly outside the domain and non zero due to the reprojecting and this
    #will retain any such data.
    septic_flux[terra::mask(septic_flux,domain,inverse=T)==0] <- NA
  }
  
  if(Wastewater_state_septic){
    # Calculate state-by-state totals using state-specific septic fraction data
    Tot_area <- nlcd_state_total_areas$open_or_low_int_area # total area of both classes in km2 from nlcd_state_total_areas.csv
    pop <- Wastewater_State_info$Population
    #Fraction that's septic as reported or calculating by scaling the 1990
    #state value by the change in national septic fraction since 1990
    Wastewater_State_info$Updated_septic_frac[Wastewater_State_info$Method=="scaled"] <- Wastewater_State_info[Wastewater_State_info$Method=="scaled","Septic_Fraction"]*National_wastewater_info[2,2]/National_wastewater_info[1,2]
    #use as reported for the year
    Wastewater_State_info$Updated_septic_frac[Wastewater_State_info$Method=="reported"]  <- Wastewater_State_info[Wastewater_State_info$Method=="reported","Septic_Fraction"]
    
    state_tot_emiss <- data.frame("Combined_Septic_EF"=pop*Wastewater_State_info$Updated_septic_frac*GHGI_wastewater_data$EF[GHGI_wastewater_data$year==GHGI_data_yr]/(16.043*24*60*60) / Tot_area, #in mol/s/km2 (GHGI EF is in g/capita/day)
                                  "State"=Wastewater_State_info$State)
    
    #make a raster with separate values for each state to combine
    state_tot_emiss <- terra::merge(NLCD_Tigerlines,state_tot_emiss,by.x="STUSPS",by.y="State",all.y=T)
    #disagg from 1000 to 200 before rasterizing to better represent pixels on
    #state borders. Create as equivalent new raster instead to avoid compute
    #time (values are irrelevant for this task)
    state_tot_emiss <- terra::rasterize(state_tot_emiss,
                                        terra::rast(resolution=200,
                                                    ext=terra::ext(Suburbia),
                                                    crs="+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"),
                                        field="Combined_Septic_EF",touches=T)
    state_tot_emiss[is.na(state_tot_emiss)] <- 0
    state_tot_emiss <- terra::aggregate(state_tot_emiss,5,mean,na.rm=T)
    
    septic_flux2 <- Suburbia*state_tot_emiss #gridded and distributed appropriately across each state in mol/s/km2
    
    # Now finalize units
    septic_flux2 <- septic_flux2*1e9*1E-6  # Convert from mol/km2/s to nmol/m2/s
    septic_flux2[is.na(septic_flux2)]<-0
    
    #prep to project to domain
    septic_flux2=terra::crop(septic_flux2,NLCD_domain,snap="out")
    septic_flux2=terra::mask(septic_flux2,NLCD_domain,touches=T,updatevalue=0)
    
    #add a few pixels worth of buffer (at the domain resolution) filled with 0's
    #so the average doesn't consider these NA values to ignore in calculations
    #(drastically impacts avg).  Then finally reproject via average.
    if(!any(terra::ext(domain)/terra::ext(State_Tigerlines) > 1.1)){
      #prep to project to domain
      cover <- terra::extract(septic_flux2,NLCD_domain,weights=T,cells=T)
      septic_flux2[cover[,'cell']] <- septic_flux2[cover[,'cell']]*cover[,'weight']
      septic_flux2=terra::extend(septic_flux2,fill=0,
                                 terra::ext(septic_flux2)+(NLCD_res*5))
    }
    septic_flux2=terra::project(septic_flux2,domain_template,method="average")
    
    
    #this approach will not NA out areas outside polygon domains - do so now.
    #Pixels that are outside the domain.  It's possible for something to be very
    #slightly outside the domain and non zero due to the reprojecting and this
    #will retain any such data.
    septic_flux2[terra::mask(septic_flux2,domain,inverse=T)==0] <- NA
  }
  
  cat("Finished calculating septic emissions at",format(Sys.time(),"%H:%M"),"\n")
  ################################################################################
  #Download the relevant emissions data using the API
  #(https://www.epa.gov/enviro/envirofacts-data-service-api) and combine the
  #facility and emission data appropriately
  
  #download the relevant industrial wastewater - sector data
  #(https://www.epa.gov/enviro/greenhouse-gas-model).  
  ghgrp_wastewater_file <- file.path(input_directory,"GHGRP","industrial_wastewater_II.csv")
  if(Source_GHGRP_wastewater=="M3T"){
    ghgrp_data <- M3T::GHGRP_wastewater
  }else{
    if(Source_GHGRP_wastewater=="download"){
      data_URL <- "https://data.epa.gov/dmapservice/ghg.ii_subpart_level_information/csv"
      Trycatch_downloader(URL = data_URL,method = "save",output_location = ghgrp_wastewater_file,
                          error_message = paste0("Greenhouse Gas Reporting Program data could not be downloaded using API link: ",data_URL))
    }else{
      invisible(file.copy(Source_GHGRP_wastewater,ghgrp_wastewater_file,overwrite = T))
    }
    ghgrp_data <- utils::read.csv(ghgrp_wastewater_file)
  }
  ghgrp_data <- make_consistent(ghgrp_data)
  ################################################################################
  #Merge with location-like data
  
  #combine the datasets by ID, and year
  ghgrp_all_data <- merge(GHGRP_facility_data,ghgrp_data,
                          by=c("facility_id","year"), all=F)
  
  #keep only data for the year of interest
  ghgrp <- ghgrp_all_data[ghgrp_all_data$year==GHGI_data_yr,]
  
  #convert the relevant columns to numeric class
  ghgrp[,c("latitude","longitude","ghg_quantity")] <- apply(ghgrp[,c("latitude","longitude","ghg_quantity")],
                                                            2,FUN = function(x){as.numeric(x)})
  ################################################################################
  # Now rasterize and save the data
  
  ghgrp <- terra::vect(ghgrp,geom=c("longitude","latitude"))
  terra::crs(ghgrp) <- "epsg:4326"
  ghgrp_crop <- terra::project(ghgrp,domain)
  ghgrp_crop <- terra::crop(ghgrp_crop,domain)
  ghgrp_crop <- terra::mask(ghgrp_crop,domain)
  ghgrp_crop$emiss <- ghgrp_crop$ghg_quantity*1e6/(16.043*365*24*60*60) #MT CH4/yr to mol/s
  
  # Now rasterise
  ghgrp_rast <- terra::rasterize(ghgrp_crop, domain_template, "emiss", fun=sum)
  ghgrp_flux <- ghgrp_rast*1e9/(terra::cellSize(ghgrp_rast,unit="m"))  # Calculate flux in nmol/m2/s
  ghgrp_flux[is.na(ghgrp_flux)]<-0
  
  cat("Finished calculating industrial treatment plant emissions at",format(Sys.time(),"%H:%M"),"\n")
  ##############################################################################
  #save a csv for easy understanding of the filtered input data
  
  ghgrp_latlong <- terra::crds(terra::project(ghgrp_crop,"epsg:4326"))
  colnames(ghgrp_latlong) <- c("longitude","latitude")
  csv_data <- as.data.frame(cbind(ghgrp_crop,ghgrp_latlong))
  
  csv_data <- csv_data[,c("facility_id","facility_name.x","state","emiss",
                          "longitude","latitude")]
  
  colnames(csv_data) <- c("GHGRP_ID","facility_name",
                          "state",
                          "Emissions_mol_per_s",
                          "longitude","latitude")
  
  csv_data <- csv_data[order(csv_data$facility_name),]
  
  utils::write.csv(csv_data,file.path(Wastewater_output_directory,"GHGRP_industrial_watewater_treatment.csv"),
                   row.names = F)
  ################################################################################
  # Write the rasters
  
  if(Wastewater_Municipal_Method_GHGI){
    if(Wastewater_use_CWNS){
      writeCDF_no_newline(WWTP_CWNS_GHGI_municipal,
                          file.path(Wastewater_output_directory,'Wastewater_CWNS_GHGI_dom_central.nc'),
                          force_v4=TRUE,
                          varname='methane_emissions',
                          unit='nmol/m2/s',
                          longname='Methane emissions from treatment of domestic wastewater at centralised municipal treatment plants using the Clean Watershed Needs flow data to distribute GHGI emissions',
                          missval=-9999,
                          overwrite=TRUE)
    }
    if(Wastewater_use_DMR){
      writeCDF_no_newline(WWTP_DMR_GHGI_municipal,
                          file.path(Wastewater_output_directory,'Wastewater_DMR_GHGI_dom_central.nc'),
                          force_v4=TRUE,
                          varname='methane_emissions',
                          unit='nmol/m2/s',
                          longname='Methane emissions from treatment of domestic wastewater at centralised municipal treatment plants using the Discharge Monitoring Report flow data to distribute GHGI emissions',
                          missval=-9999,
                          overwrite=TRUE)
    }
  }
  if(Wastewater_Municipal_Method_Moore){
    if(Wastewater_use_CWNS){
      writeCDF_no_newline(WWTP_CWNS_Moore_municipal,
                          file.path(Wastewater_output_directory,'Wastewater_CWNS_Moore_dom_central.nc'),
                          force_v4=TRUE,
                          varname='methane_emissions',
                          unit='nmol/m2/s',
                          longname='Methane emissions from treatment of domestic wastewater at centralised municipal treatment plants using the Clean Watershed Needs flow data and the Moore et al. emission factor',
                          missval=-9999,
                          overwrite=TRUE)
    }
    if(Wastewater_use_DMR){
      writeCDF_no_newline(WWTP_DMR_Moore_municipal,
                          file.path(Wastewater_output_directory,'Wastewater_DMR_Moore_dom_central.nc'),
                          force_v4=TRUE,
                          varname='methane_emissions',
                          unit='nmol/m2/s',
                          longname='Methane emissions from treatment of domestic wastewater at centralised municipal treatment plants using the Discharge Monitoring Report flow data and the Moore et al. emission factor',
                          missval=-9999,
                          overwrite=TRUE)
    }
  }
  if(Wastewater_national_septic){
    writeCDF_no_newline(septic_flux,
                        file.path(Wastewater_output_directory,'Wastewater_dom_septic_national.nc'),
                        force_v4=TRUE,
                        varname='methane_emissions',
                        unit='nmol/m2/s',
                        longname='Methane emissions from onsite treatment of domestic wastewater (e.g. septic tanks), based on EPA national values',
                        missval=-9999,
                        overwrite=TRUE)
  }
  if(Wastewater_state_septic){
    writeCDF_no_newline(septic_flux2,
                        file.path(Wastewater_output_directory,'Wastewater_dom_septic_bystate.nc'),
                        force_v4=TRUE,
                        varname='methane_emissions',
                        unit='nmol/m2/s',
                        longname='Methane emissions from onsite treatment of domestic wastewater (e.g. septic tanks), based on calculations at the state level',
                        missval=-9999,
                        overwrite=TRUE)
  }
  
  writeCDF_no_newline(ghgrp_flux,
                      file.path(Wastewater_output_directory,'Wastewater_ind.nc'),
                      force_v4=TRUE,
                      varname='methane_emissions',
                      unit='nmol/m2/s',
                      longname='Methane emissions from industrial wastewater treatment plants',
                      missval=-9999,
                      overwrite=TRUE)
  ################################################################################
  #Create a sector total, 1 per variant
  if(Wastewater_Municipal_Method_Moore){
    WWTP_method <- "Moore_municipal"
    WWTP_text <- "Moore et al. emission factor combined with"
  }else if(Wastewater_Municipal_Method_GHGI){
    WWTP_method <- "GHGI_municipal"
    WWTP_text <- "GHGI total distributed using"
  }
  
  #just build all possible variations
  if(Wastewater_use_CWNS){
    if(Wastewater_state_septic){
      if(Wastewater_Municipal_Method_GHGI){
        Summed_wastewater_treatment_CWNS_GHGI_state = sum(WWTP_CWNS_GHGI_municipal,septic_flux2,ghgrp_flux,na.rm=T)
        writeCDF_no_newline(Summed_wastewater_treatment_CWNS_GHGI_state,
                            file.path(output_directory,paste0('Wastewater_sector_total_CWNS_GHGI_state.nc')),
                            force_v4=TRUE,
                            varname='methane_emissions',
                            unit='nmol/m2/s',
                            longname='Methane emissions from municipal treatment plants, industrial treatment plants, and septic systems',
                            missval=-9999,
                            overwrite=TRUE)
      }
      if(Wastewater_Municipal_Method_Moore){
        Summed_wastewater_treatment_CWNS_Moore_state = sum(WWTP_CWNS_Moore_municipal,septic_flux2,ghgrp_flux,na.rm=T)
        writeCDF_no_newline(Summed_wastewater_treatment_CWNS_Moore_state,
                            file.path(output_directory,paste0('Wastewater_sector_total_CWNS_Moore_state.nc')),
                            force_v4=TRUE,
                            varname='methane_emissions',
                            unit='nmol/m2/s',
                            longname='Methane emissions from municipal treatment plants, industrial treatment plants, and septic systems',
                            missval=-9999,
                            overwrite=TRUE)
      }
    }
    if(Wastewater_national_septic){
      if(Wastewater_Municipal_Method_GHGI){
        Summed_wastewater_treatment_CWNS_GHGI_national = sum(WWTP_CWNS_GHGI_municipal,septic_flux,ghgrp_flux,na.rm=T)
        writeCDF_no_newline(Summed_wastewater_treatment_CWNS_GHGI_national,
                            file.path(output_directory,paste0('Wastewater_sector_total_CWNS_GHGI_national.nc')),
                            force_v4=TRUE,
                            varname='methane_emissions',
                            unit='nmol/m2/s',
                            longname='Methane emissions from municipal treatment plants, industrial treatment plants, and septic systems',
                            missval=-9999,
                            overwrite=TRUE)
      }
      if(Wastewater_Municipal_Method_Moore){
        Summed_wastewater_treatment_CWNS_Moore_national = sum(WWTP_CWNS_Moore_municipal,septic_flux,ghgrp_flux,na.rm=T)
        writeCDF_no_newline(Summed_wastewater_treatment_CWNS_Moore_national,
                            file.path(output_directory,paste0('Wastewater_sector_total_CWNS_Moore_national.nc')),
                            force_v4=TRUE,
                            varname='methane_emissions',
                            unit='nmol/m2/s',
                            longname='Methane emissions from municipal treatment plants, industrial treatment plants, and septic systems',
                            missval=-9999,
                            overwrite=TRUE)
      }
    }
  }
  if(Wastewater_use_DMR){
    if(Wastewater_state_septic){
      if(Wastewater_Municipal_Method_GHGI){
        Summed_wastewater_treatment_DMR_GHGI_state = sum(WWTP_DMR_GHGI_municipal,septic_flux2,ghgrp_flux,na.rm=T)
        writeCDF_no_newline(Summed_wastewater_treatment_DMR_GHGI_state,
                            file.path(output_directory,paste0('Wastewater_sector_total_DMR_GHGI_state.nc')),
                            force_v4=TRUE,
                            varname='methane_emissions',
                            unit='nmol/m2/s',
                            longname='Methane emissions from municipal treatment plants, industrial treatment plants, and septic systems',
                            missval=-9999,
                            overwrite=TRUE)
      }
      if(Wastewater_Municipal_Method_Moore){
        Summed_wastewater_treatment_DMR_Moore_state = sum(WWTP_DMR_Moore_municipal,septic_flux2,ghgrp_flux,na.rm=T)
        writeCDF_no_newline(Summed_wastewater_treatment_DMR_Moore_state,
                            file.path(output_directory,paste0('Wastewater_sector_total_DMR_Moore_state.nc')),
                            force_v4=TRUE,
                            varname='methane_emissions',
                            unit='nmol/m2/s',
                            longname='Methane emissions from municipal treatment plants, industrial treatment plants, and septic systems',
                            missval=-9999,
                            overwrite=TRUE)
      }
    }
    if(Wastewater_national_septic){
      if(Wastewater_Municipal_Method_GHGI){
        Summed_wastewater_treatment_DMR_GHGI_national = sum(WWTP_DMR_GHGI_municipal,septic_flux,ghgrp_flux,na.rm=T)
        writeCDF_no_newline(Summed_wastewater_treatment_DMR_GHGI_national,
                            file.path(output_directory,paste0('Wastewater_sector_total_DMR_GHGI_national.nc')),
                            force_v4=TRUE,
                            varname='methane_emissions',
                            unit='nmol/m2/s',
                            longname='Methane emissions from municipal treatment plants, industrial treatment plants, and septic systems',
                            missval=-9999,
                            overwrite=TRUE)
      }
      if(Wastewater_Municipal_Method_Moore){
        Summed_wastewater_treatment_DMR_Moore_national = sum(WWTP_DMR_Moore_municipal,septic_flux,ghgrp_flux,na.rm=T)
        writeCDF_no_newline(Summed_wastewater_treatment_DMR_Moore_national,
                            file.path(output_directory,paste0('Wastewater_sector_total_DMR_Moore_national.nc')),
                            force_v4=TRUE,
                            varname='methane_emissions',
                            unit='nmol/m2/s',
                            longname='Methane emissions from municipal treatment plants, industrial treatment plants, and septic systems',
                            missval=-9999,
                            overwrite=TRUE)
      }
    }
  }
  
  
  ################################################################################
  #Finally, plot up this output nicely
  
  if(verbose){
    zmin <- 5000
    zmax <- 0
    if(Wastewater_state_septic){
      zmin <- min(zmin,as.numeric(terra::global(min(septic_flux2,na.rm=T),min,na.rm=T)))
      zmax <- max(zmax,as.numeric(terra::global(max(septic_flux2,na.rm=T),max,na.rm=T)))
    }
    if(Wastewater_national_septic){
      zmin <- min(zmin,as.numeric(terra::global(min(septic_flux,na.rm=T),min,na.rm=T)))
      zmax <- max(zmax,as.numeric(terra::global(max(septic_flux,na.rm=T),max,na.rm=T)))
    }
    if(Wastewater_state_septic){
      not_log_plot(septic_flux2,filename="Wastewater_dom_septic_bystate",
                   "Domestic Wastewater - Septic\n estimated state septic distributed using \ndeveloped open space/low intensity land cover",
                   zmin,zmax,
                   plot_directory=plot_directory,
                   domain=domain,County_Tigerlines=County_Tigerlines,
                   State_CB=State_CB)
    }
    
    if(Wastewater_national_septic){
      not_log_plot(septic_flux,filename="Wastewater_dom_septic_national",
                   "Domestic Wastewater - Septic\n national EPA septic distributed using \ndeveloped open space/low intensity land cover",
                   zmin,zmax,
                   plot_directory=plot_directory,
                   domain=domain,County_Tigerlines=County_Tigerlines,
                   State_CB=State_CB)
    }
    
    log_plot(ghgrp_flux,filename="Wastewater_ind",
             "Industrial Wastewater -\n GHGRP Reporters",
             plot_directory=plot_directory,
             domain=domain,County_Tigerlines=County_Tigerlines,
             State_CB=State_CB)
    
    
    
    #default min/max that should be overwritten using the data - to ensure
    #consistent axes across the 4 possible method combinations.
    WWTP_min=5000
    WWTP_max=0
    if(Wastewater_use_CWNS){
      if(Wastewater_Municipal_Method_Moore){
        #set 0 to NA so the minimum ignores 0 (log plot, so 0 = -inf)
        temp <- WWTP_CWNS_Moore_municipal
        temp[temp==0] <- NA
        if(!all(is.na(terra::values(temp)))){
          WWTP_min <- min(WWTP_min,as.numeric(terra::global(temp,min,na.rm=T)))
          WWTP_max <- max(WWTP_max,as.numeric(terra::global(temp,max,na.rm=T)))
        }
      }
      if(Wastewater_Municipal_Method_GHGI){
        temp <- WWTP_CWNS_GHGI_municipal
        temp[temp==0] <- NA
        if(!all(is.na(terra::values(temp)))){
          WWTP_min <- min(WWTP_min,as.numeric(terra::global(temp,min,na.rm=T)))
          WWTP_max <- max(WWTP_max,as.numeric(terra::global(temp,max,na.rm=T)))
        }
      }
    }
    if(Wastewater_use_DMR){
      if(Wastewater_Municipal_Method_Moore){
        temp <- WWTP_DMR_Moore_municipal
        temp[temp==0] <- NA
        if(!all(is.na(terra::values(temp)))){
          WWTP_min <- min(WWTP_min,as.numeric(terra::global(temp,min,na.rm=T)))
          WWTP_max <- max(WWTP_max,as.numeric(terra::global(temp,max,na.rm=T)))
        }
      }
      if(Wastewater_Municipal_Method_GHGI){
        temp <- WWTP_DMR_GHGI_municipal
        temp[temp==0] <- NA
        if(!all(is.na(terra::values(temp)))){
          WWTP_min <- min(WWTP_min,as.numeric(terra::global(temp,min,na.rm=T)))
          WWTP_max <- max(WWTP_max,as.numeric(terra::global(temp,max,na.rm=T)))
        }
      }
    }
    
    
    
    #actually do the plotting now, slight changes to filename and title for each
    WWTP_min <- log10(WWTP_min)
    WWTP_max <- log10(WWTP_max)
    if(Wastewater_use_CWNS){
      if(Wastewater_Municipal_Method_Moore){
        log_plot(WWTP_CWNS_Moore_municipal,filename="Wastewater_dom_central_CWNS_Moore",
                 "Domestic Wastewater -\n  Moore et al. emission factor combined with\nClean Watersheds Needs Survey",
                 WWTP_min,WWTP_max,plot_directory=plot_directory,
                 domain=domain,County_Tigerlines=County_Tigerlines,
                 State_CB=State_CB)
        #else if below as GHGI vs Moore can ~double emissions, which is
        #negligible on a log scale.
      }else if(Wastewater_Municipal_Method_GHGI){
        log_plot(WWTP_CWNS_GHGI_municipal,filename="Wastewater_dom_central_CWNS_GHGI",
                 "Domestic Wastewater -\n EPA total distributed using\nClean Watersheds Needs Survey",
                 WWTP_min,WWTP_max,plot_directory=plot_directory,
                 domain=domain,County_Tigerlines=County_Tigerlines,
                 State_CB=State_CB)
      }
    }
    if(Wastewater_use_DMR){
      if(Wastewater_Municipal_Method_Moore){
        log_plot(WWTP_DMR_Moore_municipal,filename="Wastewater_dom_central_DMR_Moore",
                 "Domestic Wastewater -\n Moore et al. emission factor combined with\nDischarge Monitoring Reports",
                 WWTP_min,WWTP_max,plot_directory=plot_directory,
                 domain=domain,County_Tigerlines=County_Tigerlines,
                 State_CB=State_CB)
      }else if(Wastewater_Municipal_Method_GHGI){
        log_plot(WWTP_DMR_GHGI_municipal,filename="Wastewater_dom_central_DMR_GHGI",
                 "Domestic Wastewater -\n EPA total distributed using\nDischarge Monitoring Reports",
                 WWTP_min,WWTP_max,plot_directory=plot_directory,
                 domain=domain,County_Tigerlines=County_Tigerlines,
                 State_CB=State_CB)
      }
    }
    
    
    
    
    
    #Now plot sectoral totals (industrial + municipal WWTP + septic)
    
    #incorporate the industrial WWTPs in the max, manually set the min to show
    #small septic emissions too, but do the max in the same manner as before.
    WWTP_min <- -4
    WWTP_max=0
    
    #only one or the other here as the emissions are approximately 2x with
    #Moore's approach, but on a log scale that's ~negligible.  So no need to
    #plot separately, it has no visible impact on spatial distribution or
    #emissions.
    if(Wastewater_Municipal_Method_Moore){
      WWTP_method <- "Moore_municipal"
      WWTP_text <- "Moore et al. emission factor combined with"
    }else if(Wastewater_Municipal_Method_GHGI){
      WWTP_method <- "GHGI_municipal"
      WWTP_text <- "GHGI total distributed using"
    }
    
    #just build all possible variations and calculate the max across all of them
    if(Wastewater_use_CWNS){
      if(Wastewater_state_septic){
        Summed_wastewater_treatment_CWNS_state = sum(get(paste0("WWTP_CWNS_",WWTP_method)),septic_flux2,ghgrp_flux,na.rm=T)
        CWNS_state_text=paste0(WWTP_text," CWNS")
        if(!all(is.na(terra::values(Summed_wastewater_treatment_CWNS_state)))){
          WWTP_max <- max(WWTP_max,as.numeric(terra::global(Summed_wastewater_treatment_CWNS_state,max,na.rm=T)))
        }
      }
      if(Wastewater_national_septic){
        Summed_wastewater_treatment_CWNS_national = sum(get(paste0("WWTP_CWNS_",WWTP_method)),septic_flux,ghgrp_flux,na.rm=T)
        CWNS_national_text=paste0(WWTP_text," CWNS")
        if(!all(is.na(terra::values(Summed_wastewater_treatment_CWNS_national)))){
          WWTP_max <- max(WWTP_max,as.numeric(terra::global(Summed_wastewater_treatment_CWNS_national,max,na.rm=T)))
        }
      }
    }
    if(Wastewater_use_DMR){
      if(Wastewater_state_septic){
        Summed_wastewater_treatment_DMR_state = sum(get(paste0("WWTP_DMR_",WWTP_method)),septic_flux2,ghgrp_flux,na.rm=T)
        DMR_state_text=paste0(WWTP_text," DMR")
        if(!all(is.na(terra::values(Summed_wastewater_treatment_DMR_state)))){
          WWTP_max <- max(WWTP_max,as.numeric(terra::global(Summed_wastewater_treatment_DMR_state,max,na.rm=T)))
        }
      }
      if(Wastewater_national_septic){
        Summed_wastewater_treatment_DMR_national = sum(get(paste0("WWTP_DMR_",WWTP_method)),septic_flux,ghgrp_flux,na.rm=T)
        DMR_national_text=paste0(WWTP_text," DMR")
        if(!all(is.na(terra::values(Summed_wastewater_treatment_DMR_national)))){
          WWTP_max <- max(WWTP_max,as.numeric(terra::global(Summed_wastewater_treatment_DMR_national,max,na.rm=T)))
        }
      }
    }
    
    
    
    #now actually plot
    WWTP_max <- log10(WWTP_max)
    if(Wastewater_use_CWNS){
      if(Wastewater_state_septic){
        log_plot(Summed_wastewater_treatment_CWNS_state,
                 paste0("Wastewater Treatment Sector\n",CWNS_state_text," (Domestic facilities)\nand GHGRP (industrial) and developed open space/low intensity NLCD\nland cover * state septic data (Septic)"),
                 WWTP_min,WWTP_max,plot_directory=plot_directory,
                 domain=domain,County_Tigerlines=County_Tigerlines,
                 State_CB=State_CB)
      }
      if(Wastewater_national_septic){
        log_plot(Summed_wastewater_treatment_CWNS_national,
                 paste0("Wastewater Treatment Sector\n",CWNS_national_text," (Domestic facilities)\nand GHGRP (industrial) and developed open space/low intensity NLCD\nland cover * national septic data (Septic)"),
                 WWTP_min,WWTP_max,plot_directory=plot_directory,
                 domain=domain,County_Tigerlines=County_Tigerlines,
                 State_CB=State_CB)
      }
    }
    if(Wastewater_use_DMR){
      if(Wastewater_state_septic){
        log_plot(Summed_wastewater_treatment_DMR_state,
                 paste0("Wastewater Treatment Sector\n",DMR_state_text," (Domestic facilities)\nand GHGRP (industrial) and developed open space/low intensity NLCD\nland cover * state septic data (Septic)"),
                 WWTP_min,WWTP_max,plot_directory=plot_directory,
                 domain=domain,County_Tigerlines=County_Tigerlines,
                 State_CB=State_CB)
      }
      if(Wastewater_national_septic){
        log_plot(Summed_wastewater_treatment_DMR_national,
                 paste0("Wastewater Treatment Sector\n",DMR_national_text," (Domestic facilities)\nand GHGRP (industrial) and developed open space/low intensity NLCD\nland cover * national septic data (Septic)"),
                 WWTP_min,WWTP_max,plot_directory=plot_directory,
                 domain=domain,County_Tigerlines=County_Tigerlines,
                 State_CB=State_CB)
      }
    }
  }
  cat("Finished wastewater sector: Wastewater at",format(Sys.time(),"%H:%M"),"with a total runtime of",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes\n\n")
}
