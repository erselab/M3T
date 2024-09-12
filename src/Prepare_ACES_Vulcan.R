#'@title Create gridded methane emissions maps for waste water treatment plants
#'  and septic systems
#'
#'@description `Wastewater` writes 4 netcdf files of gridded methane emissions -
#'  1 for municipal wastewater treatment facilities, 1 for industrial wastewater
#'  treatment facilities, and 2 for septic systems.   Also writes 5 csvs, 2 for
#'  municipal facilities, 2 for industrial facilities, and 1 for septic systems
#'  with all csvs being optional.
#'
#'@details This function calculates and grids methane emissions from wastewater
#'  treatment and septic systems. It takes the total flow through municipal
#'  facilities to either distribute national total emissions proportionally or
#'  to calculate emissions using a log-linear relationship from the scientific
#'  literature.  For industrial facilities, the Environmental Protection
#'  Agency's (EPA) Greenhouse Gas Reporting Program (GHGRP) data is used. Lastly
#'  septic emissions are mapped to developed, open space and developed, low
#'  intensity land cover regions.  The emissions total is taken as either
#'  national septic emissions or state-specific values as available.
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
#'  the products do differ.  Facility locations and total effluent flow is used
#'  from these files.  The calculation method can then either be set to
#'  disaggregate the EPA Greenhouse Gas Inventory (GHGI) national total
#'  emissions to these facilities, scaled by flow, or calculate emissions for
#'  each facility using a log-linear relationship between flow and emissions
#'  (log10(CH4 emissions in g/s) = 1.2*log10(flow in m3/s)+1) as determined by
#'  Moore et al.  Moore et al. measured emission rates at 63 wastewater
#'  treatment facilities to develop this relationship, which using DMR flow
#'  rates suggests nearly 2x the emissions as the GHGI as of 2023 when their
#'  paper was written.  Note the authors actually used emission factors with
#'  organic loading rather than this log-linear relationship to scale emissions.
#'
#'  GHGRP data is used for any and all industrial wastewater treatment
#'  facilities.  The necessary GHGRP data will be automatically downloaded.
#'
#'  For septic emissions the output from `NLCD_open_and_low_int` is used.  This
#'  provides the fractional coverage of the 2 landcover types (developed, open
#'  space and developed, low intensity) for the domain, separated by state.
#'  Their are 2 approaches to calculate methane emissions from these.
#'
#'  Option 1 - we know that as of 2016 the national total landcover of these 2
#'  landcover types was 352,032 km2 according to Homer et al.  As such, we can
#'  convert the fraction of each pixel that is one of these landcover types to
#'  the area of these landcover types in each pixel relative to the national
#'  total.  This ratio can be combined with national total septic emissions to
#'  get gridded septic emissions.
#'
#'  Option 2 - we can instead take state-reported total emissions rather than
#'  using national emissions.  Some states report the number of people in the
#'  state using septic systems to the American Housing Survey in the Plumbing,
#'  Water, and Sewage Disposal survey, available here
#'  \url{https://www.census.gov/programs-surveys/ahs/data/interactive/ahstablecreator.html?s_areas=00000&s_year=2021&s_tablename=TABLE1&s_bygroup1=1&s_bygroup2=1&s_filtergroup1=1&s_filtergroup2=1}.
#'  Note this data is reported every other year, not annually. For states that
#'  don't, the 1990 census data on septic fraction is available here
#'  \url{https://www.census.gov/data/tables/time-series/dec/coh-sewage.html} and
#'  this can be scaled using the change in the national septic fraction from
#'  1990 (from census) to the desired year (from the housing survey).  This new
#'  fraction can be combined with up to date population data available from the
#'  census
#'  \url{https://www.census.gov/data/tables/time-series/demo/popest/2020s-state-total.html}
#'  and the GHGI_septic_EF (g CH4 per person per day) to get emissions.
#'
#'  The GHGRP includes only facilities that emit at least 25,000 metric tons of
#'  carbon dioxide equivalent while the GHGI is intended to capture all national
#'  emissions.  GHGRP data is available starting in 2010 and generally is about
#'  2 years behind present day, the GHGI is available starting in 1990 and is
#'  updated approximately in sync with the GHGRP.  Census data comes out
#'  annually and is generally 1 year behind.  All three datasets are annual. The
#'  GHGRP is at the facility scale while the GHGI is national totals.  Census
#'  data can be obtained at many different scales, but is used here at the state
#'  at national scale.
#'
#'  The GHGI is available at
#'  \url{https://www.epa.gov/ghgemissions/inventory-us-greenhouse-gas-emissions-and-sinks}
#'  The GHGRP is available at \url{https://www.epa.gov/ghgreporting}
#'
#'  See references \href{https://doi.org/10.1016/j.isprsjprs.2020.02.019}{Homer et
#'  al.} and \href{https://doi.org/10.1021/acs.est.2c05373}{Moore et al.}
#'@param domain SpatRaster providing the desired output grid, including the
#'  desired resolution and coordinate reference system
#'@param state_name_list Character vector listing all states within the desired
#'  domain
#'@param output_directory Character providing the full filepath to save
#'  processed data
#'@param inventory_year Numeric indicating the desired year of data to use.
#'@param verbose Logical indicating whether to save additional output.  This
#'  includes 4 csv files, 2 providing summarized and 2 providing detailed
#'  information for all landfills within the domain, separated between LMOP and
#'  GHGRP.  It also includes 2 plots of the gridded methane emissions on log
#'  scales, one for LMOP facilities and one for GHGRP facilities.
#'@param DMR_file Character providing the full filepath to the discharge
#'  monitoring report data available at
#'  \url{https://echo.epa.gov/trends/loading-tool/water-pollution-search}.  Set
#'  the industry type to Publicly Owned Treatment Works in the search tool, set
#'  the year, and select wastewater flow under the pollutant categories.  After
#'  searching, scroll to the bottom table where flow separated by facility is
#'  shown and select download all data.  This will produce a csv with 3 rows as
#'  header, columns as different variables and rows as different facilities. The
#'  variables Facility Name, Facility Latitude, Facility Longitude and Average
#'  Flow (MGD) are used.  There is an example file in the package's datasets
#'  folder that has been successfully used in this code available for reference.
#'@param CWNS_file Character providing the full filepath to the 2012 Clean
#'  Watershed Needs report data available at
#'  \url{https://www.epa.gov/cwns/clean-watersheds-needs-survey-cwns-2012-report-and-data}.
#'  Find the data download link near the bottom of the page.  This will download
#'  all data as an access database.  To convert to a useable excel file:
#' \itemize{
#'   \item Open mdb file in Microsoft Access
#'   \item Go to Create tab -> Query Wizard
#'   \item Select Simple Query Wizard
#'   \item Choose the first table you want (SUMMARY_FACILITY)
#'   \item Click the double right arrow to take all columns
#'   \item Repeat for other table (SUMMARY_FACILITY_FLOW)
#'   \item Click Finish
#'   \item In the left hand pane, make sure you have selected to view all Access objects
#'   \item Your query should be here at the bottom – right click on it and select to export to Excel (.xlsx)
#'   \item Note that Access seems to automatically save this query to the access file
#' }
#'  The resulting excel file should have a separate row for each facility and
#'  different columns for different variables.  FACILITY_NAME, LATITUDE,
#'  LONGITUDE, HORIZONTAL_COORDINATE_DATUM, and EXIST_MUNICIPAL are used.  There
#'  is an example file in the package's datasets folder that has been
#'  successfully used in this code available for reference.
#'@param Wastewater_Municipal_method Character.  Pulled from config file. Either
#'  "Moore_linear" or "GHGI" to indicate the desired method to estimate
#'  municipal wastewater emissions.
#'@param Wastewater_Municipal_file Character.  Pulled from config file.  Either
#'  "DMR" or "CWNS" to indicate the desired input data.
#'@param Wastewater_State_info Data frame with 4 columns and 1 row for each
#'  state.  Pulled from config file.  Column 1 is "State" and provides the state
#'  abbreviation.  Column 2 is Population and contains the state's current
#'  population.  Column 3 is "Septic_Fraction" and provides as a decimal the
#'  percent of people in that state with septic systems.  Column 4 is method and
#'  is either "scaled" or "reported" for each state.  This data frame is only
#'  used if calculating septic emissions using state-level data.  "scaled"
#'  states have 1990 septic fraction and are to be scaled to inventory_year
#'  using the national change in the septic fraction.  "reported" states have
#'  the septic fraction that is to be used.
#'@param National_wastewater_info Data frame with 2 columns and 2 rows.  Pulled
#'  from config file.  The first column is "Year" listing the year 1990 and the
#'  inventory_year.  The second column is "Septic_Fraction" and lists the
#'  national septic fraction for both years.  Used to scale states from
#'  Wastewater_State_info when calculating septic emissions using state-level
#'  data.
#'@param GHGI_national_wastewater_nonseptic Numeric.  Pulled from config file.
#'  Indicates the GHGI national non-septic wastewater methane emissions in
#'  kilotons per year.  This is in a table titled "Domestic Wastewater CH4
#'  Emissions from Septic and Centralized Systems".  This is the sum of all
#'  entries other than septic.  The GHGI is available at
#'  \url{https://www.epa.gov/ghgemissions/inventory-us-greenhouse-gas-emissions-and-sinks}.
#'@param GHGI_national_wastewater_septic Numeric.  Pulled from config file.
#'  Indicates the GHGI national septic wastewater methane emissions in kilotons
#'  per year.  This is in a table titled "Domestic Wastewater CH4 Emissions from
#'  Septic and Centralized Systems".  This is the septic entry.  The GHGI is
#'  available at
#'  \url{https://www.epa.gov/ghgemissions/inventory-us-greenhouse-gas-emissions-and-sinks}.
#'@param GHGI_septic_EF Numeric. Pulled from config file.  Indicates the
#'  emission factor used to calculate septic emissions in grams of methane per
#'  person per day.  This is in a table titled "Variables and Data Sources for
#'  CH4 Emisions from Septic Systems".
#'@param Total_national_open_or_low_int_area Numeric.  Pulled from config file.
#'  National total of developed open space and developed low intensity land
#'  cover from the national land cover database from Table 7 of Homer et al.
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
#'@param focus_city_tigerlines SpatVector.  United States Census Bureau county
#'  shapefile.  Available at
#'  \url{https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html}.
#'  Only relevant if a focus city was set in main and verbose=TRUE.
#'@returns Nothing is returned from the function, but the main outputs are 4
#'  netcdf files of the methane emissions from wastewater.  They are titled
#'  "Wastewater_ind.nc" for industrial wastewater,
#'  "Wastewater_X_Y_dom_central.nc" for municipal wastewater,
#'  "Wastewater_dom_septic_byZ.nc" for septic emissions.  X is the input data
#'  used for the wastewater treatment plants - CWNS or DMR, Y is the method of
#'  converting to emissions - GHGI for downscaling GHGI totals proportionally
#'  with flow or ML for applying the moore et al. log-linear relationship
#'  between flow and emissions, and Z is the approach used for septic emissions
#'  - bystate or national.
#'
#'  The csv "WWTP_septic_method_comparison.csv" is also saved and provides the
#'  population, septic fraction, method (scaled or reported) total emissions,
#'  total area, emissions per area, and the ratio between the methane emissions
#'  calculated using the bystate and national approach for each state
#'  separately.
#'
#'  If verbose is set to TRUE, then "WWTP_industrial.csv",
#'  "WWTP_industrial_all.csv", "WWTP_municipal.csv", and
#'  "WWTP_municipal_all.csv" are also saved.  The simpler csvs include only the
#'  name, location, and assigned emissions for facilities within the domain that
#'  were pulled from the corresponding input file.  The _all files include all
#'  variables that were in the corresponding input file for the same facilities.
#'@examples
#'library(terra)
#'grid_bbox=cbind(c(-76.65,-73.65),c(38.97,40.97))
#'grid_res=0.01
#'grid_crs="epsg:4326"
#'grid <- rast(nrows=diff(range(grid_bbox[,2]))/grid_res,
#'             ncols=diff(range(grid_bbox[,1]))/grid_res, xmin=min(grid_bbox[,1]),
#'             xmax=max(grid_bbox[,1]), ymin=min(grid_bbox[,2]), ymax=max(grid_bbox[,2]),
#'             crs=grid_crs)
#'
#' Urban_Tigerlines <- vect("~/../Desktop/Urban_Tigerlines/tl_2018_us_uac10.shp")
#' focus_city <- terra::subset(Urban_Tigerlines,Urban_Tigerlines$NAME10 %in% "Philadelphia, PA--NJ--DE--MD")
#'
#' Wastewater(DMR_file='~/../Desktop/DMR_2022_from_8_10_2023.csv',
#'            CWNS_file='~/../Desktop/CWNS_merged_data_2012.xlsx',
#'            output_directory="~/../Desktop/",
#'            Wastewater_Municipal_method="Moore_linear",
#'            Wastewater_Municipal_file="DMR",
#'            domain=grid,
#'            state_name_list=c("DE","MD","NJ","NY","PA"),
#'            inventory_year=2018,
#'            National_wastewater_info=data.frame("Year"=c(1990,2018),
#'                                                "Septic_Fraction"=c(0.241,0.171)),
#'            Wastewater_State_info=data.frame("State"=c("DE", "MD", "NJ", "NY", "PA"),
#'                                             "Population"=c(966985,6042153,8891730,19544098,12809107),
#'                                             "Septic_Fraction"=c(0.257,0.181,0.116,0.159,0.245),
#'                                             "Method"=c("scaled","scaled","scaled","reported","scaled")),
#'            GHGI_national_wastewater_nonseptic=246,
#'            GHGI_national_wastewater_septic=227,
#'            GHGI_septic_EF=10.7,
#'            Total_national_open_or_low_int_area=352032,
#'            verbose=TRUE,
#'            State_Tigerlines=vect("~/../Desktop/State_Tigerlines/tl_2018_us_state.shp"),
#'            County_Tigerlines=vect("~/../Desktop/County_Tigerlines/tl_2018_us_county.shp"),
#'            focus_city_tigerlines=focus_city,
#'            plot_directory="~/../Desktop/plots/")
#'@author Joe Pitt, \email{madeup@@wisc.edu}
#'@author Kris Hajny, \email{blank@@fake.edu}
#'@author Israel Lopez-Coto, \email{test@@test.edu}
#'@reference \href{https://doi.org/10.1016/j.isprsjprs.2020.02.019}{Homer et
#'  al.}
#'@reference \href{https://doi.org/10.1021/acs.est.2c05373}{Moore et al.}
#'@export




#convert ACES from hourly to annual and save an hourly timeseries for the city
#total


# Prepare_ACES_Vulcan <- function(ACES_year,
#                            input_directory){}

starttime <- Sys.time()
if(Use_ACES){
  cat("Downloading and preparing ACES: Prepare_ACES_Vulcan - this will take some time\n")
  ################################################################################
  #some initial variables that need defining
  
  #all sectors to be worked up
  sectors <- c("Air","Commercial","Elec","Industrial","Marine","Nonroad","Oilgas",
               "Onroad","Rail","Residential","Total") #all sectors
  # sectors <- c("Commercial","Elec","Industrial","Residential","Total") #only the sectors needed
  
  #all months for 1 year
  Months <- sprintf("%02d",1:12)
  
  #filenames on the server to download.  Should match exactly from DAAC.
  filenames <- paste0("aces_",rep(sectors,each=12),"_",ACES_year,Months,".nc4")
  
  #main URL to pull files.  See 
  #https://thredds.daac.ornl.gov/thredds/catalog/ornldaac/1943/catalog.html?dataset=1943/aces_Air_201201.nc4
  download_url <- "https://thredds.daac.ornl.gov/thredds/fileServer/ornldaac/1943/"
  
  #output directory and names
  destination_folder <- file.path(input_directory,"ACES")
  monthly_output_list <- paste0("ACES_monthly_",rep(sectors,each=12),"_",ACES_year,"_",Months,".nc")
  annual_output_list <- paste0("ACES_annual_",sectors,"_",ACES_year,".nc")
  
  #keep only those that haven't already been created (in case being rerun for any
  #reason)
  filenames <- filenames[!file.exists(file.path(input_directory,"ACES",monthly_output_list))]
  monthly_output_list <- monthly_output_list[!file.exists(file.path(input_directory,"ACES",monthly_output_list))]
  annual_output_list <- annual_output_list[!file.exists(file.path(input_directory,"ACES",annual_output_list))]
  
  ################################################################################
  #prep a template to work with
  
  dir.create(destination_folder,showWarnings = F)
  
  #copy pasted from an example ACES file
  ACES_crs <- "+proj=lcc +lat_0=40 +lon_0=-97 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
  Annual_ACES <- rast(nrows=2908,ncols=4634,xmin=-2300000,xmax=2334000,ymin=-1608000,
                      ymax=1300000,crs=ACES_crs,vals=0)
  
  Annual_ACES <- crop(Annual_ACES,ext(project(State_Tigerlines,crs(Annual_ACES)))*1.1)
  monthly_ACES <- Annual_ACES
  
  ################################################################################
  #First, download the ACES data, if not already downloaded
  
  #default is to timeout if a download takes more than a minute.  Set that to 1 hr
  #per file given these are large files (shouldn't take that long, but with a bad
  #internet connection...)
  default_timeout <- options("timeout")
  options(timeout=60*60)
  
  for(Sector_indx in 0:(length(sectors)-1)){
    for(File_indx in (1+12*Sector_indx):(12+12*Sector_indx)){
      cat("\rDownloading",filenames[File_indx],"which is ACES file number",File_indx,"of",length(filenames),"                ")
      #All 12 monthly files with hourly data for this sector
      aces_file <- paste0(destination_folder,"/",filenames[File_indx])
      download.file(url=paste0(download_url,filenames[File_indx]),
                    destfile <- aces_file,quiet=T,method="curl")
      
      #compared against simply using sum/mean; this was slightly faster (1.05 min vs
      #1.16 min using ACES total)
      # start=Sys.time()
      monthly_data <- rast(aces_file)
      for(hr_indx in 1:nlyr(monthly_data)){
        monthly_ACES <- monthly_ACES+crop(monthly_data[[hr_indx]],Annual_ACES)
      }
      Annual_ACES <- Annual_ACES+monthly_ACES
      monthly_ACES <- monthly_ACES/nlyr(monthly_data)
      # cat("longcode = ",Sys.time() - start)
      
      # start=Sys.time()
      # monthly_data <- rast(aces_file)
      # monthly_data <- crop(monthly_data,Annual_ACES)
      # monthly_data <- mean(monthly_data)
      # cat("shortcode = ",Sys.time() - start)
      
      writeCDF(monthly_ACES,
               file.path(destination_folder,monthly_output_list[File_indx]),
               force_v4=TRUE,
               varname="flux_co2",
               unit="kg km-2 hr-1",
               longname=paste0(sectors[Sector_indx+1],"_sector_annual_average_combustion_CO2_emissions"),
               missval=-9999,
               overwrite=TRUE)
      values(monthly_ACES) <- 0
      
      unlink(aces_file)
    }
    Annual_ACES <- Annual_ACES/8760
    
    writeCDF(Annual_ACES,
             file.path(destination_folder,annual_output_list[Sector_indx+1]),
             force_v4=TRUE,
             varname="flux_co2",
             unit="kg km-2 hr-1",
             longname=paste0(sectors[Sector_indx+1],"_sector_annual_average_combustion_CO2_emissions"),
             missval=-9999,
             overwrite=TRUE)
    
    values(Annual_ACES) <- 0
    
  }
  cat("Finished preparing ACES data at",difftime(Sys.time(),starttime,units = "min"),"minutes since start\n")
}
################################################################################
#Repeat the process, but for Vulcan

if(Use_Vulcan){
  cat("Downloading and preparing Vulcan: Prepare_ACES_Vulcan - this will take some time\n")
  ################################################################################
  #some initial variables that need defining
  
  vulcan_year <- (2010:2015)[vulcan_band]
  
  #all sectors to be worked up
  sectors <- c("airport","cement","cmv","commercial","elec_prod","industrial","nonroad",
               "onroad","rail","residential","total") #all sectors
  # sectors <- c("commercial","elec_prod","industrial","residential","total") #only the sectors needed

  #filenames on the server to download.  Should match exactly from DAAC.
  filenames <- paste0("Vulcan.v3.US.hourly.1km.",rep(sectors,each=365),".mn.",vulcan_year,".d",sprintf("%03d",1:365),".nc4")
  
  #main URL to pull files.  See 
  #https://thredds.daac.ornl.gov/thredds/fileServer/ornldaac/1810/Contiguous_US/airport.2013.hourly_UTC/Vulcan.v3.US.hourly.1km.airport.mn.2013.d007.nc4
  sectoral_folder <- paste0(sectors,".",vulcan_year,".hourly_UTC/")
  download_url <- "https://thredds.daac.ornl.gov/thredds/fileServer/ornldaac/1810/Contiguous_US/"
  
  #output directory and names
  destination_folder <- file.path(input_directory,"Vulcan")
  monthly_output_list <- paste0("Vulcan_monthly_",rep(sectors,each=12),"_",vulcan_year,"_",1:12,".nc")
  annual_output_list <- paste0("Vulcan_annual_",sectors,"_",vulcan_year,".nc")
  
  #keep only those that haven't already been created (in case being rerun for any
  #reason)
  filenames <- filenames[!file.exists(file.path(input_directory,"Vulcan",monthly_output_list))]
  monthly_output_list <- monthly_output_list[!file.exists(file.path(input_directory,"Vulcan",monthly_output_list))]
  annual_output_list <- annual_output_list[!file.exists(file.path(input_directory,"Vulcan",annual_output_list))]
  
  #based on https://stackoverflow.com/a/6244503
  calculate_days_in_month <- function(x){
    as.numeric(as.Date(cut(x+34, "month")) - as.Date(cut(x, "month")))
  }
  days_per_month <- calculate_days_in_month(as.Date(paste0(vulcan_year,"-",1:12,"-01")))
  cumulative_days_per_month <- diffinv(days_per_month)
  ################################################################################
  #prep a template to work with
  
  dir.create(destination_folder,showWarnings = F)
  
  #copy pasted from an example Vulcan file.  Same CRS as aces, slightly
  #different extent
  Vulcan_crs <- "+proj=lcc +lat_0=40 +lon_0=-97 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
  Annual_Vulcan <- rast(nrows=2900,ncols=4648,xmin=-2305363,xmax=2342637,ymin=-1624104,
                      ymax=1275896,crs=Vulcan_crs,vals=0)
  Annual_Vulcan <- crop(Annual_Vulcan,ext(project(State_Tigerlines,crs(Annual_Vulcan)))*1.1)
  monthly_Vulcan <- Annual_Vulcan
  
  for(Sector_indx in 0:(length(sectors)-1)){
    for(monthly_indx in 1:12){
      for(File_indx in (cumulative_days_per_month[monthly_indx]+1+365*Sector_indx):(cumulative_days_per_month[monthly_indx+1]+365*Sector_indx)){
        cat("\rDownloading",filenames[File_indx],"which is Vulcan file number",File_indx,"of",length(filenames),"                ")
        #All 12 monthly files with hourly data for this sector
        vulcan_file <- paste0(destination_folder,"/",filenames[File_indx])
        download.file(url=paste0(download_url,sectoral_folder[Sector_indx+1],filenames[File_indx]),
                      destfile <- vulcan_file,quiet=T,method="curl")
        
        #compared against simply using sum/mean; this was slightly faster (1.05 min vs
        #1.16 min using Vulcan total)
        # start=Sys.time()
        daily_data <- rast(vulcan_file)
        for(hr_indx in 1:nlyr(daily_data)){
          monthly_Vulcan <- monthly_Vulcan+crop(daily_data[[hr_indx]],Annual_Vulcan)
        }
        unlink(vulcan_file)
      }
      Annual_Vulcan <- Annual_Vulcan+monthly_Vulcan
      monthly_Vulcan <- monthly_Vulcan/(days_per_month[monthly_indx]*24)
      # cat("longcode = ",Sys.time() - start)
      
      # start=Sys.time()
      # monthly_data <- rast(Vulcan_file)
      # monthly_data <- crop(monthly_data,Annual_Vulcan)
      # monthly_data <- mean(monthly_data)
      # cat("shortcode = ",Sys.time() - start)
      
      writeCDF(monthly_Vulcan,
               file.path(destination_folder,monthly_output_list[Sector_indx*12+monthly_indx]),
               force_v4=TRUE,
               varname="flux_co2",
               unit="Mg km-2 hr-1",
               longname=paste0(sectors[Sector_indx+1],"_sector_annual_average_combustion_CO2_emissions"),
               missval=-9999,
               overwrite=TRUE)
      values(monthly_Vulcan) <- 0
    }
    Annual_Vulcan <- Annual_Vulcan/8760
    
    writeCDF(Annual_Vulcan,
             file.path(destination_folder,annual_output_list[Sector_indx+1]),
             force_v4=TRUE,
             varname="flux_co2",
             unit="Mg km-2 hr-1",
             longname=paste0(sectors[Sector_indx+1],"_sector_annual_average_combustion_CO2_emissions"),
             missval=-9999,
             overwrite=TRUE)
    values(Annual_Vulcan) <- 0
  }
  cat("Finished preparing Vulcan data at",difftime(Sys.time(),starttime,units = "min"),"minutes since start\n")
}
options(timeout=default_timeout)


