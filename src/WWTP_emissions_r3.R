#'@title Create gridded methane emissions maps for wastewater treatment plants
#'  and septic systems
#'
#'@description `Wastewater` writes up to 7 netcdf files of gridded methane
#'  emissions - 1 - 4  for municipal wastewater treatment facilities, 1 for
#'  industrial wastewater treatment facilities, and 1 - 2 for septic systems.
#'  Also writes up to 11 csvs, up to 8 for municipal facilities, 2 for
#'  industrial facilities, and 1 for septic systems with all csvs being
#'  optional.
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
#'  the facilities and their data can differ between these products.  Facility
#'  locations and total effluent flow is used from these files.  The calculation
#'  method can then either be set to disaggregate the EPA Greenhouse Gas
#'  Inventory (GHGI) national total emissions to these facilities, scaled by
#'  flow, or calculate emissions for each facility using a log-linear
#'  relationship between flow and emissions (log10(CH4 emissions in g/s) =
#'  1.2*log10(flow in m3/s)+1) as determined by Moore et al.  Moore et al.
#'  measured emission rates at 63 wastewater treatment facilities to develop
#'  this relationship, which suggests nearly 2x the emissions as the GHGI using
#'  DMR flow rates as of 2023 when their paper was written.  Note the authors
#'  actually used emission factors with organic loading rather than this
#'  log-linear relationship to scale emissions.
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
#'  using national emissions.  Some states report the fraction of people in the
#'  state using septic systems to the American Housing Survey in the Plumbing,
#'  Water, and Sewage Disposal survey, available here
#'  \url{https://www.census.gov/programs-surveys/ahs/data/interactive/ahstablecreator.html?s_areas=00000&s_year=2021&s_tablename=TABLE1&s_bygroup1=1&s_bygroup2=1&s_filtergroup1=1&s_filtergroup2=1}.
#'  Note this data is reported every other year, not annually. For states that
#'  don't, the 1990 census data on septic fraction is available here
#'  \url{https://www.census.gov/data/tables/time-series/dec/coh-sewage.html} and
#'  this will be scaled using the change in the national septic fraction from
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
#'@param domain SpatVector polygon outlining the desired output area
#'@param domain_template SpatRaster providing the desired output grid, including
#'  the desired resolution and coordinate reference system
#'@param ghgrp_facility_info Data.frame with the GHGRP location data for all
#'  years and states.  See
#'  https://www.epa.gov/enviro/envirofacts-data-service-api
#'@param input_directory Character providing the full filepath to save/load
#'  raw input data
#'@param output_directory Character providing the full filepath to save
#'  processed data
#'@param inventory_year Numeric indicating the desired year of data to use.
#'@param verbose Logical indicating whether to save additional output.  This
#'  includes up to 11 csv files including summarized and detailed information
#'  for all municipal wastewater treatment plants within the domain and
#'  industrial wastewater plants.  It also includes up to 5 plots of the gridded
#'  methane emissions.  Up to 2 for municipal wastewater treatment facilities,
#'  up to 2 for septic, and 1 for industrial wastewater treatment facilities
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
#'  Watershed Needs report data or the folder containing the 2022 data from the
#'  report -  available at
#'  \url{https://www.epa.gov/cwns/clean-watersheds-needs-survey-cwns-2012-report-and-data}
#'  and
#'  \url{https://www.epa.gov/cwns/clean-watersheds-needs-survey-cwns-2022-report-and-data},
#'  respectively. For 2012, find the data download link near the bottom of the
#'  page.  This will download all data as an access database.  To convert to a
#'  useable excel file:
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
#'
#'  For the 2022 data there is a link to the data dashboard which has a data
#'  download tab.  Download the data as CSVs.
#'@param Wastewater_use_CWNS Logical.  Pulled from config file.  Indicating
#'  whether or not to use the clean watershed needs survey to get the flow for
#'  municipal wastewater treatment plants.  Either CWNS or the DMR data must be
#'  used, though both can be.
#'@param Wastewater_use_DMR Logical.  Pulled from config file.  Indicating
#'  whether or not to use discharge monitoring report data to get the flow for
#'  municipal wastewater treatment plants.  Either CWNS or the DMR data must be
#'  used, though both can be.
#'@param Wastewater_Municipal_Method_Moore_linear Logical.  Pulled from config
#'  file.  Indicating whether or not to use the log-log linear relationship
#'  between flow and emissions determined in Moore et al.  Either moore linear,
#'  moore EF, or the GHGI must be used, though any combination can be.
#'@param Wastewater_Municipal_Method_Moore_EF Logical.  Pulled from config file.
#'  Indicating whether or not to use the emission factor to relate flow and
#'  emissions determined in Moore et al.  Either moore linear, moore EF, or the
#'  GHGI must be used, though any combination can be.
#'@param Wastewater_Municipal_Method_GHGI Logical.  Pulled from config file.
#'  Indicating whether or not to downscale the greenhouse gas inventory national
#'  total emissions using flow as the proxy.  Either moore linear, moore EF, or
#'  the GHGI must be used, though any combination can be.
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
#'@returns Nothing is returned from the function, but the main outputs are up to
#'  7 netcdf files of the methane emissions from wastewater.  They are titled
#'  "Wastewater_ind.nc" for industrial wastewater,
#'  "Wastewater_X_Y_dom_central.nc" for municipal wastewater,
#'  "Wastewater_dom_septic_Z.nc" for septic emissions.  X is the input data used
#'  for the wastewater treatment plants - CWNS or DMR, Y is the method of
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
#'  "WWTP_industrial_all.csv", "WWTP_X_Y_municipal.csv", and
#'  "WWTP_X_Y_municipal_all.csv" are also saved.  The simpler csvs include only
#'  the name, location, and assigned emissions for facilities within the domain
#'  that were pulled from the corresponding input file.  The _all files include
#'  all variables that were in the corresponding input file for the same
#'  facilities.
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
#'
#' Wastewater(DMR_file='~/../Desktop/in/DMR_2022_from_8_10_2023.csv',
#'            CWNS_file='~/../Desktop/in/CWNS_merged_data_2012.xlsx',
#'            input_directory="~/../Desktop/in/",
#'            output_directory="~/../Desktop/out/",
#'            Wastewater_use_CWNS=TRUE,
#'            Wastewater_use_DMR=TRUE,
#'            Wastewater_Municipal_Method_Moore_linear=TRUE,
#'            Wastewater_Municipal_Method_Moore_EF=FALSE,
#'            Wastewater_Municipal_Method_GHGI=TRUE,
#'            domain=grid_vect,
#'            domain_template=grid,
#'            ghgrp_facility_info="~/../Desktop/in/GHGRP/facility_info.csv",
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
#'            State_Tigerlines=vect("~/../Desktop/in/State_Tigerlines/tl_2018_us_state.shp"),
#'            County_Tigerlines=vect("~/../Desktop/in/County_Tigerlines/tl_2018_us_county.shp"),
#'            plot_directory="~/../Desktop/plots/")
#'@author Joe Pitt, \email{madeup@@wisc.edu}
#'@author Kris Hajny, \email{blank@@fake.edu}
#'@author Israel Lopez-Coto, \email{test@@test.edu}
#'@reference \href{https://doi.org/10.1016/j.isprsjprs.2020.02.019}{Homer et
#'  al.}
#'@reference \href{https://doi.org/10.1021/acs.est.2c05373}{Moore et al.}
#'@export




## WWTP_emissions_r2.R
## In use: 2021-11-02 20:00
## Finalized: 2023-02-03
#
# Note - to convert to fluxes this code uses the raster packages area function.
# This simplifies the area calculation of a lat/long box and is not appropriate
# near the poles
#
#
# The GEPA does something slightly weird with these emissions - see WWTP_explainer.R for details
#
# The EPA national report essentially has 3 categories of WW emissions (2019 emissions from 2021 report in brackets):
# domestic septic systems (232 kt), domestic centralised systems (250 kt), and industrial emissions (254 kt)
#
# The CWNS has the locations of publicly owned WWTPs and other facilities - here's a quote from the proposal abstract:
# "The respondents who provide this information to EPA are state agencies responsible for environmental pollution control
# and local facility contacts who provide documentation to the states."
#
# So the domestic centralised emissions from the EPA national inventory report can be allocated using the CWNS using the reported
# existing municipal flow ("EXIST_MUNICIPAL") from the 2012 submission
#
# Some septic system population counts are present in the CWNS, but it isn't clear to me if this represents all the septic
# systems or just ones that are managed by local facilities. If I want to use this, I'll also have to work out how to
# spatially allocate these emissions based on the reporting local facility
# For now, distribute septic emissions according to the NLCD land cover classes "Developed-Open Space" and "Developed-Low Intensity"
# Method 1:
# We know the combined national total (in 2016) for these classes was 352032 km2, from this paper:
# https://doi.org/10.1016/j.isprsjprs.2020.02.019
# I made a version of the NLCD for our domain that had 1 for these classes and 0 for all other classes
# Then regridded this using xesmf to get the fraction of each grid cell that was one of these classes
# Load that in here, and calculate the area of each grid cell as a fraction of the national total
# Method 2:
# Do the same as Method 1, but using estimated  state-total emissions instead of national emissions
# These are calculated by multiplying an estimate for the number of people whose
# waste is treated in onsite systems within each state by the emission factor (10.7 gCH4/capita/day) from the EPA
# GHGI.  To estimate the number of people served by onsite systems, multiply the US census state
# population estimate for 2019 by an estimate of the fraction of people served by onsite systems.
# For some states, this septic fraction estimate can be taken from the 2021 American Housing Survey.
# Such recent data is not available for most states.  In those cases we take the
# septic fraction reported in the 1990 US census (the last to provide this data at the individual state level).
# To correct for recent changes in septic fraction, we multiply these state-level values from 1990 by the ratio of
# whole-US septic fraction in 2019 (16.3%; from the American Housing Survey) to whole-US septic fraction in 1990 (24.1%). 
#
# The EPA industrial WW emissions come mainly from meat & poultry
# These industries have their on-site treatment systems, and so are not included in the CWNS
# Some of these emissions are reported to GHGRP - we can use those here
# Emissions from non-reporters in this category are not currently included in this inventory
#
# Note that actually some of the emissions in the EPA inventory (15% domestic, 7% industrial) come from effluent, not treatment
# These may not be located entirely at the WWTPs, but it's really hard to know where to put them otherwise
#

Wastewater <- function(input_directory,
                       DMR_file,
                       CWNS_file,
                       output_directory,
                       Wastewater_use_CWNS,
                       Wastewater_use_DMR,
                       Wastewater_Municipal_Method_Moore_linear,
                       Wastewater_Municipal_Method_Moore_EF,
                       Wastewater_Municipal_Method_GHGI,
                       domain,
                       domain_template,
                       ghgrp_facility_info,
                       inventory_year,
                       National_wastewater_info,
                       Wastewater_State_info,
                       GHGI_national_wastewater_nonseptic,
                       GHGI_national_wastewater_septic,
                       GHGI_septic_EF,
                       Total_national_open_or_low_int_area,
                       State_Tigerlines,
                       County_Tigerlines,
                       plot_directory,
                       verbose){
  
  
  starttime <- Sys.time()
  cat("Starting wastewater sector: Wastewater\n")
  
  Wastewater_partial_output_directory <- paste0(output_directory,"Wastewater/processed_NLCD_data/")
  
  Wastewater_output_directory <- paste0(output_directory,"Wastewater/")
  dir.create(Wastewater_output_directory,showWarnings = F)
  ################################################################################
  # First load in and prep the flow data
  
  if(Wastewater_use_CWNS){
    #if the CWNS file is 1 file
    if(file_test("-f",CWNS_file)){
      cwns_2012 <- read_xlsx(CWNS_file)
      
      #ID any that are in the western or southern hemisphere (- coordinates)
      Western_hemis <- grep("W",cwns_2012$LONGITUDE)
      Southern_hemis <- grep("S",cwns_2012$LATITUDE)
      
      #remove the hemisphere text so we can make numeric
      cwns_2012$LATITUDE <- gsub("N|S","",cwns_2012$LATITUDE)
      cwns_2012$LONGITUDE <- gsub("W|E","",cwns_2012$LONGITUDE)
      cwns_2012$LATITUDE <- as.numeric(cwns_2012$LATITUDE)
      cwns_2012$LONGITUDE <- as.numeric(cwns_2012$LONGITUDE)
      
      #make those in the S or W hemispheres the appropriate negative coordinates
      cwns_2012$LATITUDE[Southern_hemis] <- cwns_2012$LATITUDE[Southern_hemis]*-1
      cwns_2012$LONGITUDE[Western_hemis] <- cwns_2012$LONGITUDE[Western_hemis]*-1
      
      #Pick only those entries that have lat and lon coordinates
      cwns_2012_filt <- subset(cwns_2012,!is.na(LATITUDE) & !is.na(LONGITUDE))
      
      # Nearly all the entries are NAD83, but some aren't
      # Convert everything over to WGS84
      # Assume blank or unknown entries are NAD83
      cwns_2012_wgs84 <- subset(cwns_2012_filt, HORIZONTAL_COORDINATE_DATUM=="World Geodetic System of 1984")
      cwns_2012_nad27 <- subset(cwns_2012_filt, HORIZONTAL_COORDINATE_DATUM=="North American Datum of 1927")
      cwns_2012_nad83 <- subset(cwns_2012_filt, HORIZONTAL_COORDINATE_DATUM!="North American Datum of 1927" & HORIZONTAL_COORDINATE_DATUM!="World Geodetic System of 1984")
      
      cwns_2012_wgs84 <- vect(cwns_2012_wgs84,geom=c("LONGITUDE","LATITUDE"))
      cwns_2012_nad27 <- vect(cwns_2012_nad27,geom=c("LONGITUDE","LATITUDE"))
      cwns_2012_nad83 <- vect(cwns_2012_nad83,geom=c("LONGITUDE","LATITUDE"))
      
      crs(cwns_2012_wgs84) <- "EPSG:4326"  # WGS84
      crs(cwns_2012_nad27) <- "EPSG:4267"  # NAD27
      crs(cwns_2012_nad83) <- "EPSG:4269"  # NAD83
      
      cwns_2012_nad27_trans <- project(cwns_2012_nad27,crs(cwns_2012_wgs84))
      cwns_2012_nad83_trans <- project(cwns_2012_nad83,crs(cwns_2012_wgs84))
      
      CWNS_Municipal_flow <- rbind(cwns_2012_wgs84,cwns_2012_nad27_trans,cwns_2012_nad83_trans)
      
      CWNS_tot_flow <- sum(CWNS_Municipal_flow$EXIST_MUNICIPAL, na.rm=T)
      
      #if the CWNS file is a directory
    }else if(file_test("-d",CWNS_file)){
      Location <- read.csv(file.path(CWNS_file,"PHYSICAL_LOCATION.csv"))
      Flow <- read.csv(file.path(CWNS_file,"FLOW.csv"))
      Facilities <- read.csv(file.path(CWNS_file,"FACILITIES.csv"))
      
      #filter to only municipal facilities
      Flow <- Flow[Flow$FLOW_TYPE=="Municipal Flow",]
      Location <- Location[Location$CWNS_ID %in% Flow$CWNS_ID,]
      Facilities <- Facilities[Facilities$CWNS_ID %in% Flow$CWNS_ID,]
      
      #combine the relevant data from the 3 files (equivalent to merge by ID,
      #then subsetting columns)
      CWNS_Municipal_flow <- Location
      CWNS_Municipal_flow$EXIST_MUNICIPAL <- Flow$CURRENT_DESIGN_FLOW[match(Location$CWNS_ID,Flow$CWNS_ID)]
      CWNS_Municipal_flow$facility_name <- Facilities$FACILITY_NAME[match(Location$CWNS_ID,Facilities$CWNS_ID)]
      CWNS_Municipal_flow <- vect(CWNS_Municipal_flow,geom=c("LONGITUDE","LATITUDE"),crs="EPSG:4269") # NAD83
      
      CWNS_tot_flow <- sum(CWNS_Municipal_flow$EXIST_MUNICIPAL, na.rm=T)
    }
  }
  
  
  if(Wastewater_use_DMR){
    DMR_data <- read.csv(DMR_file,skip=3)
    
    #replace periods with underscores in naming for consistency/ease
    colnames(DMR_data) <- gsub("\\.","\\_",colnames(DMR_data))
    
    #remove those without location data and vect as lat/long assuming WGS
    #(didn't see one explicitly mentioned, little impact on location)
    DMR_Municipal_flow <- subset(DMR_data,!is.na(Facility_Latitude) & !is.na(Facility_Longitude))
    DMR_Municipal_flow <- vect(DMR_Municipal_flow,geom=c("Facility_Longitude","Facility_Latitude"))
    crs(DMR_Municipal_flow) <- "EPSG:4326"
    
    DMR_tot_flow <- sum(DMR_data$Average_Daily_Flow__MGD_, na.rm=T)
  }
  
  # Take total emissions for each category from the 2021 EPA report (values for 2019 in kt)
  central_EPA_emiss <- GHGI_national_wastewater_nonseptic*1e9/(16.043*365*24*60*60)   #kt/y to mol/s
  
  cat("Finished loading in municipal treatment plant data at",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes since start\n")
  ################################################################################
  #write a small helper function.  Creates a raster of emissions, cropped to the
  #domain, converted to proper units, and saved as 2 csvs.
  rasterize_plus <- function(input,outputname){
    #project and crop to the domain, remove NAs
    input_crop <- project(input,crs(domain))
    input_crop <- crop(input_crop,domain)
    input_crop_filt <- subset(input_crop,!is.na(input_crop$emiss))
    
    #if there's at least 1 facility, rasterize it (mol/s)
    if(nrow(input_crop_filt)>0){
      rast <- rasterize(input_crop_filt, domain_template, "emiss", fun=sum)
    }else{
      rast <- domain_template
    }
    
    # Calculate flux in nmol/m2/s
    rast_flux <- rast*1e9/(cellSize(rast,unit="m"))  
    rast_flux[is.na(rast_flux)]<-0
    
    #mask now - just to NA data outside non-square domains.  No need to
    #partially weight pixels by fractional coverage, cropped as points so only
    #those within the domain have been rasterized.
    rast_flux <- mask(rast_flux,domain)
    
    #save csvs with the facility info
    if(verbose){
      if(nrow(input_crop_filt)>0){
        input_crop_filt_df <- as.data.frame(input_crop_filt)
        
        #DMR and CWNS have different capitalizations, but same name - id column
        name_index <- grep("facility_name",colnames(input_crop_filt_df),ignore.case = T)
        
        #a small csv with only the relevant columns and another with all columns
        write.csv(input_crop_filt, paste0(Wastewater_output_directory,'/',outputname,"_all.csv"),row.names = F)
        output <- data.frame(input_crop_filt_df[name_index],
                             geom(input_crop_filt)[,"x"],geom(input_crop_filt)[,"y"],
                             input_crop_filt_df$emiss)
        colnames(output) <- c('Site_Name','Longitude','Latitude','Emission_mol_per_s')
        write.csv(output,paste0(Wastewater_output_directory,'/',outputname,'.csv'),row.names=FALSE)
      }
    }
    
    #save the raster to the active R environment
    assign(x = outputname,rast_flux,envir = parent.env(environment()))
  }
  
  ################################################################################
  #distribute EPA Municipal Wastewater Treatment Plant emissions from the GHGI
  #using the CWNS or DMR municipal flow as the proxy
  
  if(Wastewater_Municipal_Method_GHGI){
    if(Wastewater_use_CWNS){
      CWNS_Municipal_flow$emiss <- central_EPA_emiss*CWNS_Municipal_flow$EXIST_MUNICIPAL/CWNS_tot_flow   # in mol/s
      rasterize_plus(CWNS_Municipal_flow,"WWTP_CWNS_GHGI_municipal")
    }
    if(Wastewater_use_DMR){
      DMR_Municipal_flow$emiss <- central_EPA_emiss*DMR_Municipal_flow$Average_Daily_Flow__MGD_/DMR_tot_flow   # in mol/s
      rasterize_plus(DMR_Municipal_flow,"WWTP_DMR_GHGI_municipal")
    }
  }
  ################################################################################
  #Instead calculate Municipal Wastewater Treatment Plant emissions using the
  #moore et al. log-linear relationship
  
  if(Wastewater_Municipal_Method_Moore_linear){
    if(Wastewater_use_CWNS){
      #convert from million gallons/day to m3/s
      CWNS_Municipal_flow$EXIST_MUNICIPAL <- CWNS_Municipal_flow$EXIST_MUNICIPAL*3785.41178/(24*60*60)  
      #Apply the log-log linear relationship from Figure 2A of Moore et al.
      CWNS_Municipal_flow$emiss <- 1.2*log10(CWNS_Municipal_flow$EXIST_MUNICIPAL)+1
      #convert from log10(g/s) to mol/s
      CWNS_Municipal_flow$emiss <- (10^(CWNS_Municipal_flow$emiss))/(12.011+1.008*4)
      rasterize_plus(CWNS_Municipal_flow,"WWTP_CWNS_ML_municipal")
    }
    if(Wastewater_use_DMR){
      #convert from million gallons/day to m3/s
      DMR_Municipal_flow$Average_Daily_Flow__MGD_ <- DMR_Municipal_flow$Average_Daily_Flow__MGD_*3785.41178/(24*60*60)
      #Apply the log-log linear relationship from Figure 2A of Moore et al.
      DMR_Municipal_flow$emiss <- 1.2*log10(DMR_Municipal_flow$Average_Daily_Flow__MGD_)+1
      #convert from log10(g/s) to mol/s
      DMR_Municipal_flow$emiss <- (10^(DMR_Municipal_flow$emiss))/(12.011+1.008*4)
      rasterize_plus(DMR_Municipal_flow,"WWTP_DMR_ML_municipal")
    }
  }
  
  ################################################################################
  #Instead calculate Municipal Wastewater Treatment Plant emissions using the
  #moore et al. emission factor
  
  if(Wastewater_Municipal_Method_Moore_EF){
    stop("Moore EF method for wastewater treatment plants has not yet been built out")
    # stop("CWNS data does not include the BOD, so the EF approach isn't an option")
    #small medium and large means (on a lognormal distribution).  Need to
    #understand the BOD5 data before I can use these EFs
    exp(-2.6+(1.3^2)/2)
    exp(-4.1+(1.2^2)/2)
    exp(-3.4+(1^2)/2)
  }
  cat("Finished calculating municipal treatment plant emissions at",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes since start\n")
  ################################################################################
  #Now septic systems.  First load in output from NLCD_fractions_by_state
  
  #output from NLCD_fractions_by_state - rasters have been reprojected
  Suburbia_rasterfile <- list.files(pattern=glob2rx("*NLCD_suburban.nc"),path=Wastewater_partial_output_directory,full.names = T)
  nlcd_state_total_areas <- read.table(file.path(Wastewater_partial_output_directory,"NLCD_state_total_areas.csv"),header=T,sep=",")
  
  #quickly ensure that the state data is all in the same order, alphabetical
  Suburbia_rasterfile <- sort(Suburbia_rasterfile)
  Wastewater_State_info <- Wastewater_State_info[order(Wastewater_State_info$State),]
  nlcd_state_total_areas <- nlcd_state_total_areas[order(nlcd_state_total_areas$X),]
  ################################################################################
  #Calculate septic emissions using emission factors and the land cover data
  #from NLCD_fractions_by_state.R
  
  septic_EPA_emiss <- GHGI_national_wastewater_septic*1e9/(16.043*365*24*60*60)   #kt/y to mol/s
  tot_nlcd_area <- Total_national_open_or_low_int_area  # all states in km2
  
  # blank output to combine the states
  septic_flux <- rast(Suburbia_rasterfile[1])
  septic_flux2 <- rast(Suburbia_rasterfile[1])
  values(septic_flux2) <- 0
  values(septic_flux) <- 0
  
  septic_flux_bystate <- vector()
  septic_flux2_bystate <- vector()
  
  for(A in 1:length(Suburbia_rasterfile)){
    #from NLCD_fraction_by_state.  The fractional coverage of NLCD open or low
    #intensity urban land cover per pixel.
    Suburbia <- rast(Suburbia_rasterfile[A])
    
    if(Wastewater_national_septic){
      #Calculate state-by-state totals by equally distributing GHGI totals
      #to developed, open and developed low intensity land cover nationally.
      state_flux <- septic_EPA_emiss*Suburbia/tot_nlcd_area  # in mol/s/km2
      
      #save within-domain state total emissions for csv later
      septic_flux_bystate <- c(septic_flux_bystate,
                               as.numeric(global(state_flux*cellSize(state_flux,unit="km"),sum,na.rm=T)))
      
      # Combine across states
      septic_flux <- sum(septic_flux,state_flux,na.rm=T)
    }
    
    
    if(Wastewater_state_septic){
      # Calculate state-by-state totals using state-specific septic fraction data
      Tot_area <- nlcd_state_total_areas[A,2] # total area of both classes in km2 from nlcd_state_total_areas.csv
      pop <- Wastewater_State_info[A,2]
      
      #Fraction that's septic as reported or calculating by scaling the 1990
      #state value by the change in national septic fraction since 1990
      if(Wastewater_State_info[A,4]=="scaled"){
        septic_frac <- Wastewater_State_info[A,3]*National_wastewater_info[2,2]/National_wastewater_info[1,2]
      }else if(Wastewater_State_info[A,4]=="reported"){
        septic_frac <- Wastewater_State_info[A,3]
      }
      
      state_tot_emiss <- pop*septic_frac*GHGI_septic_EF/(16.043*24*60*60)  #in mol/s (EF is in g/capita/day)
      state_flux <- state_tot_emiss*Suburbia/Tot_area #gridded and distributed equally across the state in mol/s/km2
      
      #save within-domain state total emissions for csv later
      septic_flux2_bystate <- c(septic_flux2_bystate,
                                as.numeric(global(state_flux*cellSize(state_flux,unit="km"),sum,na.rm=T)))
      
      # Combine across states
      septic_flux2 <- sum(septic_flux2,state_flux,na.rm=T)
    }
    cat("Finished processing septic for",Wastewater_State_info[A,1],"at",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes since start\n")
  }
  
  if(Wastewater_national_septic){
    # Now finalize units
    septic_flux <- septic_flux*1e9*1E-6  # Convert from mol/km2/s to nmol/m2/s
    septic_flux[is.na(septic_flux)]<-0
    
    #mask and account for pixels partially within the domain
    septic_flux <- mask(septic_flux,domain)
    cover <- extract(septic_flux,domain,weights=T,cells=T)
    septic_flux[cover[,'cell']] <- septic_flux[cover[,'cell']]*cover[,'weight']
  }
  
  if(Wastewater_state_septic){
    # Now finalize units
    septic_flux2 <- septic_flux2*1e9*1E-6  # Convert from mol/km2/s to nmol/m2/s
    septic_flux2[is.na(septic_flux2)]<-0
    
    #mask and account for pixels partially within the domain
    septic_flux2 <- mask(septic_flux2,domain)
    cover <- extract(septic_flux2,domain,weights=T,cells=T)
    septic_flux2[cover[,'cell']] <- septic_flux2[cover[,'cell']]*cover[,'weight']
  }
  
  if(Wastewater_state_septic & Wastewater_national_septic){
    #pull the state total emissions from both methods and a few additional
    #details so state totals can be easily compared.  Because of how NLCD
    #fractions by state works, this includes only the fraction of each state
    #within the domain.
    Wastewater_State_info$State_based_septic_emissions_mol_per_s <- septic_flux2_bystate
    Wastewater_State_info$National_based_septic_emissions_mol_per_s <- septic_flux_bystate
    Wastewater_State_info$total_septic_area_km2 <- nlcd_state_total_areas[,2]
    Wastewater_State_info$State_to_national_method_ratio <- Wastewater_State_info$State_based_septic_emissions_mol_per_s/Wastewater_State_info$National_based_septic_emissions_mol_per_s
    if(verbose){
      #now save the comparison across the methods
      write.csv(Wastewater_State_info, file.path(Wastewater_output_directory,"WWTP_septic_method_comparison.csv"),row.names = F)
    }
  }
  cat("Finished calculating septic emissions at",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes since start\n")
  ################################################################################
  #Download the relevant emissions data using the API
  #(https://www.epa.gov/enviro/envirofacts-data-service-api) and combine the
  #facility and emission data appropriately
  
  #download the relevant industrial wastewater - sector data
  #(https://www.epa.gov/enviro/greenhouse-gas-model).  
  ghgrp_wastewater_file <- file.path(input_directory,"GHGRP","industrial_wastewater_II.csv")
  if(!file.exists(ghgrp_wastewater_file)){
    data_URL <- "https://data.epa.gov/dmapservice/ghg.ii_subpart_level_information/csv"
    Trycatch_downloader(URL = data_URL,method = "save",output_location = ghgrp_wastewater_file,
                        error_message = paste0("Greenhouse Gas Reporting Program data could not be downloaded using API link: ",data_URL))
  }
  
  ghgrp_data <- read.csv(ghgrp_wastewater_file)
  #convert reporting_year to year
  colnames(ghgrp_data) <- gsub("reporting_","",colnames(ghgrp_data))
  ################################################################################
  #Merge with location-like data
  
  #combine the datasets by ID, and year
  ghgrp_all_data <- merge(ghgrp_facility_info,ghgrp_data,
                          by=c("facility_id","year"), all=F)
  
  #keep only data for the year of interest
  ghgrp <- ghgrp_all_data[ghgrp_all_data$year==inventory_year,]
  
  #convert the relevant columns to numeric class
  ghgrp[,c("latitude","longitude","ghg_quantity")] <- apply(ghgrp[,c("latitude","longitude","ghg_quantity")],
                                                            2,FUN = function(x){as.numeric(x)})
  ################################################################################
  # Now rasterize and save the data
  
  ghgrp <- vect(ghgrp,geom=c("longitude","latitude"))
  crs(ghgrp) <- "epsg:4326"
  ghgrp_crop <- project(ghgrp,domain)
  ghgrp_crop <- crop(ghgrp_crop,domain)
  ghgrp_crop$emiss <- ghgrp_crop$ghg_quantity*1e6/(16.043*365*24*60*60) #MT CH4/yr to mol/s
  
  # Now rasterise
  ghgrp_rast <- rasterize(ghgrp_crop, domain_template, "emiss", fun=sum)
  ghgrp_flux <- ghgrp_rast*1e9/(cellSize(ghgrp_rast,unit="m"))  # Calculate flux in nmol/m2/s
  ghgrp_flux[is.na(ghgrp_flux)]<-0
  
  #mask now - just to NA data outside non-square domains.  No need to
  #partially weight pixels by fractional coverage, cropped as points so only
  #those within the domain have been rasterized.
  ghgrp_flux <- mask(ghgrp_flux,domain)
  
  if(verbose){
    if(nrow(ghgrp_crop)>0){
      # Save point sources as csv files - first just the raw dataframe
      write.csv(ghgrp_crop, file.path(Wastewater_output_directory,"WWTP_industrial_all.csv"))
      
      # Now just the names, coordinates and emissions
      ghgrp_crop_output <- data.frame(ghgrp_crop$facility_name.x,
                                      geom(ghgrp_crop)[,"x"],geom(ghgrp_crop)[,"y"],
                                      ghgrp_crop$emiss)
      names(ghgrp_crop_output) <- c('Site_Name','Longitude','Latitude','Emission_mol_per_s')
      write.csv(ghgrp_crop_output, file.path(Wastewater_output_directory,"WWTP_industrial.csv"),row.names = F)
    }
  }
  cat("Finished calculating industrial treatment plant emissions at",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes since start\n")
  ################################################################################
  # Write the rasters
  
  if(Wastewater_Municipal_Method_GHGI){
    if(Wastewater_use_CWNS){
      writeCDF(WWTP_CWNS_GHGI_municipal,
               file.path(Wastewater_output_directory,'Wastewater_CWNS_GHGI_dom_central.nc'),
               force_v4=TRUE,
               varname='methane_emissions',
               unit='nmol/m2/s',
               longname='Methane emissions from treatment of domestic wastewater at centralised municipal treatment plants using the Clean Watershed Needs flow data to distribute GHGI emissions',
               missval=-9999,
               overwrite=TRUE)
    }
    if(Wastewater_use_DMR){
      writeCDF(WWTP_DMR_GHGI_municipal,
               file.path(Wastewater_output_directory,'Wastewater_DMR_GHGI_dom_central.nc'),
               force_v4=TRUE,
               varname='methane_emissions',
               unit='nmol/m2/s',
               longname='Methane emissions from treatment of domestic wastewater at centralised municipal treatment plants using the Discharge Monitoring Report flow data to distribute GHGI emissions',
               missval=-9999,
               overwrite=TRUE)
    }
  }
  if(Wastewater_Municipal_Method_Moore_linear){
    if(Wastewater_use_CWNS){
      writeCDF(WWTP_CWNS_ML_municipal,
               file.path(Wastewater_output_directory,'Wastewater_CWNS_ML_dom_central.nc'),
               force_v4=TRUE,
               varname='methane_emissions',
               unit='nmol/m2/s',
               longname='Methane emissions from treatment of domestic wastewater at centralised municipal treatment plants using the Clean Watershed Needs flow data in the Moore et al. log-linear relationship',
               missval=-9999,
               overwrite=TRUE)
    }
    if(Wastewater_use_DMR){
      writeCDF(WWTP_DMR_ML_municipal,
               file.path(Wastewater_output_directory,'Wastewater_DMR_ML_dom_central.nc'),
               force_v4=TRUE,
               varname='methane_emissions',
               unit='nmol/m2/s',
               longname='Methane emissions from treatment of domestic wastewater at centralised municipal treatment plants using the Discharge Monitoring Report flow data to distribute GHGI emissions',
               missval=-9999,
               overwrite=TRUE)
    }
  }
  if(Wastewater_national_septic){
    writeCDF(septic_flux,
             file.path(Wastewater_output_directory,'Wastewater_dom_septic_national.nc'),
             force_v4=TRUE,
             varname='methane_emissions',
             unit='nmol/m2/s',
             longname='Methane emissions from onsite treatment of domestic wastewater (e.g. septic tanks), based on EPA national values',
             missval=-9999,
             overwrite=TRUE)
  }
  if(Wastewater_state_septic){
    writeCDF(septic_flux2,
             file.path(Wastewater_output_directory,'Wastewater_dom_septic_bystate.nc'),
             force_v4=TRUE,
             varname='methane_emissions',
             unit='nmol/m2/s',
             longname='Methane emissions from onsite treatment of domestic wastewater (e.g. septic tanks), based on calculations at the state level',
             missval=-9999,
             overwrite=TRUE)
  }
  
  writeCDF(ghgrp_flux,
           file.path(Wastewater_output_directory,'Wastewater_ind.nc'),
           force_v4=TRUE,
           varname='methane_emissions',
           unit='nmol/m2/s',
           longname='Methane emissions from industrial wastewater treatment plants',
           missval=-9999,
           overwrite=TRUE)
  ################################################################################
  #Create a sector total, 1 per variant
  if(Wastewater_Municipal_Method_Moore_linear){
    WWTP_method <- "ML_municipal"
    WWTP_text <- "Moore et al. log-linear relationship combined with"
  }else if(Wastewater_Municipal_Method_GHGI){
    WWTP_method <- "GHGI_municipal"
    WWTP_text <- "GHGI total distributed using"
  }
  
  #just build all possible variations
  if(Wastewater_use_CWNS){
    if(Wastewater_state_septic){
      if(Wastewater_Municipal_Method_GHGI){
        Summed_wastewater_treatment_CWNS_GHGI_state = sum(WWTP_CWNS_GHGI_municipal,septic_flux2,ghgrp_flux,na.rm=T)
        writeCDF(Summed_wastewater_treatment_CWNS_GHGI_state,
                 file.path(output_directory,paste0('Wastewater_sector_total_CWNS_GHGI_state.nc')),
                 force_v4=TRUE,
                 varname='methane_emissions',
                 unit='nmol/m2/s',
                 longname='Methane emissions from municipal treatment plants, industrial treatment plants, and septic systems',
                 missval=-9999,
                 overwrite=TRUE)
      }
      if(Wastewater_Municipal_Method_Moore_linear){
        Summed_wastewater_treatment_CWNS_ML_state = sum(WWTP_CWNS_ML_municipal,septic_flux2,ghgrp_flux,na.rm=T)
        writeCDF(Summed_wastewater_treatment_CWNS_ML_state,
                 file.path(output_directory,paste0('Wastewater_sector_total_CWNS_ML_state.nc')),
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
        writeCDF(Summed_wastewater_treatment_CWNS_GHGI_national,
                 file.path(output_directory,paste0('Wastewater_sector_total_CWNS_GHGI_national.nc')),
                 force_v4=TRUE,
                 varname='methane_emissions',
                 unit='nmol/m2/s',
                 longname='Methane emissions from municipal treatment plants, industrial treatment plants, and septic systems',
                 missval=-9999,
                 overwrite=TRUE)
      }
      if(Wastewater_Municipal_Method_Moore_linear){
        Summed_wastewater_treatment_CWNS_ML_national = sum(WWTP_CWNS_ML_municipal,septic_flux,ghgrp_flux,na.rm=T)
        writeCDF(Summed_wastewater_treatment_CWNS_ML_national,
                 file.path(output_directory,paste0('Wastewater_sector_total_CWNS_ML_national.nc')),
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
        writeCDF(Summed_wastewater_treatment_DMR_GHGI_state,
                 file.path(output_directory,paste0('Wastewater_sector_total_DMR_GHGI_state.nc')),
                 force_v4=TRUE,
                 varname='methane_emissions',
                 unit='nmol/m2/s',
                 longname='Methane emissions from municipal treatment plants, industrial treatment plants, and septic systems',
                 missval=-9999,
                 overwrite=TRUE)
      }
      if(Wastewater_Municipal_Method_Moore_linear){
        Summed_wastewater_treatment_DMR_ML_state = sum(WWTP_DMR_ML_municipal,septic_flux2,ghgrp_flux,na.rm=T)
        writeCDF(Summed_wastewater_treatment_DMR_ML_state,
                 file.path(output_directory,paste0('Wastewater_sector_total_DMR_ML_state.nc')),
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
        writeCDF(Summed_wastewater_treatment_DMR_GHGI_national,
                 file.path(output_directory,paste0('Wastewater_sector_total_DMR_GHGI_national.nc')),
                 force_v4=TRUE,
                 varname='methane_emissions',
                 unit='nmol/m2/s',
                 longname='Methane emissions from municipal treatment plants, industrial treatment plants, and septic systems',
                 missval=-9999,
                 overwrite=TRUE)
      }
      if(Wastewater_Municipal_Method_Moore_linear){
        Summed_wastewater_treatment_DMR_ML_national = sum(WWTP_DMR_ML_municipal,septic_flux,ghgrp_flux,na.rm=T)
        writeCDF(Summed_wastewater_treatment_DMR_ML_national,
                 file.path(output_directory,paste0('Wastewater_sector_total_DMR_ML_national.nc')),
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
    if(Wastewater_state_septic){
      not_log_plot(septic_flux2,filename="Wastewater_dom_septic_bystate",
                   "Domestic Wastewater - Septic\n estimated state septic distributed using \ndeveloped open space/low intensity land cover",
                   as.numeric(global(min(septic_flux,septic_flux2,na.rm=T),min,na.rm=T)),
                   as.numeric(global(max(septic_flux,septic_flux2,na.rm=T),max,na.rm=T)),
                   plot_directory=plot_directory,
                   domain=domain,County_Tigerlines=County_Tigerlines,
                   State_Tigerlines=State_Tigerlines)
    }
    
    if(Wastewater_national_septic){
      not_log_plot(septic_flux,filename="Wastewater_dom_septic_national",
                   "Domestic Wastewater - Septic\n national EPA septic distributed using \ndeveloped open space/low intensity land cover",
                   as.numeric(global(min(septic_flux,septic_flux2,na.rm=T),min,na.rm=T)),
                   as.numeric(global(max(septic_flux,septic_flux2,na.rm=T),max,na.rm=T)),
                   plot_directory=plot_directory,
                   domain=domain,County_Tigerlines=County_Tigerlines,
                   State_Tigerlines=State_Tigerlines)
    }
    
    log_plot(ghgrp_flux,filename="Wastewater_ind",
             "Industrial Wastewater -\n GHGRP Reporters",
             plot_directory=plot_directory,
             domain=domain,County_Tigerlines=County_Tigerlines,
             State_Tigerlines=State_Tigerlines)
    
    
    
    #default min/max that should be overwritten using the data - to ensure
    #consistent axes across the 4 possible method combinations.
    WWTP_min=5000
    WWTP_max=0
    if(Wastewater_use_CWNS){
      if(Wastewater_Municipal_Method_Moore_linear){
        #set 0 to NA so the minimum ignores 0 (log plot, so 0 = -inf)
        temp <- WWTP_CWNS_ML_municipal
        temp[temp==0] <- NA
        if(!all(is.na(values(temp)))){
          WWTP_min <- min(WWTP_min,as.numeric(global(temp,min,na.rm=T)))
          WWTP_max <- max(WWTP_max,as.numeric(global(temp,max,na.rm=T)))
        }
      }
      if(Wastewater_Municipal_Method_GHGI){
        temp <- WWTP_CWNS_GHGI_municipal
        temp[temp==0] <- NA
        if(!all(is.na(values(temp)))){
          WWTP_min <- min(WWTP_min,as.numeric(global(temp,min,na.rm=T)))
          WWTP_max <- max(WWTP_max,as.numeric(global(temp,max,na.rm=T)))
        }
      }
    }
    if(Wastewater_use_DMR){
      if(Wastewater_Municipal_Method_Moore_linear){
        temp <- WWTP_DMR_ML_municipal
        temp[temp==0] <- NA
        if(!all(is.na(values(temp)))){
          WWTP_min <- min(WWTP_min,as.numeric(global(temp,min,na.rm=T)))
          WWTP_max <- max(WWTP_max,as.numeric(global(temp,max,na.rm=T)))
        }
      }
      if(Wastewater_Municipal_Method_GHGI){
        temp <- WWTP_DMR_GHGI_municipal
        temp[temp==0] <- NA
        if(!all(is.na(values(temp)))){
          WWTP_min <- min(WWTP_min,as.numeric(global(temp,min,na.rm=T)))
          WWTP_max <- max(WWTP_max,as.numeric(global(temp,max,na.rm=T)))
        }
      }
    }
    
    
    
    #actually do the plotting now, slight changes to filename and title for each
    WWTP_min <- log10(WWTP_min)
    WWTP_max <- log10(WWTP_max)
    if(Wastewater_use_CWNS){
      if(Wastewater_Municipal_Method_Moore_linear){
        log_plot(WWTP_CWNS_ML_municipal,filename="Wastewater_dom_central_CWNS_ML",
                 "Domestic Wastewater -\n Moore log-linear relationship combined with\nClean Watersheds Needs Survey",
                 WWTP_min,WWTP_max,plot_directory=plot_directory,
                 domain=domain,County_Tigerlines=County_Tigerlines,
                 State_Tigerlines=State_Tigerlines)
        #else if below as GHGI vs Moore can ~double emissions, which is
        #negligible on a log scale.
      }else if(Wastewater_Municipal_Method_GHGI){
        log_plot(WWTP_CWNS_GHGI_municipal,filename="Wastewater_dom_central_CWNS_GHGI",
                 "Domestic Wastewater -\n EPA total distributed using\nClean Watersheds Needs Survey",
                 WWTP_min,WWTP_max,plot_directory=plot_directory,
                 domain=domain,County_Tigerlines=County_Tigerlines,
                 State_Tigerlines=State_Tigerlines)
      }
    }
    if(Wastewater_use_DMR){
      if(Wastewater_Municipal_Method_Moore_linear){
        log_plot(WWTP_DMR_ML_municipal,filename="Wastewater_dom_central_DMR_ML",
                 "Domestic Wastewater -\n Moore log-linear relationship combined with\nDischarge Monitoring Reports",
                 WWTP_min,WWTP_max,plot_directory=plot_directory,
                 domain=domain,County_Tigerlines=County_Tigerlines,
                 State_Tigerlines=State_Tigerlines)
      }else if(Wastewater_Municipal_Method_GHGI){
        log_plot(WWTP_DMR_GHGI_municipal,filename="Wastewater_dom_central_DMR_GHGI",
                 "Domestic Wastewater -\n EPA total distributed using\nDischarge Monitoring Reports",
                 WWTP_min,WWTP_max,plot_directory=plot_directory,
                 domain=domain,County_Tigerlines=County_Tigerlines,
                 State_Tigerlines=State_Tigerlines)
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
    if(Wastewater_Municipal_Method_Moore_linear){
      WWTP_method <- "ML_municipal"
      WWTP_text <- "Moore et al. log-linear relationship combined with"
    }else if(Wastewater_Municipal_Method_GHGI){
      WWTP_method <- "GHGI_municipal"
      WWTP_text <- "GHGI total distributed using"
    }
    
    #just build all possible variations and calculate the max across all of them
    if(Wastewater_use_CWNS){
      if(Wastewater_state_septic){
        Summed_wastewater_treatment_CWNS_state = sum(get(paste0("WWTP_CWNS_",WWTP_method)),septic_flux2,ghgrp_flux,na.rm=T)
        CWNS_state_text=paste0(WWTP_text," CWNS")
        if(!all(is.na(values(Summed_wastewater_treatment_CWNS_state)))){
          WWTP_max <- max(WWTP_max,as.numeric(global(Summed_wastewater_treatment_CWNS_state,max,na.rm=T)))
        }
      }
      if(Wastewater_national_septic){
        Summed_wastewater_treatment_CWNS_national = sum(get(paste0("WWTP_CWNS_",WWTP_method)),septic_flux,ghgrp_flux,na.rm=T)
        CWNS_national_text=paste0(WWTP_text," CWNS")
        if(!all(is.na(values(Summed_wastewater_treatment_CWNS_national)))){
          WWTP_max <- max(WWTP_max,as.numeric(global(Summed_wastewater_treatment_CWNS_national,max,na.rm=T)))
        }
      }
    }
    if(Wastewater_use_DMR){
      if(Wastewater_state_septic){
        Summed_wastewater_treatment_DMR_state = sum(get(paste0("WWTP_DMR_",WWTP_method)),septic_flux2,ghgrp_flux,na.rm=T)
        DMR_state_text=paste0(WWTP_text," DMR")
        if(!all(is.na(values(Summed_wastewater_treatment_DMR_state)))){
          WWTP_max <- max(WWTP_max,as.numeric(global(Summed_wastewater_treatment_DMR_state,max,na.rm=T)))
        }
      }
      if(Wastewater_national_septic){
        Summed_wastewater_treatment_DMR_national = sum(get(paste0("WWTP_DMR_",WWTP_method)),septic_flux,ghgrp_flux,na.rm=T)
        DMR_national_text=paste0(WWTP_text," DMR")
        if(!all(is.na(values(Summed_wastewater_treatment_DMR_national)))){
          WWTP_max <- max(WWTP_max,as.numeric(global(Summed_wastewater_treatment_DMR_national,max,na.rm=T)))
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
                 State_Tigerlines=State_Tigerlines)
      }
      if(Wastewater_national_septic){
        log_plot(Summed_wastewater_treatment_CWNS_national,
                 paste0("Wastewater Treatment Sector\n",CWNS_national_text," (Domestic facilities)\nand GHGRP (industrial) and developed open space/low intensity NLCD\nland cover * national septic data (Septic)"),
                 WWTP_min,WWTP_max,plot_directory=plot_directory,
                 domain=domain,County_Tigerlines=County_Tigerlines,
                 State_Tigerlines=State_Tigerlines)
      }
    }
    if(Wastewater_use_DMR){
      if(Wastewater_state_septic){
        log_plot(Summed_wastewater_treatment_DMR_state,
                 paste0("Wastewater Treatment Sector\n",DMR_state_text," (Domestic facilities)\nand GHGRP (industrial) and developed open space/low intensity NLCD\nland cover * state septic data (Septic)"),
                 WWTP_min,WWTP_max,plot_directory=plot_directory,
                 domain=domain,County_Tigerlines=County_Tigerlines,
                 State_Tigerlines=State_Tigerlines)
      }
      if(Wastewater_national_septic){
        log_plot(Summed_wastewater_treatment_DMR_national,
                 paste0("Wastewater Treatment Sector\n",DMR_national_text," (Domestic facilities)\nand GHGRP (industrial) and developed open space/low intensity NLCD\nland cover * national septic data (Septic)"),
                 WWTP_min,WWTP_max,plot_directory=plot_directory,
                 domain=domain,County_Tigerlines=County_Tigerlines,
                 State_Tigerlines=State_Tigerlines)
      }
    }
  }
  cat("Finished wastewater sector: Wastewater in",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes\n\n")
}
