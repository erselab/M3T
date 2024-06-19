#'@title Create gridded stationary combustion methane emissions maps
#'
#'@description `Stationary_combustion` writes up to 56 netcdf files of gridded
#'  methane emissions from stationary combustion sources, as well as optional
#'  visuals
#'
#'@details This function calculates and grids methane emissions from stationary
#'  combustion. It uses the Energy Information Administration's (EIA) State
#'  Energy Data System (SEDS), Environmental Protection Agency's (EPA)
#'  Greenhouse Gas Inventory (GHGI), EPA National Emissions Inventory (NEI) and
#'  either the Vulcan or Anthropogenic Carbon Emission System (ACES) CO2
#'  inventory.  First, the ratio of SEDS national consumption by fuel-sector
#'  combination to the equivalent GHGI data is applied to SEDS state data so
#'  that they are approximately consistent with the GHGI.  The state total
#'  consumption is then converted to emissions using emission factors.  There
#'  are two variations at this step.
#'
#'  bystate: The state level emissions are then distributed to the county level
#'  using NEI CO concentrations to calculate weighting values for each county
#'  (i.e., county CO / state total CO).
#'
#'  bydomain: The state level emissions are summed to get a total for all states
#'  in the domain.  This is distributed to the county level using NEI CO
#'  concentrations to calculate weighting values for each county (i.e., county
#'  CO / domain total CO).
#'
#'  Lastly, county total emissions are distributed using the ACES or Vulcan CO2
#'  inventory.
#'
#'  This entire process is done separately for each fuel and sector using the
#'  GHGI, SEDS, and NEI data that are broken down by sector and fuel and CO2
#'  inventories that are broken down by sector.  Sectors considered are
#'  residential, commercial, industrial, and electric and the fuels considered
#'  are coal, natural gas, petroleum, and wood.  Residential coal is ignored as
#'  it does not exist in the U.S. anymore and residential gas is ignored as it
#'  is accounted for elsewhere.
#'
#'  The necessary SEDS data will be automatically downloaded.
#'
#'  The GHGI is intended to capture all national emissions.  The SEDS
#'  consumption data provides state-level consumption broken down by fuel (coal,
#'  natural gas, petroleum, wood, geothermal, solar, and electricity) and sector
#'  (residential, commercial, industrial, electric, transportation).  The NEI
#'  data being used provides CO emissions at the county level.  The GHGI is
#'  available starting in 1990 and is generally about 2 yearrs behind present
#'  day.  SEDS data is available starting in 1960 and generally is about 2 years
#'  behind present day.  NEI data is available beginning in at least 1990, is
#'  released every three years, and generally takes three years to complete
#'  (i.e., 2023 NEI is released in 2026).  All data is annual.  The SEDS data is
#'  at the state scale, NEI data is at the county scale, and GHGI data is at the
#'  national scale.
#'
#'  The GHGI is available at
#'  \url{https://www.epa.gov/ghgemissions/inventory-us-greenhouse-gas-emissions-and-sinks}
#'  The SEDS is available at
#'  \url{https://www.eia.gov/state/seds/seds-data-complete.php?sid=US} The NEI is
#'  available at
#'  \url{https://www.epa.gov/air-emissions-inventories/national-emissions-inventory-nei}
#'
#'  Each fuel-sector-inventory-variation combination is saved separately.
#'@param NEI_file Character providing the full filepath to the NEI county level
#'  CO data for the states within the domain. This data is available at
#'  \url{https://www.epa.gov/air-emissions-inventories/2017-national-emissions-inventory-nei-data}.
#'  At the bottom of the page there is a data query - to download the desired
#'  file the national/state/county value should be set to county, the geographic
#'  aggregation should be set to the states of interest (hold control and click
#'  to select multiple), and the CAP should be set to carbon monoxide.  The
#'  resulting excel file has columns for different variables and rows for
#'  different sector - county - fuel combinations.  The variables are State,
#'  State FIPS, County, Sector, County FIPS, Pollutant, Pollutant Type,
#'  Emissions, and Unit of Measure, though the county, pollutant, pollutant
#'  type, and unit of measure are unused.  There is an example file in the
#'  package's datasets folder that has been successfully used in this code
#'  available for reference.
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
#'@param Use_ACES Logical indicating whether or not to use ACES to disaggregate
#'  from county-level to pixel level emissions.  Either ACES or Vulcan must be
#'  used, though both can be.
#'@param Use_Vulcan Logical indicating whether or not to use Vulcan to
#'  disaggregate from county-level to pixel level emissions.  Either ACES or
#'  Vulcan must be used, though both can be.
#'@param ACES_directory Character providing the full path to a folder containing
#'  the ACES sectoral CO2 inventories.  Must include the residential,
#'  commercial, electric (elec), and industrial sectors.  ACES v2.0 is available
#'  at \url{https://doi.org/10.3334/ORNLDAAC/1943}, though the hourly file should be
#'  averaged across hours to create an annually averaged inventory.  Code to do
#'  this on a linux-based HPC system is available as the script
#'  "Annualize_ACES_seawulf.R" and the accompanying batch script
#'  "Annualize_ACES.sh".  The year closest to "inventory_year" is used, but
#'  those further from that year are considered if the closest is unavailable.
#'@param vulcan_directory Character providing the full path to a folder
#'  containing the Vulcan sectoral CO2 inventories.  Must include the
#'  residential, commercial, electric (elec_prod), and industrial sectors.
#'  Vulcan v3.0 is available at \url{https://doi.org/10.3334/ORNLDAAC/1741}, and the
#'  annual mean files should be used.  The year closest to "inventory_year" is
#'  used.  As all years are contained in the same file, it does not search for
#'  other years.
#'@param ACES_year Numeric providing the year of ACES data to use
#'@param vulcan_band Numeric providing the band of Vulcan data to use (1-6 =
#'  2010 - 2015)
#'@param stationary_combustion_by_state Logical. Pulled from config file.
#'  indicating whether state-toal emissions should be distributed to the county
#'  scale as is.  Either bystate or bydomain must be used, though both can be.
#'@param stationary_combustion_by_domain Logical. Pulled from config file.
#'  indicating whether state-toal emissions should be aggregated to the domain
#'  and then distributed to the county scale.  Either bystate or bydomain must
#'  be used, though both can be.
#'@param stationary_combustion_GHGI_data Data frame.  Pulled from config file. 1
#'  by 15 data frame with consumption for each sector-fuel combination from the
#'  GHGI. Although the data is national, there is a state entry set to US_EPA.
#'  Consumption is in thousands of British Thermal Units (BTU).  The GHGI is
#'  available at
#'  \url{https://www.epa.gov/ghgemissions/inventory-us-greenhouse-gas-emissions-and-sinks}
#'  and the values can be found in the Annexes in a table titled "Fuel
#'  Consumption by Stationary Combustion for Calculating CH4 and N2O Emissions
#'  (TBtu)".  Includes every combination of residential (res), commercial (com),
#'  electric (elec), and residential (res) sectors with coal, petroleum (petr),
#'  natural gas (gas), and wood fuels with names using the abbreviations in
#'  parentheses (e.g., com_coal, elec_petr).  Excludes res_coal and res_gas as
#'  residential coal does not exist in the U.S. anymore and residential gas is
#'  accounted for elsewhere.
#'@param stationary_combustion_emission_factors Data frame.  Pulled from config
#'  file.  1 by 14 data frame with emission factors for each sector-fuel
#'  combination from the IPCC.  Built equivalently to the GHGI_data, but without
#'  an entry for the state.  Emission factors are in g/GJ, equivalent to kg/TJ.
#'  Default emission factors are available in IPCC 2006 volume 2: Energy, tables
#'  2.2 through 2.5 \url{https://www.ipcc-nggip.iges.or.jp/public/2006gl/vol2.html}.
#'  The natural gas electric sector emission factor is instead pulled from Hajny
#'  et al., 2019 \url{https://doi.org/10.1021/acs.est.9b01875}.  This is 5.7 kg/TJ,
#'  within uncertainties of the GHGI value of 4.1 kg/TJ, both of which are
#'  larger than the IPCC default of 1 kg/TJ.
#'@param EIA_API_key Character.  Pulled from config file.  API key to access
#'  SEDS data API.  The API is described at \url{https://www.eia.gov/opendata/} and
#'  one can register for a key with a link on the right hand side of this page.
#'@returns Nothing is returned from the function, but the main outputs are up to
#'  56 netcdf files of the methane emissions from stationary combustion.  They
#'  are titled as "stat_comb_sector_fuel_variation_inventory.nc" where sector is
#'  abbreviated as com (commercial), res (residential), elec (electric), and ind
#'  (industrial); fuel is abbreviated as wood, petr (petroleum), gas (natural
#'  gas), and coal; and variation is bystate or bydomain.
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
#'@examples
#'library(terra)
#'user_key = "__user_EIA_API_key__"
#' grid_bbox=cbind(c(-76.65,-73.65),c(38.97,40.97))
#' grid_res=0.01
#' grid_crs="epsg:4326"
#' grid <- rast(nrows=diff(range(grid_bbox[,2]))/grid_res,
#'              ncols=diff(range(grid_bbox[,1]))/grid_res, xmin=min(grid_bbox[,1]),
#'              xmax=max(grid_bbox[,1]), ymin=min(grid_bbox[,2]), ymax=max(grid_bbox[,2]),
#'              crs=grid_crs)
#' Urban_Tigerlines <- vect("~/../Desktop/Urban_Tigerlines/tl_2018_us_uac10.shp")
#' focus_city <- terra::subset(Urban_Tigerlines,Urban_Tigerlines$NAME10 %in% "Philadelphia, PA--NJ--DE--MD")
#' Stationary_combustion(NEI_file="~/../Desktop/NEI_2017.xlsx",
#'                       domain=grid,
#'                       state_name_list=c("DE","MD","NJ","NY","PA"),
#'                       output_directory="~/../Desktop/",
#'                       inventory_year=2018,
#'                       verbose=TRUE,
#'                       Use_ACES=TRUE,
#'                       Use_Vulcan=TRUE,
#'                       ACES_directory="~/../Desktop/Inventories/ACES_v2.0",
#'                       vulcan_directory="~/../Desktop/Inventories/Vulcan_v3.0",
#'                       ACES_year=2017,
#'                       vulcan_band=6,
#'                       stationary_combustion_by_state=TRUE,
#'                       stationary_combustion_by_domain=TRUE,
#'                       stationary_combustion_GHGI_data=data.frame(
#'                         "State"="US_EPA",
#'                         "com_coal"=17,
#'                         "ind_coal"=517,
#'                         "elec_coal"=10554,
#'                         "res_petr"=975,
#'                         "com_petr"=801,
#'                         "ind_petr"=2062,
#'                         "elec_petr"=42,
#'                         "com_gas"=3647,
#'                         "ind_gas"=9484,
#'                         "elec_gas"=11553,
#'                         "res_wood"=544,
#'                         "com_wood"=84,
#'                         "ind_wood"=1407,
#'                         "elec_wood"=68),
#'                       stationary_combustion_emission_factors=data.frame(
#'                         "com_coal"=10,
#'                         "ind_coal"=10,
#'                         "elec_coal"=1,
#'                         "res_petr"=10,
#'                         "com_petr"=10,
#'                         "ind_petr"=3,
#'                         "elec_petr"=3,
#'                         "com_gas"=5,
#'                         "ind_gas"=1,
#'                         "elec_gas"=5.4/(1.0550559*0.9), #g/mmbtu to g/GJ and low to high heating value (0.9)
#'                         "res_wood"=300,
#'                         "com_wood"=300,
#'                         "ind_wood"=30,
#'                         "elec_wood"=30),
#'                       EIA_API_key=user_key,
#'                       State_Tigerlines=vect("~/../Desktop/State_Tigerlines/tl_2018_us_state.shp"),
#'                       County_Tigerlines=vect("~/../Desktop/County_Tigerlines/tl_2018_us_county.shp"),
#'                       focus_city_tigerlines=focus_city,
#'                       plot_directory="~/../Desktop/plots/")
#'@author Joe Pitt, \email{madeup@@wisc.edu}
#'@author Kris Hajny, \email{blank@@fake.edu}
#'@author Israel Lopez-Coto, \email{test@@test.edu}
#'@export


## stationary_combustion_r3.R
## In use: 2022-03-02 16:00
#
# Spatially allocate fuel- and sector-specific stationary combustion emissions
# The totals are calculated at the state level from EIA SEDS data (and the EPA national inventory - see note below).
# These are then spatially disaggregated to the county level according to the corresponding CO emissions from the 2017 NEI
# Within each county, emissions are spatially disaggregated according to ACES or Vulcan CO2 emissions

Stationary_combustion <- function(NEI_file,
                                  domain,
                                  state_name_list,
                                  output_directory,
                                  inventory_year,
                                  verbose,
                                  County_Tigerlines,
                                  Use_ACES,
                                  Use_Vulcan,
                                  ACES_directory,
                                  vulcan_directory,
                                  ACES_year,
                                  vulcan_band,
                                  stationary_combustion_by_state,
                                  stationary_combustion_by_domain,
                                  stationary_combustion_GHGI_data,
                                  stationary_combustion_emission_factors,
                                  EIA_API_key,
                                  plot_directory,
                                  State_Tigerlines,
                                  focus_city_tigerlines){
XESMF=F
  ################################################################################
  #Quit ASAP if neither ACES or Vulcan are set to be used.  Need one of them
  if(!(Use_Vulcan | Use_ACES)){
    stop("We need ACES, Vulcan or both to disaggregate emissions.")
  }
  if(!(stationary_combustion_by_domain | stationary_combustion_by_state)){
    stop("We need to disaggregate stationary combustion emissions by domain or by state, or both.")
  }
  
  ################################################################################
  #download and prepare SEDS data
  
  state_name_list <- c(state_name_list,"US")
  
  #see https://www.eia.gov/opendata/browser/seds
  SEDS_URL <- paste0("https://api.eia.gov/v2/seds/data/?frequency=annual&data[0]=value&facets[seriesId][]=CLCCB",
                     "&facets[seriesId][]=CLEIB&facets[seriesId][]=CLICB&facets[seriesId][]=NGCCB&facets[seriesId][]=NGEIB&facets[seriesId][]=NGICB&facets[seriesId][]=PACCB&facets[seriesId][]=PAEIB&facets[seriesId][]=PAICB&facets[seriesId][]=PARCB&facets[seriesId][]=WDRCB&facets[seriesId][]=WWCCB&facets[seriesId][]=WWEIB&facets[seriesId][]=WWICB",
                     paste0("&facets[stateId][]=",state_name_list,collapse = ""),
                     "&start=",inventory_year-1,"&end=",inventory_year,
                     "&sort[0][column]=seriesId&sort[0][direction]=asc&offset=0&api_key=",EIA_API_key)
  
  #download directly into R and keep only the data table
  EIA_raw_data <- fromJSON(SEDS_URL)
  EIA_raw_data <- EIA_raw_data$response$data
  
  #rearrange columns/rows to better mesh with EPA data
  EIA_data=(reshape(EIA_raw_data[,c("seriesId","stateId","value")],idvar="stateId",timevar = "seriesId",direction="wide"))
  
  #rename to be consistent with EPA (matches SEDS webpage)
  colnames(EIA_data) <- c("State","com_coal","elec_coal","ind_coal","com_gas",
                          "elec_gas","ind_gas","com_petr","elec_petr","ind_petr",
                          "res_petr","res_wood","com_wood","elec_wood","ind_wood")
  EIA_data$State <- gsub("US","US_SEDS",EIA_data$State)
  
  #make numeric rather than text, combine with EPA, and sort by state
  EIA_data[,-1] <- apply(EIA_data[,-1], 2, FUN=function(x){as.numeric(x)/1000})
  stat_comb_data <- rbind(EIA_data,stationary_combustion_GHGI_data)
  stat_comb_data <- stat_comb_data[order(stat_comb_data$State),]
  
  #round so that EPA and SEDS have the same precision (national) and the other
  #sector-states have the same precision as the webpage data
  stat_comb_data[stat_comb_data$State=="US_SEDS",-1] <- 
    round(stat_comb_data[stat_comb_data$State=="US_SEDS",-1])
  stat_comb_data[,-1] <- 
    round(stat_comb_data[,-1],1)
  
  rm(EIA_raw_data,EIA_data)
  ################################################################################
  #Prep EPA/SEDS data a little before use.  Scaling SEDS data to match national
  #data at the US scale.
  # Notes:
  # - there is no reported residential coal use in the US
  # - all post-meter residential NG emissions (leaks + combustion) are in a separate category, so are not considered here
  # - corrections need to be applied to avoid double counting emissions in multiple sectors. Assume the national factors apply here.
  
  # stat_comb_data <- stat_comb_data[,-which(colnames(stat_comb_data) %in% c("res_coal","res_gas"))]
  #we aren't using either of these in this code, so remove them now.  US has no
  #residential coal, and residential gas is dealt with in the NG distribution code
  
  ratio_df <- (stat_comb_data[which(stat_comb_data$State == 'US_EPA'), !(names(stat_comb_data) == 'State')]/
                 stat_comb_data[which(stat_comb_data$State == 'US_SEDS'), !(names(stat_comb_data) == 'State')])
  ratio_df <- ratio_df[rep(1, nrow(stat_comb_data)-2),]
  # Construct df containing repeats of the national EPA:SEDS ratio
  
  stat_comb_data_adj <- stat_comb_data[which(!(stat_comb_data$State %in% c('US_EPA', 'US_SEDS'))),]
  stat_comb_data_adj[!(names(stat_comb_data_adj) == 'State')] <- stat_comb_data_adj[!(names(stat_comb_data_adj) == 'State')]*ratio_df
  #now multiply all the original data by this ratio
  ################################################################################
  # Calculate CH4 emissions in mol/s using the emission factors:
  # - Conversion from higher heating value to lower heating value (0.9 or 0.95)
  # - Conversion from trillion Btu to GJ (1e9/947.8170777491506)
  # - Conversion from GJ to g/yr of CH4 (IPCC default values, except natural gas power plants from Hajny et al. doi: 10.1021/acs.est.9b01875)
  # - Conversion from g/yr to mol/s (1/(16.043*365*24*60*60))
  
  stat_comb_data_adj$com_coal_ER <- stat_comb_data_adj$com_coal*0.95*(1e9/947.8170777491506)*stationary_combustion_emission_factors$com_coal/(16.043*365*24*60*60)
  stat_comb_data_adj$ind_coal_ER <- stat_comb_data_adj$ind_coal*0.95*(1e9/947.8170777491506)*stationary_combustion_emission_factors$ind_coal/(16.043*365*24*60*60)
  stat_comb_data_adj$elec_coal_ER <- stat_comb_data_adj$elec_coal*0.95*(1e9/947.8170777491506)*stationary_combustion_emission_factors$elec_coal/(16.043*365*24*60*60)
  
  stat_comb_data_adj$res_petr_ER <- stat_comb_data_adj$res_petr*0.95*(1e9/947.8170777491506)*stationary_combustion_emission_factors$res_petr/(16.043*365*24*60*60)
  stat_comb_data_adj$com_petr_ER <- stat_comb_data_adj$com_petr*0.95*(1e9/947.8170777491506)*stationary_combustion_emission_factors$com_petr/(16.043*365*24*60*60)
  stat_comb_data_adj$ind_petr_ER <- stat_comb_data_adj$ind_petr*0.95*(1e9/947.8170777491506)*stationary_combustion_emission_factors$ind_petr/(16.043*365*24*60*60)
  stat_comb_data_adj$elec_petr_ER <- stat_comb_data_adj$elec_petr*0.95*(1e9/947.8170777491506)*stationary_combustion_emission_factors$elec_petr/(16.043*365*24*60*60)
  
  stat_comb_data_adj$com_gas_ER <- stat_comb_data_adj$com_gas*0.9*(1e9/947.8170777491506)*stationary_combustion_emission_factors$com_gas/(16.043*365*24*60*60)
  stat_comb_data_adj$ind_gas_ER <- stat_comb_data_adj$ind_gas*0.9*(1e9/947.8170777491506)*stationary_combustion_emission_factors$ind_gas/(16.043*365*24*60*60)
  stat_comb_data_adj$elec_gas_ER <- stat_comb_data_adj$elec_gas*0.9*(1e9/947.8170777491506)*stationary_combustion_emission_factors$elec_gas/(16.043*365*24*60*60)
  
  stat_comb_data_adj$res_wood_ER <- stat_comb_data_adj$res_wood*0.9*(1e9/947.8170777491506)*stationary_combustion_emission_factors$res_wood/(16.043*365*24*60*60)
  stat_comb_data_adj$com_wood_ER <- stat_comb_data_adj$com_wood*0.9*(1e9/947.8170777491506)*stationary_combustion_emission_factors$com_wood/(16.043*365*24*60*60)
  stat_comb_data_adj$ind_wood_ER <- stat_comb_data_adj$ind_wood*0.9*(1e9/947.8170777491506)*stationary_combustion_emission_factors$ind_wood/(16.043*365*24*60*60)
  stat_comb_data_adj$elec_wood_ER <- stat_comb_data_adj$elec_wood*0.9*(1e9/947.8170777491506)*stationary_combustion_emission_factors$elec_wood/(16.043*365*24*60*60)
  
  state_total_ch4 <- cbind(stat_comb_data_adj$State,
                           stack(stat_comb_data_adj[grepl('_ER', names(stat_comb_data_adj))]))
  names(state_total_ch4) <- c('State', 'state_ch4_emiss', 'Sector')
  # Stack these emissions to make it easier to merge with NEI data
  
  domain_total_ch4 <- aggregate(state_total_ch4$state_ch4_emiss,
                                list(Sector=state_total_ch4$Sector),
                                sum)
  names(domain_total_ch4) <- c('Sector', 'domain_ch4_emiss')
  # Also calculate domain totals (will be identical if only 1 state in domain)
  
  ################################################################################
  #Now load in the NEI data
  
  NEI_data_orig <- as.data.frame(read_xlsx(NEI_file,skip=0,col_names = T))
  for(A in 1:ncol(NEI_data_orig)){
    if(class(NEI_data_orig[,A])=="character"){
      NEI_data_orig[,A] <- factor(NEI_data_orig[,A])
    }
  }
  # Load in NEI CO emission data
  
  # Some county-sector combinations are missing, presumably because they are zero.
  # We want to list these as zero, otherwise it will cause trouble later
  # Use reshape to do this (there is probably a neater way...)
  NEI_data_wide <- reshape(NEI_data_orig[c('SECTOR', 'STATE', 'STATE FIPS', 'COUNTY FIPS', 'EMISSIONS')],
                           idvar=c('STATE', 'STATE FIPS', 'COUNTY FIPS'),
                           timevar='SECTOR',
                           direction='wide')
  
  NEI_data <- reshape(NEI_data_wide,
                      idvar=c('STATE', 'STATE FIPS', 'COUNTY FIPS'),
                      direction='long')
  names(NEI_data) <- c('STATE', 'STATE_FIPS', 'COUNTY_FIPS', 'SECTOR', 'CO_EMISSIONS')
  NEI_data$CO_EMISSIONS[which(is.na(NEI_data$CO_EMISSIONS))] <- 0
  
  required_sectors <- c('Fuel Comb - Comm/Institutional - Biomass',
                        'Fuel Comb - Comm/Institutional - Coal',
                        'Fuel Comb - Comm/Institutional - Natural Gas',
                        'Fuel Comb - Comm/Institutional - Oil',
                        'Fuel Comb - Comm/Institutional - Other',
                        'Fuel Comb - Electric Generation - Biomass',
                        'Fuel Comb - Electric Generation - Coal',
                        'Fuel Comb - Electric Generation - Natural Gas',
                        'Fuel Comb - Electric Generation - Oil',
                        'Fuel Comb - Electric Generation - Other',
                        'Fuel Comb - Industrial Boilers, ICEs - Biomass',
                        'Fuel Comb - Industrial Boilers, ICEs - Coal',
                        'Fuel Comb - Industrial Boilers, ICEs - Natural Gas',
                        'Fuel Comb - Industrial Boilers, ICEs - Oil',
                        'Fuel Comb - Industrial Boilers, ICEs - Other',
                        'Fuel Comb - Residential - Natural Gas',
                        'Fuel Comb - Residential - Oil',
                        'Fuel Comb - Residential - Other',
                        'Fuel Comb - Residential - Wood')
  #Every sector we will be using by name
  
  
  #Need to add dummies in to avoid errors later if any required sectors are
  #completely missing from the whole domain
  if(length(required_sectors)!=sum(required_sectors %in% levels(NEI_data$SECTOR))){
    new_input <- expand.grid(unique(NEI_data$STATE_FIPS),
                             "000",
                             required_sectors[!(required_sectors %in% levels(NEI_data$SECTOR))],
                             0)
    #one entry for every state-sector combo that was missing using a dummy county
    #and 0 emissions
    State_names <- sapply(new_input$Var1,FUN=function(x){NEI_data$STATE[which(NEI_data$STATE_FIPS==x)[1]]})
    new_input <- data.frame(State_names,new_input)
    colnames(new_input) <- colnames(NEI_data)
    #format it like the CO data, including state name (need to look up using FIPS)
    
    NEI_data$SECTOR <- as.character(NEI_data$SECTOR)
    NEI_data <- rbind(NEI_data,new_input)
    NEI_data$SECTOR <- factor(NEI_data$SECTOR)
    #simplest method to ensure new factor levels are considered in the factor.
    #Otherwise, just add in these dummy values.
    
    rm(State_names,new_input)
  }
  
  NEI_data$emiss_frac <- NEI_data$CO_EMISSIONS/ave(NEI_data$CO_EMISSIONS,
                                                   NEI_data$SECTOR,
                                                   NEI_data$STATE_FIPS,
                                                   FUN=sum)
  NEI_data$emiss_frac_domain <- NEI_data$CO_EMISSIONS/ave(NEI_data$CO_EMISSIONS,
                                                          NEI_data$SECTOR,
                                                          FUN=sum)
  # Calculate the fraction of state-sector-total CO emissions represented by each
  # county.  Repeat at the domain scale
  
  NEI_data$state_county_count <- ave(NEI_data$CO_EMISSIONS,
                                     NEI_data$SECTOR,
                                     NEI_data$STATE_FIPS,
                                     FUN=length)
  NEI_data$domain_county_count <- ave(NEI_data$CO_EMISSIONS,
                                      NEI_data$SECTOR,
                                      FUN=length)
  NEI_data$emiss_frac[which(is.na(NEI_data$emiss_frac))] <- 1/NEI_data$state_county_count[which(is.na(NEI_data$emiss_frac))]
  NEI_data$emiss_frac_domain[which(is.na(NEI_data$emiss_frac_domain))] <- 1/NEI_data$domain_county_count[which(is.na(NEI_data$emiss_frac_domain))]
  # In some cases the NEI state-sector-total CO emissions are zero, but the
  # state-sector-total CH4 emissions are not In these cases we want to distribute
  # within the state according to ACES/Vulcan only Calculate the number of
  # counties and assign equal fraction to each of them
  
  levels(NEI_data$SECTOR) <- list('com_wood_ER' = 'Fuel Comb - Comm/Institutional - Biomass',
                                  'com_coal_ER' = 'Fuel Comb - Comm/Institutional - Coal',
                                  'com_gas_ER' = 'Fuel Comb - Comm/Institutional - Natural Gas',
                                  'com_petr_ER' = 'Fuel Comb - Comm/Institutional - Oil',
                                  'com_other' = 'Fuel Comb - Comm/Institutional - Other',
                                  'elec_wood_ER' = 'Fuel Comb - Electric Generation - Biomass',
                                  'elec_coal_ER' = 'Fuel Comb - Electric Generation - Coal',
                                  'elec_gas_ER' = 'Fuel Comb - Electric Generation - Natural Gas',
                                  'elec_petr_ER' = 'Fuel Comb - Electric Generation - Oil',
                                  'elec_other' = 'Fuel Comb - Electric Generation - Other',
                                  'ind_wood_ER' = 'Fuel Comb - Industrial Boilers, ICEs - Biomass',
                                  'ind_coal_ER' = 'Fuel Comb - Industrial Boilers, ICEs - Coal',
                                  'ind_gas_ER' = 'Fuel Comb - Industrial Boilers, ICEs - Natural Gas',
                                  'ind_petr_ER' = 'Fuel Comb - Industrial Boilers, ICEs - Oil',
                                  'ind_other' = 'Fuel Comb - Industrial Boilers, ICEs - Other',
                                  'res_gas' = 'Fuel Comb - Residential - Natural Gas',
                                  'res_petr_ER' = 'Fuel Comb - Residential - Oil',
                                  'res_other' = 'Fuel Comb - Residential - Other',
                                  'res_wood_ER' = 'Fuel Comb - Residential - Wood')
  # Change levels to match state_total_ch4 so that we can merge - note that some
  # of these won't be used, but name them here anyway
  
  NEI_data_merge_step1 <- merge(NEI_data,
                                state_total_ch4,
                                by.x=c("STATE","SECTOR"),by.y=c('State', 'Sector'))
  # Combine with the state-sector-total CH4 emissions to estimate the sector-total
  # CH4 emissions for each county
  
  NEI_data_merge <- merge(NEI_data_merge_step1,
                          domain_total_ch4,
                          by.x="SECTOR",by.y='Sector')
  # Also combine with domain total emissions
  
  NEI_data_merge$county_ch4_emiss_bystate <- NEI_data_merge$state_ch4_emiss*NEI_data_merge$emiss_frac
  NEI_data_merge$county_ch4_emiss_bydomain <- NEI_data_merge$domain_ch4_emiss*NEI_data_merge$emiss_frac_domain
  
  df_long <- NEI_data_merge[c('SECTOR','STATE_FIPS','COUNTY_FIPS','county_ch4_emiss_bystate','county_ch4_emiss_bydomain')]
  df_wide <- reshape(df_long, idvar=c('STATE_FIPS','COUNTY_FIPS'), timevar='SECTOR', direction='wide')
  df_wide[is.na(df_wide)] <- 0
  # Create an unstacked dataframe containing bystate and domain emissions for each
  # county, with sectors across columns
  
  rm(required_sectors)
  ################################################################################
  #Sanity check
  
  #Does the total per state downscaled to the counties match the total per
  #state/domain initially used?
  
  NEI_check <- tapply(NEI_data_merge$county_ch4_emiss_bystate,
                      list(NEI_data_merge$STATE,NEI_data_merge$SECTOR),sum)
  #sum per state
  NEI_check <- as.data.frame.table(NEI_check)
  colnames(NEI_check) <- c(colnames(state_total_ch4)[c(1,3)],"NEI_state_CH4_emiss")
  #convert to the same format as state_total_ch4
  NEI_check <- merge(state_total_ch4,NEI_check)
  NEI_check_out <- round(NEI_check[,3],7)==round(NEI_check[,4],7)
  #check if they match to within 7 decimals
  
  if(!all(NEI_check_out)){
    View(NEI_check[!NEI_check_out,])
    stop("NEI total per state doesn't match input state totals for some sector-state combos.  Something wasn't distributed properly.")
  }
  
  #repeat, domain scale
  NEI_check <- tapply(NEI_data_merge$county_ch4_emiss_bydomain,
                      list(NEI_data_merge$SECTOR),sum)
  NEI_check <- as.data.frame.table(NEI_check)
  colnames(NEI_check) <- c(colnames(domain_total_ch4)[1],"NEI_domain_CH4_emiss")
  NEI_check <- merge(domain_total_ch4,NEI_check)
  NEI_check_out <- round(NEI_check[,2],7)==round(NEI_check[,3],7)
  #check if they match to within 7 decimals
  
  if(!all(NEI_check_out)){
    View(NEI_check[!NEI_check_out,])
    stop("NEI total in the domain doesn't match input domain totals for some sectors.  Something wasn't distributed properly.")
  }
  
  rm(NEI_check,NEI_check_out)
  ################################################################################
  #Now load in the shapefiles and ACES/Vulcan, merge geometries
  
  merge_with_poly <- terra::merge(County_Tigerlines,df_wide,
                                  by.y=c('STATE_FIPS', 'COUNTY_FIPS'),
                                  by.x=c('STATEFP','COUNTYFP'),all.y=T)

  res_totals <- c('res_petr_ER',
                  'res_wood_ER')
  
  com_totals <- c('com_coal_ER',
                  'com_petr_ER',
                  'com_gas_ER',
                  'com_wood_ER')
  
  ind_totals <- c('ind_coal_ER',
                  'ind_petr_ER',
                  'ind_gas_ER',
                  'ind_wood_ER')
  
  elec_totals <- c('elec_coal_ER',
                   'elec_petr_ER',
                   'elec_gas_ER',
                   'elec_wood_ER')
  #the many subsectors
  
  if(Use_ACES){
    #load in ACES files, flip given how R loads it, set crs as it's not loaded
    #in properly
    aces_res <- rast(paste0(ACES_directory,"/Sectoral/",ACES_year,'_Annual_ACES_Residential.nc'))
    aces_res <- flip(aces_res)
    crs(aces_res) <- "+proj=lcc +lat_0=40 +lon_0=-97 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
    
    aces_com <- rast(paste0(ACES_directory,"/Sectoral/",ACES_year,'_Annual_ACES_Commercial.nc'))
    aces_com <- flip(aces_com)
    crs(aces_com) <- "+proj=lcc +lat_0=40 +lon_0=-97 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
    
    aces_ind <- rast(paste0(ACES_directory,"/Sectoral/",ACES_year,'_Annual_ACES_Industrial.nc'))
    aces_ind <- flip(aces_ind)
    crs(aces_ind) <- "+proj=lcc +lat_0=40 +lon_0=-97 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
    
    aces_elec <- rast(paste0(ACES_directory,"/Sectoral/",ACES_year,'_Annual_ACES_Elec.nc'))
    aces_elec <- flip(aces_elec)
    crs(aces_elec) <- "+proj=lcc +lat_0=40 +lon_0=-97 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
  }
  if(Use_Vulcan){
    vu_res <- rast(paste0(vulcan_directory,"/Sectoral/","Vulcan_v3_US_annual_1km_residential_mn.nc4"), subds='carbon_emissions', lyrs=vulcan_band)
    vu_com <- rast(paste0(vulcan_directory,"/Sectoral/",'Vulcan_v3_US_annual_1km_commercial_mn.nc4'), subds='carbon_emissions', lyrs=vulcan_band)
    vu_ind <- rast(paste0(vulcan_directory,"/Sectoral/",'Vulcan_v3_US_annual_1km_industrial_mn.nc4'), subds='carbon_emissions', lyrs=vulcan_band)
    vu_elec <- rast(paste0(vulcan_directory,"/Sectoral/",'Vulcan_v3_US_annual_1km_elec_prod_mn.nc4'), subds='carbon_emissions', lyrs=vulcan_band)
  }
  
  #organize it by state-county ID for consistency with later analysis
  merge_with_poly <- merge_with_poly[order(paste0(merge_with_poly$STATEFP,merge_with_poly$COUNTYFP)),]
  
  # Transform to ACES/Vulcan CRS
  all_merge_state <- merge_with_poly
  names(all_merge_state) <- gsub("county_ch4_emiss_bystate.","",names(all_merge_state))
  all_merge_domain <- merge_with_poly
  names(all_merge_domain) <- gsub("county_ch4_emiss_bydomain.","",names(all_merge_domain))
  #create a copy that has names for the bystate or bydomain version that exactly
  #match the totals.  Easier/more consistent to code.
  
  if(Use_ACES){
    #convert state/domain scale versions to the proper crs
    all_merge_LCC_state <- project(all_merge_state,aces_res)
    all_merge_LCC_domain <- project(all_merge_domain,aces_res)
    #Calculate per-pixel coverage for each county separately.  First split by
    #unique state-county number, then calculate per-pixel coverage, output = list
    #of spatvectors
    cover_all <- all_merge_LCC_state %>% 
      split(f=paste0(all_merge_LCC_state$STATEFP,all_merge_LCC_state$COUNTYFP)) %>%
      lapply(function(x){extract(aces_res,x,weights=T,exact=T,cells=T)})
    if(stationary_combustion_by_state){
      disaggregation(aces_res,res_totals,agg_level="state",NEI_input = all_merge_LCC_state,cover_all,out_envir=environment())
      disaggregation(aces_com,com_totals,agg_level="state",NEI_input=all_merge_LCC_state,cover_all,out_envir=environment())
      disaggregation(aces_ind,ind_totals,agg_level="state",NEI_input=all_merge_LCC_state,cover_all,out_envir=environment())
      disaggregation(aces_elec,elec_totals,agg_level="state",NEI_input=all_merge_LCC_state,cover_all,out_envir=environment())
    }
    
    if(stationary_combustion_by_domain){
      disaggregation(aces_res,res_totals,agg_level="domain",NEI_input=all_merge_LCC_domain,cover_all,out_envir=environment())
      disaggregation(aces_com,com_totals,agg_level="domain",NEI_input=all_merge_LCC_domain,cover_all,out_envir=environment())
      disaggregation(aces_ind,ind_totals,agg_level="domain",NEI_input=all_merge_LCC_domain,cover_all,out_envir=environment())
      disaggregation(aces_elec,elec_totals,agg_level="domain",NEI_input=all_merge_LCC_domain,cover_all,out_envir=environment())
    }
  }
  if(Use_Vulcan){
    all_merge_LCC_state <- project(all_merge_state,vu_res)
    all_merge_LCC_domain <- project(all_merge_domain,vu_res)
    cover_all <- all_merge_LCC_state %>% 
      split(f=paste0(all_merge_LCC_state$STATEFP,all_merge_LCC_state$COUNTYFP)) %>%
      lapply(function(x){extract(vu_res,x,weights=T,exact=T,cells=T)})
    if(stationary_combustion_by_state){
      disaggregation(vu_res,res_totals,agg_level="state",NEI_input=all_merge_LCC_state,cover_all,out_envir=environment())
      disaggregation(vu_com,com_totals,agg_level="state",NEI_input=all_merge_LCC_state,cover_all,out_envir=environment())
      disaggregation(vu_ind,ind_totals,agg_level="state",NEI_input=all_merge_LCC_state,cover_all,out_envir=environment())
      disaggregation(vu_elec,elec_totals,agg_level="state",NEI_input=all_merge_LCC_state,cover_all,out_envir=environment())
    }
    
    if(stationary_combustion_by_domain){
      disaggregation(vu_res,res_totals,agg_level="domain",NEI_input=all_merge_LCC_domain,cover_all,out_envir=environment())
      disaggregation(vu_com,com_totals,agg_level="domain",NEI_input=all_merge_LCC_domain,cover_all,out_envir=environment())
      disaggregation(vu_ind,ind_totals,agg_level="domain",NEI_input=all_merge_LCC_domain,cover_all,out_envir=environment())
      disaggregation(vu_elec,elec_totals,agg_level="domain",NEI_input=all_merge_LCC_domain,cover_all,out_envir=environment())
    }
  }
  rm(all_merge_LCC_state,all_merge_LCC_domain)
  
  ################################################################################
  #write a function to save, dependent on whether or not we use XESMF
  if(XESMF){
    save_data <- function(input){
      input_name <- deparse(substitute(input))
      #pull the input name (e.g., vu_com_ch4_bydomain[[total]])
      disaggregation_level <- substring(text = input_name,regexpr("by",input_name),
                                        regexpr("\\[",input_name)-1)
      inventory_name <- strsplit(input_name,"_")[[1]][1]
      #pull the bydomain/bystate/byldc and vu/aces parts
      writeCDF(input,
               paste0(output_directory,'/',inventory_name,'_',disaggregation_level,'_stat_comb_',total,'.nc'),
               force_v4=TRUE,
               varname='methane_emissions',
               unit='mol/km2/s',
               longname=paste0(inventory_name,'_',disaggregation_level,'_stat_comb_',total),
               missval=-9999,
               overwrite=TRUE)
    }
  }else{
    #project with terra
    save_data <- function(input){
      input_name <- deparse(substitute(input))
      disaggregation_level <- substring(text = input_name,regexpr("by",input_name),
                                        regexpr("\\[",input_name)-1)
      inventory_name <- strsplit(input_name,"_")[[1]][1]
      #project to a grid with the exact right resolution, extent and origin.
      input <- project(input,domain)
      #convert from mol/km2s to nmol/m2s
      input <- input*1000
      
      
      #grab some text for the longname
      if(grepl("res_",total)){
        sector_name <- "residential"
      }else if(grepl("com_",total)){
        sector_name <- "commercial"
      }else if(grepl("elec_",total)){
        sector_name <- "electricity production"
      }else if(grepl("ind_",total)){
        sector_name <- "industrial"
      }
      
      if(grepl("coal",total)){
        fuel_name <- "coal"
      }else if(grepl("petr",total)){
        fuel_name <- "petroleum products"
      }else if(grepl("gas",total)){
        fuel_name <- "natural gas"
      }else if(grepl("wood",total)){
        fuel_name <- "wood"
      }
      
      if(grepl("state",disaggregation_level)){
        disaggregation_name <- "individual-state"
      }else if(grepl("domain",disaggregation_level)){
        disaggregation_name <- "domain"
      }
      
      if("aces"==inventory_name){
        inventory_name <- "aces"
      }else if("vu"==inventory_name){
        inventory_name <- "vulcan"
      }
      
      writeCDF(input,
               paste0(output_directory,'/',"stat_comb_",sub("_ER","",total),
                      "_",disaggregation_level,"_",inventory_name,'.nc'),
               force_v4=TRUE,
               varname='methane_emissions',
               unit='nmol/m2/s',
               longname=paste0('Methane emissions from ',sector_name,' sector stationary combustion of ',
                               fuel_name,', spatially allocated from ',disaggregation_name,
                               ' totals using NEI CO emissions and ',inventory_name,' ',sector_name,' CO2 emissions'),
               missval=-9999,
               overwrite=TRUE)
    }
  }

  ################################################################################
  #Save the results
  
  # Now save the rasters for each subsector
  for(total in res_totals){
    if(Use_ACES){
      if(stationary_combustion_by_state){
        save_data(aces_res_ch4_bystate[[total]])
      }
      if(stationary_combustion_by_domain){
        save_data(aces_res_ch4_bydomain[[total]])
      }
    }
    if(Use_Vulcan){
      if(stationary_combustion_by_state){
        save_data(vu_res_ch4_bystate[[total]])
      }
      if(stationary_combustion_by_domain){
        save_data(vu_res_ch4_bydomain[[total]])
      }
    }
  }

  for(total in com_totals){
    if(Use_ACES){
      if(stationary_combustion_by_state){
        save_data(aces_com_ch4_bystate[[total]])
      }
      if(stationary_combustion_by_domain){
        save_data(aces_com_ch4_bydomain[[total]])
      }
    }
    if(Use_Vulcan){
      if(stationary_combustion_by_state){
        save_data(vu_com_ch4_bystate[[total]])
      }
      if(stationary_combustion_by_domain){
        save_data(vu_com_ch4_bydomain[[total]])
      }
    }
  }

  for(total in ind_totals){
    if(Use_ACES){
      if(stationary_combustion_by_state){
        save_data(aces_ind_ch4_bystate[[total]])
      }
      if(stationary_combustion_by_domain){
        save_data(aces_ind_ch4_bydomain[[total]])
      }
    }
    if(Use_Vulcan){
      if(stationary_combustion_by_state){
        save_data(vu_ind_ch4_bystate[[total]])
      }
      if(stationary_combustion_by_domain){
        save_data(vu_ind_ch4_bydomain[[total]])
      }
    }
  }

  for(total in elec_totals){
    if(Use_ACES){
      if(stationary_combustion_by_state){
        save_data(aces_elec_ch4_bystate[[total]])
      }
      if(stationary_combustion_by_domain){
        save_data(aces_elec_ch4_bydomain[[total]])
      }
    }
    if(Use_Vulcan){
      if(stationary_combustion_by_state){
        save_data(vu_elec_ch4_bystate[[total]])
      }
      if(stationary_combustion_by_domain){
        save_data(vu_elec_ch4_bydomain[[total]])
      }
    }
  }
  
  ################################################################################
  # Some sanity checks
  
  #find all processed residential files
  res_data_objects <- as.list(ls(pattern=glob2rx("*_res_ch4*")))
  #get the length, convert from a list of the names to the actual rasters
  res_data_length <- length(res_data_objects)
  res_data_list <- sapply(res_data_objects,get,envir=environment())
  #get the domain total for each raster, put into an organized df
  res_data <- as.data.frame(matrix(sapply(res_data_list,global,sum),
                                   ncol=res_data_length))
  #properly name it's dimensions
  names(res_data) <- gsub("res_ch4","by",unlist(res_data_objects))
  rownames(res_data) <- names(res_data_list[,1])
  
  com_data_objects <- as.list(ls(pattern=glob2rx("*_com_ch4*")))
  com_data_length <- length(com_data_objects)
  com_data_list <- sapply(com_data_objects,get,envir=environment())
  com_data <- as.data.frame(matrix(sapply(com_data_list,global,sum),
                                   ncol=com_data_length))
  names(com_data) <- gsub("com_ch4","by",unlist(com_data_objects))
  rownames(com_data) <- names(com_data_list[,1])
  
  ind_data_objects <- as.list(ls(pattern=glob2rx("*_ind_ch4*")))
  ind_data_length <- length(ind_data_objects)
  ind_data_list <- sapply(ind_data_objects,get,envir=environment())
  ind_data <- as.data.frame(matrix(sapply(ind_data_list,global,sum),
                                   ncol=ind_data_length))
  names(ind_data) <- gsub("ind_ch4","by",unlist(ind_data_objects))
  rownames(ind_data) <- names(ind_data_list[,1])
  
  elec_data_objects <- as.list(ls(pattern=glob2rx("*_elec_ch4*")))
  elec_data_length <- length(elec_data_objects)
  elec_data_list <- sapply(elec_data_objects,get,envir=environment())
  elec_data <- as.data.frame(matrix(sapply(elec_data_list,global,sum),
                                    ncol=elec_data_length))
  names(elec_data) <- gsub("elec_ch4","by",unlist(elec_data_objects))
  rownames(elec_data) <- names(elec_data_list[,1])
  
  #combine into 1 dataframe
  ch4_totals_df <- rbind(com_data,elec_data,ind_data,res_data)
  
  #original data that was distributed in the rasters.  The totals should still
  #match.
  input_totals <- values(merge_with_poly[,grep(glob2rx("county_ch4_emiss*"),names(merge_with_poly))])
  input_totals <- colSums(input_totals)
  input_totals_state <- input_totals[grep("bystate",names(input_totals))]
  input_totals_domain <- input_totals[grep("bydomain",names(input_totals))]
  names(input_totals_state) <- gsub("county_ch4_emiss_bystate.","",names(input_totals_state))
  names(input_totals_domain) <- gsub("county_ch4_emiss_bydomain.","",names(input_totals_domain))
  
  #combine all 3 now
  ch4_totals_df <- data.frame(ch4_totals_df,
                              "bystate_input"=input_totals_state[rownames(ch4_totals_df)],
                              "bydomain_input"=input_totals_domain[rownames(ch4_totals_df)])
  
  #save the rownames
  ch4_rowname_df <- rownames(ch4_totals_df)
  
  #rewrite to numeric and rename the rows
  ch4_totals_df <- apply(ch4_totals_df, 2, FUN=function(x){as.numeric(x)})
  rownames(ch4_totals_df) <- ch4_rowname_df
  
  
  #compare every column to the first, rounded to 7 digits.  All values should be
  #true
  if(!all(as.vector(round(ch4_totals_df,7)==round(ch4_totals_df[,1],7)))){
    View(ch4_totals_df)
    stop("Domain totals differ when distributed using a different inventory or distributing at the state vs domain level")
  }
  
  ################################################################################
  # plot up the data
  
  # all_objects <- unlist(c(res_data_objects,com_data_objects,ind_data_objects,elec_data_objects))
  # 
  # data_combinations <- expand.grid(c("com","elec","ind","res"),c("coal","gas","petr","wood"))[-c(4,8),]
  # coal,petr,gas,wood
  
  for(total in res_totals){
    # envir=environment()
    combined_data <- rast(sapply(res_data_objects,
                                 FUN=function(x){project(get(x,
                                                             envir=environment())[[total]]*1000,domain)}))
    combined_range=global(combined_data,range)
    zmin <- min(combined_range[,1])
    zmax <- max(combined_range[,2])
    
    #grab some text for the plot title
    disaggregation_level <- sapply(strsplit(unlist(res_data_objects),"by"),"[[",2)
    inventory_name <- sapply(strsplit(unlist(res_data_objects),"_com"),"[[",1)
    
    if(grepl("coal",total)){
      fuel_name <- "Coal"
    }else if(grepl("petr",total)){
      fuel_name <- "Petroleum"
    }else if(grepl("gas",total)){
      fuel_name <- "Gas"
    }else if(grepl("wood",total)){
      fuel_name <- "Wood"
    }
    
    inventory_name["aces"==inventory_name] <- "ACES"
    inventory_name["vu"==inventory_name] <- "Vulcan"
    
    fuel_sub_name <- strsplit(total,"_")[[1]][2]
    for(A in 1:nlyr(combined_data)){
      not_log_plot(combined_data[[A]],
                   filename=paste0("stat_comb_res_",fuel_sub_name,"_by",
                                   disaggregation_level,"_",tolower(inventory_name),".png")[A],
                   paste0("Stationary Combustion Residential - ",fuel_name,"\n ",
                          disaggregation_level," totals distributed using NEI CO emissions\n and ",
                          inventory_name," residential CO2 emissions")[A],
                   zlim_min = zmin,zlim_max = zmax)
    }
  }
  
  
  
  for(total in com_totals){
    combined_data <- rast(sapply(com_data_objects,
                                 FUN=function(x){project(get(x,
                                                             envir=environment())[[total]]*1000,domain)}))
    combined_range=global(combined_data,range)
    zmin <- min(combined_range[,1])
    zmax <- max(combined_range[,2])
    
    #grab some text for the plot title
    disaggregation_level <- sapply(strsplit(unlist(com_data_objects),"by"),"[[",2)
    inventory_name <- sapply(strsplit(unlist(com_data_objects),"_com"),"[[",1)
    
    if(grepl("coal",total)){
      fuel_name <- "Coal"
    }else if(grepl("petr",total)){
      fuel_name <- "Petroleum"
    }else if(grepl("gas",total)){
      fuel_name <- "Gas"
    }else if(grepl("wood",total)){
      fuel_name <- "Wood"
    }
    
    inventory_name["aces"==inventory_name] <- "ACES"
    inventory_name["vu"==inventory_name] <- "Vulcan"
    
    fuel_sub_name <- strsplit(total,"_")[[1]][2]
    for(A in 1:nlyr(combined_data)){
      not_log_plot(combined_data[[A]],
                   filename=paste0("stat_comb_com_",fuel_sub_name,"_by",
                                   disaggregation_level,"_",tolower(inventory_name),".png")[A],
                   paste0("Stationary Combustion Commercial - ",fuel_name,"\n ",
                          disaggregation_level," totals distributed using NEI CO emissions\n and ",
                          inventory_name," commercial CO2 emissions")[A],
                   zlim_min = zmin,zlim_max = zmax)
    }
  }
  
  
  
  for(total in ind_totals){
    combined_data <- rast(sapply(ind_data_objects,
                                 FUN=function(x){project(get(x,
                                                             envir=environment())[[total]]*1000,domain)}))
    combined_range=global(combined_data,range)
    zmin <- min(combined_range[,1])
    zmax <- max(combined_range[,2])
    
    #grab some text for the plot title
    disaggregation_level <- sapply(strsplit(unlist(ind_data_objects),"by"),"[[",2)
    inventory_name <- sapply(strsplit(unlist(ind_data_objects),"_ind"),"[[",1)
    
    if(grepl("coal",total)){
      fuel_name <- "Coal"
    }else if(grepl("petr",total)){
      fuel_name <- "Petroleum"
    }else if(grepl("gas",total)){
      fuel_name <- "Gas"
    }else if(grepl("wood",total)){
      fuel_name <- "Wood"
    }
    
    inventory_name["aces"==inventory_name] <- "ACES"
    inventory_name["vu"==inventory_name] <- "Vulcan"
    
    fuel_sub_name <- strsplit(total,"_")[[1]][2]
    for(A in 1:nlyr(combined_data)){
      not_log_plot(combined_data[[A]],
                   filename=paste0("stat_comb_ind_",fuel_sub_name,"_by",
                                   disaggregation_level,"_",tolower(inventory_name),".png")[A],
                   paste0("Stationary Combustion Industrial - ",fuel_name,"\n ",
                          disaggregation_level," totals distributed using NEI CO emissions\n and ",
                          inventory_name," industrial CO2 emissions")[A],
                   zlim_min = zmin,zlim_max = zmax)
    }
  }
  
  
  for(total in elec_totals){
    combined_data <- rast(sapply(elec_data_objects,
                                 FUN=function(x){project(get(x,
                                                             envir=environment())[[total]]*1000,domain)}))
    combined_range=global(prep_plot_data(combined_data),range,na.rm=T)
    zmin <- min(combined_range[,1])
    zmax <- max(combined_range[,2])
    
    #grab some text for the plot title
    disaggregation_level <- sapply(strsplit(unlist(elec_data_objects),"by"),"[[",2)
    inventory_name <- sapply(strsplit(unlist(elec_data_objects),"_elec"),"[[",1)
    
    if(grepl("coal",total)){
      fuel_name <- "Coal"
    }else if(grepl("petr",total)){
      fuel_name <- "Petroleum"
    }else if(grepl("gas",total)){
      fuel_name <- "Gas"
    }else if(grepl("wood",total)){
      fuel_name <- "Wood"
    }
    
    inventory_name["aces"==inventory_name] <- "ACES"
    inventory_name["vu"==inventory_name] <- "Vulcan"
    
    fuel_sub_name <- strsplit(total,"_")[[1]][2]
    for(A in 1:nlyr(combined_data)){
      log_plot(combined_data[[A]],
                   filename=paste0("stat_comb_elec_",fuel_sub_name,"_by",
                                   disaggregation_level,"_",tolower(inventory_name),".png")[A],
                   paste0("Stationary Combustion Electricity - ",fuel_name,"\n ",
                          disaggregation_level," totals distributed using NEI CO emissions\n and ",
                          inventory_name," electricity CO2 emissions")[A],
                   zlim_min = zmin,zlim_max = zmax)
    }
  }
  
  
  
  # dir.create("Summed_Sectors",showWarnings = F)
  # 
  # #use regex to ID only those for each inventory-division combination and separate
  # #wood
  # Summed_stationary_combustion_FF_ACES_bydomain <- stack(paste0(filepath_out,file_list_out[grep("aces_bydomain_stat_comb_[[:alnum:]]+_[coal|gas|petr]",file_list_in)]))
  # Summed_stationary_combustion_wood_ACES_bydomain <- stack(paste0(filepath_out,file_list_out[grep("aces_bydomain_stat_comb_[[:alnum:]]+_wood",file_list_in)]))
  # 
  # Summed_stationary_combustion_FF_ACES_bydomain <- sum(Summed_stationary_combustion_FF_ACES_bydomain)
  # Summed_stationary_combustion_wood_ACES_bydomain <- sum(Summed_stationary_combustion_wood_ACES_bydomain)
  # 
  # log_plot(Summed_stationary_combustion_FF_ACES_bydomain,
  #          "Stationary Combustion FF Sector\nSEDS domain data scaled to match GHGI national data distributed to the county\nlevel via NEI, then distributed using ACES  coal + gas + petroleum")
  # log_plot(Summed_stationary_combustion_wood_ACES_bydomain,
  #          "Stationary Combustion Wood Sector\nSEDS domain data scaled to match GHGI national data distributed to the county\nlevel via NEI, then distributed using ACES  wood")
  # 
  # #repeat for aces-bystate
  # Summed_stationary_combustion_FF_ACES_bystate <- stack(paste0(filepath_out,file_list_out[grep("aces_bystate_stat_comb_[[:alnum:]]+_[coal|gas|petr]",file_list_in)]))
  # Summed_stationary_combustion_wood_ACES_bystate <- stack(paste0(filepath_out,file_list_out[grep("aces_bystate_stat_comb_[[:alnum:]]+_wood",file_list_in)]))
  # Summed_stationary_combustion_FF_ACES_bystate <- sum(Summed_stationary_combustion_FF_ACES_bystate)
  # Summed_stationary_combustion_wood_ACES_bystate <- sum(Summed_stationary_combustion_wood_ACES_bystate)
  # log_plot(Summed_stationary_combustion_FF_ACES_bystate,
  #          "Stationary Combustion FF Sector\nSEDS state data scaled to match GHGI national data distributed to the county\nlevel via NEI, then distributed using ACES  coal + gas + petroleum")
  # log_plot(Summed_stationary_combustion_wood_ACES_bystate,
  #          "Stationary Combustion Wood Sector\nSEDS state data scaled to match GHGI national data distributed to the county\nlevel via NEI, then distributed using ACES  wood")
  # 
  # #repeat for vu-bydomain
  # Summed_stationary_combustion_FF_Vulcan_bydomain <- stack(paste0(filepath_out,file_list_out[grep("vu_bydomain_stat_comb_[[:alnum:]]+_[coal|gas|petr]",file_list_in)]))
  # Summed_stationary_combustion_wood_Vulcan_bydomain <- stack(paste0(filepath_out,file_list_out[grep("vu_bydomain_stat_comb_[[:alnum:]]+_wood",file_list_in)]))
  # Summed_stationary_combustion_FF_Vulcan_bydomain <- sum(Summed_stationary_combustion_FF_Vulcan_bydomain)
  # Summed_stationary_combustion_wood_Vulcan_bydomain <- sum(Summed_stationary_combustion_wood_Vulcan_bydomain)
  # log_plot(Summed_stationary_combustion_FF_Vulcan_bydomain,
  #          "Stationary Combustion FF Sector\nSEDS domain data scaled to match GHGI national data distributed to the county\nlevel via NEI, then distributed using Vulcan  coal + gas + petroleum")
  # log_plot(Summed_stationary_combustion_wood_Vulcan_bydomain,
  #          "Stationary Combustion Wood Sector\nSEDS domain data scaled to match GHGI national data distributed to the county\nlevel via NEI, then distributed using Vulcan  wood")
  # 
  # #repeat for vu-bystate
  # Summed_stationary_combustion_FF_Vulcan_bystate <- stack(paste0(filepath_out,file_list_out[grep("vu_bystate_stat_comb_[[:alnum:]]+_[coal|gas|petr]",file_list_in)]))
  # Summed_stationary_combustion_wood_Vulcan_bystate <- stack(paste0(filepath_out,file_list_out[grep("vu_bystate_stat_comb_[[:alnum:]]+_wood",file_list_in)]))
  # Summed_stationary_combustion_FF_Vulcan_bystate <- sum(Summed_stationary_combustion_FF_Vulcan_bystate)
  # Summed_stationary_combustion_wood_Vulcan_bystate <- sum(Summed_stationary_combustion_wood_Vulcan_bystate)
  # log_plot(Summed_stationary_combustion_FF_Vulcan_bystate,
  #          "Stationary Combustion FF Sector\nSEDS state data scaled to match GHGI national data distributed to the county\nlevel via NEI, then distributed using Vulcan  coal + gas + petroleum")
  # log_plot(Summed_stationary_combustion_wood_Vulcan_bystate,
  #          "Stationary Combustion Wood Sector\nSEDS state data scaled to match GHGI national data distributed to the county\nlevel via NEI, then distributed using Vulcan  wood")
  # 
  
}
