#'@title Create gridded natural gas distribution methane emissions maps
#'
#'@description `NG_distribution` writes up to 63 netcdf files of gridded methane
#'  emissions from natural gas distribution sources, as well as optional visuals
#'
#'@details This function calculates and grids methane emissions from natural gas
#'  distribution.  It uses a Homeland Infrastructure Foundation-Level Data
#'  (HIFLD) dataset titled Natural Gas Local Distribution Company Service
#'  Territories, Environmental Protection Agency's (EPA) Greenhouse Gas
#'  Inventory (GHGI), the EPA Greenhouse Gas Reporting Program (GHGRP), the
#'  Pipeline and Hazardous Materials Safety Administration (PHMSA) gas
#'  distribution annual report, the Energy Information Administration (EIA) Form
#'  176 - Annual Report of Natural and Supplemental Gas Supply and Disposition,
#'  and either the Vulcan or Anthropogenic Carbon Emission System (ACES) CO2
#'  inventory.  This can be done at the Local Distribution Company (LDC) level,
#'  State level, or Domain level.  As several of the input datasets have no
#'  common LDC identifier, they must be carefully matched to run this function
#'  at the LDC level.  If running at the state or domain level, data can be
#'  aggregated and applied between datasets without any manual matching needed.
#'
#'  First all of the input data excluding the GHGI are loaded in and filtered to
#'  only the states within the domain.  The GHGRP data is used to calculate the
#'  typical number of stations per mile of pipeline for each LDC.  Stations
#'  include transmission and distribution transfer stations and metering and
#'  regulating stations.  This is calculated for all stations combined, as well
#'  as separately for above grade and below grade stations.  If calculating by
#'  LDC, the PHMSA miles of pipeline per LDC is used in the calculation of
#'  stations per mile, rather than the GHGRP, though the miles of pipeline for
#'  each LDC in GHGRP and PHMSA are compared and if any LDC differs by > 5\% an
#'  error will be flagged.
#'
#'  The GHGI Annex data is then pulled and includes emission factors and
#'  activity data for metering an regulating stations separated by inlet
#'  pressure, and just the emission factors for pipeline services separated by
#'  pipeline material, meters separated by customer type (residential,
#'  commercial, industrial), and different maintenance events (pressure relief
#'  valve release, blow-downs, accidental dig-ins).
#'
#'  PHMSA data on miles of pipeline separated by material is then combined with
#'  the input "GHGI_natural_gas_pipeline_emission_factors", which by default are
#'  the emission factors from Table 2 of Weller et al. who used research
#'  equipped Google Street View cars to measure > 4000 leaks in the U.S.  PHMSA
#'  data on pipeline services, separated by pipeline material, are combined with
#'  the GHGI emission factors to get emissions for each LDC.
#'
#'  GHGRP data is aggregated to the state level to get the average number of
#'  above or below grade stations per mile.  If not calculating emission by LDC,
#'  this is combined with the PHMSA miles of pipeline to get an estimate of the
#'  number of above and below grade stations for each LDC in the PHMSA dataset.
#'
#'  The combined miles of pipeline including services is calculated from the
#'  PHMSA and the various input datasets are subset to only the necessary
#'  variables, aggregated to the state level and merged.  The variables kept
#'  differ slightly if calculating by LDC.
#'
#'  If not calculating by LDC, then there is no HIFLD csv file necessary,
#'  leaving the PHMSA activity data, the EIA sales data, and the GHGRP activity
#'  data.
#'
#'  If calculating by LDC a HIFLD csv and shapefile (containing the same
#'  information) are also included.  The residuals (LDCs that could not be
#'  matched across datasets) are assigned to an "OTHER" LDC that includes all
#'  land in the state not already accounted for by an LDC.  The number of
#'  stations for each LDC is now overwritten to be the values for that specific
#'  LDC from the GHGRP.  State average stations per mile (calculated from the
#'  GHGRP) are then applied only to LDCs that did not report to the GHGRP.
#'
#'  Service and pipeline emissions are then split into residential and
#'  commercial fractions.  This is calculated for residential as the sum of all
#'  emissions * N residential customers / N total residential and commercial
#'  customers.  The calculation would be equivalent for commercial customers.
#'
#'  Emissions for metering and regulating stations are then calculated as a
#'  function of pressure.  The number of stations of a grade (above or below)
#'  are multiplied with the national fraction of metering and regulating
#'  stations of that grade that are a certain pressure window and then
#'  multiplied by the emission factor for that pressure window.  E.g., N Above
#'  grade (by LDC or state) * national type fraction * national type emission
#'  factor.  These emissions are then split into residential and commercial in
#'  the same manner as service and pipeline emissions were.
#'
#'  Meter emissions are calculated separately for each customer type
#'  (residential, commercial, industrial) using the corresponding GHGI emission
#'  factors.  These emissions are then split into residential and commercial,
#'  incorporating industrial emissions into residential and commercial
#'  proportionally.  This was done as the industrial sectors of ACES/Vulcan are
#'  dominated by point sources, many of which don't even rely on natural gas. It
#'  was chosen to split by emissions rather than number of customers as this was
#'  considered more representative (e.g., there may be many residential
#'  customers, yet the commercial ones consume more natural gas overall).
#'
#'  Maintenance and upsets are calculated using national GHGI emission factors
#'  and split into residential and commercial in the same manner as service and
#'  pipeline emissions were.
#'
#'  Post-meter emissions are calculated, strictly for residential, using the
#'  volume of gas delivered to residential customers and the
#'  natural_gas_post_meter_emission_factor, which is based on Fischer et al. by
#'  default.  Fischer et al. measured whole-house emissions from 75 homes in
#'  California using mass balance.
#'
#'  Finally, ACES and/or Vulcan residential and commercial gridded CO2 emission
#'  maps are loaded in.  The emissions for each subsector are then distributed
#'  using the ACES or Vulcan CO2 inventory.  This can be done at the LDC, state,
#'  or domain level.  Emissions at this point are at the state level if not
#'  calculating by LDC and can then be aggregated to the domain level. Otherwise
#'  emissions are at the LDC level and can be aggregated to the state or domain
#'  level.  As such, producing output at the state and LDC level will result in
#'  slightly different output than running only at the state level.
#'
#'
#'  So, to summarize this relatively complex sector, PHMSA data on miles of
#'  pipeline by type and number of services by type of pipeline is combined with
#'  emission factors to calculate emissions.  GHGRP state average numbers of M&R
#'  facilities per mile are also combined with an emission factor, broken down
#'  by pressure.  If calculating byLDC and GHGRP data exists for an LDC, the
#'  reported counts are used instead.  These emissions are then distributed to the
#'  appropriate LDC territories using HIFLD shapefiles, or aggregated at the
#'  state level. To further disaggregate the emissions, they are broken into
#'  residential and commercial portions using the fraction of customers as
#'  reported to the EIA. They are then distributed to the pixel scale using the
#'  residential and commercial sectors of the CO2 inventories Vulcan/ACES.  As
#'  such, GHGRP emissions are not directly used at all, PHMSA is the source of
#'  most activity data with GHGRP providing the numbers of M&R facilities, HIFLD
#'  solely provides shapefiles for each LDC if operating by LDC, and EIA
#'  provides a breakdown of residential vs commercial customers.
#'
#'
#'  GHGRP data is available starting in 2010 and generally is about 2 years
#'  behind present day, the GHGI is available starting in 1990 and is updated
#'  approximately in sync with the GHGRP.  The HIFLD dataset is updated
#'  infrequently and is available for 2019 and 2017.  The EIA form is annually
#'  reported and is available starting in 1997 and is generally updated in
#'  September to the previous year (i.e., Sept 2024 adds 2023 data).  The PHMSA
#'  data is annual and is available starting in 1970 and is available up to the
#'  most recent year.  The GHGRP includes only facilities that emit at least
#'  25,000 metric tons of carbon dioxide equivalent while the GHGI is intended
#'  to capture all national emissions. All other datasets are meant to be
#'  inclusive of national facilities. All data is annual.  The GHGI is national
#'  totals while all other datasets are at the facility scale.
#'
#'  GHGRP data and the HIFLD shapefile will be automatically downloaded.
#'
#'  The GHGRP is available at \url{https://ghgdata.epa.gov/ghgp/main.do}. The
#'  GHGI is available at
#'  \url{https://www.epa.gov/ghgemissions/inventory-us-greenhouse-gas-emissions-and-sinks}.
#'  For individual LDC metering and regulating station counts, among other
#'  variables, one must filter to the natural gas local distribution companies
#'  sector, select an individual facility and select "View reported data".  The
#'  HIFLD dataset is available at
#'  \url{https://hifld-geoplatform.hub.arcgis.com/datasets/geoplatform::natural-gas-service-territories/about}
#'  and can be donwloaded as both a shapefile or a csv. EIA form 176 is
#'  available at
#'  \url{https://www.eia.gov/naturalgas/ngqs/#?report=RP4&year1=2020&year2=2020&company=Name}
#'  and can be downloaded as an excel file.  The PHMSA Gas Distribution Annual
#'  Data can be download at
#'  \url{https://www.phmsa.dot.gov/data-and-statistics/pipeline/gas-distribution-gas-gathering-gas-transmission-hazardous-liquids}
#'  as a zip file with an excel file for each year. ACES is available at
#'  \url{https://doi.org/10.3334/ORNLDAAC/1943} and Vulcan is available at
#'  \url{https://doi.org/10.3334/ORNLDAAC/1741}.
#'
#'  See references \href{https://doi.org/10.1021/acs.est.0c00437}{Weller et
#'  al.}, \href{https://doi.org/10.1021/acs.est.8b03217}{Fischer et al.},
#'  \href{https://doi.org/10.1029/2020JD032974}{Vulcan} and,
#'  \href{https://doi.org/10.1002/2017JD027359}{ACES}
#'
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
#'@param HIFLD_file \strong{Optional} character providing the full filepath to
#'  the HIFLD natural gas service territory csv file available at
#'  \url{https://hifld-geoplatform.hub.arcgis.com/datasets/geoplatform::natural-gas-service-territories/about}.
#'  On the left of the page is a download button with multiple file types.  This
#'  is only needed if running at the LDC level.  The file must be edited such
#'  that there is a consistent identifier with other input data (EIA, PHMSA,
#'  GHGRP).  There is an example file in the package's datasets folder that has
#'  been successfully used in this code available for reference.
#'@param GHGRP_file \strong{Optional} character providing the full filepath to
#'  the GHGRP xls file available at \url{https://ghgdata.epa.gov/ghgp/main.do}.
#'  \itemize{
#'    \item Filter greenhouse gases to CH4
#'    \item filter sectors to Natural Gas Local Distribution Companies
#'    \item set "browse to a State" to "choose state" to get data for all states
#'    \item apply search and then export data for all years
#'  }
#'  This is only needed if running at the LDC level.  The file must be edited
#'  such that there is a consistent identifier with other input data (HIFLD,
#'  PHMSA, HIFLD). There is an example file in the package's datasets folder
#'  that has been successfully used in this code available for reference.
#'@param EIA_file Character providing the full filepath to the EIA Form 176 -
#'  Annual Report of Natural and supplemental Gas Supply and Disposition xlsx
#'  file available at
#'  \url{https://www.eia.gov/naturalgas/ngqs/#?report=RP4&year1=2020&year2=2020&company=Name}.
#'  The year can be set to just the desired year.  Then all data can be
#'  downloaded to an xlsx file using a button to the topright of the data table.
#'  If running at the LDC level, the file must be edited such that there is a
#'  consistent identifier with other input data (HIFLD, PHMSA, GHGRP).  There is
#'  an example file in the package's datasets folder that has been successfully
#'  used in this code available for reference.
#'@param PHMSA_file Character providing the full filepath to the PHMSA Gas
#'  Distribution Annual Data xlsx file available at
#'  \url{https://www.phmsa.dot.gov/data-and-statistics/pipeline/gas-distribution-gas-gathering-gas-transmission-hazardous-liquids}.
#'  Zip files for groups of years are available via links at the bottom of the
#'  page.  If running at the LDC level the file must be edited such that there
#'  is a consistent identifier with other input data (EIA, HIFLD, GHGRP).  There
#'  is an example file in the package's datasets folder that has been
#'  successfully used in this code available for reference.
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
#'@param GHGI_EF_sheet Character providing the sheet name in "GHGI_file" that
#'  provides the "Average CH4 Emission Factors (kg/unit activity) for Natural
#'  Gas Systems Sources, for All Years".  The sheet name as of the 2022 GHGI is
#'  "3.6-2".
#'@param GHGI_Activity_sheet Character providing the sheet name in "GHGI_file"
#'  that provides the "Activity Data for Natural Gas Systems Sources, for All
#'  Years".  The sheet name as of the 2022 GHGI is "3.6-7".
#'@param State_Tigerlines SpatVector.  United States Census Bureau county
#'  shapefile.  Available at
#'  \url{https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html}.
#'@param NG_distribution_by_LDC Logical.  Pulled from config file.  Indicating
#'  whether emissions should be calculated at the Local Distribution Company
#'  level.
#'@param NG_distribution_by_state Logical.  Pulled from config file.  Indicating
#'  whether emissions should be calculated at the state level.
#'@param NG_distribution_by_domain Logical.  Pulled from config file. Indicating
#'  whether emissions should be calculated at the domain level.
#'@param GHGI_natural_gas_pipeline_emission_factors Data frame.  Pulled from
#'  config file.  2 columns by 4 rows.  Columns are "Leaks_per_mile" and
#'  "Avg_emissions_mol_per_s".  Rownames are "Bare_Steel", "Cast_Iron",
#'  "Coated_steel", and "Plastic".  Default is from Weller et al.
#'@param natural_gas_post_meter_emission_factor Numeric.  Pulled from config
#'  file.  Emission factor for whole-house residential emissions in mol/s.
#'  Default is from Fischer et al.
#'@param Use_ACES Logical indicating whether or not to use ACES to disaggregate
#'  from county-level to pixel level emissions.  Either ACES or Vulcan must be
#'  used, though both can be.
#'@param Use_Vulcan Logical indicating whether or not to use Vulcan to
#'  disaggregate from county-level to pixel level emissions.  Either ACES or
#'  Vulcan must be used, though both can be.
#'@param ACES_directory \strong{Optional} character providing the full path to a
#'  folder containing the ACES sectoral CO2 inventories.  Must include the
#'  residential and commercial sectors.  ACES v2.0 is available at
#'  \url{https://doi.org/10.3334/ORNLDAAC/1943}, though the hourly file should
#'  be averaged across hours to create an annually averaged inventory. Code to
#'  do this on a linux-based HPC system is available as the script
#'  "Annualize_ACES_seawulf.R" and the accompanying batch script
#'  "Annualize_ACES.sh".  The year closest to "inventory_year" is used, but
#'  those further from that year are considered if the closest is unavailable.
#'  Only needed if Use_ACES = TRUE.  At least one of Use_Vulcan or Use_ACES must
#'  be TRUE.
#'@param vulcan_directory \strong{Optional} character providing the full path to
#'  a folder containing the Vulcan sectoral CO2 inventories.  Must include the
#'  residential and commercial sectors. Vulcan v3.0 is available at
#'  \url{https://doi.org/10.3334/ORNLDAAC/1741}, and the annual mean files
#'  should be used.  The year closest to "inventory_year" is used.  As all years
#'  are contained in the same file, it does not search for other years.  Only
#'  needed if Use_Vulcan = TRUE.  At least one of Use_Vulcan or Use_ACES must be
#'  TRUE.
#'@param ACES_year \strong{Optional} numeric providing the year of ACES data to
#'  use. Only needed if Use_ACES = TRUE.  At least one of Use_Vulcan or Use_ACES
#'  must be TRUE.
#'@param vulcan_band \strong{Optional} numeric providing the band of Vulcan data
#'  to use (1-6 = 2010 - 2015).  Only needed if Use_Vulcan = TRUE.  At least one
#'  of Use_Vulcan or Use_ACES must be TRUE.
#'@param County_Tigerlines \strong{Optional} spatVector.  United States Census
#'  Bureau county shapefile downloaded in Main.  Only needed if verbose = TRUE.
#'@param plot_directory \strong{Optional} character providing the full filepath
#'  to save figures. Only needed if verbose = TRUE.
#'@param focus_city_tigerlines \strong{Optional} spatVector.  United States
#'  Census Bureau county shapefile.  Available at
#'  \url{https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html}.
#'  Only needed if a focus city was set in main and verbose=TRUE.
#'@returns Nothing is returned from the function, but the main outputs are up to
#'  63 netcdf files of the methane emissions from natural gas distribution. They
#'  are titled as "NG_dist_type_sector_variation.nc" where type is upset, serv
#'  (services), post_meter, MnR (metering and regulating stations), and mains;
#'  sector is abbreviated as res (residential) or com (commercial); and
#'  variation is byLDC, bystate, or bydomain.
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
#' GHGI_EF_dataframe <- data.frame("Leaks_per_mile"=
#'                                   c(0.51,1,0.61,0.43),
#'                                 "Avg_emissions_mol_per_s"=
#'                                   c(2.24,1.72,2,2.03)/(16.043*60)) #converting from g/min to mol/s
#' rownames(GHGI_EF_dataframe) <- c("Bare_Steel",
#'                                  "Cast_Iron",
#'                                  "Coated_steel",
#'                                  "Plastic")
#' NG_distribution(domain=grid,
#'                 state_name_list=c("DE","MD","NJ","NY","PA"),
#'                 output_directory="~/../Desktop/",
#'                 inventory_year=2018,
#'                 verbose=TRUE,
#'                 EIA_file = "~/../Desktop/176 Type of Operations and Sector Items.xlsx",
#'                 PHMSA_file = "~/../Desktop/annual_gas_distribution_2010_present/annual_gas_distribution_2019.xlsx",
#'                 GHGI_file = "~/../Desktop/2022_ghgi_natural_gas_systems_annex36_tables.xlsx",
#'                 GHGI_EF_sheet = "3.6-2",
#'                 GHGI_Activity_sheet = "3.6-7",
#'                 State_Tigerlines=vect("~/../Desktop/State_Tigerlines/tl_2018_us_state.shp"),
#'                 NG_distribution_by_LDC = FALSE,
#'                 NG_distribution_by_state = TRUE,
#'                 NG_distribution_by_domain = TRUE,
#'                 GHGI_natural_gas_pipeline_emission_factors=GHGI_EF_dataframe,
#'                 natural_gas_post_meter_emission_factor=7850/401*0.005/(16.043*60*60*24*365),
#'                 Use_ACES=TRUE,
#'                 Use_Vulcan=TRUE,
#'                 ACES_directory="~/../Desktop/Inventories/ACES_v2.0",
#'                 vulcan_directory="~/../Desktop/Inventories/Vulcan_v3.0",
#'                 ACES_year=2017,
#'                 vulcan_band=6,
#'                 County_Tigerlines=vect("~/../Desktop/County_Tigerlines/tl_2018_us_county.shp"),
#'                 focus_city_tigerlines=focus_city,
#'                 plot_directory="~/../Desktop/plots/")
#'@author Joe Pitt, \email{madeup@@wisc.edu}
#'@author Kris Hajny, \email{blank@@fake.edu}
#'@author Israel Lopez-Coto, \email{test@@test.edu}
#'@reference \href{https://doi.org/10.1021/acs.est.0c00437}{Weller et al.}
#'@reference \href{https://doi.org/10.1021/acs.est.8b03217}{Fischer et al.}
#'@reference \href{https://doi.org/10.1029/2020JD032974}{Vulcan}
#'@reference \href{https://doi.org/10.1002/2017JD027359}{ACES}

#'@export


## NG_distribution_emissions_r3.R
## In use: 2022-01-28 17:00
#
# Spatially allocate the various NG distribution (and residential post-meter) emission subsectors
# using sectoral CO2 emissions from either Vulcan or ACES as a spatial proxy.
# For both Vulcan and ACES, produce three maps by disaggregating emissions from the:
#     - individual company total
#     - state total
#     - domain total

NG_distribution <- function(domain,
                            state_name_list,
                            output_directory,
                            inventory_year,
                            verbose,
                            ghgrp_facility_info,
                            HIFLD_file,
                            EIA_file,
                            PHMSA_file,
                            GHGRP_file,
                            GHGI_file,
                            GHGI_EF_sheet,
                            GHGI_Activity_sheet,
                            GHGI_MnR,
                            GHGI_maintenance,
                            GHGI_meters,
                            GHGI_services,
                            State_Tigerlines,
                            NG_distribution_by_LDC,
                            NG_distribution_by_state,
                            NG_distribution_by_domain,
                            GHGI_natural_gas_pipeline_emission_factors,
                            natural_gas_post_meter_emission_factor,
                            Use_ACES,
                            Use_Vulcan,
                            ACES_directory,
                            vulcan_directory,
                            ACES_year,
                            vulcan_band,
                            plot_directory,
                            County_Tigerlines,
                            focus_city_tigerlines){
  
  
  starttime <- Sys.time()
  cat("Starting natural gas distribution sector: NG_distribution_emissions\n")
  
  if(!NG_distribution_by_LDC){
    ################################################################################
    #load in and filter the various files, excluding the GHGI one for now
    
    # Load the EIA company-level data for 2019 - this may have been edited to add a
    # dummy 'OTHER' entry
    EIA_csv <- read_xlsx(EIA_file,skip=1,col_names = T)
    # Load the PHMSA data for 2019 - this file may have had company ID's edited to
    # be consistent with EIA for the states that we will use
    PHMSA_csv <- read_xlsx(PHMSA_file,skip=2,col_names = T)
    
    # Filter the PHMSA file by commodity
    PHMSA_csv_NG <- PHMSA_csv[which(PHMSA_csv$COMMODITY == 'Natural Gas'),]
    
    #filter to only those for the relevant states and those with a company ID in
    #HIFLD (present at all).
    PHMSA_csv_NG <- PHMSA_csv_NG[which(PHMSA_csv_NG$STOP%in%state_name_list),]
    
    ################################################################################
    #Download the relevant ghgrp emissions data using the API
    #(https://www.epa.gov/enviro/envirofacts-data-service-api) and combine the
    #facility and emission data appropriately
    
    #download the relevant LDC-sector data
    #(https://www.epa.gov/enviro/greenhouse-gas-model).  
    ghgrp_w_only_emissions <- fromJSON("https://data.epa.gov/efservice/ef_w_emissions_source_ghg/JSON")
    
    #because we're getting sub-facility level information for transmission
    #compressor, first need to aggregate.  Subsetting to only the year of interest
    #now instead of later.
    ghgrp_w_only_emissions <- ghgrp_w_only_emissions[ghgrp_w_only_emissions$reporting_year==inventory_year,]
    ghgrp_w_only_emissions <- ghgrp_w_only_emissions[ghgrp_w_only_emissions$industry_segment=="Natural gas distribution [98.230(a)(8)]",]
    ghgrp_w_only_emissions <- aggregate(ghgrp_w_only_emissions$total_reported_ch4_emissions,
                                        by=list(ghgrp_w_only_emissions$facility_id,
                                                ghgrp_w_only_emissions$facility_name),
                                        sum,na.rm=T)
    
    #Now name the aggregated columns for clarity
    colnames(ghgrp_w_only_emissions) <- c("facility_id","facility_name","Reported_CH4")
    #and remove those that have 0 emissions for this category
    ghgrp_w_only_emissions <- ghgrp_w_only_emissions[ghgrp_w_only_emissions$Reported_CH4>0,]
    ################################################################################
    #Merge with location-like data
    
    #subset to the desired year
    ghgrp_facility_info <- ghgrp_facility_info[ghgrp_facility_info$year==inventory_year,]
    
    #combine the datasets by ID
    GHGRP_csv <- merge(ghgrp_facility_info,ghgrp_w_only_emissions,
                       by="facility_id")
    
    #cleanup a column that is in all GHRGP datasets and are identical (or ~so).
    GHGRP_csv$facility_name <- GHGRP_csv$facility_name.x
    GHGRP_csv[,c("facility_name.x","facility_name.y")] <- NULL
    
    #convert the relevant columns to numeric class
    GHGRP_csv[,c("latitude","longitude","Reported_CH4")] <- apply(GHGRP_csv[,c("latitude","longitude","Reported_CH4")],
                                                                  2,FUN=function(x){as.numeric(x)})
    
    #now we need to adjust the GHGRP data as some LDC's provide their
    #headquarters, not the location of operation.  Use facility names to correct
    #(e.g., Atmos Energy Corporation - Kentucky has headquarters in TX, but
    #operates in KY).
    GHGRP_csv[,"operating_state"]=GHGRP_csv$state
    GHGRP_csv[,"operating_state_name"]=GHGRP_csv$state_name
    for(A in 1:50){
      #CO and WA are a bit special.  CO often just means company, not
      #Colorado.  Washington can refer to DC or the state.
      if(A == 6){
        #search for state name anywhere in the facility name (\\b = whole word)
        match_indx <- grepl(pattern = paste0("\\b",state.name[A],"\\b"),x=GHGRP_csv$facility_name,ignore.case = T) & GHGRP_csv$state!=state.abb[A]
      }else if(A==47){
        #search for state name or abbreviation, but state name must be
        #immediately after a dash (several in/near DC have Washington in them)
        match_indx <- (grepl(pattern = paste0("- \\b",state.name[A],"\\b"),x=GHGRP_csv$facility_name,ignore.case = T) | 
                         grepl(pattern = paste0("\\b",state.abb[A],"\\b"),x=GHGRP_csv$facility_name,ignore.case = T)) & GHGRP_csv$state!=state.abb[A]
      }else{
        #search state name or abbreviation
        match_indx <- (grepl(pattern = paste0("\\b",state.name[A],"\\b"),x=GHGRP_csv$facility_name,ignore.case = T) | 
                         grepl(pattern = paste0("\\b",state.abb[A],"\\b"),x=GHGRP_csv$facility_name,ignore.case = T)) & GHGRP_csv$state!=state.abb[A]
      }
      #alert user of any updates, only if the state either was in the domain, or
      #is being updated to be in the domain
      if(sum(match_indx)>0 & (state.abb[A] %in% state_name_list | any(GHGRP_csv$state[match_indx] %in% state_name_list))){
        cat(paste(GHGRP_csv$facility_name[match_indx],collapse="  &  "),"rewritten from",paste(GHGRP_csv$state_name[match_indx],collapse="  &  "),"to",state.name[A],"\n")
      }
      GHGRP_csv[match_indx,"operating_state"]=state.abb[A]
      GHGRP_csv[match_indx,"operating_state_name"]=state.name[A]
    }
    
    #filter to the states in the domain
    GHGRP_csv <- GHGRP_csv[GHGRP_csv$operating_state %in% state_name_list,]
    
    #delete all tempfiles and clean up working environment
    # rm(A,data_URLs,ghgrp_facility_info,ghgrp_w_only_emissions,match_indx,ghgrp_NN_data)
    ################################################################################
    #do some webscraping to add a few additional variables for GHGRP facilities
    
    download_dest <- tempfile(fileext = ".html")
    GHGRP_csv[,c("Miles_of_Mains","N_of_above_grade_T-D_transfer_stations","N_of_above_grade_non_T-D_MR_stations",
                 "N_of_below_grade_T-D_transfer_stations","N_of_below_grade_non_T-D_MR_stations")] <- 0
    #save to the temp file destination.  Add several new variables to GHGRP_csv
    
    for(A in 1:nrow(GHGRP_csv)){
      counter = 0
      repeat{
        counter=counter+1
        info=tryCatch(
          #the url is build from the GHGRP ID, the desired year, and a common url.
          #This file contains more information about the facility that isn't in the
          #downloaded file.
          download.file(paste0("https://ghgdata.epa.gov/ghgp/service/html/",inventory_year,"?id=",GHGRP_csv$facility_id[A],"&et=undefined"),
                        destfile=download_dest,quiet = T),
          warning = function(w) {
            Sys.sleep(1)
            NA
          },
          error = function(e) {
            Sys.sleep(1)
            NA
          }
        )
        if(!is.na(info)) {
          break
        }
        if(counter>=10){
          stop("Failed to download ",GHGRP_csv$facility_name.x[A]," data from\n",
               paste0("https://ghgdata.epa.gov/ghgp/service/html/",inventory_year,"?id=",GHGRP_csv$facility_id[A],"&et=undefined\n"),
               "The links used may no longer be accurate.  Check the GHGRP FLIGHT website.")
        }
      }
      #try to download the url, and retry up to 10x with 1s between runs as the link
      #seems to fail on occasion.
      #from https://stackoverflow.com/a/60880960
      
      HTML_data <- readChar(download_dest,file.info(download_dest)$size)
      #Now read in the whole html as text
      
      text_loc <- gregexpr("Distribution Mains, Gas Service",text = HTML_data)
      answer <- 0
      #initialize an output and locate some text near data we want (amount of
      #pipeline of various pipe types)
      for(B in 1:length(text_loc[[1]])){
        #should have found 1 value for each type of pipeline
        text <- substr(HTML_data,text_loc[[1]][B],text_loc[[1]][B]+attributes(text_loc[[1]])$match.length[B]+200)
        sub_answer <- regexpr(">[[:digit:]]*[[:punct:]]*[[:digit:]]*</td>",text)
        #see https://www.debuggex.com/cheatsheet/regex/pcre
        answer <- answer+as.numeric(substr(text,sub_answer+1,sub_answer+attributes(sub_answer)$match.length-6))
        #first subset to the located text + buffer, then regex to find a number
        #with/without a decimal in it as formatted html text, then grab just this
        #value and add it to the answer (we only want the total across all pipeline
        #types)
      }
      GHGRP_csv$Miles_of_Mains[A] <- answer
      
      #now repeat the same type of process for various other variables
      text_loc <- regexpr("Number of above grade T-D transfer stations at the facility",text = HTML_data)
      text <- substr(HTML_data,text_loc,text_loc+attributes(text_loc)$match.length+50)
      answer <- regexpr(">[[:digit:]]*[[:punct:]]*[[:digit:]]*</td>",text)
      answer <- substr(text,answer+1,answer+attributes(answer)$match.length-6)
      GHGRP_csv$'N_of_above_grade_T-D_transfer_stations'[A] <- as.numeric(answer)
      
      text_loc <- regexpr("Number of above grade metering-regulating stations that are not T-D transfer stations",text = HTML_data)
      text <- substr(HTML_data,text_loc,text_loc+attributes(text_loc)$match.length+50)
      answer <- regexpr(">[[:digit:]]*[[:punct:]]*[[:digit:]]*</td>",text)
      answer <- substr(text,answer+1,answer+attributes(answer)$match.length-6)
      GHGRP_csv$'N_of_above_grade_non_T-D_MR_stations'[A] <- as.numeric(answer)
      
      text_loc <- gregexpr("Below Grade T-D Station, Gas Service, Inlet Pressure ",text = HTML_data)
      answer <- 0
      for(B in 1:length(text_loc[[1]])){
        text <- substr(HTML_data,text_loc[[1]][B],text_loc[[1]][B]+attributes(text_loc[[1]])$match.length[B]+200)
        sub_answer <- regexpr(">[[:digit:]]*[[:punct:]]*[[:digit:]]*</td>",text)
        answer <- answer+as.numeric(substr(text,sub_answer+1,sub_answer+attributes(sub_answer)$match.length-6))
      }
      GHGRP_csv$'N_of_below_grade_T-D_transfer_stations'[A] <- answer
      
      text_loc <- gregexpr("Below Grade M-R Station, Gas Service, Inlet Pressure",text = HTML_data)
      answer <- 0
      for(B in 1:length(text_loc[[1]])){
        text <- substr(HTML_data,text_loc[[1]][B],text_loc[[1]][B]+attributes(text_loc[[1]])$match.length[B]+200)
        sub_answer <- regexpr(">[[:digit:]]*[[:punct:]]*[[:digit:]]*</td>",text)
        answer <- answer+as.numeric(substr(text,sub_answer+1,sub_answer+attributes(sub_answer)$match.length-6))
      }
      GHGRP_csv$'N_of_below_grade_non_T-D_MR_stations'[A] <- answer
      
      cat("\rFinished downloading GHGRP data for",A,"of",nrow(GHGRP_csv),"companies                 ")
      #user update
    }
    
    #attempt to remove the downloaded html file
    unlink(download_dest)
    
    #same process, but using GHGRP Miles of mains
    GHGRP_csv$above_grade_stations_per_mile_of_pipe <- (GHGRP_csv$'N_of_above_grade_T-D_transfer_stations'+
                                                          GHGRP_csv$'N_of_above_grade_non_T-D_MR_stations')/
      GHGRP_csv$Miles_of_Mains
    GHGRP_csv$below_grade_stations_per_mile_of_pipe <- (GHGRP_csv$'N_of_below_grade_T-D_transfer_stations'+
                                                          GHGRP_csv$'N_of_below_grade_non_T-D_MR_stations')/
      GHGRP_csv$Miles_of_Mains
    GHGRP_csv$stations_per_mile_of_pipe <- (GHGRP_csv$'N_of_above_grade_T-D_transfer_stations'+
                                              GHGRP_csv$'N_of_above_grade_non_T-D_MR_stations'+
                                              GHGRP_csv$'N_of_below_grade_T-D_transfer_stations'+
                                              GHGRP_csv$'N_of_below_grade_non_T-D_MR_stations')/
      GHGRP_csv$Miles_of_Mains
    
    rm(A,B,text_loc,answer,download_dest,sub_answer,HTML_data,text,info,counter)
    ##############################################################################
    #have to calculate before aggregating/merging via sum since it applies an
    #average term
    
    # We're going to need the total miles of pipeline (inc. services) -
    # calculate that here from AVERAGE_LENGTH (converting ft to miles)
    PHMSA_csv_NG$Miles_main_and_serv <- PHMSA_csv_NG$MMILES_TOTAL + 
      PHMSA_csv_NG$NUM_SRVCS_TOTAL*PHMSA_csv_NG$AVERAGE_LENGTH/5280
    ################################################################################
    #merge the files.  Not including HIFLD or GHGRP data if not
    #calculating by LDC
    
    # Then select the columns we need and aggregate the entries which share the same company ID or state
    PHMSA_cols_to_keep <- paste0("PHMSA_",c('MMILES_STEEL_UNP_BARE',
                                            'MMILES_STEEL_UNP_COATED',
                                            'MMILES_STEEL_CP_BARE',
                                            'MMILES_STEEL_CP_COATED',
                                            'MMILES_PLASTIC',
                                            'MMILES_CI',
                                            'MMILES_DI',
                                            'MMILES_CU',
                                            'MMILES_OTHER',
                                            'MMILES_RCI',
                                            'MMILES_TOTAL',
                                            'NUM_SRVS_STEEL_UNP_BARE',
                                            'NUM_SRVS_STEEL_UNP_COATED',
                                            'NUM_SRVS_STEEL_CP_BARE',
                                            'NUM_SRVS_STEEL_CP_COATED',
                                            'NUM_SRVS_PLASTIC',
                                            'NUM_SRVS_CI',
                                            'NUM_SRVS_DI',
                                            'NUM_SRVS_CU',
                                            'NUM_SRVS_OTHER',
                                            'NUM_SRVS_RCI',
                                            'NUM_SRVCS_TOTAL',
                                            "Miles_main_and_serv"))
    
    EIA_cols_to_keep <- paste0("EIA_",c("Residential_Total_Volume_(Mcf)",
                                        "Residential_Total_Customers",
                                        'Commercial_Total_Volume_(Mcf)',
                                        'Commercial_Total_Customers',
                                        'Industrial_Total_Volume_(Mcf)',
                                        'Industrial_Total_Customers',
                                        'Electric_Total_Volume_(Mcf)',
                                        'Electric_Total_Customers'))
    
    cols_to_keep <- c(EIA_cols_to_keep,
                      PHMSA_cols_to_keep,
                      names(State_Tigerlines))
    
    #first rename all data to make it obvious where it came from - much easier to
    #understand where things come from later
    colnames(PHMSA_csv_NG) <- paste0("PHMSA_",gsub(" ","_",colnames(PHMSA_csv_NG)))
    colnames(EIA_csv) <- paste0("EIA_",gsub(" ","_",colnames(EIA_csv)))
    
    
    #rename the state data for these for clarity/consistency
    colnames(PHMSA_csv_NG) <- gsub("STOP","State",colnames(PHMSA_csv_NG))
    
    PHMSA_csv_NG_agg <- aggregate(PHMSA_csv_NG[PHMSA_cols_to_keep],
                                  list(PHMSA_State=PHMSA_csv_NG$PHMSA_State),
                                  sum,na.rm=T)
    EIA_csv_agg <- aggregate(EIA_csv[EIA_cols_to_keep],
                             list(EIA_State=EIA_csv$EIA_State),
                             sum,na.rm=T)
    
    # Now merge csv stuff and state shapefile together
    all_merge <- merge(EIA_csv_agg, PHMSA_csv_NG_agg, by.x='EIA_State', by.y='PHMSA_State')
    all_merge <- merge(State_Tigerlines,all_merge,by.x="STUSPS",by.y="EIA_State")
    
    # Clean up
    all_merge_clean <- all_merge[cols_to_keep]
    
    #just so the state variable is consistent with the byLDC version
    names(all_merge_clean) <- gsub("STUSPS","PHMSA_State",names(all_merge_clean))
    ############################################################################
    # M&R stations - can't use GHGRP data without matching facilities, so estimate
    # based on avg stations per mile for reporters in each state. Then split by
    # pressure and function assuming the same split as at the national level (from
    # the GHGI national inventory report).
    
    main_miles_ghgrp <- aggregate(GHGRP_csv$Miles_of_Mains,
                                  list(State=GHGRP_csv$operating_state),
                                  sum,
                                  na.rm=TRUE)
    above_grade_MnR <- aggregate((GHGRP_csv$`N_of_above_grade_T-D_transfer_stations` +
                                    GHGRP_csv$`N_of_above_grade_non_T-D_MR_stations`),
                                 list(State=GHGRP_csv$operating_state),
                                 sum,
                                 na.rm=TRUE)
    below_grade_MnR <- aggregate((GHGRP_csv$`N_of_below_grade_non_T-D_MR_stations` +
                                    GHGRP_csv$`N_of_below_grade_T-D_transfer_stations`),
                                 list(State=GHGRP_csv$operating_state),
                                 sum,
                                 na.rm=TRUE)
    # Calculate average stations per mile in each state
    above_grade_MnR$stations_per_mile <- above_grade_MnR$x/main_miles_ghgrp$x
    below_grade_MnR$stations_per_mile <- below_grade_MnR$x/main_miles_ghgrp$x
    
    # allocate average stations per mile in each state to all facilities if not
    # calculating by LDC
    state_indx <- match(all_merge_clean$PHMSA_State,above_grade_MnR$State)
    all_merge_clean$GHGRP_MnR_above <- all_merge_clean$PHMSA_MMILES_TOTAL*above_grade_MnR$stations_per_mile[state_indx]
    all_merge_clean$GHGRP_MnR_below <- all_merge_clean$PHMSA_MMILES_TOTAL*below_grade_MnR$stations_per_mile[state_indx]
    
    cat("\nFinished downloading and merging all input data at",difftime(Sys.time(),starttime,units = "min"),"minutes since start\n")
  }else{
    ############################################################################
    #load in the output of NG_distribution_by_LDC_prep.R.  Note that is a
    #script, not a function, as it requires some manual efforts.  This one is
    #calculated at the LDC scale, not state level.
    all_merge_clean <- vect(file.path(input_directory,"/byLDC_merged/byLDC_merged.shp"))
    names(all_merge_clean) <- unlist(read.table(file.path(input_directory,"/byLDC_merged/colnames.txt")))
  }
  ################################################################################
  #Pull the GHGI data we'll need later and save it to a few dataframes
  
  first_col <- which(read_xlsx(GHGI_file,sheet = GHGI_Activity_sheet,.name_repair = "minimal")[,1]=="Segment/Source")
  GHGI_p1 <- read_xlsx(GHGI_file,sheet = GHGI_Activity_sheet,skip=first_col,col_names = T)
  
  first_col <- which(read_xlsx(GHGI_file,sheet = GHGI_EF_sheet,.name_repair = "minimal")[,1]=="Segment/Source")
  GHGI_p2 <- read_xlsx(GHGI_file,sheet = GHGI_EF_sheet,skip=first_col,col_names = T)
  #p2 = emission factors, p1 = activity data.  Columns = year, rows = various
  #types of sources.  First col is just to identify the first column of useable
  #data
  
  if(all(GHGI_MnR=="GHGI")){
    Data_list <- c("M&R >300","M&R 100-300","M&R <100","Reg >300","R-Vault >300",
                   "Reg 100-300","R-Vault 100-300","Reg 40-100","R-Vault 40-100",
                   "Reg <40")
    #all the sources we're looking for, written exactly as in the GHGI file
    
    GHGI_MnR <- data.frame("Type"=Data_list,
                           "EF"=as.numeric(unlist(sapply(Data_list,FUN=function(x){GHGI_p2[GHGI_p2[,1]==x,as.character(inventory_year)]})))*
                             1000/(16.043*60*60*24*365),#convert from kg/yr to mol/s
                           "Total_stations"=as.numeric(unlist(sapply(Data_list,FUN=function(x){GHGI_p1[GHGI_p1[,1]==x,as.character(inventory_year)]}))),
                           row.names = NULL)
    #use sapply to find the row using data list, specify the column as the year and
    #grab the relevant EF and activity data into a dataframe.
  }
  
  if(all(GHGI_services=="GHGI")){
    #repeat for several other source types
    Data_list <- c("Services - Unprotected steel",
                   "Services Protected steel",
                   "Services - Plastic",
                   "Services - Copper")
    
    GHGI_services <- data.frame("Type"=Data_list,
                                "EF"=as.numeric(unlist(sapply(Data_list,FUN=function(x){GHGI_p2[GHGI_p2[,1]==x,as.character(inventory_year)]})))*
                                  1000/(16.043*60*60*24*365),#convert from kg/yr to mol/s
                                row.names = NULL)
  }
  
  if(all(GHGI_meters=="GHGI")){
    Data_list <- c("Residential",
                   "Commercial",
                   "Industrial")
    
    GHGI_meters <- data.frame("Type"=Data_list,
                              "EF"=as.numeric(unlist(sapply(Data_list,FUN=function(x){GHGI_p2[which(GHGI_p2[,1]==x)[1],as.character(inventory_year)]})))*
                                1000/(16.043*60*60*24*365),#convert from kg/yr to mol/s
                              row.names = NULL)
  }
  
  if(all(GHGI_maintenance=="GHGI")){
    Data_list <- c("Pressure Relief Valve Releases",
                   "Pipeline Blowdown",
                   "Mishaps (Dig-ins)")
    
    GHGI_maintenance <- data.frame("Type"=Data_list,
                                   "EF"=as.numeric(unlist(sapply(Data_list,FUN=function(x){GHGI_p2[GHGI_p2[,1]==x,as.character(inventory_year)]})))*
                                     1000/(16.043*60*60*24*365),#convert from kg/yr to mol/s
                                   row.names = NULL)
  }
  
  suppressWarnings(rm(GHGI_p1,GHGI_p2,Data_list,GHGI_file,first_col))
  ##############################################################################
  #convert a lot of the activity data to emissions data
  
  #Mains using EFs from Weller et al., or as specified in config
  all_merge_clean$bare_steel_mains_ER <- (rowSums(as.data.frame(all_merge_clean)[,c("PHMSA_MMILES_STEEL_UNP_BARE","PHMSA_MMILES_STEEL_CP_BARE","PHMSA_MMILES_CU")],
                                                  na.rm=T)*
                                            GHGI_natural_gas_pipeline_emission_factors[1,"Leaks_per_mile"]*
                                            GHGI_natural_gas_pipeline_emission_factors[1,"Avg_emissions_mol_per_s"])
  all_merge_clean$iron_mains_ER <- (rowSums(as.data.frame(all_merge_clean)[,c("PHMSA_MMILES_CI","PHMSA_MMILES_DI","PHMSA_MMILES_RCI")],
                                            na.rm=T)*
                                      GHGI_natural_gas_pipeline_emission_factors[2,"Leaks_per_mile"]*
                                      GHGI_natural_gas_pipeline_emission_factors[2,"Avg_emissions_mol_per_s"])
  all_merge_clean$coat_steel_mains_ER <- (rowSums(as.data.frame(all_merge_clean)[,c("PHMSA_MMILES_STEEL_UNP_COATED","PHMSA_MMILES_STEEL_CP_COATED","PHMSA_MMILES_OTHER")],
                                                  na.rm=T)*
                                            GHGI_natural_gas_pipeline_emission_factors[3,"Leaks_per_mile"]*
                                            GHGI_natural_gas_pipeline_emission_factors[3,"Avg_emissions_mol_per_s"])
  all_merge_clean$plastic_mains_ER <- (all_merge_clean$PHMSA_MMILES_PLASTIC*
                                         GHGI_natural_gas_pipeline_emission_factors[4,"Leaks_per_mile"]*
                                         GHGI_natural_gas_pipeline_emission_factors[4,"Avg_emissions_mol_per_s"])
  
  # Services using EFs from the EPA GHGI, or national inventory report
  all_merge_clean$UNP_steel_serv_ER <- (rowSums(as.data.frame(all_merge_clean)[,c("PHMSA_NUM_SRVS_STEEL_UNP_COATED","PHMSA_NUM_SRVS_STEEL_UNP_BARE")],
                                                na.rm=T)*
                                          GHGI_services$EF[1])
  all_merge_clean$CP_steel_serv_ER <- (rowSums(as.data.frame(all_merge_clean)[,c("PHMSA_NUM_SRVS_STEEL_CP_BARE","PHMSA_NUM_SRVS_STEEL_CP_COATED","PHMSA_NUM_SRVS_OTHER")],
                                               na.rm=T)*
                                         GHGI_services$EF[2])
  all_merge_clean$plastic_serv_ER <- (all_merge_clean$PHMSA_NUM_SRVS_PLASTIC*
                                        GHGI_services$EF[3])
  all_merge_clean$copper_serv_ER <- (rowSums(as.data.frame(all_merge_clean)[,c("PHMSA_NUM_SRVS_CU","PHMSA_NUM_SRVS_CI","PHMSA_NUM_SRVS_DI","PHMSA_NUM_SRVS_RCI")],
                                             na.rm=T)*
                                       GHGI_services$EF[4])
  
  #split by function/pressure
  GHGI_MnR_above <- sum(GHGI_MnR$Total_stations[-grep('Vault', GHGI_MnR$Type)])
  GHGI_MnR_below <- sum(GHGI_MnR$Total_stations[grep('Vault', GHGI_MnR$Type)])
  
  # Estimate emissions by function/pressure
  all_merge_clean$MnR_HiP_ER <- (all_merge_clean$GHGRP_MnR_above*                                                    # Abv grade stations
                                   GHGI_MnR$Total_stations[which(GHGI_MnR$Type == 'M&R >300')]/GHGI_MnR_above* # Type fraction
                                   GHGI_MnR$EF[which(GHGI_MnR$Type == 'M&R >300')]) # Emission factor
  
  all_merge_clean$MnR_MidP_ER <- (all_merge_clean$GHGRP_MnR_above*
                                    GHGI_MnR$Total_stations[which(GHGI_MnR$Type == 'M&R 100-300')]/GHGI_MnR_above*
                                    GHGI_MnR$EF[which(GHGI_MnR$Type == 'M&R 100-300')])
  
  all_merge_clean$MnR_LoP_ER <- (all_merge_clean$GHGRP_MnR_above*
                                   GHGI_MnR$Total_stations[which(GHGI_MnR$Type == 'M&R <100')]/GHGI_MnR_above*
                                   GHGI_MnR$EF[which(GHGI_MnR$Type == 'M&R <100')])
  
  all_merge_clean$Reg_HiP_ER <- (all_merge_clean$GHGRP_MnR_above*
                                   GHGI_MnR$Total_stations[which(GHGI_MnR$Type == 'Reg >300')]/GHGI_MnR_above*
                                   GHGI_MnR$EF[which(GHGI_MnR$Type == 'Reg >300')])
  
  all_merge_clean$Reg_MidP_ER <- (all_merge_clean$GHGRP_MnR_above*
                                    GHGI_MnR$Total_stations[which(GHGI_MnR$Type == 'Reg 100-300')]/GHGI_MnR_above*
                                    GHGI_MnR$EF[which(GHGI_MnR$Type == 'Reg 100-300')])
  
  all_merge_clean$Reg_LoP_ER <- (all_merge_clean$GHGRP_MnR_above*
                                   GHGI_MnR$Total_stations[which(GHGI_MnR$Type == 'Reg 40-100')]/GHGI_MnR_above*
                                   GHGI_MnR$EF[which(GHGI_MnR$Type == 'Reg 40-100')])
  
  all_merge_clean$Reg_VLP_ER <- (all_merge_clean$GHGRP_MnR_above*
                                   GHGI_MnR$Total_stations[which(GHGI_MnR$Type == 'Reg <40')]/GHGI_MnR_above*
                                   GHGI_MnR$EF[which(GHGI_MnR$Type == 'Reg <40')])
  
  all_merge_clean$RegV_HiP_ER <- (all_merge_clean$GHGRP_MnR_below*
                                    GHGI_MnR$Total_stations[which(GHGI_MnR$Type == 'R-Vault >300')]/GHGI_MnR_below*
                                    GHGI_MnR$EF[which(GHGI_MnR$Type == 'R-Vault >300')])
  
  all_merge_clean$RegV_MidP_ER <- (all_merge_clean$GHGRP_MnR_below*
                                     GHGI_MnR$Total_stations[which(GHGI_MnR$Type == 'R-Vault 100-300')]/GHGI_MnR_below*
                                     GHGI_MnR$EF[which(GHGI_MnR$Type == 'R-Vault 100-300')])
  
  all_merge_clean$RegV_LoP_ER <- (all_merge_clean$GHGRP_MnR_below*
                                    GHGI_MnR$Total_stations[which(GHGI_MnR$Type == 'R-Vault 40-100')]/GHGI_MnR_below*
                                    GHGI_MnR$EF[which(GHGI_MnR$Type == 'R-Vault 40-100')])
  
  # Consumer meters - use emission factors from the EPA national inventory report, also known as the GHGI
  all_merge_clean$Res_meter_ER <- all_merge_clean$EIA_Residential_Total_Customers*GHGI_meters$EF[1]
  all_merge_clean$Com_meter_ER <- all_merge_clean$EIA_Commercial_Total_Customers*GHGI_meters$EF[2]
  all_merge_clean$Ind_meter_ER <- all_merge_clean$EIA_Industrial_Total_Customers*GHGI_meters$EF[3]
  
  # Maintenance and upsets
  all_merge_clean$Relief_valve_ER <- all_merge_clean$PHMSA_MMILES_TOTAL*GHGI_maintenance$EF[1]
  all_merge_clean$Blowdown_ER <- all_merge_clean$PHMSA_Miles_main_and_serv*GHGI_maintenance$EF[2]
  all_merge_clean$Mishap_ER <- all_merge_clean$PHMSA_Miles_main_and_serv*GHGI_maintenance$EF[3]
  
  # Post-meter, in this case it's entirely allocated to residential (no data on
  # commercial buildings)
  all_merge_clean$post_meter_ER_total_res <- all_merge_clean$`EIA_Residential_Total_Volume_(Mcf)`*1000*natural_gas_post_meter_emission_factor
  #McF = thousand cubic ft
  ##############################################################################
  #break the emissions into residential and commercial fractions
  
  # Calculate the total mains emissions to be distributed according to
  # residential and commercial CO2 emissions This is calculated for each company
  # according to the ratio of residential:commercial customers Industrial
  # customer numbers are much smaller, so we ignore these here
  all_merge_clean$mains_ER_total_res <- ((all_merge_clean$bare_steel_mains_ER + 
                                            all_merge_clean$iron_mains_ER +
                                            all_merge_clean$coat_steel_mains_ER +
                                            all_merge_clean$plastic_mains_ER)*
                                           all_merge_clean$EIA_Residential_Total_Customers/
                                           (all_merge_clean$EIA_Residential_Total_Customers + all_merge_clean$EIA_Commercial_Total_Customers))
  
  all_merge_clean$mains_ER_total_com <- ((all_merge_clean$bare_steel_mains_ER + 
                                            all_merge_clean$iron_mains_ER +
                                            all_merge_clean$coat_steel_mains_ER +
                                            all_merge_clean$plastic_mains_ER)*
                                           all_merge_clean$EIA_Commercial_Total_Customers/
                                           (all_merge_clean$EIA_Residential_Total_Customers + all_merge_clean$EIA_Commercial_Total_Customers))
  
  all_merge_clean$serv_ER_total_res <- ((all_merge_clean$UNP_steel_serv_ER + 
                                           all_merge_clean$CP_steel_serv_ER +
                                           all_merge_clean$plastic_serv_ER +
                                           all_merge_clean$copper_serv_ER)*
                                          all_merge_clean$EIA_Residential_Total_Customers/
                                          (all_merge_clean$EIA_Residential_Total_Customers + all_merge_clean$EIA_Commercial_Total_Customers))
  
  all_merge_clean$serv_ER_total_com <- ((all_merge_clean$UNP_steel_serv_ER + 
                                           all_merge_clean$CP_steel_serv_ER +
                                           all_merge_clean$plastic_serv_ER +
                                           all_merge_clean$copper_serv_ER)*
                                          all_merge_clean$EIA_Commercial_Total_Customers/
                                          (all_merge_clean$EIA_Residential_Total_Customers + all_merge_clean$EIA_Commercial_Total_Customers))
  
  all_merge_clean$MnR_ER_total_res <- ((all_merge_clean$MnR_HiP_ER + 
                                          all_merge_clean$MnR_MidP_ER +
                                          all_merge_clean$MnR_LoP_ER +
                                          all_merge_clean$Reg_HiP_ER +
                                          all_merge_clean$Reg_MidP_ER +
                                          all_merge_clean$Reg_LoP_ER +
                                          all_merge_clean$Reg_VLP_ER +
                                          all_merge_clean$RegV_HiP_ER +
                                          all_merge_clean$RegV_MidP_ER +
                                          all_merge_clean$RegV_LoP_ER)*
                                         all_merge_clean$EIA_Residential_Total_Customers/
                                         (all_merge_clean$EIA_Residential_Total_Customers + all_merge_clean$EIA_Commercial_Total_Customers))
  
  all_merge_clean$MnR_ER_total_com <- ((all_merge_clean$MnR_HiP_ER + 
                                          all_merge_clean$MnR_MidP_ER +
                                          all_merge_clean$MnR_LoP_ER +
                                          all_merge_clean$Reg_HiP_ER +
                                          all_merge_clean$Reg_MidP_ER +
                                          all_merge_clean$Reg_LoP_ER +
                                          all_merge_clean$Reg_VLP_ER +
                                          all_merge_clean$RegV_HiP_ER +
                                          all_merge_clean$RegV_MidP_ER +
                                          all_merge_clean$RegV_LoP_ER)*
                                         all_merge_clean$EIA_Commercial_Total_Customers/
                                         (all_merge_clean$EIA_Residential_Total_Customers + all_merge_clean$EIA_Commercial_Total_Customers))
  
  # We could allocate the industrial meter emissions by ACES and Vulcan industrial sector
  # But this sector is dominated by a handful of large point sources, many of which don't even use natural gas
  # So instead, share these emissions out between the residential and commercial CO2 maps
  # Split according to the ratio of Res_meter_ER:Com_meter_ER - could equally have split according to the number of
  # customers, but that would shift the ratio of total meter emissions towards residential, which doesn't seem desirable
  # Keep the same naming convention as for the other subsectors (i.e. _total_res) even though it makes less sense here
  all_merge_clean$meter_ER_total_res <- (all_merge_clean$Res_meter_ER +
                                           all_merge_clean$Ind_meter_ER*
                                           all_merge_clean$Res_meter_ER/
                                           (all_merge_clean$Res_meter_ER + all_merge_clean$Com_meter_ER))
  
  all_merge_clean$meter_ER_total_com <- (all_merge_clean$Com_meter_ER +
                                           all_merge_clean$Ind_meter_ER*
                                           all_merge_clean$Com_meter_ER/
                                           (all_merge_clean$Res_meter_ER + all_merge_clean$Com_meter_ER))
  
  
  all_merge_clean$upset_ER_total_res <- ((all_merge_clean$Relief_valve_ER + 
                                            all_merge_clean$Blowdown_ER +
                                            all_merge_clean$Mishap_ER)*
                                           all_merge_clean$EIA_Residential_Total_Customers/
                                           (all_merge_clean$EIA_Residential_Total_Customers + all_merge_clean$EIA_Commercial_Total_Customers))
  
  all_merge_clean$upset_ER_total_com <- ((all_merge_clean$Relief_valve_ER + 
                                            all_merge_clean$Blowdown_ER +
                                            all_merge_clean$Mishap_ER)*
                                           all_merge_clean$EIA_Commercial_Total_Customers/
                                           (all_merge_clean$EIA_Residential_Total_Customers + all_merge_clean$EIA_Commercial_Total_Customers))
  
  cat("Finished calculating emissions and distributing to residential/commercial portions at",difftime(Sys.time(),starttime,units = "min"),"minutes since start\n")
  ################################################################################
  #Load in ACES/Vulcan 
  
  #the various subsectors
  res_totals <- c('mains_ER_total_res',
                  'serv_ER_total_res',
                  'MnR_ER_total_res',
                  'meter_ER_total_res',
                  'upset_ER_total_res',
                  'post_meter_ER_total_res')
  
  com_totals <- c('mains_ER_total_com',
                  'serv_ER_total_com',
                  'MnR_ER_total_com',
                  'meter_ER_total_com',
                  'upset_ER_total_com')
  
  # Load in ACES and Vulcan sectors - these are in different units, but it
  # doesn't matter as we'll only use fractions
  if(Use_ACES){
    aces_res <- rast(paste0(ACES_directory,"/Sectoral/",ACES_year,'_Annual_ACES_Residential.nc'))
    aces_res <- flip(aces_res)
    crs(aces_res) <- "+proj=lcc +lat_0=40 +lon_0=-97 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
    
    aces_com <- rast(paste0(ACES_directory,"/Sectoral/",ACES_year,'_Annual_ACES_Commercial.nc'))
    aces_com <- flip(aces_com)
    crs(aces_com) <- "+proj=lcc +lat_0=40 +lon_0=-97 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
  }
  if(Use_Vulcan){
    vu_res <- rast(paste0(vulcan_directory,"/Sectoral/","Vulcan_v3_US_annual_1km_residential_mn.nc4"), subds='carbon_emissions', lyrs=vulcan_band)
    vu_com <- rast(paste0(vulcan_directory,"/Sectoral/",'Vulcan_v3_US_annual_1km_commercial_mn.nc4'), subds='carbon_emissions', lyrs=vulcan_band)
  }
  
  ################################################################################
  #use ACES/Vulcan to redistribute residential/commercial emissions at the LDC
  #level
  if(NG_distribution_by_LDC){
    if(Use_ACES){
      # tempdata <- aces_res
      # values(tempdata) <- 0
      #convert to the proper crs
      all_merge_LCC <- project(all_merge_clean,aces_res)
      
      # all_merge_LCC <- all_merge_LCC[2,]
      # tempdata <- crop(tempdata,all_merge_LCC)
      # tempdata2 <- tempdata
      # tempdata3 <- tempdata
      # start <- Sys.time()
      # test1=cells(tempdata,all_merge_LCC,exact=T,touches=T)
      # tempdata[test1[,'cell']] <- test1[,"weights"]
      # end1 <- Sys.time()
      # 
      # 
      # test2=extract(tempdata,all_merge_LCC,weights=T,exact=T,cells=T)
      # tempdata2[test2[,'cell']] <- test2[,"weight"]
      # end2 <- Sys.time()
      # 
      # test3 <- cells(tempdata3,all_merge_LCC,exact=F,touches=T)
      # test3 <- cells(tempdata3[test3[,'cell'],drop=F],all_merge_LCC,exact=T,touches=T)
      # tempdata3[test3[,'cell']] <- test3[,"weights"]
      # end3 <- Sys.time()
      
      # LDC_count <- nrow(all_merge_LCC)
      # all_merge_LCC$count <- 1:nrow(all_merge_LCC)
      # cover_all <- all_merge_LCC %>% 
      #   split(f=all_merge_LCC$HIFLD_SVCTERID) %>%
      #   lapply(function(x){cells(aces_res,x,weights=T,exact=T);cat("\rProcessing",x$count,"of",LDC_count,"LDCs            ")})
      # 
      # aces_res_temp <- crop(aces_res,ext(project(State_Tigerlines,aces_res))*1.2) - untested impact
      # 
      LDC_count <- nrow(all_merge_LCC)
      if(LDC_count==1){
        cover_all <- list(extract(aces_res,all_merge_LCC,weights=T,exact=T,cells=T))
      }else{
        all_merge_LCC$count <- 1:nrow(all_merge_LCC)
        cover_all <- all_merge_LCC %>% 
        split(f=all_merge_LCC$HIFLD_SVCTERID) %>%
        lapply(function(x){cat("\rProcessing",x$count,"of",LDC_count,"LDCs using ACES                                   ");extract(aces_res,x,weights=T,exact=T,cells=T)})
      }
      
      disaggregation(aces_res,res_totals,agg_level="LDC",NEI_input = all_merge_LCC,cover_all,out_envir=environment())
      disaggregation(aces_com,com_totals,agg_level="LDC",NEI_input = all_merge_LCC,cover_all,out_envir=environment())
    }
    if(Use_Vulcan){
      all_merge_LCC <- project(all_merge_clean,vu_res)
      
      if(LDC_count==1){
        cover_all <- list(extract(vu_res,all_merge_LCC,weights=T,exact=T,cells=T))
      }else{
        LDC_count <- nrow(all_merge_LCC)
        all_merge_LCC$count <- 1:nrow(all_merge_LCC)
        cover_all <- all_merge_LCC %>% 
          split(f=all_merge_LCC$HIFLD_SVCTERID) %>%
          lapply(function(x){cat("\rProcessing",x$count,"of",LDC_count,"LDCs using vulcan                                  ");extract(vu_res,x,weights=T,exact=T,cells=T)})
      }
      
      disaggregation(vu_res,res_totals,agg_level="LDC",NEI_input = all_merge_LCC,cover_all,out_envir=environment())
      disaggregation(vu_com,com_totals,agg_level="LDC",NEI_input = all_merge_LCC,cover_all,out_envir=environment())
    }
    cat("\rFinished disaggregating emissions to pixels from the LDC scale at",difftime(Sys.time(),starttime,units = "min"),"minutes since start\n")
  }
  
  ################################################################################
  ## Now aggregate emissions at the state level and repeat
  # Side note - splitting into residential/commercial emissions at the company
  # level, then aggregating (as we do here) is probably more logical than
  # aggregating total emissions at the state level, then splitting into
  # residential/commercial This is obvious if you think of a situation where one
  # company dominates emissions, but another dominates consumers. In that case the
  # residential/commercial split should closely match that of the high emitting
  # company, not the high consumer company.
  
  if(NG_distribution_by_state){
    if(NG_distribution_by_LDC){
      all_merge_state <- aggregate(as.data.frame(all_merge_clean[,!(names(all_merge_clean) %in% c('HIFLD_SVCTERID', 'EIA_Company', 'EIA_Company_Name', 'PHMSA_State'))]),
                                   list(PHMSA_State=all_merge_clean$PHMSA_State),
                                   sum,na.rm=T)
      # Merge the geometries
      all_merge_state_poly <- merge(State_Tigerlines, all_merge_state, by.y='PHMSA_State', by.x='STUSPS')
      names(all_merge_state_poly) <- gsub("STUSPS","PHMSA_State",names(all_merge_state_poly))
    }else{
      all_merge_state_poly <- all_merge_clean
      all_merge_state <- as.data.frame(all_merge_clean)
    }
    
    
    if(Use_ACES){
      #convert state scale version to the proper crs
      all_merge_LCC_state <- project(all_merge_state_poly,aces_res)
      
      if(length(state_name_list)==1){
        cover_all <- list(extract(aces_res,all_merge_LCC_state,weights=T,exact=T,cells=T))
      }else{
        cover_all <- all_merge_LCC_state %>% 
          split(f=all_merge_LCC_state$STATEFP) %>%
          lapply(function(x){extract(aces_res,x,weights=T,exact=T,cells=T)})
      }
      
      disaggregation(aces_res,res_totals,agg_level="state",NEI_input = all_merge_LCC_state,cover_all,out_envir=environment())
      disaggregation(aces_com,com_totals,agg_level="state",NEI_input = all_merge_LCC_state,cover_all,out_envir=environment())
    }
    if(Use_Vulcan){
      all_merge_LCC_state <- project(all_merge_state_poly,vu_res)
      
      if(length(state_name_list)==1){
        cover_all <- list(extract(vu_res,all_merge_LCC_state,weights=T,exact=T,cells=T))
      }else{
        cover_all <- all_merge_LCC_state %>% 
          split(f=all_merge_LCC_state$STATEFP) %>%
          lapply(function(x){extract(vu_res,x,weights=T,exact=T,cells=T)})
      }
      
      disaggregation(vu_res,res_totals,agg_level="state",NEI_input = all_merge_LCC_state,cover_all,out_envir=environment())
      disaggregation(vu_com,com_totals,agg_level="state",NEI_input = all_merge_LCC_state,cover_all,out_envir=environment())
    }
  }
  cat("\rFinished disaggregating emissions to pixels from the state scale at",difftime(Sys.time(),starttime,units = "min"),"minutes since start\n")
  ################################################################################
  #Repeat when aggregated to the domain total.
  
  if(NG_distribution_by_domain){
    all_merge_domain <- suppressWarnings(apply(as.data.frame(all_merge_clean),2,as.numeric))
    all_merge_domain <- colSums(all_merge_domain)
    
    all_merge_domain_poly <- aggregate(State_Tigerlines)
    values(all_merge_domain_poly) <- t(all_merge_domain)
    
    if(Use_ACES){
      #convert domain scale version to the proper crs
      all_merge_LCC_domain <- project(all_merge_domain_poly,aces_res)
      cover_all <- list(extract(aces_res,all_merge_LCC_domain,weights=T,exact=T,cells=T))
      
      disaggregation(aces_res,res_totals,agg_level="domain",NEI_input=all_merge_LCC_domain,cover_all,out_envir=environment())
      disaggregation(aces_com,com_totals,agg_level="domain",NEI_input=all_merge_LCC_domain,cover_all,out_envir=environment())
    }
    if(Use_Vulcan){
      all_merge_LCC_domain <- project(all_merge_domain_poly,vu_res)
      cover_all <- list(extract(vu_res,all_merge_LCC_domain,weights=T,exact=T,cells=T))
      
      disaggregation(vu_res,res_totals,agg_level="domain",NEI_input=all_merge_LCC_domain,cover_all,out_envir=environment())
      disaggregation(vu_com,com_totals,agg_level="domain",NEI_input=all_merge_LCC_domain,cover_all,out_envir=environment())
    }
    cat("\rFinished disaggregating emissions to pixels from the domain scale at",difftime(Sys.time(),starttime,units = "min"),"minutes since start\n")
  }
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
      writeRaster(input,
                  paste0(output_directory,'/',inventory_name,'_',disaggregation_level,'_NG_dist_',total,'.nc'),
                  force_v4=TRUE,
                  varname='methane_emissions',
                  varunit='mol/km2/s',
                  longname=paste0(inventory_name,'_',disaggregation_level,'_NG_dist_',total),
                  NAflag=-9999,
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
      #First put domain in ACES/Vulcan res, then crop/mask input to it, add a
      #few pixels worth of buffer (at the domain resolution) filled with 0's so
      #the average doesn't consider these NA values to ignore in calculations
      #(drastically impacts avg).  Then finally reproject via average.
      domain_reproj <- project(domain,crs(input))
      input=crop(aces_res_ch4_byLDC$post_meter_ER_total_res,domain_reproj,snap="out")
      input=mask(input,as.polygons(domain_reproj),touches=F,updatevalue=0)
      input=extend(input,fill=0,
                  ext(input)+(res(domain_reproj)*5))
      input=project(input,domain,method="average")
      
      #convert from mol/km2s to nmol/m2s
      input <- input*1000
      
      #grab some text for the longname
      if(grepl("_res",total)){
        sector_name <- "residential"
      }else if(grepl("_com",total)){
        sector_name <- "commercial"
      }
      
      if(grepl("mains",total)){
        subsector_name <- "mains pipelines"
      }else if(grepl("serv",total)){
        subsector_name <- "service pipelines"
      }else if(grepl("MnR",total)){
        subsector_name <- "metering and regulating stations"
      }else if(grepl("^meter",total)){
        subsector_name <- "consumer meters"
      }else if(grepl("upset",total)){
        subsector_name <- "upsets and maintenance"
      }else if(grepl("post_meter",total)){
        subsector_name <- "post-meter residential leakage and usage"
      }
      
      if(grepl("LDC",disaggregation_level)){
        disaggregation_name <- "local distribution company"
      }else if(grepl("state",disaggregation_level)){
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
               paste0(output_directory,'/',"NG_dist_",sub("_ER_total","",total),
                      "_",disaggregation_level,"_",inventory_name,'.nc'),
               force_v4=TRUE,
               varname='methane_emissions',
               unit='nmol/m2/s',
               longname=paste0('Methane emissions from natural gas distribution ',subsector_name,
                               ', spatially allocated from ',disaggregation_name,
                               ' totals using ',inventory_name,' ',sector_name,' CO2 emissions'),
               missval=-9999,
               overwrite=TRUE)
    }
  }
  ################################################################################
  #Save the output
  
  # Now save the rasters for each subsector
  for(total in res_totals){
    if(NG_distribution_by_LDC){
      if(Use_ACES){
        save_data(aces_res_ch4_byLDC[[total]])
      }
      if(Use_Vulcan){
        save_data(vu_res_ch4_byLDC[[total]])
      }
    }
    if(NG_distribution_by_state){
      if(Use_ACES){
        save_data(aces_res_ch4_bystate[[total]])
      }
      if(Use_Vulcan){
        save_data(vu_res_ch4_bystate[[total]])
      }
    }
    if(NG_distribution_by_domain){
      if(Use_ACES){
        save_data(aces_res_ch4_bydomain[[total]])
      }
      if(Use_Vulcan){
        save_data(vu_res_ch4_bydomain[[total]])
      }
    }
  }
  
  for(total in com_totals){
    if(NG_distribution_by_LDC){
      if(Use_ACES){
        save_data(aces_com_ch4_byLDC[[total]])
      }
      if(Use_Vulcan){
        save_data(vu_com_ch4_byLDC[[total]])
      }
    }
    if(NG_distribution_by_state){
      if(Use_ACES){
        save_data(aces_com_ch4_bystate[[total]])
      }
      if(Use_Vulcan){
        save_data(vu_com_ch4_bystate[[total]])
      }
    }
    if(NG_distribution_by_domain){
      if(Use_ACES){
        save_data(aces_com_ch4_bydomain[[total]])
      }
      if(Use_Vulcan){
        save_data(vu_com_ch4_bydomain[[total]])
      }
    }
  }
  ################################################################################
  # Some sanity checks
  
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
  
  ch4_totals_df <- rbind(res_data,com_data)
  
  #original data that was distributed in the rasters.  The totals should still
  #match.
  if(NG_distribution_by_LDC){
    input_totals_LDC <- as.data.frame(all_merge_clean[,grep(glob2rx("*_ER*"),names(all_merge_clean))])
    input_totals_LDC <- colSums(input_totals_LDC)
    ch4_totals_df <- data.frame(ch4_totals_df,
                                "byLDC_input"=input_totals_LDC[rownames(ch4_totals_df)])
  }
  if(NG_distribution_by_state){
    input_totals_state <- all_merge_state[,grep(glob2rx("*_ER*"),colnames(all_merge_state))]
    input_totals_state <- colSums(input_totals_state)
    ch4_totals_df <- data.frame(ch4_totals_df,
                                "bystate_input"=input_totals_state[rownames(ch4_totals_df)])
  }
  if(NG_distribution_by_domain){
    input_totals_domain <- all_merge_domain[grep(glob2rx("*_ER*"),names(all_merge_domain))]
    ch4_totals_df <- data.frame(ch4_totals_df,
                                "bydomain_input"=input_totals_domain[rownames(ch4_totals_df)])
  }
  
  
  ch4_totals_df <- apply(ch4_totals_df,2,FUN=function(x){as.numeric(x)})
  ch4_totals_df[is.na(ch4_totals_df)] <- 0
  
  percent_change <- abs(ch4_totals_df - ch4_totals_df[,1])/ch4_totals_df[,1]
  if(!all(percent_change<0.0001,na.rm=T)){
    #Check if all values are within 0.01 percent of the first column (i.e.,
    #all are ~identical other than minor rounding)
    View(ch4_totals_df)
    stop("Something has gone wrong - the total across the domain when disaggregated by LDC vs by state vs by domain or by using ACES vs Vulcan disagree by more than rounding error (0.01%)")
  }
  
  ################################################################################
  #Save visuals
  
  if(verbose){
    #To simplify the naming/processing needed, lets just write a wrapper
    #function. input_data=list of all data for that sector, total=coded
    #shorthand for sub-sector
    
    wrapper_plot_plus <- function(input_data,total){
      combined_data <- rast(sapply(input_data,
                                   FUN=function(x){project(get(x,
                                                               envir=environment())[[total]]*1000,domain)}))
      combined_range=global(combined_data,range)
      zmin <- min(combined_range[,1])
      zmax <- max(combined_range[,2])
      
      #grab some text for the plot title
      disaggregation_level <- sapply(strsplit(unlist(input_data),"by"),"[[",2)
      inventory_name <- sapply(strsplit(unlist(input_data),"_"),"[[",1)
      sector_short <- sapply(strsplit(unlist(input_data),"_"),"[[",2)[1]
      sector_long <- gsub("res","residential",
                          gsub("com","commercial",sector_short))
      
      if(grepl("mains",total)){
        subsector_name <- "mains pipelines"
        subsector_short <- "mains_pipelines"
      }else if(grepl("serv",total)){
        subsector_name <- "service pipelines"
        subsector_short <- "service_pipelines"
      }else if(grepl("MnR",total)){
        subsector_name <- "metering and regulating stations"
        subsector_short <- "MnR_stations"
      }else if(grepl("^meter",total)){
        subsector_name <- "consumer meters"
        subsector_short <- "meters"
      }else if(grepl("upset",total)){
        subsector_name <- "upsets and maintenance"
        subsector_short <- "upsets"
      }else if(grepl("post_meter",total)){
        subsector_name <- "post-meter residential leakage and usage"
        subsector_short <- "post_meter"
      }
      
      inventory_name["aces"==inventory_name] <- "ACES"
      inventory_name["vu"==inventory_name] <- "Vulcan"
      
      disaggregation_name <- gsub("LDC","local distribution company",disaggregation_level)
      
      # NG_dist_MnR_ACES_res.png
      # stat_comb_com_petr_bystate_aces
      for(A in 1:nlyr(combined_data)){
        not_log_plot(combined_data[[A]],
                     filename=paste0("NG_dist_",sector_short,"_",subsector_short,"_by",
                                     disaggregation_level,"_",tolower(inventory_name))[A],
                     paste0('Methane emissions from natural gas distribution\n',subsector_name,
                            ', spatially allocated from\n',disaggregation_name,
                            ' totals using\n',inventory_name,' ',sector_long,' CO2 emissions')[A],
                     zlim_min = zmin,zlim_max = zmax)
      }
    }
    
    
    
    for(total in res_totals){
      wrapper_plot_plus(res_data_objects,total)
    }
    
    for(total in com_totals){
      wrapper_plot_plus(com_data_objects,total)
    }
    
    
    
    
    
    
    
    #Now repeat for sector-summed plots
    
    #assume at least some pixels are 0, allow max to be defined exclusively by
    #the data
    zmin <- 0
    zmax <- 0
    
    if(Use_ACES){
      if(NG_distribution_by_LDC){
        #use regex to load in all for ACES, byLDC, all sectors
        Summed_NG_dist_ACES_byLDC <- rast(list.files(output_directory,
                                                     pattern="NG_dist_.+_byLDC_aces",
                                                     full.names = T))
        Summed_NG_dist_ACES_byLDC <- sum(Summed_NG_dist_ACES_byLDC)
        zmax <- max(zmax,as.numeric(global(Summed_NG_dist_ACES_byLDC,max)))
      }
      if(NG_distribution_by_state){
        Summed_NG_dist_ACES_bystate <- rast(list.files(output_directory,
                                                       pattern="NG_dist_.+_bystate_aces",
                                                       full.names = T))
        Summed_NG_dist_ACES_bystate <- sum(Summed_NG_dist_ACES_bystate)
        zmax <- max(zmax,as.numeric(global(Summed_NG_dist_ACES_bystate,max)))
      }
      if(NG_distribution_by_domain){
        Summed_NG_dist_ACES_bydomain <- rast(list.files(output_directory,
                                                        pattern="NG_dist_.+_bydomain_aces",
                                                        full.names = T))
        Summed_NG_dist_ACES_bydomain <- sum(Summed_NG_dist_ACES_bydomain)
        zmax <- max(zmax,as.numeric(global(Summed_NG_dist_ACES_bydomain,max)))
      }
    }
    if(Use_Vulcan){
      if(NG_distribution_by_LDC){
        Summed_NG_dist_vulcan_byLDC <- rast(list.files(output_directory,
                                                       pattern="NG_dist_.+_byLDC_vulcan",
                                                       full.names = T))
        Summed_NG_dist_vulcan_byLDC <- sum(Summed_NG_dist_vulcan_byLDC)
        zmax <- max(zmax,as.numeric(global(Summed_NG_dist_vulcan_byLDC,max)))
      }
      if(NG_distribution_by_state){
        Summed_NG_dist_vulcan_bystate <- rast(list.files(output_directory,
                                                         pattern="NG_dist_.+_bystate_vulcan",
                                                         full.names = T))
        Summed_NG_dist_vulcan_bystate <- sum(Summed_NG_dist_vulcan_bystate)
        zmax <- max(zmax,as.numeric(global(Summed_NG_dist_vulcan_bystate,max)))
      }
      if(NG_distribution_by_domain){
        Summed_NG_dist_vulcan_bydomain <- rast(list.files(output_directory,
                                                          pattern="NG_dist_.+_bydomain_vulcan",
                                                          full.names = T))
        Summed_NG_dist_vulcan_bydomain <- sum(Summed_NG_dist_vulcan_bydomain)
        zmax <- max(zmax,as.numeric(global(Summed_NG_dist_vulcan_bydomain,max)))
      }
    }
    
    
    #now actually plot
    if(Use_ACES){
      if(NG_distribution_by_LDC){
        not_log_plot(Summed_NG_dist_ACES_byLDC,
                     "Natural Gas Distribution emissions\nPHMSA, EIA, and GHGRP activity data combined with GHGI and published\nemission factors at the company level and distributed using aces residential\nand commercial sectoral CO2 emissions",
                     zlim_min=zmin,zlim_max=zmax)
      }
      if(NG_distribution_by_state){
        not_log_plot(Summed_NG_dist_ACES_bystate,
                     "Natural Gas Distribution emissions\nPHMSA, EIA, and GHGRP activity data combined with GHGI and published\nemission factors state-summed and distributed using aces residential\nand commercial sectoral CO2 emissions",
                     zlim_min=zmin,zlim_max=zmax)
      }
      if(NG_distribution_by_domain){
        not_log_plot(Summed_NG_dist_ACES_bydomain,
                     "Natural Gas Distribution emissions\nPHMSA, EIA, and GHGRP activity data combined with GHGI and published\nemission factors domain-summed and distributed using aces residential\nand commercial sectoral CO2 emissions",
                     zlim_min=zmin,zlim_max=zmax)
      }
    }
    if(Use_Vulcan){
      if(NG_distribution_by_LDC){
        not_log_plot(Summed_NG_dist_vulcan_byLDC,
                     "Natural Gas Distribution emissions\nPHMSA, EIA, and GHGRP activity data combined with GHGI and published\nemission factors at the company level and distributed using Vulcan residential\nand commercial sectoral CO2 emissions",
                     zlim_min=zmin,zlim_max=zmax)
      }
      if(NG_distribution_by_state){
        not_log_plot(Summed_NG_dist_vulcan_bystate,
                     "Natural Gas Distribution emissions\nPHMSA, EIA, and GHGRP activity data combined with GHGI and published\nemission factors state-summed and distributed using Vulcan residential\nand commercial sectoral CO2 emissions",
                     zlim_min=zmin,zlim_max=zmax)
      }
      if(NG_distribution_by_domain){
        not_log_plot(Summed_NG_dist_vulcan_bydomain,
                     "Natural Gas Distribution emissions\nPHMSA, EIA, and GHGRP activity data combined with GHGI and published\nemission factors domain-summed and distributed using Vulcan residential\nand commercial sectoral CO2 emissions",
                     zlim_min=zmin,zlim_max=zmax)
      }
    }
    
    
  }
  
  cat("Finished natural gas distribution sector: NG_distribution_emissions in",difftime(Sys.time(),starttime,units = "min"),"minutes\n")
}
