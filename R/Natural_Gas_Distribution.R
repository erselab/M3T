#'@title Create gridded natural gas distribution methane emissions maps
#'
#'@description `Natural_Gas_Distribution` writes up to 63 netcdf files of gridded methane
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
#'  the input "natural_gas_pipeline_emission_factors", which by default are the
#'  emission factors from Table 2 of Weller et al. who used research equipped
#'  Google Street View cars to measure > 4000 leaks in the U.S.  PHMSA data on
#'  pipeline services, separated by pipeline material, are combined with the
#'  GHGI emission factors to get emissions for each LDC.
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
#'  reported counts are used instead.  These emissions are then distributed to
#'  the appropriate LDC territories using HIFLD shapefiles, or aggregated at the
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
#'  GHGRP data is available for this sector starting in 2011 and generally is
#'  about 2 years behind present day, the GHGI is available starting in 1990 and
#'  is updated approximately in sync with the GHGRP.  The HIFLD dataset is
#'  updated infrequently and is available for 2019 and 2017.  The EIA form is
#'  annually reported and is available starting in 1997 and is generally updated
#'  in September to the previous year (i.e., Sept 2024 adds 2023 data).  The
#'  PHMSA data is annual and is available starting in 1970 and is available up
#'  to the most recent year.  The GHGRP includes only facilities that emit at
#'  least 25,000 metric tons of carbon dioxide equivalent while the GHGI is
#'  intended to capture all national emissions. All other datasets are meant to
#'  be inclusive of national facilities. All data is annual.  The GHGI is
#'  national totals while all other datasets are at the facility scale.
#'
#'  GHGRP data, GHGI data, and the HIFLD shapefile will be automatically
#'  downloaded.
#'
#'  The GHGRP is available at \url{https://ghgdata.epa.gov/ghgp/main.do}. For
#'  individual LDC metering and regulating station counts, among other
#'  variables, one must filter to the natural gas local distribution companies
#'  sector, select an individual facility and select "View reported data".  The
#'  GHGI is available at
#'  \url{https://www.epa.gov/ghgemissions/inventory-us-greenhouse-gas-emissions-and-sinks}.
#'  The necessary GHGI Annex data is available at
#'  \url{https://www.epa.gov/ghgemissions/natural-gas-and-petroleum-systems-ghg-inventory-additional-information-1990-2022-ghg}
#'  for the 2024 GHGI.  In the GHGI Annexes, available at
#'  \url{https://www.epa.gov/ghgemissions/inventory-us-greenhouse-gas-emissions-and-sinks-1990-2022},
#'  for 2024 there is a link to the file in Section 3.6: "Methodology for
#'  Estimating CH4, CO2, and N2O Emissions from Natural Gas Systems".  The excel
#'  file has multiple sheets, each of which has a separate layout.  The HIFLD
#'  dataset is available at
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
#'@inheritParams Municipal_solid_waste 
#'
#'@param Use_ACES Logical.  Pulled from \code{\link{M3T_config}}.
#'@param Use_Vulcan Logical.  Pulled from \code{\link{M3T_config}}.
#'@param aces_res SpatRaster of residential CO2 emissions from the ACES
#'  inventory as loaded in \code{\link{CH4_inventory_build}} based on
#'  \code{Use_ACES} and \code{Source_ACES}.
#'@param aces_com SpatRaster of commercial CO2 emissions from the ACES
#'  inventory as loaded in \code{\link{CH4_inventory_build}} based on
#'  \code{Use_ACES} and \code{Source_ACES}.
#'@param vu_res SpatRaster of residential CO2 emissions from the Vulcan v4.0
#'  inventory as loaded in \code{\link{CH4_inventory_build}} based on
#'  \code{Use_Vulcan} and \code{Source_Vulcan}.
#'@param vu_com SpatRaster of commercial CO2 emissions from the Vulcan v4.0
#'  inventory as loaded in \code{\link{CH4_inventory_build}} based on
#'  \code{Use_Vulcan} and \code{Source_Vulcan}.
#'@param natural_gas_pipeline_emission_factors Data.frame.  Pulled from \code{\link{M3T_config}}.
#'@param natural_gas_res_post_meter_emission_factor Numeric.  Pulled from \code{\link{M3T_config}}.
#'@param natural_gas_com_post_meter_emission_factor Numeric.  Pulled from \code{\link{M3T_config}}.
#'@param NG_distribution_by_domain Logical.  Pulled from \code{\link{M3T_config}}.
#'@param NG_distribution_by_state Logical.  Pulled from \code{\link{M3T_config}}.
#'@param NG_distribution_by_LDC Logical.  Pulled from \code{\link{M3T_config}}.
#'@param GHGI_services Data.frame.  Pulled from \code{\link{M3T_config}}.
#'@param GHGI_meters Data.frame.  Pulled from \code{\link{M3T_config}}.
#'@param GHGI_maintenance Data.frame.  Pulled from \code{\link{M3T_config}}.
#'@param GHGI_MnR Data.frame.  Pulled from \code{\link{M3T_config}}.
#'@param Source_GHGRP_LDC Character.  Pulled from \code{\link{M3T_config}}.
#'@param Source_EIA_NG_file Character.  Pulled from \code{\link{M3T_config}}.
#'@param Source_PHMSA_file Character.  Pulled from \code{\link{M3T_config}}.
#'@param GHGRP_subpartW_emissions Data.frame with the GHGRP petroleum and
#'  natural gas systems emissions data for all years and states as prepared in
#'  \code{\link{CH4_inventory_build}} using
#'  \code{Source_GHGRP_subpartW_emissions} provided in \code{\link{M3T_config}}.
#'@param verbose Logical indicating whether to save additional output.  This
#'  includes plots of the gridded methane emissions for each
#'  fuel-sector-inventory-variation combination as well as 2 summed plots for
#'  each inventory-variation combination - one for wood and one for all other
#'  sectors.
#'@returns Nothing is returned from the function, but the main outputs are up to
#'  63 netcdf files of the methane emissions from natural gas distribution. They
#'  are titled as "NG_dist_type_sector_variation_inventory.nc" where type is
#'  upset, serv (services), post_meter, MnR (metering and regulating stations),
#'  and mains; sector is abbreviated as res (residential) or com (commercial);
#'  variation is byLDC, bystate, or bydomain; and inventory is ACES or Vulcan.
#'@references \href{https://doi.org/10.1021/acs.est.0c00437}{Weller et al.}
#'@references \href{https://doi.org/10.1021/acs.est.8b03217}{Fischer et al.}
#'@references \href{https://doi.org/10.1029/2020JD032974}{Vulcan}
#'@references \href{https://doi.org/10.1002/2017JD027359}{ACES}
#'@seealso [CH4_inventory_build()] Calculates methane inventory using settings
#'provided in config.
#'
#'[M3T_config] Generates the config function with user-editable settings used
#'throughout processing.
#'
#'[Inventory_based_disaggregation()] Disaggregates data to pixels using a sectoral CO2 inventory.
#'@keywords internal

Natural_Gas_Distribution <- function(domain,
                            domain_template,
                            state_name_list,
                            input_directory,
                            output_directory,
                            inventory_year,
                            GHGI_data_yr,
                            verbose,
                            GHGRP_facility_data,
                            GHGRP_subpartW_emissions,
                            Source_EIA_NG_file,
                            Source_PHMSA_file,
                            Source_GHGRP_LDC,
                            GHGI_MnR,
                            GHGI_maintenance,
                            GHGI_meters,
                            GHGI_services,
                            State_Tigerlines,
                            NG_distribution_by_LDC,
                            NG_distribution_by_state,
                            NG_distribution_by_domain,
                            natural_gas_pipeline_emission_factors,
                            natural_gas_res_post_meter_emission_factor,
                            natural_gas_com_post_meter_emission_factor,
                            Use_ACES,
                            Use_Vulcan,
                            aces_res,
                            aces_com,
                            vu_res,
                            vu_com,
                            plot_directory,
                            County_Tigerlines,
                            State_CB){
  
  
  starttime <- Sys.time()
  cat("Starting natural gas distribution sector: Natural_Gas_Distribution\n")
  
  NG_dist_output_directory <- file.path(output_directory,"NG_distribution")
  dir.create(NG_dist_output_directory,showWarnings = F)
  
  if(verbose){
    #because of how many subsectors this sector has, save it in it's own folder
    NG_dist_plot_directory <- file.path(plot_directory,"NG_distribution")
    dir.create(NG_dist_plot_directory,showWarnings = F)
  }
  
  if(!NG_distribution_by_LDC){
    ################################################################################
    #load in and filter the EIA file
    
    if(Source_EIA_NG_file=="M3T"){
      #UPDATE TO ZENODO
      EIA_csv <- M3T::EIA_NG_data
    }else{
      EIA_file <- file.path(input_directory,"EIA","User_supplied_EIA_form_176.csv")
      invisible(file.copy(Source_EIA_NG_file,EIA_file,overwrite = T))
      
      # Load the EIA company-level data for inventory year
      EIA_csv <- utils::read.csv(EIA_file)
      
      #older versions had 2 rows of headers, need to skip the first one to run
      #properly in that case.  Made generalized so it should appropriately acount
      #for different formatting - assuming year is still the first column.
      if(colnames(EIA_csv)[1]!="Year"){
        EIA_csv <- utils::read.csv(EIA_file,skip=which(EIA_csv[,1]=="Year"))
      }
    }
    #subset to the inventory year
    EIA_csv <- EIA_csv[EIA_csv$Year==GHGI_data_yr,]
    
    #Correct column names that matter
    colnames(EIA_csv) <- gsub("\\.|\\.BR\\.","_",
                              gsub("\\.\\.Mcf\\.","\\.(Mcf)",colnames(EIA_csv)))
    
    ################################################################################
    #download, load in and filter the PHMSA file
    
    if(Source_PHMSA_file=="M3T"){
      #UPDATE TO ZENODO
      PHMSA_csv_NG <- M3T::PHMSA_natural_gas_distribution
      PHMSA_csv_NG <- PHMSA_csv_NG[PHMSA_csv_NG$REPORT_YEAR==GHGI_data_yr,]
    }else{
      PHMSA_file <- file.path(input_directory,"User_supplied_PHMSA_annual_gas_distribution.xlsx")
      invisible(file.copy(Source_PHMSA_file,PHMSA_file,overwrite = T))
      
      # Load the PHMSA data for inventory year, similarly to EIA file
      PHMSA_csv <- suppressMessages(readxl::read_xlsx(PHMSA_file,col_names = T))
      if(colnames(PHMSA_csv)[1]!="DATAFILE_AS_OF"){
        PHMSA_csv <- readxl::read_xlsx(PHMSA_file,col_names = T,skip=which(PHMSA_csv[,1]=="DATAFILE_AS_OF"))
      }
      
      # Filter the PHMSA file by commodity - only relevant for newer years to
      # filter out landfill gas, propane, or syngas
      if("COMMODITY" %in% colnames(PHMSA_csv)){
        PHMSA_csv_NG <- PHMSA_csv[which(PHMSA_csv$COMMODITY == 'Natural Gas'),]
      }else{
        PHMSA_csv_NG <- PHMSA_csv
      }
      
      #combine several columns together for later
      PHMSA_csv_NG$MMILES_bare_steel <- rowSums(PHMSA_csv_NG[,c("MMILES_STEEL_UNP_BARE","MMILES_STEEL_CP_BARE","MMILES_CU")],na.rm=T)
      PHMSA_csv_NG$MMILES_iron <- rowSums(PHMSA_csv_NG[,c("MMILES_CI","MMILES_DI","MMILES_RCI")],na.rm=T)
      PHMSA_csv_NG$MMILES_coat_steel <- rowSums(PHMSA_csv_NG[,c("MMILES_STEEL_UNP_COATED","MMILES_STEEL_CP_COATED","MMILES_OTHER")],na.rm=T)
      PHMSA_csv_NG$MMILES_plastic <- PHMSA_csv_NG[,"MMILES_PLASTIC"]
      
      PHMSA_csv_NG$NUM_SRVS_unp_steel <- rowSums(PHMSA_csv_NG[,c("NUM_SRVS_STEEL_UNP_COATED","NUM_SRVS_STEEL_UNP_BARE")],na.rm=T)
      PHMSA_csv_NG$NUM_SRVS_cp_steel <- rowSums(PHMSA_csv_NG[,c("NUM_SRVS_STEEL_CP_BARE","NUM_SRVS_STEEL_CP_COATED","NUM_SRVS_OTHER")],na.rm=T)
      PHMSA_csv_NG$NUM_SRVS_plastic <- PHMSA_csv_NG[,c("NUM_SRVS_PLASTIC")]
      PHMSA_csv_NG$NUM_SRVS_copper_iron <- rowSums(PHMSA_csv_NG[,c("NUM_SRVS_CU","NUM_SRVS_CI","NUM_SRVS_DI","NUM_SRVS_RCI")],na.rm=T)
    }
    
    #filter to only those for the relevant states
    PHMSA_csv_NG <- PHMSA_csv_NG[which(PHMSA_csv_NG$STOP %in% state_name_list),]
    
    ################################################################################
    #Use yr determined in CH4 inventory build - closest to inventory year with
    #both GHGI and GHGRP
    
    GHGRP_year <- GHGI_data_yr
    
    ################################################################################
    #Download the relevant ghgrp emissions data using the API
    #(https://www.epa.gov/enviro/envirofacts-data-service-api) and combine the
    #facility and emission data appropriately
    
    #because we're getting sub-facility level information for transmission
    #compressor, first need to aggregate.  Subsetting to only the year of interest
    #now instead of later.
    GHGRP_subpartW_emissions <- GHGRP_subpartW_emissions[GHGRP_subpartW_emissions$reporting_year==GHGRP_year,]
    GHGRP_subpartW_emissions <- GHGRP_subpartW_emissions[GHGRP_subpartW_emissions$industry_segment=="Natural gas distribution [98.230(a)(8)]",]
    GHGRP_subpartW_emissions <- stats::aggregate(GHGRP_subpartW_emissions$total_reported_ch4_emissions,
                                                 by=list(GHGRP_subpartW_emissions$facility_id,
                                                         GHGRP_subpartW_emissions$facility_name),
                                                 sum,na.rm=T)
    
    #Now name the aggregated columns for clarity
    colnames(GHGRP_subpartW_emissions) <- c("facility_id","facility_name","Reported_CH4")
    #and remove those that have 0 emissions for this category
    GHGRP_subpartW_emissions <- GHGRP_subpartW_emissions[GHGRP_subpartW_emissions$Reported_CH4>0,]
    ################################################################################
    #get additional variables from GHGRP that are more detailed in other tables
    
    ghgrp_LDC_file <- file.path(input_directory,"GHGRP","ldc_details.csv")
    ghgrp_ngdist_file <- file.path(input_directory,"GHGRP","ngdist_leaks.csv")
    ghgrp_pop_count_file <- file.path(input_directory,"GHGRP","pop_count.csv")
    
    if(Source_GHGRP_LDC=="M3T"){
      GHGRP_LDC_info <- M3T::GHGRP_LDC
      GHGRP_LDC_info <- GHGRP_LDC_info[GHGRP_LDC_info$reporting_year==GHGRP_year,]
      GHGRP_LDC_info$reporting_year <- NULL
    }else{
      if(Source_GHGRP_LDC=="download"){
        #Reporting changed for subpart W after 2014, content is the same, format is
        #different
        if(GHGRP_year<2015 & !file.exists(ghgrp_LDC_file)){
          data_URL <- "https://data.epa.gov/dmapservice/ghg.w_local_dist_companies_details/csv"
          Trycatch_downloader(URL = data_URL,method = "save",output_location = ghgrp_LDC_file,
                              error_message = paste0("Greenhouse Gas Reporting Program data could not be downloaded using API link: ",data_URL))
        }else if(!file.exists(ghgrp_ngdist_file) | !file.exists(ghgrp_pop_count_file)){
          data_URL <- "https://data.epa.gov/dmapservice/ghg.ef_w_equip_leaks_ngdist_leaks/csv"
          Trycatch_downloader(URL = data_URL,method = "save",output_location = ghgrp_ngdist_file,
                              error_message = paste0("Greenhouse Gas Reporting Program data could not be downloaded using API link: ",data_URL))
          
          data_URL <- "https://data.epa.gov/dmapservice/ghg.ef_w_equip_leaks_pop_count/csv"
          Trycatch_downloader(URL = data_URL,method = "save",output_location = ghgrp_pop_count_file,
                              error_message = paste0("Greenhouse Gas Reporting Program data could not be downloaded using API link: ",data_URL))
        }
      }else{
        if(GHGRP_year<2015){
          invisible(file.copy(Source_GHGRP_LDC,ghgrp_LDC_file,overwrite = T))
        }else{
          invisible(file.copy(Source_GHGRP_LDC[1],ghgrp_ngdist_file,overwrite = T))
          invisible(file.copy(Source_GHGRP_LDC[2],ghgrp_pop_count_file,overwrite = T))
        }
      }
      
      #grab the needed data and merge with subpartW data
      if(GHGRP_year<2015){
        GHGRP_LDC_info <- utils::read.csv(ghgrp_LDC_file)
        GHGRP_LDC_info <- GHGRP_LDC_info[!is.na(GHGRP_LDC_info$reporting_year),]
        #Calculate the total miles of pipeline across materials
        GHGRP_LDC_info$total_miles <- rowSums(GHGRP_LDC_info[,c("miles_of_cast_iron_dist_mains","miles_of_plstic_dist_mains","miles_of_prot_steel_dist_mains","miles_of_unpr_steel_dist_mains")],na.rm=T)
        
        GHGRP_LDC_info <- GHGRP_LDC_info[GHGRP_LDC_info$reporting_year==GHGRP_year,
                                         c("facility_id","total_miles","above_grade_transfer_stations","above_grade_metering_stations","below_grade_transfer_stations","below_grade_metering_stations")]
        #rename for consistency
        colnames(GHGRP_LDC_info) <- c("facility_id","Miles_of_Mains","N_of_above_grade_T_D_transfer_stations","N_of_above_grade_non_T_D_MR_stations","N_of_below_grade_T_D_transfer_stations","N_of_below_grade_non_T_D_MR_stations")
      }else{
        ghgrp_ngdist <- utils::read.csv(ghgrp_ngdist_file)
        ghgrp_ngdist <- ghgrp_ngdist[ghgrp_ngdist$reporting_year==GHGRP_year,]
        ghgrp_ngdist <- ghgrp_ngdist[order(ghgrp_ngdist$facility_id),c("facility_id","total_td_facility_stations","total_non_td_facility_stations")]
        #rename for consistency
        colnames(ghgrp_ngdist) <- c("facility_id","N_of_above_grade_T_D_transfer_stations","N_of_above_grade_non_T_D_MR_stations")
        
        
        
        ghgrp_pop_count <- utils::read.csv(ghgrp_pop_count_file)
        ghgrp_pop_count <- ghgrp_pop_count[ghgrp_pop_count$reporting_year==GHGRP_year,]
        ghgrp_pop_count <- ghgrp_pop_count[order(ghgrp_pop_count$facility_id),]
        
        facility_id <- ghgrp_pop_count[ghgrp_pop_count$emission_src_type=="Distribution Mains, Gas Service - Cast Iron","facility_id"]
        total_miles <- rowSums(cbind(ghgrp_pop_count[ghgrp_pop_count$emission_src_type=="Distribution Mains, Gas Service - Cast Iron","source_type_count"],
                                     ghgrp_pop_count[ghgrp_pop_count$emission_src_type=="Distribution Mains, Gas Service - Plastic","source_type_count"],
                                     ghgrp_pop_count[ghgrp_pop_count$emission_src_type=="Distribution Mains, Gas Service - Protected Steel","source_type_count"],
                                     ghgrp_pop_count[ghgrp_pop_count$emission_src_type=="Distribution Mains, Gas Service - Unprotected Steel","source_type_count"]))
        N_of_below_grade_T_D_transfer_stations <- rowSums(cbind(ghgrp_pop_count[ghgrp_pop_count$emission_src_type=="Below Grade T-D Station, Gas Service, Inlet Pressure < 100 psig","source_type_count"],
                                                                ghgrp_pop_count[ghgrp_pop_count$emission_src_type=="Below Grade T-D Station, Gas Service, Inlet Pressure 100 to 300 psig","source_type_count"],
                                                                ghgrp_pop_count[ghgrp_pop_count$emission_src_type=="Below Grade T-D Station, Gas Service, Inlet Pressure > 300 psig","source_type_count"]))
        N_of_below_grade_non_T_D_MR_stations <- rowSums(cbind(ghgrp_pop_count[ghgrp_pop_count$emission_src_type=="Below Grade M-R Station, Gas Service, Inlet Pressure < 100 psig","source_type_count"],
                                                              ghgrp_pop_count[ghgrp_pop_count$emission_src_type=="Below Grade M-R Station, Gas Service, Inlet Pressure 100 to 300 psig","source_type_count"],
                                                              ghgrp_pop_count[ghgrp_pop_count$emission_src_type=="Below Grade M-R Station, Gas Service, Inlet Pressure > 300 psig","source_type_count"]))
        ghgrp_pop_count_data <- data.frame(facility_id,total_miles,N_of_below_grade_T_D_transfer_stations,N_of_below_grade_non_T_D_MR_stations)
        
        GHGRP_LDC_info <- merge(ghgrp_ngdist,ghgrp_pop_count_data,
                                by="facility_id")
        GHGRP_LDC_info <- GHGRP_LDC_info[,c("facility_id","total_miles","N_of_above_grade_T_D_transfer_stations","N_of_above_grade_non_T_D_MR_stations","N_of_below_grade_T_D_transfer_stations","N_of_below_grade_non_T_D_MR_stations")]
        colnames(GHGRP_LDC_info) <- c("facility_id","Miles_of_Mains","N_of_above_grade_T_D_transfer_stations","N_of_above_grade_non_T_D_MR_stations","N_of_below_grade_T_D_transfer_stations","N_of_below_grade_non_T_D_MR_stations")
      }
    }
    GHGRP_subpartW_emissions=merge(GHGRP_subpartW_emissions,GHGRP_LDC_info,
                                   by="facility_id")
    ################################################################################
    #Merge with location-like data
    
    #subset to the desired year
    GHGRP_facility_data <- GHGRP_facility_data[GHGRP_facility_data$year==GHGRP_year,]
    
    #combine the datasets by ID
    GHGRP_csv <- merge(GHGRP_facility_data,GHGRP_subpartW_emissions,
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
    
    #most of these have - name or (name) or similar.  Some specific cases don't
    #quite fit.  Split the text to get just the name.
    splittext <- sapply(strsplit(GHGRP_csv$facility_name,
                                 "- |\\(|of |NorthWestern Energy.? |Summit Utilities Inc., |Duke Energy "),utils::tail,1)
    splittext <- sapply(strsplit(splittext,"-"),utils::tail,1)
    splittext <- gsub("\\)","",
                      gsub(" Gas Distribution","",
                           gsub(" Gas Operation","",
                                gsub(" LDC","",splittext))))
    
    #match the name to state abbreviations or names - ignoring state name
    #capitalization
    abb_match <- match(splittext,datasets::state.abb)
    name_match <- match(tolower(splittext),tolower(datasets::state.name))
    
    GHGRP_csv[!is.na(abb_match),"operating_state"]=datasets::state.abb[stats::na.omit(abb_match)]
    GHGRP_csv[!is.na(abb_match),"operating_state_name"]=datasets::state.name[stats::na.omit(abb_match)]
    
    GHGRP_csv[!is.na(name_match),"operating_state"]=datasets::state.abb[stats::na.omit(name_match)]
    GHGRP_csv[!is.na(name_match),"operating_state_name"]=datasets::state.name[stats::na.omit(name_match)]

    ############################################################################
    #M&R stations - can't use GHGRP data without matching facilities, so
    #estimate based on avg stations per mile for reporters in each state. Then
    #split by pressure and function assuming the same split as at the national
    #level (from the GHGI national inventory report).
    
    #aggregate each to a state total
    main_miles_ghgrp <- stats::aggregate(GHGRP_csv$Miles_of_Mains,
                                         list(State=GHGRP_csv$operating_state),
                                         sum,
                                         na.rm=TRUE)
    above_grade_MnR <- stats::aggregate((GHGRP_csv$`N_of_above_grade_T_D_transfer_stations` +
                                           GHGRP_csv$`N_of_above_grade_non_T_D_MR_stations`),
                                        list(State=GHGRP_csv$operating_state),
                                        sum,
                                        na.rm=TRUE)
    below_grade_MnR <- stats::aggregate((GHGRP_csv$`N_of_below_grade_non_T_D_MR_stations` +
                                           GHGRP_csv$`N_of_below_grade_T_D_transfer_stations`),
                                        list(State=GHGRP_csv$operating_state),
                                        sum,
                                        na.rm=TRUE)
    
    # Calculate average stations per mile in each state
    above_grade_MnR$stations_per_mile <- above_grade_MnR$x/main_miles_ghgrp$x
    below_grade_MnR$stations_per_mile <- below_grade_MnR$x/main_miles_ghgrp$x
    
    ############################################################################
    #handle states with no GHGRP data using the average of neighboring states
    
    full_state_list <- c('AL','AR','AZ','CA','CA','CT','DC','DE','FL','GA','IA','ID','IL','IN',
                         'KS','KY','LA','MA','MD','ME','MI','MN','MO','MS','MT','NC','ND','NE',
                         'NH','NJ','NM','NV','NY','OH','OK','OR','PA','RI','SC','SD','TN','TX',
                         'UT','VA','VT','WA','WI','WV','WY')
    #ID states with no data
    state_indx <- match(full_state_list,above_grade_MnR$State)
    missing_states <- full_state_list[is.na(state_indx)]
    Neighboring_states <- M3T::Neighboring_states
    
    B=1
    repeat{
      for(A in 1:length(missing_states)){
        #calculate avg stations/mile across neighboring states
        sub_in <- data.frame("State" = missing_states[A],
                             "x" = NA,
                             "stations_per_mile" = mean(above_grade_MnR[state_indx,"stations_per_mile"][Neighboring_states[missing_states[A],]],na.rm=T))
        above_grade_MnR <- rbind(above_grade_MnR,sub_in)
        
        sub_in$stations_per_mile <- mean(below_grade_MnR[state_indx,"stations_per_mile"][Neighboring_states[missing_states[A],]],na.rm=T)
        below_grade_MnR <- rbind(below_grade_MnR,sub_in)
      }
      
      #if all neighbors = 0, remove that row
      above_grade_MnR <- above_grade_MnR[!is.na(above_grade_MnR$stations_per_mile),]
      below_grade_MnR <- below_grade_MnR[!is.na(below_grade_MnR$stations_per_mile),]
      
      #recalculate matches as indices are now different
      state_indx <- match(full_state_list,above_grade_MnR$State)
      missing_states <- full_state_list[is.na(state_indx)]
      
      #if any were all na, run the loop again as neighbors may have been
      #assigned through this process.  
      if(!any(is.na(state_indx))){
        break
      }
      #just in case, stop just to avoid an infinite loop
      if(B>10){
        stop()
      }
    }
    above_grade_MnR <- above_grade_MnR[state_indx,]
    below_grade_MnR <- below_grade_MnR[state_indx,]

    ################################################################################
    #finalize GHGRP data
    
    #filter to the states in the domain
    GHGRP_csv <- GHGRP_csv[GHGRP_csv$operating_state %in% state_name_list,]
    above_grade_MnR <- above_grade_MnR[above_grade_MnR$State %in% state_name_list,]
    below_grade_MnR <- below_grade_MnR[below_grade_MnR$State %in% state_name_list,]
    
    #update user about changes to any LDCs that are/were listed as states within
    #the domain
    if(any(GHGRP_csv$state!=GHGRP_csv$operating_state)){
      update_ordered <- GHGRP_csv[order(GHGRP_csv$facility_name),]
      cat(paste("\n",update_ordered$facility_name[update_ordered$state!=update_ordered$operating_state],"    rewritten from",update_ordered$state_name[update_ordered$state!=update_ordered$operating_state],"    to",update_ordered$operating_state_name[update_ordered$state!=update_ordered$operating_state]))
      rm(update_ordered)
    }
    ################################################################################
    #have to calculate before aggregating/merging via sum since it applies an
    #average term
    
    # We're going to need the total miles of pipeline (inc. services) -
    # calculate that here from AVERAGE_LENGTH (converting ft to miles)
    PHMSA_csv_NG$Miles_main_and_serv <- PHMSA_csv_NG$MMILES_TOTAL + 
      PHMSA_csv_NG$NUM_SRVCS_TOTAL*PHMSA_csv_NG$AVERAGE_LENGTH/5280
    ################################################################################
    #merge the state level PHMSA and EIA files.  Not including HIFLD or GHGRP data.
    
    # Then select the columns we need and aggregate the entries which share the same company ID or state
    PHMSA_cols_to_keep <- paste0("PHMSA_",c('MMILES_bare_steel',
                                            'MMILES_iron',
                                            'MMILES_coat_steel',
                                            'MMILES_plastic',
                                            'NUM_SRVS_unp_steel',
                                            'NUM_SRVS_cp_steel',
                                            'NUM_SRVS_plastic',
                                            'NUM_SRVS_copper_iron',
                                            'MMILES_TOTAL',
                                            'NUM_SRVCS_TOTAL',
                                            "Miles_main_and_serv"))
    
    #reconditioned iron pipe (RCI) wasn't reported in older years - add 0's
    if(!("MMILES_RCI" %in% colnames(PHMSA_csv_NG))){
      PHMSA_csv_NG$MMILES_RCI <- 0
      PHMSA_csv_NG$NUM_SRVS_RCI <- 0
    }
    
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
    
    PHMSA_csv_NG_agg <- stats::aggregate(PHMSA_csv_NG[PHMSA_cols_to_keep],
                                         list(PHMSA_State=PHMSA_csv_NG$PHMSA_State),
                                         sum,na.rm=T)
    EIA_csv_agg <- stats::aggregate(EIA_csv[EIA_cols_to_keep],
                                    list(EIA_State=EIA_csv$EIA_State),
                                    sum,na.rm=T)
    
    # Now merge csv stuff and state shapefile together
    all_merge <- merge(EIA_csv_agg, PHMSA_csv_NG_agg, by.x='EIA_State', by.y='PHMSA_State')
    all_merge <- terra::merge(State_Tigerlines,all_merge,by.x="STUSPS",by.y="EIA_State")
    
    # Clean up
    all_merge_clean <- all_merge[cols_to_keep]
    
    #just so the state variable is consistent with the byLDC version
    names(all_merge_clean) <- gsub("STUSPS","PHMSA_State",names(all_merge_clean))
    
    # allocate average stations per mile in each state to all facilities if not
    # calculating by LDC
    all_merge_clean$GHGRP_MnR_above <- all_merge_clean$PHMSA_MMILES_TOTAL*above_grade_MnR$stations_per_mile
    all_merge_clean$GHGRP_MnR_below <- all_merge_clean$PHMSA_MMILES_TOTAL*below_grade_MnR$stations_per_mile
    
    cat("\nFinished downloading and merging all input data at",format(Sys.time(),"%H:%M"),"\n")
  }else{
    ############################################################################
    #load in the output of NG_distribution_by_LDC_prep.R.  Note that is a
    #script, not a function, as it requires some manual efforts.  This one is
    #calculated at the LDC scale, not state level.
    all_merge_clean <- terra::vect(file.path(input_directory,"byLDC_merged","byLDC_merged.shp"))
    names(all_merge_clean) <- unlist(utils::read.table(file.path(input_directory,"byLDC_merged","colnames.txt")))
  }
  ##############################################################################
  #convert a lot of the activity data to emissions data
  
  #Mains using EFs from Weller et al., or as specified in config to go from
  #miles pipeline to mol/s.  Combines leaks/mile and avg mol/s/leak to get
  #mol/s/mile to combine with miles pipeline.
  all_merge_clean$bare_steel_mains_ER <- (all_merge_clean$PHMSA_MMILES_bare_steel*
                                            natural_gas_pipeline_emission_factors[1,"Leaks_per_mile"]*
                                            natural_gas_pipeline_emission_factors[1,"Avg_emissions_mol_per_s"])
  all_merge_clean$iron_mains_ER <- (all_merge_clean$PHMSA_MMILES_iron*
                                      natural_gas_pipeline_emission_factors[2,"Leaks_per_mile"]*
                                      natural_gas_pipeline_emission_factors[2,"Avg_emissions_mol_per_s"])
  all_merge_clean$coat_steel_mains_ER <- (all_merge_clean$PHMSA_MMILES_coat_steel*
                                            natural_gas_pipeline_emission_factors[3,"Leaks_per_mile"]*
                                            natural_gas_pipeline_emission_factors[3,"Avg_emissions_mol_per_s"])
  all_merge_clean$plastic_mains_ER <- (all_merge_clean$PHMSA_MMILES_plastic*
                                         natural_gas_pipeline_emission_factors[4,"Leaks_per_mile"]*
                                         natural_gas_pipeline_emission_factors[4,"Avg_emissions_mol_per_s"])
  
  # Services using EFs from the EPA GHGI, also referred to as the national
  # inventory report.  N services (PHMSA) * mol/s/service (GHGI).
  all_merge_clean$UNP_steel_serv_ER <- (all_merge_clean$PHMSA_NUM_SRVS_unp_steel*
                                          GHGI_services$EF[1])
  all_merge_clean$CP_steel_serv_ER <- (all_merge_clean$PHMSA_NUM_SRVS_cp_steel*
                                         GHGI_services$EF[2])
  all_merge_clean$plastic_serv_ER <- (all_merge_clean$PHMSA_NUM_SRVS_plastic*
                                        GHGI_services$EF[3])
  all_merge_clean$copper_serv_ER <- (all_merge_clean$PHMSA_NUM_SRVS_copper_iron*
                                       GHGI_services$EF[4])
  
  #split by function/pressure
  GHGI_MnR_above <- sum(GHGI_MnR$Total_stations[-grep('Vault', GHGI_MnR$Type)])
  GHGI_MnR_below <- sum(GHGI_MnR$Total_stations[grep('Vault', GHGI_MnR$Type)])
  
  # Estimate emissions by function/pressure.  N stations (GHGRP) * mol/s/station
  # (GHGI) * fraction M&R type (GHGI)
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
  
  # Consumer meters.   Customer count (EIA) * mol/s/meter (GHGI).  Split by type
  # of customer (dealt with later).
  all_merge_clean$Res_meter_ER <- all_merge_clean$EIA_Residential_Total_Customers*GHGI_meters$EF[1]
  all_merge_clean$Com_meter_ER <- all_merge_clean$EIA_Commercial_Total_Customers*GHGI_meters$EF[2]
  all_merge_clean$Ind_meter_ER <- all_merge_clean$EIA_Industrial_Total_Customers*GHGI_meters$EF[3]
  
  # Maintenance and upsets.  Miles pipelines (PHMSA) * mol/s/mile (GHGI)
  all_merge_clean$Relief_valve_ER <- all_merge_clean$PHMSA_MMILES_TOTAL*GHGI_maintenance$EF[1]
  all_merge_clean$Blowdown_ER <- all_merge_clean$PHMSA_Miles_main_and_serv*GHGI_maintenance$EF[2]
  all_merge_clean$Mishap_ER <- all_merge_clean$PHMSA_Miles_main_and_serv*GHGI_maintenance$EF[3]
  
  # Post-meter, in this case it's entirely allocated to residential (no data on
  # commercial buildings).  McF = thousand cubic feet.  Thousand cubic feet to
  # residential (EIA) * emission factor (from config, cubic feet/yr to mol/s).
  all_merge_clean$post_meter_ER_total_res <- all_merge_clean$`EIA_Residential_Total_Volume_(Mcf)`*1000*natural_gas_res_post_meter_emission_factor
  all_merge_clean$post_meter_ER_total_com <- all_merge_clean$`EIA_Commercial_Total_Volume_(Mcf)`*1000*natural_gas_com_post_meter_emission_factor
  ##############################################################################
  #break the emissions into residential and commercial fractions
  
  #This is calculated for each company/state according to the ratio of
  #residential:commercial customers. Industrial customer numbers are much
  #smaller, so we ignore these here
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
  
  # We could allocate the industrial meter emissions by ACES and Vulcan
  # industrial sector, but this sector is dominated by a handful of large point
  # sources, many of which don't even use natural gas so instead, share these
  # emissions out between the residential and commercial CO2 maps.  Here split
  # according to the ratio of Res_meter_ER:Com_meter_ER - could equally have
  # split according to the number of customers, but that would shift the ratio
  # of total meter emissions towards residential, which doesn't seem desirable.
  # Keep the same naming convention as for the other subsectors (i.e.
  # _total_res) even though both include a portion of industrial.
  all_merge_clean$meter_ER_total_res <- (all_merge_clean$Res_meter_ER +
                                           all_merge_clean$Ind_meter_ER*
                                           all_merge_clean$Res_meter_ER/
                                           (all_merge_clean$Res_meter_ER + all_merge_clean$Com_meter_ER))
  
  all_merge_clean$meter_ER_total_com <- (all_merge_clean$Com_meter_ER +
                                           all_merge_clean$Ind_meter_ER*
                                           all_merge_clean$Com_meter_ER/
                                           (all_merge_clean$Res_meter_ER + all_merge_clean$Com_meter_ER))
  
  cat("Finished calculating emissions and distributing to residential/commercial portions at",format(Sys.time(),"%H:%M"),"\n")
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
                  'upset_ER_total_com',
                  'post_meter_ER_total_com')
  
  ################################################################################
  #write a function to save
  
  #project with terra
  save_data <- function(input){
    #grab components from the input dataset for naming
    input_name <- gsub("\\[\\[total\\]\\]","",deparse(substitute(input)))
    disaggregation_level <- utils::tail(strsplit(input_name,"_")[[1]],1)
    inventory_name <- strsplit(input_name,"_")[[1]][1]
    
    #if CONUS or custom with a very large domain - reprojecting domain can be
    #problematic
    if(any(terra::ext(domain)/terra::ext(State_Tigerlines) > 1.1)){
      domain_reproj <- terra::as.polygons(terra::ext(domain_template)/terra::ext(State_Tigerlines) * terra::ext(terra::project(State_Tigerlines,input)))
      terra::crs(domain_reproj) <- terra::crs(input)
    }else{
      domain_reproj <- terra::as.polygons(terra::ext(terra::project(domain_template,terra::crs(input))))
      terra::crs(domain_reproj) <- terra::crs(input)
    }
    
    
    #project to a grid with the exact right resolution, extent and origin. First
    #put domain in ACES/Vulcan crs, then crop/mask input to it.  Account for
    #pixels partially within the domain.
    input=terra::crop(input,domain_reproj,snap="out")
    input=terra::mask(input,domain_reproj,touches=T,updatevalue=0)
    if(!any(terra::ext(domain)/terra::ext(State_Tigerlines) > 1.1)){
      cover <- terra::extract(input,domain_reproj,weights=T,cells=T)
      input[cover[,'cell']] <- input[cover[,'cell']]*cover[,'weight']
    }
    
    #add a few pixels worth of buffer (at the domain resolution) filled with 0's
    #so the average doesn't consider these NA values to ignore in calculations
    #(drastically impacts avg).  Then finally reproject via average.
    input=terra::extend(input,fill=0,
                        terra::ext(input)+(terra::res(terra::project(domain_template,terra::crs(input)))*5))
    input=terra::project(input,domain_template,method="average")
    
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
      subsector_name <- "post-meter leakage and usage"
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
    
    
    writeCDF_no_newline(input,
                        paste0(NG_dist_output_directory,'/',"NG_dist_",sub("_ER_total","",total),
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
  ################################################################################
  #these are assigned in the below sections from the Inventory_based_disaggregation function,
  #but R doesn't see them being created explicitly, so do so here just to make
  #usethis::check() happy for package building.
  aces_res_ch4_byLDC <- vu_res_ch4_byLDC <- 
    aces_res_ch4_bystate <- vu_res_ch4_bystate <- 
    aces_res_ch4_bydomain <- vu_res_ch4_bydomain <- 
    aces_com_ch4_byLDC <- vu_com_ch4_byLDC <- 
    aces_com_ch4_bystate <- vu_com_ch4_bystate <- 
    aces_com_ch4_bydomain <- vu_com_ch4_bydomain <- NULL
  ################################################################################
  #use ACES/Vulcan to redistribute residential/commercial emissions at the LDC
  #level
  
  if(NG_distribution_by_LDC){
    if(Use_ACES){
      #mask to only keep those LDCs that are at least partly within the domain
      all_merge_LCC <- terra::mask(all_merge_clean,terra::project(domain,all_merge_clean))
      
      #convert to the proper crs
      all_merge_LCC <- terra::project(all_merge_LCC,aces_res)
      
      #tried other methods to compare speeds
      
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
      
      # Add the count of all LDCs for a user update since this can take some
      # time
      LDC_count <- nrow(all_merge_LCC)
      if(LDC_count==1){
        cover_all <- list(terra::extract(aces_res,all_merge_LCC,weights=T,exact=T,cells=T))
      }else{
        #calculate the fractional cover of each ACES pixel within the LDC.
        all_merge_LCC$count <- 1:nrow(all_merge_LCC)
        cover_all <- all_merge_LCC %>% 
          split(f=all_merge_LCC$HIFLD_SVCTERID) %>%
          lapply(function(x){cat("\rProcessing",x$count,"of",LDC_count,"LDCs using ACES                                   ");
            terra::extract(aces_res,x,weights=T,exact=T,cells=T)})
        #make sure it is in the proper order (split by factor will alphabetize
        #by that factor)
        cover_all <- cover_all[all_merge_LCC$HIFLD_SVCTERID]
      }
      
      Inventory_based_disaggregation(aces_res,res_totals,agg_level="LDC",NEI_input = all_merge_LCC,cover_all,out_envir=environment())
      Inventory_based_disaggregation(aces_com,com_totals,agg_level="LDC",NEI_input = all_merge_LCC,cover_all,out_envir=environment())
    }
    if(Use_Vulcan){
      all_merge_LCC <- terra::mask(all_merge_clean,terra::project(domain,all_merge_clean))
      all_merge_LCC <- terra::project(all_merge_LCC,vu_res)
      
      LDC_count <- nrow(all_merge_LCC)
      if(LDC_count==1){
        cover_all <- list(terra::extract(vu_res,all_merge_LCC,weights=T,exact=T,cells=T))
      }else{
        all_merge_LCC$count <- 1:nrow(all_merge_LCC)
        cover_all <- all_merge_LCC %>% 
          split(f=all_merge_LCC$HIFLD_SVCTERID) %>%
          lapply(function(x){cat("\rProcessing",x$count,"of",LDC_count,"LDCs using vulcan                                  ")
            terra::extract(vu_res,x,weights=T,exact=T,cells=T)})
        cover_all <- cover_all[all_merge_LCC$HIFLD_SVCTERID]
      }
      
      Inventory_based_disaggregation(vu_res,res_totals,agg_level="LDC",NEI_input = all_merge_LCC,cover_all,out_envir=environment())
      Inventory_based_disaggregation(vu_com,com_totals,agg_level="LDC",NEI_input = all_merge_LCC,cover_all,out_envir=environment())
    }
    cat("\rFinished disaggregating emissions to pixels from the LDC scale at",format(Sys.time(),"%H:%M"),"\n")
  }
  
  ################################################################################
  ## Now aggregate emissions at the state level and repeat
  
  # Side note - splitting into residential/commercial emissions at the company
  # level, then aggregating (as we do here) is probably more logical than
  # aggregating total emissions at the state level, then splitting into
  # residential/commercial This is obvious if you think of a situation where one
  # company dominates emissions, but another dominates consumers. In that case
  # the residential/commercial split should closely match that of the high
  # emitting company, not the high consumer company.
  
  if(NG_distribution_by_state){
    if(NG_distribution_by_LDC){
      all_merge_state <- terra::aggregate(as.data.frame(all_merge_clean[,!(names(all_merge_clean) %in% c('HIFLD_SVCTERID', 'EIA_Company', 'EIA_Company_Name', 'PHMSA_State'))]),
                                          list(PHMSA_State=all_merge_clean$PHMSA_State),
                                          sum,na.rm=T)
      # Merge the geometries
      all_merge_state_poly <- terra::merge(State_Tigerlines, all_merge_state, by.y='PHMSA_State', by.x='STUSPS')
      names(all_merge_state_poly) <- gsub("STUSPS","PHMSA_State",names(all_merge_state_poly))
    }else{
      all_merge_state_poly <- all_merge_clean
    }
    
    
    if(Use_ACES){
      #convert state scale version to the proper crs
      all_merge_LCC_state <- terra::project(all_merge_state_poly,aces_res)
      
      if(length(state_name_list)==1){
        cover_all <- list(terra::extract(aces_res,all_merge_LCC_state,weights=T,cells=T))
      }else{
        cover_all <- all_merge_LCC_state %>% 
          split(f=all_merge_LCC_state$STATEFP) %>%
          lapply(function(x){terra::extract(aces_res,x,weights=T,cells=T)})
        #make sure it is in the proper order (split by factor will alphabetize
        #by that factor)
        cover_all <- cover_all[all_merge_LCC_state$STATEFP]
      }
      
      Inventory_based_disaggregation(aces_res,res_totals,agg_level="state",NEI_input = all_merge_LCC_state,cover_all,out_envir=environment())
      Inventory_based_disaggregation(aces_com,com_totals,agg_level="state",NEI_input = all_merge_LCC_state,cover_all,out_envir=environment())
    }
    if(Use_Vulcan){
      all_merge_LCC_state <- terra::project(all_merge_state_poly,vu_res)
      
      if(length(state_name_list)==1){
        cover_all <- list(terra::extract(vu_res,all_merge_LCC_state,weights=T,cells=T))
      }else{
        cover_all <- all_merge_LCC_state %>% 
          split(f=all_merge_LCC_state$STATEFP) %>%
          lapply(function(x){terra::extract(vu_res,x,weights=T,cells=T)})
        cover_all <- cover_all[all_merge_LCC_state$STATEFP]
      }
      
      Inventory_based_disaggregation(vu_res,res_totals,agg_level="state",NEI_input = all_merge_LCC_state,cover_all,out_envir=environment())
      Inventory_based_disaggregation(vu_com,com_totals,agg_level="state",NEI_input = all_merge_LCC_state,cover_all,out_envir=environment())
    }
    gc()
    cat("\rFinished disaggregating emissions to pixels from the state scale at",format(Sys.time(),"%H:%M"),"\n")
  }
  ################################################################################
  #Repeat when aggregated to the domain total.
  
  if(NG_distribution_by_domain){
    all_merge_domain <- suppressWarnings(apply(as.data.frame(all_merge_clean),2,as.numeric))
    all_merge_domain <- colSums(all_merge_domain,na.rm=T)
    
    all_merge_domain_poly <- terra::aggregate(State_Tigerlines)
    terra::values(all_merge_domain_poly) <- t(all_merge_domain)
    
    if(Use_ACES){
      #convert domain scale version to the proper crs
      all_merge_LCC_domain <- terra::project(all_merge_domain_poly,aces_res)
      cover_all <- list(terra::extract(aces_res,all_merge_LCC_domain,weights=T,cells=T))
      
      Inventory_based_disaggregation(aces_res,res_totals,agg_level="domain",NEI_input=all_merge_LCC_domain,cover_all,out_envir=environment())
      Inventory_based_disaggregation(aces_com,com_totals,agg_level="domain",NEI_input=all_merge_LCC_domain,cover_all,out_envir=environment())
    }
    if(Use_Vulcan){
      all_merge_LCC_domain <- terra::project(all_merge_domain_poly,vu_res)
      cover_all <- list(terra::extract(vu_res,all_merge_LCC_domain,weights=T,cells=T))
      
      Inventory_based_disaggregation(vu_res,res_totals,agg_level="domain",NEI_input=all_merge_LCC_domain,cover_all,out_envir=environment())
      Inventory_based_disaggregation(vu_com,com_totals,agg_level="domain",NEI_input=all_merge_LCC_domain,cover_all,out_envir=environment())
    }
    gc()
    cat("\rFinished disaggregating emissions to pixels from the domain scale at",format(Sys.time(),"%H:%M"),"\n")
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
    cat("\rFinished processing",which(total==res_totals),"of",length(res_totals),"residential sectors at",format(Sys.time(),"%H:%M"),"                               ")
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
    cat("\rFinished processing",which(total==com_totals),"of",length(com_totals),"commercial sectors at",format(Sys.time(),"%H:%M"),"                                        ")
  }
  suppressWarnings(rm(all_merge_LCC,LDC_count,cover_all,all_merge_state_poly,
                      all_merge_LCC_state,all_merge_domain_poly,all_merge_LCC_domain))
  ################################################################################
  #Create a sector total, 1 per variant
  
  if(Use_ACES){
    if(NG_distribution_by_LDC){
      #use regex to load in all for ACES, byLDC, all sectors
      Summed_NG_dist_ACES_byLDC <- terra::rast(list.files(NG_dist_output_directory,
                                                          pattern="NG_dist_.+_byLDC_aces",
                                                          full.names = T))
      Summed_NG_dist_ACES_byLDC <- sum(Summed_NG_dist_ACES_byLDC,na.rm=T)
      writeCDF_no_newline(Summed_NG_dist_ACES_byLDC,
                          file.path(output_directory,"NG_distribution_sector_total_ACES_byLDC.nc"),
                          force_v4=TRUE,
                          varname='methane_emissions',
                          unit='nmol/m2/s',
                          longname=paste0('Methane emissions from natural gas distribution spatially allocated from LDC totals using ACES sectoral CO2 emissions'),
                          missval=-9999,
                          overwrite=TRUE)
    }
    if(NG_distribution_by_state){
      Summed_NG_dist_ACES_bystate <- terra::rast(list.files(NG_dist_output_directory,
                                                            pattern="NG_dist_.+_bystate_aces",
                                                            full.names = T))
      Summed_NG_dist_ACES_bystate <- sum(Summed_NG_dist_ACES_bystate,na.rm=T)
      writeCDF_no_newline(Summed_NG_dist_ACES_bystate,
                          file.path(output_directory,"NG_distribution_sector_total_ACES_bystate.nc"),
                          force_v4=TRUE,
                          varname='methane_emissions',
                          unit='nmol/m2/s',
                          longname=paste0('Methane emissions from natural gas distribution spatially allocated from state totals using ACES sectoral CO2 emissions'),
                          missval=-9999,
                          overwrite=TRUE)
    }
    if(NG_distribution_by_domain){
      Summed_NG_dist_ACES_bydomain <- terra::rast(list.files(NG_dist_output_directory,
                                                             pattern="NG_dist_.+_bydomain_aces",
                                                             full.names = T))
      Summed_NG_dist_ACES_bydomain <- sum(Summed_NG_dist_ACES_bydomain,na.rm=T)
      writeCDF_no_newline(Summed_NG_dist_ACES_bydomain,
                          file.path(output_directory,"NG_distribution_sector_total_ACES_bydomain.nc"),
                          force_v4=TRUE,
                          varname='methane_emissions',
                          unit='nmol/m2/s',
                          longname=paste0('Methane emissions from natural gas distribution spatially allocated from domain totals using ACES sectoral CO2 emissions'),
                          missval=-9999,
                          overwrite=TRUE)
    }
  }
  if(Use_Vulcan){
    if(NG_distribution_by_LDC){
      Summed_NG_dist_vulcan_byLDC <- terra::rast(list.files(NG_dist_output_directory,
                                                            pattern="NG_dist_.+_byLDC_vulcan",
                                                            full.names = T))
      Summed_NG_dist_vulcan_byLDC <- sum(Summed_NG_dist_vulcan_byLDC,na.rm=T)
      writeCDF_no_newline(Summed_NG_dist_vulcan_byLDC,
                          file.path(output_directory,"NG_distribution_sector_total_Vulcan_byLDC.nc"),
                          force_v4=TRUE,
                          varname='methane_emissions',
                          unit='nmol/m2/s',
                          longname=paste0('Methane emissions from natural gas distribution spatially allocated from LDC totals using vulcan sectoral CO2 emissions'),
                          missval=-9999,
                          overwrite=TRUE)
    }
    if(NG_distribution_by_state){
      Summed_NG_dist_vulcan_bystate <- terra::rast(list.files(NG_dist_output_directory,
                                                              pattern="NG_dist_.+_bystate_vulcan",
                                                              full.names = T))
      Summed_NG_dist_vulcan_bystate <- sum(Summed_NG_dist_vulcan_bystate,na.rm=T)
      writeCDF_no_newline(Summed_NG_dist_vulcan_bystate,
                          file.path(output_directory,"NG_distribution_sector_total_Vulcan_bystate.nc"),
                          force_v4=TRUE,
                          varname='methane_emissions',
                          unit='nmol/m2/s',
                          longname=paste0('Methane emissions from natural gas distribution spatially allocated from state totals using vulcan sectoral CO2 emissions'),
                          missval=-9999,
                          overwrite=TRUE)
    }
    if(NG_distribution_by_domain){
      Summed_NG_dist_vulcan_bydomain <- terra::rast(list.files(NG_dist_output_directory,
                                                               pattern="NG_dist_.+_bydomain_vulcan",
                                                               full.names = T))
      Summed_NG_dist_vulcan_bydomain <- sum(Summed_NG_dist_vulcan_bydomain,na.rm=T)
      writeCDF_no_newline(Summed_NG_dist_vulcan_bydomain,
                          file.path(output_directory,"NG_distribution_sector_total_Vulcan_bydomain.nc"),
                          force_v4=TRUE,
                          varname='methane_emissions',
                          unit='nmol/m2/s',
                          longname=paste0('Methane emissions from natural gas distribution spatially allocated from domain totals using vulcan sectoral CO2 emissions'),
                          missval=-9999,
                          overwrite=TRUE)
    }
  }
  
  ################################################################################
  #Save visuals
  
  if(verbose){
    #To simplify the naming/processing needed, lets just write a wrapper
    #function. input_data=list of all data for that sector, total=coded
    #shorthand for sub-sector
    
    wrapper_plot_plus <- function(input_data,total){
      combined_data <- terra::rast(input_data)
      combined_range=terra::global(combined_data,range,na.rm=T)
      zmin <- min(combined_range[,1],na.rm=T)
      zmax <- max(combined_range[,2],na.rm=T)
      
      input_data <- strsplit(basename(input_data),"_")
      
      #grab some text for the plot title
      disaggregation_level <- gsub("by","",sapply(input_data,"[[",length(input_data[[1]])-1))
      inventory_name <- gsub(".nc","",sapply(input_data,"[[",length(input_data[[1]])))
      sector_short <- sapply(input_data,"[[",length(input_data[[1]])-2)
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
      }else if(grepl("post",total)){
        subsector_name <- "post-meter residential leakage and usage"
        subsector_short <- "post_meter"
      }
      
      inventory_name["aces"==inventory_name] <- "ACES"
      inventory_name["vulcan"==inventory_name] <- "Vulcan"
      
      disaggregation_name <- gsub("LDC","local distribution company",disaggregation_level)
      
      # NG_dist_MnR_ACES_res.png
      # stat_comb_com_petr_bystate_aces
      for(A in 1:terra::nlyr(combined_data)){
        not_log_plot(combined_data[[A]],
                     filename=paste0("NG_dist_",sector_short,"_",subsector_short,"_by",
                                     disaggregation_level,"_",tolower(inventory_name))[A],
                     paste0('Methane emissions from natural gas distribution\n',subsector_name,
                            ', spatially allocated from\n',disaggregation_name,
                            ' totals using\n',inventory_name,' ',sector_long,' CO2 emissions')[A],
                     zlim_min = zmin,zlim_max = zmax,plot_directory=NG_dist_plot_directory,
                     domain=domain,County_Tigerlines=County_Tigerlines,
                     State_CB = State_CB)
      }
    }
    
    
    for(total in sapply(strsplit(res_totals,"_"),"[[",1)){
      wrapper_plot_plus(list.files(NG_dist_output_directory,pattern=paste0("NG_dist_",total,".*res.*"),full.names = T),
                        total)
    }
    
    for(total in sapply(strsplit(com_totals,"_"),"[[",1)){
      wrapper_plot_plus(list.files(NG_dist_output_directory,pattern=paste0("NG_dist_",total,".*com.*"),full.names = T),
                        total)
    }
    
    
    
    
    
    
    
    #Now repeat for sector-summed plots
    
    #assume at least some pixels are 0, allow max to be defined exclusively by
    #the data
    zmin <- 0
    zmax <- 0
    
    if(Use_ACES){
      if(NG_distribution_by_LDC){
        if(!all(is.na(terra::values(Summed_NG_dist_ACES_byLDC)))){
          zmax <- max(zmax,as.numeric(terra::global(Summed_NG_dist_ACES_byLDC,max,na.rm=T)))
        }
      }
      if(NG_distribution_by_state){
        if(!all(is.na(terra::values(Summed_NG_dist_ACES_bystate)))){
          zmax <- max(zmax,as.numeric(terra::global(Summed_NG_dist_ACES_bystate,max,na.rm=T)))
        }
      }
      if(NG_distribution_by_domain){
        if(!all(is.na(terra::values(Summed_NG_dist_ACES_bydomain)))){
          zmax <- max(zmax,as.numeric(terra::global(Summed_NG_dist_ACES_bydomain,max,na.rm=T)))
        }
      }
    }
    if(Use_Vulcan){
      if(NG_distribution_by_LDC){
        if(!all(is.na(terra::values(Summed_NG_dist_vulcan_byLDC)))){
          zmax <- max(zmax,as.numeric(terra::global(Summed_NG_dist_vulcan_byLDC,max,na.rm=T)))
        }
      }
      if(NG_distribution_by_state){
        if(!all(is.na(terra::values(Summed_NG_dist_vulcan_bystate)))){
          zmax <- max(zmax,as.numeric(terra::global(Summed_NG_dist_vulcan_bystate,max,na.rm=T)))
        }
      }
      if(NG_distribution_by_domain){
        if(!all(is.na(terra::values(Summed_NG_dist_vulcan_bydomain)))){
          zmax <- max(zmax,as.numeric(terra::global(Summed_NG_dist_vulcan_bydomain,max,na.rm=T)))
        }
      }
    }
    
    
    #now actually plot
    if(Use_ACES){
      if(NG_distribution_by_LDC){
        not_log_plot(Summed_NG_dist_ACES_byLDC,
                     "Natural Gas Distribution emissions\nPHMSA, EIA, and GHGRP activity data combined with GHGI and published\nemission factors at the company level and distributed using aces residential\nand commercial sectoral CO2 emissions",
                     zlim_min=zmin,zlim_max=zmax,plot_directory=plot_directory,
                     domain=domain,County_Tigerlines=County_Tigerlines,
                     State_CB=State_CB)
      }
      if(NG_distribution_by_state){
        not_log_plot(Summed_NG_dist_ACES_bystate,
                     "Natural Gas Distribution emissions\nPHMSA, EIA, and GHGRP activity data combined with GHGI and published\nemission factors state-summed and distributed using aces residential\nand commercial sectoral CO2 emissions",
                     zlim_min=zmin,zlim_max=zmax,plot_directory=plot_directory,
                     domain=domain,County_Tigerlines=County_Tigerlines,
                     State_CB=State_CB)
      }
      if(NG_distribution_by_domain){
        not_log_plot(Summed_NG_dist_ACES_bydomain,
                     "Natural Gas Distribution emissions\nPHMSA, EIA, and GHGRP activity data combined with GHGI and published\nemission factors domain-summed and distributed using aces residential\nand commercial sectoral CO2 emissions",
                     zlim_min=zmin,zlim_max=zmax,plot_directory=plot_directory,
                     domain=domain,County_Tigerlines=County_Tigerlines,
                     State_CB=State_CB)
      }
    }
    if(Use_Vulcan){
      if(NG_distribution_by_LDC){
        not_log_plot(Summed_NG_dist_vulcan_byLDC,
                     "Natural Gas Distribution emissions\nPHMSA, EIA, and GHGRP activity data combined with GHGI and published\nemission factors at the company level and distributed using Vulcan residential\nand commercial sectoral CO2 emissions",
                     zlim_min=zmin,zlim_max=zmax,plot_directory=plot_directory,
                     domain=domain,County_Tigerlines=County_Tigerlines,
                     State_CB=State_CB)
      }
      if(NG_distribution_by_state){
        not_log_plot(Summed_NG_dist_vulcan_bystate,
                     "Natural Gas Distribution emissions\nPHMSA, EIA, and GHGRP activity data combined with GHGI and published\nemission factors state-summed and distributed using Vulcan residential\nand commercial sectoral CO2 emissions",
                     zlim_min=zmin,zlim_max=zmax,plot_directory=plot_directory,
                     domain=domain,County_Tigerlines=County_Tigerlines,
                     State_CB=State_CB)
      }
      if(NG_distribution_by_domain){
        not_log_plot(Summed_NG_dist_vulcan_bydomain,
                     "Natural Gas Distribution emissions\nPHMSA, EIA, and GHGRP activity data combined with GHGI and published\nemission factors domain-summed and distributed using Vulcan residential\nand commercial sectoral CO2 emissions",
                     zlim_min=zmin,zlim_max=zmax,plot_directory=plot_directory,
                     domain=domain,County_Tigerlines=County_Tigerlines,
                     State_CB=State_CB)
      }
    }
    
    
  }
  
  cat("\nFinished natural gas distribution sector: Natural_Gas_Distribution at",format(Sys.time(),"%H:%M"),"with a total runtime of",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes\n\n")
}
