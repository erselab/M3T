#This function provides the 'CH4_inventory_build' function with all emission
#factors and similar user-edited input data as well as options for which 
#sectors to run and, when applicable, which variations to run.

M3T_config <- new.env(parent = emptyenv())

#' Get package options
#'
#' Retrieve one or more options stored in the package environment.
#'
#' @param option Character vector of option names to retrieve. If \code{NULL}, returns all options.
#' @return A named list of option values or a single option value if one name is provided.
#' @export
#' @examples
#' M3T_get_config("Process_landfills")
#' M3T_get_config()
M3T_get_config <- function(option = NULL) {
  if (is.null(option)) {
    as.list(M3T_config)
  } else {
    M3T_config[[option]]
  }
}

#' Set package options
#'
#' Set one or more options in the package environment.
#'
#' @param ... Named option values to set.
#' @return Invisibly returns \code{NULL}.
#' @export
#' @examples
#' M3T_set_config(Process_landfills = FALSE)
#' M3T_set_config(Source_GHGRP_facility_data = "default", Source_GHGI = "default")
M3T_set_config <- function(...) {
  args <- list(...)
  for (name in names(args)) {
    M3T_config[[name]] <- args[[name]]
  }
  invisible(NULL)
}





#options for terra and downloading
M3T_config$Terra_datatype <- "FLT8S"
M3T_config$Terra_progress <- 0
M3T_config$Base_timeout <- 60*20


#Settings applicable to multiple sectors
{
  #Method variations
  {
    #used in NG distribution and stationary combustion for downscaling
    M3T_config$Use_ACES <- TRUE
    M3T_config$Use_Vulcan <- TRUE
  }
  
  #How to access datasets
  {
    #Census tigerlines
    M3T_config$Source_Tigerlines_data <- "M3T"
    
    #EPA GHGRP/GHGI data used across >1 sector
    M3T_config$Source_GHGRP_facility_data <- "M3T"
    M3T_config$Source_GHGRP_combustion="M3T"
    M3T_config$Source_GHGI="M3T"
    M3T_config$Source_GHGRP_NG="M3T"
    
    #solely for visuals
    M3T_config$Source_Cartographic_Boundaries_data <- "M3T"
    
    #used in downscaling (see below)
    M3T_config$Source_ACES="M3T"
    M3T_config$Source_Vulcan="download"
  }
}









#Landfill settings
{
  M3T_config$Process_landfills <- TRUE
  
  #Method variations
  {
    #2 GHGRP methods (reported = facility chosen method between the 2, will vary
    #across domain)
    M3T_config$landfill_ghgrp_reported <- TRUE
    M3T_config$landfill_ghgrp_modeled <- TRUE
    M3T_config$landfill_ghgrp_collection_efficiency <- TRUE
  }
  
  #How to access datasets
  {
    M3T_config$Source_GHGRP_landfills="M3T"
    M3T_config$Source_LMOP="M3T"
  }
  
  #Emission factors, and similar
  {
    M3T_config$GHGI_landfill_total <- "GHGI"
  }
}









#Natural gas distribution settings - including residential post-meter
{
  M3T_config$Process_natural_gas_distribution <- TRUE
  
  #Method variations
  {
    #aggregation before disaggregating to pixel
    M3T_config$NG_distribution_by_LDC <- FALSE
    M3T_config$NG_distribution_by_state <- TRUE
    M3T_config$NG_distribution_by_domain <- TRUE
  }
  
  #How to access datasets
  {
    M3T_config$Source_EIA_NG_file = "M3T"
    M3T_config$Source_PHMSA_file = "M3T"
    M3T_config$Source_GHGRP_LDC = "M3T"
  }
  
  #Emission factors, and similar
  {
    #pipeline emission factors from Weller et al., 2020
    #(doi:https://doi.org/10.1021/acs.est.0c00437) converted from g/min to mol/s
    M3T_config$natural_gas_pipeline_emission_factors <- data.frame("Leaks_per_mile"              =c(0.51,1.00,0.61,0.43),
                                                                   "Avg_emissions_mol_per_s"     =c(2.24,1.72,2.00,2.03)/(16.043*60))
    rownames(M3T_config$natural_gas_pipeline_emission_factors) <- c("Bare_Steel",
                                                                    "Cast_Iron",
                                                                    "Coated_steel",
                                                                    "Plastic")
    #whole-house residential post-meter emission factor from Fischer et al.,
    #2018 (doi:https://doi.org/10.1021/acs.est.8b03217).  Reported as 0.5% of
    #residential consumption in a region with 401 Giga cubic feet ~= 7850 giga
    #grams NG consumed / yr.  This is used as a conversion factor from cubic
    #feet to grams here.  Then convert from g/yr to mol/s.
    M3T_config$natural_gas_res_post_meter_emission_factor <- 0.5/100  *7850/401/(16.043*60*60*24*365)
    M3T_config$natural_gas_com_post_meter_emission_factor <- 0
    
    #emission factors and activity data for local distribution company pipeline
    #components
    M3T_config$GHGI_MnR <- "GHGI"
    M3T_config$GHGI_maintenance <- "GHGI"
    M3T_config$GHGI_meters <- "GHGI"
    M3T_config$GHGI_services <- "GHGI"
  }
}










#Natural gas transmission settings
{
  M3T_config$Process_natural_gas_transmission <- TRUE
  
  #How to access datasets
  {
    M3T_config$Source_HIFLD_compressor_file="M3T"
    M3T_config$Source_EIA_transmission_file="M3T"
  }
  
  #Emission factors, and similar
  {
    #emission factors and activity data for local distribution company pipeline
    #components
    M3T_config$GHGI_Pipeline <- "GHGI"
    M3T_config$GHGI_transmission_compressors <- "GHGI"
  }
}










#Stationary combustion settings
{
  M3T_config$Process_stationary_combustion <- TRUE
  
  #Method variations
  {
    #aggregation before disaggregating to counties
    M3T_config$stationary_combustion_by_state <- TRUE
    M3T_config$stationary_combustion_by_domain <- TRUE
  }
  
  #How to access datasets
  {
    M3T_config$Source_EIA_SEDS_data="M3T"
    M3T_config$Source_NEI_data="M3T"
  }
  
  #Emission factors, and similar
  {
    #GHGI activity data
    M3T_config$stationary_combustion_GHGI_data <- "GHGI"
    
    #IPCC EFs - Hajny et al. for elec-gas
    M3T_config$stationary_combustion_emission_factors <- data.frame(
      "com_coal"=10,
      "ind_coal"=10,
      "elec_coal"=1,
      "res_petr"=10,
      "com_petr"=10,
      "ind_petr"=3,
      "elec_petr"=3,
      "com_gas"=5,
      "ind_gas"=1,
      "elec_gas"=5.4/(1.0550559*0.9), #g/mmbtu to g/GJ and low to high heating value (0.9)
      "res_wood"=300,
      "com_wood"=300,
      "ind_wood"=30,
      "elec_wood"=30)
  }
}










#Wastewater settings
{
  M3T_config$Process_wastewater <- TRUE
  
  #Method variations
  {
    #2 datasets with flow activity data by site
    M3T_config$Wastewater_use_CWNS <- TRUE
    M3T_config$Wastewater_use_DMR <- TRUE
    
    #Downscaling GHGI estimate or use Moore et al. published EF
    M3T_config$Wastewater_Municipal_Method_Moore_EF <- TRUE
    M3T_config$Wastewater_Municipal_Method_GHGI <- TRUE
    
    #National septic fraction or state level septic fraction data
    M3T_config$Wastewater_national_septic <- TRUE
    M3T_config$Wastewater_state_septic <- TRUE
  }
  
  #How to access datasets
  {
    M3T_config$Source_wastewater_NLCD="M3T"
    M3T_config$Source_CWNS="M3T"
    M3T_config$Source_DMR="M3T"
    M3T_config$Source_State_population_data="M3T"
    M3T_config$Source_GHGRP_wastewater="M3T"
  }
  
  #Emission factors, and similar
  {
    #GHGI emission data and EFs
    M3T_config$GHGI_wastewater_data <- data.frame("EF"=rep(10.7,13),#from Leverenz et al. 2010 - https://www.waterrf.org/research/projects/evaluation-greenhouse-gas-emissions-septic-systems
                                                  "Septic Emissions"=   c(204, 200, 204, 240, 236, 236, 240, 236, 236, 232, 227, 223, 215),
                                                  "Nonseptic Emissions"=c(108, 104, 108, 128, 124, 124, 116, 104, 100, 250, 246, 273, 270),
                                                  "year"=               c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022))
    
    #National total of developed open space and developed low intensity land
    #cover from the NLCD in km2
    M3T_config$Total_national_open_or_low_int_area <- "M3T"
    
    #1990 census data on state estimated septic fractions.
    M3T_config$Wastewater_State_info <- data.frame("State"          =c("AL"    ,"AZ"    ,"AR"    ,"CA"      ,"CO"    ,"CT"    ,"DE"    ,"DC"    ,"FL"      ,"GA"    ,"ID"    ,"IL"    ,"IN"    ,"IA"    ,"KS"    ,"KY"    ,"LA"    ,"ME"    ,"MD"    ,"MA"      ,"MI"    ,"MN"    ,"MS"    ,"MO"    ,"MT"    ,"NE"    ,"NV"    ,"NH"    ,"NJ"    ,"NM"    ,"NY"      ,"NC"    ,"ND"    ,"OH"      ,"OK"    ,"OR"    ,"PA"    ,"RI"    ,"SC"    ,"SD"    ,"TN"    ,"TX"      ,"UT"    ,"VT"    ,"VA"    ,"WA"    ,"WV"    ,"WI"    ,"WY"),
                                                   "Septic_Fraction"=c(0.4360  ,0.1700  ,0.3820  ,0.0980    ,0.1240  ,0.2860  ,0.2570  ,0.0020  ,0.1265    ,0.3680  ,0.3460  ,0.1330  ,0.3130  ,0.2320  ,0.1790  ,0.3980  ,0.2580  ,0.5130  ,0.1810  ,0.2670    ,0.2830  ,0.2530  ,0.3830  ,0.2420  ,0.3750  ,0.1780  ,0.1170  ,0.4900  ,0.1160  ,0.2550  ,0.1615    ,0.4850  ,0.2410  ,0.2150    ,0.2610  ,0.2930  ,0.2450  ,0.2860  ,0.4060  ,0.2680  ,0.3860  ,0.1810    ,0.1090  ,0.5500  ,0.2830  ,0.3100  ,0.4080  ,0.2830  ,0.2410))
    
    #All state septic fractions from the american housing survey as of 2025.
    M3T_config$Wastewater_reported_State_info <- rbind(data.frame("State"          =rep("CA",3),
                                                                  "Year"           =c(2015  ,2021  ,2023),
                                                                  "Septic_Fraction"=c(0.0645,0.0408,0.0560)),
                                                       
                                                       data.frame("State"          =rep("FL",3),
                                                                  "Year"           =c(2017  ,2019  ,2023),
                                                                  "Septic_Fraction"=c(0.1509,0.1265,0.1611)),
                                                       
                                                       data.frame("State"          ="MA",
                                                                  "Year"           =2023,
                                                                  "Septic_Fraction"=0.2403),
                                                       
                                                       data.frame("State"          =rep("NY",4),
                                                                  "Year"           =c(2015  , 2019  ,2021  ,2023),
                                                                  "Septic_Fraction"=c(0.2083,0.1626,0.1592,0.2029)),
                                                       
                                                       data.frame("State"          ="OH",
                                                                  "Year"           =2015,
                                                                  "Septic_Fraction"=0.1963),
                                                       
                                                       data.frame("State"          =rep("TX",2),
                                                                  "Year"           =c(2015  ,2023),
                                                                  "Septic_Fraction"=c(0.1441,0.1276)))
    
    
    #National septic fraction for all years available
    M3T_config$National_wastewater_info <- data.frame("Year"           =c(1990 ,2011  ,2013  ,2015  ,2017  ,2019  ,2021  ,2023),
                                                      "Septic_Fraction"=c(0.241,0.1949,0.1856,0.1986,0.1791,0.1635,0.1522,0.1858))
  }
}










#Wetlands settings
{
  M3T_config$Process_wetlands_and_inland_waters <- TRUE
  
  #Method variations
  {
    #Methodology for Wetland and freshwater methane emissions.  State Of the
    #Carbon Cycle Report (SOCCR) emission factors combined with national wetland
    #inventory data, or downscaled wetcharts model.
    M3T_config$Use_SOCCR1 <- TRUE
    M3T_config$Use_SOCCR2 <- TRUE
    M3T_config$Use_Wetcharts <- TRUE
    
    M3T_config$Wetcharts_model_subset <- list(c(1913,1914,1923,1924,1933,1934,2913,2914,2923,
                                                2924,2933,2934,3913,3914,3923,3924,3933,3934))
  }
  
  #How to access datasets
  {
    M3T_config$Source_wetland_NLCD="M3T"
    M3T_config$Source_Watershed_file="M3T"
    M3T_config$Source_wetcharts="M3T"
    M3T_config$Source_NWI="M3T"
  }
  
  #Emission factors, and similar
  {
    #State of the Carbon Cycle Report EFs in g CH4 per m2 per yr
    M3T_config$Wetland_EFs <- data.frame("E2_Atlantic"=c(10.3,20.43),
                                         "M2_Atlantic"=c(10.3,20.43),
                                         "E2_Gulf"=    c(10.3,27.47),
                                         "M2_Gulf"=    c(10.3,27.47),
                                         "E2_Pacific"= c(10.3,21.87),
                                         "M2_Pacific"= c(10.3,21.87),
                                         "E2_Hudson"=  c(10.3,21.87),
                                         "M2_Hudson"=  c(10.3,21.87),
                                         "PFO"=        c(36  ,24.74),
                                         "PNF"=        c(36  ,33.28),
                                         "L1"=5,
                                         "L2"=5,
                                         "R1"=7.88,
                                         "R2"=7.88,
                                         "R3"=7.88,
                                         "R4"=7.88)
    rownames(M3T_config$Wetland_EFs) <- c("SOCCR1","SOCCR2")
  }
}










#Remaining sector settings
{
  M3T_config$Process_remaining_sectors_from_gridded_EPA <- TRUE
  
  #How to access datasets
  {
    M3T_config$Source_GEPA="download"
  }
}










#Combined inventory settings
{
  M3T_config$Combine_sectors <- TRUE
  
  #Method variations
  {
    M3T_config$Separate_thermo=TRUE
    M3T_config$Create_summary_combinations=TRUE
    M3T_config$Create_individual_combinations=FALSE
  }
}







