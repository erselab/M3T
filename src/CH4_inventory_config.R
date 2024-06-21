#'@title Config file utilized by main
#'
#'@description This function provides the main with all emission factors, and
#'  similar user-edited input data as well as options for which sectors to run
#'  and, when applicable, which variations to run.
#'
#'@details This function is intended to be the location for all user edited
#'  variables other than inputs to the main `CH4_inventory_build` function.
#'@returns Nothing is returned from the function.  It is run in
#'  `CH4_inventory_build` so that all user-edited settings are utilized
#'  throughout.
#'@author Joe Pitt, \email{madeup@@wisc.edu}
#'@author Kris Hajny, \email{blank@@fake.edu}
#'@author Israel Lopez-Coto, \email{test@@test.edu}
#'@examples
#'main_config()
#'@export

main_config <- function(){
  #Config file for all Sectors.  Modify any of the below variables as desired.
  #Separated by input type and sector.
  
  #Sectors to process
  {
    Process_wetlands_and_inland_waters <- FALSE
    Process_landfills <- FALSE
    Process_natural_gas_distribution <- FALSE	#includes residential post-meter
    Process_natural_gas_transmission <- FALSE
    Process_stationary_combustion <- FALSE
    Process_wastewater <- FALSE
    Incorporate_remaining_sectors_from_gridded_EPA <- FALSE
    Combine_sectors <- FALSE #create total CH4 inventory(s) by summing across sectors
  }
  
  #Variations on the method
  #Several sectors
  {
    Use_ACES <- TRUE
    Use_Vulcan <- TRUE
    #use ACES and/or Vulcan to distribute certain sectors
    
    #Stationary Combustion
    stationary_combustion_by_state <- TRUE
    stationary_combustion_by_domain <- TRUE
    
    #Natural Gas Distribution
    NG_distribution_by_LDC <- FALSE
    NG_distribution_by_state <- TRUE
    NG_distribution_by_domain <- TRUE
    
    #Wastewater
    Wastewater_Municipal_file <- "DMR"
    #either CWNS for 2012 clean watershed needs survey discharge flow or DMR which is
    #provided continuously. The 2 datasets do not seem to be consistent and are missing
    #data from certain facilities.
    Wastewater_Municipal_method <- "Moore_linear"
    #GHGI, Moore_EF, or Moore_linear.  Moore et al.,
    #(https://doi.org/10.1021/acs.est.2c05373) estimated a direct empirical
    #relationship between flow and emission rates using a linear fit on a log-log
    #plot, and also calculated an equivalent organic load (BOD) emission factor.
    #Their estimate was ~2x that of the GHGI.  The GHGI method takes GHGI totals and
    #distributes it to all facilities using flow as a direct proxy for emission magnitude.
    #The emission factor approach is not fully implemented yet and should not be used.
    Wastewater_State_info <- data.frame("State"=c("DE", "MD", "NJ", "NY", "PA"),
                                        "Population"=c(1018396,6164660,9261699,19677151,12972008),
                                        "Septic_Fraction"=c(0.257,0.181,0.116,0.159,0.245),
                                        "Method"=c("scaled","scaled","scaled","reported","scaled"))
    #Pulled from census data.  method is either scaled - i.e., from an old census
    #report, or reported, i.e., use as is from a relatively recent census report
  }
  
  
  
  
  
  
  #Emission factors, GHGI values, and other sector-specific information
  {
    #Landfills
    GHGI_landfill_total <- 3943 #Gg CH4/yr
    #total national municipal landfill emissions from the GHGI.  In a table titled
    #CH4 emissions from Landfills (kt)
    
    #Natural Gas Distribution
    GHGI_natural_gas_pipeline_emission_factors <- data.frame("Leaks_per_mile"=
                                                               c(0.51,1,0.61,0.43),
                                                             "Avg_emissions_mol_per_s"=
                                                               c(2.24,1.72,2,2.03)/(16.043*60)) #converting from g/min to mol/s
    rownames(GHGI_natural_gas_pipeline_emission_factors) <- c("Bare_Steel",
                                                              "Cast_Iron",
                                                              "Coated_steel",
                                                              "Plastic")
    #pipeline emission factors are from Weller et al., 2020 (doi:https://doi.org/10.1021/acs.est.0c00437)
    natural_gas_post_meter_emission_factor <- 7850/401*0.005/(16.043*60*60*24*365)
    #whole-house residential post-meter emission factor from Fischer et al., 2018
    #(doi:https://doi.org/10.1021/acs.est.8b03217).  Reported as 0.5% of residential
    #consumption in a region with 401 Giga cubic feet ~= 7850 giga grams NG consumed
    #/ yr.  This is used as a conversion factor from cubic feet to grams here.  Then
    #convert from g/yr to mol/s.
    
    #Stationary Combustion
    EIA_API_key <- "1kLep4UApTZKwdOrDkW6J8qlO0niiw8ej0JPliyc"
    stationary_combustion_GHGI_data <- data.frame("State"="US_EPA",
                                                  "com_coal"=17,
                                                  "ind_coal"=517,
                                                  "elec_coal"=10554,
                                                  "res_petr"=975,
                                                  "com_petr"=801,
                                                  "ind_petr"=2062,
                                                  "elec_petr"=42,
                                                  "com_gas"=3647,
                                                  "ind_gas"=9484,
                                                  "elec_gas"=11553,
                                                  "res_wood"=544,
                                                  "com_wood"=84,
                                                  "ind_wood"=1407,
                                                  "elec_wood"=68)
    #Data from GHGI in a table titled Fuel Consumption by Stationary Combustion for
    #Calculating CH4 and N2O Emissions (TBtu).  Names must be as such to match those
    #pulled from SEDS.  Res coal doesn't exist in US and res gas is dealt with
    #separately, so not included here.  The desired inventory year (or closest available)
    #should be used.
    stationary_combustion_emission_factors <- data.frame(
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
      "elec_wood"=30
    )
    #All emission factors are in g/GJ.  Emission factors are IPCC defaults except
    #for electric natural gas.  They can be viewed in IPCC 2006 volume 2: Energy tables
    #2.2 through 2.5 (https://www.ipcc-nggip.iges.or.jp/public/2006gl/vol2.html).
    #The natural gas electric sector emission factor is from Hajny et al., 2019
    #(https://doi.org/10.1021/acs.est.9b01875).  It is 5.4 g/MMBTU and within its
    #uncertainties of the GHGI value of 3.9 g/MMBTU.  Note this value is only for
    #Combined Cycle NG power plants, however they make up the vast majority of NG
    #use for electricity in the US.
    
    #Wastewater
    GHGI_national_wastewater_septic <- 227 #kt CH4/yr
    GHGI_national_wastewater_nonseptic <- 246 #kt CH4/yr
    #National totals from the GHGI table titled Domestic Wastewater CH4 Emissions 
    #from Septic and Centralized Systems.  Septic is provided and nonseptic is the sum
    #of all other entries in the table.
    GHGI_septic_EF <- 10.7 #g/capita/day
    #Emission factor from the GHGI table titled Variables and Data Sources for CH4
    #Emissions from Septic Systems.  Originates from 
    #https://decentralizedwater.waterrf.org/documents/DEC1R09/DEC1R09.pdf
    Total_national_open_or_low_int_area <- 352032 #km2
    #National total of developed open space and developed low intensity land cover
    #from the national land cover database from Table 7 of 
    #https://doi.org/10.1016/j.isprsjprs.2020.02.019.
    National_wastewater_info <- data.frame("Year"=c(1990,2021),
                                           "Septic_Fraction"=c(0.241,0.152))
    #Only needed if any states are using the scaled method.  National septic
    #fraction in the year of interest and the year that the scaled states reported a
    #septic fraction (typically 1990)
  }
  #assign to the parent environment
  for(object in ls(envir = environment())){
    assign(x=object,
           value = get(object,envir = environment()),
           envir = parent.env(environment()))
  }
}



