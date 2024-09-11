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
  
  #Set how some gridded data is saved. Integer (INT, whole number) values or
  #float (FLT, decimal numbers) are allowed and signed (S) or unsigned (U)
  #values too (i.e., positive vs negative). Lastly a number is related to the
  #precision, and thus size of the output in memory.  Highly recommend FLT8S if
  #memory is available or FLT4S otherwise.
  
  #Options are: "INT1U", "INT1S", "INT2U", "INT2S", "INT4U", "INT4S", "INT8U",
  #"INT8S", "FLT4S", "FLT8S".
  
  #also setting progress=0 to prevent progress bars for steps that take a longer
  #time/memory.  Individual steps won't be clear to the user, so these are not
  #useful.
  terraOptions(datatype="FLT8S",progress=0)
  
  #Sectors to process
  {
    Process_wetlands_and_inland_waters <- FALSE
    Process_landfills <- FALSE
    Process_natural_gas_distribution <- FALSE	#includes residential post-meter
    Process_natural_gas_transmission <- FALSE
    Process_stationary_combustion <- TRUE
    Process_wastewater <- FALSE
    Incorporate_remaining_sectors_from_gridded_EPA <- FALSE
    Combine_sectors <- FALSE #create total CH4 inventory(s) by summing across sectors
  }
  
  #Variations on the method
  {
    #Several sectors
    Use_ACES <- TRUE
    Use_Vulcan <- TRUE
    #use ACES and/or Vulcan to distribute certain sectors
    
    #Stationary Combustion
    stationary_combustion_by_state <- TRUE
    stationary_combustion_by_domain <- TRUE
    
    #Natural Gas Distribution
    NG_distribution_by_LDC <- TRUE
    NG_distribution_by_state <- TRUE
    NG_distribution_by_domain <- TRUE
    
    #Wastewater
    Wastewater_use_CWNS <- TRUE
    Wastewater_use_DMR <- TRUE
    #either CWNS for 2012 clean watershed needs survey discharge flow or DMR which is
    #provided continuously. The 2 datasets do not seem to be consistent and are missing
    #data from certain facilities.
    Wastewater_Municipal_Method_Moore_linear <- TRUE
    Wastewater_Municipal_Method_Moore_EF <- FALSE
    Wastewater_Municipal_Method_GHGI <- TRUE
    # Wastewater_Municipal_method <- "Moore_linear"
    #GHGI, Moore_EF, or Moore_linear.  Moore et al.,
    #(https://doi.org/10.1021/acs.est.2c05373) estimated a direct empirical
    #relationship between flow and emission rates using a linear fit on a log-log
    #plot, and also calculated an equivalent organic load (BOD) emission factor.
    #Their estimate was ~2x that of the GHGI.  The GHGI method takes GHGI totals and
    #distributes it to all facilities using flow as a direct proxy for emission magnitude.
    #The emission factor approach is not fully implemented yet and should not be used.
    Wastewater_national_septic <- TRUE
    Wastewater_state_septic <- TRUE
    Wastewater_State_info <- data.frame("State"=c("DE", "MD", "NJ", "NY", "PA"),
                                        "Population"=c(1018396,6164660,9261699,19677151,12972008),
                                        "Septic_Fraction"=c(0.257,0.181,0.116,0.159,0.245),
                                        "Method"=c("scaled","scaled","scaled","reported","scaled"))
    Wastewater_State_info[,4] <- tolower(Wastewater_State_info[,4]) #just in case manually entered with caps
    #Pulled from census data.  method is either scaled - i.e., from an old
    #census report, or reported, i.e., use as is from a relatively recent census
    #report.  Only used if state_septic=TRUE
    
    
    
    #Wetlands
    #Methodology for Wetland and freshwater methane emissions.  State Of the
    #Carbon Cycle Report emission factors combined with national wetland
    #inventory data, or downscaled wetcharts.
    Use_SOCCR1 <- TRUE
    Use_SOCCR2 <- TRUE
    Use_Wetcharts <- TRUE
    
    #Use national wetlands inventory and Rosentreter et al data to estimate
    #freshwater wetland emissions.
    Include_freshwater <- TRUE
    
    #landcover data that will be used to downscale wetcharts from 0.5 deg to 0.1
    #deg.  Only relevant if use_wetcharts is true
    Use_NLCD <- TRUE
    Use_NALCMS <- TRUE
    
    # Wetcharts models are defined with digit 1 = global scale factor (1=124.5
    # Tg/yr, 2=166 Tg/yr, 3=207.5 Tg/yr), digit 2 = heterotrophic respiration
    # model (1-8=MsTMIP models, 9=CARDAMOM), 3 = temperature dependence (CH4:C
    # q10 value of 1 - 3), and 4 = extent parameterization (1=SWAMPS+GLWD,
    # 2=SWAMPS+GLOBCOVER, 3=PREC+GLWD, 4=PREC+GLOBCOVER) as described in the
    # user guide
    # https://daac.ornl.gov/CMS/guides/MonthlyWetland_CH4_WetCHARTs.html
    
    #Users can provide a single list (the models to be used, top example) or
    #several (run multiple variations with different model subsets, bottom
    #example).  Ma et al. https://doi.org/10.1029/2021AV000408 ranked model
    #performance as compared to a GOSAT-based inversion and some subsequent
    #works subset to the 9 highest performing models, though Nesser et al.
    #https://doi.org/10.5194/acp-24-5069-2024 further subset these to only 7 as
    #2 showed overestimation in North America compared to GOSAT in Lu et al.
    #https://doi.org/10.5194/acp-22-395-2022
    
    #1 wetcharts output
    Wetcharts_model_subset <- list(c(1913,1914,1923,1924,1933,1934,2913,2914,2923,
                                     2924,2933,2934,3913,3914,3923,3924,3933,3934)) #all models
    #2 wetcharts outputs
    # Wetcharts_model_subset <- list(c(1913,1914,1923,1924,1933,1934,2913,2914,2924), #Ma subset
    #                                c(1913,1914,1924,1933,1934,2914,2924)) #Nesser subset
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
    
    #All emission factors are in g/GJ.  Emission factors are IPCC defaults except
    #for electric natural gas.  They can be viewed in IPCC 2006 volume 2: Energy tables
    #2.2 through 2.5 (https://www.ipcc-nggip.iges.or.jp/public/2006gl/vol2.html).
    #The natural gas electric sector emission factor is from Hajny et al., 2019
    #(https://doi.org/10.1021/acs.est.9b01875).  It is 5.4 g/MMBTU and within its
    #uncertainties of the GHGI value of 3.9 g/MMBTU.  Note this value is only for
    #Combined Cycle NG power plants, however they make up the vast majority of NG
    #use for electricity in the US.
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
      "elec_wood"=30)
    
    
    
    #wetlands SOCCR-based wetlands emission factors.  SOCCR1 values comes from
    #the arithmetic averages of Table F5
    #(https://www.carboncyclescience.us/state-carbon-cycle-report-soccr), SOCCR2
    #values come from the arithmetic averages of Tables 13B.8 to 13B.11 for PFO
    #and PNF and Table 15A.2 for M2 and E2, limiting values to only those with
    #salinity >=0.5 (https://carbon2018.globalchange.gov/).  SOCCR2 values are
    #in g C of CH4/m2/yr, so are converted to g CH4/m2/yr first to be consistent
    #with SOCCR1.  SOCCR2 also separated tidal emissions by region, so there is
    #a gulf of mexico, Atlantic, Hudson, and Pacific value (as mapped here:
    #http://www.cec.org/north-american-environmental-atlas/watersheds/), though
    #since there is no Hudson or Pacific data, the average across all data is
    #used.
    
    # Inland water CH4 fluxes are not included in either SOCCR1 or SOCCR2 For
    # lakes, McDonald et al. (10.4319/lo.2012.57.2.0597) show that large lakes
    # > 1 km2 constitute 71% of the total lake area in the contiguous US
    # (rising to 90% if the Great Lakes are included) So use the median flux
    # from the largest lakes class (>1 km) from Rosentreter et al.
    # (10.1038/s41561-021-00715-2) Also use the median river flux from
    # Rosentreter et al. Both this and the lake flux come from extended data
    # table 1
    
    #As per the National Wetlands Inventory, defined here
    #https://www.fws.gov/media/national-wetland-inventory-wetlands-and-deepwater-map-code-diagram
    #and discussed in detail here
    #https://www.fws.gov/media/classification-wetlands-and-deepwater-habitats-united-states
    # M2 = marine, intertidal
    # E2 = estuarine, intertidal
    # R1 = riverine, tidal
    # R2 = riverine, lower perrennial
    # R3 = riverine, upper perennial
    # R4 = riverine, intermittent
    # L1 = lacustrine, limnetic
    # L2 = lacustrine, littoral 
    # PFO = palustrine, forested
    # PNF = palustrine, all non-forested classes
    
    # the first 10 are only relevant if SOCCR1 or SOCCR2 are used.  The last 6
    # are only relevant if freshwater emissions are included.
    
    Wetland_EFs <- data.frame("E2_Atlantic"=c(10.3,20.43), 
                              "M2_Atlantic"=c(10.3,20.43),
                              "E2_Gulf"=c(10.3,27.47), 
                              "M2_Gulf"=c(10.3,27.47),
                              "E2_Pacific"=c(10.3,21.87), 
                              "M2_Pacific"=c(10.3,21.87),
                              "E2_Hudson"=c(10.3,21.87), 
                              "M2_Hudson"=c(10.3,21.87),
                              "PFO"=c(36,24.74),
                              "PNF"=c(36,33.28),
                              "L1"=5,
                              "L2"=5,
                              "R1"=7.88,
                              "R2"=7.88,
                              "R3"=7.88,
                              "R4"=7.88)
    rownames(Wetland_EFs) <- c("SOCCR1","SOCCR2")

    # convert from g CH4 per m2 per yr to nmol/m2/s
    Wetland_EFs=Wetland_EFs*1E9/(16.043*365.25*24*60*60)      
    
    
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
  
  #assign all to the parent environment as this should only be called by
  #CH4_inventory_main.R
  for(object in ls(envir = environment())){
    assign(x=object,
           value = get(object,envir = environment()),
           envir = parent.env(environment()))
  }
}



