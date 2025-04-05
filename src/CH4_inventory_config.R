#'@title Config file utilized by main
#'
#'@description This function provides the main with all emission factors and
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
  #time.  Individual steps won't be clear to the user, so these are not useful.
  terraOptions(datatype="FLT8S",progress=0)
  
  #multiple input datasets will be downloaded.  The time allowed for a download
  #defaults to 1 minute, adjust that to 20 minutes given the size of some files
  #and in case of a slow internet speed.  May need to increase if internet is
  #slow and you get download errors but the links are valid.
  options("timeout"=60*20)
  
  #Sectors to process
  {
    Process_landfills <- TRUE
    Process_natural_gas_distribution <- TRUE	#includes residential post-meter
    Process_natural_gas_transmission <- TRUE
    Process_stationary_combustion <- TRUE
    Process_wastewater <- TRUE
    Process_wetlands_and_inland_waters <- TRUE
    Incorporate_remaining_sectors_from_gridded_EPA <- TRUE
    Combine_sectors <- FALSE #create total CH4 inventory(s) by summing across sectors - not yet developed
    EIA_API_key <- "1kLep4UApTZKwdOrDkW6J8qlO0niiw8ej0JPliyc"
  }
  
  
  
  #Variations on the method
  {
    #Several sectors use ACES and/or Vulcan to distribute emissions.  
    #ACES v2.0 is available at \url{https://doi.org/10.3334/ORNLDAAC/1943},
    #though the hourly file should be averaged across hours to create an
    #annually averaged inventory. Vulcan v3.0 is available at
    #\url{https://doi.org/10.3334/ORNLDAAC/1741}, and the annual mean files
    #should be used.  At least one of Use_Vulcan or Use_ACES must be TRUE.
    Use_ACES <- TRUE
    Use_Vulcan <- TRUE
    
    #landfills
    #Landfills have 2 options for reporting their emissions - equation HH-6 and
    #HH-8.  HH-6 is based on a first order decay model, HH-8 is based on the
    #assumed collection efficiency of a gas collection system.  You can use the
    #one they report, or force it to the model or gas collection efficiency
    #value.  Note landfills without gas collection systems will still be
    #included using the modeled emission rate if forcing to the collection
    #efficiency value.
    landfill_ghgrp_reported <- TRUE
    landfill_ghgrp_modeled <- TRUE
    landfill_ghgrp_collection_efficiency <- TRUE
    
    #Stationary Combustion
    #disaggregate state or domain level total emissions to the county, then
    #pixel scale
    stationary_combustion_by_state <- TRUE
    stationary_combustion_by_domain <- TRUE
    
    #Natural Gas Distribution
    #disaggregate local distribution company (LDC), state, or domain level total
    #emissions to the pixel scale.  By LDC requires use of a function that is
    #not completely automated and is not in the normal workflow.
    NG_distribution_by_LDC <- FALSE
    NG_distribution_by_state <- TRUE
    NG_distribution_by_domain <- TRUE
    
    #Wastewater
    #either CWNS for clean watershed needs survey discharge flow which is
    #reported infrequently (2012 and 2022) or DMR for discharge monitoring
    #reports which are provided near-continuously. The 2 datasets do not seem to
    #be consistent and each is missing data from certain facilities that are in
    #the other.  Emails with the departments suggest CWNS is about need while
    #DMR reports active flow data.  

    #The discharge monitoring report data is available at
    #\url{https://echo.epa.gov/trends/loading-tool/water-pollution-search}.  Set
    #the industry type to Publicly Owned Treatment Works in the search tool, set
    #the year, and select wastewater flow under the pollutant categories.  After
    #searching, scroll to the bottom table where flow separated by facility is
    #shown and select download all data.  This will produce a csv with 3 rows as
    #header, columns as different variables and rows as different facilities.
    #The variables Facility Name, Facility Latitude, Facility Longitude and
    #Average Flow (MGD) are used.

    #The 2012 Clean Watershed Needs report data or the folder containing the
    #2022 data from the report -  available at
    #\url{https://www.epa.gov/cwns/clean-watersheds-needs-survey-cwns-2012-report-and-data}
    #and
    #\url{https://www.epa.gov/cwns/clean-watersheds-needs-survey-cwns-2022-report-and-data},
    #respectively. For 2012, find the data download link near the bottom of the
    #page.  This will download all data as an access database.  To convert to a
    #useable excel file:
    # \itemize{
    #   \item Open mdb file in Microsoft Access
    #   \item Go to Create tab -> Query Wizard
    #   \item Select Simple Query Wizard
    #   \item Choose the first table you want (SUMMARY_FACILITY)
    #   \item Click the double right arrow to take all columns
    #   \item Repeat for other table (SUMMARY_FACILITY_FLOW)
    #   \item Click Finish
    #   \item In the left hand pane, make sure you have selected to view all Access objects
    #   \item Your query should be here at the bottom – right click on it and select to export to Excel (.xlsx)
    #   \item Note that Access seems to automatically save this query to the access file
    # }
    #The resulting excel file should have a separate row for each facility and
    #different columns for different variables.  FACILITY_NAME, LATITUDE,
    #LONGITUDE, HORIZONTAL_COORDINATE_DATUM, and EXIST_MUNICIPAL are used.
    #There is an example file in the package's datasets folder that has been
    #successfully used in this code available for reference. For the 2022 data
    #there is a link to the data dashboard which has a data download tab.
    #Download the data as CSVs.
    Wastewater_use_CWNS <- TRUE
    Wastewater_use_DMR <- TRUE
    
    #GHGI, Moore_EF, or Moore_linear approach to calculate municipal wastewater
    #treatment plant emissions.  Moore et al.,
    #(https://doi.org/10.1021/acs.est.2c05373) estimated a direct empirical
    #relationship between flow and emission rates using a linear fit on a
    #log-log plot, and also calculated an equivalent organic load (BOD) emission
    #factor. Their estimate was ~2x that of the GHGI.  The GHGI method takes
    #GHGI totals and distributes it to all facilities using flow as a proxy for
    #emission The emission factor approach is not fully implemented yet and
    #should not be used.
    Wastewater_Municipal_Method_Moore_linear <- TRUE
    Wastewater_Municipal_Method_Moore_EF <- FALSE
    Wastewater_Municipal_Method_GHGI <- TRUE
    
    #Rely on state level data on septic fraction and population (input along
    #with emission factors below) or rely on national septic fraction and a
    #published total of suburban (impervious surface <50%) land cover.
    Wastewater_national_septic <- TRUE
    Wastewater_state_septic <- TRUE
    
    
    
    #Wetlands
    #Methodology for Wetland and freshwater methane emissions.  State Of the
    #Carbon Cycle Report (SOCCR) emission factors combined with national wetland
    #inventory data, or downscaled wetcharts model.
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
    #performance as compared to a GOSAT satellite-based inversion and some
    #subsequent works subset to the 9 highest performing models, though Nesser
    #et al. https://doi.org/10.5194/acp-24-5069-2024 further subset these to
    #only 7 as 2 showed overestimation in North America compared to GOSAT in Lu
    #et al. https://doi.org/10.5194/acp-22-395-2022
    
    #1 wetcharts output
    Wetcharts_model_subset <- list(c(1913,1914,1923,1924,1933,1934,2913,2914,2923,
                                     2924,2933,2934,3913,3914,3923,3924,3933,3934)) #all models
    ##2 wetcharts outputs
    # Wetcharts_model_subset <- list(c(1913,1914,1923,1924,1933,1934,2913,2914,2924), #Ma subset
    #                                c(1913,1914,1924,1933,1934,2914,2924)) #Nesser subset
  }
  
  
  
  
  
  
  #Emission factors, GHGI values, and other sector-specific information
  {
    #Landfills
    #total national municipal landfill emissions from the GHGI.  In a table
    #titled CH4 emissions from Landfills (kt)
    # GHGI_landfill_total <- 3924 #Gg CH4/yr, newer GHGI value for 2019
    # GHGI_landfill_total <- 3943 #Gg CH4/yr older GHGI value for 2019
    GHGI_landfill_total <- 3978 #Gg CH4/yr from Joe
    
    
    
    
    #Natural Gas Distribution
    #pipeline emission factors from Weller et al., 2020 (doi:https://doi.org/10.1021/acs.est.0c00437)
    GHGI_natural_gas_pipeline_emission_factors <- data.frame("Leaks_per_mile"         =c(0.51,1.00,0.61,0.43),
                                                             "Avg_emissions_mol_per_s"=c(2.24,1.72,2.00,2.03)/(16.043*60)) #converting from g/min to mol/s
    rownames(GHGI_natural_gas_pipeline_emission_factors) <- c("Bare_Steel",
                                                              "Cast_Iron",
                                                              "Coated_steel",
                                                              "Plastic")
    #whole-house residential post-meter emission factor from Fischer et al., 2018
    #(doi:https://doi.org/10.1021/acs.est.8b03217).  Reported as 0.5% of residential
    #consumption in a region with 401 Giga cubic feet ~= 7850 giga grams NG consumed
    #/ yr.  This is used as a conversion factor from cubic feet to grams here.  Then
    #convert from g/yr to mol/s.
    natural_gas_post_meter_emission_factor <- 7850/401*0.005/(16.043*60*60*24*365)
    # natural_gas_post_meter_emission_factor <- 7850/401*0.025/(16.043*60*60*24*365)
    
    #emission factors for local distribution company pipeline components in
    #kg/activity.  Either provided directly or set to GHGI to indicate that
    #these emission factors and activity data should be pulled from the
    #appropriate GHGI annex file instead (for the 2022 GHGI -
    #https://www.epa.gov/ghgemissions/natural-gas-and-petroleum-systems-ghg-inventory-additional-information-1990-2022-ghg).
    #Values listed below are from the 2022 GHGI Annex file for the year 2019.
    # GHGI_MnR <- data.frame("Type"          =c("M&R >300"        ,"M&R 100-300"     ,"M&R <100"        ,"Reg >300"        ,"R-Vault >300"    ,"Reg 100-300"     ,"R-Vault 100-300" ,"Reg 40-100"      ,"R-Vault 40-100"  ,"Reg <40"),
    #                        "EF"            =c(4.235180452534E-03,1.967524777161E-03,1.437293523655E-03,1.717415695319E-03,1.000436327370E-04,2.834569594216E-04,1.000436327370E-04,3.234744125164E-04,1.000436327370E-04,4.435267718008E-05),
    #                        "Total_stations"=c(4134.563          ,15088.505         ,8064.842          ,4520.347          ,4077.839          ,13674.863         ,12825.308         ,41036.497         ,9219.463          ,17400.594))
    # GHGI_maintenance <- data.frame("Type"=c("Pressure Relief Valve Releases","Pipeline Blowdown","Mishaps (Dig-ins)"),
    #                                "EF"  =c(1.83231029619102E-06            ,1.7916071884252E-06,5.93205444189599E-05))
    # GHGI_meters <- data.frame("Type"=c("Residential"       ,"Commercial"        ,"Industrial"),
    #                           "EF"  =c(2.94314949875421E-06,4.62512704868224E-05,2.07537752184459E-4))
    # GHGI_services <- data.frame("Type"=c("Services - Unprotected steel","Services Protected steel","Services - Plastic","Services - Copper"),
    #                             "EF"  =c(2.86336031045332E-05          ,2.56013867312366E-06      ,5.19820797350566E-07,9.68176516012578E-06))
    GHGI_MnR <- "GHGI"
    GHGI_maintenance <- "GHGI"
    GHGI_meters <- "GHGI"
    GHGI_services <- "GHGI"
    
    
    
    #natural gas transmission
    #national emissions and activity data to calculate emission factors per
    #component in kt/yr and counts/miles.   Either provided directly or set to
    #GHGI to indicate that these data should be pulled from the appropriate GHGI
    #annex file instead (for the 2022 GHGI -
    #https://www.epa.gov/ghgemissions/natural-gas-and-petroleum-systems-ghg-inventory-additional-information-1990-2022-ghg).
    #Values currently listed below are from the 2022 GHGI Annex file for the
    #year 2019.
    # GHGI_Pipeline <- data.frame("Type"          =c("Pipeline Leaks","M&R (Trans. Co. Interconnect)", "M&R (Farm Taps + Direct Sales)", "Pipeline venting"),
    #                             "Emissions"     =c(6.52811286997864,148.988084201472               ,34.5960442337367                 ,370.144374243981),
    #                             "Total_stations"=c(486570674.304   ,4331358.53227287               ,128429200.235495                 ,486570674.304))
    # GHGI_transmission_compressors <- data.frame("Type"          =c("Station Total Emissions","Dehydrator vents (Transmission)","Flaring (Transmission)","Engines (Transmission)","Turbines (Transmission)","Engines (Storage)","Turbines (Storage)","Generators (Engines)","Generators (Turbines)","Pneumatic Devices Transmission","Station Venting Transmission"),
    #                                             "Emissions"     =c(1357.80542776983         ,5.03531064009076                 ,1.00340431488143        ,306.078942722016        ,3.21765775547095         ,48.1136980930844   ,0.401407734674907   ,27.7850987048789     ,0.00778115471743365   ,73.1035216017783                ,325.070554268957),
    #                                             "Total_stations"=c(NA                       ,1411334.29888                    ,2214.08                 ,62146.3313543826        ,14828.6256215819         ,5266.15217486325   ,1849.89376479857    ,3041.14137496572     ,35.8595720791757      ,73384.96                        ,2214.08))
    GHGI_Pipeline <- "GHGI"
    GHGI_transmission_compressors <- "GHGI"

    
    
    #Stationary Combustion
    #Data from the GHGI Annex in a table titled Fuel Consumption by Stationary
    #Combustion for Calculating CH4 and N2O Emissions (TBtu).  Names must be as
    #such to match those pulled from SEDS.  Res coal doesn't exist in US and res
    #gas is dealt with separately, so not included here.  The GHGI is available
    #at
    #\url{https://www.epa.gov/ghgemissions/inventory-us-greenhouse-gas-emissions-and-sinks}.
    #The desired inventory year (or closest available) should be used.  Res =
    #residential, Com = commercial, Ind = industrial, elec = electric.  Petr =
    #petroleum.
    # stationary_combustion_GHGI_data <- data.frame("State"="US_EPA",
    #                                               "com_coal"=17,
    #                                               "ind_coal"=517,
    #                                               "elec_coal"=10181,
    #                                               "res_petr"=995,
    #                                               "com_petr"=815,
    #                                               "ind_petr"=1984,
    #                                               "elec_petr"=189,
    #                                               "com_gas"=3647,
    #                                               "ind_gas"=9482,
    #                                               "elec_gas"=11658,
    #                                               "res_wood"=546,
    #                                               "com_wood"=84,
    #                                               "ind_wood"=1407,
    #                                               "elec_wood"=201) #2024 GHGI values for 2019
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
                                                  "elec_wood"=68) #2022 GHGI values for 2019
    
    #All emission factors below are in g/GJ.  Emission factors are IPCC defaults
    #except for electric natural gas.  They can be viewed in IPCC 2006 volume 2:
    #Energy tables 2.2 through 2.5
    #(https://www.ipcc-nggip.iges.or.jp/public/2006gl/vol2.html). The natural
    #gas electric sector emission factor is from Hajny et al., 2019
    #(https://doi.org/10.1021/acs.est.9b01875).  It is 5.4 g/MMBTU and within
    #its uncertainties of the GHGI value of 3.9 g/MMBTU.  Note this value is
    #only for Combined Cycle NG power plants, however they make up the vast
    #majority of NG use for electricity in the US.
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
    
    
    
    #Wetlands and Freshwater
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
    
    # Inland water CH4 fluxes are not included in either SOCCR1 or SOCCR2.  For
    # lakes, McDonald et al. (10.4319/lo.2012.57.2.0597) show that large lakes >
    # 1 km2 constitute 71% of the total lake area in the contiguous US (rising
    # to 90% if the Great Lakes are included).  So we use the median flux from
    # the largest lakes class (>1 km) from Rosentreter et al.
    # (10.1038/s41561-021-00715-2).  We also use the median river flux from
    # Rosentreter et al. Both this and the lake flux come from extended data
    # table 1.
    
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
    
    # the first 10 (E, M, PFO, PNF) are only relevant if SOCCR1 or SOCCR2 are
    # used.  The last 6 (L, R) are only relevant if freshwater emissions are
    # included.
    
    #My updates to Joe's values given some errors in Joe's
    # Wetland_EFs <- data.frame("E2_Atlantic"=c(10.3,20.43), 
    #                           "M2_Atlantic"=c(10.3,20.43),
    #                           "E2_Gulf"=    c(10.3,27.47), 
    #                           "M2_Gulf"=    c(10.3,27.47),
    #                           "E2_Pacific"= c(10.3,21.87), 
    #                           "M2_Pacific"= c(10.3,21.87),
    #                           "E2_Hudson"=  c(10.3,21.87), 
    #                           "M2_Hudson"=  c(10.3,21.87),
    #                           "PFO"=        c(36  ,24.74),
    #                           "PNF"=        c(36  ,33.28),
    #                           "L1"=5,
    #                           "L2"=5,
    #                           "R1"=7.88,
    #                           "R2"=7.88,
    #                           "R3"=7.88,
    #                           "R4"=7.88)
    #Joe's values
    Wetland_EFs <- data.frame("E2_Atlantic"=c(1.3,20.44), 
                              "M2_Atlantic"=c(1.3,20.44),
                              "E2_Gulf"=    c(1.3,20.44), 
                              "M2_Gulf"=    c(1.3,20.44),
                              "E2_Pacific"= c(1.3,20.44), 
                              "M2_Pacific"= c(1.3,20.44),
                              "E2_Hudson"=  c(1.3,20.44), 
                              "M2_Hudson"=  c(1.3,20.44),
                              "PFO"=        c(7.6,18.52),
                              "PNF"=        c(7.6,19.71),
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
    #National totals from the GHGI table titled Domestic Wastewater CH4
    #Emissions from Septic and Centralized Systems.  Septic is provided and
    #nonseptic is the sum of all other entries in the table.  The GHGI is
    #available at
    #\url{https://www.epa.gov/ghgemissions/inventory-us-greenhouse-gas-emissions-and-sinks}
    GHGI_national_wastewater_septic <- 232 #kt CH4/yr
    GHGI_national_wastewater_nonseptic <- 250 #kt CH4/yr - 1990-2019 GHGI
    # GHGI_national_wastewater_septic <- 227 #kt CH4/yr
    # GHGI_national_wastewater_nonseptic <- 246 #kt CH4/yr, 1990-2023 GHGI values
    
    #Emission factor from the GHGI table titled Variables and Data Sources for
    #CH4 Emissions from Septic Systems.  The GHGI is available at
    #\url{https://www.epa.gov/ghgemissions/inventory-us-greenhouse-gas-emissions-and-sinks}.
    #Originates from
    #https://decentralizedwater.waterrf.org/documents/DEC1R09/DEC1R09.pdf
    GHGI_septic_EF <- 10.7 #g/capita/day
    
    #National total of developed open space and developed low intensity land cover
    #from the national land cover database from Table 7 of 
    #https://doi.org/10.1016/j.isprsjprs.2020.02.019.
    Total_national_open_or_low_int_area <- 352032 #km2
    
    #Pulled from census data.  method is either scaled - i.e., the septic
    #fraction is from an old census report, or reported, i.e., the septic
    #fraction is from a recent census report and should be used as is.  Only
    #used if state_septic=TRUE.

    #Some states report the fraction of people in the state using septic systems
    #to the American Housing Survey in the Plumbing, Water, and Sewage Disposal
    #survey, available here
    #\url{https://www.census.gov/programs-surveys/ahs/data/interactive/ahstablecreator.html?s_areas=00000&s_year=2021&s_tablename=TABLE1&s_bygroup1=1&s_bygroup2=1&s_filtergroup1=1&s_filtergroup2=1}.
    #Note this data is reported every other year, not annually. For states that
    #don't, the 1990 census data on septic fraction is available here
    #\url{https://www.census.gov/data/tables/time-series/dec/coh-sewage.html}
    #and this will be scaled using the change in the national septic fraction
    #from 1990 (from census) to the desired year (from the housing survey).
    #This new fraction can be combined with up to date population data available
    #from the census
    #\url{https://www.census.gov/data/tables/time-series/demo/popest/2020s-state-total.html}
    #and the GHGI_septic_EF (g CH4 per person per day) to get emissions.
    
    
    # Wastewater_State_info <- data.frame("State"          =c("AL"    ,"AZ"    ,"AR"    ,"CA"    ,"CO"    ,"CT"    ,"DE"    ,"DC"    ,"FL"      ,"GA"    ,"ID"    ,"IL"    ,"IN"    ,"IA"    ,"KS"    ,"KY"    ,"LA"    ,"ME"    ,"MD"    ,"MA"    ,"MI"    ,"MN"    ,"MS"    ,"MO"    ,"MT"    ,"NE"    ,"NV"    ,"NH"    ,"NJ"    ,"NM"    ,"NY"      ,"NC"    ,"ND"    ,"OH"    ,"OK"    ,"OR"    ,"PA"    ,"RI"    ,"SC"    ,"SD"    ,"TN"    ,"TX"    ,"UT"    ,"VT"    ,"VA"    ,"WA"    ,"WV"    ,"WI"    ,"WY"),
    #                                     "Population"     =c(4903185 ,7278717 ,3017804 ,39512223,5758736 ,3565287 ,973764  ,705749  ,21477737  ,10617423,1787065 ,12671821,6732219 ,3155070 ,2913314 ,4467673 ,4648794 ,1344212 ,6045680 ,6892503 ,9986857 ,5639632 ,2976149 ,6137428 ,1068778 ,1934408 ,3080156 ,1359711 ,8882190 ,2096829 ,19453561  ,10488084,762062  ,11689100,3956971 ,4217737 ,12801989,1059361 ,5148714 ,884659  ,6829174 ,28995881,3205958 ,623989  ,8535519 ,7614893 ,1792147 ,5822434 ,578759),
    #                                     "Septic_Fraction"=c(0.4360  ,0.1700  ,0.3820  ,0.0980  ,0.1240  ,0.2860  ,0.2570  ,0.0020  ,0.1265    ,0.3680  ,0.3460  ,0.1330  ,0.3130  ,0.2320  ,0.1790  ,0.3980  ,0.2580  ,0.5130  ,0.1810  ,0.2670  ,0.2830  ,0.2530  ,0.3830  ,0.2420  ,0.3750  ,0.1780  ,0.1170  ,0.4900  ,0.1160  ,0.2550  ,0.1615    ,0.4850  ,0.2410  ,0.2150  ,0.2610  ,0.2930  ,0.2450  ,0.2860  ,0.4060  ,0.2680  ,0.3860  ,0.1810  ,0.1090  ,0.5500  ,0.2830  ,0.3100  ,0.4080  ,0.2830  ,0.2410),
    #                                     "Method"         =c("scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","reported","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","reported","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled"))
    # Wastewater_State_info <- data.frame("State"          =c("AL"    ,"AK"    ,"AZ"    ,"AR"    ,"CA"    ,"CO"    ,"CT"    ,"DE"    ,"DC"    ,"FL"      ,"GA"    ,"HI"    ,"ID"    ,"IL"    ,"IN"    ,"IA"    ,"KS"    ,"KY"    ,"LA"    ,"ME"    ,"MD"    ,"MA"    ,"MI"    ,"MN"    ,"MS"    ,"MO"    ,"MT"    ,"NE"    ,"NV"    ,"NH"    ,"NJ"    ,"NM"    ,"NY"      ,"NC"    ,"ND"    ,"OH"    ,"OK"    ,"OR"    ,"PA"    ,"RI"    ,"SC"    ,"SD"    ,"TN"    ,"TX"    ,"UT"    ,"VT"    ,"VA"    ,"WA"    ,"WV"    ,"WI"    ,"WY"),
    #                                     "Population"     =c(4903185 ,731545  ,7278717 ,3017804 ,39512223,5758736 ,3565287 ,973764  ,705749  ,21477737  ,10617423,1415872 ,1787065 ,12671821,6732219 ,3155070 ,2913314 ,4467673 ,4648794 ,1344212 ,6045680 ,6892503 ,9986857 ,5639632 ,2976149 ,6137428 ,1068778 ,1934408 ,3080156 ,1359711 ,8882190 ,2096829 ,19453561  ,10488084,762062  ,11689100,3956971 ,4217737 ,12801989,1059361 ,5148714 ,884659  ,6829174 ,28995881,3205958 ,623989  ,8535519 ,7614893 ,1792147 ,5822434 ,578759),
    #                                     "Septic_Fraction"=c(0.4360  ,0.2570  ,0.1700  ,0.3820  ,0.0980  ,0.1240  ,0.2860  ,0.2570  ,0.0020  ,0.1265    ,0.3680  ,0.1870  ,0.3460  ,0.1330  ,0.3130  ,0.2320  ,0.1790  ,0.3980  ,0.2580  ,0.5130  ,0.1810  ,0.2670  ,0.2830  ,0.2530  ,0.3830  ,0.2420  ,0.3750  ,0.1780  ,0.1170  ,0.4900  ,0.1160  ,0.2550  ,0.1615    ,0.4850  ,0.2410  ,0.2150  ,0.2610  ,0.2930  ,0.2450  ,0.2860  ,0.4060  ,0.2680  ,0.3860  ,0.1810  ,0.1090  ,0.5500  ,0.2830  ,0.3100  ,0.4080  ,0.2830  ,0.2410),
    #                                     "Method"         =c("scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","reported","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","reported","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled","scaled"))
    # Wastewater_State_info <- data.frame("State"          =c("DE"    ,"MD"    ,"NJ"    ,"NY"      ,"PA"),
    #                                     "Population"     =c(1018396 ,6164660 ,9261699 ,19677151  ,12972008),
    #                                     "Septic_Fraction"=c(0.257   ,0.181   ,0.116   ,0.159     ,0.245),
    #                                     "Method"         =c("scaled","scaled","scaled","reported","scaled"))
    # Wastewater_State_info <- data.frame("State"          =c("CT"    ,"DE"    ,"MA"    ,"NJ"    ,"NY"      ,"PA"),
    #                                     "Population"     =c(3565287 ,973764  ,6892503 ,8882190 ,19453561  ,12801989),
    #                                     "Septic_Fraction"=c(0.286   ,0.257   ,0.267   ,0.116   ,0.161     ,0.245),
    #                                     "Method"         =c("scaled","scaled","scaled","scaled","reported","scaled"))
    Wastewater_State_info <- data.frame("State"          =c("DE"),
                                        "Population"     =c(1018396),
                                        "Septic_Fraction"=c(0.257),
                                        "Method"         =c("scaled"))
    
    #Only needed if any states are using the scaled method.  National septic
    #fraction in the year of interest and the year that the scaled states reported a
    #septic fraction (typically 1990)
    National_wastewater_info <- data.frame("Year"           =c(1990 ,2019),
                                           "Septic_Fraction"=c(0.241,0.1635))
    # National_wastewater_info <- data.frame("Year"           =c(1990 ,2021),
    #                                        "Septic_Fraction"=c(0.241,0.152))
  }
  
  #assign all to the parent environment as this should only be called by
  #CH4_inventory_main.R
  for(object in ls(envir = environment())){
    assign(x=object,
           value = get(object,envir = environment()),
           envir = parent.env(environment()))
  }
}



