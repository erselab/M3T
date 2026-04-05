#'@title Create gridded stationary combustion methane emissions maps
#'
#'@description \code{Stationary_combustion} is an internal function that we
#'  strongly recommend users do not use directly, instead using
#'  \code{\link{CH4_inventory_build}} and \code{\link{M3T_config}} which call
#'  this function. \code{Stationary_combustion} writes up to 64 netcdf files of
#'  gridded methane emissions from stationary combustion sources, as well as
#'  optional visuals
#'
#'@details This function calculates and grids methane emissions from stationary
#'  combustion. It uses the Energy Information Administration's (EIA) State
#'  Energy Data System (SEDS), Environmental Protection Agency's (EPA)
#'  Greenhouse Gas Inventory (GHGI), EPA National Emissions Inventory (NEI) and
#'  the Vulcan and/or Anthropogenic Carbon Emission System (ACES) CO2
#'  inventories.  First, the ratio of SEDS national consumption by fuel-sector
#'  combination to the equivalent GHGI data is applied to SEDS state data so
#'  that they are approximately consistent with the GHGI.  The state total
#'  consumption is then converted to emissions using the Integovernmental Panel
#'  on Climate Change (IPCC) emission factors.  There are two variations at this
#'  step.
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
#'  Lastly, county total emissions are distributed using the ACES and/or Vulcan
#'  CO2 inventories.
#'
#'  This entire process is done separately for each fuel and sector using the
#'  GHGI, SEDS, and NEI data that are broken down by sector and fuel and CO2
#'  inventories that are broken down by sector.  Sectors considered are
#'  residential, commercial, industrial, and electric and the fuels considered
#'  are coal, natural gas, petroleum, and wood.  Residential coal is ignored as
#'  it does not exist in the U.S. anymore and residential gas is ignored as it
#'  is accounted for elsewhere.
#'
#'  The GHGI is intended to capture all national emissions.  The SEDS
#'  consumption data provides state-level consumption broken down by fuel (coal,
#'  natural gas, petroleum, wood, geothermal, solar, and electricity) and sector
#'  (residential, commercial, industrial, electric, transportation).  The NEI
#'  data being used provides CO emissions at the county level.  The GHGI is
#'  available starting in 1990 and is generally about 2 years behind present
#'  day.  SEDS data is available starting in 1960 and generally is about 2 years
#'  behind present day, though there are periodic updates between October and
#'  June.  NEI data is available beginning in at least 1990, is released every
#'  three years, and generally takes three years to complete (i.e., 2023 NEI is
#'  released in 2026).  All data is annual.  The SEDS data is at the state
#'  scale, NEI data is at the county scale, and GHGI data is at the national
#'  scale.
#'
#'  The GHGI is available at
#'  \url{https://www.epa.gov/ghgemissions/inventory-us-greenhouse-gas-emissions-and-sinks}
#'  The SEDS bulk files are available at
#'  \url{https://www.mrlc.gov/downloads/sciweb1/shared/mrlc/data-bundles/NLCD_2011_Land_Cover_AK_20200724.zip}
#'  The NEI is available at
#'  \url{https://www.epa.gov/air-emissions-inventories/national-emissions-inventory-nei}
#'  IPCC emission factors are published at
#'  \url{https://www.ipcc-nggip.iges.or.jp/public/2006gl/vol2.html} ACES is
#'  available at \url{https://doi.org/10.3334/ORNLDAAC/1943} and Vulcan is
#'  available at \url{https://doi.org/10.5281/zenodo.15446748}.
#'
#'  Each fuel-sector-inventory-variation combination is saved separately.
#'
#'  See references \href{https://doi.org/10.1029/2020JD032974}{Vulcan} and
#'  \href{https://doi.org/10.1002/2017JD027359}{ACES}
#'@inheritParams Municipal_solid_waste
#'@inheritParams Natural_Gas_Distribution
#'
#'@param aces_ind SpatRaster of industrial CO2 emissions from the ACES inventory
#'  as loaded in \code{\link{CH4_inventory_build}} based on \code{Use_ACES} and
#'  \code{Source_ACES}.
#'@param aces_elec SpatRaster of electric power production CO2 emissions from
#'  the ACES inventory as loaded in \code{\link{CH4_inventory_build}} based on
#'  \code{Use_ACES} and \code{Source_ACES}.
#'@param vu_ind SpatRaster of industrial CO2 emissions from the Vulcan v4.0
#'  inventory as loaded in \code{\link{CH4_inventory_build}} based on
#'  \code{Use_Vulcan} and \code{Source_Vulcan}.
#'@param vu_elec SpatRaster of electric power production CO2 emissions from the
#'  Vulcan v4.0 inventory as loaded in \code{\link{CH4_inventory_build}} based
#'  on \code{Use_Vulcan} and \code{Source_Vulcan}.
#'@param Source_EIA_SEDS_data Character.  Pulled from \code{\link{M3T_config}}.
#'@param Source_NEI_data Character.  Pulled from \code{\link{M3T_config}}.
#'@param verbose Logical indicating whether to save additional output.  This
#'  includes plots of the gridded methane emissions for each
#'  fuel-sector-inventory-variation combination as well as 2 summed plots for
#'  each inventory-variation combination - one for wood and one for all other
#'  sectors.
#'@param stationary_combustion_by_state Logical.  Pulled from
#'  \code{\link{M3T_config}}.
#'@param stationary_combustion_by_domain Logical.  Pulled from
#'  \code{\link{M3T_config}}.
#'@param stationary_combustion_emission_factors Data frame.  Pulled from
#'  \code{\link{M3T_config}}.
#'@param EIA_API_key Character.  Pulled from \code{\link{M3T_config}}.
#'@returns Nothing is returned from the function, but the main outputs are up to
#'  56 netcdf files of the methane emissions from stationary combustion.  They
#'  are titled as "stat_comb_sector_fuel_variation_inventory.nc" where sector is
#'  abbreviated as com (commercial), res (residential), elec (electric), and ind
#'  (industrial); fuel is abbreviated as wood, petr (petroleum), gas (natural
#'  gas), and coal; variation is bystate or bydomain; and inventory is ACES or
#'  Vulcan.
#'
#'  Then the 4 possible combinations for fossil fuel and wood separately (for a
#'  total of 8) are saved similarly as
#'  "Stationary_combustion_sector_type_total_inventory_variation.nc" where type
#'  is either fossil_fuel or wood.
#'@seealso [CH4_inventory_build()] Calculates methane inventory using settings
#'  provided in config.
#'
#'  [M3T_config] Generates the config function with user-editable settings used
#'  throughout processing.
#'
#'  [Inventory_based_disaggregation()] Disaggregates data from the county level
#'  to pixels using a sectoral CO2 inventory.
#'@keywords internal

Stationary_combustion <- function(input_directory,
                                  domain,
                                  domain_template,
                                  state_name_list,
                                  output_directory,
                                  inventory_year,
                                  GHGI_data_yr,
                                  State_Tigerlines,
                                  verbose,
                                  County_Tigerlines,
                                  Use_ACES,
                                  Use_Vulcan,
                                  aces_res,
                                  aces_com,
                                  aces_ind,
                                  aces_elec,
                                  vu_res,
                                  vu_com,
                                  vu_ind,
                                  vu_elec,
                                  stationary_combustion_GHGI_data,
                                  stationary_combustion_by_state,
                                  stationary_combustion_by_domain,
                                  stationary_combustion_emission_factors,
                                  Source_EIA_SEDS_data,
                                  Source_NEI_data,
                                  EIA_API_key,
                                  plot_directory,
                                  State_CB){
  
  starttime <- Sys.time()
  cat("Starting stationary combustion sector: Stationary_combustion\n")
  
  #because of how many subsectors this sector has, save it in it's own folder
  stat_comb_output_directory <- file.path(output_directory,"stationary_combustion")
  dir.create(stat_comb_output_directory,showWarnings = F)
  
  if(verbose){
    stat_comb_plot_directory <- file.path(plot_directory,"stationary_combustion")
    dir.create(stat_comb_plot_directory,showWarnings = F)
  }
  
  ################################################################################
  #Use yr determined in CH4 inventory build - closest to inventory year with
  #both GHGI and SEDS
  
  SEDS_yr <- GHGI_data_yr
  
  ################################################################################
  #download SEDS data via bulk download
  
  SEDS_filename <- file.path(input_directory,"EIA","SEDS.txt")

  if(Source_EIA_SEDS_data=="M3T"){
    #UPDATE TO ZENODO
    EIA_raw_data <- M3T::EIA_SEDS
  }else{
    if(Source_EIA_SEDS_data=="download"){
      SEDS_state_name_list <- c(paste0("USA-",state_name_list),"USA\"")
      
      temp_zip <- tempfile(fileext = ".zip")
      temp_dir <- tempdir()
      Trycatch_downloader("https://www.eia.gov/opendata/bulk/SEDS.zip",temp_zip,"save","Failed download of EIA SEDS data from https://www.eia.gov/opendata/bulk/SEDS.zip, check https://www.eia.gov/opendata/")
      utils::unzip(temp_zip,exdir=temp_dir,overwrite = T)
      file.copy(file.path(temp_dir,"SEDS.txt"),SEDS_filename)
    }else{
      invisible(file.copy(Source_EIA_SEDS_data,SEDS_filename,overwrite = T))
    }
    
    #see https://www.eia.gov/opendata/browser/seds.  Filtered to only sectors,
    #states, and years of interest here.  All in billion BTU/yr units (last
    #digit B instead of P - short tons)
    EIA_raw_json <- readLines(SEDS_filename)
    EIA_raw_json <- EIA_raw_json[grep("CLCCB|CLEIB|CLICB|NGCCB|NGEIB|NGICB|PACCB|PAEIB|PAICB|PARCB|WDRCB|WWCCB|WWEIB|WWICB",EIA_raw_json)]
    EIA_raw_json <- EIA_raw_json[grep(paste0(SEDS_state_name_list,collapse="|"),EIA_raw_json)]
    
    #load data from 1 entry and format to align with the API download format
    subset_data <- jsonlite::fromJSON(EIA_raw_json[1])
    EIA_raw_data <- data.frame("period"=as.numeric(subset_data$data[,1]),
                               "seriesId"=strsplit(subset_data$series_id,"\\.")[[1]][2],
                               "seriesDescription"=subset_data$name,
                               "stateId"=strsplit(subset_data$series_id,"\\.")[[1]][3],
                               "value"=as.numeric(subset_data$data[,2]))
    for(A in 2:length(EIA_raw_json)){
      subset_data <- jsonlite::fromJSON(EIA_raw_json[A])
      temp <- data.frame("period"=as.numeric(subset_data$data[,1]),
                         "seriesId"=strsplit(subset_data$series_id,"\\.")[[1]][2],
                         "seriesDescription"=subset_data$name,
                         "stateId"=strsplit(subset_data$series_id,"\\.")[[1]][3],
                         "value"=as.numeric(subset_data$data[,2]))
      EIA_raw_data <- rbind(EIA_raw_data,temp)
    }
  }
  EIA_raw_data <- EIA_raw_data[EIA_raw_data$period==SEDS_yr,]
  
  ################################################################################
  #prepare SEDS data
  
  #rearrange columns/rows to better mesh with EPA data
  EIA_data=(stats::reshape(EIA_raw_data[,c("seriesId","stateId","value")],idvar="stateId",timevar = "seriesId",direction="wide"))
  
  #rename to be consistent with EPA (matches SEDS webpage).  All in billion
  #BTU/yr.
  colnames(EIA_data) <- c("State","com_coal","elec_coal","ind_coal","com_gas",
                          "elec_gas","ind_gas","com_petr","elec_petr","ind_petr",
                          "res_petr","res_wood","com_wood","elec_wood","ind_wood")
  EIA_data$State <- gsub("US","US_SEDS",EIA_data$State)
  
  #make numeric rather than text, combine with EPA, and sort by state, convert
  #from billion BTU to trillion BTU to be consistent with GHGI
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
  #Prep SEDS data a little before use.  Scaling SEDS data to align with
  #national GHGI data.  Scaling = GHGI nationa/SEDS national.
  # Notes:
  # - there is no reported residential coal use in the US
  # - all post-meter residential NG emissions (leaks + combustion) are in a separate category, so are not considered here
  # - corrections need to be applied to avoid double counting emissions in multiple sectors. Assume the national factors apply here.
  
  # Construct dataframe containing repeats of the national EPA:SEDS ratio
  ratio_df <- (stat_comb_data[which(stat_comb_data$State == 'US_EPA'), !(names(stat_comb_data) == 'State')]/
                 stat_comb_data[which(stat_comb_data$State == 'US_SEDS'), !(names(stat_comb_data) == 'State')])
  ratio_df <- ratio_df[rep(1, nrow(stat_comb_data)-2),]
  
  #now multiply all the original data by this ratio
  stat_comb_data_adj <- stat_comb_data[which(!(stat_comb_data$State %in% c('US_EPA', 'US_SEDS'))),]
  stat_comb_data_adj[!(names(stat_comb_data_adj) == 'State')] <- stat_comb_data_adj[!(names(stat_comb_data_adj) == 'State')]*ratio_df
  ################################################################################
  # Calculate CH4 emissions in mol/s using the emission factors:
  # - Conversion from higher heating value to lower heating value (0.9 or 0.95)
  # - Conversion from trillion Btu to GJ (1e9/947.8170777491506)
  # - Conversion from GJ to g of CH4 (from config)
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
  
  # Stack these emissions to make it easier to merge with NEI data
  state_total_ch4 <- cbind(stat_comb_data_adj$State,
                           utils::stack(stat_comb_data_adj[grepl('_ER', names(stat_comb_data_adj))]))
  names(state_total_ch4) <- c('State', 'state_ch4_emiss', 'Sector')
  
  # Also calculate domain totals
  domain_total_ch4 <- stats::aggregate(state_total_ch4$state_ch4_emiss,
                                       list(Sector=state_total_ch4$Sector),
                                       sum)
  names(domain_total_ch4) <- c('Sector', 'domain_ch4_emiss')
  
  cat("Finished loading and preparing SEDS data at",format(Sys.time(),"%H:%M"),"\n")
  ################################################################################
  #Now load in the NEI data
  #see https://enviro.epa.gov/envirofacts/metadata/model/nei
  
  NEI_filename <- file.path(input_directory,"NEI","CO_data.csv")
  
  if(Source_NEI_data=="M3T"){
    #UPDATE TO ZENODO
    NEI_data_orig <- M3T::NEI_all_years
    
    NEI_options <- unique(NEI_data_orig$`INVENTORY YEAR`)
    NEI_year <- NEI_options[which.min(abs(NEI_options-inventory_year))]
    
    #update the user if this isn't actually the inventory_year
    if(inventory_year!=NEI_year){
      cat(paste0("NEI is every 3 years and does not have an inventory for ",inventory_year,".  Using ",NEI_year," as the nearest available data.\n"))
    }
    
    NEI_data_orig <- NEI_data_orig[NEI_data_orig$`INVENTORY YEAR`==NEI_year,]
  }else{
    if(Source_NEI_data=="download"){
      #identify the closest NEI year to the inventory_year that has data
      NEI_options <- seq(2011,2060,by=3)
      NEI_year <- NEI_options[which.min(abs(NEI_options-inventory_year))]
      
      #if unavailable, find the most recent that is
      for(NEI_year in rev(NEI_options[NEI_options<=NEI_year])){
        data_URL <- paste0("https://data.epa.gov/dmapservice/nei.county_sector_summary/inventory_year/equals/",
                           NEI_year,"/1:1/json/")
        test_url <- jsonlite::fromJSON(data_URL)
        if(length(test_url)>0){
          break
        }
      }
      
      #update the user if this isn't actually the inventory_year
      if(inventory_year!=NEI_year){
        cat(paste0("NEI is every 3 years and does not have an inventory for ",inventory_year,".  Using ",NEI_year," as the nearest available data.\n"))
      }
      
      #download NEI - filtered to CO, yr, and states
      data_URL <- paste0("https://data.epa.gov/dmapservice/nei.county_sector_summary/pollutant_code/equals/CO/inventory_year/equals/",NEI_year,"/st_abbrv/in/",paste(state_name_list,collapse=","),"/json/")
      NEI_data_orig <- Trycatch_downloader(data_URL,output_location=NULL,method="JSON",
                                           error_message=paste0("\nNational Emissions Inventory could not be downloaded using API link: ",data_URL))
      
      #download sector codes
      data_URL <- "https://data.epa.gov/dmapservice/nei.sectors/json"
      NEI_sector_codes <- Trycatch_downloader(data_URL,output_location=NULL,method="JSON",
                                              error_message=paste0("\nNational Emissions Inventory sector code data not be downloaded using API link: ",data_URL))
      
      #Rewrite the sector codes from numeric to text descriptions.  Method from
      #(https://stackoverflow.com/a/50898694)
      NEI_data_orig$sector_code[NEI_data_orig$sector_code %in% NEI_sector_codes$sector_code] <- 
        NEI_sector_codes$ei_sector[match(NEI_data_orig$sector_code,NEI_sector_codes$sector_code,nomatch=0)]
      
      #change the column names to match those of the xlsx downloaded equivalent, to
      #be consistent with older versions of the code.  Just renaming a few columns.
      colnames(NEI_data_orig) <- toupper(gsub("_"," ",
                                              gsub("st_abbrv","state",
                                                   gsub("county_name","county",
                                                        gsub("sector_code","SECTOR",
                                                             gsub("uom","unit of measure",colnames(NEI_data_orig)))))))
      
      utils::write.csv(NEI_data_orig,NEI_filename,row.names = F)
    }else{
      invisible(file.copy(Source_NEI_data,NEI_filename,overwrite = T))
    }
    
    #load and rename column names.  loading from csv replaces spaces with periods,
    #switch back (note this could cause errors in other cases as other special
    #characters may also be replaced by periods)
    NEI_data_orig <- utils::read.csv(NEI_filename,header=T)
    colnames(NEI_data_orig) <- gsub("\\."," ",colnames(NEI_data_orig))
    NEI_data_orig$`COUNTY FIPS` <- sprintf("%03d",NEI_data_orig$`COUNTY FIPS`)
  }
  ################################################################################
  #prep NEI for processing
  
  #convert all character columns to factor
  for(A in 1:ncol(NEI_data_orig)){
    if(isa(NEI_data_orig[,A],"character")){
      NEI_data_orig[,A] <- factor(NEI_data_orig[,A])
    }
  }
  
  # Some county-sector combinations are missing, presumably because they are zero.
  # We want to list these as zero, otherwise it will cause trouble later.
  # Use reshape to do this (there is probably a neater way...)
  NEI_data_wide <- stats::reshape(NEI_data_orig[c('SECTOR', 'STATE', 'STATE FIPS', 'COUNTY FIPS', 'EMISSIONS')],
                                  idvar=c('STATE', 'STATE FIPS', 'COUNTY FIPS'),
                                  timevar='SECTOR',
                                  direction='wide')
  
  NEI_data <- stats::reshape(NEI_data_wide,
                             idvar=c('STATE', 'STATE FIPS', 'COUNTY FIPS'),
                             direction='long')
  names(NEI_data) <- c('STATE', 'STATE_FIPS', 'COUNTY_FIPS', 'SECTOR', 'CO_EMISSIONS')
  NEI_data$CO_EMISSIONS[which(is.na(NEI_data$CO_EMISSIONS))] <- 0
  
  #Every sector we will be using by name
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
  
  #Need to add dummies in to avoid errors later if any required sectors are
  #completely missing from the whole domain
  if(!all(required_sectors %in% levels(NEI_data$SECTOR))){
    #one entry for every state-sector combo that was missing using a dummy county
    #and 0 emissions
    new_input <- expand.grid(unique(NEI_data$STATE_FIPS),
                             "000",
                             required_sectors[!(required_sectors %in% levels(NEI_data$SECTOR))],
                             0)
    
    #format it like the real NEI data, including state name (need to look up using FIPS)
    State_names <- sapply(new_input$Var1,FUN=function(x){NEI_data$STATE[which(NEI_data$STATE_FIPS==x)[1]]})
    new_input <- data.frame(State_names,new_input)
    colnames(new_input) <- colnames(NEI_data)
    
    #simplest method to ensure new factor levels are considered in the factor.
    #Otherwise, just add these dummy values to the NEI data.
    NEI_data$SECTOR <- as.character(NEI_data$SECTOR)
    NEI_data <- rbind(NEI_data,new_input)
    NEI_data$SECTOR <- factor(NEI_data$SECTOR)
    
    rm(State_names,new_input)
  }
  
  # Calculate the fraction of state-sector-total CO emissions represented by
  # each county.  Repeat at the domain scale.
  NEI_data$emiss_frac <- NEI_data$CO_EMISSIONS/stats::ave(NEI_data$CO_EMISSIONS,
                                                          NEI_data$SECTOR,
                                                          NEI_data$STATE_FIPS,
                                                          FUN=sum)
  NEI_data$emiss_frac_domain <- NEI_data$CO_EMISSIONS/stats::ave(NEI_data$CO_EMISSIONS,
                                                                 NEI_data$SECTOR,
                                                                 FUN=sum)
  
  # In some cases the NEI state-sector-total CO emissions are zero, but the
  # state-sector-total CH4 emissions are not.  In these cases we want to
  # distribute within the state according to ACES/Vulcan only.  Calculate the
  # number of counties and assign equal fraction to each of them
  NEI_data$state_county_count <- stats::ave(NEI_data$CO_EMISSIONS,
                                            NEI_data$SECTOR,
                                            NEI_data$STATE_FIPS,
                                            FUN=length)
  NEI_data$domain_county_count <- stats::ave(NEI_data$CO_EMISSIONS,
                                             NEI_data$SECTOR,
                                             FUN=length)
  NEI_data$emiss_frac[which(is.na(NEI_data$emiss_frac))] <- 1/NEI_data$state_county_count[which(is.na(NEI_data$emiss_frac))]
  NEI_data$emiss_frac_domain[which(is.na(NEI_data$emiss_frac_domain))] <- 1/NEI_data$domain_county_count[which(is.na(NEI_data$emiss_frac_domain))]
  
  
  # Change levels to match state_total_ch4 so that we can merge - note that some
  # of these won't be used, but name them here anyway
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
  
  # Combine with the state-sector-total CH4 emissions to estimate the
  # sector-total CH4 emissions for each county.  CO data is only being used to
  # get county/state total ratios, so units are irrelevant.  state totals are
  # still in mol/s, so county totals will be as well.
  NEI_data_merge_step1 <- merge(NEI_data,
                                state_total_ch4,
                                by.x=c("STATE","SECTOR"),by.y=c('State', 'Sector'))
  
  # Also combine with domain total emissions
  NEI_data_merge <- merge(NEI_data_merge_step1,
                          domain_total_ch4,
                          by.x="SECTOR",by.y='Sector')
  
  NEI_data_merge$county_ch4_emiss_bystate <- NEI_data_merge$state_ch4_emiss*NEI_data_merge$emiss_frac
  NEI_data_merge$county_ch4_emiss_bydomain <- NEI_data_merge$domain_ch4_emiss*NEI_data_merge$emiss_frac_domain
  
  # Create an unstacked dataframe containing bystate and domain emissions for each
  # county, with sectors across columns
  df_long <- NEI_data_merge[c('SECTOR','STATE_FIPS','COUNTY_FIPS','county_ch4_emiss_bystate','county_ch4_emiss_bydomain')]
  df_wide <- stats::reshape(df_long, idvar=c('STATE_FIPS','COUNTY_FIPS'), timevar='SECTOR', direction='wide')
  df_wide[is.na(df_wide)] <- 0
  
  rm(required_sectors)
  ################################################################################
  #Now load in the shapefiles and ACES/Vulcan, merge geometries
  
  #add leading zeroes to the fips codes - when saving to csv these were removed.
  #As.character first to ensure the actual value, not factor level is used
  #(converted to factor earlier).
  df_wide$STATE_FIPS <- sprintf("%02d",as.numeric(as.character(df_wide$STATE_FIPS)))
  df_wide$COUNTY_FIPS <- sprintf("%03d",as.numeric(as.character(df_wide$COUNTY_FIPS)))
  
  merge_with_poly <- terra::merge(x = County_Tigerlines,y = df_wide,
                                  by.y=c('STATE_FIPS', 'COUNTY_FIPS'),
                                  by.x=c('STATEFP','COUNTYFP'),all.x=T)
  
  
  #organize it by state-county ID for consistency with later analysis
  merge_with_poly <- merge_with_poly[order(paste0(merge_with_poly$STATEFP,merge_with_poly$COUNTYFP)),]
  
  #create copies that have names for the bystate or bydomain version that
  #exactly match the totals.  Easier/more consistent to code.
  all_merge_state <- merge_with_poly
  names(all_merge_state) <- gsub("county_ch4_emiss_bystate.","",names(all_merge_state))
  
  all_merge_domain <- merge_with_poly
  names(all_merge_domain) <- gsub("county_ch4_emiss_bydomain.","",names(all_merge_domain))
  
  
  #the many subsectors
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
  
  ################################################################################
  #these are assigned in the below sections from the Inventory_based_disaggregation function,
  #but R doesn't see them being created explicitly, so do so here just to make
  #usethis::check() happy for package building.
  aces_res_ch4_bystate <- aces_res_ch4_bydomain <- vu_res_ch4_bystate <- vu_res_ch4_bydomain <- 
    aces_com_ch4_bystate <- aces_com_ch4_bydomain <- vu_com_ch4_bystate <- vu_com_ch4_bydomain <- 
    aces_ind_ch4_bystate <- aces_ind_ch4_bydomain <- vu_ind_ch4_bystate <- vu_ind_ch4_bydomain <- 
    aces_elec_ch4_bystate <- aces_elec_ch4_bydomain <- vu_elec_ch4_bystate <- vu_elec_ch4_bydomain <- NULL
  ################################################################################
  #process emissions at the state scale
  
  if(stationary_combustion_by_state){
    cat("This step is memory intensive!  Disaggregating state total emissions to individual counties using Vulcan/ACES at",format(Sys.time(),"%H:%M"),"\n")
    if(Use_ACES){
      #mask to only keep those counties that are at least partly within the domain
      all_merge_LCC_state <- terra::mask(all_merge_state,domain)
      #convert state scale versions to the proper crs
      all_merge_LCC_state <- terra::project(all_merge_LCC_state,aces_res)
      #Calculate per-pixel coverage for each county separately.  First split by
      #unique state-county number, then calculate per-pixel coverage, output = list
      #of spatvectors
      if(length(unique(all_merge_LCC_state$COUNTYFP))==1){
        cover_all_aces <- list(terra::extract(aces_res,all_merge_LCC_state,weights=T,cells=T))
      }else{
        cover_all_aces <- all_merge_LCC_state %>% 
          split(f=paste0(all_merge_LCC_state$STATEFP,all_merge_LCC_state$COUNTYFP)) %>%
          lapply(function(x){terra::extract(aces_res,x,weights=T,cells=T)})
        cover_all_aces <- cover_all_aces[paste0(all_merge_LCC_state$STATEFP,all_merge_LCC_state$COUNTYFP)]
      }
      
      #run the Inventory_based_disaggregation function (separate) to go from county totals to
      #pixel values using ACES
      Inventory_based_disaggregation(aces_res,res_totals,agg_level="state",NEI_input = all_merge_LCC_state,cover_all_aces,out_envir=environment())
      Inventory_based_disaggregation(aces_com,com_totals,agg_level="state",NEI_input=all_merge_LCC_state,cover_all_aces,out_envir=environment())
      Inventory_based_disaggregation(aces_ind,ind_totals,agg_level="state",NEI_input=all_merge_LCC_state,cover_all_aces,out_envir=environment())
      Inventory_based_disaggregation(aces_elec,elec_totals,agg_level="state",NEI_input=all_merge_LCC_state,cover_all_aces,out_envir=environment())
      
      #clear up memory from Inventory_based_disaggregation as it can be quite significant for a
      #large domain
      invisible(gc())
    }
    if(Use_Vulcan){
      all_merge_LCC_state <- terra::mask(all_merge_state,domain)
      all_merge_LCC_state <- terra::project(all_merge_LCC_state,vu_res)
      if(length(unique(all_merge_LCC_state$COUNTYFP))==1){
        cover_all_vulcan <- list(terra::extract(vu_res,all_merge_LCC_state,weights=T,cells=T))
      }else{
        cover_all_vulcan <- all_merge_LCC_state %>% 
          split(f=paste0(all_merge_LCC_state$STATEFP,all_merge_LCC_state$COUNTYFP)) %>%
          lapply(function(x){terra::extract(vu_res,x,weights=T,cells=T)})
        cover_all_vulcan <- cover_all_vulcan[paste0(all_merge_LCC_state$STATEFP,all_merge_LCC_state$COUNTYFP)]
      }
      Inventory_based_disaggregation(vu_res,res_totals,agg_level="state",NEI_input=all_merge_LCC_state,cover_all_vulcan,out_envir=environment())
      Inventory_based_disaggregation(vu_com,com_totals,agg_level="state",NEI_input=all_merge_LCC_state,cover_all_vulcan,out_envir=environment())
      Inventory_based_disaggregation(vu_ind,ind_totals,agg_level="state",NEI_input=all_merge_LCC_state,cover_all_vulcan,out_envir=environment())
      Inventory_based_disaggregation(vu_elec,elec_totals,agg_level="state",NEI_input=all_merge_LCC_state,cover_all_vulcan,out_envir=environment())
      invisible(gc())
    }
    rm(all_merge_LCC_state)
    
    cat("\rFinished disaggregating state-scale emissions using Vulcan/ACES at",format(Sys.time(),"%H:%M"),"                                 \n")
  }
  ################################################################################
  #now at the domain scale
  
  if(stationary_combustion_by_domain){
    cat("Disaggregating domain total emissions to individual counties using Vulcan/ACES at",format(Sys.time(),"%H:%M"),"\n")
    if(Use_ACES){
      #mask to only keep those counties that are at least partly within the domain
      all_merge_LCC_domain <- terra::mask(all_merge_domain,domain)
      
      all_merge_LCC_domain <- terra::project(all_merge_LCC_domain,aces_res)
      
      #don't recalculate this, only calculate it if we have to
      if(!stationary_combustion_by_state){
        if(length(unique(all_merge_LCC_domain$COUNTYFP))==1){
          cover_all_aces <- list(terra::extract(aces_res,all_merge_LCC_domain,weights=T,cells=T))
        }else{
          cover_all_aces <- all_merge_LCC_domain %>% 
            split(f=paste0(all_merge_LCC_domain$STATEFP,all_merge_LCC_domain$COUNTYFP)) %>%
            lapply(function(x){terra::extract(aces_res,x,weights=T,cells=T)})
          cover_all_aces <- cover_all_aces[paste0(all_merge_LCC_domain$STATEFP,all_merge_LCC_domain$COUNTYFP)]
        }
      }
      Inventory_based_disaggregation(aces_res,res_totals,agg_level="domain",NEI_input=all_merge_LCC_domain,cover_all_aces,out_envir=environment())
      Inventory_based_disaggregation(aces_com,com_totals,agg_level="domain",NEI_input=all_merge_LCC_domain,cover_all_aces,out_envir=environment())
      Inventory_based_disaggregation(aces_ind,ind_totals,agg_level="domain",NEI_input=all_merge_LCC_domain,cover_all_aces,out_envir=environment())
      Inventory_based_disaggregation(aces_elec,elec_totals,agg_level="domain",NEI_input=all_merge_LCC_domain,cover_all_aces,out_envir=environment())
      invisible(gc())
    }
    if(Use_Vulcan){
      all_merge_LCC_domain <- terra::mask(all_merge_domain,domain)
      all_merge_LCC_domain <- terra::project(all_merge_LCC_domain,vu_res)
      if(!stationary_combustion_by_state){
        if(length(unique(all_merge_LCC_domain$COUNTYFP))==1){
          cover_all_vulcan <- list(terra::extract(vu_res,all_merge_LCC_domain,weights=T,cells=T))
        }else{
          cover_all_vulcan <- all_merge_LCC_domain %>% 
            split(f=paste0(all_merge_LCC_domain$STATEFP,all_merge_LCC_domain$COUNTYFP)) %>%
            lapply(function(x){terra::extract(vu_res,x,weights=T,cells=T)})
          cover_all_vulcan <- cover_all_vulcan[paste0(all_merge_LCC_domain$STATEFP,all_merge_LCC_domain$COUNTYFP)]
        }
      }
      Inventory_based_disaggregation(vu_res,res_totals,agg_level="domain",NEI_input=all_merge_LCC_domain,cover_all_vulcan,out_envir=environment())
      Inventory_based_disaggregation(vu_com,com_totals,agg_level="domain",NEI_input=all_merge_LCC_domain,cover_all_vulcan,out_envir=environment())
      Inventory_based_disaggregation(vu_ind,ind_totals,agg_level="domain",NEI_input=all_merge_LCC_domain,cover_all_vulcan,out_envir=environment())
      Inventory_based_disaggregation(vu_elec,elec_totals,agg_level="domain",NEI_input=all_merge_LCC_domain,cover_all_vulcan,out_envir=environment())
      invisible(gc())
    }
    rm(all_merge_LCC_domain)
    
    cat("\rFinished disaggregating domain-scale emissions using Vulcan/ACES at",format(Sys.time(),"%H:%M"),"                                \n")
  }
  ################################################################################
  #write a function to save the output in an organized fashion
  
  #project with terra
  save_data <- function(input){
    input_name <- gsub("\\[\\[total\\]\\]","",deparse(substitute(input)))
    disaggregation_level <- strsplit(input_name,"by")[[1]][2]
    # disaggregation_level <- tail(strsplit(input_name,"_")[[1]],1)
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
    #put domain in ACES/Vulcan crs, then crop/mask input to it, add a few pixels
    #worth of buffer (at the domain resolution) filled with 0's so the average
    #doesn't consider these NA values to ignore in calculations (drastically
    #impacts avg).  Then finally reproject via average.  
    
    #Note this is not double counting the fractional coverage of pixels.
    #disaggregation accounts for pixels partially within a given county
    #(building emissions partially from all counties a pixel is within).  Here
    #we account for pixels partially within the domain (excluding the fraction
    #of pixels outside the domain).
    input=terra::crop(input,domain_reproj,snap="out")
    input=terra::mask(input,domain_reproj,touches=T,updatevalue=0)
    cover <- terra::extract(input,domain_reproj,weights=T,cells=T)
    input[cover[,'cell']] <- input[cover[,'cell']]*cover[,'weight']
    input=terra::extend(input,fill=0,
                        terra::ext(input)+(terra::res(terra::project(domain_template,terra::crs(input)))*5))
    input=terra::project(input,domain_template,method="average")
    
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
    
    writeCDF_no_newline(input,
                        file.path(stat_comb_output_directory,paste0("stat_comb_",sub("_ER","",total),
                                                                    "_by",disaggregation_level,"_",inventory_name,'.nc')),
                        force_v4=TRUE,
                        varname='methane_emissions',
                        unit='nmol/m2/s',
                        longname=paste0('Methane emissions from ',sector_name,' sector stationary combustion of ',
                                        fuel_name,', spatially allocated from ',disaggregation_name,
                                        ' totals using NEI CO emissions and ',inventory_name,' ',sector_name,' CO2 emissions'),
                        missval=-9999,
                        overwrite=TRUE)
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
  #Create a sector total, 1 per variant
  
  if(Use_ACES){
    if(stationary_combustion_by_state){
      #use regex to load in all but wood fuels for ACES, bystate, all sectors
      Summed_stationary_combustion_FF_ACES_bystate <- terra::rast(list.files(stat_comb_output_directory,
                                                                             pattern="stat_comb_[[:alnum:]]+_[coal|gas|petr]+_bystate_aces",
                                                                             full.names = T))
      Summed_stationary_combustion_FF_ACES_bystate <- sum(Summed_stationary_combustion_FF_ACES_bystate,na.rm=T)
      
      Summed_stationary_combustion_wood_ACES_bystate <- terra::rast(list.files(stat_comb_output_directory,
                                                                               pattern="stat_comb_[[:alnum:]]+_wood_bystate_aces",
                                                                               full.names = T))
      Summed_stationary_combustion_wood_ACES_bystate <- sum(Summed_stationary_combustion_wood_ACES_bystate,na.rm=T)
      
      writeCDF_no_newline(Summed_stationary_combustion_FF_ACES_bystate,
                          file.path(output_directory,"Stationary_combustion_sector_fossil_fuel_total_ACES_bystate.nc"),
                          force_v4=TRUE,
                          varname='methane_emissions',
                          unit='nmol/m2/s',
                          longname=paste0('Methane emissions from coal, natural gas, and petroleum stationary combustion, spatially allocated from state totals using NEI CO emissions and ACES sectoral CO2 emissions'),
                          missval=-9999,
                          overwrite=TRUE)
      writeCDF_no_newline(Summed_stationary_combustion_wood_ACES_bystate,
                          file.path(output_directory,"Stationary_combustion_sector_wood_total_ACES_bystate.nc"),
                          force_v4=TRUE,
                          varname='methane_emissions',
                          unit='nmol/m2/s',
                          longname=paste0('Methane emissions from wood stationary combustion, spatially allocated from state totals using NEI CO emissions and ACES sectoral CO2 emissions'),
                          missval=-9999,
                          overwrite=TRUE)
    }
    if(stationary_combustion_by_domain){
      Summed_stationary_combustion_FF_ACES_bydomain <- terra::rast(list.files(stat_comb_output_directory,
                                                                              pattern="stat_comb_[[:alnum:]]+_[coal|gas|petr]+_bydomain_aces",
                                                                              full.names = T))
      Summed_stationary_combustion_FF_ACES_bydomain <- sum(Summed_stationary_combustion_FF_ACES_bydomain,na.rm=T)
      
      Summed_stationary_combustion_wood_ACES_bydomain <- terra::rast(list.files(stat_comb_output_directory,
                                                                                pattern="stat_comb_[[:alnum:]]+_wood_bydomain_aces",
                                                                                full.names = T))
      Summed_stationary_combustion_wood_ACES_bydomain <- sum(Summed_stationary_combustion_wood_ACES_bydomain,na.rm=T)
      
      writeCDF_no_newline(Summed_stationary_combustion_FF_ACES_bydomain,
                          file.path(output_directory,"Stationary_combustion_sector_fossil_fuel_total_ACES_bydomain.nc"),
                          force_v4=TRUE,
                          varname='methane_emissions',
                          unit='nmol/m2/s',
                          longname=paste0('Methane emissions from coal, natural gas, and petroleum stationary combustion, spatially allocated from domain totals using NEI CO emissions and ACES sectoral CO2 emissions'),
                          missval=-9999,
                          overwrite=TRUE)
      writeCDF_no_newline(Summed_stationary_combustion_wood_ACES_bydomain,
                          file.path(output_directory,"Stationary_combustion_sector_wood_total_ACES_bydomain.nc"),
                          force_v4=TRUE,
                          varname='methane_emissions',
                          unit='nmol/m2/s',
                          longname=paste0('Methane emissions from wood stationary combustion, spatially allocated from domain totals using NEI CO emissions and ACES sectoral CO2 emissions'),
                          missval=-9999,
                          overwrite=TRUE)
    }
  }
  if(Use_Vulcan){
    if(stationary_combustion_by_state){
      Summed_stationary_combustion_FF_Vulcan_bystate <- terra::rast(list.files(stat_comb_output_directory,
                                                                               pattern="stat_comb_[[:alnum:]]+_[coal|gas|petr]+_bystate_vulcan",
                                                                               full.names = T))
      Summed_stationary_combustion_FF_Vulcan_bystate <- sum(Summed_stationary_combustion_FF_Vulcan_bystate,na.rm=T)
      
      Summed_stationary_combustion_wood_Vulcan_bystate <- terra::rast(list.files(stat_comb_output_directory,
                                                                                 pattern="stat_comb_[[:alnum:]]+_wood_bystate_vulcan",
                                                                                 full.names = T))
      Summed_stationary_combustion_wood_Vulcan_bystate <- sum(Summed_stationary_combustion_wood_Vulcan_bystate,na.rm=T)
      
      writeCDF_no_newline(Summed_stationary_combustion_FF_Vulcan_bystate,
                          file.path(output_directory,"Stationary_combustion_sector_fossil_fuel_total_Vulcan_bystate.nc"),
                          force_v4=TRUE,
                          varname='methane_emissions',
                          unit='nmol/m2/s',
                          longname=paste0('Methane emissions from coal, natural gas, and petroleum stationary combustion, spatially allocated from state totals using NEI CO emissions and vulcan sectoral CO2 emissions'),
                          missval=-9999,
                          overwrite=TRUE)
      writeCDF_no_newline(Summed_stationary_combustion_wood_Vulcan_bystate,
                          file.path(output_directory,"Stationary_combustion_sector_wood_total_Vulcan_bystate.nc"),
                          force_v4=TRUE,
                          varname='methane_emissions',
                          unit='nmol/m2/s',
                          longname=paste0('Methane emissions from wood stationary combustion, spatially allocated from state totals using NEI CO emissions and vulcan sectoral CO2 emissions'),
                          missval=-9999,
                          overwrite=TRUE)
    }
    if(stationary_combustion_by_domain){
      Summed_stationary_combustion_FF_Vulcan_bydomain <- terra::rast(list.files(stat_comb_output_directory,
                                                                                pattern="stat_comb_[[:alnum:]]+_[coal|gas|petr]+_bydomain_vulcan",
                                                                                full.names = T))
      Summed_stationary_combustion_FF_Vulcan_bydomain <- sum(Summed_stationary_combustion_FF_Vulcan_bydomain,na.rm=T)
      
      Summed_stationary_combustion_wood_Vulcan_bydomain <- terra::rast(list.files(stat_comb_output_directory,
                                                                                  pattern="stat_comb_[[:alnum:]]+_wood_bydomain_vulcan",
                                                                                  full.names = T))
      Summed_stationary_combustion_wood_Vulcan_bydomain <- sum(Summed_stationary_combustion_wood_Vulcan_bydomain,na.rm=T)
      
      writeCDF_no_newline(Summed_stationary_combustion_FF_Vulcan_bydomain,
                          file.path(output_directory,"Stationary_combustion_sector_fossil_fuel_total_Vulcan_bydomain.nc"),
                          force_v4=TRUE,
                          varname='methane_emissions',
                          unit='nmol/m2/s',
                          longname=paste0('Methane emissions from coal, natural gas, and petroleum stationary combustion, spatially allocated from domain totals using NEI CO emissions and vulcan sectoral CO2 emissions'),
                          missval=-9999,
                          overwrite=TRUE)
      writeCDF_no_newline(Summed_stationary_combustion_wood_Vulcan_bydomain,
                          file.path(output_directory,"Stationary_combustion_sector_wood_total_Vulcan_bydomain.nc"),
                          force_v4=TRUE,
                          varname='methane_emissions',
                          unit='nmol/m2/s',
                          longname=paste0('Methane emissions from wood stationary combustion, spatially allocated from domain totals using NEI CO emissions and vulcan sectoral CO2 emissions'),
                          missval=-9999,
                          overwrite=TRUE)
    }
  }
  
  ################################################################################
  # plot up the data
  
  if(verbose){
    
    # all_objects <- unlist(c(res_data_objects,com_data_objects,ind_data_objects,elec_data_objects))
    # 
    # data_combinations <- expand.grid(c("com","elec","ind","res"),c("coal","gas","petr","wood"))[-c(4,8),]
    # coal,petr,gas,wood
    
    
    #To simplify the naming/processing needed, lets just write a wrapper
    #function. input_data=list of all data for that sector, total=coded
    #shorthand for sector-fuel combo
    wrapper_plot_plus <- function(input_data,total,logscale){
      combined_data <- terra::rast(input_data)
      combined_range=terra::global(combined_data,range,na.rm=T)
      zmin <- floor(min(combined_range[,1])*100)/100
      zmax <- ceiling(max(combined_range[,2])*100)/100
      
      input_data <- strsplit(basename(input_data),"_")
      
      #grab some text for the plot title
      disaggregation_level <- gsub("by","",sapply(input_data,"[[",length(input_data[[1]])-1))
      inventory_name <- gsub(".nc","",sapply(input_data,"[[",length(input_data[[1]])))
      sector_short <- sapply(input_data,"[[",3)
      sector_long <- gsub("elec","Electric",
                          gsub("res","Residential",
                               gsub("ind","Industrial",
                                    gsub("com","Commercial",sector_short))))
      
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
      inventory_name["vulcan"==inventory_name] <- "Vulcan"
      
      fuel_sub_name <- strsplit(total,"_")[[1]][2]
      if(logscale){
        if(zmin == 0){
          zmin=-4
        }else{
          zmin=log10(zmin)
        }
        
        for(A in 1:terra::nlyr(combined_data)){
          log_plot(combined_data[[A]],
                   filename=paste0("stat_comb_",sector_short,"_",fuel_sub_name,"_by",
                                   disaggregation_level,"_",tolower(inventory_name))[A],
                   paste0("Stationary Combustion ",sector_long,
                          " - ",fuel_name,"\n ",disaggregation_level,
                          " totals distributed using NEI CO emissions\n and ",
                          inventory_name," ",tolower(sector_long)," CO2 emissions")[A],
                   zlim_min = zmin,zlim_max = log10(zmax),plot_directory=stat_comb_plot_directory,
                   domain=domain,County_Tigerlines=County_Tigerlines,
                   State_CB=State_CB)
        }
      }else{
        for(A in 1:terra::nlyr(combined_data)){
          not_log_plot(combined_data[[A]],
                       filename=paste0("stat_comb_",sector_short,"_",fuel_sub_name,"_by",
                                       disaggregation_level,"_",tolower(inventory_name))[A],
                       paste0("Stationary Combustion ",sector_long,
                              " - ",fuel_name,"\n ",disaggregation_level,
                              " totals distributed using NEI CO emissions\n and ",
                              inventory_name," ",tolower(sector_long)," CO2 emissions")[A],
                       zlim_min = zmin,zlim_max = zmax,plot_directory=stat_comb_plot_directory,
                       domain=domain,County_Tigerlines=County_Tigerlines,
                       State_CB=State_CB)
        }
      }
    }
    
    
    
    for(total in gsub("_ER","",res_totals)){
      wrapper_plot_plus(list.files(stat_comb_output_directory,pattern=paste0("stat_comb_",total,".*"),full.names = T),
                        total,logscale=F)
    }
    
    for(total in gsub("_ER","",com_totals)){
      wrapper_plot_plus(list.files(stat_comb_output_directory,pattern=paste0("stat_comb_",total,".*"),full.names = T),
                        total,logscale=F)
    }
    
    for(total in gsub("_ER","",ind_totals)){
      wrapper_plot_plus(list.files(stat_comb_output_directory,pattern=paste0("stat_comb_",total,".*"),full.names = T),
                        total,logscale=F)
    }
    
    for(total in gsub("_ER","",elec_totals)){
      wrapper_plot_plus(list.files(stat_comb_output_directory,pattern=paste0("stat_comb_",total,".*"),full.names = T),
                        total,logscale=T)
    }
    
    
    
    
    #Now repeat for sector-summed plots, separating fossil and bio sources
    
    #manually set the min to show small signals too.
    stat_comb_min <- -4
    stat_comb_FF_max <- 0
    stat_comb_wood_max <- 0
    if(Use_ACES){
      if(stationary_combustion_by_state){
        if(!all(is.na(terra::values(Summed_stationary_combustion_FF_ACES_bystate)))){
          stat_comb_FF_max <- max(stat_comb_FF_max,as.numeric(log10(terra::global(Summed_stationary_combustion_FF_ACES_bystate,max,na.rm=T))))
        }
        if(!all(is.na(terra::values(Summed_stationary_combustion_wood_ACES_bystate)))){
          stat_comb_wood_max <- max(stat_comb_wood_max,as.numeric(log10(terra::global(Summed_stationary_combustion_wood_ACES_bystate,max,na.rm=T))))
        }
      }
      if(stationary_combustion_by_domain){
        if(!all(is.na(terra::values(Summed_stationary_combustion_FF_ACES_bydomain)))){
          stat_comb_FF_max <- max(stat_comb_FF_max,as.numeric(log10(terra::global(Summed_stationary_combustion_FF_ACES_bydomain,max,na.rm=T))))
        }
        if(!all(is.na(terra::values(Summed_stationary_combustion_wood_ACES_bydomain)))){
          stat_comb_wood_max <- max(stat_comb_wood_max,as.numeric(log10(terra::global(Summed_stationary_combustion_wood_ACES_bydomain,max,na.rm=T))))
        }
      }
    }
    if(Use_Vulcan){
      if(stationary_combustion_by_state){
        if(!all(is.na(terra::values(Summed_stationary_combustion_FF_Vulcan_bystate)))){
          stat_comb_FF_max <- max(stat_comb_FF_max,as.numeric(log10(terra::global(Summed_stationary_combustion_FF_Vulcan_bystate,max,na.rm=T))))
        }
        if(!all(is.na(terra::values(Summed_stationary_combustion_wood_Vulcan_bystate)))){
          stat_comb_wood_max <- max(stat_comb_wood_max,as.numeric(log10(terra::global(Summed_stationary_combustion_wood_Vulcan_bystate,max,na.rm=T))))
        }
      }
      if(stationary_combustion_by_domain){
        if(!all(is.na(terra::values(Summed_stationary_combustion_FF_Vulcan_bydomain)))){
          stat_comb_FF_max <- max(stat_comb_FF_max,as.numeric(log10(terra::global(Summed_stationary_combustion_FF_Vulcan_bydomain,max,na.rm=T))))
        }
        if(!all(is.na(terra::values(Summed_stationary_combustion_wood_Vulcan_bydomain)))){
          stat_comb_wood_max <- max(stat_comb_wood_max,as.numeric(log10(terra::global(Summed_stationary_combustion_wood_Vulcan_bydomain,max,na.rm=T))))
        }
      }
    }
    
    
    #now actually plot
    if(Use_ACES){
      if(stationary_combustion_by_state){
        log_plot(Summed_stationary_combustion_FF_ACES_bystate,
                 "Stationary Combustion FF Sector\nSEDS state data scaled to match GHGI national data distributed\nto the county level via NEI, then distributed using ACES\nsectoral CO2 emissions",
                 zlim_min=stat_comb_min,zlim_max=stat_comb_FF_max,
                 plot_directory=plot_directory,
                 domain=domain,County_Tigerlines=County_Tigerlines,
                 State_CB=State_CB)
        log_plot(Summed_stationary_combustion_wood_ACES_bystate,
                 "Stationary Combustion Wood Sector\nSEDS state data scaled to match GHGI national data distributed\nto the county level via NEI, then distributed using ACES\nsectoral CO2 emissions",
                 zlim_min=stat_comb_min,zlim_max=stat_comb_wood_max,
                 plot_directory=plot_directory,
                 domain=domain,County_Tigerlines=County_Tigerlines,
                 State_CB=State_CB)
      }
      if(stationary_combustion_by_domain){
        log_plot(Summed_stationary_combustion_FF_ACES_bydomain,
                 "Stationary Combustion FF Sector\nSEDS domain-summed data scaled to match GHGI national data distributed\nto the county level via NEI, then distributed using ACES\nsectoral CO2 emissions",
                 zlim_min=stat_comb_min,zlim_max=stat_comb_FF_max,
                 plot_directory=plot_directory,
                 domain=domain,County_Tigerlines=County_Tigerlines,
                 State_CB=State_CB)
        log_plot(Summed_stationary_combustion_wood_ACES_bydomain,
                 "Stationary Combustion Wood Sector\nSEDS domain-summed data scaled to match GHGI national data distributed\nto the county level via NEI, then distributed using ACES\nsectoral CO2 emissions",
                 zlim_min=stat_comb_min,zlim_max=stat_comb_wood_max,
                 plot_directory=plot_directory,
                 domain=domain,County_Tigerlines=County_Tigerlines,
                 State_CB=State_CB)
      }
    }
    if(Use_Vulcan){
      if(stationary_combustion_by_state){
        log_plot(Summed_stationary_combustion_FF_Vulcan_bystate,
                 "Stationary Combustion FF Sector\nSEDS state data scaled to match GHGI national data distributed\nto the county level via NEI, then distributed using Vulcan\nsectoral CO2 emissions",
                 zlim_min=stat_comb_min,zlim_max=stat_comb_FF_max,
                 plot_directory=plot_directory,
                 domain=domain,County_Tigerlines=County_Tigerlines,
                 State_CB=State_CB)
        log_plot(Summed_stationary_combustion_wood_Vulcan_bystate,
                 "Stationary Combustion Wood Sector\nSEDS state data scaled to match GHGI national data distributed\nto the county level via NEI, then distributed using Vulcan\nsectoral CO2 emissions",
                 zlim_min=stat_comb_min,zlim_max=stat_comb_wood_max,
                 plot_directory=plot_directory,
                 domain=domain,County_Tigerlines=County_Tigerlines,
                 State_CB=State_CB)
      }
      if(stationary_combustion_by_domain){
        log_plot(Summed_stationary_combustion_FF_Vulcan_bydomain,
                 "Stationary Combustion FF Sector\nSEDS domain-summed data scaled to match GHGI national data distributed\nto the county level via NEI, then distributed using Vulcan\nsectoral CO2 emissions",
                 zlim_min=stat_comb_min,zlim_max=stat_comb_FF_max,
                 plot_directory=plot_directory,
                 domain=domain,County_Tigerlines=County_Tigerlines,
                 State_CB=State_CB)
        log_plot(Summed_stationary_combustion_wood_Vulcan_bydomain,
                 "Stationary Combustion Wood Sector\nSEDS domain-summed data scaled to match GHGI national data distributed\nto the county level via NEI, then distributed using Vulcan\nsectoral CO2 emissions",
                 zlim_min=stat_comb_min,zlim_max=stat_comb_wood_max,
                 plot_directory=plot_directory,
                 domain=domain,County_Tigerlines=County_Tigerlines,
                 State_CB=State_CB)
      }
    }
  }
  cat("Finished stationary combustion sector: Stationary_combustion at",format(Sys.time(),"%H:%M"),"with a total runtime of",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes\n\n")
}
