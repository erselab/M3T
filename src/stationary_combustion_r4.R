## stationary_combustion_r3.R
## In use: 2022-03-02 16:00
#
# Spatially allocate fuel- and sector-specific stationary combustion emissions
# The totals are calculated at the state level from EIA SEDS data (and the EPA national inventory - see note below).
# These are then spatially disaggregated to the county level according to the corresponding CO emissions from the 2017 NEI
# Within each county, emissions are spatially disaggregated according to ACES or Vulcan CO2 emissions

Stationary_combustion <- function(){
  
  ################################################################################
  #User input
  
  #compare county map to old approach.  Somehow ended up with 341061 vs 338782...
  #something different between terra::extract and sf::cellfrompolygon.  terra has
  #more pixels with values.  Need to understand - use a single county as a test
  #case to look into this.  Note - not every county differs, but some do by quite a lot.
  
  
  NEI_file <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw_data_rewrite/NEI_2017.xlsx"
  
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
      input <- project(input,domain)
      #project to a grid with the exact right resolution, extent and origin.
      writeCDF(input,
               paste0(output_directory,'/',inventory_name,'_',disaggregation_level,'_stat_comb_',total,'_regridded.nc'),
               force_v4=TRUE,
               varname='methane_emissions',
               unit='mol/km2/s',
               longname=paste0(inventory_name,'_',disaggregation_level,'_stat_comb_',total),
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
  
}
