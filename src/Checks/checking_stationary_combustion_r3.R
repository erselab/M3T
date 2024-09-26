## stationary_combustion_r3.R
## In use: 2022-03-02 16:00
#
# Spatially allocate fuel- and sector-specific stationary combustion emissions
# The totals are calculated at the state level from EIA SEDS data (and the EPA national inventory - see note below).
# These are then spatially disaggregated to the county level according to the corresponding CO emissions from the 2017 NEI
# Within each county, emissions are spatially disaggregated according to ACES or Vulcan CO2 emissions


#fairly time consuming as it goes 1 county at a time across the domain
################################################################################
#User input
Stationary_combustion_file <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw data/stationary_combustion_tBtu_2019.xlsx"
state_shapefile <- "G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Shapefiles/tl_2022_us_state/tl_2022_us_state.shp"
state_name_list <- sort(c("NJ","NY","PA","MD","DE"))
input_directory="G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw_data_rewrite/"
output_directory="G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_rewrite/"

plot_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Figures_rewrite/stat_comb_intercomparison"

inventory_year=2019
domain=as.data.frame(cbind(c(-76.65,-73.65),
                           c(38.97,40.97)))
domain_res=0.01
domain_crs="epsg:4326" #lat/long

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
ACES_year <- 2017

#year of Vulcan data.  Assuming Vulcan v3.0, 1 - 6 corresponding to years 2010 -
#2015
vulcan_band <- 6

Census_filenames <- c(paste0(input_directory,"State_Tigerlines/tl_",inventory_year,"_us_state.shp"),
                      paste0(input_directory,"County_Tigerlines/tl_",inventory_year,"_us_county.shp"))

NEI_file=file.path(input_directory,"NEI_2017.xlsx")
ACES_directory="G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Inventories/ACES_v2.0"
vulcan_directory="G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Inventories/Vulcan_v3.0"

################################################################################
#load packages
i <- 1
packagecheck <- c("raster","ncdf4","readxl","terra","jsonlite","dplyr","sp","sf")
while(i<=length(packagecheck)){
  if(length(find.package(packagecheck[i],quiet = TRUE))<1){
    install.packages(packagecheck[i],repos="https://repo.miserver.it.umich.edu/cran/")
  }
  i <- i+1
}

suppressPackageStartupMessages(invisible(lapply(packagecheck, library, character.only=TRUE)))
rm(packagecheck,i)

#raster + ncdf4 = raster and .nc filetype functionalities
#readxl = ability to load more excel filetypes flexibly
################################################################################
#create the domain and set it to all NaN
if(length(domain_res)==1){
  domain_res <- rep(domain_res,2)
}

if(class(domain)=="SpatRaster"){
  values(domain) <- NaN
}else if(class(domain)=="data.frame"){
  domain <- rast(nrows=diff(range(domain[,2]))/domain_res[2], 
                 ncols=diff(range(domain[,1]))/domain_res[1],
                 xmin=min(domain[,1]), xmax=max(domain[,1]),
                 ymin=min(domain[,2]), ymax=max(domain[,2]), 
                 crs=domain_crs)
  rm(domain_res,domain_crs)
}
domain=raster(domain)
################################################################################
#load in shapefiles

State_Tigerlines <- vect(Census_filenames[1])
County_Tigerlines <- vect(Census_filenames[2])

#project to match the domain (crs)
State_Tigerlines <- project(State_Tigerlines,domain)
County_Tigerlines <- project(County_Tigerlines,domain)

#subset to just those relevant for the domain (speedier).  For state it's any
#state that touches the domain at all.  For county, it's only those within the
#states (i.e., not just touching the states, crop vs mask for vectors).
State_Tigerlines <- mask(State_Tigerlines,mask=as.polygons(rast(domain)))
County_Tigerlines <- crop(County_Tigerlines,State_Tigerlines)

#sort by state abbreviation
State_Tigerlines <- State_Tigerlines[order(State_Tigerlines$STUSPS),]

################################################################################
#load in SEDS data for 2019

#old
{
  # Load in csv file containing national data from the EPA national inventory report and both
  # national and state-level data from EIA SEDS.
  # Notes:
  # - there is no reported residential coal use in the US
  # - all post-meter residential NG emissions (leaks + combustion) are in a separate category, so are not considered here
  # - corrections need to be applied to avoid double counting emissions in multiple sectors. Assume the national factors apply here.
  
  stat_comb_data_old <- data.frame(read_xlsx(Stationary_combustion_file,col_names = T))
  # stat_comb_data <- data.frame(read.csv(Stationary_combustion_file))
  for(A in 1:ncol(stat_comb_data_old)){
    if(class(stat_comb_data_old[,A])=="character"){
      stat_comb_data_old[,A] <- factor(stat_comb_data_old[,A])
    }
  }
  
  # Construct df containing repeats of the national EPA:SEDS ratio, then multiply all the original data by this ratio
  ratio_df <- (stat_comb_data_old[which(stat_comb_data_old$State == 'US_EPA'), !(names(stat_comb_data_old) == 'State')]/
                 stat_comb_data_old[which(stat_comb_data_old$State == 'US_SEDS'), !(names(stat_comb_data_old) == 'State')])
  ratio_df <- ratio_df[rep(1, nrow(stat_comb_data_old)-2),]
  
  stat_comb_data_adj_old <- stat_comb_data_old[which(!(stat_comb_data_old$State %in% c('US_EPA', 'US_SEDS'))),]
  stat_comb_data_adj_old[!(names(stat_comb_data_adj_old) == 'State')] <- stat_comb_data_adj_old[!(names(stat_comb_data_adj_old) == 'State')]*ratio_df
}

#new
{
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
  
  # Construct df containing repeats of the national EPA:SEDS ratio
  ratio_df <- (stat_comb_data[which(stat_comb_data$State == 'US_EPA'), !(names(stat_comb_data) == 'State')]/
                 stat_comb_data[which(stat_comb_data$State == 'US_SEDS'), !(names(stat_comb_data) == 'State')])
  ratio_df <- ratio_df[rep(1, nrow(stat_comb_data)-2),]
  
  #now multiply all the original data by this ratio
  stat_comb_data_adj <- stat_comb_data[which(!(stat_comb_data$State %in% c('US_EPA', 'US_SEDS'))),]
  stat_comb_data_adj[!(names(stat_comb_data_adj) == 'State')] <- stat_comb_data_adj[!(names(stat_comb_data_adj) == 'State')]*ratio_df
  
}

#make sure old and new are properly aligned!
stat_comb_data_old <- stat_comb_data_old[order(stat_comb_data_old[,1]),colnames(stat_comb_data)]

delta <- stat_comb_data_old - stat_comb_data[order(stat_comb_data[,1]),]
delta[,1] <- stat_comb_data[,1]
# delta <- delta[,-1]/stat_comb_data[order(stat_comb_data[,1]),-1]*100
View(delta)

#make sure old and new are properly aligned!
stat_comb_data_adj_old <- stat_comb_data_adj_old[order(stat_comb_data_adj_old[,1]),colnames(stat_comb_data_adj)]

delta <- stat_comb_data_adj_old - stat_comb_data_adj[order(stat_comb_data_adj[,1]),]
delta[,1] <- stat_comb_data_adj[,1]
View(delta)
################################################################################
# Calculate CH4 emissions in mol/s using the emission factors:
# - Conversion from higher heating value to lower heating value (0.9 or 0.95)
# - Conversion from trillion Btu to GJ (1e9/947.8)
# - Conversion from GJ to g/yr of CH4 (IPCC default values, except natural gas power plants from Hajny et al. doi: 10.1021/acs.est.9b01875)
# - Conversion from g/yr to mol/s (1/(16.043*365*24*60*60))

#this code is identical - nothing to compare.  The only difference is old
#version had a correction factor to match the elec gas to what Joe used.  The
#only reason there's any difference is Joe used 16.04 for g/mol CH4; I'm using
#16.043.
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
state_total_ch4 <- cbind(stat_comb_data_adj$State, stack(stat_comb_data_adj[grepl('_ER', names(stat_comb_data_adj))]))
names(state_total_ch4) <- c('State', 'state_ch4_emiss', 'Sector')

# Also calculate domain totals (will be identical if only 1 state in domain)
domain_total_ch4 <- aggregate(state_total_ch4$state_ch4_emiss,
                            list(Sector=state_total_ch4$Sector),
                            sum)
names(domain_total_ch4) <- c('Sector', 'domain_ch4_emiss')
################################################################################
#Now load in the NEI data

#old
{
  # Load in NEI CO emission data
  # NEI_data_orig <- as.data.frame(read.csv(NEI_file))
  NEI_data_orig <- as.data.frame(read_xlsx(NEI_file,skip=0,col_names = T))
  # colnames(NEI_data_orig) <- c("SECTOR","STATE","STATE FIPS","COUNTY",
  #                              "COUNTY FIPS","POLLUTANT","POLLUTANT TYPE",
  #                              "EMISSIONS","UNIT OF MEASURE")
  NEI_data_orig$SECTOR <- factor(NEI_data_orig$SECTOR)
  NEI_data_orig$"STATE FIPS" <- sprintf("%02s",NEI_data_orig$"STATE FIPS")
  NEI_data_orig$"COUNTY FIPS" <- sprintf("%03s",NEI_data_orig$"COUNTY FIPS")
  #force these to have the right number of digits, making them characters while at it.
  
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
  
  
  #Need to add dummies in to avoid errors later if any required sectors are
  #completely missing from the whole domain
  if(length(required_sectors)!=sum(required_sectors %in% levels(NEI_data$SECTOR))){
    new_input <- expand.grid(unique(NEI_data$STATE_FIPS),
                             "000",
                             required_sectors[!(required_sectors %in% levels(NEI_data$SECTOR))],
                             0)
    #one entry for every state-sector combo using a dummy county and 0 emissions
    State_names <- sapply(new_input$Var1,FUN=function(x){NEI_data$STATE[which(NEI_data$STATE_FIPS==x)[1]]})
    new_input <- data.frame(State_names,new_input)
    colnames(new_input) <- colnames(NEI_data)
    
    NEI_data$SECTOR <- as.character(NEI_data$SECTOR)
    NEI_data <- rbind(NEI_data,new_input)
    NEI_data$SECTOR <- factor(NEI_data$SECTOR)
    #simplest method to ensure new factor levels are considered in the factor
    
    rm(State_names,new_input)
  }
  
  # Calculate the fraction of state-sector-total CO emissions represented by each county
  NEI_data$emiss_frac <- NEI_data$CO_EMISSIONS/ave(NEI_data$CO_EMISSIONS,
                                                   NEI_data$SECTOR,
                                                   NEI_data$STATE_FIPS,
                                                   FUN=sum)
  NEI_data$emiss_frac_domain <- NEI_data$CO_EMISSIONS/ave(NEI_data$CO_EMISSIONS,
                                                          NEI_data$SECTOR,
                                                          FUN=sum)
  
  # In some cases the NEI state-sector-total CO emissions are zero, but the state-sector-total CH4 emissions are not
  # In these cases we want to distribute within the state according to ACES/Vulcan only
  # Calculate the number of counties and assign equal fraction to each of them
  NEI_data$state_county_count <- ave(NEI_data$CO_EMISSIONS,
                                     NEI_data$SECTOR,
                                     NEI_data$STATE_FIPS,
                                     FUN=length)
  NEI_data$domain_county_count <- ave(NEI_data$CO_EMISSIONS,
                                      NEI_data$SECTOR,
                                      FUN=length)
  NEI_data$emiss_frac[which(is.na(NEI_data$emiss_frac))] <- 1/NEI_data$state_county_count[which(is.na(NEI_data$emiss_frac))]
  NEI_data$emiss_frac_domain[which(is.na(NEI_data$emiss_frac_domain))] <- 1/NEI_data$domain_county_count[which(is.na(NEI_data$emiss_frac_domain))]
  
  
  # Change levels to match state_total_ch4 so that we can merge - note that some of these won't be used, but name them here anyway
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
  
  # Combine with the state-sector-total CH4 emissions to estimate the sector-total CH4 emissions for each county
  NEI_data_merge_step1 <- merge(NEI_data,
                                state_total_ch4,
                                by.x=c("STATE","SECTOR"),by.y=c('State', 'Sector'))
  
  # Also combine with 5-state total emissions
  NEI_data_merge_old <- merge(NEI_data_merge_step1,
                          domain_total_ch4,
                          by.x="SECTOR",by.y='Sector')
  
  NEI_data_merge_old$county_ch4_emiss_bystate <- NEI_data_merge_old$state_ch4_emiss*NEI_data_merge_old$emiss_frac
  NEI_data_merge_old$county_ch4_emiss_bydomain <- NEI_data_merge_old$domain_ch4_emiss*NEI_data_merge_old$emiss_frac_domain
  
  # Create an unstacked dataframe containing bystate and domain emissions for each
  # county, with sectors across columns
  df_long_old <- NEI_data_merge_old[c('SECTOR','STATE_FIPS','COUNTY_FIPS','county_ch4_emiss_bystate','county_ch4_emiss_bydomain')]
  df_wide_old <- reshape(df_long_old, idvar=c('STATE_FIPS','COUNTY_FIPS'), timevar='SECTOR', direction='wide')
  df_wide_old[is.na(df_wide_old)] <- 0
  
}

#new
{
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

}

#absolutely identical - virtually unchanged processing, so that's unsurprising

#make sure old and new are properly aligned!
all(NEI_data_merge_old$SECTOR==NEI_data_merge$SECTOR)
all(NEI_data_merge_old$STATE==NEI_data_merge$STATE)
all(NEI_data_merge_old$COUNTY_FIPS==NEI_data_merge$COUNTY_FIPS)
all(NEI_data_merge_old$CO_EMISSIONS==NEI_data_merge$CO_EMISSIONS)

#compare the NEI - merged with SEDS state total emissions
delta <- NEI_data_merge_old[,c("emiss_frac","state_ch4_emiss","county_ch4_emiss_bystate","emiss_frac_domain","domain_ch4_emiss","county_ch4_emiss_bydomain")] - 
             NEI_data_merge[,c("emiss_frac","state_ch4_emiss","county_ch4_emiss_bystate","emiss_frac_domain","domain_ch4_emiss","county_ch4_emiss_bydomain")]
delta <- cbind(NEI_data_merge[,c("SECTOR","STATE","COUNTY_FIPS")],delta)
View(delta)

################################################################################
#Now load in the shapefiles and vulcan, merge geometries

#old
{
  # Load county shapefile and merge geometries with the emissions data
  merge_with_poly <- merge(df_wide,
                           st_as_sf(County_Tigerlines),
                           by.x=c('STATE_FIPS', 'COUNTY_FIPS'),
                           by.y=c('STATEFP','COUNTYFP'))
  all_merge_sf <- st_as_sf(merge_with_poly, sf_column_name='geometry', crs=crs(County_Tigerlines))
  
  # Load in ACES and Vulcan sectors - these are in different units and one is an
  # annual sum the other an annual average, but it doesn't matter as we'll only
  # use fractions

  aces_res <- raster(file.path(ACES_directory,"Sectoral",paste0(ACES_year,'_Annual_ACES_Residential.nc')))
  aces_com <- raster(file.path(ACES_directory,"Sectoral",paste0(ACES_year,'_Annual_ACES_Commercial.nc')))
  aces_ind <- raster(file.path(ACES_directory,"Sectoral",paste0(ACES_year,'_Annual_ACES_Industrial.nc')))
  aces_elec <- raster(file.path(ACES_directory,"Sectoral",paste0(ACES_year,'_Annual_ACES_Elec.nc')))
  vu_res <- raster(file.path(vulcan_directory,"Sectoral","Vulcan_v3_US_annual_1km_residential_mn.nc4"), varname='carbon_emissions',band=vulcan_band)
  vu_com <- raster(file.path(vulcan_directory,"Sectoral",'Vulcan_v3_US_annual_1km_commercial_mn.nc4'), varname='carbon_emissions',band=vulcan_band)
  vu_ind <- raster(file.path(vulcan_directory,"Sectoral",'Vulcan_v3_US_annual_1km_industrial_mn.nc4'), varname='carbon_emissions',band=vulcan_band)
  vu_elec <- raster(file.path(vulcan_directory,"Sectoral",'Vulcan_v3_US_annual_1km_elec_prod_mn.nc4'), varname='carbon_emissions',band=vulcan_band)

  # Change nans to zeros otherwise they could mess with the regridding later
  aces_res[is.na(aces_res)] <- 0
  aces_com[is.na(aces_com)] <- 0
  aces_ind[is.na(aces_ind)] <- 0
  aces_elec[is.na(aces_elec)] <- 0
  vu_res[is.na(vu_res)] <- 0
  vu_com[is.na(vu_com)] <- 0
  vu_ind[is.na(vu_ind)] <- 0
  vu_elec[is.na(vu_elec)] <- 0
  
  # Going to assume that ACES and Vulcan have the same CRS - check that here
  if(!compareCRS(aces_res,vu_res)){
    stop('Code assumes CO2 inventories have the same CRS')
  }
  
  # Transform to ACES/Vulcan CRS
  all_merge_sf_LCC <- st_transform(all_merge_sf, crs(vu_res))
  
  # Convert all_merge_sf_LCC to Spatial so we can use it with raster more easily
  all_merge_sp_LCC <- as(all_merge_sf_LCC, 'Spatial')
  
  # Get the fraction of each cell covered by each polygon - this is much quicker that rasterize(getCover=T)
  # although it does have strange bug (as of raster_3.4-5) that calculates weights that are exactly a factor of 100 too low
  # i.e. they give 0.01 when the whole cell is covered
  aces_cover_all <- cellFromPolygon(aces_res, all_merge_sp_LCC, weights = TRUE)
  vu_cover_all <- cellFromPolygon(vu_res, all_merge_sp_LCC, weights = TRUE)
  
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

}

#new
{
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
  
  
  #don't bother loading ACES/Vulcan in 2x.  I'm not concerned that rast/raster
  #might load the data in very differently.  Checked with aces res briefly.
    #load in ACES files, flip given how R loads it, set crs as it's not loaded
    #in properly
    # aces_res_new <- rast(paste0(ACES_directory,"/Sectoral/",ACES_year,'_Annual_ACES_Residential.nc'))
    # aces_res_new <- flip(aces_res_new)
    # crs(aces_res_new) <- "+proj=lcc +lat_0=40 +lon_0=-97 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
    # aces_res_new - rast(aces_res)
    # 
    # aces_com <- rast(paste0(ACES_directory,"/Sectoral/",ACES_year,'_Annual_ACES_Commercial.nc'))
    # aces_com <- flip(aces_com)
    # crs(aces_com) <- "+proj=lcc +lat_0=40 +lon_0=-97 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
    # 
    # aces_ind <- rast(paste0(ACES_directory,"/Sectoral/",ACES_year,'_Annual_ACES_Industrial.nc'))
    # aces_ind <- flip(aces_ind)
    # crs(aces_ind) <- "+proj=lcc +lat_0=40 +lon_0=-97 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
    # 
    # aces_elec <- rast(paste0(ACES_directory,"/Sectoral/",ACES_year,'_Annual_ACES_Elec.nc'))
    # aces_elec <- flip(aces_elec)
    # crs(aces_elec) <- "+proj=lcc +lat_0=40 +lon_0=-97 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
# 
#     
#     vu_res <- rast(paste0(vulcan_directory,"/Sectoral/","Vulcan_v3_US_annual_1km_residential_mn.nc4"), subds='carbon_emissions', lyrs=vulcan_band)
#     vu_com <- rast(paste0(vulcan_directory,"/Sectoral/",'Vulcan_v3_US_annual_1km_commercial_mn.nc4'), subds='carbon_emissions', lyrs=vulcan_band)
#     vu_ind <- rast(paste0(vulcan_directory,"/Sectoral/",'Vulcan_v3_US_annual_1km_industrial_mn.nc4'), subds='carbon_emissions', lyrs=vulcan_band)
#     vu_elec <- rast(paste0(vulcan_directory,"/Sectoral/",'Vulcan_v3_US_annual_1km_elec_prod_mn.nc4'), subds='carbon_emissions', lyrs=vulcan_band)

  #organize it by state-county ID for consistency with later analysis
  merge_with_poly <- merge_with_poly[order(paste0(merge_with_poly$STATEFP,merge_with_poly$COUNTYFP)),]
  
  # Transform to ACES/Vulcan CRS
  all_merge_state <- merge_with_poly
  names(all_merge_state) <- gsub("county_ch4_emiss_bystate.","",names(all_merge_state))
  all_merge_domain <- merge_with_poly
  names(all_merge_domain) <- gsub("county_ch4_emiss_bydomain.","",names(all_merge_domain))
  #create a copy that has names for the bystate or bydomain version that exactly
  #match the totals.  Easier/more consistent to code.

  #convert state scale versions to the proper crs
  all_merge_LCC_state <- project(all_merge_state,aces_res)
  #Calculate per-pixel coverage for each county separately.  First split by
  #unique state-county number, then calculate per-pixel coverage, output = list
  #of spatvectors
  cover_all_aces <- all_merge_LCC_state %>% 
    split(f=paste0(all_merge_LCC_state$STATEFP,all_merge_LCC_state$COUNTYFP)) %>%
    lapply(function(x){extract(rast(aces_res),x,weights=T,exact=T,cells=T)})
  
  cover_all_vulcan <- all_merge_LCC_state %>% 
    split(f=paste0(all_merge_LCC_state$STATEFP,all_merge_LCC_state$COUNTYFP)) %>%
    lapply(function(x){extract(rast(vu_res),x,weights=T,exact=T,cells=T)})
}



#new cover all calculation takes noticeably longer, but has much higher
#precision.  Raster = 2 decimal places (1 = 100%, so 1% is the maximum
#precision).  Terra = 7 decimal places, same format.  Overkill?  Not DRASTICALLY
#longer to calculate?


checker <- function(input){
  new_ACES <- cover_all_aces
  old_ACES <- aces_cover_all
  new_vu <- cover_all_vulcan
  old_vu <- vu_cover_all
  
  #the new approach has a higher precision so a few cells are considered
  #fractionally within the domain when with raster they would not be.  Remove
  #those so we can compare.
  new_ACES_comparison <- new_ACES[[input]][((new_ACES[[input]][,"cell"] %in% old_ACES[[input]])),]
  old_ACES_comparison <- old_ACES[[input]]
  old_ACES_comparison[,"weight"] <- old_ACES_comparison[,"weight"]*100
  
  new_vu_comparison <- new_vu[[input]][((new_vu[[input]][,"cell"] %in% old_vu[[input]])),]
  old_vu_comparison <- old_vu[[input]]
  old_vu_comparison[,"weight"] <- old_vu_comparison[,"weight"]*100
  
  if(!all(new_vu_comparison[,"cell"] == old_vu_comparison[,"cell"])){
    stop("unequal Vulcan")
  }
  if(!all(new_ACES_comparison[,"cell"] == old_ACES_comparison[,"cell"])){
    stop("unequal ACES")
  }
  
  delta <- old_ACES_comparison[,"weight"] - new_ACES_comparison[,"weight"]
  plot(delta,main=paste0("county ",input," ACES old - new"))
  cat("range delta = ",range(delta))
  cat("\nnew=",sum(new_ACES_comparison[,"weight"]))
  cat("\nold=",sum(old_ACES_comparison[,"weight"]))
  cat("\nold/new=",sum(old_ACES_comparison[,"weight"])/sum(new_ACES_comparison[,"weight"]))
  
  delta <- old_vu_comparison[,"weight"] - new_vu_comparison[,"weight"]
  plot(delta,main=paste0("county ",input," Vulcan old - new"))
  cat("range delta = ",range(delta))
  cat("\nnew=",sum(new_vu_comparison[,"weight"]))
  cat("\nold=",sum(old_vu_comparison[,"weight"]))
  cat("\nold/new=",sum(old_vu_comparison[,"weight"])/sum(new_vu_comparison[,"weight"]))
  
}

#all agree within the 2 decimal rounding of raster package.  Not testing all 177
#counties, just a smattering.

checker(1)
checker(8)
checker(60)
checker(130)
checker(177)



################################################################################
# Set up lists of rasters for calculating ch4 emissions, with one raster for each subsector
aces_template <- aces_res
aces_template[] <- 0

aces_res_ch4_bystate <- replicate(length(res_totals), aces_template)
names(aces_res_ch4_bystate) <- res_totals
aces_com_ch4_bystate <- replicate(length(com_totals), aces_template)
names(aces_com_ch4_bystate) <- com_totals
aces_ind_ch4_bystate <- replicate(length(ind_totals), aces_template)
names(aces_ind_ch4_bystate) <- ind_totals
aces_elec_ch4_bystate <- replicate(length(elec_totals), aces_template)
names(aces_elec_ch4_bystate) <- elec_totals

aces_res_ch4_bydomain <- replicate(length(res_totals), aces_template)
names(aces_res_ch4_bydomain) <- res_totals
aces_com_ch4_bydomain <- replicate(length(com_totals), aces_template)
names(aces_com_ch4_bydomain) <- com_totals
aces_ind_ch4_bydomain <- replicate(length(ind_totals), aces_template)
names(aces_ind_ch4_bydomain) <- ind_totals
aces_elec_ch4_bydomain <- replicate(length(elec_totals), aces_template)
names(aces_elec_ch4_bydomain) <- elec_totals

vu_template <- vu_res
vu_template[] <- 0

vu_res_ch4_bystate <- replicate(length(res_totals), vu_template)
names(vu_res_ch4_bystate) <- res_totals
vu_com_ch4_bystate <- replicate(length(com_totals), vu_template)
names(vu_com_ch4_bystate) <- com_totals
vu_ind_ch4_bystate <- replicate(length(ind_totals), vu_template)
names(vu_ind_ch4_bystate) <- ind_totals
vu_elec_ch4_bystate <- replicate(length(elec_totals), vu_template)
names(vu_elec_ch4_bystate) <- elec_totals

vu_res_ch4_bydomain <- replicate(length(res_totals), vu_template)
names(vu_res_ch4_bydomain) <- res_totals
vu_com_ch4_bydomain <- replicate(length(com_totals), vu_template)
names(vu_com_ch4_bydomain) <- com_totals
vu_ind_ch4_bydomain <- replicate(length(ind_totals), vu_template)
names(vu_ind_ch4_bydomain) <- ind_totals
vu_elec_ch4_bydomain <- replicate(length(elec_totals), vu_template)
names(vu_elec_ch4_bydomain) <- elec_totals






# Set up lists of rasters for calculating ch4 emissions, with one raster for
# each subsector
template_vu <- rast(vu_res)
template_vu[is.na(template_vu)] <- 0
template_vu[] <- 0

template_aces <- rast(aces_res)
template_aces[is.na(template_aces)] <- 0
template_aces[] <- 0

input_inventory_ch4 <- replicate(length(res_totals), template_vu)
names(input_inventory_ch4) <- res_totals
vu_res_ch4_bystate_new <- input_inventory_ch4
vu_res_ch4_bydomain_new <- input_inventory_ch4
input_inventory_ch4 <- replicate(length(com_totals), template_vu)
names(input_inventory_ch4) <- com_totals
vu_com_ch4_bystate_new <- input_inventory_ch4
vu_com_ch4_bydomain_new <- input_inventory_ch4
input_inventory_ch4 <- replicate(length(ind_totals), template_vu)
names(input_inventory_ch4) <- ind_totals
vu_ind_ch4_bystate_new <- input_inventory_ch4
vu_ind_ch4_bydomain_new <- input_inventory_ch4
input_inventory_ch4 <- replicate(length(elec_totals), template_vu)
names(input_inventory_ch4) <- elec_totals
vu_elec_ch4_bystate_new <- input_inventory_ch4
vu_elec_ch4_bydomain_new <- input_inventory_ch4

input_inventory_ch4 <- replicate(length(res_totals), template_aces)
names(input_inventory_ch4) <- res_totals
aces_res_ch4_bystate_new <- input_inventory_ch4
aces_res_ch4_bydomain_new <- input_inventory_ch4
input_inventory_ch4 <- replicate(length(com_totals), template_aces)
names(input_inventory_ch4) <- com_totals
aces_com_ch4_bystate_new <- input_inventory_ch4
aces_com_ch4_bydomain_new <- input_inventory_ch4
input_inventory_ch4 <- replicate(length(ind_totals), template_aces)
names(input_inventory_ch4) <- ind_totals
aces_ind_ch4_bystate_new <- input_inventory_ch4
aces_ind_ch4_bydomain_new <- input_inventory_ch4
input_inventory_ch4 <- replicate(length(elec_totals), template_aces)
names(input_inventory_ch4) <- elec_totals
aces_elec_ch4_bystate_new <- input_inventory_ch4
aces_elec_ch4_bydomain_new <- input_inventory_ch4

################################################################################
#now compare the approaches for disaggregating within county

County_Tigerlines_trans <- project(County_Tigerlines,crs(aces_res))

for(i in 1:length(vu_cover_all)){
  
  
  
  #old
  {
    #res
    
    #using the new cover values - want consistent precision/input here
    aces_cover <- cover_all_aces[[i]]
    vu_cover <- cover_all_vulcan[[i]]

    # #aces is already cropped, Vulcan is not.  So some counties in the domain may
    # #not have any ACES data.
    # if(!is.null(aces_cover)){
    #   # Starting with a raster of zeros, set cells within polygon i equal to their cover-weighted CO2 emission
    aces_res_temp <- aces_template
    aces_res_temp[aces_cover[,'cell']] <- aces_res[aces_cover[,'cell']]*aces_cover[,'weight']
    # Calculate the fraction of the polygon-total CO2 emission within each cell
    if(cellStats(aces_res_temp, sum)){
      aces_res_frac <- aces_res_temp/cellStats(aces_res_temp, sum)
    } else {
      aces_res_temp[aces_cover[,'cell']] <- aces_cover[,'weight']
      aces_res_frac <- aces_res_temp/cellStats(aces_res_temp, sum)
    }
    # }
    
    vu_res_temp <- vu_template
    vu_res_temp[vu_cover[,'cell']] <- vu_res[vu_cover[,'cell']]*vu_cover[,'weight']
    if(cellStats(vu_res_temp, sum)){
      vu_res_frac <- vu_res_temp/cellStats(vu_res_temp, sum)
    } else {
      vu_res_temp[vu_cover[,'cell']] <- vu_cover[,'weight']
      vu_res_frac <- vu_res_temp/cellStats(vu_res_temp, sum)
    }
    
    # Loop through the different subsectors, and add the CH4 emissions map to the relevant raster
    # Note that we need to use the sf object here, because the merge function has changed the row order relative to all_merge_clean
    for(total in res_totals){
      aces_res_ch4_bystate[[total]][] <- (aces_res_ch4_bystate[[total]][] +
                                            aces_res_frac[]*
                                            as.numeric(st_drop_geometry(all_merge_sf_LCC[i, paste('county_ch4_emiss_bystate', total, sep='.')])))
      aces_res_ch4_bydomain[[total]][] <- (aces_res_ch4_bydomain[[total]][] +
                                             aces_res_frac[]*
                                             as.numeric(st_drop_geometry(all_merge_sf_LCC[i, paste('county_ch4_emiss_bydomain', total, sep='.')])))
      vu_res_ch4_bystate[[total]][] <- (vu_res_ch4_bystate[[total]][] +
                                          vu_res_frac[]*
                                          as.numeric(st_drop_geometry(all_merge_sf_LCC[i, paste('county_ch4_emiss_bystate', total, sep='.')])))
      vu_res_ch4_bydomain[[total]][] <- (vu_res_ch4_bydomain[[total]][] +
                                           vu_res_frac[]*
                                           as.numeric(st_drop_geometry(all_merge_sf_LCC[i, paste('county_ch4_emiss_bydomain', total, sep='.')])))
    }
    
    
    
    
    
    
    
    
    
    
    
    
    #being lazy - copy pasted code and ctrl+R res for each sector rather than
    #rewriting as a function.  Avoids risk of function working a little
    #differently anyway.
    
    #com
    
    #using the new cover values - want consistent precision/input here
    aces_cover <- cover_all_aces[[i]]
    vu_cover <- cover_all_vulcan[[i]]
    
    # #aces is already cropped, Vulcan is not.  So some counties in the domain may
    # #not have any ACES data.
    # if(!is.null(aces_cover)){
    #   # Starting with a raster of zeros, set cells within polygon i equal to their cover-weighted CO2 emission
    aces_com_temp <- aces_template
    aces_com_temp[aces_cover[,'cell']] <- aces_com[aces_cover[,'cell']]*aces_cover[,'weight']
    # Calculate the fraction of the polygon-total CO2 emission within each cell
    if(cellStats(aces_com_temp, sum)){
      aces_com_frac <- aces_com_temp/cellStats(aces_com_temp, sum)
    } else {
      aces_com_temp[aces_cover[,'cell']] <- aces_cover[,'weight']
      aces_com_frac <- aces_com_temp/cellStats(aces_com_temp, sum)
    }
    # }
    
    vu_com_temp <- vu_template
    vu_com_temp[vu_cover[,'cell']] <- vu_com[vu_cover[,'cell']]*vu_cover[,'weight']
    if(cellStats(vu_com_temp, sum)){
      vu_com_frac <- vu_com_temp/cellStats(vu_com_temp, sum)
    } else {
      vu_com_temp[vu_cover[,'cell']] <- vu_cover[,'weight']
      vu_com_frac <- vu_com_temp/cellStats(vu_com_temp, sum)
    }
    
    # Loop through the different subsectors, and add the CH4 emissions map to the relevant raster
    # Note that we need to use the sf object here, because the merge function has changed the row order relative to all_merge_clean
    for(total in com_totals){
      aces_com_ch4_bystate[[total]][] <- (aces_com_ch4_bystate[[total]][] +
                                            aces_com_frac[]*
                                            as.numeric(st_drop_geometry(all_merge_sf_LCC[i, paste('county_ch4_emiss_bystate', total, sep='.')])))
      aces_com_ch4_bydomain[[total]][] <- (aces_com_ch4_bydomain[[total]][] +
                                             aces_com_frac[]*
                                             as.numeric(st_drop_geometry(all_merge_sf_LCC[i, paste('county_ch4_emiss_bydomain', total, sep='.')])))
      vu_com_ch4_bystate[[total]][] <- (vu_com_ch4_bystate[[total]][] +
                                          vu_com_frac[]*
                                          as.numeric(st_drop_geometry(all_merge_sf_LCC[i, paste('county_ch4_emiss_bystate', total, sep='.')])))
      vu_com_ch4_bydomain[[total]][] <- (vu_com_ch4_bydomain[[total]][] +
                                           vu_com_frac[]*
                                           as.numeric(st_drop_geometry(all_merge_sf_LCC[i, paste('county_ch4_emiss_bydomain', total, sep='.')])))
    }
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #ind
    
    #using the new cover values - want consistent precision/input here
    aces_cover <- cover_all_aces[[i]]
    vu_cover <- cover_all_vulcan[[i]]
    
    # #aces is already cropped, Vulcan is not.  So some counties in the domain may
    # #not have any ACES data.
    # if(!is.null(aces_cover)){
    #   # Starting with a raster of zeros, set cells within polygon i equal to their cover-weighted CO2 emission
    aces_ind_temp <- aces_template
    aces_ind_temp[aces_cover[,'cell']] <- aces_ind[aces_cover[,'cell']]*aces_cover[,'weight']
    # Calculate the fraction of the polygon-total CO2 emission within each cell
    if(cellStats(aces_ind_temp, sum)){
      aces_ind_frac <- aces_ind_temp/cellStats(aces_ind_temp, sum)
    } else {
      aces_ind_temp[aces_cover[,'cell']] <- aces_cover[,'weight']
      aces_ind_frac <- aces_ind_temp/cellStats(aces_ind_temp, sum)
    }
    # }
    
    vu_ind_temp <- vu_template
    vu_ind_temp[vu_cover[,'cell']] <- vu_ind[vu_cover[,'cell']]*vu_cover[,'weight']
    if(cellStats(vu_ind_temp, sum)){
      vu_ind_frac <- vu_ind_temp/cellStats(vu_ind_temp, sum)
    } else {
      vu_ind_temp[vu_cover[,'cell']] <- vu_cover[,'weight']
      vu_ind_frac <- vu_ind_temp/cellStats(vu_ind_temp, sum)
    }
    
    # Loop through the different subsectors, and add the CH4 emissions map to the relevant raster
    # Note that we need to use the sf object here, because the merge function has changed the row order relative to all_merge_clean
    for(total in ind_totals){
      aces_ind_ch4_bystate[[total]][] <- (aces_ind_ch4_bystate[[total]][] +
                                            aces_ind_frac[]*
                                            as.numeric(st_drop_geometry(all_merge_sf_LCC[i, paste('county_ch4_emiss_bystate', total, sep='.')])))
      aces_ind_ch4_bydomain[[total]][] <- (aces_ind_ch4_bydomain[[total]][] +
                                             aces_ind_frac[]*
                                             as.numeric(st_drop_geometry(all_merge_sf_LCC[i, paste('county_ch4_emiss_bydomain', total, sep='.')])))
      vu_ind_ch4_bystate[[total]][] <- (vu_ind_ch4_bystate[[total]][] +
                                          vu_ind_frac[]*
                                          as.numeric(st_drop_geometry(all_merge_sf_LCC[i, paste('county_ch4_emiss_bystate', total, sep='.')])))
      vu_ind_ch4_bydomain[[total]][] <- (vu_ind_ch4_bydomain[[total]][] +
                                           vu_ind_frac[]*
                                           as.numeric(st_drop_geometry(all_merge_sf_LCC[i, paste('county_ch4_emiss_bydomain', total, sep='.')])))
    }
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #elec
    
    #using the new cover values - want consistent precision/input here
    aces_cover <- cover_all_aces[[i]]
    vu_cover <- cover_all_vulcan[[i]]
    
    # #aces is already cropped, Vulcan is not.  So some counties in the domain may
    # #not have any ACES data.
    # if(!is.null(aces_cover)){
    #   # Starting with a raster of zeros, set cells within polygon i equal to their cover-weighted CO2 emission
    aces_elec_temp <- aces_template
    aces_elec_temp[aces_cover[,'cell']] <- aces_elec[aces_cover[,'cell']]*aces_cover[,'weight']
    # Calculate the fraction of the polygon-total CO2 emission within each cell
    if(cellStats(aces_elec_temp, sum)){
      aces_elec_frac <- aces_elec_temp/cellStats(aces_elec_temp, sum)
    } else {
      aces_elec_temp[aces_cover[,'cell']] <- aces_cover[,'weight']
      aces_elec_frac <- aces_elec_temp/cellStats(aces_elec_temp, sum)
    }
    # }
    
    vu_elec_temp <- vu_template
    vu_elec_temp[vu_cover[,'cell']] <- vu_elec[vu_cover[,'cell']]*vu_cover[,'weight']
    if(cellStats(vu_elec_temp, sum)){
      vu_elec_frac <- vu_elec_temp/cellStats(vu_elec_temp, sum)
    } else {
      vu_elec_temp[vu_cover[,'cell']] <- vu_cover[,'weight']
      vu_elec_frac <- vu_elec_temp/cellStats(vu_elec_temp, sum)
    }
    
    # Loop through the different subsectors, and add the CH4 emissions map to the relevant raster
    # Note that we need to use the sf object here, because the merge function has changed the row order relative to all_merge_clean
    for(total in elec_totals){
      aces_elec_ch4_bystate[[total]][] <- (aces_elec_ch4_bystate[[total]][] +
                                            aces_elec_frac[]*
                                            as.numeric(st_drop_geometry(all_merge_sf_LCC[i, paste('county_ch4_emiss_bystate', total, sep='.')])))
      aces_elec_ch4_bydomain[[total]][] <- (aces_elec_ch4_bydomain[[total]][] +
                                             aces_elec_frac[]*
                                             as.numeric(st_drop_geometry(all_merge_sf_LCC[i, paste('county_ch4_emiss_bydomain', total, sep='.')])))
      vu_elec_ch4_bystate[[total]][] <- (vu_elec_ch4_bystate[[total]][] +
                                          vu_elec_frac[]*
                                          as.numeric(st_drop_geometry(all_merge_sf_LCC[i, paste('county_ch4_emiss_bystate', total, sep='.')])))
      vu_elec_ch4_bydomain[[total]][] <- (vu_elec_ch4_bydomain[[total]][] +
                                           vu_elec_frac[]*
                                           as.numeric(st_drop_geometry(all_merge_sf_LCC[i, paste('county_ch4_emiss_bydomain', total, sep='.')])))
    }
    
  }
  

  
  
  
  
  
  
  
  
  #new
  {
    #res
    template_vu <- rast(vu_res)
    template_vu[is.na(template_vu)] <- 0
    template_vu[] <- 0
    
    template_aces <- rast(aces_res)
    template_aces[is.na(template_aces)] <- 0
    template_aces[] <- 0

    cover_vu <- cover_all_vulcan[[i]]
    cover_aces <- cover_all_aces[[i]]
    
    input_inventory_temp_vu <- template_vu
    input_inventory_temp_vu[cover_vu[,'cell']] <- vu_res[cover_vu[,'cell']]*cover_vu[,'weight']
    input_inventory_temp_aces <- template_aces
    input_inventory_temp_aces[cover_aces[,'cell']] <- aces_res[cover_aces[,'cell']]*cover_aces[,'weight']
    # Starting with a raster of zeros, set cells within polygon i equal to their cover-weighted CO2 emission
    
    if(global(input_inventory_temp_vu,sum) == 0){
      input_inventory_temp_vu[cover_vu[,'cell']] <- cover_vu[,'weight']
      input_inventory_frac_vu <- input_inventory_temp_vu/unlist(global(input_inventory_temp_vu, sum))
      #if there are no inventory emissions in this polygon, set the entire
      #polygon to 1 (i.e., equally distribute emissions across the polygon)
    }else{
      input_inventory_frac_vu <- input_inventory_temp_vu/unlist(global(input_inventory_temp_vu, sum))
      # Calculate the fraction of the polygon-total CO2 emissions within each cell
    }
    if(global(input_inventory_temp_aces,sum) == 0){
      input_inventory_temp_aces[cover_aces[,'cell']] <- cover_aces[,'weight']
      input_inventory_frac_aces <- input_inventory_temp_aces/unlist(global(input_inventory_temp_aces, sum))
      #if there are no inventory emissions in this polygon, set the entire
      #polygon to 1 (i.e., equally distribute emissions across the polygon)
    }else{
      input_inventory_frac_aces <- input_inventory_temp_aces/unlist(global(input_inventory_temp_aces, sum))
      # Calculate the fraction of the polygon-total CO2 emissions within each cell
    }
    
    for(total in res_totals){
      new_addition <- as.numeric(as.data.frame(all_merge_LCC_state[i,total]))
      if(!is.na(new_addition)){
        aces_res_ch4_bystate_new[[total]] <- aces_res_ch4_bystate_new[[total]] + input_inventory_frac_aces*new_addition
        vu_res_ch4_bystate_new[[total]] <- vu_res_ch4_bystate_new[[total]] + input_inventory_frac_vu*new_addition
      }
      new_addition <- as.numeric(as.data.frame(all_merge_LCC_state[i,paste0("county_ch4_emiss_bydomain.",total)]))
      if(!is.na(new_addition)){
        aces_res_ch4_bydomain_new[[total]] <- aces_res_ch4_bydomain_new[[total]] + input_inventory_frac_aces*new_addition
        vu_res_ch4_bydomain_new[[total]] <- vu_res_ch4_bydomain_new[[total]] + input_inventory_frac_vu*new_addition
      }
    }
    # Loop through the different subsectors, and add the CH4 emissions map to
    # the relevant raster

    cat("\rFinished mapping res state","level entry",i,"of",length(cover_all_vulcan),"        ")
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #com
    
    template_vu <- rast(vu_com)
    template_vu[is.na(template_vu)] <- 0
    template_vu[] <- 0
    
    template_aces <- rast(aces_com)
    template_aces[is.na(template_aces)] <- 0
    template_aces[] <- 0
    
    cover_vu <- cover_all_vulcan[[i]]
    cover_aces <- cover_all_aces[[i]]
    
    input_inventory_temp_vu <- template_vu
    input_inventory_temp_vu[cover_vu[,'cell']] <- vu_com[cover_vu[,'cell']]*cover_vu[,'weight']
    input_inventory_temp_aces <- template_aces
    input_inventory_temp_aces[cover_aces[,'cell']] <- aces_com[cover_aces[,'cell']]*cover_aces[,'weight']
    # Starting with a raster of zeros, set cells within polygon i equal to their cover-weighted CO2 emission
    
    if(global(input_inventory_temp_vu,sum) == 0){
      input_inventory_temp_vu[cover_vu[,'cell']] <- cover_vu[,'weight']
      input_inventory_frac_vu <- input_inventory_temp_vu/unlist(global(input_inventory_temp_vu, sum))
      #if there are no inventory emissions in this polygon, set the entire
      #polygon to 1 (i.e., equally distribute emissions across the polygon)
    }else{
      input_inventory_frac_vu <- input_inventory_temp_vu/unlist(global(input_inventory_temp_vu, sum))
      # Calculate the fraction of the polygon-total CO2 emissions within each cell
    }
    if(global(input_inventory_temp_aces,sum) == 0){
      input_inventory_temp_aces[cover_aces[,'cell']] <- cover_aces[,'weight']
      input_inventory_frac_aces <- input_inventory_temp_aces/unlist(global(input_inventory_temp_aces, sum))
      #if there are no inventory emissions in this polygon, set the entire
      #polygon to 1 (i.e., equally distribute emissions across the polygon)
    }else{
      input_inventory_frac_aces <- input_inventory_temp_aces/unlist(global(input_inventory_temp_aces, sum))
      # Calculate the fraction of the polygon-total CO2 emissions within each cell
    }
    
    for(total in com_totals){
      new_addition <- as.numeric(as.data.frame(all_merge_LCC_state[i,total]))
      if(!is.na(new_addition)){
        aces_com_ch4_bystate_new[[total]] <- aces_com_ch4_bystate_new[[total]] + input_inventory_frac_aces*new_addition
        vu_com_ch4_bystate_new[[total]] <- vu_com_ch4_bystate_new[[total]] + input_inventory_frac_vu*new_addition
      }
      new_addition <- as.numeric(as.data.frame(all_merge_LCC_state[i,paste0("county_ch4_emiss_bydomain.",total)]))
      if(!is.na(new_addition)){
        aces_com_ch4_bydomain_new[[total]] <- aces_com_ch4_bydomain_new[[total]] + input_inventory_frac_aces*new_addition
        vu_com_ch4_bydomain_new[[total]] <- vu_com_ch4_bydomain_new[[total]] + input_inventory_frac_vu*new_addition
      }
    }
    # Loop through the different subsectors, and add the CH4 emissions map to
    # the relevant raster
    
    cat("\rFinished mapping com state","level entry",i,"of",length(cover_all_vulcan),"        ")
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #ind
    
    template_vu <- rast(vu_ind)
    template_vu[is.na(template_vu)] <- 0
    template_vu[] <- 0
    
    template_aces <- rast(aces_ind)
    template_aces[is.na(template_aces)] <- 0
    template_aces[] <- 0
    
    cover_vu <- cover_all_vulcan[[i]]
    cover_aces <- cover_all_aces[[i]]
    
    input_inventory_temp_vu <- template_vu
    input_inventory_temp_vu[cover_vu[,'cell']] <- vu_ind[cover_vu[,'cell']]*cover_vu[,'weight']
    input_inventory_temp_aces <- template_aces
    input_inventory_temp_aces[cover_aces[,'cell']] <- aces_ind[cover_aces[,'cell']]*cover_aces[,'weight']
    # Starting with a raster of zeros, set cells within polygon i equal to their cover-weighted CO2 emission
    
    if(global(input_inventory_temp_vu,sum) == 0){
      input_inventory_temp_vu[cover_vu[,'cell']] <- cover_vu[,'weight']
      input_inventory_frac_vu <- input_inventory_temp_vu/unlist(global(input_inventory_temp_vu, sum))
      #if there are no inventory emissions in this polygon, set the entire
      #polygon to 1 (i.e., equally distribute emissions across the polygon)
    }else{
      input_inventory_frac_vu <- input_inventory_temp_vu/unlist(global(input_inventory_temp_vu, sum))
      # Calculate the fraction of the polygon-total CO2 emissions within each cell
    }
    if(global(input_inventory_temp_aces,sum) == 0){
      input_inventory_temp_aces[cover_aces[,'cell']] <- cover_aces[,'weight']
      input_inventory_frac_aces <- input_inventory_temp_aces/unlist(global(input_inventory_temp_aces, sum))
      #if there are no inventory emissions in this polygon, set the entire
      #polygon to 1 (i.e., equally distribute emissions across the polygon)
    }else{
      input_inventory_frac_aces <- input_inventory_temp_aces/unlist(global(input_inventory_temp_aces, sum))
      # Calculate the fraction of the polygon-total CO2 emissions within each cell
    }
    
    for(total in ind_totals){
      new_addition <- as.numeric(as.data.frame(all_merge_LCC_state[i,total]))
      if(!is.na(new_addition)){
        aces_ind_ch4_bystate_new[[total]] <- aces_ind_ch4_bystate_new[[total]] + input_inventory_frac_aces*new_addition
        vu_ind_ch4_bystate_new[[total]] <- vu_ind_ch4_bystate_new[[total]] + input_inventory_frac_vu*new_addition
      }
      new_addition <- as.numeric(as.data.frame(all_merge_LCC_state[i,paste0("county_ch4_emiss_bydomain.",total)]))
      if(!is.na(new_addition)){
        aces_ind_ch4_bydomain_new[[total]] <- aces_ind_ch4_bydomain_new[[total]] + input_inventory_frac_aces*new_addition
        vu_ind_ch4_bydomain_new[[total]] <- vu_ind_ch4_bydomain_new[[total]] + input_inventory_frac_vu*new_addition
      }
    }
    # Loop through the different subsectors, and add the CH4 emissions map to
    # the relevant raster
    
    cat("\rFinished mapping ind state","level entry",i,"of",length(cover_all_vulcan),"        ")
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #elec
    
    template_vu <- rast(vu_elec)
    template_vu[is.na(template_vu)] <- 0
    template_vu[] <- 0
    
    template_aces <- rast(aces_elec)
    template_aces[is.na(template_aces)] <- 0
    template_aces[] <- 0
    
    cover_vu <- cover_all_vulcan[[i]]
    cover_aces <- cover_all_aces[[i]]
    
    input_inventory_temp_vu <- template_vu
    input_inventory_temp_vu[cover_vu[,'cell']] <- vu_elec[cover_vu[,'cell']]*cover_vu[,'weight']
    input_inventory_temp_aces <- template_aces
    input_inventory_temp_aces[cover_aces[,'cell']] <- aces_elec[cover_aces[,'cell']]*cover_aces[,'weight']
    # Starting with a raster of zeros, set cells within polygon i equal to their cover-weighted CO2 emission
    
    if(global(input_inventory_temp_vu,sum) == 0){
      input_inventory_temp_vu[cover_vu[,'cell']] <- cover_vu[,'weight']
      input_inventory_frac_vu <- input_inventory_temp_vu/unlist(global(input_inventory_temp_vu, sum))
      #if there are no inventory emissions in this polygon, set the entire
      #polygon to 1 (i.e., equally distribute emissions across the polygon)
    }else{
      input_inventory_frac_vu <- input_inventory_temp_vu/unlist(global(input_inventory_temp_vu, sum))
      # Calculate the fraction of the polygon-total CO2 emissions within each cell
    }
    if(global(input_inventory_temp_aces,sum) == 0){
      input_inventory_temp_aces[cover_aces[,'cell']] <- cover_aces[,'weight']
      input_inventory_frac_aces <- input_inventory_temp_aces/unlist(global(input_inventory_temp_aces, sum))
      #if there are no inventory emissions in this polygon, set the entire
      #polygon to 1 (i.e., equally distribute emissions across the polygon)
    }else{
      input_inventory_frac_aces <- input_inventory_temp_aces/unlist(global(input_inventory_temp_aces, sum))
      # Calculate the fraction of the polygon-total CO2 emissions within each cell
    }
    
    for(total in elec_totals){
      new_addition <- as.numeric(as.data.frame(all_merge_LCC_state[i,total]))
      if(!is.na(new_addition)){
        aces_elec_ch4_bystate_new[[total]] <- aces_elec_ch4_bystate_new[[total]] + input_inventory_frac_aces*new_addition
        vu_elec_ch4_bystate_new[[total]] <- vu_elec_ch4_bystate_new[[total]] + input_inventory_frac_vu*new_addition
      }
      new_addition <- as.numeric(as.data.frame(all_merge_LCC_state[i,paste0("county_ch4_emiss_bydomain.",total)]))
      if(!is.na(new_addition)){
        aces_elec_ch4_bydomain_new[[total]] <- aces_elec_ch4_bydomain_new[[total]] + input_inventory_frac_aces*new_addition
        vu_elec_ch4_bydomain_new[[total]] <- vu_elec_ch4_bydomain_new[[total]] + input_inventory_frac_vu*new_addition
      }
    }
    # Loop through the different subsectors, and add the CH4 emissions map to
    # the relevant raster
    
    cat("\rFinished mapping elec state","level entry",i,"of",length(cover_all_vulcan),"        ")
    
    
  }
  
  
}


################################################################################
#write a function to compare the output either across the final map or 1 county
#at a time (that part is more useful if manually walking through the loop above)

divergent <- colorRampPalette(c("red","white","blue"))

dir.create(plot_directory,showWarnings = F)

checker <- function(total,i,rawinventory,scale){
  sector <- strsplit(total,"_")[[1]][1]
  
  #files to cat to to describe the statistics of the delta
  if(file.exists(paste0(plot_directory,"/Vulcan_",total,"_",scale,".txt"))){
    unlink(paste0(plot_directory,"/Vulcan_",total,"_",scale,".txt"))
    unlink(paste0(plot_directory,"/ACES_",total,"_",scale,".txt"))
  }
  
  
  
  
  
  new_ACES <- get(paste0("aces_",sector,"_ch4_by",scale,"_new"))
  old_ACES <- get(paste0("aces_",sector,"_ch4_by",scale))
  new_vu <- get(paste0("vu_",sector,"_ch4_by",scale,"_new"))
  old_vu <- get(paste0("vu_",sector,"_ch4_by",scale))
  
  raw_sectoral_ACES <- get(paste0("aces_",sector))
  raw_sectoral_Vulcan <- get(paste0("vu_",sector))
  
  if(i){
    county_i <- County_Tigerlines_trans[which(relate(ext(rast(raw_sectoral_ACES)[cover_all_aces[[i]][which.max(cover_all_aces[[i]][,"weight"]),"cell"],drop=F]),
                                                     County_Tigerlines_trans,"within")),]
  }else{
    county_i <- County_Tigerlines_trans
  }
  
  delta <- rast(old_vu[[total]]) - new_vu[[total]]
  delta[values(delta)==0] <- NA
  new_vu[[total]][values(new_vu[[total]])==0] <- NA
  old_vu[[total]][values(old_vu[[total]])==0] <- NA
  if(i){
    plot(delta,main=paste0("county ",i," ",scale," Vulcan old - new ",total),ext=ext(county_i),
         range=unlist(global(abs(delta),max,na.rm=T))*c(-1,1),
         colNA="black",
         plg=list(title="mol/km2/s"),
         xlab="Longitude",ylab="Latitude",
         col=divergent(64),mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
    lines(county_i,col="dimgrey")
  }else{
    
    png(paste0(plot_directory,"/stat_comb_",total,"_Vulcan_",scale,"_delta.png"))
    plot(delta,main=paste0("overall ",scale," Vulcan old - new ",total),ext=ext(county_i),
         range=unlist(global(abs(delta),max,na.rm=T))*c(-1,1),
         colNA="black",
         plg=list(title="mol/km2/s"),
         xlab="Longitude",ylab="Latitude",
         col=divergent(64),mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
    lines(county_i,col="dimgrey")
    dev.off()
    
    png(paste0(plot_directory,"/stat_comb_",total,"_Vulcan_",scale,"_new.png"))
    plot(new_vu[[total]],main=paste0("overall ",scale," Vulcan new ",total),ext=ext(county_i),
         colNA="black",
         plg=list(title="mol/km2/s"),
         xlab="Longitude",ylab="Latitude",mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
    lines(county_i,col="dimgrey")
    dev.off()
    
    png(paste0(plot_directory,"/stat_comb_",total,"_Vulcan_",scale,"_old.png"))
    plot(rast(old_vu[[total]]),main=paste0("overall ",scale," Vulcan old ",total),ext=ext(county_i),
         colNA="black",
         plg=list(title="mol/km2/s"),
         xlab="Longitude",ylab="Latitude",mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
    lines(county_i,col="dimgrey")
    dev.off()
  }
  
  if(i){
    cat("\nCounty",i,total,scale,"Vulcan range delta = ",unlist(global(delta,range,na.rm=T)))
    cat("\nnew=",unlist(global(new_vu[[total]],sum,na.rm=T)))
    cat("\nold=",unlist(global(rast(old_vu[[total]]),sum,na.rm=T)))
    cat("\nold/new=",unlist(global(rast(old_vu[[total]]),sum,na.rm=T)/global(new_vu[[total]],sum,na.rm=T)))
  }else{
    cat("\nCounty",i,total,scale,"Vulcan range delta = ",unlist(global(delta,range,na.rm=T)),
        file=paste0(plot_directory,"/Vulcan_",total,"_",scale,".txt"),append=T)
    cat("\nnew=",unlist(global(new_vu[[total]],sum,na.rm=T)),
        file=paste0(plot_directory,"/Vulcan_",total,"_",scale,".txt"),append=T)
    cat("\nold=",unlist(global(rast(old_vu[[total]]),sum,na.rm=T)),
        file=paste0(plot_directory,"/Vulcan_",total,"_",scale,".txt"),append=T)
    cat("\nold/new=",unlist(global(rast(old_vu[[total]]),sum,na.rm=T)/global(new_vu[[total]],sum,na.rm=T)),
        file=paste0(plot_directory,"/Vulcan_",total,"_",scale,".txt"),append=T)
  }
  
  
  
  
  
  delta <- rast(old_ACES[[total]]) - new_ACES[[total]]
  delta[values(delta)==0] <- NA
  new_ACES[[total]][values(new_ACES[[total]])==0] <- NA
  old_ACES[[total]][values(old_ACES[[total]])==0] <- NA
  if(i){
    plot(delta,main=paste0("county ",i," ",scale," ACES old - new ",total),ext=ext(county_i),
         range=unlist(global(abs(delta),max,na.rm=T))*c(-1,1),
         colNA="black",
         plg=list(title="mol/km2/s"),
         xlab="Longitude",ylab="Latitude",
         col=divergent(64),mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
    lines(county_i,col="dimgrey")
  }else{
    png(paste0(plot_directory,"/stat_comb_",total,"_ACES_",scale,"_delta.png"))
    plot(delta,main=paste0("overall ",scale," ACES old - new ",total),ext=ext(county_i),
         range=unlist(global(abs(delta),max,na.rm=T))*c(-1,1),
         colNA="black",
         plg=list(title="mol/km2/s"),
         xlab="Longitude",ylab="Latitude",
         col=divergent(64),mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
    lines(county_i,col="dimgrey")
    dev.off()
    
    png(paste0(plot_directory,"/stat_comb_",total,"_ACES_",scale,"_new.png"))
    plot(new_ACES[[total]],main=paste0("overall ",scale," ACES new ",total),ext=ext(county_i),
         colNA="black",
         plg=list(title="mol/km2/s"),
         xlab="Longitude",ylab="Latitude",mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
    lines(county_i,col="dimgrey")
    dev.off()
    
    png(paste0(plot_directory,"/stat_comb_",total,"_ACES_",scale,"_old.png"))
    plot(rast(old_ACES[[total]]),main=paste0("overall ",scale," ACES old ",total),ext=ext(county_i),
         colNA="black",
         plg=list(title="mol/km2/s"),
         xlab="Longitude",ylab="Latitude",mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
    lines(county_i,col="dimgrey")
    dev.off()
    
  }
  
  if(i){
    cat("\nCounty",i,total,scale,"ACES range delta = ",unlist(global(delta,range,na.rm=T)))
    cat("\nnew=",unlist(global(new_ACES[[total]],sum,na.rm=T)))
    cat("\nold=",unlist(global(rast(old_ACES[[total]]),sum,na.rm=T)))
    cat("\nold/new=",unlist(global(rast(old_ACES[[total]]),sum,na.rm=T)/global(new_ACES[[total]],sum,na.rm=T)))
  }else{
    cat("\nCounty",i,total,scale,"ACES range delta = ",unlist(global(delta,range,na.rm=T)),
        file=paste0(plot_directory,"/ACES_",total,"_",scale,".txt"),append=T)
    cat("\nnew=",unlist(global(new_ACES[[total]],sum,na.rm=T)),
        file=paste0(plot_directory,"/ACES_",total,"_",scale,".txt"),append=T)
    cat("\nold=",unlist(global(rast(old_ACES[[total]]),sum,na.rm=T)),
        file=paste0(plot_directory,"/ACES_",total,"_",scale,".txt"),append=T)
    cat("\nold/new=",unlist(global(rast(old_ACES[[total]]),sum,na.rm=T)/global(new_ACES[[total]],sum,na.rm=T)),
        file=paste0(plot_directory,"/ACES_",total,"_",scale,".txt"),append=T)
  }
  
  
  if(rawinventory){
    plot(rast(raw_sectoral_ACES),ext=ext(county_i),main="ACES")
    lines(county_i,col="dimgrey")
    
    plot(rast(raw_sectoral_Vulcan),ext=ext(county_i),main="Vulcan")
    lines(county_i,col="dimgrey")
  }
}

################################################################################

for(total in res_totals){
  checker(total,i=F,rawinventory=F,scale="state")
  checker(total,i=F,rawinventory=F,scale="domain")
}
for(total in com_totals){
  checker(total,i=F,rawinventory=F,scale="state")
  checker(total,i=F,rawinventory=F,scale="domain")
}
for(total in ind_totals){
  checker(total,i=F,rawinventory=F,scale="state")
  checker(total,i=F,rawinventory=F,scale="domain")
}
for(total in elec_totals){
  checker(total,i=F,rawinventory=F,scale="state")
  checker(total,i=F,rawinventory=F,scale="domain")
}

#just map the county FIPS to know which is which
plot(aces_res_ch4_bydomain_new$res_petr_ER,ext=ext(County_Tigerlines_trans))
text(County_Tigerlines_trans,County_Tigerlines_trans$COUNTYFP)
lines(County_Tigerlines_trans)
lines(project(State_Tigerlines,County_Tigerlines_trans),col="red")

#to be run while manually parts of checker to further investigate.
county_i <- County_Tigerlines_trans[County_Tigerlines_trans$COUNTYFP=="009" & County_Tigerlines_trans$STATEFP=="24",]

#state FIPS
#NY = 36
#NJ = 34
#PA = 42
#MD = 24
#DE = 10

#which i matches the one we want to investigate?
which(paste0(all_merge_LCC_state$STATEFP,all_merge_LCC_state$COUNTYFP)=="24009")

