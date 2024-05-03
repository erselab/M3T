## stationary_combustion_r3.R
## In use: 2022-03-02 16:00
#
# Spatially allocate fuel- and sector-specific stationary combustion emissions
# The totals are calculated at the state level from EIA SEDS data (and the EPA national inventory - see note below).
# These are then spatially disaggregated to the county level according to the corresponding CO emissions from the 2017 NEI
# Within each county, emissions are spatially disaggregated according to ACES or Vulcan CO2 emissions

################################################################################
#User input
NEI_file <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw data/NEI_2017.xlsx"

county_outline_file <- "G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Shapefiles/tl_2022_us_county/tl_2022_us_county.shp"

Vulcan_folder <- 'G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Inventories/Vulcan_v3.0/Sectoral'
ACES_folder <- 'G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Inventories/ACES_v2.0/Sectoral'
#folders with all the sectoral versions of ACES/Vulcan

Output_folder <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed/"

Use_Vulcan <- TRUE
Use_ACES <- TRUE
#which inventory to use in the spatial disaggregation?  Can be both, has to be
#at least 1 to finish processing.

ACES_year <- 2017
#year of ACES data, will be part of the filename

vulcan_band <- 6
#year of Vulcan data.  Assuming Vulcan v3.0, 1 - 6 corresponding to years 2010 -
#2015

state_list <- c("DE","MD","NY","NJ","PA","US")
#States within the domain + national

SEDS_year <- 2019
#year of SEDS data to rely on.  Will automatically download the data from
#"https://www.eia.gov/state/seds/data.php?incfile=/state/seds/sep_use/",sector,"/use_",sector,"_",state,".html&sid=",state
#e.g.,
#"https://www.eia.gov/state/seds/data.php?incfile=/state/seds/sep_use/res/use_res_NY.html&sid=NY"
#the code assumes that petroleum is the only fossil fuel with multiple columns,
#and that the second table is in trillion btu.

# temp_location <- "~/../../Kristian/Desktop/"
#a place to save a downloaded HTML temporarily.  Due to permission errors that
#claim R is still using the file, it is unable to automatically delete it some
#of the time...

EPA_data <- data.frame("State"="US_EPA",
                       #"res_coal"=0,
                       "com_coal"=17,
                       "ind_coal"=517,
                       "elec_coal"=10554,
                       "res_petr"=975,
                       "com_petr"=801,
                       "ind_petr"=2062,
                       "elec_petr"=42,
                       # "res_gas"=5208,
                       "com_gas"=3647,
                       "ind_gas"=9484,
                       "elec_gas"=11553,
                       "res_wood"=544,
                       "com_wood"=84,
                       "ind_wood"=1407,
                       "elec_wood"=68)
#Data from table A-67 Fuel Consumption by Stationary Combustion for Calculating
#CH4 and N2O Emissions (TBtu) for the same year as the SEDS data.  It's a PDF,
#so no way to easily import automatically.  Names must be as such to match those
#pulled from SEDS.  Res coal doesn't exist in US and res gas is dealt with
#separately, so not included here.
#EPA=https://www.epa.gov/ghgemissions/inventory-us-greenhouse-gas-emissions-and-sinks-1990-2020

#res = residential
#com = commercial
#ind = industrial
#elec = electrical
#petr = petroleum
#gas = natural gas


#emission factors for each sector
# - Conversion from higher heating value to lower heating value (0.9 or 0.95)
# - Conversion from trillion Btu to GJ (1e9/947.8170777491506)
# - Conversion from GJ to g/yr of CH4 (IPCC default values, except natural gas power plants from Hajny et al. doi: 10.1021/acs.est.9b01875)
# - Conversion from g/yr to mol/s (1/(16.043*365*24*60*60))

Emission_factors <- data.frame(
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
#for NG fired electric.  They can be viewed in IPCC 2006 volume 2: Energy tables
#2.2 through 2.5 (https://www.ipcc-nggip.iges.or.jp/public/2006gl/vol2.html).
#The natural gas electric sector emission factor is from Hajny et al., 2019
#(https://doi.org/10.1021/acs.est.9b01875).  It is 5.4 g/MMBTU and within its
#uncertainties of the GHGI value of 3.9 g/MMBTU.  Note this value is only for
#Combined Cycle NG power plants, however they make up the vast majority of NG
#use for electricity in the US.

source("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Code and method/Scripts/Inventory_based_disaggregation.R")
#Load in a function to disaggregate total emissions using ACES/Vulcan or both
#within sub-domains (state, entire domain)

XESMF_check <- TRUE
#use xesmf to reproject (TRUE), or projectraster (FALSE)

d03_bounding_box <- cbind(c(-76.65,-73.65),c(38.97,40.97))
resolution <- 0.01
#Will be used to generate a blank raster for the output to be built onto, WGS84
#CRS.  Resolution in deg, bounding box in lat/long.  Only needed if using
#projectraster

API_key <- "1kLep4UApTZKwdOrDkW6J8qlO0niiw8ej0JPliyc"
################################################################################
#load packages
i <- 1
packagecheck <- c("raster","ncdf4","readxl","sf","rvest","httr","dplyr","jsonlite")

while(i<=length(packagecheck)){
  if(length(find.package(packagecheck[i],quiet = TRUE))<1){
    install.packages(packagecheck[i],repos="https://repo.miserver.it.umich.edu/cran/")
  }
  i <- i+1
}

suppressPackageStartupMessages(invisible(lapply(packagecheck, library, character.only=TRUE)))
rm(packagecheck,i)

#raster + ncdf4 = raster and .nc filetype functionalities
#readxl = more ways to load different excel filetypes
#sf = introduces several useful spatial functions and classes
#rvest = code to make it easier to read HTML
################################################################################
#Quit ASAP if neither ACES or Vulcan are set to be used.  Need one of them
if(!(Use_Vulcan | Use_ACES)){
  stop("We need ACES, Vulcan or both to disaggregate emissions.")
}

################################################################################
#now quickly build the output raster matrix and load in the raster NLCD data

d03_rast <- raster(nrows=diff(range(d03_bounding_box[,2]))/resolution, 
                   ncols=diff(range(d03_bounding_box[,1]))/resolution,
                   xmn=min(d03_bounding_box[,1]), xmx=max(d03_bounding_box[,1]),
                   ymn=min(d03_bounding_box[,2]), ymx=max(d03_bounding_box[,2]), 
                   crs=4326)

rm(d03_bounding_box,resolution)
################################################################################
#download and prepare SEDS data

#see https://www.eia.gov/opendata/browser/seds
SEDS_URL <- paste0("https://api.eia.gov/v2/seds/data/?frequency=annual&data[0]=value&facets[seriesId][]=CLCCB",
                   "&facets[seriesId][]=CLEIB&facets[seriesId][]=CLICB&facets[seriesId][]=NGCCB&facets[seriesId][]=NGEIB&facets[seriesId][]=NGICB&facets[seriesId][]=PACCB&facets[seriesId][]=PAEIB&facets[seriesId][]=PAICB&facets[seriesId][]=PARCB&facets[seriesId][]=WDRCB&facets[seriesId][]=WWCCB&facets[seriesId][]=WWEIB&facets[seriesId][]=WWICB",
                   paste0("&facets[stateId][]=",state_list,collapse = ""),
                   "&start=",SEDS_year,"&end=",SEDS_year,
                   "&sort[0][column]=seriesId&sort[0][direction]=asc&offset=0&api_key=",API_key)

#download directly into R and keep only the data table
EIA_raw_data <- fromJSON(SEDS_URL)
EIA_raw_data <- EIA_raw_data$response$data

#rearrange columns/rows to better mesh with EPA data
EIA_data=(reshape(EIA_raw_data[,c("seriesId","stateId","value")],idvar="stateId",timevar = "seriesId",direction="wide"))

#rename to be consistent with EPA (matches SEDS webpage)
colnames(EIA_data) <- c("State",colnames(EPA_data)[c(2,4,3,9,11,10,6,8,7,5,12,13,15,14)])
EIA_data$State <- gsub("US","US_SEDS",EIA_data$State)

#make numeric rather than text, combine with EPA, and sort by state
EIA_data[,-1] <- apply(EIA_data[,-1], 2, FUN=function(x){as.numeric(x)/1000})
stat_comb_data <- rbind(EIA_data,EPA_data)
stat_comb_data <- stat_comb_data[order(stat_comb_data$State),]

#round so that EPA and SEDS have the same precision (national) and the other
#sector-states have the same precision as the webpage data
stat_comb_data[stat_comb_data$State=="US_SEDS",-1] <- 
  round(stat_comb_data[stat_comb_data$State=="US_SEDS",-1])
stat_comb_data[,-1] <- 
  round(stat_comb_data[,-1],1)

rm(EIA_raw_data,EIA_data)
################################################################################
# # #automatically download and combine the data from the EIA SEDS database.  Also
# # #pull in the GHGI data from EPA.
# 
# state_list <- c(state_list,"US")
# #add the US total to the list
# 
# sector_list <- c('res',"com",'ind',"eu")
# fuel_list <- c("Petroleum","Coal","NaturalGas","Biomass")
# #values as named in the SEDS tables
# 
# download_dest <- paste0(temp_location,"temp.html")
# #temporary file, will be overwritten many times as values are pulled and then
# #the next file downloaded
# 
# stat_comb_data <- data.frame(matrix(ncol=16,nrow=length(state_list)))
# #initialize output
# 
# for(state in state_list){
#   for(sector in sector_list){
#     counter = 0
#     repeat{
#       counter=counter+1
#       info=tryCatch(
#         #the url is build from the GHGRP ID, the desired year, and a common url.
#         #This file contains more information about the facility that isn't in the
#         #downloaded file.
#         download.file(paste0("https://www.eia.gov/state/seds/data.php?incfile=/state/seds/sep_use/",sector,"/use_",sector,"_",state,".html&sid=",state),
#                       destfile=download_dest,quiet = T),
#         warning = function(w) {
#           Sys.sleep(1)
#           NA
#         },
#         error = function(e) {
#           Sys.sleep(1)
#           NA
#         }
#       )
#       if(!is.na(info)) {
#         break
#       }
#       if(counter>=10){
#         stop("Failed to download ",state,sector," from\n",
#              paste0("https://www.eia.gov/state/seds/data.php?incfile=/state/seds/sep_use/",sector,"/use_",sector,"_",state,".html&sid=",state),
#              "The links used may no longer be accurate.  Check the EIA SEDS website.")
#       }
#     }
#     #try to download the url, and retry up to 10x with 1s between runs as the link
#     #may fail on occasion.
#     #from https://stackoverflow.com/a/60880960
#     
#     HTML_data=read_html(download_dest)
#     HTML_data=html_table(HTML_data)
#     HTML_data <- HTML_data[[2]]
#     #load in the file and keep just the 2nd tibble (first and third are just
#     #footnotes)
#     
#     if(all(!HTML_data[,1]==SEDS_year)){
#       #there is no data for this year, quit
#       cat(paste0("https://www.eia.gov/state/seds/data.php?incfile=/state/seds/sep_use/",sector,"/use_",sector,"_",state,".html&sid=",state),"\n")
#       stop("There is no SEDS data for the specified year, or the links used are no longer accurate.  Check the website to see the latest data available.")
#     }
#     
#     state_number <- which(state==state_list)
#     sector_number <- which(sector==sector_list)
#     #need to know the number to set which row/column of output
#     for(fuel in fuel_list){
#       fuel_number <- which(fuel==fuel_list)
#       column <- grep(fuel,HTML_data[1,])
#       #which column(s) are this fuel?
#       if(fuel=="Petroleum"){
#         column <- column[grep("Total",HTML_data[2,column])]
#         #petroleum has several subcategories.  Find just the total column
#       }
#       stat_comb_data[state_number,fuel_number+(sector_number-1)*4] <-
#         HTML_data[which(HTML_data[,1]==SEDS_year)[2],column[1]]
#       #state = row, column = fuel-sector combo.  Just use the specified year and
#       #identified column.  Pull the 2nd row that is this year (first is in
#       #different units)
#     }#fuel loop
#   }#sector loop
#   cat("\nFinished downloading SEDS data for",state)
# }#state loop
# 
# state_list <- gsub('US','US_SEDS',state_list)
# state_list <- c(state_list,"US_EPA")
# sector_list <- gsub('eu','elec',sector_list)
# fuel_list <- c("petr","coal","gas","wood")
# #update the names to simpler ones for coding, and now include EPA in the state
# #list (will rbind that data to stat_comb)
# 
# stat_comb_data[] <- lapply(stat_comb_data,FUN=function(x){as.numeric(gsub(",","",x))})
# #[] allows it to remain a dataframe.  convert all to numeric, remove 1,000 place
# #commas.
# 
# colnames(stat_comb_data) <- paste0(rep(sector_list,each=length(fuel_list)),"_",fuel_list)
# #colnames = sector_fuel
# 
# stat_comb_data <- rbind(stat_comb_data,EPA_data)
# stat_comb_data <- cbind(factor(state_list),stat_comb_data)
# colnames(stat_comb_data)[1] <- "State"
# #add EPA data and a columnn with the state for each row
# 
# unlink(download_dest)
# #delete the temp file
# 
# rm(state_list,sector_list,download_dest,fuel_list,state,sector,fuel,counter,
#    column,fuel_number,sector_number,state_number,HTML_data,temp_location,info,
#    SEDS_year,EPA_data)
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

stat_comb_data_adj$com_coal_ER <- stat_comb_data_adj$com_coal*0.95*(1e9/947.8170777491506)*Emission_factors$com_coal/(16.043*365*24*60*60)
stat_comb_data_adj$ind_coal_ER <- stat_comb_data_adj$ind_coal*0.95*(1e9/947.8170777491506)*Emission_factors$ind_coal/(16.043*365*24*60*60)
stat_comb_data_adj$elec_coal_ER <- stat_comb_data_adj$elec_coal*0.95*(1e9/947.8170777491506)*Emission_factors$elec_coal/(16.043*365*24*60*60)

stat_comb_data_adj$res_petr_ER <- stat_comb_data_adj$res_petr*0.95*(1e9/947.8170777491506)*Emission_factors$res_petr/(16.043*365*24*60*60)
stat_comb_data_adj$com_petr_ER <- stat_comb_data_adj$com_petr*0.95*(1e9/947.8170777491506)*Emission_factors$com_petr/(16.043*365*24*60*60)
stat_comb_data_adj$ind_petr_ER <- stat_comb_data_adj$ind_petr*0.95*(1e9/947.8170777491506)*Emission_factors$ind_petr/(16.043*365*24*60*60)
stat_comb_data_adj$elec_petr_ER <- stat_comb_data_adj$elec_petr*0.95*(1e9/947.8170777491506)*Emission_factors$elec_petr/(16.043*365*24*60*60)

stat_comb_data_adj$com_gas_ER <- stat_comb_data_adj$com_gas*0.9*(1e9/947.8170777491506)*Emission_factors$com_gas/(16.043*365*24*60*60)
stat_comb_data_adj$ind_gas_ER <- stat_comb_data_adj$ind_gas*0.9*(1e9/947.8170777491506)*Emission_factors$ind_gas/(16.043*365*24*60*60)
stat_comb_data_adj$elec_gas_ER <- stat_comb_data_adj$elec_gas*0.9*(1e9/947.8170777491506)*Emission_factors$elec_gas/(16.043*365*24*60*60)

stat_comb_data_adj$res_wood_ER <- stat_comb_data_adj$res_wood*0.9*(1e9/947.8170777491506)*Emission_factors$res_wood/(16.043*365*24*60*60)
stat_comb_data_adj$com_wood_ER <- stat_comb_data_adj$com_wood*0.9*(1e9/947.8170777491506)*Emission_factors$com_wood/(16.043*365*24*60*60)
stat_comb_data_adj$ind_wood_ER <- stat_comb_data_adj$ind_wood*0.9*(1e9/947.8170777491506)*Emission_factors$ind_wood/(16.043*365*24*60*60)
stat_comb_data_adj$elec_wood_ER <- stat_comb_data_adj$elec_wood*0.9*(1e9/947.8170777491506)*Emission_factors$elec_wood/(16.043*365*24*60*60)

state_total_ch4 <- cbind(stat_comb_data_adj$State, stack(stat_comb_data_adj[grepl('_ER', names(stat_comb_data_adj))]))
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

county_shp <- st_read(county_outline_file)
merge_with_poly <- merge(df_wide,
                         county_shp,
                         by.x=c('STATE_FIPS', 'COUNTY_FIPS'),
                         by.y=c('STATEFP','COUNTYFP'))
all_merge_sf <- st_as_sf(merge_with_poly, sf_column_name='geometry', crs=crs(county_shp))
# Load county shapefile and merge geometries with the emissions data

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
  aces_res <- raster(file.path(ACES_folder,paste0(ACES_year,'_Annual_ACES_Residential.nc')))
  aces_com <- raster(file.path(ACES_folder,paste0(ACES_year,'_Annual_ACES_Commercial.nc')))
  aces_ind <- raster(file.path(ACES_folder,paste0(ACES_year,'_Annual_ACES_Industrial.nc')))
  aces_elec <- raster(file.path(ACES_folder,paste0(ACES_year,'_Annual_ACES_Elec.nc')))
  crs_to_use <- crs(aces_res)
  #load in ACES files, pull crs to transform all_merge_sf later
}
if(Use_Vulcan){
  vu_res <- raster(file.path(Vulcan_folder,"Vulcan_v3_US_annual_1km_residential_mn.nc4"), varname='carbon_emissions', band=vulcan_band)
  vu_com <- raster(file.path(Vulcan_folder,'Vulcan_v3_US_annual_1km_commercial_mn.nc4'), varname='carbon_emissions', band=vulcan_band)
  vu_ind <- raster(file.path(Vulcan_folder,'Vulcan_v3_US_annual_1km_industrial_mn.nc4'), varname='carbon_emissions', band=vulcan_band)
  vu_elec <- raster(file.path(Vulcan_folder,'Vulcan_v3_US_annual_1km_elec_prod_mn.nc4'), varname='carbon_emissions', band=vulcan_band)
  crs_to_use <- crs(vu_res)
}

if(Use_ACES & Use_Vulcan){
  if(!compareCRS(aces_res,vu_res)){
    stop('Code assumes CO2 inventories have the same CRS')
  }
}
# Going to assume that ACES and Vulcan have the same CRS - check that here

# Transform to ACES/Vulcan CRS
all_merge_sf_LCC <- st_transform(all_merge_sf, crs(crs_to_use))

# Convert all_merge_sf_LCC to Spatial so we can use it with raster more easily
all_merge_sp_LCC <- as(all_merge_sf_LCC, 'Spatial')

all_merge_sf_LCC_state <- all_merge_sf_LCC
colnames(all_merge_sf_LCC_state) <- gsub("county_ch4_emiss_bystate.","",colnames(all_merge_sf_LCC_state))
all_merge_sf_LCC_domain <- all_merge_sf_LCC
colnames(all_merge_sf_LCC_domain) <- gsub("county_ch4_emiss_bydomain.","",colnames(all_merge_sf_LCC_domain))
#create a copy that has names for the bystate or bydomain version that exactly
#match the totals.  Easier/more consistent to code.
if(Use_ACES){
  cover_all <- cellFromPolygon(aces_res, all_merge_sp_LCC, weights = TRUE)
  disaggregation(aces_res,res_totals,agg_level="state",sf_input=all_merge_sf_LCC_state)
  disaggregation(aces_com,com_totals,agg_level="state",sf_input=all_merge_sf_LCC_state)
  disaggregation(aces_ind,ind_totals,agg_level="state",sf_input=all_merge_sf_LCC_state)
  disaggregation(aces_elec,elec_totals,agg_level="state",sf_input=all_merge_sf_LCC_state)
  
  disaggregation(aces_res,res_totals,agg_level="domain",sf_input=all_merge_sf_LCC_domain)
  disaggregation(aces_com,com_totals,agg_level="domain",sf_input=all_merge_sf_LCC_domain)
  disaggregation(aces_ind,ind_totals,agg_level="domain",sf_input=all_merge_sf_LCC_domain)
  disaggregation(aces_elec,elec_totals,agg_level="domain",sf_input=all_merge_sf_LCC_domain)
}
if(Use_Vulcan){
  cover_all <- cellFromPolygon(vu_res, all_merge_sp_LCC, weights = TRUE)
  disaggregation(vu_res,res_totals,agg_level="state",sf_input=all_merge_sf_LCC_state)
  disaggregation(vu_com,com_totals,agg_level="state",sf_input=all_merge_sf_LCC_state)
  disaggregation(vu_ind,ind_totals,agg_level="state",sf_input=all_merge_sf_LCC_state)
  disaggregation(vu_elec,elec_totals,agg_level="state",sf_input=all_merge_sf_LCC_state)
  
  disaggregation(vu_res,res_totals,agg_level="domain",sf_input=all_merge_sf_LCC_domain)
  disaggregation(vu_com,com_totals,agg_level="domain",sf_input=all_merge_sf_LCC_domain)
  disaggregation(vu_ind,ind_totals,agg_level="domain",sf_input=all_merge_sf_LCC_domain)
  disaggregation(vu_elec,elec_totals,agg_level="domain",sf_input=all_merge_sf_LCC_domain)
}
rm(all_merge_sf_LCC_state,all_merge_sf_LCC_domain)

################################################################################
#write a function to save, dependent on whether or not we use XESMF
if(XESMF_check){
  save_data <- function(input){
    input_name <- deparse(substitute(input))
    #pull the input name (e.g., vu_com_ch4_bydomain[[total]])
    disaggregation_level <- substring(text = input_name,regexpr("by",input_name),
                                      regexpr("\\[",input_name)-1)
    inventory_name <- strsplit(input_name,"_")[[1]][1]
    #pull the bydomain/bystate/byldc and vu/aces parts
    writeRaster(input,
                paste0(Output_folder,'/',inventory_name,'_',disaggregation_level,'_stat_comb_',total,'.nc'),
                force_v4=TRUE,
                varname='methane_emissions',
                varunit='mol/km2/s',
                longname=paste0(inventory_name,'_',disaggregation_level,'_stat_comb_',total),
                NAflag=-9999,
                overwrite=TRUE)
  }
}else{
  #project with projectraster
  save_data <- function(input){
    input_name <- deparse(substitute(input))
    disaggregation_level <- substring(text = input_name,regexpr("by",input_name),
                                      regexpr("\\[",input_name)-1)
    inventory_name <- strsplit(input_name,"_")[[1]][1]
    input <- projectRaster(input,to=d03_rast)
    #project to a grid with the exact right resolution, extent and origin.
    writeRaster(input,
                paste0(Output_folder,'/',inventory_name,'_',disaggregation_level,'_stat_comb_',total,'_regridded.nc'),
                force_v4=TRUE,
                varname='methane_emissions',
                varunit='mol/km2/s',
                longname=paste0(inventory_name,'_',disaggregation_level,'_stat_comb_',total),
                NAflag=-9999,
                overwrite=TRUE)
  }
}
################################################################################
#Save the results

# Now save the rasters for each subsector
for(total in res_totals){
  if(Use_ACES){
    save_data(aces_res_ch4_bystate[[total]])
    save_data(aces_res_ch4_bydomain[[total]])
  }
  if(Use_Vulcan){
    save_data(vu_res_ch4_bystate[[total]])
    save_data(vu_res_ch4_bydomain[[total]])
  }
}

for(total in com_totals){
  if(Use_ACES){
    save_data(aces_com_ch4_bystate[[total]])
    save_data(aces_com_ch4_bydomain[[total]])
  }
  if(Use_Vulcan){
    save_data(vu_com_ch4_bystate[[total]])
    save_data(vu_com_ch4_bydomain[[total]])
  }
}

for(total in ind_totals){
  if(Use_ACES){
    save_data(aces_ind_ch4_bystate[[total]])
    save_data(aces_ind_ch4_bydomain[[total]])
  }
  if(Use_Vulcan){
    save_data(vu_ind_ch4_bystate[[total]])
    save_data(vu_ind_ch4_bydomain[[total]])
  }
}

for(total in elec_totals){
  if(Use_ACES){
    save_data(aces_elec_ch4_bystate[[total]])
    save_data(aces_elec_ch4_bydomain[[total]])
  }
  if(Use_Vulcan){
    save_data(vu_elec_ch4_bystate[[total]])
    save_data(vu_elec_ch4_bydomain[[total]])
  }
}

################################################################################
# Some sanity checks

res_data_objects <- as.list(ls(pattern=glob2rx("*_res_ch4*")))
#find all processed residential files
res_data_length <- length(res_data_objects)
res_data_list <- sapply(as.list(res_data_objects),get)
#get the length, convert from a list of the names to the actual rasters
res_data <- as.data.frame(matrix(sapply(res_data_list,cellStats,sum),
                                 ncol=res_data_length))
#get the domain total for each raster, put into an organized df
names(res_data) <- gsub("res_ch4","by",unlist(res_data_objects))
rownames(res_data) <- names(res_data_list[,1])
#properly name it's dimensions

com_data_objects <- as.list(ls(pattern=glob2rx("*_com_ch4*")))
com_data_length <- length(com_data_objects)
com_data_list <- sapply(as.list(com_data_objects),get)
com_data <- as.data.frame(matrix(sapply(com_data_list,cellStats,sum),
                                 ncol=com_data_length))
names(com_data) <- gsub("com_ch4","by",unlist(com_data_objects))
rownames(com_data) <- names(com_data_list[,1])

ind_data_objects <- as.list(ls(pattern=glob2rx("*_ind_ch4*")))
ind_data_length <- length(ind_data_objects)
ind_data_list <- sapply(as.list(ind_data_objects),get)
ind_data <- as.data.frame(matrix(sapply(ind_data_list,cellStats,sum),
                                 ncol=ind_data_length))
names(ind_data) <- gsub("ind_ch4","by",unlist(ind_data_objects))
rownames(ind_data) <- names(ind_data_list[,1])

elec_data_objects <- as.list(ls(pattern=glob2rx("*_elec_ch4*")))
elec_data_length <- length(elec_data_objects)
elec_data_list <- sapply(as.list(elec_data_objects),get)
elec_data <- as.data.frame(matrix(sapply(elec_data_list,cellStats,sum),
                                  ncol=elec_data_length))
names(elec_data) <- gsub("elec_ch4","by",unlist(elec_data_objects))
rownames(elec_data) <- names(elec_data_list[,1])

ch4_totals_df <- rbind(com_data,elec_data,ind_data,res_data)


input_totals <- st_drop_geometry(all_merge_sf_LCC[,grep(glob2rx("county_ch4_emiss*"),colnames(all_merge_sf_LCC))])
input_totals <- colSums(input_totals)
input_totals_state <- input_totals[grep("bystate",names(input_totals))]
input_totals_domain <- input_totals[grep("bydomain",names(input_totals))]
names(input_totals_state) <- gsub("county_ch4_emiss_bystate.","",names(input_totals_state))
names(input_totals_domain) <- gsub("county_ch4_emiss_bydomain.","",names(input_totals_domain))
#original data that was distributed in the rasters.  The totals should still
#match.

ch4_totals_df <- data.frame(ch4_totals_df,
                            "bystate_input"=input_totals_state[rownames(ch4_totals_df)],
                            "bydomain_input"=input_totals_domain[rownames(ch4_totals_df)])

if(!all(as.vector(round(ch4_totals_df,7)==round(ch4_totals_df[,1],7)))){
  #compare every column to the first, rounded to 7 digits.  All values should be
  #true
  View(ch4_totals_df)
  stop("Domain totals differ when distributed using a different inventory or distributing at the state vs domain level")
}

