## NG_distribution_emissions_r3.R
## In use: 2022-01-28 17:00
#
# Spatially allocate the various NG distribution (and residential post-meter) emission subsectors
# using sectoral CO2 emissions from either Vulcan or ACES as a spatial proxy.
# For both Vulcan and ACES, produce three maps by disaggregating emissions from the:
#     - individual company total
#     - state total
#     - domain total
################################################################################
#User input

plot_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Figures_rewrite/NG_dist_intercomparison"

input_directory="G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw_data_rewrite/"

#Note the code assumes the filetypes are as above (all xlsx or xls, except HIFLD
#if not calculating by LDC)

#all 4 files (HIFLD, PHMSA, EIA, GHGRP) were edited to ensure the ID's were
#consistent and named as below if calculating by LDC

# HIFLD=https://hifld-geoplatform.opendata.arcgis.com/datasets/geoplatform::natural-gas-service-territories/explore?location=38.521197%2C-86.048965%2C7.00
# PHMSA=https://www.phmsa.dot.gov/data-and-statistics/pipeline/gas-distribution-gas-gathering-gas-transmission-hazardous-liquids
# EIA=https://www.eia.gov/naturalgas/ngqs/#?report=RP4&year1=2020&year2=2020&company=Name
# GHGRP=https://ghgdata.epa.gov/ghgp/main.do

EPA_file <- file.path(input_directory,"2022_ghgi_natural_gas_systems_annex36_tables.xlsx")
EPA_EF_sheet <- "3.6-2"
EPA_Activity_sheet <- "3.6-7"
#which sheets are the needed ones in the EPA file.  We want Average CH4 Emission
#Factors (kg/unit activity) for Natural Gas Systems Sources, for All Years
#AND Activity Data for Natural Gas Systems Sources, for All Years

# EPA=https://www.epa.gov/ghgemissions/natural-gas-and-petroleum-systems-ghg-inventory-additional-information-1990-2020-ghg

#Several emission factors (meter and regulating stations, services, meters, and
#maintenance) are pulled from the EPA file in a section on line 420.

inventory_year=2019
domain=as.data.frame(cbind(c(-76.65,-73.65),
                           c(38.97,40.97)))
domain_res=0.01
domain_crs="epsg:4326" #lat/long
ACES_year <- 2017

#year of Vulcan data.  Assuming Vulcan v3.0, 1 - 6 corresponding to years 2010 -
#2015
vulcan_band <- 6

Census_filenames <- c(paste0(input_directory,"State_Tigerlines/tl_",inventory_year,"_us_state.shp"),
                      paste0(input_directory,"County_Tigerlines/tl_",inventory_year,"_us_county.shp"))

ACES_directory="G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Inventories/ACES_v2.0"
vulcan_directory="G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Inventories/Vulcan_v3.0"

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
state_name_list <- sort(c("NJ","NY","PA","MD","DE"))

#Important manual notes - 

#line 210 is a section to manually compare the HIFLD shapefile to the GHGRP one
#to identify any changes needed

#line 373 also has a manually defined value for a single PA utility (UGI
#Utilities) that should be removed if no longer relevant

#line 860 has manual adjustments for several LDCs in NY and PA that had
#different shapefiles in HFILD and the GHGRP.  These should be commented out if
#no longer relevant.

################################################################################
#load packages
i <- 1
packagecheck <- c("raster","ncdf4","readxl","terra","jsonlite","dplyr","sp","sf","fBasics")
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
#no way to compare byLDC prep as it was previously completely manual other than
#shapefile edits which were copy pasted (not even changed to terra due to
#issues).  Just comparing the result of byLDC prep to be sure the changes are
#consistent.

#first compare the manually edited files
#old
{
  HIFLD_file <- file.path(dirname(input_directory),"/Raw Data/Natural__Gas__Service__Territories_edit.xlsx")
  EIA_file <- file.path(dirname(input_directory),"/Raw Data/176 Type of Operations and Sector Items_edit.xlsx")
  PHMSA_file <- file.path(dirname(input_directory),"/Raw Data/annual_gas_distribution_2010_present/annual_gas_distribution_2019_edit.xlsx")
  GHGRP_file <- file.path(dirname(input_directory),"/Raw Data/US_GHGRP_NG_Local_Distribution_Companies_only_all_years_edit.xls")
  
  HIFLD_csv <- read_xlsx(HIFLD_file,col_names = T)
  # Load in the HIFLD csv file containing the clean company IDs that we'll use for cross-referencing
  EIA_csv <- read_xlsx(EIA_file,skip=1,col_names = T)
  # Load the EIA company-level data for 2019 - this may have been edited to add a
  # dummy 'OTHER' entry
  PHMSA_csv <- read_xlsx(PHMSA_file,skip=2,col_names = T)
  # Load the PHMSA data for 2019 - this file may have had company ID's edited to
  # be consistent with EIA for the states that we will use
  GHGRP_csv <- read_xls(GHGRP_file,sheet=as.character(inventory_year),col_names = T,skip = 5)
  # Load the GHGRP csv file - this file may have had company ID added to it for
  # the states that we will use
  
  PHMSA_csv_NG <- PHMSA_csv[which(PHMSA_csv$COMMODITY == 'Natural Gas'),]
  # Filter the PHMSA file by commodity
  
  PHMSA_csv_NG <- PHMSA_csv_NG[which(PHMSA_csv_NG$STOP%in%state_name_list),]
  GHGRP_csv <- GHGRP_csv[which(GHGRP_csv$STATE%in%state_name_list),]
  HIFLD_csv <- HIFLD_csv[HIFLD_csv$LDC_STATE %in% state_name_list | HIFLD_csv$COMPID=="OTHER",]
  HIFLD_csv <- HIFLD_csv[HIFLD_csv$COMPID!="NOT AVAILABLE",]
  HIFLD_csv <- HIFLD_csv[!is.na(HIFLD_csv$COMPID),]
}
#new
{ 
  EIA_csv_new <- read_excel(file.path(input_directory,"ByLDC_EIA_176_type_of_operations.xlsx"))
  PHMSA_csv_NG_new <- read_excel(file.path(input_directory,"ByLDC_PHMSA_annual_gas_distribution.xlsx"))
  GHGRP_csv_new <- read_excel(file.path(input_directory,"ByLDC_GHGRP.xlsx"))
  HIFLD_csv_new <- read_excel(file.path(input_directory,"ByLDC_HIFLD_natural_gas_service_territories.xlsx"))
}

#HIFLD new did this; LDC state = operations, state = HQ.  
HIFLD_csv$STATE <- HIFLD_csv$LDC_STATE

#align rows and columns
HIFLD_csv_new <- HIFLD_csv_new[!is.na(HIFLD_csv_new$COMPID),]
HIFLD_csv_new <- HIFLD_csv_new[order(HIFLD_csv_new$COMPID),1:5]
HIFLD_csv <- HIFLD_csv[order(HIFLD_csv$COMPID),which(colnames(HIFLD_csv) %in% colnames(HIFLD_csv_new))]
HIFLD_csv_new <- HIFLD_csv_new[,sort(colnames(HIFLD_csv_new))]
HIFLD_csv <- HIFLD_csv[,sort(colnames(HIFLD_csv))]

#all match 
# View(HIFLD_csv == HIFLD_csv_new)

#this 1 is NA, it's just to allow matching to those without a match
HIFLD_csv[c(47),]
HIFLD_csv_new[c(47),]

#the remapped national grid is labelled slightly differently, but matches the
#code that maps it, so no issue.  just happened to name them in a different
#order.
HIFLD_csv_new[c(3,16,36),]
HIFLD_csv[c(3,16,36),]



#align rows and columns
EIA_csv_new <- EIA_csv_new[order(EIA_csv_new$Company),]
EIA_csv <- EIA_csv[order(EIA_csv$Company),]
EIA_csv_new <- EIA_csv_new[,sort(colnames(EIA_csv_new))]
EIA_csv <- EIA_csv[,sort(colnames(EIA_csv))]
EIA_csv <- EIA_csv[EIA_csv$State %in% c("DUMMY",state_name_list),]

#all match 
# View((EIA_csv == EIA_csv_new)[,c("Commercial Total Customers","Commercial Total Volume (Mcf)",
#                                  "Company","Company Name",
#                                  "Electric Total Customers","Electric Total Volume (Mcf)",
#                                  "Industrial Total Customers","Industrial Total Volume (Mcf)",
#                                  "Residential Total Customers","Residential Total Volume (Mcf)")])

#Several are NA, that's just from the raw data
EIA_csv[c(110:120),c("Commercial Total Customers","Commercial Total Volume (Mcf)",
                     "Company","Company Name",
                     "Electric Total Customers","Electric Total Volume (Mcf)",
                     "Industrial Total Customers","Industrial Total Volume (Mcf)",
                     "Residential Total Customers","Residential Total Volume (Mcf)")]
EIA_csv_new[c(110:120),c("Commercial Total Customers","Commercial Total Volume (Mcf)",
                    "Company","Company Name",
                    "Electric Total Customers","Electric Total Volume (Mcf)",
                    "Industrial Total Customers","Industrial Total Volume (Mcf)",
                    "Residential Total Customers","Residential Total Volume (Mcf)")]






relevant_cols <- c("State","Company_ID","OPERATOR_NAME","REPORT_YEAR",'MMILES_STEEL_UNP_BARE',
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
                   "AVERAGE_LENGTH")
#align rows and columns
colnames(PHMSA_csv_NG) <- gsub("STOP","State",colnames(PHMSA_csv_NG))
PHMSA_csv_NG_new <- PHMSA_csv_NG_new[order(PHMSA_csv_NG_new$Company_ID,PHMSA_csv_NG_new$MMILES_TOTAL),relevant_cols]
PHMSA_csv_NG <- PHMSA_csv_NG[order(PHMSA_csv_NG$Company_ID,PHMSA_csv_NG$MMILES_TOTAL),relevant_cols]
PHMSA_csv_NG_new <- PHMSA_csv_NG_new[,sort(colnames(PHMSA_csv_NG_new))]
PHMSA_csv_NG <- PHMSA_csv_NG[,sort(colnames(PHMSA_csv_NG))]

#all match other than NAs
# View((PHMSA_csv_NG == PHMSA_csv_NG_new)[,relevant_cols])









#ID left blank in new, not in old
GHGRP_csv_new[GHGRP_csv_new$facility_name=="UGI Utilities, Inc.","Company ID"] <- "17614088PA"

#align rows and columns
GHGRP_csv_new <- GHGRP_csv_new[order(GHGRP_csv_new$`Company ID`),]
GHGRP_csv <- GHGRP_csv[order(GHGRP_csv$`Company ID`),]
GHGRP_csv_new <- GHGRP_csv_new[,sort(colnames(GHGRP_csv_new))]
GHGRP_csv <- GHGRP_csv[,sort(colnames(GHGRP_csv))]
GHGRP_csv_new <- GHGRP_csv_new[,c("city","Company ID","county","facility_name",
                                  "facility_id","latitude","longitude","parent_company",
                                  "address1","reporting_year","state","reported_subparts","zip")]
GHGRP_csv <- GHGRP_csv[,colnames(GHGRP_csv)!="GHG QUANTITY (METRIC TONS CO2e)"]

#all match 
# View((GHGRP_csv == GHGRP_csv_new))
# plot(GHGRP_csv$LATITUDE - GHGRP_csv_new$latitude)
# plot(GHGRP_csv$LONGITUDE - GHGRP_csv_new$longitude)

#I updated the state for 2 to be where they operate, no impact on how it runs
#(uses different input for state)
GHGRP_csv[c(19:20),]
GHGRP_csv_new[c(19:20),]

#zip codes include leading zeroes in new.  Unused variable anyway.
GHGRP_csv[c(15:17),"ZIP CODE"]
GHGRP_csv_new[c(15:17),"zip"]

################################################################################
#now that the inputs have been compared, compare the output of byLDC

#old - reorganized from this to make the by ldc or not by ldc far
#cleaner/simpler in new
{
  HIFLD_shapefile <- file.path(dirname(input_directory),"/Raw Data/Natural__Gas__Service__Territories/NG_Service_Terr.shp")
  HIFLD_file <- file.path(dirname(input_directory),"/Raw Data/Natural__Gas__Service__Territories_edit.xlsx")
  EIA_file <- file.path(dirname(input_directory),"/Raw Data/176 Type of Operations and Sector Items_edit.xlsx")
  PHMSA_file <- file.path(dirname(input_directory),"/Raw Data/annual_gas_distribution_2010_present/annual_gas_distribution_2019_edit.xlsx")
  GHGRP_file <- file.path(dirname(input_directory),"/Raw Data/US_GHGRP_NG_Local_Distribution_Companies_only_all_years_edit.xls")
  
  Cartographic_boundary_file <- "G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Shapefiles/cb_2021_us_state_500k/cb_2021_us_state_500k.shp"
  #Slightly different census outlines for states, excluding water areas
  
  ################################################################################
  #load in and filter the various files, excluding the EPA one for now
  
  HIFLD_shp <- st_read(HIFLD_shapefile)
  # Load in HIFLD shapefile containing the LDC service territories
  HIFLD_csv <- read_xlsx(HIFLD_file,col_names = T)
  # Load in the HIFLD csv file containing the clean company IDs that we'll use for cross-referencing
  EIA_csv <- read_xlsx(EIA_file,skip=1,col_names = T)
  # Load the EIA company-level data for 2019 - this may have been edited to add a
  # dummy 'OTHER' entry
  PHMSA_csv <- read_xlsx(PHMSA_file,skip=2,col_names = T)
  # Load the PHMSA data for 2019 - this file may have had company ID's edited to
  # be consistent with EIA for the states that we will use
  GHGRP_csv <- read_xls(GHGRP_file,sheet=as.character(inventory_year),col_names = T,skip = 5)
  # Load the GHGRP csv file - this file may have had company ID added to it for
  # the states that we will use
  
  PHMSA_csv_NG <- PHMSA_csv[which(PHMSA_csv$COMMODITY == 'Natural Gas'),]
  # Filter the PHMSA file by commodity
  
  PHMSA_csv_NG <- PHMSA_csv_NG[which(PHMSA_csv_NG$STOP%in%state_name_list),]
  GHGRP_csv <- GHGRP_csv[which(GHGRP_csv$STATE%in%state_name_list),]
  HIFLD_csv <- HIFLD_csv[HIFLD_csv$LDC_STATE %in% state_name_list | HIFLD_csv$COMPID=="OTHER",]
  HIFLD_csv <- HIFLD_csv[HIFLD_csv$COMPID!="NOT AVAILABLE",]
  HIFLD_csv <- HIFLD_csv[!is.na(HIFLD_csv$COMPID),]
  #filter to only those for the relevant states and those with a company ID in
  #HIFLD (present at all).
  
  HIFLD_check <- substr(HIFLD_csv$COMPID,start = nchar(HIFLD_csv$COMPID)-1,stop = nchar(HIFLD_csv$COMPID))
  for(A in 1:length(HIFLD_check)){
    if(sapply(gregexpr("[[:alpha:]]",HIFLD_check),FUN=function(x){x==-1}[1])[A]){
      HIFLD_csv[A,"COMPID"] <- paste0(HIFLD_csv[A,"COMPID"],HIFLD_csv[A,"LDC_STATE"])
    }
  }
  #add the LDC state abbreviation if the user hasn't done so manually (irrelevant
  #if not calculating by LDC)
  
  rm(PHMSA_csv,EIA_file,GHGRP_file,HIFLD_check,HIFLD_file,HIFLD_shapefile,PHMSA_file,
     A)
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
        download.file(paste0("https://ghgdata.epa.gov/ghgp/service/html/",inventory_year,"?id=",GHGRP_csv$`GHGRP ID`[A],"&et=undefined"),
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
        stop("Failed to download ",GHGRP_csv$`FACILITY NAME`[A]," data from\n",
             paste0("https://ghgdata.epa.gov/ghgp/service/html/",inventory_year,"?id=",GHGRP_csv$`GHGRP ID`[A],"&et=undefined\n"),
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
    
    cat("\rFinished downloading GHGRP data for",A,"of",nrow(GHGRP_csv),"                 ")
    #user update
  }
  
  #attempt to remove the downloaded html file
  unlink(download_dest)
  
  GHGRP_csv$'Miles_of_Mains(PHMSA)' <- sapply(GHGRP_csv$`Company ID`,
                                              FUN=function(x){sum(PHMSA_csv_NG$MMILES_TOTAL[which(x==PHMSA_csv_NG$Company_ID)])})
  #copy the corresponding PHMSA total miles to the GHGRP file for comparison and
  #simpler calculations
  GHGRP_csv$'Miles_of_Mains(PHMSA)'[GHGRP_csv$`FACILITY NAME`=="UGI Utilities, Inc."] <- 12028
  #manually correct this one.  It's set to other in PHMSA as it corresponds to a
  #few facilities, but also varying shapes depending on the datasource.
  
  GHGRP_csv$above_grade_stations_per_mile_of_pipe <- (GHGRP_csv$'N_of_above_grade_T-D_transfer_stations'+
                                                        GHGRP_csv$'N_of_above_grade_non_T-D_MR_stations')/
    GHGRP_csv$`Miles_of_Mains(PHMSA)`
  GHGRP_csv$below_grade_stations_per_mile_of_pipe <- (GHGRP_csv$'N_of_below_grade_T-D_transfer_stations'+
                                                        GHGRP_csv$'N_of_below_grade_non_T-D_MR_stations')/
    GHGRP_csv$`Miles_of_Mains(PHMSA)`
  GHGRP_csv$stations_per_mile_of_pipe <- (GHGRP_csv$'N_of_above_grade_T-D_transfer_stations'+
                                            GHGRP_csv$'N_of_above_grade_non_T-D_MR_stations'+
                                            GHGRP_csv$'N_of_below_grade_T-D_transfer_stations'+
                                            GHGRP_csv$'N_of_below_grade_non_T-D_MR_stations')/
    GHGRP_csv$`Miles_of_Mains(PHMSA)`
  #calculate a few ratios
  
  GHGRP_PHMSA_comparison <- abs(GHGRP_csv$'Miles_of_Mains(PHMSA)' - GHGRP_csv$Miles_of_Mains)/mean(c(GHGRP_csv$'Miles_of_Mains(PHMSA)',GHGRP_csv$Miles_of_Mains))*100
  if(max(GHGRP_PHMSA_comparison)>5){
    View(GHGRP_csv[GHGRP_PHMSA_comparison>5,c("FACILITY NAME","Miles_of_Mains","Miles_of_Mains(PHMSA)")])
    stop("Double check the GHGRP facilities:\n",paste(GHGRP_csv$`FACILITY NAME`[GHGRP_PHMSA_comparison>5],collapse = "\n"),"\n\nas the miles of mains was >5% different than the corresponding PHMSA facility.  One of them is likely wrong.")
  }
  #user update check - PHMSA and GHGRP should agree very well.  Any that differ a
  #lot could be due to mislabeling.
  rm(GHGRP_PHMSA_comparison)
  ################################################################################
  #Pull the EPA data we'll need later and save it to a few dataframes
  
  first_col <- which(read_xlsx(EPA_file,sheet = EPA_Activity_sheet,.name_repair = "minimal")[,1]=="Segment/Source")
  EPA_p1 <- read_xlsx(EPA_file,sheet = EPA_Activity_sheet,skip=first_col,col_names = T)
  
  first_col <- which(read_xlsx(EPA_file,sheet = EPA_EF_sheet,.name_repair = "minimal")[,1]=="Segment/Source")
  EPA_p2 <- read_xlsx(EPA_file,sheet = EPA_EF_sheet,skip=first_col,col_names = T)
  #p2 = emission factors, p1 = activity data.  Columns = year, rows = various
  #types of sources.  First col is just to identify the first column of useable
  #data
  
  Data_list <- c("M&R >300","M&R 100-300","M&R <100","Reg >300","R-Vault >300",
                 "Reg 100-300","R-Vault 100-300","Reg 40-100","R-Vault 40-100",
                 "Reg <40")
  #all the sources we're looking for, written exactly as in the EPA file
  
  EPA_MnR <- data.frame("Type"=Data_list,
                        "EF"=as.numeric(unlist(sapply(Data_list,FUN=function(x){EPA_p2[EPA_p2[,1]==x,as.character(inventory_year)]})))*
                          1000/(16.043*60*60*24*365),#convert from kg/yr to mol/s
                        "Total_stations"=as.numeric(unlist(sapply(Data_list,FUN=function(x){EPA_p1[EPA_p1[,1]==x,as.character(inventory_year)]}))),
                        row.names = NULL)
  #use sapply to find the row using data list, specify the column as the year and
  #grab the relevant EF and activity data into a dataframe.
  
  #repeat for several other source types
  Data_list <- c("Services - Unprotected steel",
                 "Services Protected steel",
                 "Services - Plastic",
                 "Services - Copper")
  
  EPA_Services <- data.frame("Type"=Data_list,
                             "EF"=as.numeric(unlist(sapply(Data_list,FUN=function(x){EPA_p2[EPA_p2[,1]==x,as.character(inventory_year)]})))*
                               1000/(16.043*60*60*24*365),#convert from kg/yr to mol/s
                             row.names = NULL)
  
  Data_list <- c("Residential",
                 "Commercial",
                 "Industrial")
  
  EPA_meters <- data.frame("Type"=Data_list,
                           "EF"=as.numeric(unlist(sapply(Data_list,FUN=function(x){EPA_p2[which(EPA_p2[,1]==x)[1],as.character(inventory_year)]})))*
                             1000/(16.043*60*60*24*365),#convert from kg/yr to mol/s
                           row.names = NULL)
  
  Data_list <- c("Pressure Relief Valve Releases",
                 "Pipeline Blowdown",
                 "Mishaps (Dig-ins)")
  
  EPA_maintenance <- data.frame("Type"=Data_list,
                                "EF"=as.numeric(unlist(sapply(Data_list,FUN=function(x){EPA_p2[EPA_p2[,1]==x,as.character(inventory_year)]})))*
                                  1000/(16.043*60*60*24*365),#convert from kg/yr to mol/s
                                row.names = NULL)
  
  Data_list <- c("Pressure Relief Valve Releases",
                 "Pipeline Blowdown",
                 "Mishaps (Dig-ins)")
  
  EPA_maintenance <- data.frame("Type"=Data_list,
                                "EF"=as.numeric(unlist(sapply(Data_list,FUN=function(x){EPA_p2[EPA_p2[,1]==x,as.character(inventory_year)]})))*
                                  1000/(16.043*60*60*24*365),#convert from kg/yr to mol/s
                                row.names = NULL)
  
  rm(EPA_p1,EPA_p2,Data_list,first_col)
  ################################################################################
  ## Calculate emissions (all in mol/s):
  
  PHMSA_csv_NG$bare_steel_mains_ER <- (rowSums(PHMSA_csv_NG[,c("MMILES_STEEL_UNP_BARE","MMILES_STEEL_CP_BARE","MMILES_CU")],
                                               na.rm=T)*
                                         rowProds(GHGI_natural_gas_pipeline_emission_factors)[1])
  PHMSA_csv_NG$iron_mains_ER <- (rowSums(PHMSA_csv_NG[,c("MMILES_CI","MMILES_DI","MMILES_RCI")],
                                         na.rm=T)*
                                   rowProds(GHGI_natural_gas_pipeline_emission_factors)[2])
  PHMSA_csv_NG$coat_steel_mains_ER <- (rowSums(PHMSA_csv_NG[,c("MMILES_STEEL_UNP_COATED","MMILES_STEEL_CP_COATED","MMILES_OTHER")],
                                               na.rm=T)*
                                         rowProds(GHGI_natural_gas_pipeline_emission_factors)[3])
  PHMSA_csv_NG$plastic_mains_ER <- (PHMSA_csv_NG$MMILES_PLASTIC*
                                      rowProds(GHGI_natural_gas_pipeline_emission_factors)[4])
  #Mains using EFs from Weller et al., or as specified at the top of the code
  
  PHMSA_csv_NG$UNP_steel_serv_ER <- (rowSums(PHMSA_csv_NG[,c("NUM_SRVS_STEEL_UNP_COATED","NUM_SRVS_STEEL_UNP_BARE")],
                                             na.rm=T)*
                                       EPA_Services$EF[1])
  PHMSA_csv_NG$CP_steel_serv_ER <- (rowSums(PHMSA_csv_NG[,c("NUM_SRVS_STEEL_CP_BARE","NUM_SRVS_STEEL_CP_COATED","NUM_SRVS_OTHER")],
                                            na.rm=T)*
                                      EPA_Services$EF[2])
  PHMSA_csv_NG$plastic_serv_ER <- (PHMSA_csv_NG$NUM_SRVS_PLASTIC*
                                     EPA_Services$EF[3])
  PHMSA_csv_NG$copper_serv_ER <- (rowSums(PHMSA_csv_NG[,c("NUM_SRVS_CU","NUM_SRVS_CI","NUM_SRVS_DI","NUM_SRVS_RCI")],
                                          na.rm=T)*
                                    EPA_Services$EF[4])
  # Services using EFs from the EPA national inventory report
  
  # M&R stations - can't use GHGRP data without matching facilities, so estimate
  # based on avg stations per mile for reporters in each state. Then split by
  # pressure and function assuming the same split as at the national level (from
  # the EPA national inventory report).
  
  main_miles_ghgrp <- aggregate(GHGRP_csv$Miles_of_Mains,
                                list(State=GHGRP_csv$STATE),
                                sum,
                                na.rm=TRUE)
  above_grade_MnR <- aggregate((GHGRP_csv$`N_of_above_grade_T-D_transfer_stations` +
                                  GHGRP_csv$`N_of_above_grade_non_T-D_MR_stations`),
                               list(State=GHGRP_csv$STATE),
                               sum,
                               na.rm=TRUE)
  below_grade_MnR <- aggregate((GHGRP_csv$`N_of_below_grade_non_T-D_MR_stations` +
                                  GHGRP_csv$`N_of_below_grade_T-D_transfer_stations`),
                               list(State=GHGRP_csv$STATE),
                               sum,
                               na.rm=TRUE)
  above_grade_MnR$stations_per_mile <- above_grade_MnR$x/main_miles_ghgrp$x
  below_grade_MnR$stations_per_mile <- below_grade_MnR$x/main_miles_ghgrp$x
  # Calculate average stations per mile in each state
  
  ################################################################################
  #prep to merge the many files, excluding the EPA for now, calculate a few
  #additional variables
  
  PHMSA_csv_NG$Miles_main_and_serv <- PHMSA_csv_NG$MMILES_TOTAL + PHMSA_csv_NG$NUM_SRVCS_TOTAL*PHMSA_csv_NG$AVERAGE_LENGTH/5280
  # We're going to need the total miles of pipeline (inc. services) later -
  # calculate that here from AVERAGE_LENGTH (in ft)
  
  # Then select the columns we need and aggregate the entries which share the same company ID or state
  PHMSA_cols_to_keep <- c('MMILES_STEEL_UNP_BARE',
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
                          'Miles_main_and_serv',
                          "bare_steel_mains_ER",
                          "iron_mains_ER",
                          "coat_steel_mains_ER",
                          "plastic_mains_ER",
                          "UNP_steel_serv_ER",
                          "CP_steel_serv_ER",
                          "plastic_serv_ER",
                          "copper_serv_ER")
  
  
  EIA_cols_to_keep <- c("Residential Total Volume (Mcf)",
                        "Residential Total Customers",
                        'Commercial Total Volume (Mcf)',
                        'Commercial Total Customers',
                        'Industrial Total Volume (Mcf)',
                        'Industrial Total Customers',
                        'Electric Total Volume (Mcf)',
                        'Electric Total Customers')
  
  cols_to_keep <- c('Company',
                    'Company Name',
                    'SVCTERID',
                    'State',
                    EIA_cols_to_keep,
                    PHMSA_cols_to_keep,
                    'MMiles_PHMSA_GHGRP',
                    'MnR_above',
                    'MnR_below')
  #No HIFLD or GHGRP data if not calculating by LDC
  
  PHMSA_csv_NG_agg <- aggregate(PHMSA_csv_NG[PHMSA_cols_to_keep],
                                list(COMPANY_ID=PHMSA_csv_NG$Company_ID,
                                     STOP=PHMSA_csv_NG$STOP),
                                sum,na.rm=T)
  #combine by ID
  
  
  EIA_PHMSA_merge <- merge(EIA_csv, PHMSA_csv_NG_agg, by.x='Company', by.y='COMPANY_ID')
  EIA_PHMSA_HIFLD_merge <- merge(EIA_PHMSA_merge, HIFLD_csv, by.x='Company', by.y='COMPID')
  all_merge <- merge(EIA_PHMSA_HIFLD_merge, GHGRP_csv, by.x='Company', by.y='Company ID', all.x=TRUE)
  # Now merge csv stuff together
  
  all_merge$State <- all_merge$STOP  # rename for clarity
  all_merge$MMiles_PHMSA_GHGRP <- all_merge$`Miles_of_Mains(PHMSA)`# Essentially the same as MMILES_TOTAL, but slightly different due to the way we've combined some PHMSA entries
  all_merge$MnR_above <- all_merge$`N_of_above_grade_T-D_transfer_stations`+ all_merge$`N_of_above_grade_non_T-D_MR_stations`
  all_merge$MnR_below <- all_merge$`N_of_below_grade_T-D_transfer_stations` + all_merge$`N_of_below_grade_non_T-D_MR_stations`
  # Clean up
  
  all_merge_clean <- all_merge[cols_to_keep]
  
  EIA_state_totals <- aggregate(EIA_csv[EIA_cols_to_keep],
                                list(State=EIA_csv$State),
                                sum,
                                na.rm = TRUE)
  # Calculate residual EIA values from state totals
  
  EIA_merge_state_totals <-  aggregate(all_merge_clean[EIA_cols_to_keep],
                                       list(State=all_merge_clean$State),
                                       sum,
                                       na.rm = TRUE)
  
  for(a_state in unique(all_merge_clean$State)){
    residuals <- (EIA_state_totals[which(EIA_state_totals$State == a_state),-1] -
                    EIA_merge_state_totals[which(EIA_merge_state_totals$State == a_state),-1])
    all_merge_clean[which(all_merge_clean$Company == 'OTHER' & all_merge_clean$State == a_state), EIA_cols_to_keep] <- residuals
  }
  # Loop through states and assign residual EIA values to OTHER
  
  # M&R stations - can use GHGRP data for those stations that report, otherwise estimate based on avg stations per mile
  # for reporters in each state. Then split by pressure and function assuming the same split as at the national level
  # (from the EPA national inventory report).
  
  # Use the original GHGRP_csv df here - it includes UGI data in PA, which we had to exclude from all_merge_clean because there
  # was no good shapefile for it, but the underlying activity data is fine.
  # Note that for PA this means the average stations_per_mile value for reporters included here does not equal the default
  # stations_per_mile value assigned to non-reporters below.
  main_miles_ghgrp <- aggregate(GHGRP_csv$`Miles_of_Mains(PHMSA)`,
                                list(State=GHGRP_csv$STATE),
                                sum,
                                na.rm=TRUE)
  above_grade_MnR <- aggregate((GHGRP_csv$`N_of_above_grade_T-D_transfer_stations` +
                                  GHGRP_csv$`N_of_above_grade_non_T-D_MR_stations`),
                               list(State=GHGRP_csv$STATE),
                               sum,
                               na.rm=TRUE)
  below_grade_MnR <- aggregate((GHGRP_csv$`N_of_below_grade_non_T-D_MR_stations` +
                                  GHGRP_csv$`N_of_below_grade_T-D_transfer_stations`),
                               list(State=GHGRP_csv$STATE),
                               sum,
                               na.rm=TRUE)
  
  above_grade_MnR$stations_per_mile <- above_grade_MnR$x/main_miles_ghgrp$x
  below_grade_MnR$stations_per_mile <- below_grade_MnR$x/main_miles_ghgrp$x
  # Calculate average stations per mile in each state
  
  non_ghgrp_indx <- which(is.na(all_merge_clean$MnR_above))
  non_ghgrp_state <- all_merge_clean$State[non_ghgrp_indx]
  state_indx <- match(non_ghgrp_state,above_grade_MnR$State)
  all_merge_clean$MnR_above[non_ghgrp_indx] <- all_merge_clean$MMILES_TOTAL[non_ghgrp_indx]*above_grade_MnR$stations_per_mile[state_indx]
  all_merge_clean$MnR_below[non_ghgrp_indx] <- all_merge_clean$MMILES_TOTAL[non_ghgrp_indx]*below_grade_MnR$stations_per_mile[state_indx]
  # Estimate number of stations for non-reporters
  
  ############################################################################
  # Calculate the total mains emissions to be distributed according to residential and commercial CO2 emissions
  # This is calculated for each company according to the ratio of residential:commercial customers
  # Industrial customer numbers are much smaller, so we ignore these here
  all_merge_clean$mains_ER_total_res <- ((all_merge_clean$bare_steel_mains_ER + 
                                            all_merge_clean$iron_mains_ER +
                                            all_merge_clean$coat_steel_mains_ER +
                                            all_merge_clean$plastic_mains_ER)*
                                           all_merge_clean$"Residential Total Customers"/
                                           (all_merge_clean$"Residential Total Customers" + all_merge_clean$"Commercial Total Customers"))
  
  all_merge_clean$mains_ER_total_com <- ((all_merge_clean$bare_steel_mains_ER + 
                                            all_merge_clean$iron_mains_ER +
                                            all_merge_clean$coat_steel_mains_ER +
                                            all_merge_clean$plastic_mains_ER)*
                                           all_merge_clean$"Commercial Total Customers"/
                                           (all_merge_clean$"Residential Total Customers" + all_merge_clean$"Commercial Total Customers"))
  
  all_merge_clean$serv_ER_total_res <- ((all_merge_clean$UNP_steel_serv_ER + 
                                           all_merge_clean$CP_steel_serv_ER +
                                           all_merge_clean$plastic_serv_ER +
                                           all_merge_clean$copper_serv_ER)*
                                          all_merge_clean$"Residential Total Customers"/
                                          (all_merge_clean$"Residential Total Customers" + all_merge_clean$"Commercial Total Customers"))
  
  all_merge_clean$serv_ER_total_com <- ((all_merge_clean$UNP_steel_serv_ER + 
                                           all_merge_clean$CP_steel_serv_ER +
                                           all_merge_clean$plastic_serv_ER +
                                           all_merge_clean$copper_serv_ER)*
                                          all_merge_clean$"Commercial Total Customers"/
                                          (all_merge_clean$"Residential Total Customers" + all_merge_clean$"Commercial Total Customers"))
  
  ################################################################################
  #Calculate a few additional emissions
  
  EPA_MnR_above <- sum(EPA_MnR$Total_stations[-grep('Vault', EPA_MnR$Type)])
  EPA_MnR_below <- sum(EPA_MnR$Total_stations[grep('Vault', EPA_MnR$Type)])
  #split by function/pressure
  
  # Estimate emissions by function/pressure
  all_merge_clean$MnR_HiP_ER <- (all_merge_clean$MnR_above*                                                    # Abv grade stations
                                   EPA_MnR$Total_stations[which(EPA_MnR$Type == 'M&R >300')]/EPA_MnR_above* # Type fraction
                                   EPA_MnR$EF[which(EPA_MnR$Type == 'M&R >300')]) # Emission factor
  
  all_merge_clean$MnR_MidP_ER <- (all_merge_clean$MnR_above*
                                    EPA_MnR$Total_stations[which(EPA_MnR$Type == 'M&R 100-300')]/EPA_MnR_above*
                                    EPA_MnR$EF[which(EPA_MnR$Type == 'M&R 100-300')])
  
  all_merge_clean$MnR_LoP_ER <- (all_merge_clean$MnR_above*
                                   EPA_MnR$Total_stations[which(EPA_MnR$Type == 'M&R <100')]/EPA_MnR_above*
                                   EPA_MnR$EF[which(EPA_MnR$Type == 'M&R <100')])
  
  all_merge_clean$Reg_HiP_ER <- (all_merge_clean$MnR_above*
                                   EPA_MnR$Total_stations[which(EPA_MnR$Type == 'Reg >300')]/EPA_MnR_above*
                                   EPA_MnR$EF[which(EPA_MnR$Type == 'Reg >300')])
  
  all_merge_clean$Reg_MidP_ER <- (all_merge_clean$MnR_above*
                                    EPA_MnR$Total_stations[which(EPA_MnR$Type == 'Reg 100-300')]/EPA_MnR_above*
                                    EPA_MnR$EF[which(EPA_MnR$Type == 'Reg 100-300')])
  
  all_merge_clean$Reg_LoP_ER <- (all_merge_clean$MnR_above*
                                   EPA_MnR$Total_stations[which(EPA_MnR$Type == 'Reg 40-100')]/EPA_MnR_above*
                                   EPA_MnR$EF[which(EPA_MnR$Type == 'Reg 40-100')])
  
  all_merge_clean$Reg_VLP_ER <- (all_merge_clean$MnR_above*
                                   EPA_MnR$Total_stations[which(EPA_MnR$Type == 'Reg <40')]/EPA_MnR_above*
                                   EPA_MnR$EF[which(EPA_MnR$Type == 'Reg <40')])
  
  all_merge_clean$RegV_HiP_ER <- (all_merge_clean$MnR_below*
                                    EPA_MnR$Total_stations[which(EPA_MnR$Type == 'R-Vault >300')]/EPA_MnR_below*
                                    EPA_MnR$EF[which(EPA_MnR$Type == 'R-Vault >300')])
  
  all_merge_clean$RegV_MidP_ER <- (all_merge_clean$MnR_below*
                                     EPA_MnR$Total_stations[which(EPA_MnR$Type == 'R-Vault 100-300')]/EPA_MnR_below*
                                     EPA_MnR$EF[which(EPA_MnR$Type == 'R-Vault 100-300')])
  
  all_merge_clean$RegV_LoP_ER <- (all_merge_clean$MnR_below*
                                    EPA_MnR$Total_stations[which(EPA_MnR$Type == 'R-Vault 40-100')]/EPA_MnR_below*
                                    EPA_MnR$EF[which(EPA_MnR$Type == 'R-Vault 40-100')])
  
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
                                         all_merge_clean$"Residential Total Customers"/
                                         (all_merge_clean$"Residential Total Customers" + all_merge_clean$"Commercial Total Customers"))
  
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
                                         all_merge_clean$"Commercial Total Customers"/
                                         (all_merge_clean$"Residential Total Customers" + all_merge_clean$"Commercial Total Customers"))
  
  # Consumer meters - use emission factors from the EPA national inventory report
  all_merge_clean$Res_meter_ER <- all_merge_clean$"Residential Total Customers"*EPA_meters$EF[1]
  all_merge_clean$Com_meter_ER <- all_merge_clean$"Commercial Total Customers"*EPA_meters$EF[2]
  all_merge_clean$Ind_meter_ER <- all_merge_clean$"Industrial Total Customers"*EPA_meters$EF[3]
  
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
  
  # Maintenance and upsets
  all_merge_clean$Relief_valve_ER <- all_merge_clean$MMILES_TOTAL*EPA_maintenance$EF[1]
  all_merge_clean$Blowdown_ER <- all_merge_clean$Miles_main_and_serv*EPA_maintenance$EF[2]
  all_merge_clean$Mishap_ER <- all_merge_clean$Miles_main_and_serv*EPA_maintenance$EF[3]
  
  all_merge_clean$upset_ER_total_res <- ((all_merge_clean$Relief_valve_ER + 
                                            all_merge_clean$Blowdown_ER +
                                            all_merge_clean$Mishap_ER)*
                                           all_merge_clean$"Residential Total Customers"/
                                           (all_merge_clean$"Residential Total Customers" + all_merge_clean$"Commercial Total Customers"))
  
  all_merge_clean$upset_ER_total_com <- ((all_merge_clean$Relief_valve_ER + 
                                            all_merge_clean$Blowdown_ER +
                                            all_merge_clean$Mishap_ER)*
                                           all_merge_clean$"Commercial Total Customers"/
                                           (all_merge_clean$"Residential Total Customers" + all_merge_clean$"Commercial Total Customers"))
  
  # Post-meter
  all_merge_clean$post_meter_ER_total_res <- all_merge_clean$"Residential Total Volume (Mcf)"*1000*natural_gas_post_meter_emission_factor
  #McF = thousand cubic ft
  
  ################################################################################
  #Start working with the shape files if calculating by LDC
  
  ## Now we have the emissions by company as a dataframe
  # Next do some pre-processing of the HIFLD shapefile so we can sptially allocate these emissions:
  # - SVCTERID number LDC360007 needs splitting into three bits based on NYS county polygons
  # - SVCTERID numbers LDC420001 and LDC420022 need merging (these companies merged so the 2019 data is combined)
  
  # Load the county and state shapefiles - use cb file for states because we don't want to include water areas
  county_shp <- st_as_sf(County_Tigerlines)
  state_shp <- st_read(Cartographic_boundary_file)
  
  LI_shp <- st_combine(county_shp[which(county_shp$COUNTYNS %in% c('00974149', '00974128')),])
  NYC_shp <- st_combine(county_shp[which(county_shp$COUNTYNS %in% c('00974122', '00974139', '00974141', '00974129', '00974101')),])
  #combine a few LDCs
  
  state_shp_trans <- st_transform(state_shp, crs(HIFLD_shp))
  LI_shp_trans <- st_transform(LI_shp, crs(HIFLD_shp))
  NYC_shp_trans <- st_transform(NYC_shp, crs(HIFLD_shp))
  # Move everything onto the target crs
  
  NGrid_shp <- HIFLD_shp[which(HIFLD_shp$SVCTERID == 'LDC360007'),]
  NGrid_LI <- st_intersection(NGrid_shp, LI_shp_trans)
  NGrid_NYC <- st_intersection(NGrid_shp, NYC_shp_trans)
  NGrid_other <- st_difference(NGrid_shp, st_union(LI_shp_trans, NYC_shp_trans))
  # # Split up the National Grid LDC polygon
  
  HIFLD_shp <- rbind(HIFLD_shp, HIFLD_shp[rep(which(HIFLD_shp$SVCTERID == 'LDC360007'), 3),])
  # Add new entries for the shapefile containing the new split NGrid polygon
  
  HIFLD_shp$SVCTERID[nrow(HIFLD_shp)-2] <- 'LDC360007a'
  HIFLD_shp$SVCTERID[nrow(HIFLD_shp)-1] <- 'LDC360007b'
  HIFLD_shp$SVCTERID[nrow(HIFLD_shp)] <- 'LDC360007c'
  
  st_geometry(HIFLD_shp[which(HIFLD_shp$SVCTERID == 'LDC360007a'),]) <- st_geometry(NGrid_LI)
  st_geometry(HIFLD_shp[which(HIFLD_shp$SVCTERID == 'LDC360007b'),]) <- st_geometry(NGrid_NYC)
  st_geometry(HIFLD_shp[which(HIFLD_shp$SVCTERID == 'LDC360007c'),]) <- st_geometry(NGrid_other)
  
  PPL_NG_combined <- st_combine(HIFLD_shp[which(HIFLD_shp$SVCTERID %in% c('LDC420001','LDC420022')),])
  st_geometry(HIFLD_shp[which(HIFLD_shp$SVCTERID == 'LDC420001'),]) <- PPL_NG_combined
  # Merge the Peoples Natural Gas LDC geometries
  
  HIFLD_shp[which(HIFLD_shp$SVCTERID=="LDC240007"),] <- st_intersection(HIFLD_shp[which(HIFLD_shp$SVCTERID=="LDC240007"),],
                                                                        state_shp_trans[which(state_shp_trans$STUSPS == "MD"),])[names(HIFLD_shp)]
  #this shapefile bleeds over into VA, even though GHGRP and the other inputs
  #separate the data by state.  Removing the VA portion here
  
  all_merge_with_poly <- merge(all_merge_clean, HIFLD_shp[c('SVCTERID', 'geometry')], all.x=TRUE)
  # Now merge the geometries from HIFLD_shp with the entries in all_merge_clean
  
  all_merge_sf <- st_as_sf(all_merge_with_poly, sf_column_name='geometry', crs=crs(HIFLD_shp))
  # Turn into sf object
  
  for(a_state in unique(all_merge_sf$State)){
    other_indx <- which(all_merge_sf$State == a_state & all_merge_sf$Company == 'OTHER')
    if(length(other_indx)){  # if there is an 'OTHER' entry for this state
      state_poly <- state_shp_trans[which(state_shp_trans$STUSPS == a_state),]
      st_geometry(all_merge_sf[other_indx,]) <- st_geometry(st_difference(state_poly, st_union(all_merge_sf)))
      all_merge_sf[other_indx,'SVCTERID'] <- paste0('DUMMY_', a_state)
    }
  }
  # Go through each state and get the geometry of the OTHER entry for all_merge_clean (i.e. areas not covered by all_merge_sf)
  # Also change SVCTERID from DUMMY to a unique value
  
  
}

#new
{
  
  EIA_file = file.path(input_directory,"176 Type of Operations and Sector Items.xlsx")
  GHGI_file = file.path(input_directory,"2022_ghgi_natural_gas_systems_annex36_tables.xlsx")
  GHGI_EF_sheet = "3.6-2"
  GHGI_Activity_sheet = "3.6-7"
  
  ############################################################################
  #load in the output of NG_distribution_by_LDC_prep.R.  Note that is a
  #script, not a function, as it requires some manual efforts.  This one is
  #calculated at the LDC scale, not state level.
  all_merge_clean_new <- vect(file.path(input_directory,"/byLDC_merged/byLDC_merged.shp"))
  names(all_merge_clean_new) <- unlist(read.table(file.path(input_directory,"/byLDC_merged/colnames.txt")))
  ################################################################################
  #Pull the GHGI data we'll need later and save it to a few dataframes
  
  first_col <- which(read_xlsx(GHGI_file,sheet = GHGI_Activity_sheet,.name_repair = "minimal")[,1]=="Segment/Source")
  GHGI_p1 <- read_xlsx(GHGI_file,sheet = GHGI_Activity_sheet,skip=first_col,col_names = T)
  
  first_col <- which(read_xlsx(GHGI_file,sheet = GHGI_EF_sheet,.name_repair = "minimal")[,1]=="Segment/Source")
  GHGI_p2 <- read_xlsx(GHGI_file,sheet = GHGI_EF_sheet,skip=first_col,col_names = T)
  #p2 = emission factors, p1 = activity data.  Columns = year, rows = various
  #types of sources.  First col is just to identify the first column of useable
  #data
  
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
  
  #repeat for several other source types
  Data_list <- c("Services - Unprotected steel",
                 "Services Protected steel",
                 "Services - Plastic",
                 "Services - Copper")
  
  GHGI_Services <- data.frame("Type"=Data_list,
                              "EF"=as.numeric(unlist(sapply(Data_list,FUN=function(x){GHGI_p2[GHGI_p2[,1]==x,as.character(inventory_year)]})))*
                                1000/(16.043*60*60*24*365),#convert from kg/yr to mol/s
                              row.names = NULL)
  
  Data_list <- c("Residential",
                 "Commercial",
                 "Industrial")
  
  GHGI_meters <- data.frame("Type"=Data_list,
                            "EF"=as.numeric(unlist(sapply(Data_list,FUN=function(x){GHGI_p2[which(GHGI_p2[,1]==x)[1],as.character(inventory_year)]})))*
                              1000/(16.043*60*60*24*365),#convert from kg/yr to mol/s
                            row.names = NULL)
  
  Data_list <- c("Pressure Relief Valve Releases",
                 "Pipeline Blowdown",
                 "Mishaps (Dig-ins)")
  
  GHGI_maintenance <- data.frame("Type"=Data_list,
                                 "EF"=as.numeric(unlist(sapply(Data_list,FUN=function(x){GHGI_p2[GHGI_p2[,1]==x,as.character(inventory_year)]})))*
                                   1000/(16.043*60*60*24*365),#convert from kg/yr to mol/s
                                 row.names = NULL)
  
  rm(GHGI_p1,GHGI_p2,Data_list,GHGI_file,first_col)
  ##############################################################################
  #convert a lot of the activity data to emissions data
  
  #Mains using EFs from Weller et al., or as specified in config
  all_merge_clean_new$bare_steel_mains_ER <- (rowSums(as.data.frame(all_merge_clean_new)[,c("PHMSA_MMILES_STEEL_UNP_BARE","PHMSA_MMILES_STEEL_CP_BARE","PHMSA_MMILES_CU")],
                                                  na.rm=T)*
                                            GHGI_natural_gas_pipeline_emission_factors[1,"Leaks_per_mile"]*
                                            GHGI_natural_gas_pipeline_emission_factors[1,"Avg_emissions_mol_per_s"])
  all_merge_clean_new$iron_mains_ER <- (rowSums(as.data.frame(all_merge_clean_new)[,c("PHMSA_MMILES_CI","PHMSA_MMILES_DI","PHMSA_MMILES_RCI")],
                                            na.rm=T)*
                                      GHGI_natural_gas_pipeline_emission_factors[2,"Leaks_per_mile"]*
                                      GHGI_natural_gas_pipeline_emission_factors[2,"Avg_emissions_mol_per_s"])
  all_merge_clean_new$coat_steel_mains_ER <- (rowSums(as.data.frame(all_merge_clean_new)[,c("PHMSA_MMILES_STEEL_UNP_COATED","PHMSA_MMILES_STEEL_CP_COATED","PHMSA_MMILES_OTHER")],
                                                  na.rm=T)*
                                            GHGI_natural_gas_pipeline_emission_factors[3,"Leaks_per_mile"]*
                                            GHGI_natural_gas_pipeline_emission_factors[3,"Avg_emissions_mol_per_s"])
  all_merge_clean_new$plastic_mains_ER <- (all_merge_clean_new$PHMSA_MMILES_PLASTIC*
                                         GHGI_natural_gas_pipeline_emission_factors[4,"Leaks_per_mile"]*
                                         GHGI_natural_gas_pipeline_emission_factors[4,"Avg_emissions_mol_per_s"])
  
  # Services using EFs from the EPA GHGI, or national inventory report
  all_merge_clean_new$UNP_steel_serv_ER <- (rowSums(as.data.frame(all_merge_clean_new)[,c("PHMSA_NUM_SRVS_STEEL_UNP_COATED","PHMSA_NUM_SRVS_STEEL_UNP_BARE")],
                                                na.rm=T)*
                                          GHGI_Services$EF[1])
  all_merge_clean_new$CP_steel_serv_ER <- (rowSums(as.data.frame(all_merge_clean_new)[,c("PHMSA_NUM_SRVS_STEEL_CP_BARE","PHMSA_NUM_SRVS_STEEL_CP_COATED","PHMSA_NUM_SRVS_OTHER")],
                                               na.rm=T)*
                                         GHGI_Services$EF[2])
  all_merge_clean_new$plastic_serv_ER <- (all_merge_clean_new$PHMSA_NUM_SRVS_PLASTIC*
                                        GHGI_Services$EF[3])
  all_merge_clean_new$copper_serv_ER <- (rowSums(as.data.frame(all_merge_clean_new)[,c("PHMSA_NUM_SRVS_CU","PHMSA_NUM_SRVS_CI","PHMSA_NUM_SRVS_DI","PHMSA_NUM_SRVS_RCI")],
                                             na.rm=T)*
                                       GHGI_Services$EF[4])
  
  #split by function/pressure
  GHGI_MnR_above <- sum(GHGI_MnR$Total_stations[-grep('Vault', GHGI_MnR$Type)])
  GHGI_MnR_below <- sum(GHGI_MnR$Total_stations[grep('Vault', GHGI_MnR$Type)])
  
  # Estimate emissions by function/pressure
  all_merge_clean_new$MnR_HiP_ER <- (all_merge_clean_new$GHGRP_MnR_above*                                                    # Abv grade stations
                                   GHGI_MnR$Total_stations[which(GHGI_MnR$Type == 'M&R >300')]/GHGI_MnR_above* # Type fraction
                                   GHGI_MnR$EF[which(GHGI_MnR$Type == 'M&R >300')]) # Emission factor
  
  all_merge_clean_new$MnR_MidP_ER <- (all_merge_clean_new$GHGRP_MnR_above*
                                    GHGI_MnR$Total_stations[which(GHGI_MnR$Type == 'M&R 100-300')]/GHGI_MnR_above*
                                    GHGI_MnR$EF[which(GHGI_MnR$Type == 'M&R 100-300')])
  
  all_merge_clean_new$MnR_LoP_ER <- (all_merge_clean_new$GHGRP_MnR_above*
                                   GHGI_MnR$Total_stations[which(GHGI_MnR$Type == 'M&R <100')]/GHGI_MnR_above*
                                   GHGI_MnR$EF[which(GHGI_MnR$Type == 'M&R <100')])
  
  all_merge_clean_new$Reg_HiP_ER <- (all_merge_clean_new$GHGRP_MnR_above*
                                   GHGI_MnR$Total_stations[which(GHGI_MnR$Type == 'Reg >300')]/GHGI_MnR_above*
                                   GHGI_MnR$EF[which(GHGI_MnR$Type == 'Reg >300')])
  
  all_merge_clean_new$Reg_MidP_ER <- (all_merge_clean_new$GHGRP_MnR_above*
                                    GHGI_MnR$Total_stations[which(GHGI_MnR$Type == 'Reg 100-300')]/GHGI_MnR_above*
                                    GHGI_MnR$EF[which(GHGI_MnR$Type == 'Reg 100-300')])
  
  all_merge_clean_new$Reg_LoP_ER <- (all_merge_clean_new$GHGRP_MnR_above*
                                   GHGI_MnR$Total_stations[which(GHGI_MnR$Type == 'Reg 40-100')]/GHGI_MnR_above*
                                   GHGI_MnR$EF[which(GHGI_MnR$Type == 'Reg 40-100')])
  
  all_merge_clean_new$Reg_VLP_ER <- (all_merge_clean_new$GHGRP_MnR_above*
                                   GHGI_MnR$Total_stations[which(GHGI_MnR$Type == 'Reg <40')]/GHGI_MnR_above*
                                   GHGI_MnR$EF[which(GHGI_MnR$Type == 'Reg <40')])
  
  all_merge_clean_new$RegV_HiP_ER <- (all_merge_clean_new$GHGRP_MnR_below*
                                    GHGI_MnR$Total_stations[which(GHGI_MnR$Type == 'R-Vault >300')]/GHGI_MnR_below*
                                    GHGI_MnR$EF[which(GHGI_MnR$Type == 'R-Vault >300')])
  
  all_merge_clean_new$RegV_MidP_ER <- (all_merge_clean_new$GHGRP_MnR_below*
                                     GHGI_MnR$Total_stations[which(GHGI_MnR$Type == 'R-Vault 100-300')]/GHGI_MnR_below*
                                     GHGI_MnR$EF[which(GHGI_MnR$Type == 'R-Vault 100-300')])
  
  all_merge_clean_new$RegV_LoP_ER <- (all_merge_clean_new$GHGRP_MnR_below*
                                    GHGI_MnR$Total_stations[which(GHGI_MnR$Type == 'R-Vault 40-100')]/GHGI_MnR_below*
                                    GHGI_MnR$EF[which(GHGI_MnR$Type == 'R-Vault 40-100')])
  
  # Consumer meters - use emission factors from the EPA national inventory report, also known as the GHGI
  all_merge_clean_new$Res_meter_ER <- all_merge_clean_new$EIA_Residential_Total_Customers*GHGI_meters$EF[1]
  all_merge_clean_new$Com_meter_ER <- all_merge_clean_new$EIA_Commercial_Total_Customers*GHGI_meters$EF[2]
  all_merge_clean_new$Ind_meter_ER <- all_merge_clean_new$EIA_Industrial_Total_Customers*GHGI_meters$EF[3]
  
  # Maintenance and upsets
  # We're going to need the total miles of pipeline (inc. services) -
  # calculate that here from AVERAGE_LENGTH (converting ft to miles)
  all_merge_clean_new$Relief_valve_ER <- all_merge_clean_new$PHMSA_MMILES_TOTAL*GHGI_maintenance$EF[1]
  all_merge_clean_new$Blowdown_ER <- all_merge_clean_new$PHMSA_Miles_main_and_serv*GHGI_maintenance$EF[2]
  all_merge_clean_new$Mishap_ER <- all_merge_clean_new$PHMSA_Miles_main_and_serv*GHGI_maintenance$EF[3]
  
  # Post-meter, in this case it's entirely allocated to residential (no data on
  # commercial buildings)
  all_merge_clean_new$post_meter_ER_total_res <- all_merge_clean_new$`EIA_Residential_Total_Volume_(Mcf)`*1000*natural_gas_post_meter_emission_factor
  #McF = thousand cubic ft
  ##############################################################################
  #break the emissions into residential and commercial fractions
  
  # Calculate the total mains emissions to be distributed according to
  # residential and commercial CO2 emissions This is calculated for each company
  # according to the ratio of residential:commercial customers Industrial
  # customer numbers are much smaller, so we ignore these here
  all_merge_clean_new$mains_ER_total_res <- ((all_merge_clean_new$bare_steel_mains_ER + 
                                            all_merge_clean_new$iron_mains_ER +
                                            all_merge_clean_new$coat_steel_mains_ER +
                                            all_merge_clean_new$plastic_mains_ER)*
                                           all_merge_clean_new$EIA_Residential_Total_Customers/
                                           (all_merge_clean_new$EIA_Residential_Total_Customers + all_merge_clean_new$EIA_Commercial_Total_Customers))
  
  all_merge_clean_new$mains_ER_total_com <- ((all_merge_clean_new$bare_steel_mains_ER + 
                                            all_merge_clean_new$iron_mains_ER +
                                            all_merge_clean_new$coat_steel_mains_ER +
                                            all_merge_clean_new$plastic_mains_ER)*
                                           all_merge_clean_new$EIA_Commercial_Total_Customers/
                                           (all_merge_clean_new$EIA_Residential_Total_Customers + all_merge_clean_new$EIA_Commercial_Total_Customers))
  
  all_merge_clean_new$serv_ER_total_res <- ((all_merge_clean_new$UNP_steel_serv_ER + 
                                           all_merge_clean_new$CP_steel_serv_ER +
                                           all_merge_clean_new$plastic_serv_ER +
                                           all_merge_clean_new$copper_serv_ER)*
                                          all_merge_clean_new$EIA_Residential_Total_Customers/
                                          (all_merge_clean_new$EIA_Residential_Total_Customers + all_merge_clean_new$EIA_Commercial_Total_Customers))
  
  all_merge_clean_new$serv_ER_total_com <- ((all_merge_clean_new$UNP_steel_serv_ER + 
                                           all_merge_clean_new$CP_steel_serv_ER +
                                           all_merge_clean_new$plastic_serv_ER +
                                           all_merge_clean_new$copper_serv_ER)*
                                          all_merge_clean_new$EIA_Commercial_Total_Customers/
                                          (all_merge_clean_new$EIA_Residential_Total_Customers + all_merge_clean_new$EIA_Commercial_Total_Customers))
  
  all_merge_clean_new$MnR_ER_total_res <- ((all_merge_clean_new$MnR_HiP_ER + 
                                          all_merge_clean_new$MnR_MidP_ER +
                                          all_merge_clean_new$MnR_LoP_ER +
                                          all_merge_clean_new$Reg_HiP_ER +
                                          all_merge_clean_new$Reg_MidP_ER +
                                          all_merge_clean_new$Reg_LoP_ER +
                                          all_merge_clean_new$Reg_VLP_ER +
                                          all_merge_clean_new$RegV_HiP_ER +
                                          all_merge_clean_new$RegV_MidP_ER +
                                          all_merge_clean_new$RegV_LoP_ER)*
                                         all_merge_clean_new$EIA_Residential_Total_Customers/
                                         (all_merge_clean_new$EIA_Residential_Total_Customers + all_merge_clean_new$EIA_Commercial_Total_Customers))
  
  all_merge_clean_new$MnR_ER_total_com <- ((all_merge_clean_new$MnR_HiP_ER + 
                                          all_merge_clean_new$MnR_MidP_ER +
                                          all_merge_clean_new$MnR_LoP_ER +
                                          all_merge_clean_new$Reg_HiP_ER +
                                          all_merge_clean_new$Reg_MidP_ER +
                                          all_merge_clean_new$Reg_LoP_ER +
                                          all_merge_clean_new$Reg_VLP_ER +
                                          all_merge_clean_new$RegV_HiP_ER +
                                          all_merge_clean_new$RegV_MidP_ER +
                                          all_merge_clean_new$RegV_LoP_ER)*
                                         all_merge_clean_new$EIA_Commercial_Total_Customers/
                                         (all_merge_clean_new$EIA_Residential_Total_Customers + all_merge_clean_new$EIA_Commercial_Total_Customers))
  
  # We could allocate the industrial meter emissions by ACES and Vulcan industrial sector
  # But this sector is dominated by a handful of large point sources, many of which don't even use natural gas
  # So instead, share these emissions out between the residential and commercial CO2 maps
  # Split according to the ratio of Res_meter_ER:Com_meter_ER - could equally have split according to the number of
  # customers, but that would shift the ratio of total meter emissions towards residential, which doesn't seem desirable
  # Keep the same naming convention as for the other subsectors (i.e. _total_res) even though it makes less sense here
  all_merge_clean_new$meter_ER_total_res <- (all_merge_clean_new$Res_meter_ER +
                                           all_merge_clean_new$Ind_meter_ER*
                                           all_merge_clean_new$Res_meter_ER/
                                           (all_merge_clean_new$Res_meter_ER + all_merge_clean_new$Com_meter_ER))
  
  all_merge_clean_new$meter_ER_total_com <- (all_merge_clean_new$Com_meter_ER +
                                           all_merge_clean_new$Ind_meter_ER*
                                           all_merge_clean_new$Com_meter_ER/
                                           (all_merge_clean_new$Res_meter_ER + all_merge_clean_new$Com_meter_ER))
  
  
  all_merge_clean_new$upset_ER_total_res <- ((all_merge_clean_new$Relief_valve_ER + 
                                            all_merge_clean_new$Blowdown_ER +
                                            all_merge_clean_new$Mishap_ER)*
                                           all_merge_clean_new$EIA_Residential_Total_Customers/
                                           (all_merge_clean_new$EIA_Residential_Total_Customers + all_merge_clean_new$EIA_Commercial_Total_Customers))
  
  all_merge_clean_new$upset_ER_total_com <- ((all_merge_clean_new$Relief_valve_ER + 
                                            all_merge_clean_new$Blowdown_ER +
                                            all_merge_clean_new$Mishap_ER)*
                                           all_merge_clean_new$EIA_Commercial_Total_Customers/
                                           (all_merge_clean_new$EIA_Residential_Total_Customers + all_merge_clean_new$EIA_Commercial_Total_Customers))
  
  
}


# #compare spatially
# all_merge_sf <- vect(all_merge_sf)
# all_merge_sf <- project(all_merge_sf,all_merge_clean_new)
# 
# #identical polygon outlines other than slight differences (CB vs TL for states
# #likely, definitely difference in water areas since new uses a different file
# #for them)
# plot(all_merge_sf - all_merge_clean_new,main="old only")
# plot(all_merge_clean_new - all_merge_sf,main="new only")
# 
# #Joe's version also looks identical, though in the ACES/Vulcan grid.  Only
# #difference is minor state outline differences and he had CT, I have MD
# joe=st_read("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Joe_data/Natural_gas/NG_service_territories/HIFLD_LDC_jpitt/HIFLD_LDC_jpitt.shp")
# plot(project(vect(joe),all_merge_sf),"Cmpny")
# plot(project(vect(joe),all_merge_sf) - all_merge_sf,main = "Joe only")
# plot(all_merge_sf - project(vect(joe),all_merge_sf),main = "Mine only")
# 
# 
# #spatially match - though very minor differences in outlines pretty much every polygon
# data_chunk <- seq(0,nrow(all_merge_sf),by=5)
# # A=A+1
# for(A in 1:(nrow(all_merge_sf)/5)){
#   plot(all_merge_sf[(data_chunk[A]+1):data_chunk[A+1]],"Company",main='old')
#   plot(all_merge_clean_new[(data_chunk[A]+1):data_chunk[A+1]],"EIA_Company",main="new")
#   
#   plot(all_merge_sf[(data_chunk[A]+1):data_chunk[A+1]] - all_merge_clean_new[(data_chunk[A]+1):data_chunk[A+1]],main="delta (old - new)")
#   plot(all_merge_clean_new[(data_chunk[A]+1):data_chunk[A+1]] - all_merge_sf[(data_chunk[A]+1):data_chunk[A+1]],main="delta (new - old)")
# }
# 
# #minor - the old approach assigned water areas surrounding
# #long island "OTHER" NG emissions for NY.  New avoids this.




#compare values - all agree
all_merge_sf <- as.data.frame(all_merge_sf)
all_merge_clean_new <- as.data.frame(all_merge_clean_new)

names(all_merge_sf)
names(all_merge_clean_new)

#naming/order differs, make consistent.  First 34 differ in name, but are
#identical.  New has some extra channels and removes 3 (GHGRP activity data is
#saved differently).
old_col_subset <- c(1:35,(36:73)[colnames(all_merge_sf)[36:73] %in% colnames(all_merge_clean_new)[36:77]])
new_col_subset <- c(colnames(all_merge_clean_new)[1:35],colnames(all_merge_sf)[36:73][colnames(all_merge_sf)[36:73] %in% colnames(all_merge_clean_new)[35:77]])

all_merge_sf <- all_merge_sf[order(all_merge_sf$SVCTERID),old_col_subset]
all_merge_clean_new <- all_merge_clean_new[order(all_merge_clean_new$HIFLD_SVCTERID),new_col_subset]


# View(all_merge_clean_new==all_merge_sf)
all(all_merge_clean_new[,1:34]==all_merge_sf[,1:34],na.rm=T)

#some are not numeric, the PHMSA comparisons fail a direct numeric comparison.
#Adding a reasonable rounding
delta <- all_merge_clean_new[,c(35:70)]-all_merge_sf[,c(35:70)]
# View(delta[1:3,])
all(round(delta,8)==0)


################################################################################
#now post domain/state level aggregating, though I don't expect issues here

#old
{
  all_merge_state <- aggregate(all_merge_clean[!(names(all_merge_clean) %in% c('SVCTERID', 'Company', 'Company Name', 'State'))],
                               list(State=all_merge_clean$State),
                               sum,na.rm=T)
  
  # Merge the geometries and change to sf object
  all_merge_state_poly <- merge(all_merge_state, state_shp[c('STUSPS', 'geometry')], by.x='State', by.y='STUSPS')
  all_merge_state_sf <- st_as_sf(all_merge_state_poly, sf_column_name='geometry', crs=crs(state_shp))
  
  all_merge_domain <- colSums(all_merge_clean[!(names(all_merge_clean) %in% c('SVCTERID', 'Company', 'Company Name', 'State'))])
  shp_domain_sf <- st_union(all_merge_state_sf[which(all_merge_state_sf$State %in% state_name_list), 'geometry'])
  shp_domain_sp <- as(shp_domain_sf, 'Spatial')
}


#new
{
  all_merge_state_new <- aggregate(as.data.frame(all_merge_clean_new[,!(names(all_merge_clean_new) %in% c('HIFLD_SVCTERID', 'EIA_Company', 'EIA_Company_Name', 'PHMSA_State'))]),
                               list(PHMSA_State=all_merge_clean_new$PHMSA_State),
                               sum,na.rm=T)
  # Merge the geometries
  all_merge_state_poly_new <- merge(State_Tigerlines, all_merge_state_new, by.y='PHMSA_State', by.x='STUSPS')
  names(all_merge_state_poly_new) <- gsub("STUSPS","PHMSA_State",names(all_merge_state_poly_new))
  
  all_merge_domain_new <- apply(as.data.frame(all_merge_clean_new),2,as.numeric)
  all_merge_domain_new <- colSums(all_merge_domain_new)
  
  all_merge_domain_poly <- aggregate(State_Tigerlines)
  values(all_merge_domain_poly) <- t(all_merge_domain_new)
}


#compare numeric - all agree

names(all_merge_state_new) <- gsub("EIA_","",names(all_merge_state_new))
names(all_merge_state_new) <- gsub("PHMSA_","",names(all_merge_state_new))

#naming/order differs, make consistent.  First 10 differ in name, but are
#identical.  New has some extra channels and removes 3 (GHGRP activity data is
#saved differently).
old_col_subset <- c(1:10,(11:70)[colnames(all_merge_state)[11:70] %in% colnames(all_merge_state_new)[11:70]])
new_col_subset <- c(colnames(all_merge_state_new)[1:10],colnames(all_merge_state)[11:70][colnames(all_merge_state)[11:70] %in% colnames(all_merge_state_new)[11:70]])

all_merge_state <- all_merge_state[order(all_merge_state$State),old_col_subset]
all_merge_state_new <- all_merge_state_new[order(all_merge_state_new$State),new_col_subset]

all(all_merge_state[,1:10]==all_merge_state_new[,1:10])
delta <- all_merge_state[,c(10:67)]-all_merge_state_new[,c(10:67)]
# View(delta[1:3,])
all(round(delta,8)==0)




names(all_merge_domain_new) <- gsub("EIA_","",names(all_merge_domain_new))
names(all_merge_domain_new) <- gsub("PHMSA_","",names(all_merge_domain_new))
all_merge_domain_new <- all_merge_domain_new[5:70]

#naming/order differs, make consistent.  First 34 differ in name, but are
#identical.  New has some extra channels and removes 3 (GHGRP activity data is
#saved differently).
old_col_subset <- c(1:10,(11:69)[names(all_merge_domain)[11:69] %in% names(all_merge_domain_new)[11:69]])
new_col_subset <- c(names(all_merge_domain_new)[1:10],names(all_merge_domain)[11:69][names(all_merge_domain)[11:69] %in% names(all_merge_domain_new)[11:69]])

all_merge_domain <- all_merge_domain[old_col_subset]
all_merge_domain_new <- all_merge_domain_new[new_col_subset]

all(all_merge_domain[1:10]==all_merge_domain_new[1:10],na.rm=T)
delta <- all_merge_domain[c(10:66)]-all_merge_domain_new[c(10:66)]
all(round(delta,8)==0)
sort(abs(delta))

#the only one that differs by more than 1E-5; -4.325679e-03
all_merge_domain["Miles_main_and_serv"] - all_merge_domain_new["Miles_main_and_serv"]

#each individual state agrees ~ exactly
all_merge_state[,"Miles_main_and_serv"]
all_merge_state_new[,"Miles_main_and_serv"]








#compare spatial - agree, just differ in that state outlines are cb rather than
#tl in old ones
plot(vect(all_merge_state_sf))
plot(all_merge_state_poly_new,add=T,border="red")

plot(vect(shp_domain_sf))
plot(all_merge_domain_poly,add=T,border="red")

################################################################################
#compare GHGRP download to API download - need to be consistent to compare
#results using them

#old
{
  GHGRP_file <- file.path(dirname(input_directory),"/Raw Data/US_GHGRP_NG_Local_Distribution_Companies_only_all_years.xls")
  GHGRP_csv <- read_xls(GHGRP_file,sheet=as.character(inventory_year),col_names = T,skip = 5)
  # Load the GHGRP csv file - this file may have had company ID added to it for
  # the states that we will use
  
  GHGRP_csv_old <- GHGRP_csv[which(GHGRP_csv$STATE%in%state_name_list),]
}

#new
{
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
  #Download the relevant facility (e.g., location) data using the API and merge
  
  #see https://www.epa.gov/enviro/envirofacts-data-service-api
  data_URLs <- paste0("https://data.epa.gov/efservice/PUB_DIM_FACILITY/JSON")
  
  #download data
  ghgrp_facility_info <- fromJSON(data_URLs)
  
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
}


#2 facilities were converted from the state of HQ to a state of operation in the
#domain (Washington gas light company and Columbia gas of PA) in new
GHGRP_csv <- GHGRP_csv[!(GHGRP_csv$facility_id %in% c(1007849,1003839)),]

#otherwise, all equal.  Don't actually use the emissions, just the ID to pull MR
#station counts via webscraping (same method new and old)
all(sort(GHGRP_csv$facility_id) - sort(GHGRP_csv_old$`GHGRP ID`)==0)

################################################################################
#Now compare the calculations if not byLDC - recall it does give different than
#byLDC results as some counts are estimated while exacts can be used with byLDC

#using the new GHGRP csv to include the 2 added facilities

#old
{
  EIA_file = file.path(input_directory,"176 Type of Operations and Sector Items.xlsx")
  PHMSA_file = file.path(input_directory,"annual_gas_distribution_2010_present/annual_gas_distribution_2019.xlsx")
  GHGI_file = file.path(input_directory,"2022_ghgi_natural_gas_systems_annex36_tables.xlsx")

  Cartographic_boundary_file <- "G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Shapefiles/cb_2021_us_state_500k/cb_2021_us_state_500k.shp"
  #Slightly different census outlines for states, excluding water areas
  ################################################################################
  #load in and filter the various files, excluding the EPA one for now
  
  EIA_csv <- read_xlsx(EIA_file,skip=1,col_names = T)
  # Load the EIA company-level data for 2019 - this may have been edited to add a
  # dummy 'OTHER' entry
  PHMSA_csv <- read_xlsx(PHMSA_file,skip=2,col_names = T)
  # Load the PHMSA data for 2019 - this file may have had company ID's edited to
  # be consistent with EIA for the states that we will use

  PHMSA_csv_NG <- PHMSA_csv[which(PHMSA_csv$COMMODITY == 'Natural Gas'),]
  # Filter the PHMSA file by commodity
  
  PHMSA_csv_NG <- PHMSA_csv_NG[which(PHMSA_csv_NG$STOP%in%state_name_list),]
  #filter to only those for the relevant states and those with a company ID in
  #HIFLD (present at all).
  
  
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
  #Download the relevant facility (e.g., location) data using the API and merge
  
  #see https://www.epa.gov/enviro/envirofacts-data-service-api
  data_URLs <- paste0("https://data.epa.gov/efservice/PUB_DIM_FACILITY/JSON")
  
  #download data
  ghgrp_facility_info <- fromJSON(data_URLs)
  
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
        #the url is build from the GHGRP ID, the desired inventory_year, and a common url.
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
        stop("Failed to download ",GHGRP_csv$`FACILITY NAME`[A]," data from\n",
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
    
    cat("\rFinished downloading GHGRP data for",A,"of",nrow(GHGRP_csv),"                 ")
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
  ################################################################################
  #Pull the EPA data we'll need later and save it to a few dataframes
  
  first_col <- which(read_xlsx(EPA_file,sheet = EPA_Activity_sheet,.name_repair = "minimal")[,1]=="Segment/Source")
  EPA_p1 <- read_xlsx(EPA_file,sheet = EPA_Activity_sheet,skip=first_col,col_names = T)
  
  first_col <- which(read_xlsx(EPA_file,sheet = EPA_EF_sheet,.name_repair = "minimal")[,1]=="Segment/Source")
  EPA_p2 <- read_xlsx(EPA_file,sheet = EPA_EF_sheet,skip=first_col,col_names = T)
  #p2 = emission factors, p1 = activity data.  Columns = inventory_year, rows = various
  #types of sources.  First col is just to identify the first column of useable
  #data
  
  Data_list <- c("M&R >300","M&R 100-300","M&R <100","Reg >300","R-Vault >300",
                 "Reg 100-300","R-Vault 100-300","Reg 40-100","R-Vault 40-100",
                 "Reg <40")
  #all the sources we're looking for, written exactly as in the EPA file
  
  EPA_MnR <- data.frame("Type"=Data_list,
                        "EF"=as.numeric(unlist(sapply(Data_list,FUN=function(x){EPA_p2[EPA_p2[,1]==x,as.character(inventory_year)]})))*
                          1000/(16.043*60*60*24*365),#convert from kg/yr to mol/s
                        "Total_stations"=as.numeric(unlist(sapply(Data_list,FUN=function(x){EPA_p1[EPA_p1[,1]==x,as.character(inventory_year)]}))),
                        row.names = NULL)
  #use sapply to find the row using data list, specify the column as the as.character(inventory_year) and
  #grab the relevant EF and activity data into a dataframe.
  
  #repeat for several other source types
  Data_list <- c("Services - Unprotected steel",
                 "Services Protected steel",
                 "Services - Plastic",
                 "Services - Copper")
  
  EPA_Services <- data.frame("Type"=Data_list,
                             "EF"=as.numeric(unlist(sapply(Data_list,FUN=function(x){EPA_p2[EPA_p2[,1]==x,as.character(inventory_year)]})))*
                               1000/(16.043*60*60*24*365),#convert from kg/yr to mol/s
                             row.names = NULL)
  
  Data_list <- c("Residential",
                 "Commercial",
                 "Industrial")
  
  EPA_meters <- data.frame("Type"=Data_list,
                           "EF"=as.numeric(unlist(sapply(Data_list,FUN=function(x){EPA_p2[which(EPA_p2[,1]==x)[1],as.character(inventory_year)]})))*
                             1000/(16.043*60*60*24*365),#convert from kg/yr to mol/s
                           row.names = NULL)
  
  Data_list <- c("Pressure Relief Valve Releases",
                 "Pipeline Blowdown",
                 "Mishaps (Dig-ins)")
  
  EPA_maintenance <- data.frame("Type"=Data_list,
                                "EF"=as.numeric(unlist(sapply(Data_list,FUN=function(x){EPA_p2[EPA_p2[,1]==x,as.character(inventory_year)]})))*
                                  1000/(16.043*60*60*24*365),#convert from kg/yr to mol/s
                                row.names = NULL)
  
  Data_list <- c("Pressure Relief Valve Releases",
                 "Pipeline Blowdown",
                 "Mishaps (Dig-ins)")
  
  EPA_maintenance <- data.frame("Type"=Data_list,
                                "EF"=as.numeric(unlist(sapply(Data_list,FUN=function(x){EPA_p2[EPA_p2[,1]==x,as.character(inventory_year)]})))*
                                  1000/(16.043*60*60*24*365),#convert from kg/yr to mol/s
                                row.names = NULL)
  
  rm(EPA_p1,EPA_p2,Data_list,first_col)
  ################################################################################
  ## Calculate emissions (all in mol/s):
  
  PHMSA_csv_NG$bare_steel_mains_ER <- (rowSums(PHMSA_csv_NG[,c("MMILES_STEEL_UNP_BARE","MMILES_STEEL_CP_BARE","MMILES_CU")],
                                               na.rm=T)*
                                         rowProds(GHGI_natural_gas_pipeline_emission_factors)[1])
  PHMSA_csv_NG$iron_mains_ER <- (rowSums(PHMSA_csv_NG[,c("MMILES_CI","MMILES_DI","MMILES_RCI")],
                                         na.rm=T)*
                                   rowProds(GHGI_natural_gas_pipeline_emission_factors)[2])
  PHMSA_csv_NG$coat_steel_mains_ER <- (rowSums(PHMSA_csv_NG[,c("MMILES_STEEL_UNP_COATED","MMILES_STEEL_CP_COATED","MMILES_OTHER")],
                                               na.rm=T)*
                                         rowProds(GHGI_natural_gas_pipeline_emission_factors)[3])
  PHMSA_csv_NG$plastic_mains_ER <- (PHMSA_csv_NG$MMILES_PLASTIC*
                                      rowProds(GHGI_natural_gas_pipeline_emission_factors)[4])
  #Mains using EFs from Weller et al., or as specified at the top of the code
  
  PHMSA_csv_NG$UNP_steel_serv_ER <- (rowSums(PHMSA_csv_NG[,c("NUM_SRVS_STEEL_UNP_COATED","NUM_SRVS_STEEL_UNP_BARE")],
                                             na.rm=T)*
                                       EPA_Services$EF[1])
  PHMSA_csv_NG$CP_steel_serv_ER <- (rowSums(PHMSA_csv_NG[,c("NUM_SRVS_STEEL_CP_BARE","NUM_SRVS_STEEL_CP_COATED","NUM_SRVS_OTHER")],
                                            na.rm=T)*
                                      EPA_Services$EF[2])
  PHMSA_csv_NG$plastic_serv_ER <- (PHMSA_csv_NG$NUM_SRVS_PLASTIC*
                                     EPA_Services$EF[3])
  PHMSA_csv_NG$copper_serv_ER <- (rowSums(PHMSA_csv_NG[,c("NUM_SRVS_CU","NUM_SRVS_CI","NUM_SRVS_DI","NUM_SRVS_RCI")],
                                          na.rm=T)*
                                    EPA_Services$EF[4])
  # Services using EFs from the EPA national inventory report
  
  # M&R stations - can't use GHGRP data without matching facilities, so estimate
  # based on avg stations per mile for reporters in each state. Then split by
  # pressure and function assuming the same split as at the national level (from
  # the EPA national inventory report).
  
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
  above_grade_MnR$stations_per_mile <- above_grade_MnR$x/main_miles_ghgrp$x
  below_grade_MnR$stations_per_mile <- below_grade_MnR$x/main_miles_ghgrp$x
  # Calculate average stations per mile in each state
  
    state_indx <- match(PHMSA_csv_NG$STOP,above_grade_MnR$State)
    PHMSA_csv_NG$MnR_above <- PHMSA_csv_NG$MMILES_TOTAL*above_grade_MnR$stations_per_mile[state_indx]
    PHMSA_csv_NG$MnR_below <- PHMSA_csv_NG$MMILES_TOTAL*below_grade_MnR$stations_per_mile[state_indx]
  # allocate average stations per mile in each state to all facilities if not
  # calculating by LDC
  
  ################################################################################
  #prep to merge the many files, excluding the EPA for now, calculate a few
  #additional variables
  
  PHMSA_csv_NG$Miles_main_and_serv <- PHMSA_csv_NG$MMILES_TOTAL + PHMSA_csv_NG$NUM_SRVCS_TOTAL*PHMSA_csv_NG$AVERAGE_LENGTH/5280
  # We're going to need the total miles of pipeline (inc. services) later -
  # calculate that here from AVERAGE_LENGTH (in ft)
  
  # Then select the columns we need and aggregate the entries which share the same company ID or state
  PHMSA_cols_to_keep <- c('MMILES_STEEL_UNP_BARE',
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
                          'Miles_main_and_serv',
                          "bare_steel_mains_ER",
                          "iron_mains_ER",
                          "coat_steel_mains_ER",
                          "plastic_mains_ER",
                          "UNP_steel_serv_ER",
                          "CP_steel_serv_ER",
                          "plastic_serv_ER",
                          "copper_serv_ER")
  
    PHMSA_cols_to_keep <- c(PHMSA_cols_to_keep,
                            'MnR_above',
                            'MnR_below')
  #if not calculating by LDC, these variables are in the PHMSA.  Otherwise,
  #they're in the ghgrp data
  
  EIA_cols_to_keep <- c("Residential Total Volume (Mcf)",
                        "Residential Total Customers",
                        'Commercial Total Volume (Mcf)',
                        'Commercial Total Customers',
                        'Industrial Total Volume (Mcf)',
                        'Industrial Total Customers',
                        'Electric Total Volume (Mcf)',
                        'Electric Total Customers')
  
    cols_to_keep <- c('State',
                      EIA_cols_to_keep,
                      PHMSA_cols_to_keep,
                      'Miles_of_Mains')
  #No HIFLD or GHGRP data if not calculating by LDC
  
    PHMSA_csv_NG_agg <- aggregate(PHMSA_csv_NG[PHMSA_cols_to_keep],
                                  list(STOP=PHMSA_csv_NG$STOP),
                                  sum,na.rm=T)
    EIA_csv_agg <- aggregate(EIA_csv[EIA_cols_to_keep],
                             list(State=EIA_csv$State),
                             sum,na.rm=T)
    GHGRP_csv_agg <- aggregate(GHGRP_csv[,"Miles_of_Mains"],
                               list(STATE=GHGRP_csv$operating_state),
                               sum,na.rm=T)
    colnames(GHGRP_csv_agg) <- c("STATE","Miles_of_Mains")
    EIA_PHMSA_merge <- merge(EIA_csv_agg, PHMSA_csv_NG_agg, by.x='State', by.y='STOP')
    all_merge <- merge(EIA_PHMSA_merge, GHGRP_csv_agg, by.x='State', by.y='STATE', all.x=TRUE)
    # Now merge csv stuff together
    
    all_merge_clean <- all_merge[cols_to_keep]
    # Clean up

  # Calculate the total mains emissions to be distributed according to residential and commercial CO2 emissions
  # This is calculated for each company according to the ratio of residential:commercial customers
  # Industrial customer numbers are much smaller, so we ignore these here
  all_merge_clean$mains_ER_total_res <- ((all_merge_clean$bare_steel_mains_ER + 
                                            all_merge_clean$iron_mains_ER +
                                            all_merge_clean$coat_steel_mains_ER +
                                            all_merge_clean$plastic_mains_ER)*
                                           all_merge_clean$"Residential Total Customers"/
                                           (all_merge_clean$"Residential Total Customers" + all_merge_clean$"Commercial Total Customers"))
  
  all_merge_clean$mains_ER_total_com <- ((all_merge_clean$bare_steel_mains_ER + 
                                            all_merge_clean$iron_mains_ER +
                                            all_merge_clean$coat_steel_mains_ER +
                                            all_merge_clean$plastic_mains_ER)*
                                           all_merge_clean$"Commercial Total Customers"/
                                           (all_merge_clean$"Residential Total Customers" + all_merge_clean$"Commercial Total Customers"))
  
  all_merge_clean$serv_ER_total_res <- ((all_merge_clean$UNP_steel_serv_ER + 
                                           all_merge_clean$CP_steel_serv_ER +
                                           all_merge_clean$plastic_serv_ER +
                                           all_merge_clean$copper_serv_ER)*
                                          all_merge_clean$"Residential Total Customers"/
                                          (all_merge_clean$"Residential Total Customers" + all_merge_clean$"Commercial Total Customers"))
  
  all_merge_clean$serv_ER_total_com <- ((all_merge_clean$UNP_steel_serv_ER + 
                                           all_merge_clean$CP_steel_serv_ER +
                                           all_merge_clean$plastic_serv_ER +
                                           all_merge_clean$copper_serv_ER)*
                                          all_merge_clean$"Commercial Total Customers"/
                                          (all_merge_clean$"Residential Total Customers" + all_merge_clean$"Commercial Total Customers"))
  
  ################################################################################
  #Calculate a few additional emissions
  
  EPA_MnR_above <- sum(EPA_MnR$Total_stations[-grep('Vault', EPA_MnR$Type)])
  EPA_MnR_below <- sum(EPA_MnR$Total_stations[grep('Vault', EPA_MnR$Type)])
  #split by function/pressure
  
  # Estimate emissions by function/pressure
  all_merge_clean$MnR_HiP_ER <- (all_merge_clean$MnR_above*                                                    # Abv grade stations
                                   EPA_MnR$Total_stations[which(EPA_MnR$Type == 'M&R >300')]/EPA_MnR_above* # Type fraction
                                   EPA_MnR$EF[which(EPA_MnR$Type == 'M&R >300')]) # Emission factor
  
  all_merge_clean$MnR_MidP_ER <- (all_merge_clean$MnR_above*
                                    EPA_MnR$Total_stations[which(EPA_MnR$Type == 'M&R 100-300')]/EPA_MnR_above*
                                    EPA_MnR$EF[which(EPA_MnR$Type == 'M&R 100-300')])
  
  all_merge_clean$MnR_LoP_ER <- (all_merge_clean$MnR_above*
                                   EPA_MnR$Total_stations[which(EPA_MnR$Type == 'M&R <100')]/EPA_MnR_above*
                                   EPA_MnR$EF[which(EPA_MnR$Type == 'M&R <100')])
  
  all_merge_clean$Reg_HiP_ER <- (all_merge_clean$MnR_above*
                                   EPA_MnR$Total_stations[which(EPA_MnR$Type == 'Reg >300')]/EPA_MnR_above*
                                   EPA_MnR$EF[which(EPA_MnR$Type == 'Reg >300')])
  
  all_merge_clean$Reg_MidP_ER <- (all_merge_clean$MnR_above*
                                    EPA_MnR$Total_stations[which(EPA_MnR$Type == 'Reg 100-300')]/EPA_MnR_above*
                                    EPA_MnR$EF[which(EPA_MnR$Type == 'Reg 100-300')])
  
  all_merge_clean$Reg_LoP_ER <- (all_merge_clean$MnR_above*
                                   EPA_MnR$Total_stations[which(EPA_MnR$Type == 'Reg 40-100')]/EPA_MnR_above*
                                   EPA_MnR$EF[which(EPA_MnR$Type == 'Reg 40-100')])
  
  all_merge_clean$Reg_VLP_ER <- (all_merge_clean$MnR_above*
                                   EPA_MnR$Total_stations[which(EPA_MnR$Type == 'Reg <40')]/EPA_MnR_above*
                                   EPA_MnR$EF[which(EPA_MnR$Type == 'Reg <40')])
  
  all_merge_clean$RegV_HiP_ER <- (all_merge_clean$MnR_below*
                                    EPA_MnR$Total_stations[which(EPA_MnR$Type == 'R-Vault >300')]/EPA_MnR_below*
                                    EPA_MnR$EF[which(EPA_MnR$Type == 'R-Vault >300')])
  
  all_merge_clean$RegV_MidP_ER <- (all_merge_clean$MnR_below*
                                     EPA_MnR$Total_stations[which(EPA_MnR$Type == 'R-Vault 100-300')]/EPA_MnR_below*
                                     EPA_MnR$EF[which(EPA_MnR$Type == 'R-Vault 100-300')])
  
  all_merge_clean$RegV_LoP_ER <- (all_merge_clean$MnR_below*
                                    EPA_MnR$Total_stations[which(EPA_MnR$Type == 'R-Vault 40-100')]/EPA_MnR_below*
                                    EPA_MnR$EF[which(EPA_MnR$Type == 'R-Vault 40-100')])
  
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
                                         all_merge_clean$"Residential Total Customers"/
                                         (all_merge_clean$"Residential Total Customers" + all_merge_clean$"Commercial Total Customers"))
  
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
                                         all_merge_clean$"Commercial Total Customers"/
                                         (all_merge_clean$"Residential Total Customers" + all_merge_clean$"Commercial Total Customers"))
  
  # Consumer meters - use emission factors from the EPA national inventory report
  all_merge_clean$Res_meter_ER <- all_merge_clean$"Residential Total Customers"*EPA_meters$EF[1]
  all_merge_clean$Com_meter_ER <- all_merge_clean$"Commercial Total Customers"*EPA_meters$EF[2]
  all_merge_clean$Ind_meter_ER <- all_merge_clean$"Industrial Total Customers"*EPA_meters$EF[3]
  
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
  
  # Maintenance and upsets
  all_merge_clean$Relief_valve_ER <- all_merge_clean$MMILES_TOTAL*EPA_maintenance$EF[1]
  all_merge_clean$Blowdown_ER <- all_merge_clean$Miles_main_and_serv*EPA_maintenance$EF[2]
  all_merge_clean$Mishap_ER <- all_merge_clean$Miles_main_and_serv*EPA_maintenance$EF[3]
  
  all_merge_clean$upset_ER_total_res <- ((all_merge_clean$Relief_valve_ER + 
                                            all_merge_clean$Blowdown_ER +
                                            all_merge_clean$Mishap_ER)*
                                           all_merge_clean$"Residential Total Customers"/
                                           (all_merge_clean$"Residential Total Customers" + all_merge_clean$"Commercial Total Customers"))
  
  all_merge_clean$upset_ER_total_com <- ((all_merge_clean$Relief_valve_ER + 
                                            all_merge_clean$Blowdown_ER +
                                            all_merge_clean$Mishap_ER)*
                                           all_merge_clean$"Commercial Total Customers"/
                                           (all_merge_clean$"Residential Total Customers" + all_merge_clean$"Commercial Total Customers"))
  
  # Post-meter
  all_merge_clean$post_meter_ER_total_res <- all_merge_clean$"Residential Total Volume (Mcf)"*1000*natural_gas_post_meter_emission_factor
  #McF = thousand cubic ft
}

#new
{
  EIA_file = file.path(input_directory,"176 Type of Operations and Sector Items.xlsx")
  PHMSA_file = file.path(input_directory,"annual_gas_distribution_2010_present/annual_gas_distribution_2019.xlsx")
  GHGI_file = file.path(input_directory,"2022_ghgi_natural_gas_systems_annex36_tables.xlsx")
  
  
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
  #Download the relevant facility (e.g., location) data using the API and merge
  
  #see https://www.epa.gov/enviro/envirofacts-data-service-api
  data_URLs <- paste0("https://data.epa.gov/efservice/PUB_DIM_FACILITY/JSON")
  
  #download data
  ghgrp_facility_info <- fromJSON(data_URLs)
  
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
    
    cat("\rFinished downloading GHGRP data for",A,"of",nrow(GHGRP_csv),"                 ")
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
  all_merge_clean_new <- all_merge[cols_to_keep]
  
  #just so the state variable is consistent with the byLDC version
  names(all_merge_clean_new) <- gsub("STUSPS","PHMSA_State",names(all_merge_clean_new))
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
  state_indx <- match(all_merge_clean_new$PHMSA_State,above_grade_MnR$State)
  all_merge_clean_new$GHGRP_MnR_above <- all_merge_clean_new$PHMSA_MMILES_TOTAL*above_grade_MnR$stations_per_mile[state_indx]
  all_merge_clean_new$GHGRP_MnR_below <- all_merge_clean_new$PHMSA_MMILES_TOTAL*below_grade_MnR$stations_per_mile[state_indx]
  
  ################################################################################
  #Pull the GHGI data we'll need later and save it to a few dataframes
  
  first_col <- which(read_xlsx(GHGI_file,sheet = GHGI_Activity_sheet,.name_repair = "minimal")[,1]=="Segment/Source")
  GHGI_p1 <- read_xlsx(GHGI_file,sheet = GHGI_Activity_sheet,skip=first_col,col_names = T)
  
  first_col <- which(read_xlsx(GHGI_file,sheet = GHGI_EF_sheet,.name_repair = "minimal")[,1]=="Segment/Source")
  GHGI_p2 <- read_xlsx(GHGI_file,sheet = GHGI_EF_sheet,skip=first_col,col_names = T)
  #p2 = emission factors, p1 = activity data.  Columns = year, rows = various
  #types of sources.  First col is just to identify the first column of useable
  #data
  
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
  
  #repeat for several other source types
  Data_list <- c("Services - Unprotected steel",
                 "Services Protected steel",
                 "Services - Plastic",
                 "Services - Copper")
  
  GHGI_Services <- data.frame("Type"=Data_list,
                              "EF"=as.numeric(unlist(sapply(Data_list,FUN=function(x){GHGI_p2[GHGI_p2[,1]==x,as.character(inventory_year)]})))*
                                1000/(16.043*60*60*24*365),#convert from kg/yr to mol/s
                              row.names = NULL)
  
  Data_list <- c("Residential",
                 "Commercial",
                 "Industrial")
  
  GHGI_meters <- data.frame("Type"=Data_list,
                            "EF"=as.numeric(unlist(sapply(Data_list,FUN=function(x){GHGI_p2[which(GHGI_p2[,1]==x)[1],as.character(inventory_year)]})))*
                              1000/(16.043*60*60*24*365),#convert from kg/yr to mol/s
                            row.names = NULL)
  
  Data_list <- c("Pressure Relief Valve Releases",
                 "Pipeline Blowdown",
                 "Mishaps (Dig-ins)")
  
  GHGI_maintenance <- data.frame("Type"=Data_list,
                                 "EF"=as.numeric(unlist(sapply(Data_list,FUN=function(x){GHGI_p2[GHGI_p2[,1]==x,as.character(inventory_year)]})))*
                                   1000/(16.043*60*60*24*365),#convert from kg/yr to mol/s
                                 row.names = NULL)
  
  rm(GHGI_p1,GHGI_p2,Data_list,GHGI_file,first_col)
  ##############################################################################
  #convert a lot of the activity data to emissions data
  
  #Mains using EFs from Weller et al., or as specified in config
  all_merge_clean_new$bare_steel_mains_ER <- (rowSums(as.data.frame(all_merge_clean_new)[,c("PHMSA_MMILES_STEEL_UNP_BARE","PHMSA_MMILES_STEEL_CP_BARE","PHMSA_MMILES_CU")],
                                                  na.rm=T)*
                                            GHGI_natural_gas_pipeline_emission_factors[1,"Leaks_per_mile"]*
                                            GHGI_natural_gas_pipeline_emission_factors[1,"Avg_emissions_mol_per_s"])
  all_merge_clean_new$iron_mains_ER <- (rowSums(as.data.frame(all_merge_clean_new)[,c("PHMSA_MMILES_CI","PHMSA_MMILES_DI","PHMSA_MMILES_RCI")],
                                            na.rm=T)*
                                      GHGI_natural_gas_pipeline_emission_factors[2,"Leaks_per_mile"]*
                                      GHGI_natural_gas_pipeline_emission_factors[2,"Avg_emissions_mol_per_s"])
  all_merge_clean_new$coat_steel_mains_ER <- (rowSums(as.data.frame(all_merge_clean_new)[,c("PHMSA_MMILES_STEEL_UNP_COATED","PHMSA_MMILES_STEEL_CP_COATED","PHMSA_MMILES_OTHER")],
                                                  na.rm=T)*
                                            GHGI_natural_gas_pipeline_emission_factors[3,"Leaks_per_mile"]*
                                            GHGI_natural_gas_pipeline_emission_factors[3,"Avg_emissions_mol_per_s"])
  all_merge_clean_new$plastic_mains_ER <- (all_merge_clean_new$PHMSA_MMILES_PLASTIC*
                                         GHGI_natural_gas_pipeline_emission_factors[4,"Leaks_per_mile"]*
                                         GHGI_natural_gas_pipeline_emission_factors[4,"Avg_emissions_mol_per_s"])
  
  # Services using EFs from the EPA GHGI, or national inventory report
  all_merge_clean_new$UNP_steel_serv_ER <- (rowSums(as.data.frame(all_merge_clean_new)[,c("PHMSA_NUM_SRVS_STEEL_UNP_COATED","PHMSA_NUM_SRVS_STEEL_UNP_BARE")],
                                                na.rm=T)*
                                          GHGI_Services$EF[1])
  all_merge_clean_new$CP_steel_serv_ER <- (rowSums(as.data.frame(all_merge_clean_new)[,c("PHMSA_NUM_SRVS_STEEL_CP_BARE","PHMSA_NUM_SRVS_STEEL_CP_COATED","PHMSA_NUM_SRVS_OTHER")],
                                               na.rm=T)*
                                         GHGI_Services$EF[2])
  all_merge_clean_new$plastic_serv_ER <- (all_merge_clean_new$PHMSA_NUM_SRVS_PLASTIC*
                                        GHGI_Services$EF[3])
  all_merge_clean_new$copper_serv_ER <- (rowSums(as.data.frame(all_merge_clean_new)[,c("PHMSA_NUM_SRVS_CU","PHMSA_NUM_SRVS_CI","PHMSA_NUM_SRVS_DI","PHMSA_NUM_SRVS_RCI")],
                                             na.rm=T)*
                                       GHGI_Services$EF[4])
  
  #split by function/pressure
  GHGI_MnR_above <- sum(GHGI_MnR$Total_stations[-grep('Vault', GHGI_MnR$Type)])
  GHGI_MnR_below <- sum(GHGI_MnR$Total_stations[grep('Vault', GHGI_MnR$Type)])
  
  # Estimate emissions by function/pressure
  all_merge_clean_new$MnR_HiP_ER <- (all_merge_clean_new$GHGRP_MnR_above*                                                    # Abv grade stations
                                   GHGI_MnR$Total_stations[which(GHGI_MnR$Type == 'M&R >300')]/GHGI_MnR_above* # Type fraction
                                   GHGI_MnR$EF[which(GHGI_MnR$Type == 'M&R >300')]) # Emission factor
  
  all_merge_clean_new$MnR_MidP_ER <- (all_merge_clean_new$GHGRP_MnR_above*
                                    GHGI_MnR$Total_stations[which(GHGI_MnR$Type == 'M&R 100-300')]/GHGI_MnR_above*
                                    GHGI_MnR$EF[which(GHGI_MnR$Type == 'M&R 100-300')])
  
  all_merge_clean_new$MnR_LoP_ER <- (all_merge_clean_new$GHGRP_MnR_above*
                                   GHGI_MnR$Total_stations[which(GHGI_MnR$Type == 'M&R <100')]/GHGI_MnR_above*
                                   GHGI_MnR$EF[which(GHGI_MnR$Type == 'M&R <100')])
  
  all_merge_clean_new$Reg_HiP_ER <- (all_merge_clean_new$GHGRP_MnR_above*
                                   GHGI_MnR$Total_stations[which(GHGI_MnR$Type == 'Reg >300')]/GHGI_MnR_above*
                                   GHGI_MnR$EF[which(GHGI_MnR$Type == 'Reg >300')])
  
  all_merge_clean_new$Reg_MidP_ER <- (all_merge_clean_new$GHGRP_MnR_above*
                                    GHGI_MnR$Total_stations[which(GHGI_MnR$Type == 'Reg 100-300')]/GHGI_MnR_above*
                                    GHGI_MnR$EF[which(GHGI_MnR$Type == 'Reg 100-300')])
  
  all_merge_clean_new$Reg_LoP_ER <- (all_merge_clean_new$GHGRP_MnR_above*
                                   GHGI_MnR$Total_stations[which(GHGI_MnR$Type == 'Reg 40-100')]/GHGI_MnR_above*
                                   GHGI_MnR$EF[which(GHGI_MnR$Type == 'Reg 40-100')])
  
  all_merge_clean_new$Reg_VLP_ER <- (all_merge_clean_new$GHGRP_MnR_above*
                                   GHGI_MnR$Total_stations[which(GHGI_MnR$Type == 'Reg <40')]/GHGI_MnR_above*
                                   GHGI_MnR$EF[which(GHGI_MnR$Type == 'Reg <40')])
  
  all_merge_clean_new$RegV_HiP_ER <- (all_merge_clean_new$GHGRP_MnR_below*
                                    GHGI_MnR$Total_stations[which(GHGI_MnR$Type == 'R-Vault >300')]/GHGI_MnR_below*
                                    GHGI_MnR$EF[which(GHGI_MnR$Type == 'R-Vault >300')])
  
  all_merge_clean_new$RegV_MidP_ER <- (all_merge_clean_new$GHGRP_MnR_below*
                                     GHGI_MnR$Total_stations[which(GHGI_MnR$Type == 'R-Vault 100-300')]/GHGI_MnR_below*
                                     GHGI_MnR$EF[which(GHGI_MnR$Type == 'R-Vault 100-300')])
  
  all_merge_clean_new$RegV_LoP_ER <- (all_merge_clean_new$GHGRP_MnR_below*
                                    GHGI_MnR$Total_stations[which(GHGI_MnR$Type == 'R-Vault 40-100')]/GHGI_MnR_below*
                                    GHGI_MnR$EF[which(GHGI_MnR$Type == 'R-Vault 40-100')])
  
  # Consumer meters - use emission factors from the EPA national inventory report, also known as the GHGI
  all_merge_clean_new$Res_meter_ER <- all_merge_clean_new$EIA_Residential_Total_Customers*GHGI_meters$EF[1]
  all_merge_clean_new$Com_meter_ER <- all_merge_clean_new$EIA_Commercial_Total_Customers*GHGI_meters$EF[2]
  all_merge_clean_new$Ind_meter_ER <- all_merge_clean_new$EIA_Industrial_Total_Customers*GHGI_meters$EF[3]
  
  # Maintenance and upsets
  all_merge_clean_new$Relief_valve_ER <- all_merge_clean_new$PHMSA_MMILES_TOTAL*GHGI_maintenance$EF[1]
  all_merge_clean_new$Blowdown_ER <- all_merge_clean_new$PHMSA_Miles_main_and_serv*GHGI_maintenance$EF[2]
  all_merge_clean_new$Mishap_ER <- all_merge_clean_new$PHMSA_Miles_main_and_serv*GHGI_maintenance$EF[3]
  
  # Post-meter, in this case it's entirely allocated to residential (no data on
  # commercial buildings)
  all_merge_clean_new$post_meter_ER_total_res <- all_merge_clean_new$`EIA_Residential_Total_Volume_(Mcf)`*1000*natural_gas_post_meter_emission_factor
  #McF = thousand cubic ft
  ##############################################################################
  #break the emissions into residential and commercial fractions
  
  # Calculate the total mains emissions to be distributed according to
  # residential and commercial CO2 emissions This is calculated for each company
  # according to the ratio of residential:commercial customers Industrial
  # customer numbers are much smaller, so we ignore these here
  all_merge_clean_new$mains_ER_total_res <- ((all_merge_clean_new$bare_steel_mains_ER + 
                                            all_merge_clean_new$iron_mains_ER +
                                            all_merge_clean_new$coat_steel_mains_ER +
                                            all_merge_clean_new$plastic_mains_ER)*
                                           all_merge_clean_new$EIA_Residential_Total_Customers/
                                           (all_merge_clean_new$EIA_Residential_Total_Customers + all_merge_clean_new$EIA_Commercial_Total_Customers))
  
  all_merge_clean_new$mains_ER_total_com <- ((all_merge_clean_new$bare_steel_mains_ER + 
                                            all_merge_clean_new$iron_mains_ER +
                                            all_merge_clean_new$coat_steel_mains_ER +
                                            all_merge_clean_new$plastic_mains_ER)*
                                           all_merge_clean_new$EIA_Commercial_Total_Customers/
                                           (all_merge_clean_new$EIA_Residential_Total_Customers + all_merge_clean_new$EIA_Commercial_Total_Customers))
  
  all_merge_clean_new$serv_ER_total_res <- ((all_merge_clean_new$UNP_steel_serv_ER + 
                                           all_merge_clean_new$CP_steel_serv_ER +
                                           all_merge_clean_new$plastic_serv_ER +
                                           all_merge_clean_new$copper_serv_ER)*
                                          all_merge_clean_new$EIA_Residential_Total_Customers/
                                          (all_merge_clean_new$EIA_Residential_Total_Customers + all_merge_clean_new$EIA_Commercial_Total_Customers))
  
  all_merge_clean_new$serv_ER_total_com <- ((all_merge_clean_new$UNP_steel_serv_ER + 
                                           all_merge_clean_new$CP_steel_serv_ER +
                                           all_merge_clean_new$plastic_serv_ER +
                                           all_merge_clean_new$copper_serv_ER)*
                                          all_merge_clean_new$EIA_Commercial_Total_Customers/
                                          (all_merge_clean_new$EIA_Residential_Total_Customers + all_merge_clean_new$EIA_Commercial_Total_Customers))
  
  all_merge_clean_new$MnR_ER_total_res <- ((all_merge_clean_new$MnR_HiP_ER + 
                                          all_merge_clean_new$MnR_MidP_ER +
                                          all_merge_clean_new$MnR_LoP_ER +
                                          all_merge_clean_new$Reg_HiP_ER +
                                          all_merge_clean_new$Reg_MidP_ER +
                                          all_merge_clean_new$Reg_LoP_ER +
                                          all_merge_clean_new$Reg_VLP_ER +
                                          all_merge_clean_new$RegV_HiP_ER +
                                          all_merge_clean_new$RegV_MidP_ER +
                                          all_merge_clean_new$RegV_LoP_ER)*
                                         all_merge_clean_new$EIA_Residential_Total_Customers/
                                         (all_merge_clean_new$EIA_Residential_Total_Customers + all_merge_clean_new$EIA_Commercial_Total_Customers))
  
  all_merge_clean_new$MnR_ER_total_com <- ((all_merge_clean_new$MnR_HiP_ER + 
                                          all_merge_clean_new$MnR_MidP_ER +
                                          all_merge_clean_new$MnR_LoP_ER +
                                          all_merge_clean_new$Reg_HiP_ER +
                                          all_merge_clean_new$Reg_MidP_ER +
                                          all_merge_clean_new$Reg_LoP_ER +
                                          all_merge_clean_new$Reg_VLP_ER +
                                          all_merge_clean_new$RegV_HiP_ER +
                                          all_merge_clean_new$RegV_MidP_ER +
                                          all_merge_clean_new$RegV_LoP_ER)*
                                         all_merge_clean_new$EIA_Commercial_Total_Customers/
                                         (all_merge_clean_new$EIA_Residential_Total_Customers + all_merge_clean_new$EIA_Commercial_Total_Customers))
  
  # We could allocate the industrial meter emissions by ACES and Vulcan industrial sector
  # But this sector is dominated by a handful of large point sources, many of which don't even use natural gas
  # So instead, share these emissions out between the residential and commercial CO2 maps
  # Split according to the ratio of Res_meter_ER:Com_meter_ER - could equally have split according to the number of
  # customers, but that would shift the ratio of total meter emissions towards residential, which doesn't seem desirable
  # Keep the same naming convention as for the other subsectors (i.e. _total_res) even though it makes less sense here
  all_merge_clean_new$meter_ER_total_res <- (all_merge_clean_new$Res_meter_ER +
                                           all_merge_clean_new$Ind_meter_ER*
                                           all_merge_clean_new$Res_meter_ER/
                                           (all_merge_clean_new$Res_meter_ER + all_merge_clean_new$Com_meter_ER))
  
  all_merge_clean_new$meter_ER_total_com <- (all_merge_clean_new$Com_meter_ER +
                                           all_merge_clean_new$Ind_meter_ER*
                                           all_merge_clean_new$Com_meter_ER/
                                           (all_merge_clean_new$Res_meter_ER + all_merge_clean_new$Com_meter_ER))
  
  
  all_merge_clean_new$upset_ER_total_res <- ((all_merge_clean_new$Relief_valve_ER + 
                                            all_merge_clean_new$Blowdown_ER +
                                            all_merge_clean_new$Mishap_ER)*
                                           all_merge_clean_new$EIA_Residential_Total_Customers/
                                           (all_merge_clean_new$EIA_Residential_Total_Customers + all_merge_clean_new$EIA_Commercial_Total_Customers))
  
  all_merge_clean_new$upset_ER_total_com <- ((all_merge_clean_new$Relief_valve_ER + 
                                            all_merge_clean_new$Blowdown_ER +
                                            all_merge_clean_new$Mishap_ER)*
                                           all_merge_clean_new$EIA_Commercial_Total_Customers/
                                           (all_merge_clean_new$EIA_Residential_Total_Customers + all_merge_clean_new$EIA_Commercial_Total_Customers))

}

# all_merge_clean_new <- as.data.frame(all_merge_clean_new)
# 
# #compare values - all agree
# names(all_merge_clean)
# names(all_merge_clean_new)
# 
# colnames(all_merge_clean_new) <- gsub("GHGRP_","",colnames(all_merge_clean_new))
# 
# #naming/order differs, make consistent.  First 34 differ in name, but are
# #identical.  New has some extra channels and removes 3 (GHGRP activity data is
# #saved differently).
# old_col_subset <- c(1:32,(33:70)[colnames(all_merge_clean)[33:70] %in% colnames(all_merge_clean_new)[33:82]])
# new_col_subset <- c(colnames(all_merge_clean_new)[1:31],colnames(all_merge_clean)[33:70][colnames(all_merge_clean)[33:70] %in% colnames(all_merge_clean_new)[33:82]])
# new_col_subset <- c("PHMSA_State",new_col_subset)
# 
# all_merge_clean <- all_merge_clean[order(all_merge_clean$State),old_col_subset]
# all_merge_clean_new <- all_merge_clean_new[order(all_merge_clean_new$PHMSA_State),new_col_subset]
# 
# 
# # View(all_merge_clean_new==all_merge_clean)
# all(all_merge_clean_new[,1:32]==all_merge_clean[,1:32],na.rm=T)
# 
# #some are not numeric, the PHMSA comparisons fail a direct numeric comparison.
# #Adding a reasonable rounding
# delta <- all_merge_clean_new[,c(32:67)]-all_merge_clean[,c(32:67)]
# # View(delta[1:3,])
# all(round(delta,8)==0)

################################################################################
#now compare post aggregation

#old
{
  all_merge_state <- aggregate(all_merge_clean[!(names(all_merge_clean) %in% c('SVCTERID', 'Company', 'Company Name', 'State'))],
                               list(State=all_merge_clean$State),
                               sum,na.rm=T)
  
  # Merge the geometries and change to sf object
  all_merge_state_poly <- merge(all_merge_state, state_shp[c('STUSPS', 'geometry')], by.x='State', by.y='STUSPS')
  all_merge_state_sf <- st_as_sf(all_merge_state_poly, sf_column_name='geometry', crs=crs(state_shp))
  
  all_merge_domain <- colSums(all_merge_clean[!(names(all_merge_clean) %in% c('SVCTERID', 'Company', 'Company Name', 'State'))])
  shp_domain_sf <- st_union(all_merge_state_sf[which(all_merge_state_sf$State %in% state_name_list), 'geometry'])
  shp_domain_sp <- as(shp_domain_sf, 'Spatial')
}


#new
{
  all_merge_state_poly_new <- all_merge_clean_new
  all_merge_state_new <- as.data.frame(all_merge_clean_new)
  
  # all_merge_state_new <- aggregate(as.data.frame(all_merge_clean_new[,!(names(all_merge_clean_new) %in% c('HIFLD_SVCTERID', 'EIA_Company', 'EIA_Company_Name', 'PHMSA_State'))]),
  #                                  list(PHMSA_State=all_merge_clean_new$PHMSA_State),
  #                                  sum,na.rm=T)
  # # Merge the geometries
  # all_merge_state_poly_new <- merge(State_Tigerlines, all_merge_state_new, by.y='PHMSA_State', by.x='STUSPS')
  # names(all_merge_state_poly_new) <- gsub("STUSPS","PHMSA_State",names(all_merge_state_poly_new))
  
  all_merge_domain_new <- apply(as.data.frame(all_merge_clean_new),2,as.numeric)
  all_merge_domain_new <- colSums(all_merge_domain_new)
  
  all_merge_domain_poly_new <- aggregate(State_Tigerlines)
  values(all_merge_domain_poly_new) <- t(all_merge_domain_new)
}


#compare numeric - all agree

names(all_merge_state_new) <- gsub("EIA_","",names(all_merge_state_new))
names(all_merge_state_new) <- gsub("PHMSA_","",names(all_merge_state_new))
names(all_merge_state_new) <- gsub("GHGRP_","",names(all_merge_state_new))

#naming/order differs, make consistent.  First 10 differ in name, but are
#identical.  New has some extra channels and removes 3 (GHGRP activity data is
#saved differently).
old_col_subset <- c(1:32,(33:70)[colnames(all_merge_state)[33:70] %in% colnames(all_merge_state_new)[33:82]])
new_col_subset <- c(colnames(all_merge_state_new)[1:31],colnames(all_merge_state)[33:70][colnames(all_merge_state)[33:70] %in% colnames(all_merge_state_new)[32:82]])
new_col_subset <- c("State",new_col_subset)

all_merge_state <- all_merge_state[order(all_merge_state$State),old_col_subset]
all_merge_state_new <- all_merge_state_new[order(all_merge_state_new$State),new_col_subset]

all(all_merge_state[,1:32]==all_merge_state_new[,1:32])
delta <- all_merge_state[,c(33:69)]-all_merge_state_new[,c(33:69)]
# View(delta[1:3,])
all(round(delta,8)==0)




names(all_merge_domain_new) <- gsub("EIA_","",names(all_merge_domain_new))
names(all_merge_domain_new) <- gsub("PHMSA_","",names(all_merge_domain_new))
names(all_merge_domain_new) <- gsub("GHGRP_","",names(all_merge_domain_new))

#naming/order differs, make consistent.  First 34 differ in name, but are
#identical.  New has some extra channels and removes 3 (GHGRP activity data is
#saved differently).
old_col_subset <- c(1:31,(32:69)[names(all_merge_domain)[32:69] %in% names(all_merge_domain_new)[32:82]])
new_col_subset <- c(names(all_merge_domain_new)[1:31],names(all_merge_domain)[32:69][names(all_merge_domain)[32:69] %in% names(all_merge_domain_new)[32:82]])

all_merge_domain <- all_merge_domain[old_col_subset]
all_merge_domain_new <- all_merge_domain_new[new_col_subset]

delta <- all_merge_domain-all_merge_domain_new
all(round(delta,8)==0)
sort(abs(delta))

#the only one that differs by more than 1E-5; -4.325679e-03
all_merge_domain["Miles_main_and_serv"] - all_merge_domain_new["Miles_main_and_serv"]

#each individual state agrees ~ exactly
all_merge_state[,"Miles_main_and_serv"]
all_merge_state_new[,"Miles_main_and_serv"]








#compare spatial - agree, just differ in that state outlines are cb rather than
#tl in old ones
plot(vect(all_merge_state_sf))
plot(all_merge_state_poly_new,add=T,border="red")

plot(vect(shp_domain_sf))
plot(all_merge_domain_poly,add=T,border="red")

################################################################################
#Now load in the shapefiles and vulcan, merge geometries

#old
{
  # Load in ACES and Vulcan sectors - these are in different units and one is an
  # annual sum the other an annual average, but it doesn't matter as we'll only
  # use fractions
  
  aces_res <- raster(file.path(ACES_directory,"Sectoral",paste0(ACES_year,'_Annual_ACES_Residential.nc')))
  aces_com <- raster(file.path(ACES_directory,"Sectoral",paste0(ACES_year,'_Annual_ACES_Commercial.nc')))
  vu_res <- raster(file.path(vulcan_directory,"Sectoral","Vulcan_v3_US_annual_1km_residential_mn.nc4"), varname='carbon_emissions',band=vulcan_band)
  vu_com <- raster(file.path(vulcan_directory,"Sectoral",'Vulcan_v3_US_annual_1km_commercial_mn.nc4'), varname='carbon_emissions',band=vulcan_band)

  # Change nans to zeros otherwise they could mess with the regridding later
  aces_res[is.na(aces_res)] <- 0
  aces_com[is.na(aces_com)] <- 0
  vu_res[is.na(vu_res)] <- 0
  vu_com[is.na(vu_com)] <- 0

  # Going to assume that ACES and Vulcan have the same CRS - check that here
  if(!compareCRS(aces_res,vu_res)){
    stop('Code assumes CO2 inventories have the same CRS')
  }
  
  # Transform to ACES/Vulcan CRS
  all_merge_sf_LCC <- st_transform(all_merge_state_sf, crs(vu_res))
  
  # Convert all_merge_sf_LCC to Spatial so we can use it with raster more easily
  all_merge_sp_LCC <- as(all_merge_sf_LCC, 'Spatial')
  
  # Get the fraction of each cell covered by each polygon - this is much quicker that rasterize(getCover=T)
  # although it does have strange bug (as of raster_3.4-5) that calculates weights that are exactly a factor of 100 too low
  # i.e. they give 0.01 when the whole cell is covered
  aces_cover_all <- cellFromPolygon(aces_res, all_merge_sp_LCC, weights = TRUE)
  vu_cover_all <- cellFromPolygon(vu_res, all_merge_sp_LCC, weights = TRUE)
  
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
  #the various subsectors

}

#new
{
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
  #the various subsectors

  #convert state scale versions to the proper crs
  all_merge_LCC_state <- project(all_merge_state_poly_new,aces_res)
  #Calculate per-pixel coverage for each county separately.  First split by
  #unique state-county number, then calculate per-pixel coverage, output = list
  #of spatvectors
  cover_all_aces <- all_merge_LCC_state %>% 
    split(f=paste0(all_merge_LCC_state$STATEFP)) %>%
    lapply(function(x){extract(rast(aces_res),x,weights=T,exact=T,cells=T)})
  
  cover_all_vulcan <- all_merge_LCC_state %>% 
    split(f=paste0(all_merge_LCC_state$STATEFP)) %>%
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

#Most agree well, but each state has some pixels with 100% disagreement.  Likely
#the water areas being an issue...

checker(1)
checker(2)
checker(3)
checker(4)
checker(5)


################################################################################
# Set up lists of rasters for calculating ch4 emissions, with one raster for each subsector
aces_template <- aces_res
aces_template[] <- 0

aces_res_ch4_bystate <- replicate(length(res_totals), aces_template)
names(aces_res_ch4_bystate) <- res_totals
aces_com_ch4_bystate <- replicate(length(com_totals), aces_template)
names(aces_com_ch4_bystate) <- com_totals

aces_res_ch4_bydomain <- replicate(length(res_totals), aces_template)
names(aces_res_ch4_bydomain) <- res_totals
aces_com_ch4_bydomain <- replicate(length(com_totals), aces_template)
names(aces_com_ch4_bydomain) <- com_totals

vu_template <- vu_res
vu_template[] <- 0

vu_res_ch4_bystate <- replicate(length(res_totals), vu_template)
names(vu_res_ch4_bystate) <- res_totals
vu_com_ch4_bystate <- replicate(length(com_totals), vu_template)
names(vu_com_ch4_bystate) <- com_totals

vu_res_ch4_bydomain <- replicate(length(res_totals), vu_template)
names(vu_res_ch4_bydomain) <- res_totals
vu_com_ch4_bydomain <- replicate(length(com_totals), vu_template)
names(vu_com_ch4_bydomain) <- com_totals






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

input_inventory_ch4 <- replicate(length(res_totals), template_aces)
names(input_inventory_ch4) <- res_totals
aces_res_ch4_bystate_new <- input_inventory_ch4
aces_res_ch4_bydomain_new <- input_inventory_ch4
input_inventory_ch4 <- replicate(length(com_totals), template_aces)
names(input_inventory_ch4) <- com_totals
aces_com_ch4_bystate_new <- input_inventory_ch4
aces_com_ch4_bydomain_new <- input_inventory_ch4

################################################################################
#now compare the approaches for disaggregating within county

for(i in 1:length(vu_cover_all)){
  
  
  
  #old
  {
    #res
    
    # #using the old aces cover - I want to compare the shapefile impact this time
    # aces_cover <- aces_cover_all[[i]]
    # vu_cover <- vu_cover_all[[i]]
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
                                            as.numeric(st_drop_geometry(all_merge_sf_LCC[i, total])))
      vu_res_ch4_bystate[[total]][] <- (vu_res_ch4_bystate[[total]][] +
                                          vu_res_frac[]*
                                          as.numeric(st_drop_geometry(all_merge_sf_LCC[i, total])))
    }

    
    
    
    
    #being lazy - copy pasted code and ctrl+R res for each sector rather than
    #rewriting as a function.  Avoids risk of function working a little
    #differently anyway.
    
    #com
    
    # #using the old aces cover - I want to compare the shapefile impact this time
    # aces_cover <- aces_cover_all[[i]]
    # vu_cover <- vu_cover_all[[i]]
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
                                            as.numeric(st_drop_geometry(all_merge_sf_LCC[i, total])))
      vu_com_ch4_bystate[[total]][] <- (vu_com_ch4_bystate[[total]][] +
                                          vu_com_frac[]*
                                          as.numeric(st_drop_geometry(all_merge_sf_LCC[i, total])))
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
    }
    # Loop through the different subsectors, and add the CH4 emissions map to
    # the relevant raster
    
    cat("\rFinished mapping com state","level entry",i,"of",length(cover_all_vulcan),"        ")
    
    }

}
################################################################################
#write a function to compare the output either across the final map or 1 county
#at a time (that part is more useful if manually walking through the loop above)

divergent <- colorRampPalette(c("red","white","blue"))

dir.create(plot_directory,showWarnings = F)

checker <- function(total,rawinventory,scale){
  sector <- tail(strsplit(total,"_")[[1]],1)
  
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
  
  delta <- rast(old_vu[[total]]) - new_vu[[total]]
  delta[values(delta)==0] <- NA
  new_vu[[total]][values(new_vu[[total]])==0] <- NA
  old_vu[[total]][values(old_vu[[total]])==0] <- NA
  
    png(paste0(plot_directory,"/NG_dist_",total,"_Vulcan_",scale,"_delta.png"))
    plot(delta,main=paste0("overall ",scale," Vulcan old - new ",total),ext=ext(project(State_Tigerlines,delta)),
         range=unlist(global(abs(delta),max,na.rm=T))*c(-1,1),
         colNA="black",
         plg=list(title="mol/km2/s"),
         xlab="Longitude",ylab="Latitude",
         col=divergent(64),mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
    lines(project(State_Tigerlines,delta),col="dimgrey")
    dev.off()
    
    png(paste0(plot_directory,"/NG_dist_",total,"_Vulcan_",scale,"_new.png"))
    plot(new_vu[[total]],main=paste0("overall ",scale," Vulcan new ",total),ext=project(State_Tigerlines,delta),
         colNA="black",
         plg=list(title="mol/km2/s"),
         xlab="Longitude",ylab="Latitude",mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
    lines(project(State_Tigerlines,delta),col="dimgrey")
    dev.off()
    
    png(paste0(plot_directory,"/NG_dist_",total,"_Vulcan_",scale,"_old.png"))
    plot(rast(old_vu[[total]]),main=paste0("overall ",scale," Vulcan old ",total),ext=project(State_Tigerlines,delta),
         colNA="black",
         plg=list(title="mol/km2/s"),
         xlab="Longitude",ylab="Latitude",mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
    lines(project(State_Tigerlines,delta),col="dimgrey")
    dev.off()

    cat("\nCounty",total,scale,"Vulcan range delta = ",unlist(global(delta,range,na.rm=T)),
        file=paste0(plot_directory,"/Vulcan_",total,"_",scale,".txt"),append=T)
    cat("\nnew=",unlist(global(new_vu[[total]],sum,na.rm=T)),
        file=paste0(plot_directory,"/Vulcan_",total,"_",scale,".txt"),append=T)
    cat("\nold=",unlist(global(rast(old_vu[[total]]),sum,na.rm=T)),
        file=paste0(plot_directory,"/Vulcan_",total,"_",scale,".txt"),append=T)
    cat("\nold/new=",unlist(global(rast(old_vu[[total]]),sum,na.rm=T)/global(new_vu[[total]],sum,na.rm=T)),
        file=paste0(plot_directory,"/Vulcan_",total,"_",scale,".txt"),append=T)

  
  
  
  
  delta <- rast(old_ACES[[total]]) - new_ACES[[total]]
  delta[values(delta)==0] <- NA
  new_ACES[[total]][values(new_ACES[[total]])==0] <- NA
  old_ACES[[total]][values(old_ACES[[total]])==0] <- NA
  
    png(paste0(plot_directory,"/NG_dist_",total,"_ACES_",scale,"_delta.png"))
    plot(delta,main=paste0("overall ",scale," ACES old - new ",total),ext=ext(project(State_Tigerlines,delta)),
         range=unlist(global(abs(delta),max,na.rm=T))*c(-1,1),
         colNA="black",
         plg=list(title="mol/km2/s"),
         xlab="Longitude",ylab="Latitude",
         col=divergent(64),mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
    lines(project(State_Tigerlines,delta),col="dimgrey")
    dev.off()
    
    png(paste0(plot_directory,"/NG_dist_",total,"_ACES_",scale,"_new.png"))
    plot(new_ACES[[total]],main=paste0("overall ",scale," ACES new ",total),ext=ext(project(State_Tigerlines,delta)),
         colNA="black",
         plg=list(title="mol/km2/s"),
         xlab="Longitude",ylab="Latitude",mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
    lines(project(State_Tigerlines,delta),col="dimgrey")
    dev.off()
    
    png(paste0(plot_directory,"/NG_dist_",total,"_ACES_",scale,"_old.png"))
    plot(rast(old_ACES[[total]]),main=paste0("overall ",scale," ACES old ",total),ext=ext(project(State_Tigerlines,delta)),
         colNA="black",
         plg=list(title="mol/km2/s"),
         xlab="Longitude",ylab="Latitude",mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
    lines(project(State_Tigerlines,delta),col="dimgrey")
    dev.off()
    
    cat("\nCounty",total,scale,"ACES range delta = ",unlist(global(delta,range,na.rm=T)),
        file=paste0(plot_directory,"/ACES_",total,"_",scale,".txt"),append=T)
    cat("\nnew=",unlist(global(new_ACES[[total]],sum,na.rm=T)),
        file=paste0(plot_directory,"/ACES_",total,"_",scale,".txt"),append=T)
    cat("\nold=",unlist(global(rast(old_ACES[[total]]),sum,na.rm=T)),
        file=paste0(plot_directory,"/ACES_",total,"_",scale,".txt"),append=T)
    cat("\nold/new=",unlist(global(rast(old_ACES[[total]]),sum,na.rm=T)/global(new_ACES[[total]],sum,na.rm=T)),
        file=paste0(plot_directory,"/ACES_",total,"_",scale,".txt"),append=T)
}

################################################################################

for(total in res_totals){
  checker(total,rawinventory=FALSE,scale="state")
  # checker(total,rawinventory=FALSE,scale="domain")
}
for(total in com_totals){
  checker(total,rawinventory=FALSE,scale="state")
  # checker(total,rawinventory=FALSE,scale="domain")
}

