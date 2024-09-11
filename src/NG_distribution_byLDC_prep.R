
#this is a script meant to assist the user in building a shapefile for the byLDC
#option of NG_distribution_emissions.R.  This cannot be completely automated,
#but there are some steps that can be automated to simplify the process.

#copied from main - still need domain and state tigerlines.  Requires some user
#input in top section.  Includes an extra package or so compared to main.
{
  ################################################################################
  #User input
  input_directory="G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw_data_rewrite/"
  output_directory="G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_rewrite/"
  plot_directory="G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Figures_rewrite/"
  inventory_year=2019
  domain=as.data.frame(cbind(c(-76.65,-73.65),
                             c(38.97,40.97)))
  domain_res=0.01
  domain_crs="epsg:4326" #lat/long
  
  EIA_file = file.path(input_directory,"176 Type of Operations and Sector Items.xlsx")
  PHMSA_file = file.path(input_directory,"annual_gas_distribution_2010_present/annual_gas_distribution_2019.xlsx")
  #T/F to indicate whether to overwrite the xl files that will need to be
  #manually edited, only needed if rerunning and you want to recreate the
  #initial output.  Here as this output will need to be manually edited and you
  #don't want to overwrite your work.
  overwrite_xl = F
  ################################################################################
  #load all packages necessary throughout processing
  
  packagecheck <- c("terra", "ncdf4", "readxl","jsonlite","dplyr","xlsx","sf")

  #quick way to install only packages that are not already installed
  i=1
  while(i<=length(packagecheck)){
    if(length(find.package(packagecheck[i],quiet = TRUE))<1){
      install.packages(packagecheck[i],repos="https://repo.miserver.it.umich.edu/cran/")
    }
    i <- i+1
  }
  
  suppressPackageStartupMessages(invisible(lapply(packagecheck, library, character.only=TRUE)))
  rm(packagecheck,i)
  
  #terra = raster dataclasses and processing functions
  #ncdf4 = .nc filetype functions
  #readxl = enables loading in .xlsx or similar filetypes
  #jsonlite = allows simple loading of JSON files, primarily for downloading input data via API
  #xlsx = write xlsx files
  #sf = many spatial dataclasses and functions.  Issues with terra for certain aspects
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
  ################################################################################
  #load in Census tigerlines necessary 

  Census_filenames <- c(paste0(input_directory,"State_Tigerlines/tl_",inventory_year,"_us_state.shp"),
                        paste0(input_directory,"County_Tigerlines/tl_",inventory_year,"_us_county.shp"))
  
  if(!all(file.exists(Census_filenames))){
    #URLs for state and county shapefiles
    Census_FTP_URLs <- c(paste0("https://www2.census.gov/geo/tiger/TIGER",inventory_year,"/STATE/tl_",inventory_year,"_us_state.zip"),
                         paste0("https://www2.census.gov/geo/tiger/TIGER",inventory_year,"/COUNTY/tl_",inventory_year,"_us_county.zip"))
    download_location <- tempfile(fileext = ".zip")
    #download each to a temp file then unzip to the input directory
    for(A in 1:length(Census_FTP_URLs)){
      download.file(Census_FTP_URLs[A],destfile = download_location,quiet = T)
      unzip(download_location,exdir=file.path(input_directory,c("State_Tigerlines","County_Tigerlines")[A]))
    }
    #delete the temp file
    unlink(download_location)
    rm(Census_FTP_URLs,download_location,A)
  }
  
  #load them in
  State_Tigerlines <- vect(Census_filenames[1])
  County_Tigerlines <- vect(Census_filenames[2])
  
  #project to match the domain (crs)
  State_Tigerlines <- project(State_Tigerlines,domain)
  County_Tigerlines <- project(County_Tigerlines,domain)
  
  #subset to just those relevant for the domain (speedier)
  State_Tigerlines <- mask(State_Tigerlines,mask=as.polygons(domain))
  County_Tigerlines <- crop(County_Tigerlines,State_Tigerlines)
  
  #sort by state abbreviation
  State_Tigerlines <- State_Tigerlines[order(State_Tigerlines$STUSPS),]
  
  #save the states in the domain for use
  state_name_list <- State_Tigerlines$STUSPS
  
  rm(Census_filenames)
  ################################################################################
  #Extend to load census data on water areas - as these should not have NG
  #distribution emissions at all.
  
  #see
  #https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2019/TGRSHP2019_TechDoc.pdf#%5B%7B%22num%22%3A382%2C%22gen%22%3A0%7D%2C%7B%22name%22%3A%22XYZ%22%7D%2C69%2C259%2C0%5D
  state_county_list <- sort(unique(paste0(County_Tigerlines$STATEFP,County_Tigerlines$COUNTYFP)))
  
  Census_filenames <- paste0(input_directory,"Area_Water/tl_",inventory_year,"_",state_county_list,"_areawater.shp")
  
  if(!all(file.exists(Census_filenames))){
    #URLs for state, county, and urban shapefiles
    Census_FTP_URLs <- paste0("https://www2.census.gov/geo/tiger/TIGER",inventory_year,"/AREAWATER/tl_",inventory_year,"_",
                              state_county_list,"_areawater.zip")
    download_location <- tempfile(fileext = ".zip")
    #download each to a temp file then unzip to the input directory
    for(A in 1:length(Census_FTP_URLs)){
      download.file(Census_FTP_URLs[A],destfile = download_location,quiet = T)
      unzip(download_location,exdir=file.path(input_directory,"Area_Water"))
    }
    #delete the temp file
    unlink(download_location)
    rm(Census_FTP_URLs,download_location,A)
  }
  
  Water_Tigerlines <- lapply(Census_filenames,vect)
  Water_Tigerlines <- vect(Water_Tigerlines)
  
  #project to match the domain (crs)
  Water_Tigerlines <- project(Water_Tigerlines,domain)

  rm(Census_filenames,state_county_list)
}


#first, grab all GHGRP data for the states in the domain (even if outside the
#domain itself).  No input needed.  

#A user update will appear describing GHGRP facilities whose state is rewritten
#as many list the state of the owners/headquarters rather than that of
#operation.  It uses names to do this as they generally clarify (e.g., Atmos
#Energy - Kansas has state = TX, but operates in Kansas)
{
  ################################################################################
  #Download the relevant ghgrp emissions data using the API
  #(https://www.epa.gov/enviro/envirofacts-data-service-api) and combine the
  #facility and emission data appropriately
  
  #download the relevant LDC-sector data
  #(https://www.epa.gov/enviro/greenhouse-gas-model).  W = emissions, NN =
  #volume delivered per customer type.
  ghgrp_w_only_emissions <- fromJSON("https://data.epa.gov/efservice/ef_w_emissions_source_ghg/JSON")
  ghgrp_NN_data <- fromJSON("https://data.epa.gov/efservice/nn_ldc_nat_gas_deliveries/JSON")
  
  #because we're getting sub-facility level information, first we need to
  #aggregate.  Subsetting to only the year of interest now instead of later.
  ghgrp_w_only_emissions <- ghgrp_w_only_emissions[ghgrp_w_only_emissions$reporting_year==inventory_year,]
  ghgrp_w_only_emissions <- ghgrp_w_only_emissions[ghgrp_w_only_emissions$industry_segment=="Natural gas distribution [98.230(a)(8)]",]
  ghgrp_w_only_emissions <- aggregate(ghgrp_w_only_emissions$total_reported_ch4_emissions,
                                      by=list(ghgrp_w_only_emissions$facility_id,
                                              ghgrp_w_only_emissions$facility_name),
                                      sum,na.rm=T)
  
  ghgrp_NN_data <- ghgrp_NN_data[ghgrp_NN_data$reporting_year==inventory_year,]
  
  #reshape NN data so instead of multiple rows, 1 per customer type, each
  #customer type is a different column.
  ghgrp_NN_data <- reshape(ghgrp_NN_data,idvar = c("facility_id","facility_name","reporting_year"),
                           timevar = "end_user_category",direction = "wide")
  
  
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
  GHGRP_csv=Reduce(function(dtf1, dtf2){merge(dtf1, dtf2, by = c("facility_id"))},
                   list(ghgrp_facility_info,
                        ghgrp_w_only_emissions,
                        ghgrp_NN_data))
  
  #convert the relevant columns to numeric class
  GHGRP_csv[,c("latitude","longitude","Reported_CH4",
               "volume_delivered.Electricity generating facilities",
               "volume_delivered.Residential consumers",
               "volume_delivered.Commercial consumers",
               "volume_delivered.Industrial consumers")] <- apply(GHGRP_csv[,c("latitude","longitude","Reported_CH4",
                                                                               "volume_delivered.Electricity generating facilities",
                                                                               "volume_delivered.Residential consumers",
                                                                               "volume_delivered.Commercial consumers",
                                                                               "volume_delivered.Industrial consumers")],
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
      match_indx <- grepl(pattern = paste0("\\b",state.name[A],"\\b"),
                          x=GHGRP_csv$facility_name,ignore.case = T) & GHGRP_csv$state!=state.abb[A]
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
  
  #cleanup a column that is in all 3 GHRGP datasets and are identical (or ~so).
  GHGRP_csv[,c("facility_name.x","facility_name.y")] <- NULL

  #delete all tempfiles and clean up working environment
  rm(A,data_URLs,ghgrp_facility_info,ghgrp_w_only_emissions,match_indx,ghgrp_NN_data)
  ################################################################################
  #do some webscraping to add a few additional variables for GHGRP facilities

  #save to the temp file destination.  Add several new blank variables to GHGRP_csv
  download_dest <- tempfile(fileext = ".html")
  GHGRP_csv[,c("Miles_of_Mains","N_of_above_grade_T-D_transfer_stations","N_of_above_grade_non_T-D_MR_stations",
               "N_of_below_grade_T-D_transfer_stations","N_of_below_grade_non_T-D_MR_stations")] <- 0

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

    #Now read in the whole html as text
    HTML_data <- readChar(download_dest,file.info(download_dest)$size)

    #initialize an output and locate some text near data we want (amount of
    #pipeline of various pipe types)
    text_loc <- gregexpr("Distribution Mains, Gas Service",text = HTML_data)
    answer <- 0
    #should have found 1 value for each type of pipeline
    for(B in 1:length(text_loc[[1]])){
      #see https://www.debuggex.com/cheatsheet/regex/pcre
      text <- substr(HTML_data,text_loc[[1]][B],text_loc[[1]][B]+attributes(text_loc[[1]])$match.length[B]+200)
      #first subset to the located text + buffer, then regex to find a number
      #with/without a decimal in it as formatted html text, then grab just this
      #value and add it to the answer (we only want the total across all pipeline
      #types)
      sub_answer <- regexpr(">[[:digit:]]*[[:punct:]]*[[:digit:]]*</td>",text)
      answer <- answer+as.numeric(substr(text,sub_answer+1,sub_answer+attributes(sub_answer)$match.length-6))
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

    #user update
    cat("\rFinished downloading GHGRP data for",A,"of",nrow(GHGRP_csv),"                 ")
  }

  #attempt to remove the downloaded html file
  unlink(download_dest)
  rm(text_loc,A,B,answer,counter,download_dest,HTML_data,info,sub_answer,text)
}


#prepare the other datafiles.  No input needed.
{
  #note that the dates are not loaded in properly from this
  HIFLD_shp <- vect("https://services1.arcgis.com/Hp6G80Pky0om7QvQ/arcgis/rest/services/Natural_Gas_Service_Territories/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
  #very unlikely to be important, but there are a few HIFLD shapes that have the
  #same shape ID.  Rename them to ensure each is unique.  Those that were
  #duplicated seemed to be several states apart.  Just adding a,b,c etc.
  for(A in 1:26){
    dup_indx <- duplicated(HIFLD_shp$SVCTERID)
    if(sum(dup_indx)>0){
      HIFLD_shp$SVCTERID[dup_indx] <- paste0(HIFLD_shp$SVCTERID[dup_indx],letters[A])
    }else{
      break
    }
  }
  
  #convert to dataframe, this will be edited to match to the other datasets later.
  HIFLD_csv <- as.data.frame(HIFLD_shp)
  HIFLD_csv[,c("VAL_DATE","SOURCEDATE")] <- NA
  
  # Load the EIA company-level data
  EIA_csv <- read_xlsx(EIA_file,skip=1,col_names = T)
  # Load the PHMSA data
  PHMSA_csv <- read_xlsx(PHMSA_file,skip=2,col_names = T)
  
  # Filter the PHMSA file by commodity
  PHMSA_csv_NG <- PHMSA_csv[which(PHMSA_csv$COMMODITY == 'Natural Gas'),]
  
  #make the EIA and HIFLD ID's comparable
  HIFLD_csv$COMPID <- paste0(HIFLD_csv$COMPID,HIFLD_csv$LDC_STATE)

  #filter to only those for the relevant states and those with a company ID in
  #HIFLD (just check it's present at all).
  PHMSA_csv_NG <- PHMSA_csv_NG[which(PHMSA_csv_NG$STOP%in%state_name_list),]
  HIFLD_csv <- HIFLD_csv[HIFLD_csv$LDC_STATE %in% state_name_list,]
  HIFLD_csv <- HIFLD_csv[HIFLD_csv$COMPID!="NOT AVAILABLE",]
  HIFLD_csv <- HIFLD_csv[!is.na(HIFLD_csv$COMPID),]
  EIA_csv <- EIA_csv[which(EIA_csv$State %in% state_name_list),]
  
  rm(PHMSA_csv,EIA_file,PHMSA_file)
}

#identify likely matches across the 4 datasets.  These are NOT definitive.  The
#datasets represent slightly different years and as such facilities can change
#quite a lot (merge, split, expand, etc.).  It just identifies those with the
#most similar activity data.  No input needed.

#Here the data is saved to the input folder and should be manually edited to
#align all of the datasets.  Users should manually compare the names and search
#the internet to identify appropriate matches.  All files should have a common
#COMPID or Company ID column.  The likely matches have been saved to the xlsx
#files, but the user will need to update these.  Some have no match, some
#matches may be wrong, and some may have multiple which cannot remain the case
#(if desired, the facility must be listed 2x with 2 different IDs, not listed 1x
#with 2 IDs). These are also limited to matches to GHGRP facilities and between
#HIFLD and EIA.

#PHMSA facilities without a match should have the ID set to OTHER.  HIFLD
#facilities without a match should be removed. Deleting the ID alone is
#sufficient (code will remove it from there).  EIA data doesn't need to be
#edited.  Matches must exist across all 3 datasets (HIFLD, PHMSA, EIA).

#PHMSA and GHGRP (if available) provide activity data to calculate emissions,
#HIFLD data provides polygons for each LDC operating territory, and EIA provides
#a customer breakdown used to help distribute emissions from LDC to pixel
#resolution.  This is why no PHMSA data is removed (the "OTHER" is distributed
#to any regions without a known LDC).
{
  #find PHMSA - GHGRP matches by comparing the miles of pipeline
  PHMSA_matches <- sapply(GHGRP_csv$Miles_of_Mains,FUN=function(x){
    which.min(abs(x - PHMSA_csv_NG$MMILES_TOTAL))})
  PHMSA_match_data <- PHMSA_csv_NG[PHMSA_matches,]
  
  #compare EIA to GHGRP by identifying the most similar volume of gas delivered
  #for each type of customer.
  EIA_csv <- as.data.frame(EIA_csv)
  EIA_matches1 <- sapply(GHGRP_csv$`volume_delivered.Commercial consumers`,FUN=function(x){
    which.min(abs(x - EIA_csv$`Commercial Total Volume (Mcf)`))})
  EIA_matches2 <-
    sapply(GHGRP_csv$`volume_delivered.Electricity generating facilities`,FUN=function(x){
      which.min(abs(x - EIA_csv$`Electric Total Volume (Mcf)`))})
  EIA_matches3 <-
    sapply(GHGRP_csv$`volume_delivered.Residential consumers`,FUN=function(x){
      which.min(abs(x - EIA_csv$`Residential Total Volume (Mcf)`))})
  EIA_matches4 <-
    sapply(GHGRP_csv$`volume_delivered.Industrial consumers`,FUN=function(x){
      which.min(abs(x - EIA_csv$`Industrial Total Volume (Mcf)`))})
  
  EIA_matches <- cbind(EIA_matches1,EIA_matches2,EIA_matches3,EIA_matches4)
  
  #if all 4 types best matched a different facility, we'll consider that no
  #match and set the value to a very large value.  When used as an index, this
  #will result in NA since it's longer than the dataset.
  EIA_matches[apply(EIA_matches,1,function(x){sum(duplicated(x))})==0] <- 1E10
  
  #The match is whichever was the closest match most often
  EIA_matches <- apply(EIA_matches,1,function(x) names(which.max(table(x))))
  EIA_match_data <- EIA_csv[EIA_matches,]
  
  
  #no longer used.  Alternative to the above, instead finding the facility with
  #the smallest difference across all 4 types of customer.  Performed worse.
  # EIA_matches1 <- sapply(GHGRP_csv$`volume_delivered.Commercial consumers`,FUN=function(x){
  #   abs(x - EIA_csv$`Commercial Total Volume (Mcf)`)})
  # EIA_matches2 <- 
  #   sapply(GHGRP_csv$`volume_delivered.Electricity generating facilities`,FUN=function(x){
  #     abs(x - EIA_csv$`Electric Total Volume (Mcf)`)})
  # EIA_matches3 <- 
  #   sapply(GHGRP_csv$`volume_delivered.Residential consumers`,FUN=function(x){
  #     abs(x - EIA_csv$`Residential Total Volume (Mcf)`)})
  # EIA_matches4 <- 
  #   sapply(GHGRP_csv$`volume_delivered.Industrial consumers`,FUN=function(x){
  #     abs(x - EIA_csv$`Industrial Total Volume (Mcf)`)})
  # 
  # EIA_matches <- EIA_matches1
  # EIA_matches[] <- NA
  # for(A in 1:nrow(EIA_matches1)){
  #   for(B in 1:ncol(EIA_matches1)){
  #     if(all(is.na(EIA_matches1[A,B]),is.na(EIA_matches2[A,B]),is.na(EIA_matches3[A,B]),is.na(EIA_matches4[A,B]))){
  #       EIA_matches[A,B] <- NA
  #     }else{
  #     EIA_matches[A,B] <- sum(EIA_matches1[A,B],EIA_matches2[A,B],EIA_matches3[A,B],EIA_matches4[A,B],na.rm=T)
  #     }
  #   }
  # }
  # EIA_matches <- apply(EIA_matches,2,which.min)
  # EIA_match_data <- EIA_csv[EIA_matches,]
  # 
  # View(cbind(EIA_match_data[,c("Company Name","Electric Total Volume (Mcf)","Industrial Total Volume (Mcf)","Commercial Total Volume (Mcf)","Residential Total Volume (Mcf)")],
  #            GHGRP_csv[,c("facility_name","volume_delivered.Electricity generating facilities","volume_delivered.Industrial consumers","volume_delivered.Commercial consumers","volume_delivered.Residential consumers")]))
  
  
  #match HIFLD and EIA as they have a common ID across them.  Matching just to
  #EIA data already matched to GHGRP.
  HIFLD_match_data <- merge(x=EIA_match_data,y=HIFLD_csv,by.x="Company",by.y="COMPID",all.x=T)
  HIFLD_match_data <- HIFLD_match_data[,c("Company",colnames(subset(HIFLD_csv, select = -c(COMPID))))]
  HIFLD_match_data <- HIFLD_match_data[match(EIA_match_data$Company,HIFLD_match_data$Company),]

  #combine all the GHGRP matched data and give simpler column names
  GHGRP_matched_dataset <- cbind(EIA_match_data$Company,HIFLD_match_data$Company,
                                 GHGRP_csv$facility_name,PHMSA_match_data$OPERATOR_NAME,EIA_match_data$`Company Name`,HIFLD_match_data$NAME,
                                 GHGRP_csv$Miles_of_Mains,PHMSA_match_data$MMILES_TOTAL,
                                 GHGRP_csv$`volume_delivered.Commercial consumers`,EIA_match_data$`Commercial Total Volume (Mcf)`,
                                 GHGRP_csv$`volume_delivered.Electricity generating facilities`,EIA_match_data$`Electric Total Volume (Mcf)`,
                                 GHGRP_csv$`volume_delivered.Industrial consumers`,EIA_match_data$`Industrial Total Volume (Mcf)`,
                                 GHGRP_csv$`volume_delivered.Residential consumers`,EIA_match_data$`Residential Total Volume (Mcf)`)
  colnames(GHGRP_matched_dataset) <- c("EIA_ID","HIFLD_ID",
                                       "GHGRP_name","PHMSA_name","EIA_name","HIFLD_name",
                                       "GHGRP_miles_of_mains","PHMSA_miles_of_mains",
                                       "GHGRP_volume_delivered_commercial_consumers","EIA_volume_delivered_commercial_consumers",
                                       "GHGRP_volume_delivered_electricity_consumers","EIA_volume_delivered_electricity_consumers",
                                       "GHGRP_volume_delivered_industrial_consumers","EIA_volume_delivered_industrial_consumers",
                                       "GHGRP_volume_delivered_residential_consumers","EIA_volume_delivered_residential_consumers")
  
  #match HIFLD and EIA as they have a common ID across them.  Matching to the
  #entire EIA dataset this time.
  HIFLD_match_data <- merge(x=EIA_csv,y=HIFLD_csv,by.x="Company",by.y="COMPID",all.x=T)
  HIFLD_match_data <- HIFLD_match_data[,c("Company",colnames(subset(HIFLD_csv, select = -c(COMPID))))]
  HIFLD_match_data <- HIFLD_match_data[match(EIA_csv$Company,HIFLD_match_data$Company),]
  
  EIA_matched_dataset <- cbind(EIA_csv$Company,HIFLD_match_data$Company,
                               EIA_csv$`Company Name`,HIFLD_match_data$NAME)
  colnames(EIA_matched_dataset) <- c("EIA_ID","HIFLD_ID","EIA_name","HIFLD_name")
  
  #user update + popping open these tables to give the user a chance to use this
  #in updating the xl files.
  cat("Facilities across the datasets were matched to the GHGRP data using miles of pipeline (PHMSA) and volume of gas delivered by customer type (EIA).  HIFLD and EIA have consistent ID's, so were matched using those.  The GHGRP_matched_dataset and EIA_matched_dataset tables can help manually edit the xl files.\n")
  View(GHGRP_matched_dataset)
  View(EIA_matched_dataset)
  
  #Add a blank row with a few columns set to DUMMY.  These are meant to be a
  #catch-all for LDCs without shapefiles to be distributed across the remainder
  #of the state.
  EIA_csv[nrow(EIA_csv)+1,] <- NA
  EIA_csv[nrow(EIA_csv),1:4] <- c(EIA_csv$Year[1],"DUMMY","OTHER","DUMMY")
  HIFLD_csv[nrow(HIFLD_csv)+1,] <- NA
  HIFLD_csv[nrow(HIFLD_csv),c("OBJECTID","SVCTERID","NAME","COMPID")] <- c(rep("DUMMY",3),"OTHER")
  
  #Add a new ID column and grab those ID'd already from GHGRP_matched_dataset.
  GHGRP_csv$`Company ID` <- GHGRP_matched_dataset[,"EIA_ID"]
  PHMSA_csv_NG <- as.data.frame(PHMSA_csv_NG)
  PHMSA_csv_NG$Company_ID <- NA
  #find those that match report number since match data doesn't have company ID
  #and this is a similarly unique ID
  PHMSA_indx <- sapply(PHMSA_csv_NG$REPORT_NUMBER,FUN = function(x){which(PHMSA_match_data$REPORT_NUMBER==x)})
  for(A in 1:length(PHMSA_indx)){
    if(length(PHMSA_indx[[A]])>0){
      #report numbers aren't totally unique, some are reused in different
      #states.  filter to just those that match report and state now.
      PHMSA_indx[[A]] <- PHMSA_indx[[A]][PHMSA_csv_NG$STOP[A] == substr(GHGRP_matched_dataset[PHMSA_indx[[A]],"EIA_ID"],9,10)]
      #pull and combine if multiple, just in case report number was reused even
      #within state.
      PHMSA_csv_NG$Company_ID[A] <- paste(GHGRP_matched_dataset[PHMSA_indx[[A]],"EIA_ID"],collapse=" & ")
    }
  }
  #for those that had an NA EIA ID, but matching PHMSA report, NA is character.
  #Replace with proper NA.
  PHMSA_csv_NG$Company_ID[which(PHMSA_csv_NG$Company_ID=="NA")] <- NA

  #rename the state data for these for clarity/consistency
  colnames(PHMSA_csv_NG) <- gsub("STOP","State",colnames(PHMSA_csv_NG))
  HIFLD_csv$STATE <- HIFLD_csv$LDC_STATE
  HIFLD_csv$LDC_STATE <- NULL
  
  #keep only the equivalent data in the shapefile now
  HIFLD_shp <- HIFLD_shp[HIFLD_shp$SVCTERID %in% HIFLD_csv$SVCTERID,]
  
  #just separate out those that have multiple matches or NAs
  PHMSA_full_list <- unlist(strsplit(na.omit(PHMSA_csv_NG$Company_ID),split=" & "))
  GHGRP_full_list <- na.omit(GHGRP_csv$`Company ID`)

  #identify any IDs that are duplicated to make easier for users to notice when
  #manually editing
  PHMSA_dup_indx <- duplicated(PHMSA_full_list)
  GHGRP_dup_indx <- duplicated(GHGRP_full_list)
  HIFLD_dup_indx <- duplicated(HIFLD_csv$COMPID)
  
  #Any ID's used multiple times have " duplicated" added so the user can easily
  #identify them.  using grep for PHMSA since some can have multiple IDs and if
  #either matches that should count (i.e., partial matching).
  PHMSA_csv_NG$Company_ID[unique(unlist(sapply(PHMSA_full_list[PHMSA_dup_indx],
                                               FUN=function(x){grep(x,PHMSA_csv_NG$Company_ID)})))] <- 
    paste0(PHMSA_csv_NG$Company_ID[unique(unlist(sapply(PHMSA_full_list[PHMSA_dup_indx],
                                                        FUN=function(x){grep(x,PHMSA_csv_NG$Company_ID)})))]," duplicated")
  GHGRP_csv$`Company ID`[GHGRP_csv$`Company ID` %in% GHGRP_full_list[GHGRP_dup_indx]] <- 
    paste0(GHGRP_csv$`Company ID`[GHGRP_csv$`Company ID` %in% GHGRP_full_list[GHGRP_dup_indx]]," duplicated")
  HIFLD_csv$COMPID[HIFLD_csv$COMPID %in% HIFLD_csv$COMPID[HIFLD_dup_indx]] <- 
    paste0(HIFLD_csv$COMPID[HIFLD_csv$COMPID %in% HIFLD_csv$COMPID[HIFLD_dup_indx]]," duplicated")
  
  
  #reorder all of them so the first few columns are state, ID, name
  HIFLD_csv <- HIFLD_csv[,c("STATE","COMPID","NAME",
                            colnames(HIFLD_csv)[-which(colnames(HIFLD_csv) %in% c("STATE","COMPID","NAME"))])]
  GHGRP_csv <- GHGRP_csv[,c("operating_state","Company ID","facility_name",
                            colnames(GHGRP_csv)[-which(colnames(GHGRP_csv) %in% c("operating_state","Company ID","facility_name"))])]
  EIA_csv <- EIA_csv[,c("State","Company","Company Name",
                        colnames(EIA_csv)[-which(colnames(EIA_csv) %in% c("State","Company","Company Name"))])]
  PHMSA_csv_NG <- PHMSA_csv_NG[,c("State","Company_ID","OPERATOR_NAME",
                                  colnames(PHMSA_csv_NG)[-which(colnames(PHMSA_csv_NG) %in% c("State","Company_ID","OPERATOR_NAME"))])]
  
  
  #and reorganize them to all be in state-name order, to facilitate comparison (even
  #if some names do differ).
  HIFLD_csv <- HIFLD_csv[order(HIFLD_csv$STATE,HIFLD_csv$NAME),]
  GHGRP_csv <- GHGRP_csv[order(GHGRP_csv$state,GHGRP_csv$facility_name),]
  EIA_csv <- EIA_csv[order(EIA_csv$State,EIA_csv$`Company Name`),]
  PHMSA_csv_NG <- PHMSA_csv_NG[order(PHMSA_csv_NG$State,PHMSA_csv_NG$OPERATOR_NAME),]
  
  
  #save the xl files now for the user to edit further as needed.
  if(overwrite_xl==T & file.exists(file.path(input_directory,"ByLDC_EIA_176_type_of_operations.xlsx"))){
    write.xlsx(EIA_csv,file = file.path(input_directory,"ByLDC_EIA_176_type_of_operations.xlsx"),
               row.names = F,showNA = F)
    write.xlsx(PHMSA_csv_NG,file = file.path(input_directory,"ByLDC_PHMSA_annual_gas_distribution.xlsx"),
               row.names = F,showNA = F)
    write.xlsx(GHGRP_csv,file = file.path(input_directory,"ByLDC_GHGRP.xlsx"),
               row.names = F,showNA = F)
    write.xlsx(HIFLD_csv,file = file.path(input_directory,"ByLDC_HIFLD_natural_gas_service_territories.xlsx"),
               row.names = F,showNA = F)
  }
  
  rm(EIA_match_data,HIFLD_match_data,PHMSA_match_data,EIA_matches,overwrite_xl,
     EIA_matches1,EIA_matches2,EIA_matches3,EIA_matches4,PHMSA_matches,A,
     PHMSA_dup_indx,GHGRP_dup_indx,HIFLD_dup_indx,PHMSA_full_list,GHGRP_full_list,
     PHMSA_indx,dup_indx)
}



#Now compare the shapefiles visually to the GHGRP.  Unfortunately, GHGRP is
#proprietary.  Manual.
{
  ################################################################################
  #plot up the facilities for each state to visualize and compare against GHGRP
  #LDC shapefiles manually.  GHGRP shapefiles can be viewed on the GHGRP flight
  #website
  
  #GHGRP polygons are more up to date and can be visually compared to HIFLD.  Be
  #sure to set the year properly and do NOT set the state, as some GHGRP data is
  #set to the state of the headquarters/owner rather than that of operation.
  #Searching to show 1 LDC at a time can help.  Note GHGRP is only large
  #sources, so smaller LDCs may not report, but still have HIFLD/PHMSA/EIA data.
  #GHGRP data is available at:
  #https://ghgdata.epa.gov/ghgp/main.do#/facility/?q=Find%20a%20Facility%20or%20Location&st=&bs=&fid=&sf=11001100&lowE=-20000&highE=23000000&g1=1&g2=1&g3=1&g4=1&g5=1&g6=0&g7=1&g8=1&g9=1&g10=1&g11=1&g12=1&s1=0&s2=0&s3=0&s4=0&s5=0&s6=0&s7=0&s8=0&s9=1&s10=0&s201=0&s202=0&s203=0&s204=0&s301=0&s302=0&s303=0&s304=0&s305=0&s306=0&s307=0&s401=0&s402=0&s403=0&s404=0&s405=0&s601=0&s602=0&s701=0&s702=0&s703=0&s704=0&s705=0&s706=0&s707=0&s708=0&s709=0&s710=0&s711=0&s801=0&s802=0&s803=0&s804=0&s805=0&s806=0&s807=0&s808=0&s809=0&s810=0&s901=0&s902=0&s903=0&s904=0&s905=1&s906=0&s907=0&s908=0&s909=0&s910=0&s911=0&si=&ss=&so=0&ds=L&yr=2019&tr=current&cyr=2022&ol=0&sl=0&rs=ALL
  
  plot_LDC <- function(LDC_OBJECT_ID){
    LDC_OBJECT_ID <- as.numeric(LDC_OBJECT_ID)
    subset_HIFLD <- HIFLD_shp[HIFLD_shp$OBJECTID==LDC_OBJECT_ID,]
    subset_HIFLD_state <- HIFLD_shp[HIFLD_shp$LDC_STATE %in% subset_HIFLD$LDC_STATE,]
    plot(subset_HIFLD_state[order(subset_HIFLD_state$Shape__Area,decreasing = T),],"NAME",
         ext=ext(project(State_Tigerlines[State_Tigerlines$STUSPS==subset_HIFLD$LDC_STATE[1]],HIFLD_shp)),
         mar=c(3.1, 3.1, 2.1, 7.1) + c(0,0,0,8),main=subset_HIFLD$NAME)
    lines(subset_HIFLD,lwd=5)
    lines(project(State_Tigerlines[State_Tigerlines$STUSPS==subset_HIFLD$LDC_STATE[1]],HIFLD_shp),lwd=3,col="red")
  }
  plot_state <- function(input,statename){
    statename <- as.character(statename)
    subset_input <- input[input$LDC_STATE %in% statename,]
    plot(subset_input[order(expanse(subset_input),decreasing = T),],"NAME",
         ext=ext(project(State_Tigerlines[State_Tigerlines$STUSPS==statename],HIFLD_shp)),
         mar=c(3.1, 3.1, 2.1, 7.1) + c(0,0,0,4),main=statename)
    lines(project(State_Tigerlines[State_Tigerlines$STUSPS==statename],HIFLD_shp),lwd=3,col="red")
  }
  # plot_state(HIFLD_shp,"DE")
  # plot_state(HIFLD_shp,"MD")
  # plot_state(HIFLD_shp,"NJ")
  # plot_state(HIFLD_shp,"NY")
  # plot_state(HIFLD_shp,"PA")
  # 
  # #plot LDC can't use the name as LDC's often cover various states and use
  # #the same name for these different operations, but different OBJECTID's.  
  # 
  # View(as.data.frame(HIFLD_shp))
  # #PA
  # plot_LDC(1253)#columbia gas of PA
  # plot_LDC(1114)#KNOX energy
  # plot_LDC(1148)#PPL Gas
  # plot_LDC(57)#Phillips TW
  # plot_LDC(1111)#SAR GAS
  # plot_LDC(614)#Sergeant Gas
  # plot_LDC(611)#Swissvale
  # plot_LDC(239)#Riemer
  # plot_LDC(1107)#Valley Energy
  # 
  # #NY
  # plot_LDC(967)#valley energy
  # plot_LDC(963)#national grid
  # 
  # #MD
  # plot_LDC(813)#UGI Central
  # plot_LDC(1042)#Washington Gas
}


#modify the shapefiles.  Any changes need to be accounted for in the HIFLD xl
#file as well.  For example, a merged polygon should be manually combined in the
#xl and the SVCTERID of the output polygon should match between the shapefile
#and the xl.  Similarly, splitting an LDC should result in new polygons with
#unique SVCTERID values that are consistent between the xl and polygons.
{
  ################################################################################
  #plot the original LDCs

  for(A in 1:length(state_name_list)){
    png(file.path(input_directory,paste0(state_name_list[A],'_LDC_shapefile_original.png')),
        width = 480*2,height = 480*2)
    plot_state(HIFLD_shp,state_name_list[A])
    plot(project(crop(Water_Tigerlines,State_Tigerlines[State_Tigerlines$STUSPS==state_name_list[A]]),HIFLD_shp),
         add=T,col="black")
    graphics.off()
  }
  
  ################################################################################
  #do the modification work
  
  #work entirely in sf here, as terra had issues crashing
  sf_state_tigerlines <- st_as_sf(project(State_Tigerlines,HIFLD_shp))
  sf_county_tigerlines <- st_as_sf(project(County_Tigerlines,HIFLD_shp))
  sf_HIFLD_shp <- st_as_sf(HIFLD_shp)
  
  #combine a few counties
  LI_shp <- st_make_valid(st_combine(sf_county_tigerlines[which(sf_county_tigerlines$COUNTYNS %in% c('00974149', '00974128')),]))
  NYC_shp <- st_make_valid(st_combine(sf_county_tigerlines[which(sf_county_tigerlines$COUNTYNS %in% c('00974122', '00974139', '00974141', '00974129', '00974101')),]))

  # Split up the National Grid LDC polygon into 3 LDCs
  NGrid_shp <- st_make_valid(sf_HIFLD_shp[which(sf_HIFLD_shp$SVCTERID == 'LDC360007'),])
  NGrid_LI <- st_intersection(NGrid_shp, LI_shp)
  NGrid_NYC <- st_intersection(NGrid_shp, NYC_shp)
  NGrid_other <- st_difference(NGrid_shp, st_union(LI_shp, NYC_shp))
  
  # Add new entries for the shapefiles containing the new split NGrid polygon
  sf_HIFLD_shp <- rbind(sf_HIFLD_shp, sf_HIFLD_shp[rep(which(sf_HIFLD_shp$SVCTERID == 'LDC360007'), 3),])
  
  #assign these new polygons unique IDs and delete the original
  sf_HIFLD_shp$SVCTERID[nrow(sf_HIFLD_shp)-2] <- 'LDC360007a'
  sf_HIFLD_shp$SVCTERID[nrow(sf_HIFLD_shp)-1] <- 'LDC360007b'
  sf_HIFLD_shp$SVCTERID[nrow(sf_HIFLD_shp)] <- 'LDC360007c'
  sf_HIFLD_shp$NAME[nrow(sf_HIFLD_shp)-2] <- 'KEYSPAN - NYC'
  sf_HIFLD_shp$NAME[nrow(sf_HIFLD_shp)-1] <- 'NIAGARA MOHAWK'
  sf_HIFLD_shp$NAME[nrow(sf_HIFLD_shp)] <- 'KEYSPAN - LONG ISLAND'
  sf_HIFLD_shp <- sf_HIFLD_shp[sf_HIFLD_shp$SVCTERID != 'LDC360007',]
  
  #associate the polygons with the sf
  st_geometry(sf_HIFLD_shp[which(sf_HIFLD_shp$SVCTERID == 'LDC360007a'),]) <- st_geometry(NGrid_LI)
  st_geometry(sf_HIFLD_shp[which(sf_HIFLD_shp$SVCTERID == 'LDC360007b'),]) <- st_geometry(NGrid_NYC)
  st_geometry(sf_HIFLD_shp[which(sf_HIFLD_shp$SVCTERID == 'LDC360007c'),]) <- st_geometry(NGrid_other)
  
  # Merge the Peoples Natural Gas LDC geometries
  PPL_NG_combined <- st_combine(sf_HIFLD_shp[which(sf_HIFLD_shp$SVCTERID %in% c('LDC420001','LDC420022')),])
  st_geometry(sf_HIFLD_shp[which(sf_HIFLD_shp$SVCTERID == 'LDC420001'),]) <- PPL_NG_combined
  sf_HIFLD_shp <- sf_HIFLD_shp[sf_HIFLD_shp$SVCTERID != 'LDC420022',]
  
  #this shapefile bleeds over into VA, even though GHGRP and the other inputs
  #separate the data by state.  Removing the VA portion here
  sf_HIFLD_shp[which(sf_HIFLD_shp$SVCTERID=="LDC240007"),] <- st_intersection(sf_HIFLD_shp[which(sf_HIFLD_shp$SVCTERID=="LDC240007"),],
                                                                              sf_state_tigerlines[which(sf_state_tigerlines$STUSPS == "MD"),])[names(sf_HIFLD_shp)]
  
  rm(PPL_NG_combined,LI_shp,NYC_shp,NGrid_shp,NGrid_NYC,NGrid_LI,NGrid_other)
  ################################################################################
  #plot the updated LDCs
  
  for(A in 1:length(state_name_list)){
    png(file.path(input_directory,paste0(state_name_list[A],'_LDC_shapefile_update.png')),
        width = 480*2,height = 480*2)
    plot_state(vect(sf_HIFLD_shp),state_name_list[A])
    plot(project(crop(Water_Tigerlines,State_Tigerlines[State_Tigerlines$STUSPS==state_name_list[A]]),HIFLD_shp),
         add=T,col="black")
    graphics.off()
  }
  rm(A)

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # #attempts with terra - almost immediately crashes except for sections commented out 2x.
  # LI_shp <- sf_county_tigerlines[which(sf_county_tigerlines$COUNTYNS %in% c('00974149', '00974128')),]
  # LI_shp <- aggregate(LI_shp)
  # NYC_shp <- sf_county_tigerlines[which(sf_county_tigerlines$COUNTYNS %in% c('00974122', '00974139', '00974141', '00974129', '00974101')),]
  # NYC_shp <- aggregate(NYC_shp)
  # #combine a few LDCs
  # 
  # NGrid_shp <- HIFLD_shp[which(HIFLD_shp$SVCTERID == 'LDC360007'),]
  # NGrid_LI <- split(LI_shp,NGrid_shp)
  # 
  # NGrid_LI <- intersect(LI_shp,NGrid_shp)
  # NGrid_LI <- NGrid_shp*LI_shp
  # 
  # NGrid_LI <- st_intersection(st_as_sf(NGrid_shp), st_as_sf(LI_shp))
  # NGrid_NYC <- st_intersection(st_as_sf(NGrid_shp), st_as_sf(NYC_shp))
  # NGrid_other <- st_difference(NGrid_shp, st_union(LI_shp, NYC_shp))
  # # # Split up the National Grid LDC polygon
  # 
  # HIFLD_shp <- rbind(HIFLD_shp, HIFLD_shp[rep(which(HIFLD_shp$SVCTERID == 'LDC360007'), 3),])
  # # Add new entries for the shapefile containing the new split NGrid polygon
  # 
  # HIFLD_shp$SVCTERID[nrow(HIFLD_shp)-2] <- 'LDC360007a'
  # HIFLD_shp$SVCTERID[nrow(HIFLD_shp)-1] <- 'LDC360007b'
  # HIFLD_shp$SVCTERID[nrow(HIFLD_shp)] <- 'LDC360007c'
  # 
  # st_geometry(HIFLD_shp[which(HIFLD_shp$SVCTERID == 'LDC360007a'),]) <- st_geometry(NGrid_LI)
  # st_geometry(HIFLD_shp[which(HIFLD_shp$SVCTERID == 'LDC360007b'),]) <- st_geometry(NGrid_NYC)
  # st_geometry(HIFLD_shp[which(HIFLD_shp$SVCTERID == 'LDC360007c'),]) <- st_geometry(NGrid_other)
  # 
  # # PPL_NG_combined <- terra::combineGeoms(HIFLD_shp[which(HIFLD_shp$SVCTERID %in% c('LDC420001')),],
  # #                                        HIFLD_shp[which(HIFLD_shp$SVCTERID %in% c('LDC420022')),])
  # # HIFLD_shp <- HIFLD_shp[which(!HIFLD_shp$SVCTERID %in% c('LDC420022',"LDC420001")),]
  # # HIFLD_shp <- rbind(HIFLD_shp,PPL_NG_combined)
  # # # Merge the Peoples Natural Gas LDC geometries
  # 
  # HIFLD_shp[which(HIFLD_shp$SVCTERID=="LDC240007"),] <- st_intersection(HIFLD_shp[which(HIFLD_shp$SVCTERID=="LDC240007"),],
  #                                                                       state_shp_trans[which(state_shp_trans$STUSPS == "MD"),])[names(HIFLD_shp)]
  # #this shapefile bleeds over into VA, even though GHGRP and the other inputs
  # #separate the data by state.  Removing the VA portion here
  # 
  # all_merge_with_poly <- merge(all_merge_clean, HIFLD_shp[c('SVCTERID', 'geometry')], all.x=TRUE)
  # # Now merge the geometries from HIFLD_shp with the entries in all_merge_clean
  # 
  # all_merge_sf <- st_as_sf(all_merge_with_poly, sf_column_name='geometry', crs=crs(HIFLD_shp))
  # # Turn into sf object
  # 
  # for(a_state in unique(all_merge_sf$State)){
  #   other_indx <- which(all_merge_sf$State == a_state & all_merge_sf$Company == 'OTHER')
  #   if(length(other_indx)){  # if there is an 'OTHER' entry for this state
  #     state_poly <- state_shp_trans[which(state_shp_trans$STUSPS == a_state),]
  #     st_geometry(all_merge_sf[other_indx,]) <- st_geometry(st_difference(state_poly, st_union(all_merge_sf)))
  #     all_merge_sf[other_indx,'SVCTERID'] <- paste0('DUMMY_', a_state)
  #   }
  # }
  # # Go through each state and get the geometry of the OTHER entry for all_merge_clean (i.e. areas not covered by all_merge_sf)
  # # Also change SVCTERID from DUMMY to a unique value
  # 
  # ################################################################################
  # #plot the updated LDCs
  # 
  # for(A in 1:length(state_name_list)){
  #   png(file.path(output_directory,paste0('/Updated_',state_name_list[A],'_LDC_shapefile.png')),)
  #   par(oma = c(0, 0, 0, 4))
  #   current_state <- state_name_list[A]
  #   plot(all_merge_sf[all_merge_sf$State==current_state,1],key.length=0.9,
  #        key.pos=4,main=paste0(current_state," SVCTERID"),
  #        pal=timPalette(n=nrow(all_merge_sf[all_merge_sf$State==current_state,1])))
  #   graphics.off()
  # }
}




#now load in the modified xl files and merge everything as well as the modified
#shapefile.  This will generate the final output used in
#NG_distribution_emissions_r4.R.  Should be automatic, but may require manual
#work depending on changes made to the input files.
{
  EIA_csv <- read_excel(file.path(input_directory,"ByLDC_EIA_176_type_of_operations.xlsx"))
  PHMSA_csv_NG <- read_excel(file.path(input_directory,"ByLDC_PHMSA_annual_gas_distribution.xlsx"))
  GHGRP_csv <- read_excel(file.path(input_directory,"ByLDC_GHGRP.xlsx"))
  HIFLD_csv <- read_excel(file.path(input_directory,"ByLDC_HIFLD_natural_gas_service_territories.xlsx"))
  
  ##############################################################################
  #Do the work to merge all input data + shapefiles
  
  # First select the columns we need
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
                                          "AVERAGE_LENGTH"))
  
  EIA_cols_to_keep <- paste0("EIA_",c("Residential_Total_Volume_(Mcf)",
                                      "Residential_Total_Customers",
                                      'Commercial_Total_Volume_(Mcf)',
                                      'Commercial_Total_Customers',
                                      'Industrial_Total_Volume_(Mcf)',
                                      'Industrial_Total_Customers',
                                      'Electric_Total_Volume_(Mcf)',
                                      'Electric_Total_Customers'))
  
  cols_to_keep <- c('EIA_Company',
                    'EIA_Company_Name',
                    'HIFLD_SVCTERID',
                    'PHMSA_State',
                    EIA_cols_to_keep,
                    PHMSA_cols_to_keep,
                    "GHGRP_N_of_above_grade_T-D_transfer_stations",
                    "GHGRP_N_of_above_grade_non_T-D_MR_stations",
                    "GHGRP_N_of_below_grade_T-D_transfer_stations",
                    "GHGRP_N_of_below_grade_non_T-D_MR_stations")
  
  
  #first rename all data to make it obvious where it came from - much easier to
  #understand where things come from later
  colnames(PHMSA_csv_NG) <- paste0("PHMSA_",gsub(" ","_",colnames(PHMSA_csv_NG)))
  colnames(EIA_csv) <- paste0("EIA_",gsub(" ","_",colnames(EIA_csv)))
  colnames(HIFLD_csv) <- paste0("HIFLD_",gsub(" ","_",colnames(HIFLD_csv)))
  colnames(GHGRP_csv) <- paste0("GHGRP_",gsub(" ","_",colnames(GHGRP_csv)))
  
  #combine by ID/state since, at the very least, there are likely multiple OTHER
  #within any given state
  PHMSA_csv_NG_agg <- aggregate(PHMSA_csv_NG[PHMSA_cols_to_keep],
                                list(PHMSA_Company_ID = PHMSA_csv_NG$PHMSA_Company_ID,
                                     PHMSA_State = PHMSA_csv_NG$PHMSA_State),
                                sum,na.rm=T)

  EIA_PHMSA_merge <- merge(EIA_csv, PHMSA_csv_NG_agg, by.x='EIA_Company', by.y='PHMSA_Company_ID')
  EIA_PHMSA_HIFLD_merge <- merge(EIA_PHMSA_merge, HIFLD_csv, by.x='EIA_Company', by.y='HIFLD_COMPID')
  all_merge <- merge(EIA_PHMSA_HIFLD_merge, GHGRP_csv, by.x='EIA_Company', by.y='GHGRP_Company_ID', all.x=TRUE)
  
  if(!all.equal(nrow(PHMSA_csv_NG_agg),
               nrow(all_merge),
               nrow(EIA_PHMSA_HIFLD_merge),
               nrow(EIA_PHMSA_merge))){
    stop("somehow have a different number of LDCs between PHMSA and merged data.  Check what happened to cause this before proceeding.")
  }
  
  all_merge <- all_merge[,cols_to_keep]
  
  rm(PHMSA_cols_to_keep,cols_to_keep,EIA_PHMSA_merge,
     EIA_PHMSA_HIFLD_merge)
  ##############################################################################
  #now merge with the shapefile and then set the OTHER facilities to the
  #uncovered regions, also removing water areas.
  
  all_merge_with_poly <- merge(all_merge, sf_HIFLD_shp[c('SVCTERID', 'geometry')],
                               by.x="HIFLD_SVCTERID", by.y="SVCTERID", all.x=TRUE)
  
  # Turn into sf object
  all_merge_sf <- st_make_valid(st_as_sf(all_merge_with_poly, sf_column_name='geometry', crs=crs(sf_HIFLD_shp)))
  
  sf_Water_Tigerlines <- st_as_sf(Water_Tigerlines)
  sf_Water_Tigerlines <- st_transform(sf_Water_Tigerlines,crs = crs(sf_state_tigerlines))
  
  # Go through each state and get the geometry of the OTHER entry for
  # all_merge_clean (i.e. all land areas not covered by all_merge_sf) Also
  # change SVCTERID from DUMMY to a unique value
  for(a_state in unique(all_merge_sf$PHMSA_State)){
    other_indx <- which(all_merge_sf$PHMSA_State == a_state & all_merge_sf$EIA_Company == 'OTHER')
    if(length(other_indx)){  # if there is an 'OTHER' entry for this state
      #subset to just 1 state, get the water data for just that state, then take
      #the difference (i.e., only land within state bounds).
      state_poly <- sf_state_tigerlines[which(sf_state_tigerlines$STUSPS == a_state),]
      state_water_poly <- st_simplify(st_union(st_intersection(sf_Water_Tigerlines,state_poly)),dTolerance=500)
      state_land <- st_make_valid(st_collection_extract(st_difference(st_geometry(state_poly),
                                                                      state_water_poly),"POLYGON"))
      
      #now save the land data not already covered by any other LDC and update
      #the SVCTERID for clarity
      st_geometry(all_merge_sf[other_indx,]) <- st_collection_extract(st_geometry(st_make_valid(st_difference(state_land,
                                                                                                              st_make_valid(st_union(all_merge_sf))))),"POLYGON")
      # st_geometry(all_merge_sf[other_indx,]) <- st_geometry(st_make_valid(st_difference(state_land,
      #                                                                                   st_make_valid(st_collection_extract(st_union(all_merge_sf),"POLYGON")))))
      # st_geometry(all_merge_sf[other_indx,]) <- st_geometry(st_simplify(all_merge_sf[other_indx,],dTolerance=500))
      all_merge_sf[other_indx,'HIFLD_SVCTERID'] <- paste0('DUMMY_', a_state)
    }
    cat("\nFinished distributing OTHER for",a_state)
  }
  
  all_merge <- vect(all_merge_sf)
  ##############################################################################
  #now assign all unassigned EIA data to the other categories
  
  # Calculate residual EIA values from state totals
  EIA_state_totals <- aggregate(EIA_csv[EIA_cols_to_keep],
                                list(EIA_State=EIA_csv$EIA_State),
                                sum,
                                na.rm = TRUE)
  
  EIA_merge_state_totals <-  aggregate(as.data.frame(all_merge[EIA_cols_to_keep]),
                                       list(EIA_State=all_merge$PHMSA_State),
                                       sum,
                                       na.rm = TRUE)
  
  # Loop through states and assign residual EIA values to OTHER
  for(a_state in unique(all_merge$PHMSA_State)){
    residuals <- (EIA_state_totals[which(EIA_state_totals$EIA_State == a_state),-1] -
                    EIA_merge_state_totals[which(EIA_merge_state_totals$EIA_State == a_state),-1])
    all_merge[which(all_merge$EIA_Company == 'OTHER' & all_merge$PHMSA_State == a_state), EIA_cols_to_keep] <- residuals
  }
  ##############################################################################
  #plot the final state LDC maps - this time not including water areas (already
  #clear by the OTHER category)
  for(A in 1:length(state_name_list)){
    png(file.path(input_directory,paste0(state_name_list[A],'_LDC_shapefile_final_version.png')),
        width = 480*2,height = 480*2)
    subset_merge_state <- all_merge[all_merge$PHMSA_State %in% state_name_list[A],]
    plot(subset_merge_state[order(expanse(subset_merge_state),decreasing = T),],"EIA_Company_Name",
         ext=ext(project(State_Tigerlines[State_Tigerlines$STUSPS==state_name_list[A]],all_merge)),
         mar=c(3.1, 3.1, 2.1, 7.1) + c(0,0,0,4),main=state_name_list[A])
    lines(project(State_Tigerlines[State_Tigerlines$STUSPS==state_name_list[A]],all_merge),lwd=3,col="red")
    graphics.off()
  }
  rm(all_merge_with_poly,all_merge_sf,sf_Water_Tigerlines,other_indx,
     state_poly,state_water_poly,state_land,sf_HIFLD_shp,A,EIA_cols_to_keep,
     a_state,residuals,EIA_state_totals,EIA_merge_state_totals)
}
  
  
  
  
  
  


#calculate anything that is calculated differently in byLDC than it is
#otherwise.  May require some manual work depending on changes made to the input
#files.
{
  ##############################################################################
  #compare GHGRP and PHMSA miles of mains in the final files
  
  #copy the corresponding PHMSA total miles to the GHGRP file for comparison
  GHGRP_csv$'GHGRP_Miles_of_Mains(PHMSA)' <- sapply(GHGRP_csv$`GHGRP_Company_ID`,
                                              FUN=function(x){sum(PHMSA_csv_NG$PHMSA_MMILES_TOTAL[which(x==PHMSA_csv_NG$PHMSA_Company_ID)])})
  #this utility exists in GHGRP, but has no shape file so has to be manually
  #copied over (just 1 of many "OTHER"s in PHMSA).  
  GHGRP_csv$'GHGRP_Miles_of_Mains(PHMSA)'[GHGRP_csv$GHGRP_facility_id==1007356] <- 12028
  
  #user update check - PHMSA and GHGRP should agree very well.  Any that differ
  #a lot could be due to mislabeling.  If it was left blank in GHGRP, set to 0
  #delta.
  GHGRP_PHMSA_comparison <- abs(GHGRP_csv$'GHGRP_Miles_of_Mains(PHMSA)' - GHGRP_csv$GHGRP_Miles_of_Mains)/mean(c(GHGRP_csv$'GHGRP_Miles_of_Mains(PHMSA)',GHGRP_csv$GHGRP_Miles_of_Mains))*100
  GHGRP_PHMSA_comparison[is.na(GHGRP_csv$`GHGRP_Company_ID`)] <- 0
  if(max(GHGRP_PHMSA_comparison)>5){
    View(GHGRP_csv[GHGRP_PHMSA_comparison>5,c("facility_name","Miles_of_Mains","Miles_of_Mains(PHMSA)")])
    stop("Double check the GHGRP facilities:\n",paste(GHGRP_csv$facility_name[GHGRP_PHMSA_comparison>5],collapse = "\n"),"\n\nas the miles of mains was >5% different than the corresponding PHMSA facility.  One of them is likely wrong.")
  }
  rm(GHGRP_PHMSA_comparison)
  ##############################################################################
  #calculate the number of M&R stations per LDC using state averages for those
  #where we don't have exact GHGRP data.  So calculate state averages here.
  
  # M&R stations - can use GHGRP data for those stations that report, otherwise
  # estimate based on avg stations per mile for reporters in each state. Then
  # split by pressure and function assuming the same split as at the national
  # level (from the EPA national inventory report, also called GHGI).
  
  #Pull from GHGRP when available
  all_merge$GHGRP_MnR_above <- all_merge$`GHGRP_N_of_above_grade_T-D_transfer_stations`+ all_merge$`GHGRP_N_of_above_grade_non_T-D_MR_stations`
  all_merge$GHGRP_MnR_below <- all_merge$`GHGRP_N_of_below_grade_T-D_transfer_stations` + all_merge$`GHGRP_N_of_below_grade_non_T-D_MR_stations`
  
  # Use the original GHGRP_csv df to calculate avg per mile - it includes UGI
  # data in PA, which we had to exclude the all_merge below because there was no
  # good shapefile for it, but the underlying activity data is fine. Note that
  # for PA this means the average stations_per_mile value for reporters included
  # here does not equal the default stations_per_mile value assigned to
  # non-reporters below.
  main_miles_ghgrp <- aggregate(GHGRP_csv$`GHGRP_Miles_of_Mains(PHMSA)`,
                                list(State=GHGRP_csv$GHGRP_operating_state),
                                sum,
                                na.rm=TRUE)
  above_grade_MnR <- aggregate((GHGRP_csv$`GHGRP_N_of_above_grade_T-D_transfer_stations` +
                                  GHGRP_csv$`GHGRP_N_of_above_grade_non_T-D_MR_stations`),
                               list(State=GHGRP_csv$GHGRP_operating_state),
                               sum,
                               na.rm=TRUE)
  below_grade_MnR <- aggregate((GHGRP_csv$`GHGRP_N_of_below_grade_non_T-D_MR_stations` +
                                  GHGRP_csv$`GHGRP_N_of_below_grade_T-D_transfer_stations`),
                               list(State=GHGRP_csv$GHGRP_operating_state),
                               sum,
                               na.rm=TRUE)
  
  # Calculate average stations per mile in each state
  above_grade_MnR$stations_per_mile <- above_grade_MnR$x/main_miles_ghgrp$x
  below_grade_MnR$stations_per_mile <- below_grade_MnR$x/main_miles_ghgrp$x
  
  # Estimate number of stations for non-reporters using PHMSA miles and GHGRP
  # avg facilities/mile by state.
  non_ghgrp_indx <- which(is.na(all_merge$GHGRP_MnR_above))
  non_ghgrp_state <- all_merge$PHMSA_State[non_ghgrp_indx]
  state_indx <- match(non_ghgrp_state,above_grade_MnR$State)
  all_merge$GHGRP_MnR_above[non_ghgrp_indx] <- all_merge$PHMSA_MMILES_TOTAL[non_ghgrp_indx]*above_grade_MnR$stations_per_mile[state_indx]
  all_merge$GHGRP_MnR_below[non_ghgrp_indx] <- all_merge$PHMSA_MMILES_TOTAL[non_ghgrp_indx]*below_grade_MnR$stations_per_mile[state_indx]
  ##############################################################################
  #save this output
  unlink(file.path(input_directory,"byLDC_merged"),recursive = T)
  writeVector(all_merge,file.path(input_directory,"byLDC_merged"))
  write.table(names(all_merge),file.path(input_directory,"byLDC_merged/colnames.txt"),row.names=F,col.names=F)
  
}




