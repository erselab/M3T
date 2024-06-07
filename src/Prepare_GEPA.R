#download the gridded epa inventory if it hasn't already been downloaded, then
#split into the components that we don't have in our inventory and save them to
#incorporate into the total easily later.

Prepare_GEPA <- function(){
  
  #Zenodo API to download the appropriate GEPA v2 file.
  #https://zenodo.org/records/8367082
  if(inventory_year>2011 & inventory_year<2019){
    GEPA_filename <- paste0("Gridded_GHGI_Methane_v2_",inventory_year,".nc")
    GEPA_URL <- paste0("https://zenodo.org/api/records/8367082/files/",GEPA_filename,"/content")
  }else if(inventory_year<2021){
    GEPA_filename <- paste0("Express_Extension_Gridded_GHGI_Methane_v2_",inventory_year,".nc")
    GEPA_URL <- paste0("https://zenodo.org/api/records/8367082/files/",GEPA_filename,"/content")
  }else{
    stop("No GEPA available for the chosen year, ",inventory_year)
  }
  
  #download the GEPA file.  for some reason failed without setting method.
  if(!file.exists(file.path(input_directory,GEPA_filename))){
    download.file(GEPA_URL,destfile=paste0(input_directory,GEPA_filename),
                  quiet = T,method='curl')
  }
  rm(GEPA_URL)
  ################################################################################
  #load in the file and split into the fossil fuel and non-fossil components we need
  GEPA <- rast(file.path(input_directory,GEPA_filename))
  
  GEPA <- project(GEPA,domain)
  
  GEPA_non_FF_sectors <- c("emi_ch4_5B1_Composting",
                           "emi_ch4_3A_Enteric_Fermentation",
                           "emi_ch4_3B_Manure_Management",
                           "emi_ch4_3C_Rice_Cultivation",
                           "emi_ch4_3F_Field_Burning")
  GEPA_FF_sectors <- c("emi_ch4_1A_Combustion_Mobile",
                       "emi_ch4_1B1a_Abandoned_Coal",
                       "emi_ch4_1B1a_Surface_Coal",
                       "emi_ch4_1B1a_Underground_Coal",
                       "emi_ch4_1B2a_Petroleum_Systems_Exploration",
                       "emi_ch4_1B2a_Petroleum_Systems_Production",
                       "emi_ch4_1B2a_Petroleum_Systems_Refining",
                       "emi_ch4_1B2a_Petroleum_Systems_Transport",
                       "emi_ch4_1B2ab_Abandoned_Oil_Gas",
                       "emi_ch4_1B2b_Natural_Gas_Exploration",
                       "emi_ch4_1B2b_Natural_Gas_Processing",
                       "emi_ch4_1B2b_Natural_Gas_Production",
                       "emi_ch4_2B8_Industry_Petrochemical",
                       "emi_ch4_2C2_Industry_Ferroalloy")
  
  #convert units
  #molec/cm2/s to nmol/m2/s
  GEPA <- GEPA*(1e9*100^2)/(6.022141e+23)
  
  #subset to the 3 types of GEPA data we need
  GEPA_landfill <- GEPA$emi_ch4_5A1_Landfills_Industrial
  GEPA_non_FF <- GEPA[[which(names(GEPA) %in% GEPA_non_FF_sectors)]]
  GEPA_FF <- GEPA[[which(names(GEPA) %in% GEPA_FF_sectors)]]
  
  #sum across layers for those that are multiple individual sectors
  GEPA_non_FF <- sum(GEPA_non_FF)
  GEPA_FF <- sum(GEPA_FF)
  
  ################################################################################
  #Save the output
  
  writeCDF(GEPA_landfill,
           file.path(output_directory,'GEPA_ind_landfill.nc'),
           force_v4=TRUE,
           varname='methane_emissions',
           unit='nmol/m2/s',
           longname=paste0(gsub("_"," ",gsub(".nc","",GEPA_filename))," industrial landfills"),
           missval=-9999,
           overwrite=TRUE)
  writeCDF(GEPA_non_FF,
           file.path(output_directory,'GEPA_non_FF.nc'),
           force_v4=TRUE,
           varname='methane_emissions',
           unit='nmol/m2/s',
           longname=paste0(gsub("_"," ",gsub(".nc","",GEPA_filename))," enteric fermentation, manure management, rice cultivation, field burning, and composting"),
           missval=-9999,
           overwrite=TRUE)
  writeCDF(GEPA_FF,
           file.path(output_directory,'GEPA_FF.nc'),
           force_v4=TRUE,
           varname='methane_emissions',
           unit='nmol/m2/s',
           longname=paste0(gsub("_"," ",gsub(".nc","",GEPA_filename))," mobile combustion, coal, petroleum, abandoned oil and gas, natural gas exploration processing and production, petrochemicals, and ferroalloy"),
           missval=-9999,
           overwrite=TRUE)
}

