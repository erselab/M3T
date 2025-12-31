## code to prepare `GEPA` dataset goes here

library(jsonlite)
library(terra)
library(ncdf4)
################################################################################
#Zenodo API to download the appropriate GEPA v2 file.
#https://zenodo.org/records/8367082

#Identify the files available
File_list <- jsonlite::read_json("https://zenodo.org/api/records/8367082/files")
File_list <- sort(sapply(File_list$entries,"[[","key"))

#filter out monthly scale factors and sort for consistency
year_list <- as.numeric(unique(gsub("\\.nc","",sapply(strsplit(File_list,"_"),utils::tail,1))))

#get the most relevant files for each year
GEPA_files <- paste0("Gridded_GHGI_Methane_v2_",year_list,".nc")
Express_years <- year_list[!GEPA_files %in% File_list]
GEPA_files[!GEPA_files %in% File_list] <- paste0("Express_Extension_Gridded_GHGI_Methane_v2_",Express_years,".nc")

cat("Using Express Extension for",paste(Express_years,collapse=","),"- see https://zenodo.org/records/8367082 to understand the difference\n")

GEPA_URL <- paste0("https://zenodo.org/api/records/8367082/files/",GEPA_files,"/content")

output_dir <- tempdir()
dir.create(output_dir,showWarnings = F)

for(A in 1:length(GEPA_URL)){
  download.file(GEPA_URL[A],file.path(output_dir,GEPA_files[A]),quiet=T,method='curl')
  cat("\rFinished downloading",A,"of",length(GEPA_files),"                    ")
}

################################################################################
#load in the file and split into the fossil fuel and non-fossil components we need

GEPA_non_thermo_sectors <- c("emi_ch4_5A1_Landfills_Industrial",
                             "emi_ch4_5B1_Composting",
                             "emi_ch4_3A_Enteric_Fermentation",
                             "emi_ch4_3B_Manure_Management",
                             "emi_ch4_3C_Rice_Cultivation",
                             "emi_ch4_3F_Field_Burning")
GEPA_thermo_sectors <- c("emi_ch4_1A_Combustion_Mobile",
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

GEPA <- terra::rast(file.path(output_dir,GEPA_files[1]))

#subset to the 2 types of GEPA data we need
GEPA_non_thermo <- GEPA[[which(names(GEPA) %in% GEPA_non_thermo_sectors)]]
GEPA_thermo <- GEPA[[which(names(GEPA) %in% GEPA_thermo_sectors)]]

#sum across layers for those that are multiple individual sectors
GEPA_non_thermo <- sum(GEPA_non_thermo)
GEPA_thermo <- sum(GEPA_thermo)

Combined_GEPA_non_thermo <- GEPA_non_thermo
Combined_GEPA_thermo <- GEPA_thermo

for(A in 2:length(GEPA_files)){
  GEPA <- terra::rast(file.path(output_dir,GEPA_files[A]))
  
  GEPA_non_thermo <- GEPA[[which(names(GEPA) %in% GEPA_non_thermo_sectors)]]
  GEPA_thermo <- GEPA[[which(names(GEPA) %in% GEPA_thermo_sectors)]]
  GEPA_non_thermo <- sum(GEPA_non_thermo)
  GEPA_thermo <- sum(GEPA_thermo)
  
  Combined_GEPA_non_thermo <- c(Combined_GEPA_non_thermo,GEPA_non_thermo)
  Combined_GEPA_thermo <- c(Combined_GEPA_thermo,GEPA_thermo)
}

names(Combined_GEPA_non_thermo) <- year_list
names(Combined_GEPA_thermo) <- year_list
################################################################################
#save

usethis::use_data(Combined_GEPA_non_thermo, overwrite = TRUE)
usethis::use_data(Combined_GEPA_thermo, overwrite = TRUE)


unlink(output_dir,recursive = T)
