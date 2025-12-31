## code to prepare `Vulcan_v4_all_years` dataset goes here

#those available for vulcan version 4
#https://earth.gov/ghgcenter/data-catalog/vulcan-ffco2-yeargrid-v4
year <- 2010:2021

#data is in metric tons of carbon dioxide per 1 km x 1 km grid cell per year
#(tonne CO₂/km²/year)
################################################################################
#download

sector <- c("COM","ELC","IND","RES","TOT")
sector_name <- c("commercial","electric","industrial","residential","total")

data_URL <- paste0("https://data.ghg.center/vulcan-ffco2-yeargrid-v4/",
                   sector,"_CO2_USA_mosaic_grid_1km_mn_",
                   rep(year,each=length(sector)),".tif")

filenames <- paste0("Vulcan_",sector_name,"_1km_mn_",
                   rep(year,each=length(sector)),".tif")

output_directory <- tempdir()
dir.create(file.path(output_directory,"Vulcan_v4.0"),recursive = T,showWarnings = F)

for(A in 1:length(data_URL)){
  utils::download.file(data_URL[A],file.path(output_directory,"Vulcan_v4.0",filenames[A]),quiet = T,method='curl')
  cat("\rFinished Downloading",A,"of",length(data_URL),"                            ")
}
################################################################################
#load in and combine across years

Vulcan_commercial <- list.files(path = file.path(output_directory,"Vulcan_v4.0"),
                                pattern="Vulcan_commercial*",full.names = T)
Vulcan_residential <- list.files(path = file.path(output_directory,"Vulcan_v4.0"),
                                 pattern="Vulcan_residential*",full.names = T)
Vulcan_electric <- list.files(path = file.path(output_directory,"Vulcan_v4.0"),
                              pattern="Vulcan_electric*",full.names = T)
Vulcan_industrial <- list.files(path = file.path(output_directory,"Vulcan_v4.0"),
                                pattern="Vulcan_industrial*",full.names = T)
Vulcan_total <- list.files(path = file.path(output_directory,"Vulcan_v4.0"),
                           pattern="Vulcan_total*",full.names = T)

Vulcan_commercial <- rast(Vulcan_commercial)
names(Vulcan_commercial) <- year

Vulcan_residential <- rast(Vulcan_residential)
names(Vulcan_residential) <- year

Vulcan_electric <- rast(Vulcan_electric)
names(Vulcan_electric) <- year

Vulcan_industrial <- rast(Vulcan_industrial)
names(Vulcan_industrial) <- year

Vulcan_total <- rast(Vulcan_total)
names(Vulcan_total) <- year

################################################################################
#save

usethis::use_data(Vulcan_commercial)
usethis::use_data(Vulcan_residential)
usethis::use_data(Vulcan_electric)
usethis::use_data(Vulcan_industrial)
usethis::use_data(Vulcan_total)

