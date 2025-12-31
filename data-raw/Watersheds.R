## code to prepare `Watersheds` dataset goes here

library(terra)
################################################################################
#download

Watershed_dir <- tempdir()
dir.create(Watershed_dir,showWarnings = F)

#download the data.  URL is slightly different than the catalog.  See
#https://www.cec.org/north-american-environmental-atlas/watersheds/
data_URL <- "https://www.cec.org/files/atlas_layers/0_reference/0_04_watersheds/watersheds_shapefile.zip"
temp_out <- tempfile(fileext = ".zip")
utils::download.file(data_URL,temp_out,quiet=T)
unzip(temp_out,exdir = Watershed_dir)

Watershed_file <- list.files(Watershed_dir,recursive=T,pattern=".shp$",
                             full.names = T)

watersheds <- vect(Watershed_file)

#we only care about NAW1 in English, so aggregate all polygons to this level
#and remove extra data
watersheds <- watersheds["NAW1_EN"]
watersheds <- terra::aggregate(watersheds,by="NAW1_EN")

################################################################################
#save
usethis::use_data(watersheds, overwrite = TRUE)

unlink(temp_out,recursive=T)


