## code to prepare `NLCD_all_years` dataset goes here


for(year in 2010:2024){
  download_location <- tempfile(fileext = ".zip")
  NLCD_test_URL <- paste0("https://www.mrlc.gov/downloads/sciweb1/shared/mrlc/data-bundles/Annual_NLCD_LndCov_",year,"_CU_C1V1.zip")
  download.file(NLCD_test_URL,download_location,method = "curl",quiet = T)
  utils::unzip(download_location,exdir=file.path(output_directory,"NLCD",year))
  #delete the temp file
  unlink(download_location)
}
# test=rast(file.path(output_directory,"NLCD",year,"Annual_NLCD_LndCov_2020_CU_C1V1.tif"))
# NLCD_key <- data.frame("Value"=c(11,12,21:24,31,41:43,52,71,81:82,90,95),
#                        "Land_Class"=levels(test)[[1]][,2])
# levels(test) <- NLCD_key


download_location <- tempfile(fileext = ".zip")
NLCD_test_URL <- paste0("https://www.cec.org/files/atlas_layers/1_terrestrial_ecosystems/1_01_0_land_cover_2020_30m/land_cover_2020v2_30m_tif.zip")
download.file(NLCD_test_URL,download_location,method = "curl")
utils::unzip(download_location,exdir=file.path(output_directory,"NALCMS"))
#delete the temp file
unlink(download_location)


#usethis::use_data(NLCD_all_years, overwrite = TRUE)
