## code to prepare `EIA_transmission` dataset goes here


library(terra)
################################################################################
#Download

#EIA inter and intrastate transmission pipeline map from the EIA atlas

#download via API, load directly in.  Saving to file directly instead caused a
#memory issue so only a small amount of data was downloaded. 
data_URL <- "https://services2.arcgis.com/FiaPA4ga0iQKduv3/arcgis/rest/services/Natural_Gas_Interstate_and_Intrastate_Pipelines_1/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"
EIA_transmission <- vect(data_URL)

################################################################################
#crop out AK
EIA_transmission <- crop(EIA_transmission,ext(c(-130,-60,20,55)))
################################################################################
#save
usethis::use_data(EIA_transmission, overwrite = TRUE)
