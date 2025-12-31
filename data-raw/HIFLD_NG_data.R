## code to prepare `HIFLD_NG_data` dataset goes here


input_directory <- "D:\\MMMT STUFF\\All inventory data\\Not Automated/"
library(readxl)
################################################################################
#load in
HIFLD_file <- file.path(input_directory,"HIFLD_Natural_Gas_Compressor_Stations_updated.xlsx")

HIFLD_NG_data <- as.data.frame(readxl::read_excel(HIFLD_file))

################################################################################
#clean up

HIFLD_cols_to_keep <- c("LATITUDE","LONGITUDE","GHGRP ID")

HIFLD_NG_data <- HIFLD_NG_data[,HIFLD_cols_to_keep]

################################################################################
#save
usethis::use_data(HIFLD_NG_data, overwrite = TRUE)
