## code to prepare `EIA_NG_data` dataset.
## https://www.eia.gov/naturalgas/ngqs/#?report=RP4&year1=2020&year2=2020&company=Name
## EIA Form 176 - the Annual Report of Natural and supplemental Gas Supply and
## Disposition that includes sold natural gas volume by company and customer
## type. Can be downloaded as a csv file using a button in the topright on the
## webpage. Default "M3T".

input_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Manuscript/All inventory data/Prepared inventory data/"

################################################################################
#load in
EIA_file <- list.files(input_directory,pattern = "EIA_form_176_.*.csv",full.names = T)

EIA_NG_data <- read.csv(EIA_file)

################################################################################
#clean up

#Correct column names that matter
colnames(EIA_NG_data) <- gsub("\\.|\\.BR\\.","_",
                          gsub("\\.\\.Mcf\\.","\\.(Mcf)",colnames(EIA_NG_data)))
colnames(EIA_NG_data) <- paste0("EIA_",gsub(" ","_",colnames(EIA_NG_data)))

EIA_cols_to_keep <- paste0("EIA_",c("Year",
                                    "State",
                                    "Residential_Total_Volume_(Mcf)",
                                    "Residential_Total_Customers",
                                    'Commercial_Total_Volume_(Mcf)',
                                    'Commercial_Total_Customers',
                                    'Industrial_Total_Volume_(Mcf)',
                                    'Industrial_Total_Customers',
                                    'Electric_Total_Volume_(Mcf)',
                                    'Electric_Total_Customers'))
EIA_NG_data <- EIA_NG_data[,EIA_cols_to_keep]

################################################################################
#save
usethis::use_data(EIA_NG_data, overwrite = TRUE)
