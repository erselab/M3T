## code to prepare `EIA_NG_data` dataset goes here


input_directory <- "D:\\MMMT STUFF\\All inventory data\\Not Automated"

################################################################################
#load in
EIA_file <- file.path(input_directory,"EIA_form_176_all_years_downloaded_2025_09_27.csv")

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
