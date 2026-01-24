## code to prepare ghgi_data

################################################################################
#Download main and annex GHGI data tables for the most recent year.  Data
#includes updated values for past year so only the most recent is needed

output_directory <- tempdir()

#Iteratively test GHGI webpages down to 2022 (most recent available when code
#was written) to find the newest one available.
for(GHGI_data_yr in 2029:2022){
  data_URL <- paste0("https://www.epa.gov/ghgemissions/inventory-us-greenhouse-gas-emissions-and-sinks-1990-",GHGI_data_yr)
  test_url <- suppressWarnings(try(utils::download.file(data_URL,tempfile(".zip"),quiet = T),silent = T))
  if(test_url==0){
    break
  }
}

#download the webpage and load in the HTML
download_dest <- tempfile(fileext = ".html")
utils::download.file(data_URL,download_dest,quiet=T)
HTML_data <- readChar(download_dest,file.info(download_dest)$size)

#Search for https:// - any 60 or fewer characters - main - any
#characters.zip in the HTML_data. This should identify the file if it's
#named similarly to any other in the 2020's. The HTML_data webpage must
#still be up to date though.
Matchtext <- regexpr("https://www.epa.gov/.{1,60}main.{0,60}.zip",HTML_data,ignore.case = T)
Matchtext_annex <- regexpr("https://www.epa.gov/.{1,60}annex.{0,60}.zip",HTML_data,ignore.case = T)
data_URL2 <- substring(HTML_data,Matchtext[1],Matchtext[1]+attr( Matchtext , "match.length")-1)
data_URL_annex <- substring(HTML_data,Matchtext_annex[1],Matchtext_annex[1]+attr( Matchtext_annex , "match.length")-1)

#Use regex to save the year of the dataset as part of the download for
#clarity.  Download and unzip.
GHGI_yr <- substr(data_URL2,regexpr("20??",data_URL2)[1],regexpr("20??",data_URL2)[1]+3)
GHGI_file <- file.path(output_directory,paste0(GHGI_yr,"_GHGI_tables.zip"))

utils::download.file(data_URL2,GHGI_file,quiet=T)
utils::unzip(GHGI_file,exdir = file.path(output_directory,paste0(GHGI_yr,"_GHGI_tables")),overwrite=T)

#annex too
utils::download.file(data_URL_annex,GHGI_file,quiet=T)
utils::unzip(GHGI_file,exdir = file.path(output_directory,paste0(GHGI_yr,"_GHGI_tables")),overwrite=T)

#delete zip files
unlink(GHGI_file)
GHGI_file <- gsub("\\.zip","",GHGI_file)

################################################################################
#unzip all subfolders for use

#ID zipped subfolders
sub_zips <- list.files(file.path(output_directory,paste0(GHGI_yr,"_GHGI_tables")),pattern = "*.zip",full.names = T)
sub_folders <- gsub(".zip","",sub_zips)

#just a duplicate of the GHGI folder - delete
unlink(sub_zips[grep("Main Text",sub_zips)])
sub_zips <- sub_zips[-grep("Main Text",sub_zips)]

#unzip subfolders
for(A in 1:length(sub_zips)){
  utils::unzip(zipfile = sub_zips[A],exdir = sub_folders[A],overwrite = T)
  unlink(sub_zips[A])
}
################################################################################
#download NG specific annex tables (separate from the rest of the annex)

#now repeat for the petroleum and NG annex tables
NG_annex <- paste0(gsub("inventory-us-greenhouse-gas-emissions-and-sinks",
                        "natural-gas-and-petroleum-systems-ghg-inventory-additional-information",
                        data_URL),"-ghg")

utils::download.file(NG_annex,download_dest,quiet=T)
HTML_data <- readChar(download_dest,file.info(download_dest)$size)

Matchtext <- regexpr("https://www.epa.gov/.{1,100}ghgi_natural_gas_systems.{0,60}.xlsx",HTML_data,ignore.case = T)
data_URL2 <- substring(HTML_data,Matchtext[1],Matchtext[1]+attr( Matchtext , "match.length")-1)

NG_annex_file <- file.path(GHGI_file,paste0(GHGI_yr,"_ghgi_natural_gas_systems_annex36_tables.xlsx"))
utils::download.file(data_URL2,NG_annex_file,quiet=T,method="curl")
unlink(download_dest)
################################################################################
#grab landfill data

#find the relevant folder and file using regex of folder names and file headers
Waste_folder <- list.files(GHGI_file,pattern="*Waste*",full.names = T)
Waste_files <- list.files(pattern=".csv",Waste_folder,full.names=T)
GHGI_landfill_total <- sapply(Waste_files,readLines,n=1)
GHGI_landfill_total <- Waste_files[grep("*CH4 Emissions from Landfills \\(kt CH4\\)*",GHGI_landfill_total)]
GHGI_landfill_total <- utils::read.csv(GHGI_landfill_total,skip = 1)
#get the required data
GHGI_landfill_total <- sapply(GHGI_landfill_total[GHGI_landfill_total$Activity=="MSW net CH4 Emissions",-1],FUN = function(x){as.numeric(gsub(",","",x))})
GHGI_landfill_total <- as.data.frame(GHGI_landfill_total)
GHGI_landfill_total$Year <- gsub("X","",rownames(GHGI_landfill_total))
rownames(GHGI_landfill_total) <- NULL
colnames(GHGI_landfill_total) <- c("Emissions","Year")
GHGI_landfill_total <- GHGI_landfill_total[GHGI_landfill_total$Year>2010,]
################################################################################
#grab the NG distribution data

#use grep and the index page of the annex file to identify the pages we want
GHGI_index <- readxl::read_excel(NG_annex_file,sheet = "Index",.name_repair = "minimal")

GHGI_Activity_sheet <- gsub("Table ","",
                            GHGI_index[sapply(GHGI_index[,2],FUN=function(x){grep(pattern="Activity Data for Natural Gas Systems Sources",x)}),1])
GHGI_Emission_Factor_sheet <- gsub("Table ","",
                                   GHGI_index[sapply(GHGI_index[,2],FUN=function(x){grep(pattern="Average CH4 Emission Factors \\(kg/unit activity\\) for Natural Gas Systems Sources",x)}),1])

#Columns = year, rows = various types of sources.  First row is just to
#identify the first row of the tables as there is also header information that
#we want to exclude
first_row <- which(readxl::read_xlsx(NG_annex_file,sheet = GHGI_Activity_sheet,.name_repair = "minimal")[,1]=="Segment/Source")
GHGI_Activity <- readxl::read_xlsx(NG_annex_file,sheet = GHGI_Activity_sheet,skip=first_row,col_names = T)

first_row <- which(readxl::read_xlsx(NG_annex_file,sheet = GHGI_Emission_Factor_sheet,.name_repair = "minimal")[,1]=="Segment/Source")
GHGI_Emission_Factors <- readxl::read_xlsx(NG_annex_file,sheet = GHGI_Emission_Factor_sheet,skip=first_row,col_names = T)

#relevant years for this work
EF_years <- suppressWarnings(as.numeric(colnames(GHGI_Emission_Factors))>2010)
EF_years[is.na(EF_years)] <- F

Activity_years <- suppressWarnings(as.numeric(colnames(GHGI_Activity))>2010)
Activity_years[is.na(Activity_years)] <- F
####################
#metering and regulating stations

#all the sources we're looking for, written exactly as in the GHGI file
Data_list <- c("M&R >300","M&R 100-300","M&R <100","Reg >300","R-Vault >300",
               "Reg 100-300","R-Vault 100-300","Reg 40-100","R-Vault 40-100",
               "Reg <40")

#use sapply to find the row using data list, specify the column as the year and
#grab the relevant EF and activity data into a dataframe.

#Metering and regulating stations in mol/s/station
GHGI_MnR_EF <- data.frame("EF"=t(sapply(Data_list,FUN=function(x){as.numeric(GHGI_Emission_Factors[GHGI_Emission_Factors[,1]==x,EF_years])}))*
                            1000/(16.043*60*60*24*365))#convert from kg/yr to mol/s
GHGI_MnR_Activity <- data.frame("Total_stations"=t(sapply(Data_list,FUN=function(x){as.numeric(GHGI_Activity[GHGI_Activity[,1]==x,Activity_years])})))

colnames(GHGI_MnR_EF) <- as.numeric(colnames(GHGI_Emission_Factors[,EF_years]))
colnames(GHGI_MnR_Activity) <- as.numeric(colnames(GHGI_Activity[,Activity_years]))
####################
#services
Data_list <- c("Services - Unprotected steel",
               "Services Protected steel",
               "Services - Plastic",
               "Services - Copper")

#Service emissions in mol/s/event
GHGI_services <- data.frame("EF"=t(sapply(Data_list,FUN=function(x){as.numeric(GHGI_Emission_Factors[GHGI_Emission_Factors[,1]==x,EF_years])}))*
                              1000/(16.043*60*60*24*365))#convert from kg/yr to mol/s
colnames(GHGI_services) <- as.numeric(colnames(GHGI_Emission_Factors[,EF_years]))

####################
#meters
Data_list <- c("Residential",
               "Commercial",
               "Industrial")

#meter emissions in mol/s/meter - need to subset the first row as each has 2
GHGI_meters <- data.frame("EF"=t(sapply(Data_list,FUN=function(x){as.numeric(GHGI_Emission_Factors[GHGI_Emission_Factors[,1]==x,EF_years][1,])}))*
                            1000/(16.043*60*60*24*365))#convert from kg/yr to mol/s
colnames(GHGI_meters) <- as.numeric(colnames(GHGI_Emission_Factors[,EF_years]))
####################
#maintenance
Data_list <- c("Pressure Relief Valve Releases",
               "Pipeline Blowdown",
               "Mishaps (Dig-ins)")

GHGI_maintenance <- data.frame("EF"=t(sapply(Data_list,FUN=function(x){as.numeric(GHGI_Emission_Factors[GHGI_Emission_Factors[,1]==x,EF_years])}))*
                                 1000/(16.043*60*60*24*365))#convert from kg/yr to mol/s
colnames(GHGI_maintenance) <- as.numeric(colnames(GHGI_Emission_Factors[,EF_years]))
####################
#combine for saving
GHGI_NG_distribution <- list(GHGI_MnR_EF,GHGI_MnR_Activity,GHGI_services,GHGI_meters,GHGI_maintenance)
names(GHGI_NG_distribution) <- c("GHGI_MnR_EF","GHGI_MnR_Activity","GHGI_services","GHGI_meters","GHGI_maintenance")
################################################################################
#grab the NG transmission data

#identical to NG distribution one, but emissions and activity instead of
#emission factors and activity
GHGI_index <- readxl::read_excel(NG_annex_file,sheet = "Index",.name_repair = "minimal")
GHGI_Activity_sheet <- gsub("Table ","",
                            GHGI_index[sapply(GHGI_index[,2],FUN=function(x){grep(pattern="Activity Data for Natural Gas Systems Sources",x)}),1])
GHGI_Emission_sheet <- gsub("Table ","",
                            GHGI_index[sapply(GHGI_index[,2],FUN=function(x){grep(pattern="CH4 Emissions \\(kt\\) for Natural Gas Systems",x)}),1])
first_row <- which(readxl::read_xlsx(NG_annex_file,sheet = GHGI_Activity_sheet,.name_repair = "minimal")[,1]=="Segment/Source")
GHGI_Activity <- readxl::read_xlsx(NG_annex_file,sheet = GHGI_Activity_sheet,skip=first_row,col_names = T)
first_row <- which(readxl::read_xlsx(NG_annex_file,sheet = GHGI_Emission_sheet,.name_repair = "minimal")[,1]=="Segment/Source")
GHGI_Emissions <- readxl::read_xlsx(NG_annex_file,sheet = GHGI_Emission_sheet,skip=first_row,col_names = T)

#relevant years for this work
Emission_years <- suppressWarnings(as.numeric(colnames(GHGI_Emissions))>2010)
Emission_years[is.na(Emission_years)] <- F

Activity_years <- suppressWarnings(as.numeric(colnames(GHGI_Activity))>2010)
Activity_years[is.na(Activity_years)] <- F
####################
#transmission pipeline and metering and regulating data
#all the sources we're looking for, written exactly as in the GHGI file
Data_list <- c("Pipeline Leaks","M&R (Trans. Co. Interconnect)","M&R (Farm Taps + Direct Sales)",
               "Pipeline venting")

#use sapply to find the row using data list, specify the column as the year and
#grab the relevant emissions and activity data into a dataframe. Need to subset
#the first row as pipeline leaks has 2
GHGI_Pipeline_Emissions <- data.frame("Emissions"=t(sapply(Data_list,FUN=function(x){as.numeric(GHGI_Emissions[GHGI_Emissions[,1]==x,Emission_years][1,])}))*
                                        1E9/(16.043*60*60*24*365))#convert from kg/yr to mol/s
GHGI_Pipeline_Activity <- data.frame("Total_stations"=t(sapply(Data_list,FUN=function(x){as.numeric(GHGI_Activity[GHGI_Activity[,1]==x,Activity_years][1,])}))*
                                       1609.344)#convert from miles to meters

colnames(GHGI_Pipeline_Emissions) <- as.numeric(colnames(GHGI_Emissions[,Emission_years]))
colnames(GHGI_Pipeline_Activity) <- as.numeric(colnames(GHGI_Activity[,Activity_years]))
####################
#transmission station total + emissions during operations (vents, flaring,
#leaks, exhaust, etc.)
Data_list <- c("Station Total Emissions","Dehydrator vents (Transmission)",
               "Flaring (Transmission)","Engines (Transmission)",
               "Turbines (Transmission)","Engines (Storage)",
               "Turbines (Storage)","Generators (Engines)",
               "Generators (Turbines)","Pneumatic Devices Transmission",
               "Station Venting Transmission")

#use sapply to find the row using data list, specify the column as the year
#and grab the relevant emissions and activity data into a dataframe.
GHGI_transmission_compressors_Emissions <- data.frame("Emissions"=t(sapply(Data_list,FUN=function(x){as.numeric(GHGI_Emissions[GHGI_Emissions[,1]==x,Emission_years][1,])}))*
                                                        1E9/(16.043*60*60*24*365))#convert from kg/yr to mol/s
GHGI_transmission_compressors_Activity <- data.frame("Total_stations"=t(sapply(Data_list,FUN=function(x){as.numeric(GHGI_Activity[GHGI_Activity[,1]==x,Activity_years][1,])})))

colnames(GHGI_transmission_compressors_Emissions) <- as.numeric(colnames(GHGI_Emissions[,Emission_years]))
colnames(GHGI_transmission_compressors_Activity) <- as.numeric(colnames(GHGI_Activity[,Activity_years]))
####################
#combine for saving
GHGI_NG_transmission <- list(GHGI_Pipeline_Activity,GHGI_Pipeline_Emissions,GHGI_transmission_compressors_Activity,GHGI_transmission_compressors_Emissions)
names(GHGI_NG_transmission) <- c("GHGI_Pipeline_Activity","GHGI_Pipeline_Emissions","GHGI_transmission_compressors_Activity","GHGI_transmission_compressors_Emissions")
################################################################################
#grab the stationary combustion data

#find the relevant folder and file using regex of folder names and file headers
stationary_combustion_folder <- file.path(GHGI_file,"2024 Annex 3 Tables")
stationary_combustion_files <- list.files(stationary_combustion_folder,full.names=T)
GHGI_stationary_combustion <- sapply(stationary_combustion_files,readLines,n=1)
GHGI_stationary_combustion <- stationary_combustion_files[suppressWarnings(grep("*Fuel Consumption by Stationary Combustion.*\\(TBtu\\)*",GHGI_stationary_combustion))]
GHGI_stationary_combustion <- utils::read.csv(GHGI_stationary_combustion,skip = 1)

stationary_combustion_yrs <- suppressWarnings(as.numeric(gsub("X","",colnames(GHGI_stationary_combustion))))
stationary_combustion_yrs[is.na(stationary_combustion_yrs)] <- 0
stationary_combustion_yrs <- stationary_combustion_yrs[stationary_combustion_yrs>2010]

#reformat to match SEDS format
rownames(GHGI_stationary_combustion) <- paste0(rep(GHGI_stationary_combustion[seq(1,26,6),1],each=6) , " ",
                                                    GHGI_stationary_combustion[,1])[1:nrow(GHGI_stationary_combustion)]
GHGI_stationary_combustion <- t(GHGI_stationary_combustion)
GHGI_stationary_combustion <- as.data.frame(matrix(GHGI_stationary_combustion[paste0("X",stationary_combustion_yrs),c("Coal Commercial","Coal Industrial","Coal Electric Power",
                                                                                                                                "Petroleum Residential","Petroleum Commercial","Petroleum Industrial","Petroleum Electric Power",
                                                                                                                                "Natural Gas Commercial","Natural Gas Industrial","Natural Gas Electric Power",
                                                                                                                                "Wood Residential","Wood Commercial","Wood Industrial","Wood Electric Power")],nrow=length(stationary_combustion_yrs)))
GHGI_stationary_combustion <- cbind("US_EPA",GHGI_stationary_combustion)
colnames(GHGI_stationary_combustion) <- c("State",
                                               "com_coal","ind_coal","elec_coal",
                                               "res_petr","com_petr","ind_petr","elec_petr",
                                               "com_gas","ind_gas","elec_gas",
                                               "res_wood","com_wood","ind_wood","elec_wood")
rownames(GHGI_stationary_combustion) <- stationary_combustion_yrs

#make numeric rather than text
GHGI_stationary_combustion[,-1] <- apply(GHGI_stationary_combustion[,-1], 2, FUN=function(x){as.numeric(gsub(",","",x))})
################################################################################
#cleanup

unlink(GHGI_file,recursive=T)
################################################################################
#save each GHGI dataset

usethis::use_data(GHGI_landfill_total, overwrite = TRUE)
usethis::use_data(GHGI_NG_distribution, overwrite = TRUE)
usethis::use_data(GHGI_NG_transmission, overwrite = TRUE)
usethis::use_data(GHGI_stationary_combustion, overwrite = TRUE)
