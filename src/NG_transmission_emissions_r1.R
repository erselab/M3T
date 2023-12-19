## NG_transmission_emissions_r1.R
## In use: 2021-11-02 20:00
## Finalized: 2023-02-03
#
# Calculate NG transmission emissions for d03 domain

################################################################################
#Manually defined variables

Input_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw data/"
year <- "2019"
Output_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed/"

d03_bounding_box <- cbind(c(-76.65,-73.65),c(38.97,40.97))
resolution <- 0.01
#Will be used to generate a blank raster for the output to be built onto, WGS84
#CRS.  Resolution in deg, bounding box in lat/long.

EIA_pipeline_file <- 'NaturalGas_InterIntrastate_Pipelines_US_EIA/NaturalGas_Pipelines_US_202001.shp'
# Pipeline file comes from the EIA (https://www.eia.gov/maps/layer_info-m.php)
EIA_compressor_file <- 'Natural_Gas_Compressor_Stations.csv'
# This compressor file comes from the Homeland Infrastructure Foundation-Level Database (https://hifld-geoplatform.opendata.arcgis.com/datasets/geoplatform::natural-gas-compressor-stations/about)
GHGRP_compressor_file <- 'US_GHGRP_NG_transmission_and_Compression_only_all_years.xls'
# GHGRP compressor data comes in a spreadsheet from flight

EPA_file <- file.path(Input_directory,"2022_ghgi_natural_gas_systems_annex36_tables.xlsx")
EPA_Emissions_sheet <- "3.6-1"
EPA_Activity_sheet <- "3.6-7"
#which sheets are the needed ones in the EPA file.  We want Average CH4
#Emissions (kt/yr) for Natural Gas Systems Sources, for All Years AND Activity
#Data for Natural Gas Systems Sources, for All Years

# EPA=https://www.epa.gov/ghgemissions/natural-gas-and-petroleum-systems-ghg-inventory-additional-information-1990-2020-ghg

#Transmission compressor station and transmission pipeline emissions factors
#(emissions/compressor and emissions/meter) are pulled from the EPA file in a
#section on line 90.

# EIA_compressor_update <- cbind(c(-74.86595,-75.62250),c(41.58635,40.41962))
# EIA_compressor_update_df <- data.frame("NAME"=c("MILLENIUM HIGHLAND","BECHTELSVILLE"),
#                                        "STATE"=c("NY","PA"),
#                                        "ZIP"=c("12732","19504"),
#                                        "TYPE"=c("NATURAL GAS COMPRESSOR STATION","NATURAL GAS COMPRESSOR STATION"))
#manually adding facilities that are in GHGRP, but not in the EIA file (as ID'd
#from later in the code)

# All of the GHGRP stations are also in the EIA shapefile for our Philly domain

plotting_function <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Code and method/Scripts/Plotting_individual_sectors.R"
#the location of another script that just creates some functions for consistent,
#quick plotting

################################################################################
#load packages
i <- 1
packagecheck <- c("raster","sf","readxl","terra","ncdf4","geosphere","pracma")
while(i<=length(packagecheck)){
  if(length(find.package(packagecheck[i],quiet = TRUE))<1){
    install.packages(packagecheck[i],repos="https://repo.miserver.it.umich.edu/cran/")
  }
  i <- i+1
}

invisible(suppressPackageStartupMessages(lapply(packagecheck, library, character.only=TRUE)))
rm(packagecheck,i)

#raster + ncdf4 + terra = raster and .nc filetype functionalities
#readxl = ability to load more excel filetypes flexibly
#geosphere+sf = certain spatial functions
#pracma = haversine function to calculate distance in lat/long coords
################################################################################
#now quickly build the output raster matrix

d03_rast <- raster(nrows=diff(range(d03_bounding_box[,2]))/resolution, 
                   ncols=diff(range(d03_bounding_box[,1]))/resolution,
                   xmn=min(d03_bounding_box[,1]), xmx=max(d03_bounding_box[,1]),
                   ymn=min(d03_bounding_box[,2]), ymx=max(d03_bounding_box[,2]), 
                   crs=4326)

rm(d03_bounding_box,resolution)

################################################################################
#process the transmission pipeline data

first_col <- which(read_xlsx(EPA_file,sheet = EPA_Activity_sheet,.name_repair = "minimal")[,1]=="Segment/Source")
EPA_p1 <- read_xlsx(EPA_file,sheet = EPA_Activity_sheet,skip=first_col,col_names = T)

first_col <- which(read_xlsx(EPA_file,sheet = EPA_Emissions_sheet,.name_repair = "minimal")[,1]=="Segment/Source")
EPA_p2 <- read_xlsx(EPA_file,sheet = EPA_Emissions_sheet,skip=first_col,col_names = T)
#p2 = emissions, p1 = activity data.  Columns = year, rows = various types of
#sources.  First col is just to identify the first column of useable data

Data_list <- c("Pipeline Leaks","M&R (Trans. Co. Interconnect)","M&R (Farm Taps + Direct Sales)",
               "Pipeline venting")
#all the sources we're looking for, written exactly as in the EPA file

EPA_Pipeline <- data.frame("Type"=Data_list,
                           "Emissions"=as.numeric(unlist(sapply(Data_list,FUN=function(x){EPA_p2[which(EPA_p2[,1]==x)[1],year]})))*
                             1E9/(16.043*60*60*24*365),#convert from kt/yr to mol/s
                           "Total_stations"=as.numeric(unlist(sapply(Data_list,FUN=function(x){EPA_p1[which(EPA_p1[,1]==x)[1],year]})))*
                             1609.344,#convert from miles to meters
                           row.names = NULL)
#use sapply to find the row using data list, specify the column as the year and
#grab the relevant EF and activity data into a dataframe.

pipeline_EF <- sum(EPA_Pipeline[,2])/EPA_Pipeline[1,3] #mol/m/s
#sum of emissions / miles of pipelines (activity data from leaks entry)


Data_list <- c("Station Total Emissions","Dehydrator vents (Transmission)",
               "Flaring (Transmission)","Engines (Transmission)",
               "Turbines (Transmission)","Engines (Storage)",
               "Turbines (Storage)","Generators (Engines)",
               "Generators (Turbines)","Pneumatic Devices Transmission",
               "Station Venting Transmission")
#transmission station total + emissions during operations (vents, flaring,
#leaks, exhaust, etc.)

EPA_transmission_compressors <- data.frame("Type"=Data_list,
                                           "Emissions"=as.numeric(unlist(sapply(Data_list,FUN=function(x){EPA_p2[which(EPA_p2[,1]==x)[1],year]})))*
                                             1E9/(16.043*60*60*24*365),#convert from kt/yr to mol/s
                                           "Total_stations"=as.numeric(unlist(sapply(Data_list,FUN=function(x){EPA_p1[which(EPA_p1[,1]==x)[1],year]}))),
                                           row.names = NULL)
#use sapply to find the row using data list, specify the column as the year and
#grab the relevant EF and activity data into a dataframe.

Engine_transmission_fraction <- EPA_transmission_compressors[4,2]/sum(EPA_transmission_compressors[c(4,6),2])
Turbine_transmission_fraction <- EPA_transmission_compressors[5,2]/sum(EPA_transmission_compressors[c(5,7),2])
#calculate the ratio between transmission and storage emissions from engines and
#turbines

EPA_transmission_compressors[8,2] <- Engine_transmission_fraction*EPA_transmission_compressors[8,2]
EPA_transmission_compressors[9,2] <- Turbine_transmission_fraction*EPA_transmission_compressors[9,2]
#apply those ratios to the Generators for engines or turbines since they're not
#separated into transmission and storage

EPA_transmission_compressors <- EPA_transmission_compressors[c(1:5,8:11),]
#remove the storage data
compressor_avg_emissions <- sum(EPA_transmission_compressors[,2])/EPA_transmission_compressors[3,3] #mol/station/s
#sum of emissions / N stations (activity data from flaring entry)

rm(EPA_transmission_compressors,EPA_Pipeline,EPA_p1,EPA_p2,first_col,Data_list,
   Engine_transmission_fraction,Turbine_transmission_fraction)
################################################################################
#process the transmission pipeline data

# Load pipeline data into a SpatialLinesDataFrame
pipes <- terra::vect(file.path(Input_directory,EIA_pipeline_file))
# Crop to just larger than d03 - don't know if it's necessary to have this buffer but it can't hurt
e <- extent(d03_rast)+c(-0.5,0.5,-0.5,0.5)
pipes_crop <- crop(pipes,e)
pipes_crop <- as(pipes_crop,"Spatial")
#ran into errors just using sp, so instead using terra, then converting to sp
#(faster to reload than convert for the large one)

d03_rast[] <- 1:ncell(d03_rast)   # Fill raster with values
d03_polygons <- rasterToPolygons(d03_rast)   # Create a polygon for each raster grid cell
pipes_by_cell <- intersect(pipes_crop,d03_polygons)   # Now get a new SpatialLinesDataFrame that contains the pipes in each grid cell separately
pipes_by_cell$length <- lengthLine(pipes_by_cell)   # Find the length of each pipe in each cell in metres
x <- tapply(pipes_by_cell$length, pipes_by_cell$layer, sum)   # Get the total pipe length in each cell

pipes_rast <- raster(d03_rast)  # Create new raster to contain pipe emissions
pipes_rast[as.integer(names(x))] <- x*pipeline_EF   # Set values to the pipe length (in metres) in each cell, multiplied by the effective emission factor in mol/m/s
pipes_flux <- pipes_rast*1e9/(area(pipes_rast)*1e6)  # Calculate flux, mol/s to nmol/m2/s
pipes_flux[is.na(pipes_flux)]<-0

################################################################################
# Now onto the transmission compressor stations
compressors_EIA <- read.csv(file.path(Input_directory,EIA_compressor_file))
coordinates(compressors_EIA) <- ~LONGITUDE + LATITUDE
proj4string(compressors_EIA) <- CRS(SRS_string="EPSG:4326")  # WGS84
compressors_crop_EIA <- crop(compressors_EIA, d03_rast)

compressors_ghgrp <- read_xls(file.path(Input_directory,GHGRP_compressor_file),sheet=year,col_names = T,skip = 5)
#read the appropriate year.  
coordinates(compressors_ghgrp) <- ~LONGITUDE + LATITUDE
proj4string(compressors_ghgrp) <- crs(d03_rast) # WGS84
compressors_ghgrp_crop <- crop(compressors_ghgrp, d03_rast)

if(exists("EIA_compressor_update_df")){
  EIA_compressor_update <- SpatialPoints(EIA_compressor_update)
  temp <- EIA_compressor_update_df
  EIA_compressor_update_df <- compressors_crop_EIA@data[1:nrow(temp),]
  EIA_compressor_update_df[1:nrow(temp),] <- NA
  EIA_compressor_update_df[,c("NAME","STATE","ZIP","TYPE")] <- temp
  EIA_compressor_update <- SpatialPointsDataFrame(EIA_compressor_update,EIA_compressor_update_df)
  crs(EIA_compressor_update) <- crs(compressors_crop_EIA)
  #convert the manually added GHGRP facilities to an equivalent format
  
  compressors_final <- rbind(compressors_crop_EIA,EIA_compressor_update)
  #combine them
  
  rm(temp,EIA_compressor_update_df,EIA_compressor_update)
}else{
  compressors_final <- compressors_crop_EIA
}

compressors_final$emiss <- compressor_avg_emissions
#default for all are the national avg

################################################################################
#Now add the emissions data from the ones in GHGRP.  First scale them so that
#the domain-averaged GHGRP emissions match the GHGI average emissions.

match_numbers <- apply(compressors_ghgrp_crop@coords,MARGIN=1,FUN=function(x){
  which.min(apply(compressors_final@coords,MARGIN=1,FUN=function(y){
    haversine(x,y)}))})
Distances <- vector(length=length(match_numbers))
for(A in 1:length(match_numbers)){
  Distances[A] <- haversine(compressors_ghgrp_crop@coords[A,],compressors_final@coords[match_numbers,][A,])
}
#compressor names differ between these 2 datasets, so just calculate which EIA
#and GHGRP facilities are closest
if(length(match_numbers)>0){
  check <- cbind(compressors_ghgrp_crop@data[,c("FACILITY.NAME","CITY.NAME","STATE")],
                 compressors_final@data[match_numbers,c("NAME","CITY","STATE")],
                 Distances)
  colnames(check) <- c("GHGRP_name","GHGRP_city","GHGRP_state","EIA_name","EIA_city","EIA_state","Distance_km")
  #subset to variables that should make it clear if the facilities are or are
  #not the same, rename for clarity
  View(check)
  stop("Line 156 - Check the subset facilities (GHGRP vs EIA).  These are matched based on the nearest facility for each GHGRP facility, but names, distances, and details should be checked for reasonable agreement.  If they seem to be the same facilities, continue.  If not, make sure that they're not in the EIA files at all, then add the GHGRP facilities to EIA_compressor_update at the start to add them as a new facility in the EIA dataset, rerun, and continue.")
  View(cbind(compressors_final@coords,compressors_final@data[,3:38]))
}
#Now check if the names seem similar (for Philly, all seem to have a perfect
#match)

GHGRP_scaling <- compressor_avg_emissions/mean(compressors_ghgrp_crop$`GHG QUANTITY (METRIC TONS CO2e)`*1e6/(25*16.043*365*24*60*60))

compressors_final$emiss[match_numbers] <- compressors_ghgrp_crop$`GHG QUANTITY (METRIC TONS CO2e)`*1e6/(25*16.043*365*24*60*60)*GHGRP_scaling
#if you have exact matches based on location for each and are confident based on
#names, etc that they are the same facilities, simply replace their emissions
#accordingly

compressor_rast <- rasterize(compressors_final, d03_rast, "emiss", fun=sum) # in mol/s
compressor_flux <- compressor_rast*1e9/(area(compressor_rast)*1e6)  # Calculate flux in nmol/m2/s
compressor_flux[is.na(compressor_flux)]<-0

################################################################################
# And save the output

# Save point sources as csv files - first just the raw dataframe
write.csv(compressors_final, file.path(Output_directory,"NG_trans_compressors_all.csv"))

# Now just the names, coordinates and emissions
compressors_output <- data.frame(compressors_final$NAME,coordinates(compressors_final),compressors_final$emiss)
names(compressors_output) <- c('Site_Name','Longitude','Latitude','Emission_mol_per_s')
write.csv(compressors_output,file.path(Output_directory,"NG_trans_compressors.csv"),row.names=FALSE)

writeRaster(pipes_flux,
            file.path(Output_directory,"NG_trans_pipes.nc"),
            force_v4=TRUE,
            varname='methane_emissions',
            varunit='nmol/m2/s',
            longname='Methane emissions from natural gas transmission pipelines (inc. leaks, transmission M&R stations, farm taps, direct sales and pipeline venting)',
            NAflag=-9999,
            overwrite=TRUE)

writeRaster(compressor_flux,
            file.path(Output_directory,"NG_trans_compressors.nc"),
            force_v4=TRUE,
            varname='methane_emissions',
            varunit='nmol/m2/s',
            longname='Methane emissions from natural gas transmission compressor stations',
            NAflag=-9999,
            overwrite=TRUE)
################################################################################
#Finally, load up some functions/datasets and plot up this output nicely

source(plotting_function,verbose=F)

log_plot(compressor_flux,filename="NG_trans_compressors",
         "NG transmission - compressors\n GHGRP reporters + (average GHGI emissions distributed using\n Homeland Infrastructure Foundation-Level Database)")

not_log_plot(pipes_flux,filename="NG_trans_pipes",
             "NG transmission - pipelines\n EIA pipeline data * EPA EF")

dir.create("Summed_Sectors",showWarnings = F)
setwd("Summed_Sectors")

Summed_NG_transmission = compressor_flux+pipes_flux
log_plot(Summed_NG_transmission,
         "NG Transmission Sector\nEIA for pipelines + HFILD/GHGRP for compressors")

