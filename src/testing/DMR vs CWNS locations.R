#Copied from the actual WWTP code.  Modified to just investigate facilities that report to EITHER DMR or CWNS, rather than both.  
################################################################################
#Manually defined variables

Input_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw data/"
Output_directory <- "C:/Users/krist/Desktop"

d03_bounding_box <- cbind(c(-76.65,-73.65),c(38.97,40.97))
resolution <- 0.01
#Will be used to generate a blank raster for the output to be built onto, WGS84
#CRS.  Resolution in deg, bounding box in lat/long.

# DMR_file <- file.path(Input_directory,'DMR_2022_from_8_10_2023.csv')
DMR_file <- file.path(Input_directory,'DMR_2012_from_8_10_2023.csv')
#Discharge Monitoring Report (DMR) from
#(https://echo.epa.gov/trends/loading-tool/water-pollution-search) for all
#facilities in the US.

CWNS_file <- file.path(Input_directory,'CWNS_merged_data_2012_KH.xlsx')
# ACCESS database from (https://www.epa.gov/cwns) that converted to xlsx

low_int_rasterfile <- list.files(pattern=glob2rx("*low_int_regridded.nc"),path="G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed/",full.names = T)
open_rasterfile <- list.files(pattern=glob2rx("*open_regridded.nc"),path="G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed/",full.names = T)
#output from NLCD_fractions_by_state after reprojecting

GHGRP_file <- file.path(Input_directory,"US_GHGRP_WWTP_only_all_years.xls")
year <- "2019"

state_shapefile <- "G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Shapefiles/tl_2022_us_state/tl_2022_us_state.shp"
#Census tigerline files for states

GHGI_national_Wastewater_Septic <- 227 #kt CH4/yr
GHGI_national_Wastewater_nonSeptic <- 246 #kt CH4/yr
#from GHGI table 7-9

GHGI_septic_EF <- 10.7 #g/capita/day
#from GHGI table 7-10

Total_national_open_or_low_int_area <- 352032 #km2
#From https://doi.org/10.1016/j.isprsjprs.2020.02.019

State_info <- data.frame("State"=c("DE", "MD", "NJ", "NY", "PA"),
                         "Population"=c(1018396,6164660,9261699,19677151,12972008),
                         "Septic_Fraction"=c(0.257,0.181,0.116,0.159,0.245),
                         "Method"=c("scaled","scaled","scaled","reported","scaled"))
#Pulled from census data.  method is either scaled - i.e., from an old census
#report, or reported, i.e., use as is from a recent census report

National_info <- data.frame("Year"=c(1990,2021),
                            "Septic_Fraction"=c(0.241,0.152))
#Only needed if any states are using the scaled method.  National septic
#fraction in the year of interest and the year that the scaled state reported a
#septic fraction

plotting_function <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Code and method/Scripts/Plotting_individual_sectors.R"
#the location of another script that just creates some functions for consistent,
#quick plotting

# Municipal_file <- "DMR"
#either CWNS for 2012 discharge flow or DMR for any selected year. The 2
#datasets do not seem to be consistent and are missing data from certain
#facilities.

# Municipal_method <- "Moore_linear"
#GHGI, Moore_EF, or Moore_linear.  Moore et al.,
#(https://doi.org/10.1021/acs.est.2c05373) estimated a direct empirical
#relationship between flow and emission rates using a linear fit on a log-log
#plot, and also calculated an equivalent organic load (BOD) emission factor.
#Their estimate was ~2x that of the GHGI.  The GHGI method takes GHGI totals and
#distributes it to all facilities based on flow

################################################################################
#load packages
i <- 1
packagecheck <- c("raster","ncdf4","readxl","sf","clipr")
while(i<=length(packagecheck)){
  if(length(find.package(packagecheck[i],quiet = TRUE))<1){
    install.packages(packagecheck[i],repos="https://repo.miserver.it.umich.edu/cran/")
  }
  i <- i+1
}

invisible(suppressPackageStartupMessages(lapply(packagecheck, require, character.only=TRUE)))
rm(packagecheck,i)

#raster + ncdf4 = raster and .nc filetype functionalities
#sf = additional spatial functionalities
#readxl = ability to load more excel filetypes flexibly
################################################################################
#now quickly build the output raster matrix

d03_rast <- raster(nrows=diff(range(d03_bounding_box[,2]))/resolution, 
                   ncols=diff(range(d03_bounding_box[,1]))/resolution,
                   xmn=min(d03_bounding_box[,1]), xmx=max(d03_bounding_box[,1]),
                   ymn=min(d03_bounding_box[,2]), ymx=max(d03_bounding_box[,2]), 
                   crs=4326)

rm(d03_bounding_box,resolution)

################################################################################
#quickly ensure that the state data is all in the same order, alphabetical

nlcd_state_total_areas <- read.table(file.path(Output_directory,"nlcd_state_total_areas.csv"),header=T,sep=",")
#output from NLCD_fractions_by_state.R

low_int_rasterfile <- sort(low_int_rasterfile)
open_rasterfile <- sort(open_rasterfile)
State_info <- State_info[order(State_info$State),]
nlcd_state_total_areas <- nlcd_state_total_areas[order(nlcd_state_total_areas$X),]

################################################################################
# First load in and prep the flow data

cwns_2012 <- read_xlsx(CWNS_file)

#ID any that are in the western or southern hemisphere (- coordinates)
Western_hemis <- grep("W",cwns_2012$LONGITUDE)
Southern_hemis <- grep("S",cwns_2012$LATITUDE)
#remove the hemisphere text so we can make numeric
cwns_2012$LATITUDE <- gsub("N|S","",cwns_2012$LATITUDE)
cwns_2012$LONGITUDE <- gsub("W|E","",cwns_2012$LONGITUDE)
cwns_2012$LATITUDE <- as.numeric(cwns_2012$LATITUDE)
cwns_2012$LONGITUDE <- as.numeric(cwns_2012$LONGITUDE)
#make those in the S or W hemispheres the appropriate negative coordinates
cwns_2012$LATITUDE[Southern_hemis] <- cwns_2012$LATITUDE[Southern_hemis]*-1
cwns_2012$LONGITUDE[Western_hemis] <- cwns_2012$LONGITUDE[Western_hemis]*-1

#Pick only those entries that have lat and lon coordinates
cwns_2012_filt <- subset(cwns_2012,!is.na(LATITUDE) & !is.na(LONGITUDE))
coordinates(cwns_2012_filt) <- ~LONGITUDE + LATITUDE

# Nearly all the entries are NAD83, but some aren't
# Convert everything over to WGS84
# Assume blank or unknown entries are NAD83
cwns_2012_wgs84 <- subset(cwns_2012_filt, HORIZONTAL_COORDINATE_DATUM=="World Geodetic System of 1984")
cwns_2012_nad27 <- subset(cwns_2012_filt, HORIZONTAL_COORDINATE_DATUM=="North American Datum of 1927")
cwns_2012_nad83 <- subset(cwns_2012_filt, HORIZONTAL_COORDINATE_DATUM!="North American Datum of 1927" & HORIZONTAL_COORDINATE_DATUM!="World Geodetic System of 1984")

proj4string(cwns_2012_wgs84) <- CRS(SRS_string="EPSG:4326")  # WGS84
proj4string(cwns_2012_nad27) <- CRS(SRS_string="EPSG:4267")  # NAD27
proj4string(cwns_2012_nad83) <- CRS(SRS_string="EPSG:4269")  # NAD83

cwns_2012_nad27_trans <- spTransform(cwns_2012_nad27,crs(cwns_2012_wgs84))
cwns_2012_nad83_trans <- spTransform(cwns_2012_nad83,crs(cwns_2012_wgs84))

Municipal_flow <- rbind(cwns_2012_wgs84,cwns_2012_nad27_trans,cwns_2012_nad83_trans)

tot_flow <- sum(Municipal_flow$EXIST_MUNICIPAL, na.rm=T)



DMR_data <- read.csv(DMR_file,skip=3)
colnames(DMR_data) <- gsub("\\.","\\_",colnames(DMR_data))
Municipal_flow_DMR <- subset(DMR_data,!is.na(Facility_Latitude) & !is.na(Facility_Longitude))
coordinates(Municipal_flow_DMR) <- ~Facility_Longitude + Facility_Latitude
proj4string(Municipal_flow_DMR) <- CRS(SRS_string="EPSG:4326")
tot_flow_DMR <- sum(DMR_data$Average_Flow__MGD_, na.rm=T)

################################################################################
# Separate to just NY and NJ

DMR <- Municipal_flow_DMR[Municipal_flow_DMR@data$State=="NY" | Municipal_flow_DMR@data$State=="NJ",]
CWNS <- Municipal_flow[Municipal_flow@data$STATE=="NY" | Municipal_flow@data$STATE=="NJ",]

tot_flow_DMR <- sum(DMR@data$Average_Flow__MGD_, na.rm=T)
tot_flow <- sum(CWNS@data$EXIST_MUNICIPAL, na.rm=T)

################################################################################
#ID facilities without a match within 3 km in the other and those that almost
#perfectly match

match_numbers <- apply(DMR@coords,MARGIN=1,FUN=function(x){
  which.min(apply(CWNS@coords,MARGIN=1,FUN=function(y){
    haversine(x,y)}))})
Distances <- vector(length=length(match_numbers))
for(A in 1:length(match_numbers)){
  Distances[A] <- haversine(DMR@coords[A,],CWNS@coords[match_numbers,][A,])
}

matched_data <- cbind(DMR@data[which(Distances<1),],CWNS@data[match_numbers,][which(Distances<1),],
                      DMR@coords[which(Distances<1),],CWNS@coords[match_numbers,][which(Distances<1),])
only_in_DMR <- DMR@data[which(Distances>3),]
only_in_DMR <- cbind(only_in_DMR,Distances[which(Distances>3)],DMR@coords[which(Distances>3),])


match_numbers <- apply(CWNS@coords,MARGIN=1,FUN=function(x){
  which.min(apply(DMR@coords,MARGIN=1,FUN=function(y){
    haversine(x,y)}))})
Distances <- vector(length=length(match_numbers))
for(A in 1:length(match_numbers)){
  Distances[A] <- haversine(CWNS@coords[A,],DMR@coords[match_numbers,][A,])
}

only_in_CWNS <- CWNS@data[which(Distances>3),]
only_in_CWNS <- cbind(only_in_CWNS,Distances[which(Distances>3)],CWNS@coords[which(Distances>3),])
#export

clipr::write_clip(only_in_CWNS[,c("STATE","FACILITY_NAME","EXIST_MUNICIPAL","Distances[which(Distances > 3)]","LATITUDE","LONGITUDE")])
clipr::write_clip(only_in_DMR[,c("State","Facility_Name","Average_Flow__MGD_","Distances[which(Distances > 3)]","Facility_Latitude","Facility_Longitude")])

clipr::write_clip(only_in_DMR[,c("State","Facility_Name","Average_Flow__MGD_","Distances[which(Distances > 3)]","Facility_Latitude","Facility_Longitude")])

clipr::write_clip(matched_data[which(matched_data$Average_Flow__MGD_>50),
                               c("State","Facility_Name","FACILITY_NAME",
                                 "Average_Flow__MGD_","EXIST_MUNICIPAL",
                                 "Facility_Latitude","Facility_Longitude",
                                 "LATITUDE","LONGITUDE")])


