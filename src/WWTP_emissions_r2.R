## WWTP_emissions_r2.R
## In use: 2021-11-02 20:00
## Finalized: 2023-02-03
#
# Note - to convert to fluxes this code uses the raster packages area function.
# This simplifies the area calculation of a lat/long box and is not appropriate
# near the poles
#
#
# The GEPA does something slightly weird with these emissions - see WWTP_explainer.R for details
#
# The EPA national report essentially has 3 categories of WW emissions (2019 emissions from 2021 report in brackets):
# domestic septic systems (232 kt), domestic centralised systems (250 kt), and industrial emissions (254 kt)
#
# The CWNS has the locations of publicly owned WWTPs and other facilities - here's a quote from the proposal abstract:
# "The respondents who provide this information to EPA are state agencies responsible for environmental pollution control
# and local facility contacts who provide documentation to the states."
#
# So the domestic centralised emissions from the EPA national inventory report can be allocated using the CWNS using the reported
# existing municipal flow ("EXIST_MUNICIPAL") from the 2012 submission
#
# Some septic system population counts are present in the CWNS, but it isn't clear to me if this represents all the septic
# systems or just ones that are managed by local facilities. If I want to use this, I'll also have to work out how to
# spatially allocate these emissions based on the reporting local facility
# For now, distribute septic emissions according to the NLCD land cover classes "Developed-Open Space" and "Developed-Low Intensity"
# Method 1:
# We know the combined national total (in 2016) for these classes was 352032 km2, from this paper:
# https://doi.org/10.1016/j.isprsjprs.2020.02.019
# I made a version of the NLCD for our domain that had 1 for these classes and 0 for all other classes
# Then regridded this using xesmf to get the fraction of each grid cell that was one of these classes
# Load that in here, and calculate the area of each grid cell as a fraction of the national total
# Method 2:
# Do the same as Method 1, but using estimated  state-total emissions instead of national emissions
# These are calculated by multiplying an estimate for the number of people whose
# waste is treated in onsite systems within each state by the emission factor (10.7 gCH4/capita/day) from the EPA
# GHGI.  To estimate the number of people served by onsite systems, multiply the US census state
# population estimate for 2019 by an estimate of the fraction of people served by onsite systems.
# For some states, this septic fraction estimate can be taken from the 2021 American Housing Survey.
# Such recent data is not available for most states.  In those cases we take the
# septic fraction reported in the 1990 US census (the last to provide this data at the individual state level).
# To correct for recent changes in septic fraction, we multiply these state-level values from 1990 by the ratio of
# whole-US septic fraction in 2019 (16.3%; from the American Housing Survey) to whole-US septic fraction in 1990 (24.1%). 
#
# The EPA industrial WW emissions come mainly from meat & poultry
# These industries have their on-site treatment systems, and so are not included in the CWNS
# Some of these emissions are reported to GHGRP - we can use those here
# Emissions from non-reporters in this category are not currently included in this inventory
#
# Note that actually some of the emissions in the EPA inventory (15% domestic, 7% industrial) come from effluent, not treatment
# These may not be located entirely at the WWTPs, but it's really hard to know where to put them otherwise
#
################################################################################
#Manually defined variables

Input_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw data/"
Output_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed/"

d03_bounding_box <- cbind(c(-76.65,-73.65),c(38.97,40.97))
resolution <- 0.01
#Will be used to generate a blank raster for the output to be built onto, WGS84
#CRS.  Resolution in deg, bounding box in lat/long.

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


################################################################################
#load packages
i <- 1
packagecheck <- c("raster","ncdf4","readxl","sf")
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
# First load in and prep 2012 CWNS data

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

cwns_2012_trans <- rbind(cwns_2012_wgs84,cwns_2012_nad27_trans,cwns_2012_nad83_trans)

tot_flow <- sum(cwns_2012_trans$EXIST_MUNICIPAL, na.rm=T)

################################################################################
# distribute EPA emissions across the CWNS facilities

# Take total emissions for each category from the 2021 EPA report (values for 2019 in kt)
central_EPA_emiss <- GHGI_national_Wastewater_nonSeptic*1e9/(16.043*365*24*60*60)   #kt/y to mol/s

cwns_2012_trans$emiss <- central_EPA_emiss*cwns_2012_trans$EXIST_MUNICIPAL/tot_flow   # in mol/s

# Rasterise
cwns_crop <- crop(cwns_2012_trans,d03_rast)
cwns_crop_filt <- subset(cwns_crop,!is.na(emiss))
central_rast <- rasterize(cwns_crop_filt, d03_rast, "emiss", fun=sum)

central_flux <- central_rast*1e9/(area(central_rast)*1e6)  # Calculate flux in nmol/m2/s
central_flux[is.na(central_flux)]<-0

# Save point sources as csv files - first just the raw dataframe
write.csv(cwns_crop_filt, file.path(Output_directory,"WWTP_municipal_all.csv"))

# Now just the names, coordinates and emissions
cwns_crop_output <- data.frame(cwns_crop_filt$FACILITY_NAME,coordinates(cwns_crop_filt),cwns_crop_filt$emiss)
names(cwns_crop_output) <- c('Site_Name','Longitude','Latitude','Emission_mol_per_s')
write.csv(cwns_crop_output,file.path(Output_directory,'WWTP_municipal.csv'),row.names=FALSE)

################################################################################
#Now septic systems.  Load reprojected output  - this is divided by state and
#open vs low intensity land use.

septic_EPA_emiss <- GHGI_national_Wastewater_Septic*1e9/(16.043*365*24*60*60)   #kt/y to mol/s
tot_nlcd_area <- Total_national_open_or_low_int_area  # all states in km2

# blank output to combine the states
domain_nlcd_frac <- raster(open_rasterfile[1])
septic_emiss2 <- raster(open_rasterfile[1])
septic_emiss2[] <- 0
domain_nlcd_frac[] <- 0

states <- st_read(state_shapefile)
for(A in 1:length(open_rasterfile)){
  open <- raster(open_rasterfile[A])
  low_int <- raster(low_int_rasterfile[A])
  
  # Method 1:
  # Calculate fractional area (of national total) in each grid cell
  nlcd_frac <- open + low_int
  domain_nlcd_frac <- nlcd_frac + domain_nlcd_frac
  
  # Method 2:
  # Calculate state-by-state totals and disaggregate within each state
  states_trans <- st_transform(states,crs(nlcd_frac))
  
  state_poly <- subset(states_trans, STUSPS==State_info[A,1])
  Tot_area <- sum(nlcd_state_total_areas[which(State_info[A,1]==nlcd_state_total_areas[,1]),c(2,3)]) # total area of both classes in km2 from nlcd_state_total_areas.csv
  pop <- State_info[A,2]
  if(State_info[A,4]=="scaled"){
    septic_frac <- State_info[A,3]*National_info[2,2]/National_info[1,2]
  }else if(State_info[A,4]=="reported"){
    septic_frac <- State_info[A,3]
  }else{
    stop("State info's method needs to be \"scaled\" or \"reported\" ")
  }
  state_tot_emiss <- pop*septic_frac*GHGI_septic_EF/(16.043*24*60*60)  #in mol/s (EF is in g/capita/day)
  state_emiss <- state_tot_emiss*nlcd_frac*area(nlcd_frac)/Tot_area #gridded and distributed equally in mol/s
  
  septic_emiss2 <- septic_emiss2+state_emiss
  #add this state's emissions in
  
  State_info$total_emissions_mol_per_s[A] <- state_tot_emiss
  cat("Finished",State_info[A,1],"\n")
}

#calculate some info to compare the 2 methods.  The actual calculation is the
#same, it's just the emissions per area that changes.
State_info$total_septic_area_km2 <- rowSums(nlcd_state_total_areas[,c(2,3)])
State_info$emission_per_area <- State_info$total_emissions/State_info$total_septic_area
State_info$State_to_national_method_ratio <- State_info$emission_per_area/(septic_EPA_emiss/tot_nlcd_area)

# Method 1:
# Now multiply by total EPA emissions
septic_emiss <- septic_EPA_emiss*domain_nlcd_frac*area(domain_nlcd_frac)/tot_nlcd_area  # in mol/s
septic_flux <- septic_emiss*1e9/(area(septic_emiss)*1e6)  # Calculate flux in nmol/m2/s
septic_flux[is.na(septic_flux)]<-0

# Method 2:
# Now converting the totals to a per/area gridded product
septic_flux2 <- septic_emiss2*1e9/(area(septic_emiss2)*1e6)  # Calculate flux in nmol/m2/s
septic_flux2[is.na(septic_flux2)]<-0


################################################################################
# Now the GHGRP industrial wastewater facilities

ghgrp <- read_xls(GHGRP_file,sheet=year,col_names = T,skip = 5)

#read the appropriate year.  If it's just the one year, the error will flag and
#it will load that file without issue.

coordinates(ghgrp) <- ~LONGITUDE + LATITUDE
proj4string(ghgrp) <- CRS(SRS_string="EPSG:4326")  # WGS84
ghgrp_crop <- crop(ghgrp, d03_rast)
ghgrp_crop$emiss <- ghgrp_crop$`GHG QUANTITY (METRIC TONS CO2e)`*1e6/(25*16.043*365*24*60*60)   # MT CO2e/yr to mol/s of CH4

# Now rasterise
ghgrp_rast <- rasterize(ghgrp_crop, d03_rast, "emiss", fun=sum)
ghgrp_flux <- ghgrp_rast*1e9/(area(ghgrp_rast)*1e6)  # Calculate flux in nmol/m2/s
ghgrp_flux[is.na(ghgrp_flux)]<-0

# Save point sources as csv files - first just the raw dataframe
write.csv(ghgrp_crop, file.path(Output_directory,"WWTP_industrial_all.csv"))

# Now just the names, coordinates and emissions
ghgrp_crop_output <- data.frame(ghgrp_crop$`FACILITY NAME`,coordinates(ghgrp_crop),ghgrp_crop$emiss)
names(ghgrp_crop_output) <- c('Site_Name','Longitude','Latitude','Emission_mol_per_s')
write.csv(ghgrp_crop_output, file.path(Output_directory,"WWTP_industrial.csv"),row.names = F)

#now save the comparison across the methods
write.csv(State_info, file.path(Output_directory,"WWTP_septic_method_comparison.csv"),row.names = F)

# Write the rasters
writeRaster(central_flux,
            file.path(Output_directory,'Wastewater_dom_central.nc'),
            force_v4=TRUE,
            varname='methane_emissions',
            varunit='nmol/m2/s',
            longname='Methane emissions from treatment of domestic wastewater at centralised municipal treatment plants',
            NAflag=-9999,
            overwrite=TRUE)

writeRaster(septic_flux,
            file.path(Output_directory,'Wastewater_dom_septic_national.nc'),
            force_v4=TRUE,
            varname='methane_emissions',
            varunit='nmol/m2/s',
            longname='Methane emissions from onsite treatment of domestic wastewater (e.g. septic tanks), based on calculations at the state level',
            NAflag=-9999,
            overwrite=TRUE)

writeRaster(septic_flux2,
            file.path(Output_directory,'Wastewater_dom_septic_bystate.nc'),
            force_v4=TRUE,
            varname='methane_emissions',
            varunit='nmol/m2/s',
            longname='Methane emissions from onsite treatment of domestic wastewater (e.g. septic tanks), based on EPA national values',
            NAflag=-9999,
            overwrite=TRUE)

writeRaster(ghgrp_flux,
            file.path(Output_directory,'Wastewater_ind.nc'),
            force_v4=TRUE,
            varname='methane_emissions',
            varunit='nmol/m2/s',
            longname='Methane emissions from industrial wastewater treatment plants',
            NAflag=-9999,
            overwrite=TRUE)

################################################################################
#Finally, load up some functions/datasets and plot up this output nicely

source(plotting_function)

not_log_plot(septic_flux2,filename="Wastewater_dom_septic_bystate",
             "Domestic Wastewater - Septic v2\n estimated state septic distributed using \ndeveloped open space/low intensity land cover",
             min(minValue(septic_flux2),minValue(septic_flux)),
             max(maxValue(septic_flux2),maxValue(septic_flux)))

not_log_plot(septic_flux,filename="Wastewater_dom_septic_national",
             "Domestic Wastewater - Septic\n national EPA septic distributed using \ndeveloped open space/low intensity land cover",
             min(minValue(septic_flux2),minValue(septic_flux)),
             max(maxValue(septic_flux2),maxValue(septic_flux)))

log_plot(central_flux,filename="Wastewater_dom_central",
         "Domestic Wastewater -\n EPA total distributed using \nClean Watersheds Needs Survey")

log_plot(ghgrp_flux,filename="Wastewater_ind",
         "Industrial Wastewater -\n GHGRP Reporters")

dir.create("Summed_Sectors",showWarnings = F)
setwd("Summed_Sectors")

Summed_wastewater_treatment = central_flux+septic_flux+ghgrp_flux

log_plot(Summed_wastewater_treatment,
         "Wastewater Treatment Sector\nGHGI total distributed with CWNS (Domestic facilities) and GHGRP (industrial)\nand developed open space/low intensity NLCD land cover (Septic)")

