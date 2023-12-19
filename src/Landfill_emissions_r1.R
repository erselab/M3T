## Landfill_emissions_r1.R
## Developed: 2021-10-22 17:00
## Finalized: 2023-02-03
#
# Calculate emissions from landfills
# There are 2 sources of data:
# GHGRP - has emissions but only certain landfills
# LMOP - has more landfills than GHGRP, and some details about them, but not 
# emissions
#
# Note - to convert to fluxes this code uses the raster packages area function.
# This simplifies the area calculation of a lat/long box and is not appropriate
# near the poles
#
# The national EPA (GHGI) inventory total comes from taking the GHGRP emissions 
# and applying a scale factor to account for the non-reporting landfills.
# This scale factor is based on the LMOP database and the WBJ directory (which it appears you have to pay for: https://www.wasteinfo.com/diratlas.htm)
# Details are given on page 168 (A-457) of this report:
# https://www.epa.gov/sites/production/files/2021-04/documents/us-ghg-inventory-2021-annex-3-additional-source-or-sink-categories-part-b.pdf
# They identified 1544 landfills that accepted MSW between 1940 and 2016 and had never reported to the GHGRP
# For now, calculate total emissions for non-GHGRP-reporting landfills in LMOP as a residual between the GHGI and GHGRP totals
# Then allocate equally among non-GHGRP reporters at the national level (note that many landfills don't report waste in place)
# Note that we can only do MSW landfills like this, as industrial waste landfills don't report to LMOP
# There are far fewer GHGRP industrial waste landfills
# To get the non-reporters I could potentially trawl through the FRS data and work out what is an industrial landfill and what isn't
# But leave this out here and just use the GEPA industrial landfill emissions
################################################################################
#User input

Input_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw data/"
Output_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed/"

d03_bounding_box <- cbind(c(-76.65,-73.65),c(38.97,40.97))
resolution <- 0.01
#Will be used to generate a blank raster for the output to be built onto, WGS84
#CRS.  Resolution in deg, bounding box in lat/long.

ghgrp_file <- "US_GHGRP_Landfills_only_all_years.xls"
LMOP_file <- "lmopdata(Aug_22)_landfill_only.xlsx"
year <- "2019"

GHGI_value <- 3943 #Gg CH4/yr
#total national municipal landfill emissions from the GHGI

# 1 site stopped reporting without a valid reason, as ID'd from an error message
# in the code - instead using the most recent reported value for that facility
LMOP_update <- data.frame("Facility"="Kearny 1-D",
                          "GHGRP.ID"=1011381,
                          "CH4.Data"=85308,
                          "latest.year"=2016)

plotting_function <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Code and method/Scripts/Plotting_individual_sectors.R"
#the location of another script that just creates some functions for consistent,
#quick plotting
################################################################################
#load packages
i <- 1
packagecheck <- c("raster","ncdf4","readxl")
while(i<=length(packagecheck)){
  if(length(find.package(packagecheck[i],quiet = TRUE))<1){
    install.packages(packagecheck[i],repos="https://repo.miserver.it.umich.edu/cran/")
  }
  i <- i+1
}

suppressPackageStartupMessages(invisible(lapply(packagecheck, library, character.only=TRUE)))
rm(packagecheck,i)

#raster + ncdf4 = raster and .nc filetype functionalities
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
# First load all data and convert as necessary

ghgrp <- read_xls(file.path(Input_directory,ghgrp_file),sheet=year,col_names = T,skip = 5)
#read the appropriate year.  

#convert to a spatial object, crop to d03, convert units
coordinates(ghgrp) <- ~LONGITUDE + LATITUDE
proj4string(ghgrp) <- CRS(SRS_string="EPSG:4326")  # WGS84
ghgrp_crop <- crop(ghgrp, d03_rast)
ghgrp_crop$emiss <- ghgrp_crop$`GHG QUANTITY (METRIC TONS CO2e)`*1e6/(25*16.043*365*24*60*60)   # MT CO2e/yr to mol/s of CH4

# Now calculate national totals
ghgrp_national <- sum(ghgrp$`GHG QUANTITY (METRIC TONS CO2e)`)/25000   # MT CO2e/yr to Gg CH4/yr
EPA_total <- GHGI_value 
non_ghgrp_total <- EPA_total - ghgrp_national

# Read in LMOP and remove those in GHGRP
LMOP <- read_xlsx(file.path(Input_directory,LMOP_file),sheet="LMOP Database",col_names = T)
LMOP_non_ghgrp <- LMOP[!(LMOP$`GHGRP ID` %in% ghgrp$`GHGRP ID`),] 

#This has some nans in, remove those
LMOP_filt <- subset(LMOP_non_ghgrp,!is.na(Latitude))
coordinates(LMOP_filt) <- ~Longitude + Latitude
proj4string(LMOP_filt) <- CRS(SRS_string="EPSG:4326")  # WGS84
LMOP_crop <- crop(LMOP_filt, d03_rast)

# Find avg emission per non-GHGRP LMOP landfill (including the ones with no coordinates)
avg_non_ghgrp <- non_ghgrp_total/nrow(LMOP_non_ghgrp)
# For comparison, calculate avg ghgrp
avg_ghgrp <- ghgrp_national/nrow(ghgrp)
################################################################################
#Check for any potential GHGRP reporters who've stopped reporting to check if
#they did so without a valid reason

ghgrp_old <- read_xls(file.path(Input_directory,ghgrp_file),sheet="2010",col_names = T,skip = 5)
for(old_year in as.character(2011:(as.numeric(year)-1))){
  ghgrp_one_year <- read_xls(file.path(Input_directory,ghgrp_file),sheet=old_year,col_names = T,skip = 5)
  ghgrp_old <- rbind(ghgrp_one_year,ghgrp_old)
}
#load in and combine GHGRP data from 2010 to the year before the year of
#interest

coordinates(ghgrp_old) <- ~LONGITUDE + LATITUDE
proj4string(ghgrp_old) <- CRS(SRS_string="EPSG:4326")  # WGS84
ghgrp_old <- crop(ghgrp_old, d03_rast)
#same as previous section
ghgrp_old <- ghgrp_old[!duplicated(ghgrp_old$`GHGRP ID`),]
ghgrp_old <- ghgrp_old[!(ghgrp_old$`GHGRP ID` %in% ghgrp$`GHGRP ID`),]
ghgrp_old <- ghgrp_old[!(ghgrp_old$`GHGRP ID` %in% LMOP_update$GHGRP.ID),]
#keep only 1 row per facility (the most recent comes first, so it's the one that
#will be kept) and remove any that still report in the year of interest or have
#already been dealt with in LMOP_update.

if(nrow(ghgrp_old)>0){
  View(ghgrp_old@data)
  stop("Line 137 - The table lists the most recent data for all GHGRP facilities that are no longer reporting.  Look into these missing facilities on GHGRP's website to check if they stopped reporting for a valid reason or not.  If not, add them to LMOP_update in the beginning, rerun, then run the remainder of the code.  If all had a valid reason, simply continue.")
}
#check these no longer reporting facilities to check if they stopped reporting
#without a valid reason

#sort them by GHGRP ID so that matches can be made more easily
LMOP_crop <- LMOP_crop[order(LMOP_crop$`GHGRP ID`),]
LMOP_update <- LMOP_update[order(LMOP_update$GHGRP.ID),]

# Assign the avg emissions to LMOP landfills
LMOP_crop$emiss <- avg_non_ghgrp*1e9/(16.043*365*24*60*60)   #Gg CH4/yr to mol/s of CH4
# and assign the non-reporters who stopped reporting without a valid reason
LMOP_update$CH4.Data <- LMOP_update$CH4.Data*1e6/(25*16.043*365*24*60*60) #MT CO2e/yr to mol/s of CH4
LMOP_crop$emiss[LMOP_crop$`GHGRP ID`%in%LMOP_update$GHGRP.ID] <- LMOP_update$CH4.Data

rm(ghgrp_old,ghgrp_one_year)
################################################################################
# Now rasterise and save

ghgrp_rast <- rasterize(ghgrp_crop, d03_rast, "emiss", fun=sum)
ghgrp_flux <- ghgrp_rast*1e9/(area(ghgrp_rast)*1e6)  # Calculate flux, mol/s to nmol/m2/s
ghgrp_flux[is.na(ghgrp_flux)]<-0

LMOP_rast <- rasterize(LMOP_crop, d03_rast, field="emiss", fun=sum)
LMOP_flux <- LMOP_rast*1e9/(area(LMOP_rast)*1e6)  # Calculate flux, mol/s to nmol/m2/s
LMOP_flux[is.na(LMOP_flux)]<-0

# Save point sources as csv files - first just the raw dataframe
write.csv(ghgrp_crop, file.path(Output_directory,'MSW_GHGRP_all.csv'))
write.csv(LMOP_crop, file.path(Output_directory,"MSW_LMOP_all.csv"))

# Now just the names, coordinates and emissions
ghgrp_crop_output <- data.frame(ghgrp_crop$`FACILITY NAME`,coordinates(ghgrp_crop),ghgrp_crop$emiss)
names(ghgrp_crop_output) <- c('Site_Name','Longitude','Latitude','Emission_mol_per_s')
write.csv(ghgrp_crop_output,file.path(Output_directory,'MSW_GHGRP.csv'),row.names=FALSE)

LMOP_crop_output <- data.frame(LMOP_crop$`Landfill Name`,coordinates(LMOP_crop),LMOP_crop$emiss)
names(LMOP_crop_output) <- c('Site_Name','Longitude','Latitude','Emission_mol_per_s')
write.csv(LMOP_crop_output,file.path(Output_directory,'MSW_LMOP.csv'),row.names=FALSE)

# Now write the rasters as netcdf files
writeRaster(ghgrp_flux,
            file.path(Output_directory,'MSW_GHGRP.nc'),
            force_v4=TRUE,
            varname='methane_emissions',
            varunit='nmol/m2/s',
            longname='Methane emissions from municipal solid waste landfills that report to GHGRP',
            NAflag=-9999,
            overwrite=TRUE)
writeRaster(LMOP_flux,
            file.path(Output_directory,'MSW_LMOP.nc'),
            force_v4=TRUE,
            varname='methane_emissions',
            varunit='nmol/m2/s',
            longname='Methane emissions from municipal solid waste landfills that report to LMOP but not GHGRP',
            NAflag=-9999,
            overwrite=TRUE)


################################################################################
#Finally, load up some functions/datasets and plot up this output nicely

source(plotting_function,verbose=F)

log_plot(ghgrp_flux,filename="MSW_GHGRP",
         "Municipal Solid Waste -\n GHGRP reporters")
log_plot(LMOP_flux,filename="MSW_LMOP",
         "Municipal Solid Waste -\n (GHGI - GHGRP) distributed using \nLandfill Methane Outreach Program")

dir.create("Summed_Sectors",showWarnings = F)
setwd("Summed_Sectors")

Summed_landfill <- ghgrp_flux+LMOP_flux
log_plot(Summed_landfill,
         "Landfill Sector\nGHGRP + LMOP for municipal")

