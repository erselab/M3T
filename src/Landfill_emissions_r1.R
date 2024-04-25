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

LMOP_file <- "lmopdata(Aug_22)_landfill_only.xlsx"
year <- "2019"

#states in the domain
state_list=c("NY","NJ","MD","DE","PA")

GHGI_value <- 3943 #Gg CH4/yr
#total national municipal landfill emissions from the GHGI

plotting_function <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Code and method/Scripts/Plotting_individual_sectors.R"
#the location of another script that just creates some functions for consistent,
#quick plotting
################################################################################
#load packages
i <- 1
packagecheck <- c("raster","ncdf4","readxl","httr","dplyr")
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
#Download the relevant data using the API
#(https://www.epa.gov/enviro/envirofacts-data-service-api) and combine the
#facility and emission data appropriately

#initialize output
outfile <- vector()
ghgrp_facility_info <- data.frame()

for(A in 1:length(state_list)){
  #create a temp file for the download, use HTTR to download it to that file,
  #and then read/combine them in an R dataframe
  outfile <- c(outfile,tempfile(fileext = ".xlsx"))
  GET(paste0("https://data.epa.gov/efservice/PUB_DIM_FACILITY/STATE/=/",state_list[A],"/EXCEL"),
      write_disk(outfile[A]))
  ghgrp_facility_info <- rbind(ghgrp_facility_info,read_excel(outfile[A]))
}
#download the relevant landfill-sector data (https://www.epa.gov/enviro/greenhouse-gas-model)
outfile <- c(outfile,tempfile(fileext = ".xlsx"))
invisible(GET("https://data.epa.gov/efservice/HH_SUBPART_LEVEL_INFORMATION/EXCEL",
              write_disk(outfile[A+1])))
ghgrp_landfill_emissions <- read_excel(outfile[A+1])

#force all names to be lowercase to allow matches even if case sometimes differs
ghgrp_facility_info$facility_name <- tolower(ghgrp_facility_info$facility_name)
ghgrp_landfill_emissions$facility_name <- tolower(ghgrp_landfill_emissions$facility_name)

#facility info uses column name year, landfill data uses reporting_year, change
#to be consistent
colnames(ghgrp_landfill_emissions) <- gsub("reporting_","",colnames(ghgrp_landfill_emissions))

#combine the datasets by ID, facility name, and year
ghgrp_all_data <- merge(ghgrp_facility_info,ghgrp_landfill_emissions,
                        by=c("facility_id","facility_name","year"), all=F)

#keep only data for the year of interest
ghgrp <- ghgrp_all_data[ghgrp_all_data$year==year,]

#Calculate national total in the GHGRP for the year of interest
ghgrp_national <- sum(as.numeric(ghgrp_landfill_emissions$ghg_quantity[ghgrp_landfill_emissions$year==year]))/1000   # MT CH4/yr to Gg CH4/yr


#identify facilities that stopped reporting without a valid reason, then subset
#to only landfill facilities
nonreporting_facilities <- unique(ghgrp_facility_info$facility_id[ghgrp_facility_info$reporting_status=="STOPPED_REPORTING_UNKNOWN_REASON" & ghgrp_facility_info$year<=year])
nonreporting_landfills <- nonreporting_facilities[which(nonreporting_facilities %in% unique(ghgrp_landfill_emissions$facility_id))]

#find the latest data available for those that stopped reporting
nonreporting_landfill_data <- ghgrp_all_data[ghgrp_all_data$facility_id %in% nonreporting_landfills,] %>% 
  group_by(facility_id) %>% 
  slice_max(order_by = year) %>%
  as.data.frame()

#add this most recent data to the GHGRP dataset
ghgrp <- rbind(nonreporting_landfill_data,ghgrp)

#convert the relevant columns to numeric class
ghgrp[,c("latitude","longitude","ghg_quantity")] <- apply(ghgrp[,c("latitude","longitude","ghg_quantity")],
                                                          2,FUN = function(x){as.numeric(x)})

#delete all tempfiles and clean up working environment
unlink(outfile)
rm(A,ghgrp_all_data,ghgrp_facility_info,
   nonreporting_landfill_data,nonreporting_facilities,nonreporting_landfills,
   outfile,state_list)
################################################################################
#Now convert to spatial and load/convert LMOP.  Assign GHGI_national -
#GHGRP_national to all LMOP facilities equally.

#convert to a spatial object, crop to d03, convert units
coordinates(ghgrp) <- ~longitude + latitude
proj4string(ghgrp) <- CRS(SRS_string="EPSG:4326")  # WGS84
ghgrp_crop <- crop(ghgrp, d03_rast)
ghgrp_crop$emiss <- ghgrp_crop$ghg_quantity*1e6/(16.043*365*24*60*60)   # MT CH4/yr to mol/s of CH4

# Now calculate national totals
EPA_total <- GHGI_value 
non_ghgrp_total <- EPA_total - ghgrp_national

# Read in LMOP and remove those in GHGRP
LMOP <- read_xlsx(file.path(Input_directory,LMOP_file),sheet="LMOP Database",col_names = T)
LMOP_non_ghgrp <- LMOP[!(LMOP$`GHGRP ID` %in% ghgrp_landfill_emissions$facility_id),] 

#This has some nans in, remove those
LMOP_filt <- subset(LMOP_non_ghgrp,!is.na(Latitude))
coordinates(LMOP_filt) <- ~Longitude + Latitude
proj4string(LMOP_filt) <- CRS(SRS_string="EPSG:4326")  # WGS84
LMOP_crop <- crop(LMOP_filt, d03_rast)

# Find avg emission per non-GHGRP LMOP landfill (including the ones with no coordinates)
avg_non_ghgrp <- non_ghgrp_total/nrow(LMOP_non_ghgrp)
# For comparison, calculate avg ghgrp
avg_ghgrp <- ghgrp_national/nrow(ghgrp)
# Assign the avg emissions to LMOP landfills
LMOP_crop$emiss <- avg_non_ghgrp*1e9/(16.043*365*24*60*60)   #Gg CH4/yr to mol/s of CH4

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
# ghgrp_crop_output <- data.frame(ghgrp_crop$`FACILITY NAME`,coordinates(ghgrp_crop),ghgrp_crop$emiss)
ghgrp_crop_output <- data.frame(ghgrp_crop$facility_name,coordinates(ghgrp_crop),ghgrp_crop$emiss)
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

