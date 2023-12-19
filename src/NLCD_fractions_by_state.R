# Calculate NLCD fractions for the states in the d03 domain
## Finalized: 2023-02-03

################################################################################
#Manually defined variables

Input_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw data/"
Output_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed/"

d03_bounding_box <- cbind(c(-76.65,-73.65),c(38.97,40.97))
resolution <- 0.01
#Will be used to generate a blank raster for the output to be built onto, WGS84
#CRS.  Resolution in deg, bounding box in lat/long.

State_subset <- c("DE", "MD", "NJ", 'NY', "PA")
#state acronyms for the domain of interest

nlcd <- file.path("G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Shapefiles",
                  "nlcd_2019_land_cover_l48_20210604/nlcd_2019_land_cover_l48_20210604.img")
#national land cover data at 30 m (https://www.usgs.gov/centers/eros/science/national-land-cover-database)

state_shapefile <- "G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Shapefiles/tl_2022_us_state/tl_2022_us_state.shp"
#Census tigerline shapefiles for states

XESMF_check <- TRUE
#use xesmf to reproject (TRUE), or projectraster (FALSE)
################################################################################
#load packages
i <- 1
packagecheck <- c("raster","sf")
while(i<=length(packagecheck)){
  if(length(find.package(packagecheck[i],quiet = TRUE))<1){
    install.packages(packagecheck[i],repos="https://repo.miserver.it.umich.edu/cran/")
  }
  i <- i+1
}

invisible(suppressPackageStartupMessages(lapply(packagecheck, require, character.only=TRUE)))
rm(packagecheck,i)

#raster = raster and data functionalities
#sf = simple features, adds spatial functionalities
################################################################################
#now quickly build the output raster matrix and load in the raster NLCD data

d03_rast <- raster(nrows=diff(range(d03_bounding_box[,2]))/resolution, 
                   ncols=diff(range(d03_bounding_box[,1]))/resolution,
                   xmn=min(d03_bounding_box[,1]), xmx=max(d03_bounding_box[,1]),
                   ymn=min(d03_bounding_box[,2]), ymx=max(d03_bounding_box[,2]), 
                   crs=4326)

rm(d03_bounding_box,resolution)

nlcd <- brick(nlcd)

################################################################################
#subset NLCD to the appropriate states

# Make a polygon for d03 (with a small buffer just to be safe)
area_x_coords <- c(xmin(d03_rast),xmax(d03_rast),xmax(d03_rast),xmin(d03_rast))+c(-0.5,0.5,0.5,-0.5)
area_y_coords <- c(ymin(d03_rast),ymin(d03_rast),ymax(d03_rast),ymax(d03_rast))+c(-0.5,-0.5,0.5,0.5)
xym <- cbind(area_x_coords,area_y_coords)
p <- Polygon(xym)
ps <- Polygons(list(p),1)
crop_area <- SpatialPolygons(list(ps))
proj4string(crop_area) <- CRS(SRS_string="EPSG:4326")  # WGS84
crop_area_trans <- spTransform(crop_area, crs(nlcd))

# Read state file and reproject
states <- st_read(state_shapefile)
states <- as(states,"Spatial")
states_trans <- spTransform(states,crs(nlcd))

#crop/mask the nlcd to the states in the domain so that it's a bit more
#manageable in size from the get-go
state_poly <- subset(states_trans, STUSPS %in% State_subset)
# First crop to state polygon
nlcd <- crop(nlcd,state_poly,snap='out')
# Now mask
nlcd <- mask(nlcd, state_poly, updatevalue=0)

################################################################################
# Make new rasters for Developed, Open Space, and Developed, Low Intensity
# Do everything one at a time to avoid memory issues
nlcd_open <- nlcd
nlcd_open[] <- 0
nlcd_open[nlcd==21] <- 1
nlcd_low_int <- nlcd
nlcd_low_int[] <- 0
nlcd_low_int[nlcd==22] <- 1
remove(nlcd)

################################################################################
# Finally calculate the sum total for each state and crop to d03

#initialize output data frame for state totals
area_df <- data.frame("developed_open"=vector(),"developed_low_intensity"=vector())

for(A in 1:length(State_subset)){
  state_sub_poly <- subset(state_poly, STUSPS==State_subset[A])
  
  #separate states
  open_d03 <- crop(nlcd_open,state_sub_poly,snap='out')
  open_d03 <- mask(open_d03,state_sub_poly,snap='out')
  low_int_d03 <- crop(nlcd_low_int,state_sub_poly,snap='out')
  low_int_d03 <- mask(low_int_d03,state_sub_poly,snap='out')
  
  #calculate total areas per state per type
  open_area <- cellStats(open_d03,sum)*0.03*0.03  # area in km2 (NLCD is 30 m2).  N pixels * area per pixel.
  low_int_area <- cellStats(low_int_d03,sum)*0.03*0.03
  area_df <- rbind(area_df,cbind(open_area,low_int_area))
  rownames(area_df)[A] <- State_subset[A]
  
  #crop to domain
  open_d03 <- crop(open_d03,crop_area_trans,snap='out')
  low_int_d03 <- crop(low_int_d03,crop_area_trans,snap='out')
  
  #save
  if(XESMF_check){
    writeRaster(open_d03,file.path(Output_directory,paste0(State_subset[A],"_NLCD_open.grd")),overwrite=T)
    writeRaster(low_int_d03,file.path(Output_directory,paste0(State_subset[A],'_NLCD_low_int.grd')),overwrite=T)
  }else{
    #project with projectraster
    template <- d03_rast
    reprojected_open <- aggregate(x = open_d03,na.rm=T,
                                   fact = 37,
                                   fun = mean,expand=T)
    reprojected_low_int <- aggregate(x = low_int_d03,na.rm=T,
                                   fact = 37,
                                   fun = mean,expand=T)
    reprojected_open <- projectRaster(reprojected_open,to=template,method="ngb")
    reprojected_low_int <- projectRaster(reprojected_low_int,to=template,method="ngb")
    #project to a grid with the exact right resolution, extent and origin using
    #nearest neighbor, NOT interpolation, so that we do not change totals.  This
    #is why the data have to be aggregated to an ~0.01 deg grid first.
    writeRaster(reprojected_open,file.path(Output_directory,paste0(State_subset[A],"_NLCD_open_regridded.nc"),force_v4=T),overwrite=T)
    writeRaster(reprojected_low_int,file.path(Output_directory,paste0(State_subset[A],'_NLCD_low_int_regridded.nc'),force_v4=T),overwrite=T)
    rm(template)
  }
  
  cat("Finished",State_subset[A],"\n")
}

#save state-level totals
write.csv(area_df,file.path(Output_directory,'nlcd_state_total_areas.csv'))
