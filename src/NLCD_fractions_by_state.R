# Calculate NLCD fractions for the states in the d03 domain
## Finalized: 2023-02-03

NLCD_open_and_low_int <- function(){
  
  # ################################################################################
  # #Manually defined variables
  # 
  # Input_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw data/"
  # Output_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed/"
  # 
  # d03_bounding_box <- cbind(c(-76.65,-73.65),c(38.97,40.97))
  # resolution <- 0.01
  # #Will be used to generate a blank raster for the output to be built onto, WGS84
  # #CRS.  Resolution in deg, bounding box in lat/long.
  # 
  # State_subset <- c("DE", "MD", "NJ", 'NY', "PA")
  # #state acronyms for the domain of interest
  # 
  nlcd_file <- file.path("G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Shapefiles",
                         "nlcd_2019_land_cover_l48_20210604/nlcd_2019_land_cover_l48_20210604.img")
  #national land cover data at 30 m (https://www.usgs.gov/centers/eros/science/national-land-cover-database)
  # 
  # state_shapefile <- "G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Shapefiles/tl_2022_us_state/tl_2022_us_state.shp"
  # #Census tigerline shapefiles for states
  # 
  # XESMF_check <- TRUE
  # #use xesmf to reproject (TRUE), or projectraster (FALSE)
  # ################################################################################
  # #load packages
  # i <- 1
  # packagecheck <- c("raster","sf")
  # while(i<=length(packagecheck)){
  #   if(length(find.package(packagecheck[i],quiet = TRUE))<1){
  #     install.packages(packagecheck[i],repos="https://repo.miserver.it.umich.edu/cran/")
  #   }
  #   i <- i+1
  # }
  # 
  # invisible(suppressPackageStartupMessages(lapply(packagecheck, require, character.only=TRUE)))
  # rm(packagecheck,i)
  # 
  # #raster = raster and data functionalities
  # #sf = simple features, adds spatial functionalities
  # ################################################################################
  # #now quickly build the output raster matrix and load in the raster NLCD data
  # 
  # d03_rast <- raster(nrows=diff(range(d03_bounding_box[,2]))/resolution, 
  #                    ncols=diff(range(d03_bounding_box[,1]))/resolution,
  #                    xmn=min(d03_bounding_box[,1]), xmx=max(d03_bounding_box[,1]),
  #                    ymn=min(d03_bounding_box[,2]), ymx=max(d03_bounding_box[,2]), 
  #                    crs=4326)
  # 
  # rm(d03_bounding_box,resolution)
  # 
  # nlcd <- brick(nlcd)
  # 
  ################################################################################
  #subset NLCD to the appropriate states
  
  nlcd <- rast(nlcd_file)
  
  # Make a polygon for the domain (with a small buffer just to be safe)
  crop_area <- vect(ext(domain)+0.5)
  crs(crop_area) <- crs(domain)
  crop_area_trans <- project(crop_area,crs(nlcd))
  
  # reproject states to NLCD crs
  states_trans <- project(State_Tigerlines,crs(nlcd))
  
  #crop/mask the nlcd to the states in the domain so that it's a bit more
  #manageable in size from the get-go.  Again, add a slight buffer.
  nlcd <- terra::crop(nlcd,1.01*ext(states_trans))
  
  ################################################################################
  # Make new rasters for Developed, Open Space, and Developed, Low Intensity
  # Do everything one at a time to avoid memory issues
  nlcd_open <- nlcd
  values(nlcd_open) <- 0
  nlcd_open[nlcd==21] <- 1
  nlcd_low_int <- nlcd
  values(nlcd_low_int) <- 0
  nlcd_low_int[nlcd==22] <- 1
  remove(nlcd)
  ################################################################################
  # calculate the sum total for each state
  
  #initialize output data frame for state totals
  area_df <- data.frame("developed_open"=vector(),
                        "developed_low_intensity"=vector())
  
  for(A in 1:length(state_name_list)){
    state_sub_poly <- terra::subset(states_trans, states_trans$STUSPS==state_name_list[A])
    
    #separate states
    open_d03 <- crop(nlcd_open,state_sub_poly,snap='out')
    open_d03 <- mask(open_d03,state_sub_poly)
    low_int_d03 <- crop(nlcd_low_int,state_sub_poly,snap='out')
    low_int_d03 <- mask(low_int_d03,state_sub_poly)
    
    #calculate total areas per state per type.  N pixels * area per pixel.
    open_area <- global(open_d03*cellSize(open_d03,unit="km"),sum,na.rm=T)
    low_int_area <- global(low_int_d03*cellSize(low_int_d03,unit="km"),sum,na.rm=T)
    area_df <- rbind(area_df,cbind(open_area,low_int_area))
    rownames(area_df)[A] <- state_name_list[A]
    cat("Finished processing",state_name_list[A],"\n")
  }
  ################################################################################
  #project/crop to exactly the domain, then save
  
  #first aggregate to a similar resolution
  reprojected_open <- aggregate(nlcd_open,na.rm=T,
                                fact=floor(res(project(domain,crs(nlcd_open)))/res(nlcd_open)/10)*10,
                                mean)
  reprojected_low_int <- aggregate(nlcd_low_int,na.rm=T,
                                   fact=floor(res(project(domain,crs(nlcd_low_int)))/res(nlcd_low_int)/10)*10,
                                   mean)
  #then reproject to the exact desired domain using nearest neighbor to have less
  #of an impact on the state/domain totals
  reprojected_open <- project(reprojected_open,y=domain,method="near")
  reprojected_low_int <- project(reprojected_low_int,y=domain,method="near")
  
  
  
  for(A in 1:length(state_name_list)){
    #mask to just the 1 state
    
    subset_open <- mask(reprojected_open,State_Tigerlines[State_Tigerlines$STUSPS==state_name_list[A]])
    subset_low_int <- mask(reprojected_low_int,State_Tigerlines[State_Tigerlines$STUSPS==state_name_list[A]])
    #save
    if(XESMF){
      writeRaster(open_d03,file.path(Output_directory,paste0(state_name_list[A],"_NLCD_open.grd")),overwrite=T)
      writeRaster(low_int_d03,file.path(Output_directory,paste0(state_name_list[A],'_NLCD_low_int.grd')),overwrite=T)
    }else{
      writeCDF(subset_open,file.path(output_directory,
                                     paste0(state_name_list[A],"_NLCD_open_regridded.nc")),
               force_v4=T,
               overwrite=T)
      writeCDF(subset_low_int,file.path(output_directory,
                                        paste0(state_name_list[A],"_NLCD_low_int_regridded.nc")),
               force_v4=T,
               overwrite=T)
    }
    cat("Finished saving",state_name_list[A],"\n")
  }
  colnames(area_df) <- c("open_area","low_int_area")
  #save state-level totals
  write.csv(area_df,file.path(output_directory,'nlcd_state_total_areas.csv'))
  
}
