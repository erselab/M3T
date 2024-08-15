#'@title Process the National Land Cover Database for `Wastewater` function
#'
#'@description `NLCD_open_and_low_int` writes 2 netcdf files per state within
#'  the domain as well as a csv.  These netcdf files have the 30 m National Land
#'  Cover Database Developed, Open Space and Developed, Low Intensity land cover
#'  types for each state.  The csv provides the total area covered by these two
#'  land cover types for each state separately.  This information is used in
#'  `Wastewater` to map septic emissions.
#'
#'@details This function first crops the 30 m National Land Cover Database to
#'  the states within the domain.  Then the data is subset to create one
#'  SpatRaster that is 1 for Developed, Open Space and 0 for all other land
#'  cover types, and another equivalent SpatRaster focused on Developed, Low
#'  Intensity land cover instead.  These are then split into separate
#'  SpatRasters for each state and the total area of these two land cover types
#'  for each state is calculated.  The data is then aggregated using the mean to
#'  a similar resolution as the domain and reprojected to the domain's
#'  projection using a nearest neighbor approach. Finally these SpatRasters are
#'  saved for use in the `Wastewater` function. The total area of the two land
#'  cover types for each state is saved as a csv.
#'
#'@param NLCD_file Character providing the full filepath to the National Land
#'  Cover Database.  This data is available at \url{https://www.mrlc.gov/data}. This
#'  is a geotif of 30 m land cover data for the Continental United States where
#'  different values represent the different land cover classifications.  There
#'  is an example file in the package's datasets folder that has been
#'  successfully used in this code available for reference.
#'@param domain SpatRaster providing the desired output grid, including the
#'  desired resolution and coordinate reference system
#'@param state_name_list Character vector listing all states within the desired
#'  domain
#'@param output_directory Character providing the full filepath to save
#'  processed data
#'@param State_Tigerlines SpatVector.  United States Census Bureau county
#'  shapefile.  Available at
#'  \url{https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html}.
#'@returns Nothing is returned from the function, but the main outputs are 2
#'  netcdf files per state in the domain and a csv.  The netcdf files are titled
#'  as "X_NLCD_Y_regridded.nc" where X is the state abbreviation (e.g., MD, DE)
#'  and Y is the land cover type (either open or low_int for low intensity).
#'  Each file contains a SpatRaster with the fractional coverage of the land
#'  cover type for the state on the same grid as the input domain. The csv is
#'  titled "NLCD_state_total_areas.csv" and provides the total area of each land
#'  cover type in each state.
#'@examples
#'library(terra)
#' grid_bbox=cbind(c(-76.65,-73.65),c(38.97,40.97))
#' grid_res=0.01
#' grid_crs="epsg:4326"
#' grid <- rast(nrows=diff(range(grid_bbox[,2]))/grid_res,
#'              ncols=diff(range(grid_bbox[,1]))/grid_res, xmin=min(grid_bbox[,1]),
#'              xmax=max(grid_bbox[,1]), ymin=min(grid_bbox[,2]), ymax=max(grid_bbox[,2]),
#'              crs=grid_crs)
#' NLCD_open_and_low_int <- function(NLCD_file="~/../Desktop/NLCD_2019_land_cover_l48_20210604/NLCD_2019_land_cover_l48_20210604.img",
#'                                   State_Tigerlines=vect("~/../Desktop/State_Tigerlines/tl_2018_us_state.shp"),
#'                                   state_name_list=c("DE","MD","NJ","NY","PA"),
#'                                   output_directory="~/../Desktop/")
#'@author Joe Pitt, \email{madeup@@wisc.edu}
#'@author Kris Hajny, \email{blank@@fake.edu}
#'@author Israel Lopez-Coto, \email{test@@test.edu}
#'@export


# Calculate NLCD fractions for the states in the d03 domain
## Finalized: 2023-02-03

NLCD_open_and_low_int <- function(NLCD_file,
                                  domain,
                                  State_Tigerlines,
                                  state_name_list,
                                  output_directory){
  starttime <- Sys.time()
  cat("Starting wastewater sector: NLCD_open_and_low_int")
  ################################################################################
  #load in the landcover - national land cover database or North
  #American Land Change Monitoring System (NLCD and NALCMS)
  
  # Make a polygon for the domain (with a small buffer just to be safe)
  crop_area <- vect(ext(domain)+0.5)
  crs(crop_area) <- crs(domain)
  
  NLCD <- rast(NLCD_file)
  NLCD_crop_area_trans <- project(crop_area,crs(NLCD))
  # reproject states to proper CRS
  NLCD_states_trans <- project(State_Tigerlines,crs(NLCD))
  #crop to the states in the domain so that it's a bit more
  #manageable in size from the get-go.  Again, add a slight buffer.
  NLCD <- crop(NLCD,1.01*ext(NLCD_states_trans))
  
  ################################################################################
  #Make a new raster for only Developed, Open Space, and Developed, Low Intensity (21
  #and 22 of NLCD)
  
  NLCD_suburban <- NLCD
  NLCD_suburban <- classify(NLCD_suburban,matrix(ncol=3,c(0,20.5,0,
                                                          20.5,22.5,1,
                                                          22.5,1000,0),byrow=T))
  ################################################################################
  # calculate the total for each state
  
  #initialize output data frame for state totals
  area_df <- data.frame("developed_open_or_low_intensity"=vector())
  
  for(A in 1:length(state_name_list)){
    state_sub_poly <- subset(NLCD_states_trans, NLCD_states_trans$STUSPS==state_name_list[A])
    
    #separate states
    NLCD_suburban_subset <- crop(NLCD_suburban,state_sub_poly,snap='out')
    NLCD_suburban_subset <- mask(NLCD_suburban_subset,state_sub_poly)
    
    #calculate total areas per state.  N pixels * area per pixel.
    NLCD_subset_area <- global(NLCD_suburban_subset*cellSize(NLCD_suburban_subset,unit="km"),sum,na.rm=T)
    area_df <- rbind(area_df,NLCD_subset_area)
    rownames(area_df)[A] <- state_name_list[A]
    cat("Finished processing",state_name_list[A],"landcover at",difftime(Sys.time(),starttime,units = "min"),"minutes since start\n")
  }
  
  ################################################################################
  #testing different ways to project/agg/crop/mask
  
  # domain_trans <- project(as.polygons(domain),crs(NLCD_suburban))
  # test <- crop(NLCD_suburban,domain_trans,snap="out")
  # test <- mask(test,domain_trans)
  # test <- crop(test,state_sub_poly,snap='out')
  # test <- mask(test,state_sub_poly)
  # global(test*cellSize(test,unit="km"),sum,na.rm=T)
  # 
  # 
  # 
  # aggregate, reproject(ngb), then mask
  # NLCD_suburban_subset3 <- mask(project(aggregate(NLCD_suburban,na.rm=T,
  #                                                 fact=floor(res(project(domain,crs(NLCD_suburban)))/res(NLCD_suburban)/10)*10,mean),
  #                                       domain,
  #                                       method="near"),
  #                               State_Tigerlines[State_Tigerlines$STUSPS==state_name_list[A]],touch=T)
  # NLCD_extract <- extract(NLCD_suburban_subset3,State_Tigerlines[State_Tigerlines$STUSPS==state_name_list[A]],
  #                         weights=T,exact=T,cells=T)
  # NLCD_suburban_subset3[NLCD_extract[,'cell']] <- NLCD_suburban_subset3[NLCD_extract[,'cell']]*NLCD_extract[,'weight']
  # NLCD_suburban_subset4 <- mask(project(aggregate(NLCD_suburban,na.rm=T,
  #                                                 fact=floor(res(project(domain,crs(NLCD_suburban)))/res(NLCD_suburban)/10)*10,mean),
  #                                       domain,
  #                                       method="near"),
  #                               State_Tigerlines[State_Tigerlines$STUSPS==state_name_list[A]],touch=T)
  # NLCD_extract <- extract(NLCD_suburban_subset4,State_Tigerlines[State_Tigerlines$STUSPS==state_name_list[A]],
  #                         weights=T,exact=T,cells=T)
  # NLCD_suburban_subset4[NLCD_extract[,'cell']] <- NLCD_suburban_subset4[NLCD_extract[,'cell']]*NLCD_extract[,'weight']
  # 
  # 
  # 
  # #alt - try to reproject, then mask, then aggregate
  # NLCD_suburban_reprojected <- project(NLCD_suburban,crs(domain),method="near")
  # NLCD_suburban_reprojected2 <- project(NLCD_suburban,crs(domain))
  # 
  # NLCD_suburban_subset <- mask(NLCD_suburban_reprojected,
  #                              State_Tigerlines[State_Tigerlines$STUSPS==state_name_list[A]],
  #                              touch=T)
  # #project to the exact domain (resolution, origin, extent, etc.)
  # NLCD_suburban_subset <- project(NLCD_suburban_subset,domain)
  # NLCD_suburban_subset2 <- mask(NLCD_suburban_reprojected2,
  #                               State_Tigerlines[State_Tigerlines$STUSPS==state_name_list[A]],
  #                               touch=T)
  # NLCD_suburban_subset2 <- project(NLCD_suburban_subset2,domain)
  # global(NLCD_suburban_subset*cellSize(NLCD_suburban_subset,unit="km"),sum,na.rm=T)
  # global(NLCD_suburban_subset2*cellSize(NLCD_suburban_subset2,unit="km"),sum,na.rm=T)
  # global(NLCD_suburban_subset3*cellSize(NLCD_suburban_subset3,unit="km"),sum,na.rm=T)
  # global(NLCD_suburban_subset4*cellSize(NLCD_suburban_subset4,unit="km"),sum,na.rm=T)
  ################################################################################
  #project/crop/mask to exactly the domain, then save
  
  #first aggregate to a similar resolution (rounded to the nearest whole number)
  NLCD_suburban_reprojected <- aggregate(NLCD_suburban,na.rm=T,
                                         fact=floor(res(project(domain,crs(NLCD_suburban)))/
                                                      res(NLCD_suburban)/10)*10,mean)
  
  #then reproject to the exact desired domain using nearest neighbor to have less
  #of an impact on the state/domain totals
  NLCD_suburban_reprojected <- project(NLCD_suburban_reprojected,
                                        domain)

  for(A in 1:length(state_name_list)){
    #mask to just the 1 state
    NLCD_suburban_subset <- mask(NLCD_suburban_reprojected,
                                 State_Tigerlines[State_Tigerlines$STUSPS==state_name_list[A]],
                                 touch=T)
    #account for any pixels that are only partially within the state
    NLCD_extract <- extract(NLCD_suburban_subset,
                            State_Tigerlines[State_Tigerlines$STUSPS==state_name_list[A]],
                            weights=T,exact=T,cells=T)
    NLCD_suburban_subset[NLCD_extract[,'cell']] <- NLCD_suburban_subset[NLCD_extract[,'cell']]*NLCD_extract[,'weight']

    #save
    if(XESMF){
      writeRaster(open_d03,file.path(Output_directory,paste0(state_name_list[A],"_NLCD_open.grd")),overwrite=T)
      writeRaster(low_int_d03,file.path(Output_directory,paste0(state_name_list[A],'_NLCD_low_int.grd')),overwrite=T)
    }else{
      writeCDF(NLCD_suburban_subset,file.path(output_directory,
                                              paste0(state_name_list[A],"_NLCD_suburban.nc")),
               force_v4=T,
               overwrite=T)
    }
    cat("Finished saving",state_name_list[A],"at",difftime(Sys.time(),starttime,units = "min"),"minutes since start\n")
  }
  colnames(area_df) <- c("open_or_low_int_area")
  #save state-level totals
  write.csv(area_df,file.path(output_directory,'NLCD_state_total_areas.csv'))
  cat("Finished wastewater sector: NLCD_open_and_low_int in",difftime(Sys.time(),starttime,units = "min"),"minutes")
}
