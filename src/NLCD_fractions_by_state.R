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
#'@param nlcd_file Character providing the full filepath to the National Land
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
#'  titled "nlcd_state_total_areas.csv" and provides the total area of each land
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
#' NLCD_open_and_low_int <- function(nlcd_file="~/../Desktop/nlcd_2019_land_cover_l48_20210604/nlcd_2019_land_cover_l48_20210604.img",
#'                                   State_Tigerlines=vect("~/../Desktop/State_Tigerlines/tl_2018_us_state.shp"),
#'                                   state_name_list=c("DE","MD","NJ","NY","PA"),
#'                                   output_directory="~/../Desktop/")
#'@author Joe Pitt, \email{madeup@@wisc.edu}
#'@author Kris Hajny, \email{blank@@fake.edu}
#'@author Israel Lopez-Coto, \email{test@@test.edu}
#'@export


# Calculate NLCD fractions for the states in the d03 domain
## Finalized: 2023-02-03

NLCD_open_and_low_int <- function(nlcd_file,
                                  domain,
                                  State_Tigerlines,
                                  state_name_list,
                                  output_directory){
  XESMF=F
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
  
  # plot(septic_emiss2,ylim=c(39.5,39.8),xlim=c(-75.85,-75.7))
  # abline(v=-75.77);abline(v=-75.78);abline(v=-75.79)
  ## DE and MD overlap here, so they can be compared more clearly.
}
