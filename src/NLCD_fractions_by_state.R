#'@title Process the National Land Cover Database for `Wastewater` function
#'
#'@description `NLCD_open_and_low_int` writes 1 netcdf file per state within the
#'  domain as well as a csv.  These netcdf files have the 30 m National Land
#'  Cover Database Developed, Open Space and Developed, Low Intensity land cover
#'  types for each state.  The csv provides the total area covered by these two
#'  land cover types for each state separately.  This information is used in
#'  `Wastewater` to map septic emissions.
#'
#'@details This function first crops the 30 m National Land Cover Database to
#'  the states within the domain.  Then the data is subset to create a
#'  SpatRaster that is 1 for Developed, Open Space or Developed, Low Intensity
#'  land cover and 0 for all other land cover types.  This is then split into
#'  separate SpatRasters for each state and the total area of these two land
#'  cover types for each state is calculated.  The data is then reprojected
#'  using the mean to the domain's projection. Finally these SpatRasters are
#'  saved for use in the `Wastewater` function. The total area of the two land
#'  cover types for each state, including data outside the domain, is saved as a
#'  csv.
#'
#'@param NLCD_file Character providing the full filepath to the National Land
#'  Cover Database.  This data is available at \url{https://www.mrlc.gov/data}.
#'  This is a geotif of 30 m land cover data for the Continental United States
#'  where different values represent the different land cover classifications.
#'  There is an example file in the package's datasets folder that has been
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
#'@returns Nothing is returned from the function, but the main outputs are 1
#'  netcdf file per state in the domain and a csv.  The netcdf files are titled
#'  as "X_NLCD_suburban.nc" where X is the state abbreviation (e.g., MD, DE).
#'  Each file contains a SpatRaster with the fractional coverage of the
#'  developed open space + developed low intensity for the state on the same
#'  grid as the input domain. The csv is titled "NLCD_state_total_areas.csv" and
#'  provides the total area of each land cover type in each state.
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
                                  domain_template,
                                  XESMF,
                                  State_Tigerlines,
                                  state_name_list,
                                  output_directory){
  
  
  starttime <- Sys.time()
  cat("Starting wastewater sector: NLCD_open_and_low_int\n")
  ################################################################################
  #load in the landcover - national land cover database (NLCD)
  
  NLCD <- rast(NLCD_file)
  # reproject states to proper CRS
  NLCD_states_trans <- project(State_Tigerlines,crs(NLCD))
  #crop to the states in the domain so that it's a bit more
  #manageable in size from the get-go.  Add a slight buffer.
  NLCD <- crop(NLCD,1.01*ext(NLCD_states_trans))
  
  ################################################################################
  #Make a new raster for only Developed, Open Space, and Developed, Low
  #Intensity (21 and 22 of NLCD)
  
  NLCD_suburban <- NLCD
  NLCD_suburban <- classify(NLCD_suburban,matrix(ncol=3,c(0,20.5,0,
                                                          20.5,22.5,1,
                                                          22.5,1000,0),byrow=T))
  ################################################################################
  #calculate the total for each state and project/crop/mask to exactly the
  #domain, then save

  #initialize output data frame for state totals
  area_df <- data.frame("developed_open_or_low_intensity"=vector())

  subset_tmpfile <- tempfile(fileext = ".tif")
  for(A in 1:length(state_name_list)){
    #separate states
    state_sub_poly <- subset(NLCD_states_trans, NLCD_states_trans$STUSPS==state_name_list[A])
    NLCD_suburban_subset <- crop(NLCD_suburban,state_sub_poly,snap='out')
    NLCD_suburban_subset <- mask(NLCD_suburban_subset,state_sub_poly,touches=F)
    NLCD_suburban_subset[is.na(NLCD_suburban_subset)] <- 0
    
    #calculate total septic-relevant area per state.  State total, not just the
    #fraction of the state in the domain
    NLCD_subset_area <- global(NLCD_suburban_subset*cellSize(NLCD_suburban_subset,unit="km",transform=F),sum,na.rm=T)
    area_df <- rbind(area_df,NLCD_subset_area)
    rownames(area_df)[A] <- state_name_list[A]
    
    #Add a few pixels worth of buffer (at the domain resolution) filled with
    #0's.  Average would otherwise ignore these NA values in calculations.
    NLCD_suburban_subset <- extend(NLCD_suburban_subset,fill=0,
                                   ext(NLCD_suburban_subset)+(res(project(domain_template,crs(NLCD_suburban)))*5))
    
    writeRaster(NLCD_suburban_subset,subset_tmpfile)
    NLCD_suburban_subset <- rast(subset_tmpfile)
    
    #project to the exact domain (resolution, origin, extent, etc.) using an
    #average.  Represents the fractional coverage of wetlands in each pixel (0 -
    #1).
    NLCD_suburban_reprojected <- project(NLCD_suburban_subset,domain_template,method="average")
    
    #save
    if(XESMF){
      writeRaster(open_d03,file.path(Output_directory,paste0(state_name_list[A],"_NLCD_open.grd")),overwrite=T)
      writeRaster(low_int_d03,file.path(Output_directory,paste0(state_name_list[A],'_NLCD_low_int.grd')),overwrite=T)
    }else{
      writeCDF(NLCD_suburban_reprojected,file.path(output_directory,
                                              paste0(state_name_list[A],"_NLCD_suburban.nc")),
               force_v4=T,
               overwrite=T)
    }
    unlink(subset_tmpfile)
    cat("Finished processing",state_name_list[A],"landcover at",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes since start\n")
  }
  colnames(area_df) <- c("open_or_low_int_area")
  #save state-level totals
  write.csv(area_df,file.path(output_directory,'NLCD_state_total_areas.csv'))
  cat("Finished wastewater sector: NLCD_open_and_low_int in",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes\n\n")
}
