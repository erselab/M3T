#'@title Process the National Land Cover Database for `Wastewater` function
#'
#'@description `NLCD_open_and_low_int` writes 1 netcdf file per state within the
#'  domain as well as a csv.  These netcdf files have the 30 m National Land
#'  Cover Database Developed, Open Space and Developed, Low Intensity land cover
#'  types for each state.  The csv provides the total area covered by these two
#'  land cover types for each state separately.  This information is used in
#'  `Wastewater` to map septic emissions.
#'
#'@details This function first crops the 30 m
#'  \href{https://www.sciencebase.gov/catalog/item/655ceb8ad34ee4b6e05cc51a}{National
#'  Land Cover Database} to the states within the domain.  Then the data is
#'  subset to create a SpatRaster that is 1 for Developed, Open Space or
#'  Developed, Low Intensity land cover and 0 for all other land cover types.
#'  This is then split into separate SpatRasters for each state and the total
#'  area of these two land cover types for each state is calculated.  The data
#'  is then reprojected to the domain's projection. Finally these SpatRasters
#'  are saved for use in the \code{\link{Wastewater}} function. The total area
#'  of the two land cover types for each state, including data outside the
#'  domain if relevant, is saved as a csv.
#'
#'@inheritParams Municipal_solid_waste
#'
#'@param Source_wastewater_NLCD Character.  Pulled from
#'  \code{\link{M3T_config}}.
#'@returns Nothing is returned from the function, but the main outputs are 1
#'  netcdf file per state in the domain and a csv.  The netcdf files are titled
#'  as "X_NLCD_suburban.nc" where X is the state abbreviation (e.g., MD, DE).
#'  Each file contains a SpatRaster with the fractional coverage of the
#'  developed open space + developed low intensity for the state on the same
#'  grid as the input domain. The csv is titled "NLCD_state_total_areas.csv" and
#'  provides the total area of each land cover type in each state.
#'@inherit CH4_inventory_build author
#'@seealso [CH4_inventory_build()] Calculates methane inventory using settings
#'provided in config.
#'
#'[M3T_config] Generates the config function with user-editable settings used
#'throughout processing.
#'
#'[Wastewater()] Calculates methane emissions for the wastewater sector.
#'@keywords internal





#@examples
# library(terra)
# grid_bbox=cbind(c(-76.65,-73.65),c(38.97,40.97))
# grid_res=0.01
# grid_crs="epsg:4326"
# grid <- rast(nrows=diff(range(grid_bbox[,2]))/grid_res,
#              ncols=diff(range(grid_bbox[,1]))/grid_res, xmin=min(grid_bbox[,1]),
#              xmax=max(grid_bbox[,1]), ymin=min(grid_bbox[,2]), ymax=max(grid_bbox[,2]),
#              crs=grid_crs)
# grid_vect <- as.polygons(ext(grid),crs=grid_crs)
# NLCD_open_and_low_int <- function(NLCD_file="~/../Desktop/in/NLCD_2019_land_cover_l48_20210604/NLCD_2019_land_cover_l48_20210604.img",
#                                   State_Tigerlines=vect("~/../Desktop/in/State_Tigerlines/tl_2018_us_state.shp"),
#                                   state_name_list=c("DE","MD","NJ","NY","PA"),
#                                   domain=grid_vect,
#                                   domain_template=grid,
#                                   output_directory="~/../Desktop/out/")



# Calculate NLCD fractions for the states in the d03 domain
## Finalized: 2023-02-03

NLCD_open_and_low_int <- function(input_directory,
                                  Source_wastewater_NLCD,
                                  domain,
                                  domain_template,
                                  State_Tigerlines,
                                  state_name_list,
                                  output_directory){
  
  
  starttime <- Sys.time()
  cat("Starting wastewater sector: NLCD_open_and_low_int\n")
  
  Wastewater_partial_output_directory <- file.path(output_directory,"Wastewater","processed_NLCD_data")
  dir.create(Wastewater_partial_output_directory,showWarnings = F,recursive = T)
  ################################################################################
  #load in the landcover - national land cover database (NLCD)
  NLCD_file <- file.path(input_directory,"NLCD")
  dir.create(NLCD_file,showWarnings = F)
  if(dir.exists(Source_wastewater_NLCD)){
    invisible(file.copy(list.files(Source_wastewater_NLCD,full.names=T),
                        NLCD_file,overwrite = T,recursive=T))
  }else{
    invisible(file.copy(list.files(dirname(Source_wastewater_NLCD),full.names=T),
                        NLCD_file,overwrite = T,recursive=T))
  }
  NLCD_file <- list.files(NLCD_file,pattern="*.tif$",full.names=T)
  NLCD <- terra::rast(NLCD_file)
  
  #correct levels from the R interpreted ones (provided in manual)
  NLCD_key <- data.frame("Value"=c(11,12,21:24,31,41:43,52,71,81:82,90,95),
                         "Land_Class"=terra::levels(NLCD)[[1]][,2])
  levels(NLCD) <- NLCD_key
  
  # reproject states to matching CRS
  NLCD_states_trans <- terra::project(State_Tigerlines,terra::crs(NLCD))
  
  
  #crop to the states in the domain so that it's a bit more manageable in size
  #from the get-go.  Add a slight buffer.  Only bother if NLCD is larger than
  #the domain (i.e., domain != CONUS).
  if(1.05*terra::ext(NLCD_states_trans) < terra::ext(NLCD)){
    NLCD <- terra::crop(NLCD,1.01*terra::ext(NLCD_states_trans))
  }
  ################################################################################
  #Make a new raster for only Developed, Open Space, and Developed, Low
  #Intensity (21 and 22 of NLCD)
  
  NLCD_suburban <- NLCD
  NLCD_suburban <- terra::classify(NLCD_suburban,matrix(ncol=3,c(0,20.5,0,
                                                                 20.5,22.5,1,
                                                                 22.5,1000,0),byrow=T))
  
  ################################################################################
  #calculate the total for each state and project/crop/mask to exactly the
  #domain, then save
  
  #initialize output data frame for state totals
  area_df <- data.frame("open_or_low_int_area"=vector())
  
  for(A in 1:length(state_name_list)){
    #separate states
    state_sub_poly <- subset(NLCD_states_trans, NLCD_states_trans$STUSPS==state_name_list[A])
    NLCD_suburban_subset <- terra::crop(NLCD_suburban,state_sub_poly,snap='out')
    NLCD_suburban_subset <- terra::mask(NLCD_suburban_subset,state_sub_poly,touches=F)
    NLCD_suburban_subset[is.na(NLCD_suburban_subset)] <- 0
    
    
    #calculate total septic-relevant area per state.  State total, not just the
    #part of the state in the domain (if it's only partially within the domain).
    #area is identical to, but faster than,
    #terra::global(NLCD_suburban_subset*terra::cellsize(NLCD_suburban_subset,unit="km"),sum,na.rm=T)
    #since the raster is a constant size 30 m grid.
    NLCD_subset_area <- unlist(terra::global(NLCD_suburban_subset,sum,na.rm=T)*0.03*0.03)
    area_df <- rbind(area_df,NLCD_subset_area)
    rownames(area_df)[A] <- state_name_list[A]
    
    cat("Finished processing",state_name_list[A],"landcover at",format(Sys.time(),"%H:%M"),"\n")
  }
  colnames(area_df) <- c("open_or_low_int_area")
  #save state-level totals
  utils::write.csv(area_df,file.path(Wastewater_partial_output_directory,'NLCD_state_total_areas.csv'))
  
  ################################################################################
  #reproject for the domain
  
  #if CONUS or custom with a very large domain - reprojecting domain can be
  #problematic
  if(all(terra::ext(domain)/terra::ext(State_Tigerlines) > 1.1)){
    NLCD_domain <- terra::as.polygons(terra::ext(domain)/terra::ext(State_Tigerlines) * terra::ext(NLCD_states_trans))
  }else{
    NLCD_domain <- terra::project(domain,NLCD_states_trans)
  }
  
  #aggregate slightly first for speed/memory (slight precision error, dramatic
  #speed increase)
  NLCD_suburban <- terra::aggregate(NLCD_suburban,10,"mean",na.rm=T)
  
  #coarsen significantly to 1 km2 to work with more easily from here.
  coarsened_domain <- terra::rast(NLCD_domain,resolution=1000,crs=terra::crs(NLCD_suburban),vals=NA)
  NLCD_suburban_reproj <- terra::project(NLCD_suburban,
                                         coarsened_domain,
                                         method="average")
  
  #save
  terra::writeRaster(NLCD_suburban_reproj,file.path(Wastewater_partial_output_directory,
                                             "NLCD_suburban.tif"),
                     overwrite=T)
  
  
  cat("Finished wastewater sector: NLCD_open_and_low_int at",format(Sys.time(),"%H:%M"),"with a total runtime of",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes\n\n")
}
