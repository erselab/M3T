## code to prepare `Wastewater_Landcover` dataset goes here.  Note this is a
## very time and memory intensive piece of code to run

#Location with data from the national land cover database (NLCD).  This data is
#available at \url{https://www.mrlc.gov/data}. This is a geotif of 30 m land
#cover data for the Continental United States where different values represent
#the different land cover classifications. It's download has not been automated
#here.
NLCD_input_directory <- "D:/MMMT STUFF/All inventory data/Automated/NLCD"

#US census tigerlines data necessary for processing as much of it is
#state-specific.  Assumes the format matches that created using the data-raw
#script in this package
tigerlines_input_directory <- "D:/MMMT STUFF/All inventory data/Automated/"

# Calculate NLCD fractions for CONUS states
Wastewater_partial_output_directory <- tempdir()
dir.create(Wastewater_partial_output_directory,showWarnings = F)

library(terra)

#save data at a higher resolution, don't show progress bars during processing
#(meaningless without context)
terraOptions(datatype="FLT8S",progress=0)
################################################################################
#prep to load in the landcover - national land cover database (NLCD)

NLCD_files <- list.files(NLCD_input_directory,pattern="*.tif$",full.names = T,
                         recursive = T)
NLCD_years <- basename(dirname(NLCD_files))

total_suburban <- vector(length = length(NLCD_files))

################################################################################
#output grid
domain <- as.data.frame(cbind(c(-130,-60),
                              c(20,55)))
domain_template <- terra::rast(nrows=diff(range(domain[,2]))/0.01,
                               ncols=diff(range(domain[,1]))/0.01,
                               xmin=min(domain[,1]), xmax=max(domain[,1]),
                               ymin=min(domain[,2]), ymax=max(domain[,2]),
                               vals=1)
NLCD_suburban <- rast(NLCD_files[1])

domain_template <- project(domain_template,crs(NLCD_suburban))
################################################################################
#load through and process each

#loop through files rather than all at once to make it a much more manageable
#memory requirement
multiyear_area_df <- list()
for(B in 1:length(NLCD_files)){
  NLCD_suburban <- rast(NLCD_files[B])
  
  #correct levels from the R interpreted ones (as provided in manual)
  NLCD_key <- data.frame("Value"=c(11,12,21:24,31,41:43,52,71,81:82,90,95),
                         "Land_Class"=levels(NLCD_suburban)[[1]][,2])
  levels(NLCD_suburban) <- NLCD_key
  
  ################################################################################
  #Make a new raster for only Developed, Open Space, and Developed, Low
  #Intensity (21 and 22 of NLCD)
  
  cat("\rReclassifying 30 m national dataset - this is a very time consuming step")
  NLCD_suburban <- classify(NLCD_suburban,matrix(ncol=3,c(0,20.5,0,
                                                          20.5,22.5,1,
                                                          22.5,1000,0),byrow=T))
  ################################################################################
  #load in state tigerlines and reproject to match NLCD projection
  State_Tigerlines <- terra::vect(file.path(tigerlines_input_directory,"combined_state_tigerlines.gpkg"),layer=NLCD_years[B])
  # reproject states to matching CRS
  NLCD_states_trans <- project(State_Tigerlines,crs(NLCD_suburban))
  ################################################################################
  #calculate the total for each state, then save
  
  #initialize output data frame for state totals
  area_df <- data.frame("state"=vector(),"developed_open_or_low_intensity"=vector())
  
  subset_tmpfile <- tempfile(fileext = ".tif")
  for(A in 1:length(State_Tigerlines)){
    #separate states
    state_sub_poly <- NLCD_states_trans[A]
    NLCD_suburban_subset <- crop(NLCD_suburban,state_sub_poly,snap='out')
    NLCD_suburban_subset <- mask(NLCD_suburban_subset,state_sub_poly,touches=F)
    NLCD_suburban_subset[is.na(NLCD_suburban_subset)] <- 0
    
    #calculate total septic-relevant area per state.  State total, not just the
    #part of the state in the domain (if it's only partially within the domain)
    NLCD_subset_area <- global(NLCD_suburban_subset*cellSize(NLCD_suburban_subset,unit="km",transform=F),sum,na.rm=T)
    area_df <- rbind(area_df,data.frame("state"=NLCD_states_trans$STUSPS[A],
                                        "developed_open_or_low_intensity"=NLCD_subset_area))

    #Add a few pixels worth of buffer (at 1 km resolution) filled with
    #0's.  Average would otherwise ignore these NA values in calculations.
    NLCD_suburban_subset <- extend(NLCD_suburban_subset,fill=0,
                                   ext(NLCD_suburban_subset)+(res(domain_template)*20))
    
    #save/load the raster (help with memory limitations, rather than holding
    #everything in memory)
    writeRaster(NLCD_suburban_subset,subset_tmpfile,overwrite=T)
    NLCD_suburban_subset <- terra::rast(subset_tmpfile)
    
    #project to the exact domain (resolution, origin, extent, etc.) using an
    #average.  Represents the fractional coverage of wetlands in each pixel (0 -
    #1).
    NLCD_suburban_reprojected <- project(NLCD_suburban_subset,domain_template,
                                         method="average")
    
    #save
    writeRaster(NLCD_suburban_reprojected,file.path(Wastewater_partial_output_directory,
                                                    paste0(NLCD_years[B],NLCD_states_trans$STUSPS[A],"_NLCD_suburban.tif")),
                overwrite=T)
    unlink(subset_tmpfile);invisible(gc())
    cat("\rFinished processing state number",A,"of",length(NLCD_states_trans),"                   ")
  }
  ################################################################################
  colnames(area_df) <- c("open_or_low_int_area")
  #save state-level totals
  multiyear_area_df[[B]]=area_df
  cat("\rFinished NLCD year",B,"of",length(NLCD_files),"\n")
}

################################################################################
#Clean up, combine across years, and finalize

output_rasts <- list.files(Wastewater_partial_output_directory,full.names = T,
                           pattern=paste0(NLCD_years[1],"_NLCD_suburban.tif$"))
wastewater_Landcover_subset <- rast(output_rasts)
wastewater_Landcover <- sum(wastewater_Landcover_subset,na.rm=T)
names(wastewater_Landcover)[1]=NLCD_years[1]

for(A in 2:length(NLCD_files)){
  output_rasts <- list.files(Wastewater_partial_output_directory,full.names = T,
                             pattern=paste0(NLCD_years[A],"_NLCD_suburban.tif$"))
  wastewater_Landcover_subset <- rast(output_rasts)
  wastewater_Landcover <- c(wastewater_Landcover,sum(wastewater_Landcover_subset,na.rm=T))
  names(wastewater_Landcover)[A]=NLCD_years[A]
}

wastewater_total_septic_landcover <- multiyear_area_df

#quickly ensure that the state data is all in the same order, alphabetical
Suburbia_rasterfile <- sort(Suburbia_rasterfile)
wastewater_total_septic_landcover <- lapply(wastewater_total_septic_landcover,function(x){x[order(x$state),]})
################################################################################
#save

usethis::use_data(wastewater_Landcover, overwrite = TRUE)
usethis::use_data(wastewater_total_septic_landcover, overwrite = TRUE)
