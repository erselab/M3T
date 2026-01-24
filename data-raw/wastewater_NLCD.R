## code to prepare `wastewater_NLCD` and 'wastewater_state_septic_area'
## datasets.  This processes the 30 m national land cover database to calculate
## the total area of "Developed, Open Space" and "Developed, Low Intensity" land
## cover in each state and create a 1 km x 1 km CONUS grid of these land covers
## for use in mapping septic emissions. Assumes NLCD data and state tigerlines
## have already been downloaded (separate scripts).  Note this is extremely time
## consuming to run as it's processing high resolution data across multiple
## years and has calculations to run per state.



input_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Manuscript/All inventory data/Prepared inventory data/"

################################################################################
#Where to load in NLCD data

NLCD_directory <- file.path(input_directory,"NLCD_data")
################################################################################
#setup a CONUS domain at 1 km x 1 km resolution

domain_res <- c(1000,1000)
domain_crs <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
domain <- as.data.frame(cbind(c(-3590475,3792475),
                              c(-510497,3990107)))
domain <- terra::rast(nrows=diff(range(domain[,2]))/domain_res[2],
                      ncols=diff(range(domain[,1]))/domain_res[1],
                      xmin=min(domain[,1]), xmax=max(domain[,1]),
                      ymin=min(domain[,2]), ymax=max(domain[,2]),
                      vals=1)
domain <- terra::as.polygons(terra::ext(domain),crs=domain_crs)
domain_template <- terra::rast(domain,resolution=domain_res,crs=domain_crs,vals=NA)

################################################################################
#save partial data given the significant processing time/memory needed

Wastewater_partial_output_directory <- file.path(input_directory,"processed_wastewater_NLCD_data")
dir.create(Wastewater_partial_output_directory,showWarnings = F,recursive = T)

################################################################################
#loop through each year with NLCD data

NLCD_files <- list.files(NLCD_directory,recursive=T,pattern=".tif$",full.names = T)
Tigerlines_years <- terra::vector_layers(file.path(input_directory,"combined_state_tigerlines.gpkg"))

Tigerlines_years <- Tigerlines_years[(Tigerlines_years %in% basename(dirname(NLCD_files)))]
NLCD_files <- NLCD_files[(basename(dirname(NLCD_files)) %in% Tigerlines_years)]

annual_national_area <- vector(length = length(NLCD_files))
names(annual_national_area) <- as.numeric(Tigerlines_years)
annual_state_area <- list()


for(A in 1:length(NLCD_files)){
  ################################################################################
  #load in the appropriate state tigerlines
  
  State_Tigerlines <- terra::vect(file.path(input_directory,"combined_state_tigerlines.gpkg"),layer=Tigerlines_years[A])
  State_Tigerlines <- State_Tigerlines[order(State_Tigerlines$STUSPS),]
  state_name_list <- State_Tigerlines$STUSPS
  
  ################################################################################
  #load in the landcover - national land cover database (NLCD)
  
  NLCD <- terra::rast(NLCD_files[A])
  
  #correct levels from the R interpreted ones (provided in manual)
  NLCD_key <- data.frame("Value"=c(11,12,21:24,31,41:43,52,71,81:82,90,95),
                         "Land_Class"=terra::levels(NLCD)[[1]][,2])
  levels(NLCD) <- NLCD_key
  
  # reproject states to matching CRS
  NLCD_states_trans <- terra::project(State_Tigerlines,terra::crs(NLCD))
  
  ################################################################################
  #Make a new raster for only Developed, Open Space, and Developed, Low
  #Intensity (21 and 22 of NLCD)
  
  cat("Reclassifying NLCD\n")
  NLCD <- terra::classify(NLCD,matrix(ncol=3,c(0,20.5,0,
                                               20.5,22.5,1,
                                               22.5,1000,0),byrow=T))
  
  
  cat("Calculating national total Septic relevant landcover\n")
  # national_area <- terra::global(NLCD*terra::cellSize(NLCD,unit="km",transform=F),sum,na.rm=T)
  national_area <- terra::global(NLCD,sum,na.rm=T)*0.03*0.03
  
  annual_national_area[A] <- national_area
  ################################################################################
  #calculate the total for each state and project/crop/mask to exactly the
  #domain, then save
  
  #initialize output data frame for state totals
  area_df <- data.frame("developed_open_or_low_intensity"=vector())
  
  subset_tmpfile <- tempfile(fileext = ".tif")
  for(B in 1:length(state_name_list)){
    #separate states
    cat("Isolating",state_name_list[B],"\n")
    state_sub_poly <- subset(NLCD_states_trans, NLCD_states_trans$STUSPS==state_name_list[B])
    NLCD_subset <- terra::crop(NLCD,state_sub_poly,snap='out')
    NLCD_subset <- terra::mask(NLCD_subset,state_sub_poly,touches=F)
    NLCD_subset[is.na(NLCD_subset)] <- 0
    
    #calculate total septic-relevant area per state.  State total, not just the
    #part of the state in the domain (if it's only partially within the domain).
    #area is identical to, but faster than,
    #terra::global(NLCD_subset*terra::cellsize(NLCD_subset,unit="km"),sum,na.rm=T)
    #since the raster is a constant size 30 m grid.
    cat("Calculating state total Septic relevant landcover\n")
    NLCD_subset_area <- terra::global(NLCD_subset,sum,na.rm=T)*0.03*0.03
    area_df <- rbind(area_df,NLCD_subset_area)
    rownames(area_df)[B] <- state_name_list[B]
    # 
    # #Add a few pixels worth of buffer (at the domain resolution) filled with
    # #0's.  Average would otherwise ignore these NA values in calculations.
    # cat("Adding buffer\n")
    # NLCD_subset <- terra::extend(NLCD_subset,fill=0,
    #                              terra::ext(NLCD_subset)+
    #                                (terra::res(terra::project(domain_template,terra::crs(NLCD)))*20))
    # 
    # #save/load the raster (help with memory limitations, rather than holding
    # #everything in memory)
    # terra::writeRaster(NLCD_subset,subset_tmpfile)
    # NLCD_subset <- terra::rast(subset_tmpfile)
    # 
    # #project to the exact domain (resolution, origin, extent, etc.) using an
    # #average.  Represents the fractional coverage of wetlands in each pixel (0 -
    # #1).
    # NLCD_reprojected <- terra::project(NLCD_subset,domain_template,method="average")
    # 
    # #save
    # terra::writeRaster(NLCD_reprojected,file.path(Wastewater_partial_output_directory,
    #                                               paste0(state_name_list[B],"_NLCD.tif")),
    #                    overwrite=T)
    unlink(subset_tmpfile)
    gc()
    cat("Finished processing",state_name_list[B],"landcover,",B,"of",length(state_name_list),", at",format(Sys.time(),"%H:%M:%S"),"\n\n")
  }
  colnames(area_df) <- c("open_or_low_int_area")
  #save state-level totals
  annual_state_area[[A]] <- area_df
  
  #reproject nationally
  national_NLCD_reprojected <- terra::project(NLCD,domain_template,method="average")
  terra::writeRaster(national_NLCD_reprojected,file.path(Wastewater_partial_output_directory,
                                                         paste0(Tigerlines_years[A],"_wastewater_NLCD.tif")),
                     overwrite=T)
  
  cat("Finished",A,"of",length(NLCD_files),"years at",format(Sys.time(),"%H:%M:%S"),"\n\n\n\n\n\n")
}

names(annual_state_area) <- as.numeric(Tigerlines_years)

################################################################################
#combine across years into a single multilayer raster

Combined_wastewater_NLCD <- terra::rast(list.files(Wastewater_partial_output_directory,pattern=".tif$",full.names = T))
names(Combined_wastewater_NLCD) <- as.numeric(Tigerlines_years)

################################################################################
#save

wastewater_state_septic_area <- annual_state_area
Total_national_open_or_low_int_area <- annual_national_area

usethis::use_data(wastewater_state_septic_area, overwrite = TRUE)
usethis::use_data(Total_national_open_or_low_int_area, overwrite = TRUE)
terra::writeRaster(Combined_wastewater_NLCD,file.path(input_directory,"combined_wastewater_NLCD.tif"),
                   overwrite=T)

# this approach at 1 km x 1 km compared closest to masking a state at high
# resolution instead. typically agreed to better than 1%.
# test=mask(national_NLCD_reprojected,NLCD_states_trans[NLCD_states_trans$STUSPS=="AZ",],touches=F)*
#   cellSize(mask(national_NLCD_reprojected,NLCD_states_trans[NLCD_states_trans$STUSPS=="AZ",],touches=F),unit="km")
# global(test,sum,na.rm=T)














main_page_URL <- "https://www.sciencebase.gov/catalog/item/5f650b8682ce38aaa23be1bd?format=json"
NLCD_filenames <- jsonlite::fromJSON(main_page_URL)
NLCD_filenames <- NLCD_filenames$files$name

NLCD_filenames <- NLCD_filenames[c(grep("NLCD_2011_Land_Cover_AK",NLCD_filenames),
                                   grep("NLCD_2016_Land_Cover_AK",NLCD_filenames))]

download_location <- tempfile(fileext = ".zip")

AK_area <- vector(length=2)
#loop to download and unzip each 1 in sequence
for(A in 1:length(NLCD_filenames)){
  NLCD_folder <- file.path(NLCD_directory,paste0("AK_",c(2011,2016)[A],"_NLCD_Land_Cover"))
  
  NLCD_URL <- paste0("https://www.mrlc.gov/downloads/sciweb1/shared/mrlc/data-bundles/",NLCD_filenames[A])
  utils::download.file(NLCD_URL,download_location,method = "curl",quiet = T)
  utils::unzip(download_location,exdir=NLCD_folder)
  
  #delete the temp file
  unlink(download_location)
  
  
  AK_NLCD=rast(list.files(pattern=".img",NLCD_folder,full.names=T))
  cat("Reclassifying AK NLCD\n")
  AK_NLCD <- terra::classify(AK_NLCD,matrix(ncol=3,c(0,20.5,0,
                                                     20.5,22.5,1,
                                                     22.5,1000,0),byrow=T))
  cat("Calculating AK total Septic relevant landcover\n")
  AK_area[A] <- terra::global(AK_NLCD,sum,na.rm=T)*0.03*0.03
}







