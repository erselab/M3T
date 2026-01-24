## code to prepare `NLCD_downscaled_wetcharts'.  This uses the 30 m national
## land cover database to downscale the 0.5 deg Wetcharts model data to 0.1 deg
## for CONUS. Assumes NLCD data and state tigerlines have already been
## downloaded (separate scripts).  Note this is extremely time consuming to run
## as it's processing high resolution data across multiple years.


input_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Manuscript/All inventory data/Prepared inventory data/"

################################################################################
#Where to load in NLCD and wetcharts data

NLCD_directory <- file.path(input_directory,"NLCD_data")
Wetcharts_directory <- file.path(input_directory,"MonthlyWetland_CH4_WetCHARTsV2_2346","MonthlyWetland_CH4_WetCHARTsV2_2346","data")
################################################################################
#setup a CONUS domain at 0.01 deg x 0.01 deg resolution

domain_res <- c(0.01,0.01)
domain_crs <- "+proj=longlat +ellps=WGS84 +no_defs"
domain <- as.data.frame(cbind(c(-130,-60),
                              c(20,55)))
domain <- terra::rast(nrows=diff(range(domain[,2]))/domain_res[2],
                      ncols=diff(range(domain[,1]))/domain_res[1],
                      xmin=min(domain[,1]), xmax=max(domain[,1]),
                      ymin=min(domain[,2]), ymax=max(domain[,2]),
                      vals=1)
domain <- terra::as.polygons(terra::ext(domain),crs=domain_crs)
domain_template <- terra::rast(domain,resolution=domain_res,crs=domain_crs,vals=NA)

################################################################################
#save partial data given the significant processing time/memory needed

Wetlands_partial_output_directory <- file.path(input_directory,"processed_wetlands_NLCD_data")
dir.create(Wetlands_partial_output_directory,showWarnings = F,recursive = T)

################################################################################
#loop through each year with NLCD data

NLCD_files <- list.files(NLCD_directory,recursive=T,pattern=".tif$",full.names = T)
Wetcharts_files <- list.files(Wetcharts_directory,
                              pattern="WetCHARTs_v1_3_3_.{4}\\.nc$",full.names = T)

Wetcharts_years <- sapply(strsplit(gsub(".nc","",basename(Wetcharts_files)),split = "_"),FUN = "[[",5)

NLCD_years <- basename(dirname(NLCD_files))

Tigerlines_years <- terra::vector_layers(file.path(input_directory,"combined_state_tigerlines.gpkg"))


Wetcharts_files <- Wetcharts_files[(Wetcharts_years %in% Tigerlines_years) & 
                                     Wetcharts_years>2010 & 
                                     (Wetcharts_years %in% NLCD_years)]

Tigerlines_years <- Tigerlines_years[(Tigerlines_years %in% Wetcharts_years) & 
                                       (Tigerlines_years %in% NLCD_years)]

NLCD_files <- NLCD_files[(NLCD_years %in% Wetcharts_years) &
                           (NLCD_years %in% Tigerlines_years)]

################################################################################
#function to speed processing

#loop through files rather than all at once to make it a much more manageable
#memory requirement

output_wetcharts <- list()
for(A in 1:length(NLCD_files)){
  ################################################################################
  #limit Wetcharts to CONUS + a little buffer, crop wetcharts
  
  Wetcharts <- terra::rast(Wetcharts_files[A])
  Wetcharts <- terra::crop(Wetcharts,terra::ext(domain)+2)
  ################################################################################
  #load in, reclassify, and project NLCD to wetcharts CRS at 0.1 deg.
  
  #solely for the exact grid to project to
  template <- terra::disagg(Wetcharts[[1]],fact=5)
  terra::values(template) <- 1
  
  NLCD <- terra::rast(NLCD_files[A])
  
  #correct levels from the R interpreted ones (as provided in manual)
  NLCD_key <- data.frame("Value"=c(11,12,21:24,31,41:43,52,71,81:82,90,95),
                         "Land_Class"=terra::levels(NLCD)[[1]][,2])
  levels(NLCD) <- NLCD_key
  
  #force all values between 0 and 89 to 0.  values between 89 and 200 are forced
  #to 1.  90 and 95 = wetland land cover for NLCD.
  cat("Reclassifying NLCD\n")
  NLCD <- terra::classify(NLCD,matrix(c(0,89,0,
                                        89,200,1),
                                      ncol=3,byrow=T))
  
  #Aggregate slightly first for memory/speed
  cat("Aggregating and then reprojecting NLCD\n")
  NLCD <- terra::aggregate(NLCD,
                           na.rm=T,
                           fact=10,
                           fun=sum)
  
  #project to a grid with the exact right resolution, extent and origin.
  NLCD <- terra::project(NLCD,template,method="sum")
  
  ################################################################################
  #now calculate the wetland fraction
  
  #this process was taken in part from
  #https://gis.stackexchange.com/questions/262015/calculation-of-fractional-cover-for-each-vegetation-class-at-30-m-resolution-mat/262958#262958
  
  #aggregate to 0.5 degrees.  Each 0.5 deg pixel = sum of 30 m wetland
  #pixels that are wetlands (i.e., the fraction of the land in the pixel that is
  #wetlands).
  NLCD_0.5_deg <- terra::aggregate(NLCD,
                                   na.rm=T,
                                   fact=5,
                                   fun=sum)
  
  #convert the 0.5 deg version to the same resolution as domain.  This is just
  #so pixels align and does NOT change the values in each pixel.
  NLCD_0.5_deg <- terra::disagg(NLCD_0.5_deg,fact=5)
  
  #now get the ratio of wetlands in each 0.1 deg pixel relative to the 0.5 deg
  #pixels.  Note doing this and then projecting, rather than projecting first,
  #will NOT conserve mass as the ratios within the 0.5 deg pixel will no longer
  #sum exactly to 1.
  NLCD_wetland_fraction <- NLCD/NLCD_0.5_deg
  
  #for any without a value, just distribute equally to the 25 pixels in each 0.5
  #deg pixel (0/# or no data from landcover)
  NLCD_wetland_fraction[is.na(NLCD_wetland_fraction)] <- 1/25
  
  ################################################################################
  #Average monthly Wetcharts separately for each model.
  
  #pull the model numbers from the names of wetcharts
  Wetcharts_models <- sapply(strsplit(names(Wetcharts),"_"),"[[",4)
  Wetcharts_models <- as.numeric(substring(Wetcharts_models,7,20))
  
  #pull the months from the names of wetcharts this time
  Wetcharts_months <- sapply(Wetcharts,FUN = function(x){
    as.numeric(sapply(strsplit(names(x),"_"),"[[",5))})
  
  #Now annualize separately for each model
  # #initialize output, 1 per model subset with 12 blank layers each
  # Averaged_wetcharts <- Wetcharts
  # for(B in 1:length(Wetcharts)){
  #   nlyr(Averaged_wetcharts[[B]]) <- 12
  # }
  # 
  # #average across models for each month separately, then repeat for each model
  # #subset
  # for(B in 1:length(Wetcharts)){
  #   for(A in 1:12){
  #     Averaged_wetcharts[[B]][[A]] <- app(Wetcharts[[B]][[Wetcharts_months[[B]]==A]],
  #                                         mean,na.rm=T)
  #   }
  #   names(Averaged_wetcharts[[B]]) <- month.name
  # }
  
  #Annualize each model
  Averaged_wetcharts <- Wetcharts[[1]]
  terra::nlyr(Averaged_wetcharts) <- length(unique(Wetcharts_models))
  for(B in 1:length(unique(Wetcharts_models))){
    Averaged_wetcharts[[B]] <- terra::mean(Wetcharts[[Wetcharts_models==unique(Wetcharts_models)[B]]])
  }
  names(Averaged_wetcharts) <- unique(Wetcharts_models)
  
  #Convert from mg/m2day to nmol/m2s
  Averaged_wetcharts <- Averaged_wetcharts*1e9/(1000*16.043*24*3600)
  
  #convert any NA's to 0's.  Wetcharts sets any pixels including ocean to NA.
  #We use NA to mean outside domain, so 0 makes more sense for these.
  Averaged_wetcharts[is.na(Averaged_wetcharts)] <- 0
  
  ################################################################################
  #disaggregate Wetcharts using wetland fractions from the landcover
  
  #important - wetcharts is in flux units (which is per area).  Multiply by the
  #area before downscaling, then divide by the new smaller area after
  #downscaling. This conserves the total EMISSIONS, not the total FLUX.
  
  #Disaggregate wetcharts to 0.1 deg and convert from nmol/m2s to nmol/s
  Downscaled_Averaged_wetcharts <- terra::disagg(Averaged_wetcharts*terra::cellSize(Averaged_wetcharts),fact=5)
  
  #redistribute using wetland fraction, crop to domain, and reconvert to
  #nmol/m2s for the smaller pixels
  NLCD_Downscaled_Averaged_wetcharts <- terra::crop(Downscaled_Averaged_wetcharts*NLCD_wetland_fraction/terra::cellSize(Downscaled_Averaged_wetcharts),terra::project(domain,terra::crs(NLCD_wetland_fraction)),snap="out")
  
  terra::writeRaster(NLCD_Downscaled_Averaged_wetcharts,file.path(Wetlands_partial_output_directory,
                                                                  paste0(Tigerlines_years[A],"_NLCD_downscaled_wetcharts.tif")),
                     overwrite=T)
  cat("Finished NLCD year",A,"of",length(NLCD_files),"\n\n\n\n")
}

################################################################################
#Combine and save

NLCD_Downscaled_wetcharts <- terra::rast(list.files(Wetlands_partial_output_directory,pattern=".tif$",full.names=T))
names(NLCD_Downscaled_wetcharts) <- paste0(rep(Tigerlines_years,each=length(unique(Wetcharts_models))),
                                           "_model_",
                                           names(NLCD_Downscaled_wetcharts))
terra::writeRaster(NLCD_Downscaled_wetcharts,file.path(input_directory,"combined_NLCD_downscaled_wetcharts.tif"),
                   overwrite=T)

