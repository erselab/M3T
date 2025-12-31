## code to prepare `Wetland_Landcover` dataset goes here.  Note this is a
## very time and memory intensive piece of code to run

#Location with data from the national land cover database (NLCD).  This data is
#available at https://www.mrlc.gov/data. This is a geotif of 30 m land
#cover data for the Continental United States where different values represent
#the different land cover classifications. It's download has not been automated
#here.
NLCD_input_directory <- "D:/MMMT STUFF/All inventory data/Automated/NLCD"

#WETcharts data for all relevant years.  This data is available at
#https://www.earthdata.nasa.gov/data/catalog/ornl-cloud-monthlywetland-ch4-wetchartsv2-2346-1.3.3.
#It includes monthly modeled wetland CH4 for multiple biogeochemical models. It
#has not been automated as, at the time of writing, files are being transferred
#from the ORNL DAAC to NASA's Earthdata site and the THREDDS API is not
#available for some files.
wetcharts_input_directory <- "D:/MMMT STUFF/All inventory data/Automated/MonthlyWetland_CH4_WetCHARTsV2_2346/MonthlyWetland_CH4_WetCHARTsV2_2346/data/"

# #US census tigerlines data necessary for processing as much of it is
# #state-specific.  Assumes the format matches that created using the data-raw
# #script in this package
# tigerlines_input_directory <- "D:/MMMT STUFF/All inventory data/Automated/"

# Calculate NLCD fractions for CONUS states
output_directory <- "D:/MMMT STUFF/All inventory data/Automated/"

library(terra)

#save data at a higher resolution
terraOptions(datatype="FLT8S")
# terraOptions(datatype="FLT8S",progress=0)
################################################################################
#prep to load in the landcover - national land cover database (NLCD)

NLCD_files <- list.files(NLCD_input_directory,pattern="*.tif$",full.names = T,
                         recursive = T)
NLCD_years <- basename(dirname(NLCD_files))

# total_suburban <- vector(length = length(NLCD_files))

#loop through files rather than all at once to make it a much more manageable
#memory requirement
################################################################################
#CONUS outline
domain <- as.data.frame(cbind(c(-130,-60),
                              c(20,55)))
domain_template <- terra::rast(nrows=diff(range(domain[,2]))/0.01,
                               ncols=diff(range(domain[,1]))/0.01,
                               xmin=min(domain[,1]), xmax=max(domain[,1]),
                               ymin=min(domain[,2]), ymax=max(domain[,2]),
                               vals=1)
domain <- terra::as.polygons(terra::ext(domain_template),crs=terra::crs(domain_template))
# NLCD_suburban <- rast(NLCD_files[1])
# 
# domain_template <- project(domain_template,crs(NLCD_suburban))
################################################################################
#prep to load in  wetcharts

Wetcharts_file <- list.files(wetcharts_input_directory,
                             pattern="WetCHARTs_v1_3_3_.{4}\\.nc$",full.names = T)
Wetcharts_years <- sapply(strsplit(gsub(".nc","",Wetcharts_file),split = "_"),FUN = "[[",11)

################################################################################
#retain only relevant years and align wetcharts and NLCD

Wetcharts_file <- Wetcharts_file[as.numeric(Wetcharts_years)>2010]
Wetcharts_years <- Wetcharts_years[as.numeric(Wetcharts_years)>2010]

NLCD_files <- NLCD_files[as.numeric(NLCD_years)>2010 & NLCD_years %in% Wetcharts_years]
NLCD_years <- NLCD_years[as.numeric(NLCD_years)>2010 & NLCD_years %in% Wetcharts_years]

################################################################################
#function to speed processing

Annual_Wetcharts_prep <- function(file_num){
  ################################################################################
  #limit Wetcharts to CONUS + a little buffer, crop wetcharts
  
  Wetcharts <- terra::rast(Wetcharts_file[file_num])
  Wetcharts <- terra::crop(Wetcharts,terra::ext(terra::project(domain,terra::crs(Wetcharts)))+0.5)
  ################################################################################
  #load in, reclassify, and project NLCD to wetcharts CRS at 0.1 deg.
  
  #solely for the exact grid to project to
  template <- terra::disagg(Wetcharts[[1]],fact=5)
  terra::values(template) <- 1
  
  NLCD <- rast(NLCD_files[file_num])
  
  #correct levels from the R interpreted ones (as provided in manual)
  NLCD_key <- data.frame("Value"=c(11,12,21:24,31,41:43,52,71,81:82,90,95),
                         "Land_Class"=levels(NLCD)[[1]][,2])
  levels(NLCD) <- NLCD_key
  
  cat("Reclassifying 30 m national dataset - this is a very time consuming step\n")
  
  #force all values between 0 and 89 to 0.  values between 89 and 200 are forced
  #to 1.  90 and 95 = wetland land cover for NLCD.
  NLCD <- terra::classify(NLCD,matrix(c(0,89,0,
                                        89,200,1),
                                      ncol=3,byrow=T))
  
  cat("Removing 0's\n")
  NLCD[is.na(NLCD)] <- 0
  
  # #save this temporarily for speed/memory
  # NLCD_tempfile <- tempfile(fileext = '.tif')
  # writeRaster(NLCD,NLCD_tempfile)
  # NLCD <- rast(NLCD_tempfile)
  
  cat("Reprojecting\n")
  #project to a grid with the exact right resolution, extent and origin.
  NLCD=terra::project(NLCD,template,method="sum")
  
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
  #Subset to the user-set models and average monthly Wetcharts across models.
  
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
    Averaged_wetcharts[[B]] <- mean(Wetcharts[[Wetcharts_models==unique(Wetcharts_models)[B]]])
  }
  terra::names(Averaged_wetcharts) <- unique(Wetcharts_models)
  
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
  
  cat("\rFinished NLCD year",A,"of",length(NLCD_files),"\n")
  return(NLCD_Downscaled_Averaged_wetcharts)
}

################################################################################
#loop through files rather than all at once to make it a much more manageable
#memory requirement

output_wetcharts <- list()
for(A in 1:length(NLCD_files)){
  output_wetcharts[[A]] <- Annual_Wetcharts_prep(A)
}

################################################################################
# reproject output to match domain exactly

#disaggregate to a similar resolution
domain_trans <- terra::project(domain_template,terra::crs(output_wetcharts[[1]]))
domain_res <- terra::res(domain_trans)

output_wetcharts <- lapply(output_wetcharts,FUN=function(x){
  terra::disagg(x,round(terra::res(x)/domain_res,3),
                "near")})

#reproject to exact domain now.  Here using nearest neighbor to prevent
#only 1 row/column of higher res pixels on the border from being
#interpolated.
output_wetcharts <- lapply(output_wetcharts,FUN=function(x){
  terra::mask(terra::project(x,domain_template,method="near"),
              domain)})

cover <- terra::extract(output_wetcharts[[1]][[1]],
                        terra::project(domain,output_wetcharts[[1]]),
                        weights=T,cells=T)
output_wetcharts[cover[,'cell']] <- output_wetcharts[cover[,'cell']]*cover[,'weight']
for(A in 1:length(output_wetcharts)){
  output_wetcharts[[A]][cover[,'cell']] <- output_wetcharts[[A]][cover[,'cell']]*cover[,'weight']
}

################################################################################
#save

output_directory
# usethis::use_data(Wetland_Landcover, overwrite = TRUE,internal = T)
