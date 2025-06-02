#'@title Create model-averaged downscaled Wetcharts wetland methane maps
#'
#'@description `Disaggregate_Wetcharts` writes 1 monthly netcdf file of gridded
#'  wetland methane emissions per model subset set in the config and per land
#'  classification dataset used for the downscaling.  Includes optional visuals
#'  as well.
#'
#'@details This function takes the Wetcharts model ensemble and land cover data
#'  to downscale the wetland emissions from 0.5 deg x 0.5 deg to 0.1 deg x 0.1
#'  deg.  The data is also cropped to the domain, converted to the proper units,
#'  and averaged across wetcharts models using the user-defined subset(s).  The
#'  National Land Cover Database (NLCD) and/or the North American Land Change
#'  Monitoring System (NALCMS) can be used for the land cover data.
#'
#'  The 30 m x 30 m land classification data is aggregated to 0.1 deg x 0.1 deg
#'  and used to calculate the fraction of each 0.5 deg x 0.5 deg pixel's total
#'  wetland area that is within each 0.1 deg x 0.1 deg pixel.  Any 0.5 deg x 0.5
#'  deg pixel without land cover data is distributed equally to the 0.1 deg x
#'  0.1 deg pixels.
#'
#'  Wetcharts data is then subset to the user-defined subset(s) models and
#'  averaged.  This is multiplied by the wetland fraction calculated using the
#'  land cover data to disaggregate fluxes to 0.1 deg x 0.1 deg and saved.  It
#'  is first converted from flux to emissions, then downscaled, then converted
#'  back to fluxes as fluxes are per area and this must be considered when
#'  changing the area of pixels. There is a separate file for each model subset
#'  and each land cover dataset used.
#'
#'  The appropriate year of Wetcharts v1.3.1 will be automatically downloaded.
#'
#'  Wetcharts is available at \url{https://doi.org/10.3334/ORNLDAAC/2346}, the
#'  NLCD is available at
#'  \url{https://www.mrlc.gov/data?f%5B0%5D=category%3ALand%20Cover&f%5B1%5D=region%3Aconus},
#'  and the NALCMS is available at
#'  \url{http://www.cec.org/north-american-land-change-monitoring-system/}.
#'@param domain SpatVector polygon outlining the desired output area
#'@param domain_template SpatRaster providing the desired output grid, including
#'  the desired resolution and coordinate reference system
#'@param output_directory Character providing the full filepath to save
#'  processed data
#'@param input_directory Character providing the full filepath to save/load
#'  input data
#'@param inventory_year Numeric indicating the desired year of data to use.
#'@param verbose Logical indicating whether to save additional output.  This
#'  includes monthly plots of the gridded methane emissions on log scales, saved
#'  separately for each model subset and each land cover dataset used.
#'@param Use_NLCD Logical. Pulled from config file. Indicating whether to use
#'  the National Land Cover Database (NLCD) to downscale Wetcharts.
#'@param Use_NALCMS Logical. Pulled from config file. Indicating whether to use
#'  the North America Land Change Monitoring System (NALCMS) to downscale
#'  Wetcharts.
#'@param NLCD_file Character.  The NLCD land cover data as an img. Available at
#'  \url{https://www.mrlc.gov/data?f%5B0%5D=category%3ALand%20Cover&f%5B1%5D=region%3Aconus}.
#'@param NALCMS_file Character.  The NALCMS land cover data as a tif.  Available
#'  at \url{http://www.cec.org/north-american-land-change-monitoring-system/}.
#'@param Wetcharts_model_subset Numeric list. Pulled from config file. Indicates
#'  which models of Wetcharts to average across.  Multiple list entries are
#'  allowed, providing multiple variations of wetcharts to be run
#'  simultaneously.  This is far faster than running them separately as
#'  landcover processing is more time consuming and avoided if running multiple
#'  wetcharts subsets simultaneously.
#'@param Wetcharts_file Character providing the full filepath to the Wetcharts
#'  model file
#'@param inventory_year Character indicating the desired year of data to use.
#'@param plot_directory Character providing the full filepath to save figures.
#'  Only relevant if verbose = TRUE.
#'@param County_Tigerlines SpatVector.  United States Census Bureau county
#'  shapefile.  Available at
#'  \url{https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html}.
#'  Only relevant if verbose=TRUE.
#'@param State_Tigerlines SpatVector.  United States Census Bureau county
#'  shapefile.  Available at
#'  \url{https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html}.
#'  Only relevant if verbose=TRUE.
#'@returns Nothing is returned from the function, but the main outputs are
#'  netcdf files of the methane emissions from wetlands with 1 file per land
#'  cover used and per model subset.  They are titled
#'  "Wetcharts_landcover_Downscaled_subset_A.nc" where landcover is NLCD or
#'  NALCMS and A is just a numeric to identify which model subset, in case
#'  multiple were set.
#'
#'  If verbose is set to TRUE, then multiple figures are also saved.  Log scale
#'  plots with consistent axes are saved for each model subset and land cover
#'  used.  They are named "Wetcharts_landcover_subset_A_annual.png" where
#'  landcover is NLCD or NALCMS, A is a numeric to identify the model subset.
#'@examples
#'library(terra)
#' grid_bbox=cbind(c(-76.65,-73.65),c(38.97,40.97))
#' grid_res=0.01
#' grid_crs="epsg:4326"
#' grid <- rast(nrows=diff(range(grid_bbox[,2]))/grid_res,
#'              ncols=diff(range(grid_bbox[,1]))/grid_res, xmin=min(grid_bbox[,1]),
#'              xmax=max(grid_bbox[,1]), ymin=min(grid_bbox[,2]), ymax=max(grid_bbox[,2]),
#'              crs=grid_crs)
#' grid_vect <- as.polygons(ext(grid),crs=grid_crs)
#'
#' Disaggregate_Wetcharts(input_directory="~/../Desktop/in/",
#'                        output_directory="~/../Desktop/out/",
#'                        domain=grid_vect,
#'                        domain_template=grid,
#'                        verbose=TRUE,
#'                        inventory_year=2018,
#'                        plot_directory="~/../Desktop/plots/",
#'                        State_Tigerlines=vect("~/../Desktop/in/State_Tigerlines/tl_2018_us_state.shp"),
#'                        County_Tigerlines=vect("~/../Desktop/in/County_Tigerlines/tl_2018_us_county.shp"),
#'                        Use_NLCD=TRUE,
#'                        Use_NALCMS=TRUE,
#'                        NLCD_file=file.path("~/../Desktop/in/nlcd_2019_land_cover_l48_20210604/nlcd_2019_land_cover_l48_20210604.img"),
#'                        NALCMS_file=file.path("~/../Desktop/in/NALCMS_2020_land_cover/NA_NALCMS_landcover_2020_30m/data/NA_NALCMS_landcover_2020_30m.tif"),
#'                        Wetcharts_model_subset=list(c(1913,1914,1923,1924,1933,1934,2913,2914,2923,
#'                                                      2924,2933,2934,3913,3914,3923,3924,3933,3934)))
#'
#'
#'@author Joe Pitt, \email{madeup@@wisc.edu}
#'@author Kris Hajny, \email{blank@@fake.edu}
#'@author Israel Lopez-Coto, \email{test@@test.edu}
#'@export



#code to disaggregate Wetcharts by a factor of ~5 using NALCMS or NLCD data.
#Assumes that NALCMS value 14 = wetlands and NLCD values > 89 = wetlands.

Disaggregate_Wetcharts <- function(input_directory,
                                   output_directory,
                                   domain,
                                   domain_template,
                                   verbose,
                                   plot_directory,
                                   inventory_year,
                                   County_Tigerlines,
                                   State_Tigerlines,
                                   Use_NLCD,
                                   Use_NALCMS,
                                   NLCD_file,
                                   NALCMS_file,
                                   Wetcharts_file,
                                   Wetcharts_model_subset){
  
  starttime <- Sys.time()
  cat("Starting wetland sector: Disaggregate_Wetcharts\n")
  
  Wetland_output_directory <- paste0(output_directory,"Wetlands/")
  dir.create(Wetland_output_directory,showWarnings = F)
  ################################################################################
  #download and load in  wetcharts
  
  # #first use the catalog and webscrape then use grep to identify the years of
  # #available files
  # Wetcharts_catalog_url <- "https://thredds.daac.ornl.gov/thredds/catalog/ornldaac/1915/catalog.html"
  # Wetcharts_page <- readLines(Wetcharts_catalog_url)
  # Wetcharts_page <- Wetcharts_page[grep(glob2rx("*WetCHARTs_v1_3_1_*.nc*"),Wetcharts_page)]
  # Wetcharts_years <- regexpr("WetCHARTs_v1_3_1_.*.nc",Wetcharts_page)
  # Wetcharts_years <- as.numeric(substring(Wetcharts_page,Wetcharts_years+17,Wetcharts_years+17+3))
  # 
  # #actually use whichever is closest to the inventory_year, update the user if
  # #this isn't actually the inventory_year
  # Wetcharts_year <- Wetcharts_years[which.min(abs(Wetcharts_years-inventory_year))]
  # if(inventory_year!=Wetcharts_year){
  #   cat("Wetcharts does not include",inventory_year,"using",Wetcharts_year,"as the nearest data available\n")
  # }
  # 
  # Wetcharts_file <- paste0(input_directory,"WetCHARTs_v1_3_1_",Wetcharts_year,".nc")
  # 
  # if(!file.exists(Wetcharts_file)){
  #   cat("Downloading Wetcharts data, this may take a few minutes\n")
  # 
  #   #download the data.  URL is slightly different than the catalog.  See
  #   #https://thredds.daac.ornl.gov/thredds/catalog/ornldaac/1915/catalog.html?dataset=1915/WetCHARTs_v1_3_1_2001.nc
  #   #for details on using thredds for this dataset and
  #   #https://docs.unidata.ucar.edu/tds/current/userguide/index.html for THREDDS in
  #   #general.
  #   data_URL <- paste0("https://thredds.daac.ornl.gov/thredds/fileServer/ornldaac/1915/WetCHARTs_v1_3_1_",Wetcharts_year,".nc")
  #   Trycatch_downloader(data_URL,output_location=paste0(input_directory,"WetCHARTs_v1_3_1_",Wetcharts_year,".nc"),
  #                       method="save",
  #                       error_message=paste0("\nFailed to download Wetcharts data from the DAAC THREDDS database at URL",data_URL))
  # }
  
  Wetcharts <- rast(Wetcharts_file)
  ################################################################################
  #define the domain of interest + a little buffer, crop wetcharts
  
  Wetcharts <- crop(Wetcharts,ext(project(domain,crs(Wetcharts)))+0.5)
  ################################################################################
  #load in the landcover - national land cover database or North American Land
  #Change Monitoring System (NLCD and NALCMS).  Crop both to just the extent of
  #Wetcharts
  
  if(Use_NLCD){
    NLCD <- rast(NLCD_file)
    NLCD <- crop(NLCD,
                 project(x=ext(Wetcharts),from=crs(Wetcharts),to=crs(NLCD)))
  }
  if(Use_NALCMS){
    NALCMS <- rast(NALCMS_file)
    NALCMS <- crop(NALCMS,
                   project(x=ext(Wetcharts),from=crs(Wetcharts),to=crs(NALCMS)))
  }
  cat("Finished loading in all data at",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes since start. Next step is time-consuming.\n")
  ################################################################################
  #set wetlands to a value of 1 and all other land cover to 0, then project to
  #domain CRS at 0.1 deg.
  
  #solely for the exact grid to project to
  template <- disagg(Wetcharts[[1]],fact=5)
  values(template) <- 1
  template_poly <- as.polygons(template)
  
  if(Use_NLCD){
    #force all values between 0 and 89 to 0.  values between 89 and 200 are forced
    #to 1.  90 and 95 = wetland land cover for NLCD.
    NLCD <- classify(NLCD,matrix(c(0,89,0,
                                   89,200,1),
                                 ncol=3,byrow=T))
    
    #project to a grid with the exact right resolution, extent and origin. First
    #crop/mask to the proper extent, set NA's to 0, then project to the exact
    #domain grid.  No need to extend given this is far higher resolution.
    template_poly <- project(template_poly,crs(NLCD))
    NLCD=crop(NLCD,template_poly,snap="out")
    NLCD=mask(x=NLCD,mask=template_poly,touches=F)
    NLCD[is.na(NLCD)] <- 0
    NLCD=project(NLCD,template,method="sum")
  }
  if(Use_NALCMS){
    #force all values between 0 and 13 or 14.6 to 5000 to 0.  values between 13.5
    #and 14.5 are 1.  14 = wetland land cover for NALCMS.
    NALCMS <- classify(NALCMS,matrix(c(0,13,0,
                                       13.5,14.5,1,
                                       14.6,5000,0),
                                     ncol=3,byrow=T))
    
    
    template_poly <- project(template_poly,crs(NALCMS))
    NALCMS=crop(NALCMS,template_poly,snap="out")
    NALCMS=mask(x=NALCMS,mask=template_poly,touches=F)
    NALCMS[is.na(NALCMS)] <- 0
    NALCMS=project(NALCMS,template,method="sum")
  }
  cat("Finished reprojecting land cover data at",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes since start\n")
  ################################################################################
  #now calculate the wetland fraction
  
  #this process was taken in part from
  #https://gis.stackexchange.com/questions/262015/calculation-of-fractional-cover-for-each-vegetation-class-at-30-m-resolution-mat/262958#262958
  
  if(Use_NLCD){
    #aggregate to 0.5 degrees.  Each 0.5 deg pixel = sum of 30 m wetland
    #pixels that are wetlands (i.e., the fraction of the land in the pixel that is
    #wetlands).
    NLCD_0.5_deg <- aggregate(NLCD,
                              na.rm=T,
                              fact=5,
                              fun=sum)
    
    #convert the 0.5 deg version to the same resolution as domain.  This is just
    #so pixels align and does NOT change the values in each pixel.
    NLCD_0.5_deg <- disagg(NLCD_0.5_deg,fact=5)
    
    #now get the ratio of wetlands in each 0.1 deg pixel relative to the 0.5 deg
    #pixels.  Note doing this and then projecting, rather than projecting first,
    #will NOT conserve mass as the ratios within the 0.5 deg pixel will no longer
    #sum exactly to 1.
    NLCD_wetland_fraction <- NLCD/NLCD_0.5_deg
    
    #for any without a value, just distribute equally to the 25 pixels in each 0.5
    #deg pixel (0/# or no data from landcover)
    NLCD_wetland_fraction[is.na(NLCD_wetland_fraction)] <- 1/25
  }
  if(Use_NALCMS){
    NALCMS_0.5_deg <- aggregate(NALCMS,
                                na.rm=T,
                                fact=5,
                                fun=sum)
    NALCMS_0.5_deg <- disagg(NALCMS_0.5_deg,fact=5)
    NALCMS_wetland_fraction <- NALCMS/NALCMS_0.5_deg
    NALCMS_wetland_fraction[is.na(NALCMS_wetland_fraction)] <- 1/25
  }
  
  ################################################################################
  #Subset to the user-set models and average monthly Wetcharts across models.
  
  #pull the model numbers from the names of wetcharts
  Wetcharts_models <- sapply(strsplit(names(Wetcharts),"_"),"[[",4)
  Wetcharts_models <- as.numeric(substring(Wetcharts_models,7,20))
  
  Wetcharts_model_indx <- lapply(Wetcharts_model_subset,
                                 FUN = function(x){which(Wetcharts_models %in% x)})
  
  #Now create a list of the various wetcharts subsets
  Wetcharts <- lapply(Wetcharts_model_indx,FUN=function(x){subset(Wetcharts,x)})
  
  #pull the months from the names of wetcharts this time
  Wetcharts_months <- lapply(Wetcharts,FUN = function(x){
    as.numeric(sapply(strsplit(names(x),"_"),"[[",5))})
  
  #to run annually
  Averaged_wetcharts <- lapply(Wetcharts,mean)
  
  #to run monthly
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
  
  #Convert from mg/m2day to nmol/m2s
  Averaged_wetcharts <- lapply(Averaged_wetcharts,FUN=function(x){
    x*1e9/(1000*16.043*24*3600)})
  
  #convert any NA's to 0's.  Wetcharts sets any pixels including ocean to NA.
  #We use NA to mean outside domain, so 0 makes more sense for these.
  for(B in 1:length(Wetcharts_model_subset)){
    Averaged_wetcharts[[B]][is.na(Averaged_wetcharts[[B]])] <- 0
  }
  
  ################################################################################
  #disaggregate Wetcharts using wetland fractions from the landcover
  
  #important - wetcharts is in flux units (which is per area).  Multiply by the
  #area before downscaling, then divide by the new smaller area after
  #downscaling. This conserves the total EMISSIONS, not the total FLUX.
  
  #Disaggregate wetcharts to 0.1 deg and convert from nmol/m2s to nmol/s
  Downscaled_Averaged_wetcharts <- lapply(Averaged_wetcharts,
                                          FUN=function(x){disagg(x*cellSize(x),fact=5)})
  
  #redistribute using wetland fraction, crop to domain, and reconvert to
  #nmol/m2s for the smaller pixels
  if(Use_NLCD){
    NLCD_Downscaled_Averaged_wetcharts <- lapply(Downscaled_Averaged_wetcharts,FUN=function(x){
      crop(x*NLCD_wetland_fraction/cellSize(x),project(domain,crs(NLCD_wetland_fraction)),snap="out")})
    
    #for comparison later
    NLCD_pixel_check <- NLCD_Downscaled_Averaged_wetcharts[[1]]
  }
  if(Use_NALCMS){
    NALCMS_Downscaled_Averaged_wetcharts <- lapply(Downscaled_Averaged_wetcharts,FUN=function(x){
      crop(x*NALCMS_wetland_fraction/cellSize(x),project(domain,crs(NALCMS_wetland_fraction)),snap="out")})
    NALCMS_pixel_check <- NALCMS_Downscaled_Averaged_wetcharts[[1]]
  }
  
  ################################################################################
  # reproject output to match domain exactly
  
  #aggregate/disaggregate to a similar resolution
  domain_trans <- project(domain_template,crs(Downscaled_Averaged_wetcharts[[1]]))
  domain_res <- res(domain_trans)
  if(any(domain_res<res(Downscaled_Averaged_wetcharts[[1]]))){
    if(Use_NLCD){
      NLCD_Downscaled_Averaged_wetcharts <- lapply(NLCD_Downscaled_Averaged_wetcharts,FUN=function(x){disagg(x,round(res(x)/domain_res,3),"near")})
      
      #reproject to exact domain now.  Here using nearest neighbor to prevent
      #only 1 row/column of higher res pixels on the border from being
      #interpolated.
      NLCD_Downscaled_Averaged_wetcharts <- lapply(NLCD_Downscaled_Averaged_wetcharts,FUN=function(x){mask(project(x,domain_template,method="near"),domain)})
      
      cover <- extract(NLCD_Downscaled_Averaged_wetcharts[[1]][[1]],
                       project(domain,NLCD_Downscaled_Averaged_wetcharts[[1]]),
                       weights=T,cells=T)
      for(A in 1:length(NLCD_Downscaled_Averaged_wetcharts)){
        NLCD_Downscaled_Averaged_wetcharts[[A]][cover[,'cell']] <- NLCD_Downscaled_Averaged_wetcharts[[A]][cover[,'cell']]*cover[,'weight']
      }
    }
    if(Use_NALCMS){
      NALCMS_Downscaled_Averaged_wetcharts <- lapply(NALCMS_Downscaled_Averaged_wetcharts,FUN=function(x){disagg(x,round(res(x)/domain_res,3),"near")})
      NALCMS_Downscaled_Averaged_wetcharts <- lapply(NALCMS_Downscaled_Averaged_wetcharts,FUN=function(x){mask(project(x,domain_template,method="near"),domain)})
      if(!Use_NLCD){
        cover <- extract(NALCMS_Downscaled_Averaged_wetcharts[[1]][[1]],project(domain,NALCMS_Downscaled_Averaged_wetcharts[[1]]),weights=T,cells=T)
      }
      for(A in 1:length(NALCMS_Downscaled_Averaged_wetcharts)){
        NALCMS_Downscaled_Averaged_wetcharts[[A]][cover[,'cell']] <- NALCMS_Downscaled_Averaged_wetcharts[[A]][cover[,'cell']]*cover[,'weight']
      }
    }
    
    
    
    
    
  }else if(any(domain_res>res(Downscaled_Averaged_wetcharts[[1]]))){
    if(Use_NLCD){
      domain_reproj <- project(domain,crs(NLCD_Downscaled_Averaged_wetcharts[[1]]))
      
      #reproject to exact domain now using an average to effectively aggregate
      #while reprojecting.
      NLCD_Downscaled_Averaged_wetcharts <- lapply(NLCD_Downscaled_Averaged_wetcharts,FUN=function(x){mask(crop(x,domain_reproj,snap="out"),domain_reproj,touches=T,updatevalue=0)})
      cover <- extract(NLCD_Downscaled_Averaged_wetcharts[[1]][[1]],
                       project(domain,NLCD_Downscaled_Averaged_wetcharts[[1]]),
                       weights=T,cells=T)
      for(A in 1:length(NLCD_Downscaled_Averaged_wetcharts)){
        NLCD_Downscaled_Averaged_wetcharts[[A]][cover[,'cell']] <- NLCD_Downscaled_Averaged_wetcharts[[A]][cover[,'cell']]*cover[,'weight']
      }
      NLCD_Downscaled_Averaged_wetcharts <- lapply(NLCD_Downscaled_Averaged_wetcharts,FUN=function(x){project(extend(x,fill=0,ext(x)+(res(project(domain_template,crs(x)))*5)),domain_template,method="average")})
    }
    if(Use_NALCMS){
      domain_reproj <- project(domain,crs(NALCMS_Downscaled_Averaged_wetcharts[[1]]))
      NALCMS_Downscaled_Averaged_wetcharts <- lapply(NALCMS_Downscaled_Averaged_wetcharts,FUN=function(x){mask(crop(x,domain_reproj,snap="out"),domain_reproj,touches=T,updatevalue=0)})
      cover <- extract(NALCMS_Downscaled_Averaged_wetcharts[[1]][[1]],
                       project(domain,NALCMS_Downscaled_Averaged_wetcharts[[1]]),
                       weights=T,cells=T)
      for(A in 1:length(NALCMS_Downscaled_Averaged_wetcharts)){
        NALCMS_Downscaled_Averaged_wetcharts[[A]][cover[,'cell']] <- NALCMS_Downscaled_Averaged_wetcharts[[A]][cover[,'cell']]*cover[,'weight']
      }
      NALCMS_Downscaled_Averaged_wetcharts <- lapply(NALCMS_Downscaled_Averaged_wetcharts,FUN=function(x){project(extend(x,fill=0,ext(x)+(res(project(domain_template,crs(x)))*5)),domain_template,method="average")})
    }
  }
  ################################################################################
  #write output
  
  for(B in 1:length(Downscaled_Averaged_wetcharts)){
    if(Use_NLCD){
      writeCDF(NLCD_Downscaled_Averaged_wetcharts[[B]],
               paste0(Wetland_output_directory,'/Wetcharts_NLCD_Downscaled_subset_',B,'.nc'),
               force_v4=TRUE,
               varname='methane_emissions',
               unit='nmol/m2/s',
               longname=paste0('Methane emissions from Wetlands from the Wetcharts models, subset to models ',paste(Wetcharts_model_subset[[B]],collapse = ", ")),
               missval=-9999,
               overwrite=TRUE)
    }
    if(Use_NALCMS){
      writeCDF(NALCMS_Downscaled_Averaged_wetcharts[[B]],
               paste0(Wetland_output_directory,'/Wetcharts_NALCMS_Downscaled_subset_',B,'.nc'),
               force_v4=TRUE,
               varname='methane_emissions',
               unit='nmol/m2/s',
               longname=paste0('Methane emissions from Wetlands from the Wetcharts models, subset to models ',paste(Wetcharts_model_subset[[B]],collapse = ", ")),
               missval=-9999,
               overwrite=TRUE)
    }
  }
  
  ################################################################################
  #Visuals
  
  if(verbose){
    #the minimum is a ~arbitrary value given the log scale can go quite negative.
    #the max is the max across wetcharts pre and post downscaling.
    zlim_min <- -3
    zlim_max <- unlist(sapply(Averaged_wetcharts,FUN=function(x){global(x,max,na.rm=T)}))
    if(Use_NLCD){
      zlim_max <- max(unlist(sapply(NLCD_Downscaled_Averaged_wetcharts,FUN=function(x){global(x,max,na.rm=T)})),
                      zlim_max)
    }
    if(Use_NALCMS){
      zlim_max <- max(zlim_max,
                      unlist(sapply(NALCMS_Downscaled_Averaged_wetcharts,FUN=function(x){global(x,max,na.rm=T)})))
    }
    zlim_max <- log10(zlim_max)
    
    
    for(B in 1:length(Downscaled_Averaged_wetcharts)){
      model_list_string <- paste(Wetcharts_model_subset[[B]],collapse = ",")
      if(nchar(model_list_string)>50){
        model_list_string <- paste0(substr(model_list_string,0,50),"\n",
                                    substr(model_list_string,51,999))
      }
      
      #annual
      if(Use_NLCD){
        log_plot(input = NLCD_Downscaled_Averaged_wetcharts[[B]],zlim_min = zlim_min,
                 zlim_max = zlim_max,filename = paste0('Wetcharts_NLCD_Downscaled_subset_',B,"_annual"),
                 title = paste0("Annual NLCD downscaled Wetcharts CH4\nSaturated colorscale low end\nmodels ",
                                model_list_string),plot_directory=plot_directory,
                 domain=domain,County_Tigerlines=County_Tigerlines,
                 State_Tigerlines=State_Tigerlines)
      }
      if(Use_NALCMS){
        log_plot(input = NALCMS_Downscaled_Averaged_wetcharts[[B]],zlim_min = zlim_min,
                 zlim_max = zlim_max,filename = paste0('Wetcharts_NALCMS_Downscaled_subset_',B,"_annual"),
                 title = paste0("Annual NALCMS downscaled Wetcharts CH4\nSaturated colorscale low end\nmodels ",
                                model_list_string),plot_directory=plot_directory,
                 domain=domain,County_Tigerlines=County_Tigerlines,
                 State_Tigerlines=State_Tigerlines)
      }
      
      # Monthly
      # for(A in 1:12){
      #   if(Use_NLCD){
      #     log_plot(input = NLCD_Downscaled_Averaged_wetcharts[[B]][[A]],zlim_min = zlim_min,
      #              zlim_max = zlim_max,filename = paste0('Wetcharts_NLCD_Downscaled_subset_',B,"_month_",A),
      #              title = paste0(month.abb[A]," NLCD downscaled Wetcharts CH4\nSaturated colorscale low end\nmodels ",
      #                             model_list_string),plot_directory=plot_directory,
      #              domain=domain,County_Tigerlines=County_Tigerlines,
      #              State_Tigerlines=State_Tigerlines)
      #   }
      #   if(Use_NALCMS){
      #     log_plot(input = NALCMS_Downscaled_Averaged_wetcharts[[B]][[A]],zlim_min = zlim_min,
      #              zlim_max = zlim_max,filename = paste0('Wetcharts_NALCMS_Downscaled_subset_',B,"_month_",A),
      #              title = paste0(month.abb[A]," NALCMS downscaled Wetcharts CH4\nSaturated colorscale low end\nmodels ",
      #                             model_list_string),plot_directory=plot_directory,
      #              domain=domain,County_Tigerlines=County_Tigerlines,
      #              State_Tigerlines=State_Tigerlines)
      #   }
      # }
    }
  }
  
  
  
  #not possible unless a square domain because the output has already been
  #masked to exactly the domain polygon.  The coarser wetcharts data can't be
  #masked to cover the same exact area and fractional cover weighting would not
  #result in equivalent values as land cover based weighing.
  
  # ################################################################################
  # #Quick sanity check.  The total emissions in the domain should be the same pre
  # #and post downscaling regardless of land cover used.  Multiply by area to
  # #cancel out the per area unit (flux -> emission rate).
  # 
  # #if the domain cuts off partial 0.5 deg pixels from Wetcharts, the domain-total
  # #pre downscaling will not match.  So crop everything slightly to avoid this.
  # comparable_domain <- crop(Averaged_wetcharts[[1]][[1]],project(domain,crs(Averaged_wetcharts[[1]])),snap="in")
  # comparable_poly <- project(as.polygons(comparable_domain),domain)
  # 
  # colnamelist <- vector()
  # if(Use_NLCD){
  #   #combine the list into multiple layers (we don't care about separation here)
  #   NLCD_check <- rast(NLCD_Downscaled_Averaged_wetcharts)
  #   #crop to proper domain and sum
  #   NLCD_check <- crop(NLCD_check*cellSize(NLCD_check),comparable_poly)
  #   domain_total <- global(NLCD_check,sum,na.rm=T)
  #   colnamelist <- c(colnamelist,"NLCD_downscaled")
  # }
  # 
  # if(Use_NALCMS){
  #   NALCMS_check <- rast(NALCMS_Downscaled_Averaged_wetcharts)
  #   NALCMS_check <- crop(NALCMS_check*cellSize(NALCMS_check),comparable_poly)
  #   if(Use_NLCD){
  #     domain_total <- cbind(domain_total,global(NALCMS_check,sum,na.rm=T))
  #   }else{
  #     domain_total <- global(NALCMS_check,sum,na.rm=T)
  #   }
  #   colnamelist <- c(colnamelist,"NALCMS_downscaled")
  # }
  # 
  # Averaged_wetcharts_check <- rast(Averaged_wetcharts)
  # Averaged_wetcharts_check <- crop(Averaged_wetcharts_check*cellSize(Averaged_wetcharts_check),comparable_domain)
  # domain_total <- cbind(domain_total,global(Averaged_wetcharts_check,sum,na.rm=T))
  # colnamelist <- c(colnamelist,"not_downscaled")
  # 
  # domain_total <- as.data.frame(domain_total)
  # colnames(domain_total) <- colnamelist
  # 
  # percent_change <- abs(domain_total - domain_total[,1])/domain_total[,1]*100
  # if(!all(percent_change<1E-4,na.rm=T)){
  #   View(domain_total)
  #   stop("Something's gone wrong.  The total emissions (nmol/s) across the domain differs between the original and downscaled wetcharts.")
  # }
  # 
  # 
  # 
  # 
  # 
  # 
  # #NA's can't be used in math or the result is also an NA.  Force all NA's
  # #across both wetcharts and downscaled wetcharts to 0 so we can be sure there
  # #aren't any cases where there's an NA in one product, not the other
  # 
  # Averaged_wetcharts_check[is.na(Averaged_wetcharts_check)] <- 0
  # 
  # #can't really do this as it would depend on the domain projection.  Could
  # #potentially use the value pre projection if saved...
  # 
  # if(Use_NLCD){
  #   NLCD_pixel_check <- crop(NLCD_pixel_check*cellSize(NLCD_pixel_check),comparable_poly)
  #   NLCD_pixel_check[is.na(NLCD_pixel_check)] <- 0
  #   #given pixels can be > 1E11 mol/s, differing by < 1 is considered rounding
  #   #error.
  #   if(max(global(abs(aggregate(NLCD_pixel_check,fact=5,sum) -
  #                     Averaged_wetcharts_check),max,na.rm=T))>1){
  #     stop("Something's gone wrong.  There are pixels that differ between downscaled and original wetcharts when aggregating the NLCD downscaled values back to the original resolution.")
  #   }
  # }
  # 
  # if(Use_NALCMS){
  #   NALCMS_pixel_check <- crop(NALCMS_pixel_check*cellSize(NALCMS_pixel_check),comparable_poly)
  #   NALCMS_pixel_check[is.na(NALCMS_pixel_check)] <- 0
  #   if(max(global(abs(aggregate(NALCMS_pixel_check,fact=5,sum) -
  #                     Averaged_wetcharts_check),max,na.rm=T))>1){
  #     stop("Something's gone wrong.  There are pixels that differ between downscaled and original wetcharts when aggregating the NALCMS downscaled values back to the original resolution.")
  #   }
  # }
  # 
  cat("Finished wetland sector: Disaggregate_Wetcharts in",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes\n\n")
}

