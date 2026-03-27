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
#'@inheritParams Municipal_solid_waste
#'
#'@param verbose Logical indicating whether to save additional output.  This
#'  includes monthly plots of the gridded methane emissions on log scales, saved
#'  separately for each model subset used.
#'@param Source_wetland_NLCD Character.  Pulled from \code{\link{M3T_config}}.
#'@param Source_wetcharts Character.  Pulled from \code{\link{M3T_config}}.
#'@param Wetcharts_model_subset List of numeric vectors.  Pulled from
#'  \code{\link{M3T_config}}.
#'@returns Nothing is returned from the function, but the main outputs are
#'  netcdf files of the methane emissions from wetlands with 1 file per land
#'  cover used and per model subset.  They are titled
#'  "Wetcharts_NLCD_Downscaled_subset_#.nc" where # is just a numeric to
#'  identify which model subset, in case multiple were set.
#'
#'  If verbose is set to TRUE, then multiple figures are also saved.  Log scale
#'  plots with consistent axes are saved for each model subset used.  They are
#'  named "Wetcharts_NLCD_subset_#_annual.png" where # is a numeric to identify
#'  the model subset.
#'@seealso [CH4_inventory_build()] Calculates methane inventory using settings
#'  provided in config.
#'
#'  [M3T_config] Generates the config function with user-editable settings used
#'  throughout processing.
#'
#'  [SOCCR_Wetlands()] Calculates methane emissions for the wetland sector using
#'  the state of the carbon cycle report instead.
#'@keywords internal

Disaggregate_Wetcharts <- function(input_directory,
                                   output_directory,
                                   domain,
                                   domain_template,
                                   verbose,
                                   plot_directory,
                                   inventory_year,
                                   County_Tigerlines,
                                   State_Tigerlines,
                                   State_CB,
                                   Source_wetland_NLCD,
                                   Source_wetcharts,
                                   Wetcharts_model_subset){
  
  starttime <- Sys.time()
  cat("Starting wetland sector: Disaggregate_Wetcharts\n")
  
  Wetland_output_directory <- file.path(output_directory,"Wetlands")
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
  
  Wetcharts_file <- file.path(input_directory,paste0("User_supplied_WetCHARTs_v1_3_3.nc"))
  invisible(file.copy(Source_wetcharts,Wetcharts_file,overwrite = T))
  
  Wetcharts <- terra::rast(Wetcharts_file)
  ################################################################################
  #define the domain of interest + a little buffer, crop wetcharts
  
  Wetcharts <- terra::crop(Wetcharts,terra::ext(terra::project(domain,terra::crs(Wetcharts)))+2)
  ################################################################################
  #load in the landcover - national land cover database (NLCD)
  NLCD_file <- file.path(input_directory,"NLCD")
  dir.create(NLCD_file,showWarnings = F)
  if(dir.exists(Source_wetland_NLCD)){
    invisible(file.copy(list.files(Source_wetland_NLCD,full.names=T),
                        NLCD_file,overwrite = T,recursive=T))
  }else{
    invisible(file.copy(list.files(dirname(Source_wetland_NLCD),full.names=T),
                        NLCD_file,overwrite = T,recursive=T))
  }
  NLCD_file <- list.files(NLCD_file,pattern="*.tif$|*.img$",full.names=T)
  NLCD <- terra::rast(NLCD_file)
  
  if(nrow(terra::levels(NLCD)[[1]])<100){
    #correct levels from the R interpreted ones (provided in manual)
    NLCD_key <- data.frame("Value"=c(11,12,21:24,31,41:43,52,71,81:82,90,95),
                           "Land_Class"=terra::levels(NLCD)[[1]][,2])
    levels(NLCD) <- NLCD_key
  }

  # reproject states to matching CRS
  NLCD_states_trans <- terra::project(State_Tigerlines,terra::crs(NLCD))
  
  #Crop to just the extent of Wetcharts
  NLCD <- terra::crop(NLCD,
                      terra::project(x=terra::ext(Wetcharts),from=terra::crs(Wetcharts),to=terra::crs(NLCD)))
  
  cat("Finished loading in all data at",format(Sys.time(),"%H:%M"),". Next step is time-consuming.\n")
  ################################################################################
  #set wetlands to a value of 1 and all other land cover to 0, then project to
  #domain CRS at 0.1 deg.
  
  #solely for the exact grid to project to
  template <- terra::disagg(Wetcharts[[1]],fact=5)
  terra::values(template) <- 1
  template_poly <- terra::as.polygons(template)
  
  #force all values between 0 and 89 to 0.  values between 89 and 200 are forced
  #to 1.  90 and 95 = wetland land cover for NLCD.
  NLCD <- terra::classify(NLCD,matrix(c(0,89,0,
                                        89,200,1),
                                      ncol=3,byrow=T))
  
  #project to a grid with the exact right resolution, extent and origin. First
  #crop/mask to the proper extent, set NA's to 0, then project to the exact
  #domain grid.  No need to extend given this is far higher resolution.
  template_poly <- terra::project(template_poly,terra::crs(NLCD))
  NLCD=terra::crop(NLCD,template_poly,snap="out")
  NLCD=terra::mask(x=NLCD,mask=template_poly,touches=F)
  NLCD=terra::project(NLCD,template,method="sum")

  cat("Finished reprojecting land cover data at",format(Sys.time(),"%H:%M"),"\n")
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
  
  Wetcharts_model_indx <- lapply(Wetcharts_model_subset,
                                 FUN = function(x){which(Wetcharts_models %in% x)})
  
  #Now create a list of the various wetcharts subsets
  Wetcharts <- lapply(Wetcharts_model_indx,FUN=function(x){terra::subset(Wetcharts,x)})
  
  #pull the months from the names of wetcharts this time
  Wetcharts_months <- lapply(Wetcharts,FUN = function(x){
    as.numeric(sapply(strsplit(names(x),"_"),"[[",5))})
  
  #to run annually
  Averaged_wetcharts <- lapply(Wetcharts,terra::mean)
  
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
                                          FUN=function(x){terra::disagg(x*terra::cellSize(x),fact=5)})
  
  #redistribute using wetland fraction, crop to domain, and reconvert to
  #nmol/m2s for the smaller pixels
  NLCD_Downscaled_Averaged_wetcharts <- lapply(Downscaled_Averaged_wetcharts,FUN=function(x){
    terra::crop(x*NLCD_wetland_fraction/terra::cellSize(x),terra::project(domain,terra::crs(NLCD_wetland_fraction)),snap="out")})
  
  ################################################################################
  # reproject output to match domain exactly
  
  #aggregate/disaggregate to a similar resolution
  domain_trans <- terra::project(domain_template,terra::crs(Downscaled_Averaged_wetcharts[[1]]))
  domain_res <- terra::res(domain_trans)
  if(any(domain_res<terra::res(Downscaled_Averaged_wetcharts[[1]]))){
    NLCD_Downscaled_Averaged_wetcharts <- lapply(NLCD_Downscaled_Averaged_wetcharts,FUN=function(x){terra::disagg(x,round(terra::res(x)/domain_res,3),"near")})
    
    #reproject to exact domain now.  Here using nearest neighbor to prevent
    #only 1 row/column of higher res pixels on the border from being
    #interpolated.
    NLCD_Downscaled_Averaged_wetcharts <- lapply(NLCD_Downscaled_Averaged_wetcharts,FUN=function(x){terra::mask(terra::project(x,domain_template,method="near"),domain)})
    
    cover <- terra::extract(NLCD_Downscaled_Averaged_wetcharts[[1]][[1]],
                            terra::project(domain,NLCD_Downscaled_Averaged_wetcharts[[1]]),
                            weights=T,cells=T)
    for(A in 1:length(NLCD_Downscaled_Averaged_wetcharts)){
      NLCD_Downscaled_Averaged_wetcharts[[A]][cover[,'cell']] <- NLCD_Downscaled_Averaged_wetcharts[[A]][cover[,'cell']]*cover[,'weight']
    }
    
  }else if(any(domain_res>terra::res(Downscaled_Averaged_wetcharts[[1]]))){
    domain_reproj <- terra::project(domain,terra::crs(NLCD_Downscaled_Averaged_wetcharts[[1]]))
    
    #reproject to exact domain now using an average to effectively aggregate
    #while reprojecting.
    NLCD_Downscaled_Averaged_wetcharts <- lapply(NLCD_Downscaled_Averaged_wetcharts,FUN=function(x){terra::mask(terra::crop(x,domain_reproj,snap="out"),domain_reproj,touches=T,updatevalue=0)})
    cover <- terra::extract(NLCD_Downscaled_Averaged_wetcharts[[1]][[1]],
                            terra::project(domain,NLCD_Downscaled_Averaged_wetcharts[[1]]),
                            weights=T,cells=T)
    for(A in 1:length(NLCD_Downscaled_Averaged_wetcharts)){
      NLCD_Downscaled_Averaged_wetcharts[[A]][cover[,'cell']] <- NLCD_Downscaled_Averaged_wetcharts[[A]][cover[,'cell']]*cover[,'weight']
    }
    NLCD_Downscaled_Averaged_wetcharts <- lapply(NLCD_Downscaled_Averaged_wetcharts,FUN=function(x){terra::project(terra::extend(x,fill=0,terra::ext(x)+(terra::res(terra::project(domain_template,terra::crs(x)))*5)),domain_template,method="average")})
    
    #this approach will not NA out areas outside polygon domains - do so now.
    #Pixels with no data and are outside the domain.  It's possible for
    #something to be very slightly outside the domain and non zero due to the
    #reprojecting and this will retain any such data.
    for(A in 1:length(NLCD_Downscaled_Averaged_wetcharts)){
      NLCD_Downscaled_Averaged_wetcharts[[A]][terra::mask(NLCD_Downscaled_Averaged_wetcharts[[A]],domain,inverse=T)==0] <- NA
    }
  }
  ################################################################################
  #write output
  
  for(B in 1:length(Downscaled_Averaged_wetcharts)){
    writeCDF_no_newline(NLCD_Downscaled_Averaged_wetcharts[[B]],
                        paste0(Wetland_output_directory,'/Wetcharts_NLCD_Downscaled_subset_',B,'.nc'),
                        force_v4=TRUE,
                        varname='methane_emissions',
                        unit='nmol/m2/s',
                        longname=paste0('Methane emissions from Wetlands from the Wetcharts models, subset to models ',paste(Wetcharts_model_subset[[B]],collapse = ", ")),
                        missval=-9999,
                        overwrite=TRUE)
  }
  
  ################################################################################
  #Visuals
  
  if(verbose){
    #the minimum is a ~arbitrary value given the log scale can go quite negative.
    #the max is the max across wetcharts pre and post downscaling.
    zlim_min <- -3
    zlim_max <- unlist(sapply(Averaged_wetcharts,FUN=function(x){terra::global(x,max,na.rm=T)}))
    zlim_max <- max(unlist(sapply(NLCD_Downscaled_Averaged_wetcharts,FUN=function(x){terra::global(x,max,na.rm=T)})),
                    zlim_max)
    zlim_max <- log10(zlim_max)
    
    
    for(B in 1:length(Downscaled_Averaged_wetcharts)){
      model_list_string <- paste(Wetcharts_model_subset[[B]],collapse = ",")
      if(nchar(model_list_string)>50){
        model_list_string <- paste0(substr(model_list_string,0,50),"\n",
                                    substr(model_list_string,51,999))
      }
      
      #annual
      log_plot(input = NLCD_Downscaled_Averaged_wetcharts[[B]],zlim_min = zlim_min,
               zlim_max = zlim_max,filename = paste0('Wetcharts_NLCD_Downscaled_subset_',B,"_annual"),
               title = paste0("Annual NLCD downscaled Wetcharts CH4\nSaturated colorscale low end\nmodels ",
                              model_list_string),plot_directory=plot_directory,
               domain=domain,County_Tigerlines=County_Tigerlines,
               State_CB=State_CB)
      
      # Monthly
      # for(A in 1:12){
      #   if(Use_NLCD){
      #     log_plot(input = NLCD_Downscaled_Averaged_wetcharts[[B]][[A]],zlim_min = zlim_min,
      #              zlim_max = zlim_max,filename = paste0('Wetcharts_NLCD_Downscaled_subset_',B,"_month_",A),
      #              title = paste0(month.abb[A]," NLCD downscaled Wetcharts CH4\nSaturated colorscale low end\nmodels ",
      #                             model_list_string),plot_directory=plot_directory,
      #              domain=domain,County_Tigerlines=County_Tigerlines,
      #              State_CB=State_CB)
      #   }
      #   if(Use_NALCMS){
      #     log_plot(input = NALCMS_Downscaled_Averaged_wetcharts[[B]][[A]],zlim_min = zlim_min,
      #              zlim_max = zlim_max,filename = paste0('Wetcharts_NALCMS_Downscaled_subset_',B,"_month_",A),
      #              title = paste0(month.abb[A]," NALCMS downscaled Wetcharts CH4\nSaturated colorscale low end\nmodels ",
      #                             model_list_string),plot_directory=plot_directory,
      #              domain=domain,County_Tigerlines=County_Tigerlines,
      #              State_CB=State_CB)
      #   }
      # }
    }
  }
  
  cat("Finished wetland sector: Disaggregate_Wetcharts at",format(Sys.time(),"%H:%M"),"with a total runtime of",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes\n\n")
}

