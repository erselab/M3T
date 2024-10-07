#'@title Create SOCCR based wetland methane maps
#'
#'@description `SOCCR_Wetlands` writes 2 netcdf files of gridded wetland methane
#'  emissions, 1 for SOCCR1 and 1 for SOCCR2.  May include freshwater wetland
#'  emissions using Rosentretet et al. too.  Includes optional visuals as well.
#'
#'@details This function takes the output of `NWI_Wetland_fraction`, which is
#'  per pixel fractional coverage of wetlands separated by wetland type, and
#'  applies emission factors to convert coverage to methane emissions.  It is
#'  simply applying SOCCR1 or SOCCR2 average emissions from wetlands to the NWI
#'  activity data.  Additionally, freshwater emissions are calculated using
#'  emissions from Rosentreter et al.
#'
#'  SOCCR1 values are based on the arithmetic averages of Table F5, SOCC2 values
#'  are based on the arithmetic averages of Tables 13B.8 to 13B.11 for PFO and
#'  PNF and Table 15A.2 for M2 and E2, and Lakes and Rivers (L1, L2, and R1 -
#'  R4) are from Rosentreter et al. using the median flux from rivers and the
#'  largest lake class (>1 km).  This lake flux was chosen as McDonald et al.
#'  show that large lakes (>1 km2) constitute 71\% of the total lake area in
#'  CONUS (rising to 90\% if including the Great Lakes).
#'
#'  SOCCR1 is available at
#'  \url{https://www.carboncyclescience.us/state-carbon-cycle-report-soccr} and
#'  SOCCR2 is available at \url{https://carbon2018.globalchange.gov/}.
#'
#'  See references \href{https://doi.org/10.4319/lo.2012.57.2.0597}{McDonald et
#'  al.} and \href{https://doi.org/10.1038/s41561-021-00715-2}{Rosentreter et al.}
#'@param output_directory Character providing the full filepath to save
#'  processed data
#'@param verbose Logical indicating whether to save additional output.  This
#'  includes monthly plots of the gridded methane emissions on log scales, saved
#'  separately for each model subset and each land cover dataset used.
#'@param domain SpatRaster providing the desired output grid, including the
#'  desired resolution and coordinate reference system
#'@param Use_SOCCR1 Logical.  Pulled from config file.  Indicating whether or
#'  not to calculate emissions using SOCCR1.
#'@param Use_SOCCR2 Logical.  Pulled from config file.  Indicating whether or
#'  not to calculate emissions using SOCCR2.
#'@param Include_freshwater Logical.  Pulled from config file.  Indicating
#'  whether or not to calculate emissions for freshwater wetlands using
#'  Rosentreter et al.
#'@param Wetland_EFs Data frame.  Pulled from config file. Emission factors to
#'  use for all wetlands - including SOCCR1, SOCCR2, and Rosentreter et al.
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
#'@param focus_city_tigerlines SpatVector.  United States Census Bureau county
#'  shapefile.  Available at
#'  \url{https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html}.
#'  Only relevant if a focus city was set in main and verbose=TRUE.
#'@param  watershed_shapefile Character.  Commission for Environmental
#'  Cooperation watershed shapefile.  Available at
#'  \url{http://www.cec.org/north-american-environmental-atlas/watersheds/}.
#'  Only relevant if USE_SOCCR2 = TRUE.
#'@returns Nothing is returned from the function, but the main outputs are 2
#'  netcdcf files of the methane emissions from wetlands with 1 file for SOCCR1
#'  and 1 file for SOCCR2 based emissions.  Lakes and rivers from Rosentreter et
#'  al. emissions can also be included.  They are titled "SOCCR1.nc" and
#'  "SOCCR2.nc".
#'
#'  If verbose is set to TRUE, then multiple figures are also saved.  Log scale
#'  plots with consistent axes are saved for the 2 SOCCR emissions.  They are
#'  saved as "SOCCR1.png" and "SOCCR2.png".
#'@examples
#' library(terra)
#' grid_bbox=cbind(c(-76.65,-73.65),c(38.97,40.97))
#' grid_res=0.01
#' grid_crs="epsg:4326"
#' grid <- rast(nrows=diff(range(grid_bbox[,2]))/grid_res,
#'             ncols=diff(range(grid_bbox[,1]))/grid_res, xmin=min(grid_bbox[,1]),
#'             xmax=max(grid_bbox[,1]), ymin=min(grid_bbox[,2]), ymax=max(grid_bbox[,2]),
#'             crs=grid_crs)
#' EFs <- data.frame("E2"=c(10.3,15.29*16.043/12.011),
#'                           "M2"=c(10.3,15.29*16.043/12.011),
#'                           "PFO"=c(36,18.52*16.043/12.011),
#'                           "PNF"=c(36,24.92*16.043/12.011),
#'                           "L1"=5,
#'                           "L2"=5,
#'                           "R1"=7.88,
#'                           "R2"=7.88,
#'                           "R3"=7.88,
#'                           "R4"=7.88)
#' rownames(EFs) <- c("SOCCR1","SOCCR2")
#'
#' # convert from g CH4 per m2 per yr to nmol/m2/s
#' EFs=EFs*1E9/(16.043*365.25*24*60*60)
#'
#' SOCCR_Wetlands(output_directory="~/../Desktop/",
#'                plot_directory="~/../Desktop/plots/",
#'                domain=grid,
#'                Use_SOCCR1=TRUE,
#'                Use_SOCCR2=TRUE,
#'                Include_freshwater=TRUE,
#'                Wetland_EFs=EFs,
#'                verbose=TRUE,
#'                County_Tigerlines=vect("~/../Desktop/County_Tigerlines/tl_2018_us_county.shp"),
#'                State_Tigerlines=vect("~/../Desktop/State_Tigerlines/tl_2018_us_state.shp"),
#'                focus_city_tigerlines=focus_city,
#'                watershed_shapefile="~/../Desktop/watersheds_shapefile/watershed_p_v2.shp")
#'
#'@author Joe Pitt, \email{madeup@@wisc.edu}
#'@author Kris Hajny, \email{blank@@fake.edu}
#'@author Israel Lopez-Coto, \email{test@@test.edu}
#'@reference \href{https://doi.org/10.4319/lo.2012.57.2.0597}{McDonald et al.}
#'@reference \href{https://doi.org/10.1038/s41561-021-00715-2}{Rosentreter et
#'  al.}
#'@export


SOCCR_Wetlands <- function(output_directory,
                           plot_directory,
                           domain,
                           Use_SOCCR1,
                           Use_SOCCR2,
                           Include_freshwater,
                           Wetland_EFs,
                           verbose,
                           County_Tigerlines,
                           State_Tigerlines,
                           focus_city_tigerlines,
                           watershed_shapefile){
  
  ## Wetland_emissions_r2.R
  ## In use: 2021-11-02 20:00
  #
  # Load in the various state wetland fraction rasters
  # These overlap somewhat, so crop each to the squares within each state
  # Then add together and assign fluxes to each class
  
  starttime <- Sys.time()
  cat("Starting wetland sector: SOCCR_Wetlands\n")
  ################################################################################
  #load in the watersheds and prepare for use with SOCCR2 EFs
  
  if(Use_SOCCR2){
    watershed <- vect(watershed_shapefile)
    
    #we only care about NAW1 in English, so aggregate all polygons to this level
    #and remove extra data
    watershed <- watershed["NAW1_EN"]
    watershed <- aggregate(watershed,by="NAW1_EN")
    watershed <- crop(watershed,ext(project(domain,crs(watershed)))*1.1)
    expanded_watershed <- buffer(watershed,2E4)
    watershed <- aggregate(expanded_watershed-watershed+watershed,"NAW1_EN")
    watershed <- project(watershed,crs(domain))
    
    if(Include_freshwater){
      #now associate the corresponding EF to each region
      regional_EFs <- data.frame(t(Wetland_EFs["SOCCR2",c("E2_Atlantic","E2_Gulf","E2_Pacific","E2_Hudson")]),
                                 t(Wetland_EFs["SOCCR2",c("M2_Atlantic","M2_Gulf","M2_Pacific","M2_Hudson")]),
                                 Wetland_EFs["SOCCR2","PFO"],
                                 Wetland_EFs["SOCCR2","PNF"],
                                 Wetland_EFs["SOCCR2","L1"],
                                 Wetland_EFs["SOCCR2","L2"],
                                 Wetland_EFs["SOCCR2","R1"],
                                 Wetland_EFs["SOCCR2","R2"],
                                 Wetland_EFs["SOCCR2","R3"],
                                 Wetland_EFs["SOCCR2","R4"],
                                 "Region"=c("Atlantic Ocean","Gulf of Mexico","Pacific Ocean","Hudson Bay"))
      colnames(regional_EFs) <- c("E2","M2","PFO","PNF","L1","L2","R1","R2","R3","R4","Region")
      watershed <- merge(watershed,regional_EFs,by.x="NAW1_EN",by.y="Region")
    }else{
      regional_EFs <- data.frame(t(Wetland_EFs["SOCCR2",c("E2_Atlantic","E2_Gulf","E2_Pacific","E2_Hudson")]),
                                 t(Wetland_EFs["SOCCR2",c("M2_Atlantic","M2_Gulf","M2_Pacific","M2_Hudson")]),
                                 Wetland_EFs["SOCCR2","PFO"],
                                 Wetland_EFs["SOCCR2","PNF"],
                                 "Region"=c("Atlantic Ocean","Gulf of Mexico","Pacific Ocean","Hudson Bay"))
      colnames(regional_EFs) <- c("E2","M2","PFO","PNF","Region")
      watershed <- merge(watershed,regional_EFs,by.x="NAW1_EN",by.y="Region")
    }
  }
  
  #update wetland EF to simplify SOCCR1 since SOCCR2 E2 and M2 have already been
  #dealt with
  Wetland_EFs <- Wetland_EFs[,c("E2_Atlantic","M2_Atlantic","PFO","PNF","L1","L2","R1","R2","R3","R4")]
  colnames(Wetland_EFs)[1:2] <- c("E2","M2")
  
  ################################################################################
  #load in and process the Wetland_fraction_r1 output to convert from wetland
  #coverage to wetland emissions
  
  NWI_files <- list.files(paste0(output_directory,"/NWI/"),".tiff",full.names = T)
  
  SOCCR_wetland_types <- c("M2","E2","PFO","PNF")
  Freshwater_wetland_types <- c("R1","R2","R3","R4","L1","L2")

  if(Use_SOCCR2){
    subset_data <- rast(NWI_files[1])
    if(nrow(watershed)!=1){
      coverage <- watershed[,c("NAW1_EN",Wetland_types[i])] %>% 
        split(f=watershed$NAW1_EN) %>% 
        lapply(function(x){extract(subset_data,x,weights=T,exact=T,cells=T)})
    }
  }
  
  #process separately for each type (different EFs)
  if(Use_SOCCR1 | Use_SOCCR2){
    subset_files <- NWI_files[grep(SOCCR_wetland_types[1],NWI_files)]
    subset_data <- rast(subset_files)
    #given NWI extends somewhat beyond state bounds, there is overlap.  So max
    #should combine them akin to sum, but without double counting.
    subset_data <- max(subset_data)
    names(subset_data) <- SOCCR_wetland_types[1]
    #for a later sanity check
    all_frac <- subset_data
    if(Use_SOCCR1){
      SOCCR1_flux <- subset_data*Wetland_EFs["SOCCR1",SOCCR_wetland_types[1]]
    }
    if(Use_SOCCR2){
      if(nrow(watershed)==1){
        subset_data <- subset_data*as.numeric(values(watershed[,SOCCR_wetland_types[1]]))
      }else{
        subset_data[coverage[,'cell'],drop=F] <- watershed[coverage[,'cell']*coverage[,'weight'],SOCCR_wetland_types[1]]
      }
      SOCCR2_flux <- subset_data
    }
    
    for(i in 2:length(SOCCR_wetland_types)){
      subset_files <- NWI_files[grep(SOCCR_wetland_types[i],NWI_files)]
      subset_data <- rast(subset_files)
      #given NWI extends somewhat beyond state bounds, there is overlap.  So max
      #should combine them akin to sum, but without double counting.
      subset_data <- max(subset_data)
      names(subset_data) <- SOCCR_wetland_types[i]
      #for a later sanity check
      all_frac <- all_frac+subset_data
      if(Use_SOCCR1){
        SOCCR1_flux <- c(SOCCR1_flux,subset_data*Wetland_EFs["SOCCR1",SOCCR_wetland_types[i]]) 
      }
      if(Use_SOCCR2){
        if(nrow(watershed)==1){
          subset_data <- subset_data*as.numeric(values(watershed[,SOCCR_wetland_types[i]]))
        }else{
          subset_data[coverage[,'cell'],drop=F] <- watershed[coverage[,'cell']*coverage[,'weight'],SOCCR_wetland_types[i]]
        }
        SOCCR2_flux <- c(SOCCR2_flux,subset_data)
      }
    }
  }
  
  
  
  #repeat the process for freshwater
  if(Include_freshwater){
    subset_files <- NWI_files[grep(Freshwater_wetland_types[1],NWI_files)]
    subset_data <- rast(subset_files)
    subset_data <- max(subset_data)
    names(subset_data) <- Freshwater_wetland_types[1]
    if(Use_SOCCR1 | Use_SOCCR2){
      all_frac <- all_frac+subset_data
    }else{
      all_frac <- subset_data
    }
    Freshwater_flux <- subset_data*Wetland_EFs["SOCCR1",Freshwater_wetland_types[1]]
    
    for(i in 2:length(Freshwater_wetland_types)){
      subset_files <- NWI_files[grep(Freshwater_wetland_types[i],NWI_files)]
      subset_data <- rast(subset_files)
      subset_data <- max(subset_data)
      all_frac <- all_frac+subset_data
      names(subset_data) <- Freshwater_wetland_types[i]
      Freshwater_flux <- c(Freshwater_flux,subset_data*Wetland_EFs["SOCCR1",Freshwater_wetland_types[i]])
    }
  }
  
  # Check that the fractions are always between 0 and 1
  max_frac <- unlist(global(all_frac,max))*100
  min_frac <- unlist(global(all_frac,min))*100
  if(max_frac>100.1){
    stop(paste0("some pixels have over 100% wetland coverage.  Range is ",min_frac,"% to ",max_frac,"%"))
  }
  ################################################################################
  #save the output
  
  if(Use_SOCCR1){
    SOCCR1_flux <- crop(SOCCR1_flux,ext(domain))
    writeCDF(sum(SOCCR1_flux),
             file.path(output_directory,'SOCCR1.nc'),
             force_v4=TRUE,
             varname='methane_emissions',
             unit='nmol/m2/s',
             longname='Methane emissions from National Wetland Inventory separated by classes, using fluxes from the SOCCR1 report',
             missval=-9999,
             overwrite=TRUE)
  }
  if(Use_SOCCR2){
    SOCCR2_flux <- crop(SOCCR2_flux,ext(domain))
    writeCDF(sum(SOCCR2_flux),
             file.path(output_directory,'SOCCR2.nc'),
             force_v4=TRUE,
             varname='methane_emissions',
             unit='nmol/m2/s',
             longname='Methane emissions from National Wetland Inventory separated by classes, using fluxes from the SOCCR2 report',
             missval=-9999,
             overwrite=TRUE)
  }
  if(Include_freshwater){
    Freshwater_flux <- crop(Freshwater_flux,ext(domain))
    writeCDF(sum(Freshwater_flux),
             file.path(output_directory,'Freshwater.nc'),
             force_v4=TRUE,
             varname='methane_emissions',
             unit='nmol/m2/s',
             longname='Methane emissions from National Wetland Inventory separated by classes, using the median flux from Rosentreter et al. (2021) for lakes and rivers',
             missval=-9999,
             overwrite=TRUE)
  }

  ################################################################################
  #visuals
  
  if(verbose){
    #the minimum is a ~arbitrary value given the log scale can go quite negative.
    zlim_min <- -3
    zlim_max <- 0
    if(Use_SOCCR1){
      SOCCR1_flux <- sum(SOCCR1_flux)
      zlim_max <- max(zlim_max,as.numeric(global(SOCCR1_flux,max)))
    }
    if(Use_SOCCR2){
      SOCCR2_flux <- sum(SOCCR2_flux)
      zlim_max <- max(zlim_max,as.numeric(global(SOCCR2_flux,max)))
    }
    if(Include_freshwater){
      Freshwater_flux <- sum(Freshwater_flux)
      zlim_max <- max(zlim_max,as.numeric(global(Freshwater_flux,max)))
    }
    
    
    zlim_max <- log10(zlim_max)
    if(Use_SOCCR1){
      log_plot(input = SOCCR1_flux,
               zlim_min = zlim_min, zlim_max = zlim_max,
               filename = 'SOCCR1',
               title = paste0("SOCCR1 CH4\nSaturated colorscale low end"))
    }
    if(Use_SOCCR2){
      log_plot(input = SOCCR2_flux,
               zlim_min = zlim_min, zlim_max = zlim_max,
               filename = 'SOCCR2',
               title = paste0("SOCCR2 CH4\nSaturated colorscale low end"))
    }
    if(Include_freshwater){
      log_plot(input = Freshwater_flux,
               zlim_min = zlim_min, zlim_max = zlim_max,
               filename = 'Freshwater',
               title = paste0("Freshwater CH4\nSaturated colorscale low end"))
    }
  }
  cat("Finished wastewater sector: SOCCR_Wetlands in",difftime(Sys.time(),starttime,units = "min"),"minutes\n")
}