#'@title Create SOCCR based wetland methane maps
#'
#'@description `SOCCR_Wetlands` writes up to 3 netcdf files of gridded wetland
#'  methane emissions, 1 for SOCCR1, 1 for SOCCR2, and 1 for freshwater.
#'  Includes optional visuals as well.
#'
#'@details This function takes the output of `NWI_Wetland_fraction`, which is
#'  per pixel fractional coverage of wetlands separated by wetland type, and
#'  applies emission factors to convert coverage to methane emissions.  It is
#'  simply applying SOCCR1 or SOCCR2 average emissions from wetlands to the NWI
#'  activity data.  Additionally, freshwater emissions can be calculated using
#'  emissions from Rosentreter et al.
#'
#'  SOCCR1 values are based on the arithmetic averages of Table F5, SOCC2 values
#'  are based on the arithmetic averages of Tables 13B.8 to 13B.11 for PFO and
#'  PNF and Table 15A.2 for M2 and E2, and Lakes and Rivers (L1, L2, and R1 -
#'  R4) are from Rosentreter et al. using the median flux from rivers and the
#'  largest lake class (>1 km).  SOCCR2 is calculated by watershed as there was
#'  regionally separated data.  The lake flux was chosen as McDonald et al. show
#'  that large lakes (>1 km2) constitute 71\% of the total lake area in CONUS
#'  (rising to 90\% if including the Great Lakes).
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
#'  includes plots of the gridded methane emissions on log scales, saved
#'  separately for SOCCR1, SOCCR2, and freshwater.
#'@param domain SpatVector polygon outlining the desired output area
#'@param domain_template SpatRaster providing the desired output grid, including
#'  the desired resolution and coordinate reference system
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
#'@param State_CB SpatVector.  United States Census Bureau county
#'  shapefile.  Available at
#'  \url{https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html}.
#'  Only relevant if verbose=TRUE.
#'@param  Watershed_file Character.  Commission for Environmental
#'  Cooperation watershed shapefile.  Available at
#'  \url{http://www.cec.org/north-american-environmental-atlas/watersheds/}.
#'  Only relevant if USE_SOCCR2 = TRUE.
#'@returns Nothing is returned from the function, but the main outputs are up to
#'  3 netcdcf files of the methane emissions from wetlands with 1 file for
#'  SOCCR1 and 1 file for SOCCR2 based emissions.  Lakes and rivers from
#'  Rosentreter et al. emissions can also be included.  They are titled
#'  "SOCCR1.nc", "SOCCR2.nc", and "Freshwater.nc".
#'
#'  If verbose is set to TRUE, then multiple figures are also saved.  Log scale
#'  plots with consistent axes are saved for the 2 SOCCR emissions and
#'  freshwater emissions.  They are saved as "SOCCR1.png", "SOCCR2.png", and
#'  "Freshwater.png".
#'@examples
#' library(terra)
#' grid_bbox=cbind(c(-76.65,-73.65),c(38.97,40.97))
#' grid_res=0.01
#' grid_crs="epsg:4326"
#' grid <- rast(nrows=diff(range(grid_bbox[,2]))/grid_res,
#'              ncols=diff(range(grid_bbox[,1]))/grid_res, xmin=min(grid_bbox[,1]),
#'              xmax=max(grid_bbox[,1]), ymin=min(grid_bbox[,2]), ymax=max(grid_bbox[,2]),
#'              crs=grid_crs)
#' grid_vect <- as.polygons(ext(grid),crs=grid_crs)
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
#' SOCCR_Wetlands(output_directory="~/../Desktop/out/",
#'                plot_directory="~/../Desktop/plots/",
#'                domain=grid,
#'                Use_SOCCR1=TRUE,
#'                Use_SOCCR2=TRUE,
#'                Include_freshwater=TRUE,
#'                Wetland_EFs=EFs,
#'                verbose=TRUE,
#'                County_Tigerlines=vect("~/../Desktop/in/County_Tigerlines/tl_2018_us_county.shp"),
#'                State_CB=vect("~/../Desktop/in/State_CB/tl_2018_us_state.shp"),
#'                Watershed_file="~/../Desktop/in/watersheds_shapefile/watershed_p_v2.shp")
#'
#'@author Joe Pitt, \email{madeup@@wisc.edu}
#'@author Kris Hajny, \email{blank@@fake.edu}
#'@author Israel Lopez-Coto, \email{test@@test.edu}
#'@references \href{https://doi.org/10.4319/lo.2012.57.2.0597}{McDonald et al.}
#'@references \href{https://doi.org/10.1038/s41561-021-00715-2}{Rosentreter et
#'  al.}
#'@export
#'@seealso 
#' * [CH4_inventory_build()] Calculates methane inventory using settings provided in config.
#' * [Disaggregate_Wetcharts()] Calculates methane emissions for the wetland sector using wetcharts instead.
#' * [NWI_Wetland_fraction()] Calculates the fraction of wetland land cover by wetland type in each pixel.


SOCCR_Wetlands <- function(input_directory,
                           output_directory,
                           plot_directory,
                           state_name_list,
                           domain,
                           domain_template,
                           Use_SOCCR1,
                           Use_SOCCR2,
                           Include_freshwater,
                           Wetland_EFs,
                           verbose,
                           County_Tigerlines,
                           State_CB,
                           Source_Watershed_file,
                           Use_NLCD,
                           Use_NALCMS,
                           Use_Wetcharts,
                           Wetcharts_model_subset){
  
  ## Wetland_emissions_r2.R
  ## In use: 2021-11-02 20:00
  #
  # Load in the various state wetland fraction rasters
  # These overlap somewhat, so crop each to the squares within each state
  # Then add together and assign fluxes to each class
  
  
  # convert from g CH4 per m2 per yr to nmol/m2/s
  Wetland_EFs=Wetland_EFs*1E9/(16.043*365.25*24*60*60)      
  
  starttime <- Sys.time()
  cat("Starting wetland sector: SOCCR_Wetlands\n")
  
  Wetland_output_directory <- file.path(output_directory,"Wetlands")
  dir.create(Wetland_output_directory,showWarnings = F)
  
  ################################################################################
  #access the watersheds shapefile
  
  Watershed_file <- file.path(input_directory,"Watersheds_Shapefile/NA_Watersheds/data/watershed_p_v2.shp")
  
  if(Source_Watershed_file=="download"){
    dir.create(file.path(input_directory,"Watersheds"),showWarnings = F)
    #download the data.  URL is slightly different than the catalog.  See
    #https://www.cec.org/north-american-environmental-atlas/watersheds/
    data_URL <- paste0("https://www.cec.org/files/atlas_layers/0_reference/0_04_watersheds/watersheds_shapefile.zip")
    temp_out <- tempfile(fileext = ".zip")
    Trycatch_downloader(data_URL,output_location=temp_out,
                        method="save",
                        error_message=paste0("\nFailed to download watershed shapefile at URL",data_URL))
    utils::unzip(temp_out,,exdir = input_directory)
  }else if(Source_Watershed_file=="default"){
    #UPDATE TO ZENODO
  }else{
    invisible(file.copy(Source_Watershed_file,Watershed_file,overwrite = T))
  }
  
  ################################################################################
  #load in the watersheds and prepare for use with SOCCR2 EFs
  
  if(Use_SOCCR2){
    watershed <- terra::vect(Watershed_file)
    
    #we only care about NAW1 in English, so aggregate all polygons to this level
    #and remove extra data
    watershed <- watershed["NAW1_EN"]
    watershed <- terra::aggregate(watershed,by="NAW1_EN")
    watershed <- terra::crop(watershed,terra::project(domain_template,terra::crs(watershed))*1.1)
    expanded_watershed <- terra::buffer(watershed,2E4)
    watershed <- terra::aggregate(expanded_watershed-watershed+watershed,"NAW1_EN")
    watershed <- terra::project(watershed,terra::crs(domain))
    
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
    watershed <- terra::merge(watershed,regional_EFs,by.x="NAW1_EN",by.y="Region")
  }
  
  #update wetland EF to simplify SOCCR1 since SOCCR2 E2 and M2 have already been
  #dealt with.  SOCCR1 is not regional.
  Wetland_EFs_subset <- Wetland_EFs[,c("E2_Atlantic","M2_Atlantic","PFO","PNF","L1","L2","R1","R2","R3","R4")]
  colnames(Wetland_EFs_subset)[1:2] <- c("E2","M2")
  
  ################################################################################
  #load in and process the Wetland_fraction_r1 output to convert from wetland
  #coverage to wetland emissions
  
  NWI_files <- list.files(paste0(Wetland_output_directory,"/processed_NWI_data/"),".tiff",full.names = T)
  
  #just in case this is a rerun with more output here than the states being run
  #now
  NWI_files <- NWI_files[sapply(strsplit(basename(NWI_files),"_"),"[[",1) %in% state_name_list]
  
  
  NWI_filetypes <- sapply(strsplit(basename(gsub(".tiff","",(NWI_files))),"_"),"[[",2)
  
  SOCCR_wetland_types <- c("M2","E2","PFO","PNF")
  Freshwater_wetland_types <- c("R1","R2","R3","R4","L1","L2")
  
  #filter out any wetland types that we don't have within the domain
  SOCCR_wetland_types <- SOCCR_wetland_types[SOCCR_wetland_types %in% NWI_filetypes]
  Freshwater_wetland_types <- Freshwater_wetland_types[Freshwater_wetland_types %in% NWI_filetypes]
  
  #process separately for each type (different EFs)
  if(Use_SOCCR1 | Use_SOCCR2){
    #load in the first files to build the output
    subset_files <- NWI_files[grep(SOCCR_wetland_types[1],NWI_files)]
    subset_data <- terra::rast(subset_files)
    
    #given NWI extends somewhat beyond state bounds, there is overlap.  So max
    #should combine them akin to sum, but without double counting.
    subset_data <- max(subset_data)
    names(subset_data) <- SOCCR_wetland_types[1]
    
    #for a later sanity check
    all_frac <- subset_data
    if(Use_SOCCR1){
      SOCCR1_flux <- subset_data*Wetland_EFs_subset["SOCCR1",SOCCR_wetland_types[1]]
    }
    if(Use_SOCCR2){
      temp <- terra::rasterize(watershed,subset_data,field=SOCCR_wetland_types[1])
      SOCCR2_flux <- temp*subset_data
    }
    
    #repeat for all remaining states, now adding to all frac and combining as
    #new layers of soccr1_flux and soccr2_flux
    for(i in 2:length(SOCCR_wetland_types)){
      subset_files <- NWI_files[grep(SOCCR_wetland_types[i],NWI_files)]
      subset_data <- terra::rast(subset_files)
      subset_data <- max(subset_data)
      names(subset_data) <- SOCCR_wetland_types[i]
      all_frac <- all_frac+subset_data
      if(Use_SOCCR1){
        SOCCR1_flux <- c(SOCCR1_flux,subset_data*Wetland_EFs_subset["SOCCR1",SOCCR_wetland_types[i]]) 
      }
      if(Use_SOCCR2){
        temp <- terra::rasterize(watershed,subset_data,field=SOCCR_wetland_types[i])
        SOCCR2_flux <- c(SOCCR2_flux,temp*subset_data)
      }
    }
  }
  
  #repeat the process for freshwater
  subset_files <- NWI_files[grep(Freshwater_wetland_types[1],NWI_files)]
  subset_data <- terra::rast(subset_files)
  subset_data <- max(subset_data)
  names(subset_data) <- Freshwater_wetland_types[1]
  if(Use_SOCCR1 | Use_SOCCR2){
    all_frac <- all_frac+subset_data
  }else{
    all_frac <- subset_data
  }
  Freshwater_flux <- subset_data*Wetland_EFs_subset["SOCCR1",Freshwater_wetland_types[1]]
  
  for(i in 2:length(Freshwater_wetland_types)){
    subset_files <- NWI_files[grep(Freshwater_wetland_types[i],NWI_files)]
    subset_data <- terra::rast(subset_files)
    subset_data <- max(subset_data)
    all_frac <- all_frac+subset_data
    names(subset_data) <- Freshwater_wetland_types[i]
    Freshwater_flux <- c(Freshwater_flux,subset_data*Wetland_EFs_subset["SOCCR1",Freshwater_wetland_types[i]])
  }
  
  ################################################################################
  #save the output
  
  if(Use_SOCCR1){
    #crop/mask to exact domain and account for pixels partially within a
    #polygonal domain
    SOCCR1_flux <- terra::crop(SOCCR1_flux,domain_template)
    SOCCR1_flux <- terra::mask(SOCCR1_flux,domain)
    cover <- terra::extract(SOCCR1_flux,domain,weights=T,cells=T)
    SOCCR1_flux[cover[,'cell']] <- SOCCR1_flux[cover[,'cell']]*cover[,'weight']
    SOCCR1_flux <- sum(SOCCR1_flux,na.rm=T)
    writeCDF_no_newline(SOCCR1_flux,
                        file.path(Wetland_output_directory,'SOCCR1.nc'),
                        force_v4=TRUE,
                        varname='methane_emissions',
                        unit='nmol/m2/s',
                        longname='Methane emissions from National Wetland Inventory separated by classes, using fluxes from the SOCCR1 report',
                        missval=-9999,
                        overwrite=TRUE)
  }
  if(Use_SOCCR2){
    SOCCR2_flux <- terra::crop(SOCCR2_flux,domain_template)
    SOCCR2_flux <- terra::mask(SOCCR2_flux,domain)
    if(!Use_SOCCR1){
      cover <- terra::extract(SOCCR2_flux,domain,weights=T,cells=T)
    }
    SOCCR2_flux[cover[,'cell']] <- SOCCR2_flux[cover[,'cell']]*cover[,'weight']
    SOCCR2_flux <- sum(SOCCR2_flux,na.rm=T)
    writeCDF_no_newline(SOCCR2_flux,
                        file.path(Wetland_output_directory,'SOCCR2.nc'),
                        force_v4=TRUE,
                        varname='methane_emissions',
                        unit='nmol/m2/s',
                        longname='Methane emissions from National Wetland Inventory separated by classes, using fluxes from the SOCCR2 report',
                        missval=-9999,
                        overwrite=TRUE)
  }
  Freshwater_flux <- terra::crop(Freshwater_flux,domain_template)
  Freshwater_flux <- terra::mask(Freshwater_flux,domain)
  if(!(Use_SOCCR1 | Use_SOCCR2)){
    cover <- terra::extract(Freshwater_flux,domain,weights=T,cells=T)
  }
  Freshwater_flux[cover[,'cell']] <- Freshwater_flux[cover[,'cell']]*cover[,'weight']
  Freshwater_flux <- sum(Freshwater_flux,na.rm=T)
  writeCDF_no_newline(Freshwater_flux,
                      file.path(Wetland_output_directory,'Freshwater.nc'),
                      force_v4=TRUE,
                      varname='methane_emissions',
                      unit='nmol/m2/s',
                      longname='Methane emissions from National Wetland Inventory separated by classes, using the median flux from Rosentreter et al. (2021) for lakes and rivers',
                      missval=-9999,
                      overwrite=TRUE)
  
  ################################################################################
  #Create a sector total, 1 per variant.  This is the only reason this function
  #requires wetcharts variant info
  
  #start with freshwater (no variants)
  partial_total <- Freshwater_flux
  
  if(Use_Wetcharts){
    for(B in 1:length(Wetcharts_model_subset)){
      NLCD_Downscaled_Averaged_wetcharts <- terra::rast(file.path(Wetland_output_directory,paste0('Wetcharts_NLCD_Downscaled_subset_',B,'.nc')))
      writeCDF_no_newline(NLCD_Downscaled_Averaged_wetcharts+partial_total,
                          file.path(output_directory,paste0('Wetland_sector_total_Wetcharts_NLCD_subset_',B,'.nc')),
                          force_v4=TRUE,
                          varname='methane_emissions',
                          unit='nmol/m2/s',
                          longname='Methane emissions from wetlands and optionally freshwater',
                          missval=-9999,
                          overwrite=TRUE)
    }
  }
  if(Use_SOCCR1){
    writeCDF_no_newline(SOCCR1_flux+partial_total,
                        file.path(output_directory,paste0('Wetland_sector_total_SOCCR1.nc')),
                        force_v4=TRUE,
                        varname='methane_emissions',
                        unit='nmol/m2/s',
                        longname='Methane emissions from wetlands and optionally freshwater',
                        missval=-9999,
                        overwrite=TRUE)
  }
  if(Use_SOCCR2){
    writeCDF_no_newline(SOCCR2_flux+partial_total,
                        file.path(output_directory,paste0('Wetland_sector_total_SOCCR2.nc')),
                        force_v4=TRUE,
                        varname='methane_emissions',
                        unit='nmol/m2/s',
                        longname='Methane emissions from wetlands and optionally freshwater',
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
      if(!all(is.na(terra::values(SOCCR1_flux)))){
        zlim_max <- max(zlim_max,as.numeric(terra::global(SOCCR1_flux,max,na.rm=T)))
      }
    }
    if(Use_SOCCR2){
      if(!all(is.na(terra::values(SOCCR2_flux)))){
        zlim_max <- max(zlim_max,as.numeric(terra::global(SOCCR2_flux,max,na.rm=T)))
      }
    }
    if(!all(is.na(terra::values(Freshwater_flux)))){
      zlim_max <- max(zlim_max,as.numeric(terra::global(Freshwater_flux,max,na.rm=T)))
    }
    
    
    zlim_max <- log10(zlim_max)
    if(Use_SOCCR1){
      log_plot(input = SOCCR1_flux,
               zlim_min = zlim_min, zlim_max = zlim_max,
               filename = 'SOCCR1',
               title = paste0("SOCCR1 CH4\nSaturated colorscale low end"),
               plot_directory=plot_directory,
               domain=domain,County_Tigerlines=County_Tigerlines,
               State_CB=State_CB)
    }
    if(Use_SOCCR2){
      log_plot(input = SOCCR2_flux,
               zlim_min = zlim_min, zlim_max = zlim_max,
               filename = 'SOCCR2',
               title = paste0("SOCCR2 CH4\nSaturated colorscale low end"),
               plot_directory=plot_directory,
               domain=domain,County_Tigerlines=County_Tigerlines,
               State_CB=State_CB)
    }
    log_plot(input = Freshwater_flux,
             zlim_min = zlim_min, zlim_max = zlim_max,
             filename = 'Freshwater',
             title = paste0("Freshwater CH4\nSaturated colorscale low end"),
             plot_directory=plot_directory,
             domain=domain,County_Tigerlines=County_Tigerlines,
             State_CB=State_CB)
  }
  cat("Finished wetland sector: SOCCR_Wetlands in",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes\n\n")
}

