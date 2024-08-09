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
#'                focus_city_tigerlines=focus_city)
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
                           focus_city_tigerlines){
  
  ## Wetland_emissions_r2.R
  ## In use: 2021-11-02 20:00
  #
  # Load in the various state wetland fraction rasters
  # These overlap somewhat, so crop each to the squares within each state
  # Then add together and assign fluxes to each class
  
  
  ################################################################################
  #load in and process the Wetland_fraction_r1 output to convert from wetland
  #coverage to wetland emissions
  
  NWI_files <- list.files(paste0(output_directory,"/NWI/"),".tiff",full.names = T)
  
  
  Wetland_types <- vector()
  if(Use_SOCCR1 | Use_SOCCR2){
    Wetland_types <- c(Wetland_types,"M2","E2","PFO","PNF")
  }
  if(Include_freshwater){
    Wetland_types <- c(Wetland_types,"R1","R2","R3","R4","L1","L2")
  }
  
  
  #process separately for each type (different EFs)
  for(i in 1:length(Wetland_types)){
    subset_files <- NWI_files[grep(Wetland_types[i],NWI_files)]
    subset_data <- rast(subset_files)
    #given NWI extends somewhat beyond state bounds, there is overlap.  So max
    #should combine them akin to sum, but without double counting.
    subset_data <- max(subset_data)
    names(subset_data) <- Wetland_types[i]
    
    if(i==1){
      if(Use_SOCCR1 | Include_freshwater){
        SOCCR1_flux <- subset_data*Wetland_EFs["SOCCR1",Wetland_types[i]]
      }
      if(Use_SOCCR2){
        SOCCR2_flux <- subset_data*Wetland_EFs["SOCCR2",Wetland_types[i]]
      }
      #for a later sanity check
      all_frac <- subset_data
    }else{
      if(Use_SOCCR1 | Include_freshwater){
        SOCCR1_flux <- c(SOCCR1_flux,subset_data*Wetland_EFs["SOCCR1",Wetland_types[i]])
      }
      if(Use_SOCCR2){
        SOCCR2_flux <- c(SOCCR2_flux,subset_data*Wetland_EFs["SOCCR2",Wetland_types[i]])
      }
      all_frac <- all_frac+subset_data
    }
  }
  
  # Check that the fractions are always between 0 and 1
  max_frac <- unlist(global(all_frac,max))*100
  min_frac <- unlist(global(all_frac,min))*100
  if((Use_SOCCR1 | Use_SOCCR2) & Include_freshwater){
    cat("total SOCCR-relevant + freshwater wetland area per pixel ranges from",min_frac,"to",max_frac,"%\n")
  }else if(Use_SOCCR1 | Use_SOCCR2){
    cat("total SOCCR-relevant wetland area per pixel ranges from",min_frac,"to",max_frac,"%\n")
  }else if(Include_freshwater){
    cat("total freshwater wetland area per pixel ranges from",min_frac,"to",max_frac,"%\n")
  }
  
  ################################################################################
  #save the output
  
  if(Use_SOCCR1 | Include_freshwater){
    SOCCR1_flux <- crop(SOCCR1_flux,ext(domain))
    writeCDF(SOCCR1_flux,
             file.path(output_directory,'SOCCR1.nc'),
             force_v4=TRUE,
             varname='methane_emissions',
             unit='nmol/m2/s',
             longname='Methane emissions from National Wetland Inventory separated by classes, using fluxes from the SOCCR1 report and the median flux from Rosentreter et al. (2021) for lakes and rivers',
             missval=-9999,
             overwrite=TRUE)
  }
  if(Use_SOCCR2){
    SOCCR2_flux <- crop(SOCCR2_flux,ext(domain))
    writeCDF(SOCCR2_flux,
             file.path(output_directory,'SOCCR2.nc'),
             force_v4=TRUE,
             varname='methane_emissions',
             unit='nmol/m2/s',
             longname='Methane emissions from National Wetland Inventory separated by classes, using fluxes from the SOCCR2 report and the median flux from Rosentreter et al. (2021) for lakes and rivers',
             missval=-9999,
             overwrite=TRUE)
  }
  ################################################################################
  #visuals
  
  if(verbose){
    #the minimum is a ~arbitrary value given the log scale can go quite negative.
    zlim_min <- -3
    if(Use_SOCCR1 & Use_SOCCR2){
      SOCCR1_flux <- sum(subset(SOCCR1_flux,c("M2","E2","PFO","PNF")))
      SOCCR2_flux <- sum(subset(SOCCR2_flux,c("M2","E2","PFO","PNF")))
      zlim_max <- log10(max(global(SOCCR1_flux,max),global(SOCCR2_flux,max)))
      log_plot(input = SOCCR1_flux,
               zlim_min = zlim_min, zlim_max = zlim_max,
               filename = 'SOCCR1',
               title = paste0("SOCCR1 CH4\nlog10(nmol/m2s), Saturated colorscale low end"))
      log_plot(input = SOCCR2_flux,
               zlim_min = zlim_min, zlim_max = zlim_max,
               filename = 'SOCCR2',
               title = paste0("SOCCR2 CH4\nlog10(nmol/m2s), Saturated colorscale low end"))
    }else if(Use_SOCCR1){
      SOCCR1_flux <- sum(subset(SOCCR1_flux,c("M2","E2","PFO","PNF")))
      zlim_max <- log10(global(SOCCR1_flux,max))
      log_plot(input = SOCCR1_flux,
               zlim_min = zlim_min, zlim_max = zlim_max,
               filename = 'SOCCR1',
               title = paste0("SOCCR1 CH4\nlog10(nmol/m2s), Saturated colorscale low end"))
    }else if(Use_SOCCR2){
      SOCCR2_flux <- sum(subset(SOCCR2_flux,c("M2","E2","PFO","PNF")))
      zlim_max <- log10(global(SOCCR2_flux,max))
      log_plot(input = SOCCR2_flux,
               zlim_min = zlim_min, zlim_max = zlim_max,
               filename = 'SOCCR2',
               title = paste0("SOCCR2 CH4\nlog10(nmol/m2s), Saturated colorscale low end"))
    }
  }
}