#'@title Pull gridded methane emissions maps for sectors not built with other
#'  functions from the Maasakkers gridded EPA inventory
#'
#'@description `Prepare_GEPA` writes 3 netcdf files of gridded methane emissions
#'  - 1 for thermogenic sectors, 1 for industrial landfills, and 1 for
#'  non-thermogenic sources.
#'
#'@details This function downloads the most appropriate year of gridded methane
#'  data at 0.1 deg by 0.1 deg from Maasakkers et al. and pulls sectors that
#'  have not been calculated in other scripts.  They are projected to the domain
#'  grid and combined into thermogenic and non-thermogenic groups, with the
#'  exception of industrial landfills which are saved separately.  For
#'  thermogenic this includes
#' \itemize{
#'   \item mobile combustion
#'   \item Coal (abandoned, surface, and underground)
#'   \item Petroleum Systems (exploration, production, refining, and transport)
#'   \item Abandoned Oil and Gas
#'   \item Natural Gas (exploration, processing, and production)
#'   \item Industry (petrochemical, and ferroalloy)
#'   }
#'  And for non-thermogenic this includes
#'   \itemize{
#'   \item Composting
#'   \item Enteric Fermentation
#'   \item Manure Management
#'   \item Rice Cultivation
#'   \item Field Burning
#'   }
#'   
#'  The data is available at \url{https://doi.org/10.5281/zenodo.8367082}.  The
#'  closest year will be automatically downloaded.
#'
#'  See reference \href{https://doi.org/10.1021/acs.est.3c05138}{Maasakkers et
#'  al.}
#'@param domain SpatVector polygon outlining the desired output area
#'@param domain_template SpatRaster providing the desired output grid, including
#'  the desired resolution and coordinate reference system
#'@param input_directory Character providing the full filepath to save/load
#'  input data
#'@param output_directory Character providing the full filepath to save
#'  processed data
#'@param inventory_year Numeric indicating the desired year of data to use.
#'@param verbose Logical indicating whether to save visuals.  It includes 3
#'  plots of the gridded methane emissions, 1 for each netcdf file.
#'@param County_Tigerlines SpatVector.  United States Census Bureau county
#'  shapefile downloaded in Main.
#'@param plot_directory Character providing the full filepath to save figures.
#'  Only relevant if verbose = TRUE.
#'@param State_CB SpatVector.  United States Census Bureau county
#'  shapefile.  Available at
#'  \url{https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html}.
#'  Only relevant if verbose=TRUE.
#'@returns Nothing is returned from the function, but the main outputs are 3
#'  netcdf files of the methane emissions from the gridded EPA product.  They
#'  are titled "GEPA_thermo.nc" for gridded EPA thermogenic,
#'  "GEPA_non_thermo.nc" for non-thermogenic, "GEPA_ind_landfill.nc" for
#'  industrial landfills.
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
#' Prepare_GEPA(inventory_year=2018,
#'              input_directory="~/../Desktop/in/",
#'              output_directory="~/../Desktop/out/",
#'              domain=grid_vect,
#'              domain_template=grid,
#'              verbose=TRUE,
#'              State_CB=vect("~/../Desktop/in/State_CB/tl_2018_us_state.shp"),
#'              County_Tigerlines=vect("~/../Desktop/in/County_Tigerlines/tl_2018_us_county.shp"),
#'              plot_directory="~/../Desktop/plots/")
#'@author Joe Pitt, \email{madeup@@wisc.edu}
#'@author Kris Hajny, \email{blank@@fake.edu}
#'@author Israel Lopez-Coto, \email{test@@test.edu}
#'@references \href{https://doi.org/10.1021/acs.est.3c05138}{Maasakkers et al.}
#'@export
#'@seealso [CH4_inventory_build()] Calculates methane inventory using settings provided in config.


#download the gridded epa inventory if it hasn't already been downloaded, then
#split into the components that we don't have in our inventory and save them to
#incorporate into the total easily later.

Prepare_GEPA <- function(inventory_year,
                         input_directory,
                         output_directory,
                         plot_directory,
                         County_Tigerlines,
                         State_CB,
                         domain,
                         domain_template,
                         verbose){
  
  starttime <- Sys.time()
  cat("Pulling remaining sectors from gridded EPA inventory: Prepare_GEPA\n")
  ################################################################################
  #Zenodo API to download the appropriate GEPA v2 file.
  #https://zenodo.org/records/8367082
  
  #Identify the files available
  File_list <- jsonlite::read_json("https://zenodo.org/api/records/8367082/files")
  File_list <- sort(sapply(File_list$entries,"[[","key"))
  
  #filter out monthly scale factors and sort for consistency
  year_list <- as.numeric(unique(gsub("\\.nc","",sapply(strsplit(File_list,"_"),utils::tail,1))))
  
  #find the best matching yr and alert if needed
  match_yr <- year_list[which.min(abs(year_list - inventory_year))]
  if(inventory_year!=match_yr){
    cat("GEPA does not include",inventory_year,"using",match_yr,"for GEPA as the nearest data available\n")
  }
  
  #subset to the right year
  File_choice <- File_list[grep(match_yr,File_list)]
  File_choice <- File_choice[!grepl("Scale_Factors",File_choice)]
  
  #2 files = Express extension, gridded product; 1 file = just express
  if(length(File_choice)==2){
    GEPA_filename <- File_choice[2]
    GEPA_URL <- paste0("https://zenodo.org/api/records/8367082/files/",GEPA_filename,"/content")
  }else if(length(File_choice)==1){
    GEPA_filename <- File_choice
    GEPA_URL <- paste0("https://zenodo.org/api/records/8367082/files/",GEPA_filename,"/content")
    cat("Using Express Extension - see https://zenodo.org/records/8367082 to understand the difference\n")
  }
  
  GEPA_file <- file.path(input_directory,GEPA_filename)
  
  if(Source_GEPA=="download"){
    Trycatch_downloader(URL=GEPA_URL,output_location=GEPA_file,method="save")
  }else if(Source_GEPA=="default"){
    #UPDATE TO ZENODO
  }else{
    invisible(file.copy(Source_GEPA,GEPA_file,overwrite = T))
  }

  ################################################################################
  #load in the file and split into the fossil fuel and non-fossil components we need
  
  GEPA <- terra::rast(GEPA_file)
  
  #convert units
  #molec/cm2/s to nmol/m2/s
  GEPA <- GEPA*(1e9*100^2)/(6.022141e+23)
  
  #aggregate/disaggregate to a similar resolution
  domain_trans <- terra::project(domain_template,terra::crs(GEPA))
  domain_res <- terra::res(domain_trans)
  if(any(domain_res<terra::res(GEPA))){
    #crop to the domain + buffer first to speed up process
    GEPA <- terra::crop(GEPA,terra::ext(domain_trans)*1.1,snap="out")
    GEPA <- terra::disagg(GEPA,round(terra::res(GEPA)/domain_res,3),"near")
    
    #reproject to exact domain now.  Here using nearest neighbor to prevent only
    #1 row/column of higher res pixels on the border of each GEPA pixel from
    #being interpolated.
    GEPA <- terra::project(GEPA,domain_template,method="near")
    GEPA <- terra::mask(GEPA,domain)
    
    #account for pixels partially within the domain
    cover <- terra::extract(GEPA[[1]],domain,weights=T,exact=T,cells=T)
    GEPA[cover[,'cell']] <- GEPA[cover[,'cell']]*cover[,'weight']
  }else if(any(domain_res>terra::res(GEPA))){
    GEPA <- terra::crop(GEPA,terra::project(domain,GEPA),snap="out")
    GEPA <- terra::mask(GEPA,terra::project(domain,GEPA),touches=T,updatevalue=0)
    cover <- terra::extract(GEPA,terra::project(domain,GEPA),weights=T,exact=T,cells=T)
    GEPA[cover[,'cell']] <- GEPA[cover[,'cell']]*cover[,'weight']
    GEPA=terra::extend(GEPA,fill=0,
                       terra::ext(GEPA)+(terra::res(terra::project(domain_template,terra::crs(GEPA)))*5))
    
    #reproject to exact domain now using an average to effectively aggregate
    #while reprojecting instead.
    GEPA <- terra::project(GEPA,domain_template,method="average")
  }
  
  GEPA_non_thermo_sectors <- c("emi_ch4_5B1_Composting",
                               "emi_ch4_3A_Enteric_Fermentation",
                               "emi_ch4_3B_Manure_Management",
                               "emi_ch4_3C_Rice_Cultivation",
                               "emi_ch4_3F_Field_Burning")
  GEPA_thermo_sectors <- c("emi_ch4_1A_Combustion_Mobile",
                           "emi_ch4_1B1a_Abandoned_Coal",
                           "emi_ch4_1B1a_Surface_Coal",
                           "emi_ch4_1B1a_Underground_Coal",
                           "emi_ch4_1B2a_Petroleum_Systems_Exploration",
                           "emi_ch4_1B2a_Petroleum_Systems_Production",
                           "emi_ch4_1B2a_Petroleum_Systems_Refining",
                           "emi_ch4_1B2a_Petroleum_Systems_Transport",
                           "emi_ch4_1B2ab_Abandoned_Oil_Gas",
                           "emi_ch4_1B2b_Natural_Gas_Exploration",
                           "emi_ch4_1B2b_Natural_Gas_Processing",
                           "emi_ch4_1B2b_Natural_Gas_Production",
                           "emi_ch4_2B8_Industry_Petrochemical",
                           "emi_ch4_2C2_Industry_Ferroalloy")
  
  
  #subset to the 3 types of GEPA data we need
  GEPA_landfill <- GEPA$emi_ch4_5A1_Landfills_Industrial
  GEPA_non_thermo <- GEPA[[which(names(GEPA) %in% GEPA_non_thermo_sectors)]]
  GEPA_thermo <- GEPA[[which(names(GEPA) %in% GEPA_thermo_sectors)]]

  #sum across layers for those that are multiple individual sectors
  GEPA_non_thermo <- sum(GEPA_non_thermo)
  GEPA_thermo <- sum(GEPA_thermo)
  
  ################################################################################
  #Save the output
  
  writeCDF_no_newline(GEPA_landfill,
                      file.path(output_directory,'GEPA_ind_landfill.nc'),
                      force_v4=TRUE,
                      varname='methane_emissions',
                      unit='nmol/m2/s',
                      longname=paste0(gsub("_"," ",gsub(".nc","",GEPA_filename))," industrial landfills"),
                      missval=-9999,
                      overwrite=TRUE)
  writeCDF_no_newline(GEPA_non_thermo,
                      file.path(output_directory,'GEPA_non_thermo.nc'),
                      force_v4=TRUE,
                      varname='methane_emissions',
                      unit='nmol/m2/s',
                      longname=paste0(gsub("_"," ",gsub(".nc","",GEPA_filename))," enteric fermentation, manure management, rice cultivation, field burning, and composting"),
                      missval=-9999,
                      overwrite=TRUE)
  writeCDF_no_newline(GEPA_thermo,
                      file.path(output_directory,'GEPA_thermo.nc'),
                      force_v4=TRUE,
                      varname='methane_emissions',
                      unit='nmol/m2/s',
                      longname=paste0(gsub("_"," ",gsub(".nc","",GEPA_filename))," mobile combustion, coal, petroleum, abandoned oil and gas, natural gas exploration processing and production, petrochemicals, and ferroalloy"),
                      missval=-9999,
                      overwrite=TRUE)
  
  ################################################################################
  #Plots
  
  if(verbose){
    zlim_min <- min(terra::global(GEPA_landfill,min,na.rm=T),terra::global(GEPA_non_thermo,min,na.rm=T),terra::global(GEPA_thermo,min,na.rm=T))
    zlim_max <- max(terra::global(GEPA_landfill,max,na.rm=T),terra::global(GEPA_non_thermo,max,na.rm=T),terra::global(GEPA_thermo,max,na.rm=T))
    not_log_plot(GEPA_landfill,filename="GEPA_industrial_landfills",
                 "Gridded EPA Inventory -\nIndustrial landfills",
                 zlim_min=zlim_min,zlim_max=zlim_max,
                 plot_directory=plot_directory,
                 domain=domain,County_Tigerlines=County_Tigerlines,
                 State_CB=State_CB)
    not_log_plot(GEPA_thermo,filename="GEPA_inventory_thermogenic_subset",
                 "Gridded EPA Inventory -\nThermogenic Sectors",
                 zlim_min=zlim_min,zlim_max=zlim_max,
                 plot_directory=plot_directory,
                 domain=domain,County_Tigerlines=County_Tigerlines,
                 State_CB=State_CB)
    not_log_plot(GEPA_non_thermo,filename="GEPA_inventory_non_thermogenic_subset",
                 "Gridded EPA Inventory -\nNon-thermogenic Sectors",
                 zlim_min=zlim_min,zlim_max=zlim_max,
                 plot_directory=plot_directory,
                 domain=domain,County_Tigerlines=County_Tigerlines,
                 State_CB=State_CB)
    
    Summed_GEPA_inventory_subset <- sum(GEPA_landfill,GEPA_non_thermo,GEPA_thermo,na.rm=T)
    not_log_plot(Summed_GEPA_inventory_subset,
                 "Gridded EPA Inventory -\nOnly sectors used",
                 plot_directory=plot_directory,
                 domain=domain,County_Tigerlines=County_Tigerlines,
                 State_CB=State_CB)
    
    
    
    Summed_GEPA_saturated <- sum(GEPA,na.rm=T)
    not_log_plot(Summed_GEPA_saturated,
                 "Gridded EPA Inventory - \nAll sectors, saturated colorscale high end",
                 plot_directory=plot_directory,
                 zlim_min=as.numeric(terra::global(Summed_GEPA_inventory_subset,min,na.rm=T)),
                 zlim_max=as.numeric(terra::global(Summed_GEPA_inventory_subset,max,na.rm=T)),
                 domain=domain,County_Tigerlines=County_Tigerlines,
                 State_CB=State_CB)
    
    Summed_GEPA <- Summed_GEPA_saturated
    log_plot(Summed_GEPA,
             "Gridded EPA Inventory - \nAll sectors\nSaturated low end",
             plot_directory=plot_directory,
             domain=domain,County_Tigerlines=County_Tigerlines,
             zlim_min=-4,
             State_CB=State_CB)
  }
  cat("Finished pulling remaining sectors from gridded EPA inventory: Prepare_GEPA in",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes\n\n")
}

