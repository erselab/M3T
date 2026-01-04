#'@title Create gridded wetland coverage maps using National Wetlands Inventory
#'  data
#'
#'@description `NWI_Wetland_fraction` writes multiple tiff files of the
#'  fractional wetland cover in each pixel.  There is one file for each state -
#'  wetland type combination.
#'
#'@details This function downloads National Wetlands Inventory (NWI) data and
#'  calculates the fraction of each gridcell in the domain that is covered by
#'  the NWI polygons.  This is done separately for each state as the data is
#'  available by state or watershed.  It also processes each type of wetland
#'  separately.
#'
#'  The wetland types are pulled from the NWI system, subsystem, and class as
#'  defined here
#'  \url{https://www.fws.gov/media/national-wetland-inventory-wetlands-and-deepwater-map-code-diagram}
#'  and discussed in detail here
#'  \url{https://www.fws.gov/media/classification-wetlands-and-deepwater-habitats-united-states}.
#'  They are M1 (marine, subtidal), M2 (marine, intertidal), E1 (estuarine,
#'  subtidal), E2 (estuarine, intertidal), R1 (riverine, tidal), R2 (riverine,
#'  lower perrennial), R3 (riverine, upper perennial), R4 (riverine,
#'  intermittent), L1 (lacustrine, limnetic), L2 (lacustrine, littoral), PFO
#'  (palustrine, forested), and PNF (palustrine, all non-forested classes).
#'
#'  The NWI data for each state will be automatically downloaded.  This can be
#'  up to several GB per state.
#'
#'  Given the high resolution polygons and significant number being processed,
#'  this function can be time consuming.
#'
#'  The NWI is a biannually updated dataset including the extent and type of
#'  wetlands across the United States.  It is available at
#'  \url{https://www.fws.gov/program/national-wetlands-inventory/wetlands-data.}
#'
#'  A separate TIFF file is saved for each state - wetland type combination.
#'  These are used in further processing.
#'@param domain SpatVector polygon outlining the desired output area
#'@param domain_template SpatRaster providing the desired output grid, including
#'  the desired resolution and coordinate reference system
#'@param state_name_list Character vector listing all states within the desired
#'  domain
#'@param input_directory Character providing the full filepath to save/load
#'  input data
#'@param output_directory Character providing the full filepath to save
#'  processed data
#'@param Use_SOCCR1 Logical.  Pulled from config file.  Indicating whether or
#'  not to calculate emissions using SOCCR1.
#'@param Use_SOCCR2 Logical.  Pulled from config file.  Indicating whether or
#'  not to calculate emissions using SOCCR2.
#'@param Include_freshwater Logical.  Pulled from config file.  Indicating
#'  whether or not to calculate emissions for freshwater wetlands using
#'  Rosentreter et al.
#'@returns Nothing is returned from the function, but the main outputs are TIFF
#'  files of the fractional wetland coverage per pixel for each wetland type and
#'  state.  They are titled "state abbreviation _ wetland type
#'  abbreviation.tiff".
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
#'
#' NWI_Wetland_fraction(input_directory="~/../Desktop/in/",
#'                      output_directory="~/../Desktop/out/",
#'                      Use_SOCCR1=TRUE,
#'                      Use_SOCCR2=TRUE,
#'                      Include_freshwater=TRUE,
#'                      domain=grid_vect,
#'                      domain_template=grid,
#'                      state_name_list=c("DE","MD","NJ","NY","PA"))
#'@author Joe Pitt, \email{madeup@@wisc.edu}
#'@author Kris Hajny, \email{blank@@fake.edu}
#'@author Israel Lopez-Coto, \email{test@@test.edu}
#'@export
#'@seealso 
#' * [CH4_inventory_build()] Calculates methane inventory using settings provided in config.
#' * [SOCCR_Wetlands()] Calculates methane emissions for the wetland sector using the state of the carbon cycle report.

NWI_Wetland_fraction <- function(input_directory,
                                 output_directory,
                                 domain,
                                 domain_template,
                                 state_name_list,
                                 Use_SOCCR1,
                                 Use_SOCCR2){
  
  # Calculate wetland fraction using NWI state wetland cover data
  
  starttime <- Sys.time()
  cat("Starting wetland sector: NWI_Wetland_fraction\n")
  
  NWI_output_directory <- file.path(output_directory,"Wetlands","processed_NWI_data")
  dir.create(NWI_output_directory,showWarnings = F,recursive = T)
  ################################################################################
  #Some settings
  
  #URL base to download NWI data if not already downloaded
  NWI_url <- "https://documentst.ecosphere.fws.gov/wetlands/data/State-Downloads/"
  
  ################################################################################
  #create input folder for raw NWI data
  
  NWI_input_directory <- file.path(input_directory,"NWI")
  dir.create(NWI_input_directory,showWarnings = F)
  
  ################################################################################
  #Create a function that will reproject to the proper CRS and then rasterize,
  #and account for invalid polygons if they exist
  
  rasterize_plus <- function(input){
    #pull the name of input for later, have to do so before editing input within
    #the function in any way
    input_name <- substitute(input)
    if(paste0(state_name_list[i],'_',input_name,'.tiff') %in% Processed_NWI_files){
      cat(input_name,"already processed for",state_name_list[i],"\n")
    }else{
      cat("Starting",input_name,"for",state_name_list[i],"at",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes since start\n")
      
      #identify any invalid polygons (errors such as too few vertices or
      #self-intersection).  Has to be done in multiple steps or it doesn't seem to
      #work.
      invalid_polygons <- !terra::is.valid(input)
      if(any(invalid_polygons)){
        valid_polygons <- input[!invalid_polygons]
        invalid_polygons <- input[invalid_polygons]
        invalid_polygons <- terra::makeValid(invalid_polygons)
        if(all(terra::is.valid(invalid_polygons))){
          #if successful, just recombine
          validated_input <- terra::vect(c(valid_polygons,invalid_polygons))
        }else{
          stop('failed to fix some polygons')
        }
      }else{
        validated_input <- input
      }
      
      #rasterize cover=true is significantly faster than extract
      output_rast <- terra::rasterize(validated_input, state_template, fun=sum,
                                      cover=TRUE)
      output_rast[is.na(output_rast)] <- 0
      
      #extend to the domain + some buffer, crop out any other excess and save
      output_rast <- terra::extend(output_rast,terra::ext(domain)+terra::res(domain_template)*20,fill=0)
      output_rast <- terra::crop(output_rast,terra::ext(domain)+terra::res(domain_template)*20)
      terra::writeRaster(output_rast,
                         file.path(NWI_output_directory,paste0(state_name_list[i],'_',input_name,'.tiff')),
                         overwrite=T)
    }
  }
  ################################################################################
  #load in the wetlands files
  
  #first check which have already been downloaded if any (note MN is special).
  input_directory_data <- list.files(NWI_input_directory)
  Downloaded_NWI_files <- paste0(state_name_list,"_Wetlands_Geopackage.gpkg") %in% input_directory_data
  Downloaded_NWI_files[state_name_list=="MN"] <- "MN_geodatabase_wetlands" %in% input_directory_data
  
  #check those that have already been processed
  Processed_NWI_files <- list.files(NWI_output_directory,pattern=".tiff")
  
  for(i in 1:length(state_name_list)){
    cat("Starting processing for",state_name_list[i],"at",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes since start\n")
    
    #filename on the NWI website
    if(state_name_list[i]=="MN"){
      NWI_filename <- "MN_geodatabase_wetlands.zip"
    }else{
      NWI_filename <- paste0(state_name_list[i],"_geopackage_wetlands.zip")
    }
    
    #download any not already downloaded
    if(!Downloaded_NWI_files[i]){
      data_URL <- paste0(NWI_url,NWI_filename)
      NWI_full_filename <- file.path(NWI_input_directory,NWI_filename)
      
      # Wetlands inventory has a shapefile for each state - download them, retry
      # if failed (e.g., intermittent internet)
      Trycatch_downloader(URL=data_URL,
                          output_location=NWI_full_filename,
                          method="save",
                          error_message=paste0("\nFailed to download National Wetland Inventory data using url: ",data_URL))
      
      #unzip the downloaded file and delete the zip file
      utils::unzip(NWI_full_filename,
                   exdir=NWI_input_directory)
      unlink(NWI_full_filename, recursive=TRUE)
      
      #in some cases, the file name in the zip differs.  Force it to match
      #expected.
      new_file <- list.files(NWI_input_directory,full.names = T)[grep(state_name_list[i],list.files(NWI_input_directory,full.names = T))]
      file.rename(new_file,file.path(NWI_input_directory,paste0(state_name_list[i],"_Wetlands_Geopackage.gpkg")))
    }
    
    #The filename switches here from the .zip to the unzipped filename
    if(state_name_list[i]=="MN"){
      NWI_full_filename <- file.path(NWI_input_directory,"MN_geodatabase_wetlands.gdb")
      wetlands <- terra::vect(NWI_full_filename,layer="MN_wetlands")
      wetlands <- wetlands[,"ATTRIBUTE"]
    }else{
      NWI_full_filename <- file.path(NWI_input_directory,paste0(state_name_list[i],"_Wetlands_Geopackage.gpkg"))
      
      #load and subset to just the "attribute" variable that provides the wetland
      #type and subtype (we won't need other variables).
      wetlands <- terra::vect(NWI_full_filename,layer=paste0(state_name_list[i],"_Wetlands"))
      wetlands <- wetlands[,"ATTRIBUTE"]
    }
    
    cat("Finished loading and combining all wetland files at",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes since start\n")
    ################################################################################
    #split them into the relevant parts and rasterize/save them
    
    #reproject and create a template for output, slightly larger than the
    #wetland shapefile to avoid losing any data
    wetlands <- terra::project(wetlands,terra::crs(domain))
    state_template <- domain_template
    terra::values(state_template) <- 0
    state_template <- terra::extend(state_template,terra::ext(wetlands)+0.5,snap="out")
    
    #split the wetlands file into the relevant wetland types
    Attribute_text <- wetlands$ATTRIBUTE
    #freshwater
    R1 <- wetlands[startsWith(Attribute_text, 'R1'),]
    R2 <- wetlands[startsWith(Attribute_text, 'R2'),]
    R3 <- wetlands[startsWith(Attribute_text, 'R3'),]
    R4 <- wetlands[startsWith(Attribute_text, 'R4'),]
    L1 <- wetlands[startsWith(Attribute_text, 'L1'),]
    L2 <- wetlands[startsWith(Attribute_text, 'L2'),]
    if(Use_SOCCR1 | Use_SOCCR2){
      M2 <- wetlands[startsWith(Attribute_text, 'M2'),]
      E2 <- wetlands[startsWith(Attribute_text, 'E2'),]
      PFO <- wetlands[startsWith(Attribute_text, 'PFO'),]
      PNF <- wetlands[startsWith(Attribute_text, 'P')&!startsWith(Attribute_text, 'PFO'),]
    }
    
    # if there is some of category R1
    if(dim(R1)[1]!=0){
      rasterize_plus(R1)
    }
    
    if(dim(R2)[1]!=0){
      rasterize_plus(R2)
    }
    
    if(dim(R3)[1]!=0){
      rasterize_plus(R3)
    }
    
    if(dim(R4)[1]!=0){
      rasterize_plus(R4)
    }
    
    if(dim(L1)[1]!=0){
      rasterize_plus(L1)
    }
    
    if(dim(L2)[1]!=0){
      rasterize_plus(L2)
    }
    if(Use_SOCCR1 | Use_SOCCR2){
      if(dim(M2)[1]!=0){
        rasterize_plus(M2)
      }
      
      if(dim(E2)[1]!=0){
        rasterize_plus(E2)
      }
      
      if(dim(PFO)[1]!=0){
        rasterize_plus(PFO)
      }
      
      if(dim(PNF)[1]!=0){
        rasterize_plus(PNF)
      }
    }
    #minor - just have one less newline at the end if this is the last one
    if(i==length(state_name_list)){
      cat("Finished processing",state_name_list[i],"which is",i,"of",length(state_name_list),"at",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes since start\n")
    }else{
      cat("Finished processing",state_name_list[i],"which is",i,"of",length(state_name_list),"at",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes since start\n\n")
    }
  }
  cat("Finished wetland sector: NWI_Wetland_fraction in",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes\n\n")
}