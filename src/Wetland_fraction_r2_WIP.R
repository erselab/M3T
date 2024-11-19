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
#'  The NWI is a biannually updated dataset including the extent and type of
#'  wetlands across the United States.  It is available at
#'  \url{https://www.fws.gov/program/national-wetlands-inventory/wetlands-data.}
#'
#'  A separate TIFF file is saved for each state - wetland type combination.
#'  These are used in further processing.
#'@param domain SpatRaster providing the desired output grid, including the
#'  desired resolution and coordinate reference system
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
#'             ncols=diff(range(grid_bbox[,1]))/grid_res, xmin=min(grid_bbox[,1]),
#'             xmax=max(grid_bbox[,1]), ymin=min(grid_bbox[,2]), ymax=max(grid_bbox[,2]),
#'             crs=grid_crs)
#'
#' NWI_Wetland_fraction(input_directory="~/../Desktop/input/",
#'                      output_directory="~/../Desktop/",
#'                      Use_SOCCR1=TRUE,
#'                      Use_SOCCR2=TRUE,
#'                      Include_freshwater=TRUE,
#'                      domain=grid,
#'                      state_name_list=c("DE","MD","NJ","NY","PA"))
#'@author Joe Pitt, \email{madeup@@wisc.edu}
#'@author Kris Hajny, \email{blank@@fake.edu}
#'@author Israel Lopez-Coto, \email{test@@test.edu}
#'@export

NWI_Wetland_fraction <- function(input_directory,
                                 output_directory,
                                 domain,
                                 domain_template,
                                 state_name_list,
                                 Use_SOCCR1,
                                 Use_SOCCR2,
                                 Include_freshwater){
  
  # Calculate wetland fraction using NWI state wetland cover data

  starttime <- Sys.time()
  cat("Starting wetland sector: NWI_Wetland_fraction\n")
  ################################################################################
  #Some settings
  
  #URL base to download NWI data if not already downloaded
  NWI_url <- "https://documentst.ecosphere.fws.gov/wetlands/data/State-Downloads/"
  
  #these are larger files, so set the download timeout to 20 minutes (default is 1
  #min)
  options(timeout = 60*20)
  
  ################################################################################
  #create output folders (multiple nested folders will be created via download)
  
  dir.create(paste0(input_directory,"NWI"),showWarnings = F)
  dir.create(paste0(output_directory,"NWI"),showWarnings = F)
  
  NWI_input_directory <- paste0(input_directory,"NWI/")
  NWI_output_directory <- paste0(output_directory,"NWI/")
  
  ################################################################################
  #Create a function that will reproject to the proper CRS and then rasterize,
  #potentially account for invalid polygons 
  
  rasterize_plus <- function(input){
    #need for naming - substitute only works if it's pulled from elsewhere.  Once
    #created within the function, it will be ~= get.
    input_name <- substitute(input)
    if(paste0(state_name_list[i],'_',input_name,'.tiff') %in% Processed_NWI_files){
      cat(input_name,"already processed for",state_name_list[i],"\n")
    }else{
      cat("Starting",input_name,"for",state_name_list[i],"at",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes since start\n")
      # if(paste0(state_name_list[i],'_',input_name,".tiff") %in% Processed_NWI_files){
      #   #if already processed, stop the function without error
      #   return(NULL)
      # }
      #identify any invalid polygons (errors such as too few vertices or
      #self-intersection).  Has to be done in multiple steps or it doesn't seem to
      #work.
      invalid_polygons <- !is.valid(input)
      if(any(invalid_polygons)){
        valid_polygons <- input[!invalid_polygons]
        invalid_polygons <- input[invalid_polygons]
        invalid_polygons <- makeValid(invalid_polygons)
        if(all(is.valid(invalid_polygons))){
          #if successful, just recombine
          validated_input <- vect(c(valid_polygons,invalid_polygons))
        }else{
          stop('failed to fix some polygons')
        }
      }else{
        validated_input <- input
      }
      
      # #Aggregate to dissolve polygons that are inside others and convert to 1
      # #overall polygon instead.  Avoids excessive rounding error when combining
      # #extract results from many polygons.
      # validated_input <- aggregate(validated_input)
      
      #below is less than 7E-18 delta compared to for loop equivalent.  But it takes
      #~10 seconds compared to 2.6 minutes (for WV R4, 14,853 geometries)
      
      #calculate fractional coverage and save these weights to a raster.  Sum across
      #all polygons for a given raster cell (they all represent the same wetland
      #type after all)
      cover <- extract(state_template,validated_input,weights=T,exact=T,cells=T)
      cover <- aggregate(cover$weight,by=list(cover$cell),FUN=sum)
      colnames(cover) <- c("cell","weight")
      output_rast=state_template
      output_rast[cover[,'cell']] <- cover[,'weight']
      output_rast[is.na(output_rast)] <- 0
      
      #extend to the domain + some buffer, crop out any other excess and save
      output_rast <- extend(output_rast,ext(domain)+res(domain_template)*5,fill=0)
      output_rast <- crop(output_rast,ext(domain)+res(domain_template)*5)
      writeRaster(output_rast,
                  paste0(NWI_output_directory,state_name_list[i],'_',input_name,'.tiff'),
                  overwrite=T)
    }
  }
  ################################################################################
  #load in the wetlands files
  
  input_directory_data <- list.files(NWI_input_directory)
  Downloaded_NWI_files <- paste0(state_name_list,"_Wetlands_Geopackage.gpkg") %in% input_directory_data
  Downloaded_NWI_files[state_name_list=="MN"] <- "MN_geodatabase_wetlands" %in% input_directory_data
  # Downloaded_NWI_files <- paste0(state_name_list,"_shapefile_wetlands") %in% input_directory_data
  Processed_NWI_files <- list.files(NWI_output_directory,pattern=".tiff")

  for(i in 1:length(state_name_list)){
    cat("Starting processing for",state_name_list[i],"at",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes since start\n")

    # if(state_name_list[i] %in% Processed_NWI_files){
    #   #if already processed, move on to the next state.
    #   cat("Already processed\n")
    #   next
    # }
    
    #filename on the NWI website
    # NWI_filename <- paste0(state_name_list[i],"_shapefile_wetlands.zip")
    if(state_name_list[i]=="MN"){
      NWI_filename <- "MN_geodatabase_wetlands.zip"
    }else{
      NWI_filename <- paste0(state_name_list[i],"_geopackage_wetlands.zip")
    }
    if(!Downloaded_NWI_files[i]){
      data_URL <- paste0(NWI_url,NWI_filename)
      NWI_full_filename <- file.path(NWI_input_directory,NWI_filename)
      
      # Wetlands inventory has a shapefile for each state - download them, retry
      # if failed (e.g., intermittent internet)
      
      Trycatch_downloader(URL=data_URL,
                          output_location=NWI_full_filename,
                          method="FTP",
                          error_message=paste0("\nFailed to download National Wetland Inventory data using url: ",data_URL))
      
      #unzip the downloaded file and delete the zip file
      unzip(NWI_full_filename,
            exdir=NWI_input_directory)
      unlink(NWI_full_filename, recursive=TRUE)
    }
    
    # NWI_filename <- gsub(".zip","",NWI_filename)
    
    #The filename switches here from state gpkg wetlands to state wetalnds gpkg
    if(state_name_list[i]=="MN"){
      NWI_full_filename <- file.path(NWI_input_directory,"MN_geodatabase_wetlands.gdb")
      wetlands <- vect(NWI_full_filename,layer="MN_wetlands")
      wetlands <- wetlands[,"ATTRIBUTE"]
    }else{
      NWI_full_filename <- file.path(NWI_input_directory,paste0(state_name_list[i],"_Wetlands_Geopackage.gpkg"))
      
      #VA has a layer named "VA_Wetlands_Project_Metdata instead of Met A data,
      #so this failed.  This was needed for the shapefiles version anyway where
      #some states had multiple layers that needed to be combined, this does not
      #appear to be the case for geopackage files.
      
      # file_layers <- vector_layers(NWI_full_filename)
      # file_layers <- file_layers[!grepl("Historic",file_layers)]
      # file_layers <- file_layers[!grepl("Metadata",file_layers)]
      # file_layers <- file_layers[grepl("Wetlands",file_layers)]
      # wetlands <- vect(NWI_full_filename,layer=file_layers)
      
      #load and subset to just the "attribute" variable that provides the wetland
      #type and subtype (we won't need other variables).  Combine the multiple
      #files via rbind if needed
      wetlands <- vect(NWI_full_filename,layer=paste0(state_name_list[i],"_Wetlands"))
      wetlands <- wetlands[,"ATTRIBUTE"]
    }
    
    #   #progressively remove files to isolate just the relevant wetland files. some
    #   #have historic files, there's metadata, shape outline files, some states have
    #   #other types (e.g. MO riparian), etc.
    #   wetland_files <- list.files(file.path(NWI_input_directory,NWI_filename),pattern=glob2rx("*.shp"),full.names = T)
    #   wetland_files <- wetland_files[!grepl("Historic",wetland_files)]
    #   wetland_files <- wetland_files[!grepl("Metadata",wetland_files)]
    #   wetland_files <- wetland_files[grepl("Wetlands",wetland_files)]
    #   
    #   #load and subset to just the "attribute" variable that provides the wetland
    #   #type and subtype (we won't need other variables).  Combine the multiple
    #   #files via rbind if needed
    #   if(length(wetland_files)>1){
    #     wetlands <- vect(wetland_files[1])
    #     wetlands <- wetlands[,"ATTRIBUTE"]
    #     for(A in 2:length(wetland_files)){
    #       additional_wetlands <- vect(wetland_files[A])
    #       additional_wetlands <- additional_wetlands[,"ATTRIBUTE"]
    #       wetlands <- rbind(wetlands,additional_wetlands)
    #     }
    #   }else{
    #     wetlands <- vect(wetland_files)
    #     wetlands <- wetlands[,"ATTRIBUTE"]
    #   }
      cat("Finished loading and combining all wetland files at",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes since start\n")
      ################################################################################
      #split them into the relevant parts and rasterize/save them
      
      #reproject and create a template for output, slightly larger than the
      #wetland shapefile to avoid losing any data
      wetlands <- project(wetlands,crs(domain))
      state_template <- domain_template
      values(state_template) <- 0
      state_template <- extend(state_template,ext(wetlands)+0.5,snap="out")
      
      #split the wetlands file into the relevant wetland types
      Attribute_text <- wetlands$ATTRIBUTE
      if(Include_freshwater){
        R1 <- wetlands[startsWith(Attribute_text, 'R1'),]
        R2 <- wetlands[startsWith(Attribute_text, 'R2'),]
        R3 <- wetlands[startsWith(Attribute_text, 'R3'),]
        R4 <- wetlands[startsWith(Attribute_text, 'R4'),]
        L1 <- wetlands[startsWith(Attribute_text, 'L1'),]
        L2 <- wetlands[startsWith(Attribute_text, 'L2'),]
      }
      if(Use_SOCCR1 | Use_SOCCR2){
        M2 <- wetlands[startsWith(Attribute_text, 'M2'),]
        E2 <- wetlands[startsWith(Attribute_text, 'E2'),]
        PFO <- wetlands[startsWith(Attribute_text, 'PFO'),]
        PNF <- wetlands[startsWith(Attribute_text, 'P')&!startsWith(Attribute_text, 'PFO'),]
      }
      
      if(Include_freshwater){
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
      
      # }
    cat("Finished processing",state_name_list[i],"which is",i,"of",length(state_name_list),"at",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes since start\n\n")
  }
  cat("Finished wetland sector: NWI_Wetland_fraction in",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes\n\n")
}