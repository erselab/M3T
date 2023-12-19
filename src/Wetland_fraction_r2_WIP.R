# Calculate wetland fraction using NWI state wetland cover data

#need to make crop or extend depending on d01

################################################################################
#Manually defined variables

Input_directory <- "G:/My Drive/Shepson Group Drive/Kris/Indy Inversion/Inventories/Hi-res_CH4_inventory_development/Raw_data_files/NWI_shapefiles/"
Output_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed/NWI/"
# Input_directory <- "/gpfs/projects/ShepsonGroup/khajny/NWI_in/"
# Output_directory <- "/gpfs/projects/ShepsonGroup/khajny/NWI_out/"
#Input is Shapefiles for the National Wetlands Inventory.  Either already
#downloaded, or will be automatically downloaded to the input directory.

d01_bounding_box <- cbind(c(-179.5,-9.5),c(5,85))
resolution <- 0.01
#Will be used to generate a blank raster for the output to be built onto, WGS84
#CRS.  Resolution in deg, bounding box in lat/long.  This domain covers the
#entire contiguous US.

# state_list <- "IN"
state_list <- state.abb[!state.abb %in% c("HI")]
#remove Hawaii to get all contiguous US states + Alaska

NWI_url <- "https://documentst.ecosphere.fws.gov/wetlands/data/State-Downloads/"
#URL base to download NWI data if not already downloaded

options(timeout = 60*20)
#these are larger files, so set the download timeout to 20 minutes (default is 1
#min)

parallelized = F
#do we want to run this in parallel?  If so, include 2 arguments as input after
#Rscript
#1 = a number from 1 to the number of parellel runs to tell the code which
#fraction of the data to process in this run
#2 = number of parallel runs overall.  Data is split into this many parts

################################################################################
#load packages

i <- 1
packagecheck <- c("raster","sf","sp")

while(i<=length(packagecheck)){
  if(length(find.package(packagecheck[i],quiet = TRUE))<1){
    install.packages(packagecheck[i],repos="https://repo.miserver.it.umich.edu/cran/")
  }
  i <- i+1
}

invisible(suppressPackageStartupMessages(lapply(packagecheck, require, character.only=TRUE)))
rm(packagecheck,i)

#raster + sf + sp = raster and .nc filetype functionalities
################################################################################
#now quickly build the output raster matrix

d01_rast <- raster(nrows=diff(range(d01_bounding_box[,2]))/resolution,
                   ncols=diff(range(d01_bounding_box[,1]))/resolution,
                   xmn=min(d01_bounding_box[,1]), xmx=max(d01_bounding_box[,1]),
                   ymn=min(d01_bounding_box[,2]), ymx=max(d01_bounding_box[,2]),
                   crs="+proj=longlat +datum=WGS84 +no_defs")
rm(d01_bounding_box,resolution)
################################################################################
#subset the data for various parellelized runs

if(parallelized){
  #only for parallelization.
  additional_args = commandArgs(trailingOnly=T)
  #pull in arguments after the script.  1 = which part is this (sequential from 1
  #to N cores), 2 = N tasks to parallellize to
  
  additional_args=as.numeric(additional_args)
  
  Parallel_fraction <- floor(seq(0,length(state_list),length.out=additional_args[2]+1))
  Parallel_fraction <- (Parallel_fraction[additional_args[1]]+1):Parallel_fraction[additional_args[1]+1]
  state_list <- state_list[Parallel_fraction]
  #which fraction of the data is being run in this parallel part?
  
  unlink(paste0(Output_directory,"/progress_update_run_",additional_args[1]))
  #remove the progress update from a previous run, if it exists
  
  rm(Parallel_fraction)
}
################################################################################
#Rewrite cat to either print to the console or to a file if parallelized

if(parallelized){
  my_cat <- function(...){
    cat(...,
        file=paste0(Output_directory,"/progress_update_run_",additional_args[1]),
        append=T)
  }
}else{
  my_cat <- function(...){
    cat(...)
  }
}

################################################################################
#Write a simple function to rm a raster, delete any tmp files, and gc() in one
cleanup <- function(input){
  input_name <- deparse(substitute(input))
  remove(list=input_name,pos = .GlobalEnv)
  invisible(gc())
  removeTmpFiles(h=0)
}
################################################################################
#Create a function that will reproject to the proper CRS and then rasterize,
#potentially account for invalid polygons, and if too large, split the job into
#many smaller parts (doable/faster)

Split_rasterize <- function(input){
  my_cat("\rStarting",substitute(input),"for",state_list[i],"at",as.character(Sys.time()),"             \n")
  if(paste0(state_list[i],'_',substitute(input),'.grd') %in% Processed_NWI_files){
    return(NULL)
  }
  invalid_polygons <- st_is_valid(input)
  # invalid_polygons_check1 <- grepl("Non-empty, non-full loops must have at least 3 vertices",
  #                                  invalid_polygons)
  invalid_polygons <- is.na(invalid_polygons)
  # invalid_polygons <- invalid_polygons_check1+invalid_polygons_check2
  input_partial <- input[!invalid_polygons,]
  #transform to right CRS and remove any polygons < 4 pts (cannot be
  #processed).  If using R3.6.0, polygons < 4 pts are NA.  If using
  #R4.2.3 they are listed as written in check1.  Adding the 2 makes
  #it run successfully for either situation.
  if(sum(invalid_polygons)>0){
    Fudge_df <<- rbind(Fudge_df,c(state_list[i],
                                  as.character(substitute(input)),
                                  sum(invalid_polygons)))
  }
  #note any which had polygons < 4 pts
  input_partial <- as(input_partial, 'Spatial')
  #convert to sp to more easily work with rasters
  output_rast <- state_raster
  for(A in 1:length(input_partial)){
    cover <- cellFromPolygon(state_raster, input_partial[A,], weights = TRUE)
    # Calculate the fractional coverage of each polygon in each d01 pixel.
    if(is.null(cover[[1]])){
      next
      #occassionally, a polygon is so tiny, it is calculated to have no area
      #within any pixels (e.g., AL E1, feature 213, area of 3.4E-4 acres)
    }else{
      template <- state_raster
      template[cover[[1]][,'cell']] <- cover[[1]][,'weight']*100
      # Starting with a raster of zeros, set cells within polygon A equal to
      # their weighting
      output_rast <- sum(output_rast,template)
      #combine all coverage rasters
      my_cat("\rFinished polygon",A,"of",length(input_partial),"                     ")
      # removeTmpFiles(h=0)
    }
  }
  #  output_rast <- rasterize(input_partial,state_raster,getCover=T)
  output_rast <- extend(output_rast,d01_rast,value=0)
  writeRaster(output_rast, paste0(Output_directory,state_list[i],'_',substitute(input),'.grd'),overwrite=T)
}
# 
# Split_rasterize <- function(input){
#   my_cat("Starting",substitute(input),"for",state_list[i],"\n")
#   start2 <<- Sys.time()
#   if(paste0(state_list[i],'_',substitute(input),'.grd') %in% Processed_NWI_files){
#     return(NULL)
#   }
#   invalid_polygons <- st_is_valid(input,reason=T)
#   invalid_polygons <- grepl("Non-empty, non-full loops must have at least 3 vertices",
#                             invalid_polygons)
#   input_partial <- input[!invalid_polygons,]
#   #transform to right CRS and remove any polygons < 4 pts (cannot be
#   #processed)
#   if(sum(invalid_polygons)>0){
#     Fudge_df <<- rbind(Fudge_df,c(state_list[i],
#                                   as.character(substitute(input)),
#                                   sum(invalid_polygons)))
#   }
#   #note any which had polygons < 4 pts
#   output_rast <- rasterize(input_partial,state_raster,getCover=T)
#   output_rast <- extend(output_rast,d01_rast,value=0)
#   writeRaster(output_rast, paste0(Output_directory,state_list[i],'_',substitute(input),'_rasterize.grd'),overwrite=T)
#   end2 <<- Sys.time()
# }
# Split_rasterize_alt <- function(input){
#   my_cat("Starting",substitute(input),"for",state_list[i],"\n")
#   start <<- Sys.time()
#   if(paste0(state_list[i],'_',substitute(input),'.grd') %in% Processed_NWI_files){
#     return(NULL)
#   }
#   invalid_polygons <- st_is_valid(input,reason=T)
#   invalid_polygons <- grepl("Non-empty, non-full loops must have at least 3 vertices",
#                             invalid_polygons)
#   input_partial <- input[!invalid_polygons,]
#   #transform to right CRS and remove any polygons < 4 pts (cannot be
#   #processed)
#   if(sum(invalid_polygons)>0){
#     Fudge_df <<- rbind(Fudge_df,c(state_list[i],
#                                   as.character(substitute(input)),
#                                   sum(invalid_polygons)))
#   }
#   #note any which had polygons < 4 pts
#   input_partial <- as(input_partial, 'Spatial')
#   #convert to sp to more easily work with rasters
#   temp_name <- paste0(Output_directory,state_list[i],'_',substitute(input),'_temp.grd')
#   output_rast <- state_raster
#   for(A in 1:length(input_partial)){
#     cover <- cellFromPolygon(state_raster, input_partial[A,], weights = TRUE)
#     # Calculate the fractional coverage of each polygon in each d01 pixel.
#     if(is.null(cover[[1]])){
#       next
#       #occassionally, a polygon is so tiny, it is calculated to have no area
#       #within any pixels (e.g., AL E1, feature 213, area of 3.4E-4 acres)
#     }else{
#       template <- state_raster
#       template[cover[[1]][,'cell']] <- cover[[1]][,'weight']*100
#       # Starting with a raster of zeros, set cells within polygon A equal to
#       # their weighting
#       output_rast <- sum(output_rast,template)
#       #combine all coverage rasters
#       my_cat("\rFinished polygon",A,"of",length(input_partial),"                     ")
#       # removeTmpFiles(h=0)
#     }
#   }
#   output_rast <- extend(output_rast,d01_rast,value=0)
#   writeRaster(output_rast, paste0(Output_directory,state_list[i],'_',substitute(input),'.grd'),overwrite=T)
#   end <<- Sys.time()
# }
# 
# Split_rasterize_alt2 <- function(input){
#   my_cat("Starting",substitute(input),"for",state_list[i],"\n")
#   start3 <<- Sys.time()
#   if(paste0(state_list[i],'_',substitute(input),'.grd') %in% Processed_NWI_files){
#     return(NULL)
#   }
#   invalid_polygons <- st_is_valid(input,reason=T)
#   invalid_polygons <- grepl("Non-empty, non-full loops must have at least 3 vertices",
#                             invalid_polygons)
#   input_partial <- input[!invalid_polygons,]
#   #transform to right CRS and remove any polygons < 4 pts (cannot be
#   #processed)
#   if(sum(invalid_polygons)>0){
#     Fudge_df <<- rbind(Fudge_df,c(state_list[i],
#                                   as.character(substitute(input)),
#                                   sum(invalid_polygons)))
#   }
#   #note any which had polygons < 4 pts
#   input_partial <- as(input_partial, 'Spatial')
#   #convert to sp to more easily work with rasters
#   temp_name <- paste0(Output_directory,state_list[i],'_',substitute(input),'_temp.grd')
#   output_rast <- state_raster
#   for(A in 1:length(input_partial)){
#     cover <- cellFromPolygon(state_raster, input_partial[A,], weights = TRUE)
#     # Calculate the fractional coverage of each polygon in each d01 pixel.
#     if(is.null(cover[[1]])){
#       next
#       #occassionally, a polygon is so tiny, it is calculated to have no area
#       #within any pixels (e.g., AL E1, feature 213, area of 3.4E-4 acres)
#     }else{
#       template <- state_raster
#       template[cover[[1]][,'cell']] <- cover[[1]][,'weight']*100
#       # Starting with a raster of zeros, set cells within polygon A equal to
#       # their weighting
#       output_rast <- sum(output_rast,template)
#       #combine all coverage rasters
#       if(!A %% 1000){
#         my_cat("\rFinished polygon",A,"of",length(input_partial),"                     ")
#       }
#       # removeTmpFiles(h=0)
#     }
#   }
#   output_rast <- extend(output_rast,d01_rast,value=0)
#   writeRaster(output_rast, paste0(Output_directory,state_list[i],'_',substitute(input),'.grd'),overwrite=T)
#   end3 <<- Sys.time()
# }
################################################################################
#load in the wetlands files

Fudge_df <- data.frame("state"="in","Type"="in","Fudge_count"=0,stringsAsFactors = F)
#create a dataframe to check if/how many polygons are removed in a few
#situations due to errors (usually vertices < 4)

state_name_list <- state.name[which(state.abb %in% state_list)]

Input_directory_data <- list.files(Input_directory)
Downloaded_NWI_files <- paste0(state_list,"_shapefile_wetlands") %in% Input_directory_data
Processed_NWI_files <- list.files(Output_directory,pattern=".grd")

for(i in 1:length(state_list)){
  # Wetlands inventory has a shapefile for each state - load them
  # Use st_read because these are very large polygon files
  my_cat("\rStarting processing for",state_name_list[i],"                              \n")
  
  NWI_filename <- paste0(state_list[i],"_shapefile_wetlands.zip")
  if(!Downloaded_NWI_files[i]){
    download_repeatedly <- tryCatch({download.file(url = paste0(NWI_url,NWI_filename),
                                                   destfile = paste0(Input_directory,NWI_filename),
                                                   quiet=T)},
                                    error = function(e){
                                      my_cat("\rconnection lost, retrying.  Use esc to exit if needed                         \n")
                                      Sys.sleep(10)
                                      download.file(url = paste0(NWI_url,NWI_filename),
                                                    destfile = paste0(Input_directory,NWI_filename),
                                                    quiet=T)})
    unzip(file.path(Input_directory,NWI_filename),
          exdir=Input_directory)
    unlink(file.path(Input_directory,NWI_filename), recursive=TRUE)
  }
  NWI_filename <- gsub(".zip","",NWI_filename)
  
  wetland_files <- list.files(file.path(Input_directory,NWI_filename),pattern=glob2rx("*.shp"),full.names = T)
  wetland_files <- wetland_files[!grepl("Historic",wetland_files)]
  wetland_files <- wetland_files[!grepl("Metadata",wetland_files)]
  wetland_files <- wetland_files[grepl("Wetlands",wetland_files)]
  #progressively remove files to isolate just the relevant wetland files. some
  #have historic files, there's metadata, shape outline files, some states have
  #other types (e.g. MO riparian), etc.
  if(length(wetland_files)>1){
    wetlands <- suppressWarnings(st_read(wetland_files[1]))
    wetlands <- wetlands[,"ATTRIBUTE"]
    for(A in 2:length(wetland_files)){
      additional_wetlands <- suppressWarnings(st_read(wetland_files[A]))
      additional_wetlands <- additional_wetlands[,"ATTRIBUTE"]
      wetlands <- rbind(wetlands,additional_wetlands)
    }
    remove(additional_wetlands)
  }else{
    wetlands <- suppressWarnings(st_read(wetland_files))
    wetlands <- wetlands[,"ATTRIBUTE"]
  }
  my_cat("\rFinished loading and combining all wetland files                                ")
  
  rm(wetland_files,NWI_filename)
  ################################################################################
  #split them into the relevant parts and rasterize/save them
  
  wetlands <- st_transform(wetlands,crs(d01_rast))
  state_raster <- raster(xmn=floor(extent(wetlands)[1]),xmx=ceiling(extent(wetlands)[2]),
                         ym=floor(extent(wetlands)[3]),ymx=ceiling(extent(wetlands)[4]),
                         resolution=res(d01_rast),crs=crs(wetlands))
  state_raster[] <- 0
  M1 <- wetlands[startsWith(as.character(wetlands$ATTRIBUTE), 'M1'),]
  M2 <- wetlands[startsWith(as.character(wetlands$ATTRIBUTE), 'M2'),]
  E1 <- wetlands[startsWith(as.character(wetlands$ATTRIBUTE), 'E1'),]
  E2 <- wetlands[startsWith(as.character(wetlands$ATTRIBUTE), 'E2'),]
  R1 <- wetlands[startsWith(as.character(wetlands$ATTRIBUTE), 'R1'),]
  R2 <- wetlands[startsWith(as.character(wetlands$ATTRIBUTE), 'R2'),]
  R3 <- wetlands[startsWith(as.character(wetlands$ATTRIBUTE), 'R3'),]
  R4 <- wetlands[startsWith(as.character(wetlands$ATTRIBUTE), 'R4'),]
  L1 <- wetlands[startsWith(as.character(wetlands$ATTRIBUTE), 'L1'),]
  L2 <- wetlands[startsWith(as.character(wetlands$ATTRIBUTE), 'L2'),]
  PFO <- wetlands[startsWith(as.character(wetlands$ATTRIBUTE), 'PFO'),]
  PNF <- wetlands[startsWith(as.character(wetlands$ATTRIBUTE), 'P')&!startsWith(as.character(wetlands$ATTRIBUTE), 'PFO'),]
  cleanup(wetlands)
  #split the wetlands file into the relevant wetland types, then delete the
  #original to save memory a bit
  
  my_cat("\rFinished splitting wetland data into wetland types                                ")
  
  if(dim(M1)[1]!=0){  # if there is some of category M1
    Split_rasterize(M1)
    if(parallelized){
      write.csv(Fudge_df,paste0(Output_directory,"Incomplete_polygons_run_",additional_args[1],".csv"))
    }else{
      write.csv(Fudge_df,paste0(Output_directory,"Incomplete_polygons.csv"))
    }
  }
  cleanup(M1)
  
  if(dim(M2)[1]!=0){
    Split_rasterize(M2)
    if(parallelized){
      write.csv(Fudge_df,paste0(Output_directory,"Incomplete_polygons_run_",additional_args[1],".csv"))
    }else{
      write.csv(Fudge_df,paste0(Output_directory,"Incomplete_polygons.csv"))
    }
  }
  cleanup(M2)
  
  if(dim(E1)[1]!=0){
    Split_rasterize(E1)
    if(parallelized){
      write.csv(Fudge_df,paste0(Output_directory,"Incomplete_polygons_run_",additional_args[1],".csv"))
    }else{
      write.csv(Fudge_df,paste0(Output_directory,"Incomplete_polygons.csv"))
    }
  }
  cleanup(E1)
  
  if(dim(E2)[1]!=0){
    Split_rasterize(E2)
    if(parallelized){
      write.csv(Fudge_df,paste0(Output_directory,"Incomplete_polygons_run_",additional_args[1],".csv"))
    }else{
      write.csv(Fudge_df,paste0(Output_directory,"Incomplete_polygons.csv"))
    }
  }
  cleanup(E2)
  
  if(dim(R1)[1]!=0){
    Split_rasterize(R1)
    if(parallelized){
      write.csv(Fudge_df,paste0(Output_directory,"Incomplete_polygons_run_",additional_args[1],".csv"))
    }else{
      write.csv(Fudge_df,paste0(Output_directory,"Incomplete_polygons.csv"))
    }
  }
  cleanup(R1)
  
  if(dim(R2)[1]!=0){
    Split_rasterize(R2)
    if(parallelized){
      write.csv(Fudge_df,paste0(Output_directory,"Incomplete_polygons_run_",additional_args[1],".csv"))
    }else{
      write.csv(Fudge_df,paste0(Output_directory,"Incomplete_polygons.csv"))
    }
  }
  cleanup(R2)
  
  if(dim(R3)[1]!=0){
    Split_rasterize(R3)
    if(parallelized){
      write.csv(Fudge_df,paste0(Output_directory,"Incomplete_polygons_run_",additional_args[1],".csv"))
    }else{
      write.csv(Fudge_df,paste0(Output_directory,"Incomplete_polygons.csv"))
    }
  }
  cleanup(R3)
  
  if(dim(R4)[1]!=0){
    Split_rasterize(R4)
    if(parallelized){
      write.csv(Fudge_df,paste0(Output_directory,"Incomplete_polygons_run_",additional_args[1],".csv"))
    }else{
      write.csv(Fudge_df,paste0(Output_directory,"Incomplete_polygons.csv"))
    }
  }
  cleanup(R4)
  
  if(dim(L1)[1]!=0){
    Split_rasterize(L1)
    if(parallelized){
      write.csv(Fudge_df,paste0(Output_directory,"Incomplete_polygons_run_",additional_args[1],".csv"))
    }else{
      write.csv(Fudge_df,paste0(Output_directory,"Incomplete_polygons.csv"))
    }
  }
  cleanup(L1)
  
  if(dim(L2)[1]!=0){
    # Split_rasterize_alt(L2)
    Split_rasterize(L2)
    if(parallelized){
      write.csv(Fudge_df,paste0(Output_directory,"Incomplete_polygons_run_",additional_args[1],".csv"))
    }else{
      write.csv(Fudge_df,paste0(Output_directory,"Incomplete_polygons.csv"))
    }
  }
  cleanup(L2)
  
  if(dim(PFO)[1]!=0){
    # Split_rasterize_alt(PFO)
    Split_rasterize(PFO)
    # Split_rasterize_alt2(PFO)
    if(parallelized){
      write.csv(Fudge_df,paste0(Output_directory,"Incomplete_polygons_run_",additional_args[1],".csv"))
    }else{
      write.csv(Fudge_df,paste0(Output_directory,"Incomplete_polygons.csv"))
    }
  }
  cleanup(PFO)
  
  # if(dim(PNF)[1]!=0){
  #   Split_rasterize(PNF)
  #   if(parallelized){
  #     write.csv(Fudge_df,paste0(Output_directory,"Incomplete_polygons_run_",additional_args[1],".csv"))
  #   }else{
  #     write.csv(Fudge_df,paste0(Output_directory,"Incomplete_polygons.csv"))
  #   }
  # }
  # cleanup(PNF)
  my_cat("\nFinished processing",state_list[i],"which is",i,"of",length(state_list),"                        ")
# }

################################################################################
#Check that most polygons were used without issue

# Fudge_df <- Fudge_df[-1,]
if(parallelized){
  write.csv(Fudge_df,paste0(Output_directory,"Incomplete_polygons_run_",additional_args[1],".csv"))
}else{
  write.csv(Fudge_df,paste0(Output_directory,"Incomplete_polygons.csv"))
}
my_cat("\nCode finished.  See table to see how many polygons were removed as they were too small (<4 vertices) to be properly processed using sf")
