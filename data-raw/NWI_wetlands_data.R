## code to prepare `NWI_wetlands_data` dataset.  This processes the very high
## resolution national wetland inventory polygons to grid the fractional cover
## of certain water types (river, wetland, etc.) in each state file and create a
## 1 km x 1 km CONUS grid of these land covers for use in calculating wetland
## and freshwater emissions. Assumes state tigerlines have already been
## downloaded (separate scripts).  Note this is extremely time consuming and
## memory intensive to run as it's processing hundreds of thousands of polygons
## for each state.



input_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Manuscript/All inventory data/Prepared inventory data/"
state_name_list <- c('AL','AR','AZ','CA','CO','CT','DC','DE','FL','GA','IA','ID',
                     'IL','IN','KS','KY','LA','MA','MD','ME','MI','MN','MO','MS',
                     'MT','NC','ND','NE','NH','NJ','NM','NV','NY','OH','OK','OR',
                     'PA','RI','SC','SD','TN','TX','UT','VA','VT','WA','WI','WV',
                     'WY')

################################################################################
#save partial data given the significant processing time/memory needed

NWI_partial_output_directory <- file.path(input_directory,"processed_wetland_NWI_data")
dir.create(NWI_partial_output_directory,showWarnings = F,recursive = T)
################################################################################
#setup a CONUS domain at 1 km x 1 km resolution

domain_res <- c(1000,1000)
domain_crs <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
domain <- as.data.frame(cbind(c(-3590475,3792475),
                              c(-510497,3990107)))
domain <- terra::rast(nrows=diff(range(domain[,2]))/domain_res[2],
                      ncols=diff(range(domain[,1]))/domain_res[1],
                      xmin=min(domain[,1]), xmax=max(domain[,1]),
                      ymin=min(domain[,2]), ymax=max(domain[,2]),
                      vals=1)
domain <- terra::as.polygons(terra::ext(domain),crs=domain_crs)
domain_template <- terra::rast(domain,resolution=domain_res,crs=domain_crs,vals=NA)

################################################################################
#Some settings

#URL base to download NWI data if not already downloaded
NWI_url <- "https://documentst.ecosphere.fws.gov/wetlands/data/State-Downloads/"

################################################################################
#create input folder for raw NWI data

NWI_input_directory <- file.path(input_directory,"raw_wetland_NWI_data")
dir.create(NWI_input_directory,showWarnings = F)

################################################################################
#Create a function that will reproject to the proper CRS and then rasterize,
#and account for invalid polygons if they exist

rasterize_plus <- function(input){
  #pull the name of input for later, have to do so before editing input within
  #the function in any way
  input_name <- substitute(input)
  if(paste0(state_name_list[i],'_',input_name,'.tif') %in% Processed_NWI_files){
    cat(input_name,"already processed for",state_name_list[i],"\n")
  }else{
    cat("Starting",input_name,"at",format(Sys.time(),"%H:%M:%S"),"\n")
    
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
    
    terra::writeRaster(output_rast,
                       file.path(NWI_partial_output_directory,paste0(state_name_list[i],'_',input_name,'.tif')),
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
Processed_NWI_files <- list.files(NWI_partial_output_directory,pattern=".tif")

for(i in 1:length(state_name_list)){
  cat("Starting processing for",state_name_list[i],"which is,",i,"of",length(state_name_list),", at",format(Sys.time(),"%H:%M:%S"),"\n")
  
  ################################################################################
  #Quick check for speed on restarting partial runs
  if(state_name_list[i] %in% sapply(strsplit(Processed_NWI_files[grepl("PNF.tif",Processed_NWI_files)],"_"),"[[",1)){
    cat(state_name_list[i],"already processed, skipping\n\n")
    next
  }

  ################################################################################
  #filename on the NWI website
  if(state_name_list[i]=="MN"){
    NWI_filename <- "MN_geodatabase_wetlands.zip"
  }else{
    NWI_filename <- paste0(state_name_list[i],"_geopackage_wetlands.zip")
  }
  
  ################################################################################
  #download any not already downloaded
  if(!Downloaded_NWI_files[i]){
    data_URL <- paste0(NWI_url,NWI_filename)
    NWI_full_filename <- file.path(NWI_input_directory,NWI_filename)
    
    # Download shapefile for this state
    utils::download.file(data_URL,NWI_full_filename,method="curl",quiet=T)
    
    #unzip the downloaded file and delete the zip file
    utils::unzip(NWI_full_filename,
                 exdir=NWI_input_directory)
    unlink(NWI_full_filename, recursive=TRUE)
    
    #in some cases, the file name in the zip differs.  Force it to match
    #expected.
    if(state_name_list[i]=="MN"){
      new_file <- list.files(NWI_input_directory,full.names = T)[grep("MN",list.files(NWI_input_directory,full.names = T))]
      file.rename(new_file,file.path(NWI_input_directory,"MN_geodatabase_wetlands.gdb"))
    }else{
      output_state <- sapply(strsplit(list.files(NWI_input_directory),"_"),"[[",1)
      new_file <- list.files(NWI_input_directory,full.names = T)[output_state==state_name_list[i]]
      file.rename(new_file,file.path(NWI_input_directory,paste0(state_name_list[i],"_Wetlands_Geopackage.gpkg")))
    }
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
  
  cat("Finished loading in wetland files at",format(Sys.time(),"%H:%M:%S"),"\n")
  ################################################################################
  #split them into the relevant parts and rasterize/save them
  
  #reproject and create a template for output, slightly larger than the
  #wetland shapefile to avoid losing any data
  state_template <- domain_template
  terra::values(state_template) <- 0
  state_template <- terra::extend(state_template,terra::ext(wetlands)+2,snap="out")
  
  #split the wetlands file into the relevant wetland types
  Attribute_text <- wetlands$ATTRIBUTE
  #freshwater
  R1 <- wetlands[startsWith(Attribute_text, 'R1'),]
  R2 <- wetlands[startsWith(Attribute_text, 'R2'),]
  R3 <- wetlands[startsWith(Attribute_text, 'R3'),]
  R4 <- wetlands[startsWith(Attribute_text, 'R4'),]
  L1 <- wetlands[startsWith(Attribute_text, 'L1'),]
  L2 <- wetlands[startsWith(Attribute_text, 'L2'),]
  M2 <- wetlands[startsWith(Attribute_text, 'M2'),]
  E2 <- wetlands[startsWith(Attribute_text, 'E2'),]
  PFO <- wetlands[startsWith(Attribute_text, 'PFO'),]
  PNF <- wetlands[startsWith(Attribute_text, 'P')&!startsWith(Attribute_text, 'PFO'),]
  
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
  
  rm(R1,R2,R3,R4,L1,L2,M2,E2,PFO,PNF)
  invisible(gc())
  cat("Finished processing",state_name_list[i],"which is",i,"of",length(state_name_list),"at",format(Sys.time(),"%H:%M:%S"),"\n\n\n\n")
}

################################################################################
#Combine across states and resave

wetland_types <- c("R1","R2","R3","R4",
                   "L1","L2",
                   "M2",
                   "E2",
                   "PFO","PNF")

for(A in 1:length(wetland_types)){
  Processed_files <- list.files(NWI_partial_output_directory,
                                pattern=paste0(wetland_types[A],".*tif"),
                                full.names=T)
  combined <- terra::rast(Processed_files)
  combined <- max(combined)
  terra::writeRaster(combined,
                     file.path(input_directory,"combined_NWI_wetland_landcover.tif"),
                     overwrite=T)
}



