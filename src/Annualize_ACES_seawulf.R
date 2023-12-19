#convert ACES from hourly to annual and save an hourly timeseries for the city
#total

args = as.numeric(commandArgs(trailingOnly=T))
# Stilt R code directory

################################################################################
#User input

urban_outline = "/gpfs/projects/ShepsonGroup/khajny/tl_2019_us_uac10 - cities/tl_2019_us_uac10.shp"
#urban area census tigerlines

cities_of_interest <- c("New York--Newark, NY--NJ--CT",
                        "Chicago, IL--IN",
                        "Los Angeles--Long Beach--Anaheim, CA",
                        "Houston, TX",
                        "Philadelphia, PA--NJ--DE--MD",
                        "Allentown, PA--NJ",
                        "Minneapolis--St. Paul, MN--WI",
                        "Dallas--Fort Worth--Arlington, TX",
                        "Miami, FL",
                        "Detroit, MI",
                        "Atlanta, GA",
                        "St. Louis, MO--IL",
                        "Cincinnati, OH--KY--IN",
                        "Pittsburgh, PA",
                        "Boston, MA--NH--RI",
                        "Washington, DC--VA--MD",
                        "Baltimore, MD",
                        "Indianapolis, IN",
                        "San Francisco--Oakland, CA")
#cities of interest to save hourly data for.  Must exactly match city names in
#the file.  Line 82 can be used to ID the names.  A map is available at
#https://www.census.gov/geographies/reference-maps/2020/geo/2020-census-urban-areas.html.

sectors <- c("Air","Commercial","Elec","Industrial","Marine","Nonroad","Oilgas",
             "Onroad","Rail","Residential","Total")
#all sectors to be worked up

Year <- "2017"
Months <- sprintf("%02d",1:12)
#year and months needed (all months for 1 year)

Subset_sectors <- round(seq(1,12,length.out=5))
Subset_sectors <- Subset_sectors[args]:(Subset_sectors[args+1]-1)
sectors <- sectors[Subset_sectors]
#to parallelize.  Keep only some fraction of the sectors to work up per
#parallelized run.

filenames <- paste0(Year,Months)
filenames <- paste0("aces_",rep(sectors,each=12),"_",filenames,".nc4")
#filenames on the server to download.  Should match exactly from below.

download_url <- "https://thredds.daac.ornl.gov/thredds/fileServer/ornldaac/1943/"
#main URL to save files.  See 
#https://thredds.daac.ornl.gov/thredds/catalog/ornldaac/1943/catalog.html?dataset=1943/aces_Air_201201.nc4

destination_folder <- "/gpfs/projects/ShepsonGroup/khajny/Annualizing/"
#output

################################################################################
#load packages
i <- 1
.libPaths("/gpfs/projects/ShepsonGroup/r_packages/R_v4")
packagecheck <- c("raster","ncdf4","sf","lubridate")
while(i<=length(packagecheck)){
  if(length(find.package(packagecheck[i],quiet = TRUE))<1){
    install.packages(packagecheck[i],repos="https://repo.miserver.it.umich.edu/cran/")
  }
  i <- i+1
}

suppressPackageStartupMessages(invisible(lapply(packagecheck, library, character.only=TRUE)))
rm(packagecheck,i)

#raster = raster functionalities
#sf = simple features (spatial objects) functionalities
#ncdf4 = functionalities for the netcdf, .nc, filetype
#lubridate = easier date/time functions, mostly days_in_month() here
################################################################################
#Subset urban outline to just cities of interest, then project it to ACES CRS

ACES_crs <- "+proj=lcc +lat_0=40 +lon_0=-97 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
urban_outline <- st_read(urban_outline)
urban_outline <- as(urban_outline,"Spatial")
#load in as sf and convert to sp (necessary for later functions)

# city_names <- sort(urban_outline$NAME10)
# city_names[which(regexpr("Boston",city_names)>0)]
# #replace Boston with any city name or partial name to see the exact name in the
# #urban outlines file

urban_outline <- urban_outline[urban_outline$NAME10 %in% cities_of_interest,]
urban_outline <- spTransform(urban_outline,ACES_crs)

cities_of_interest <- urban_outline$NAME10
#make sure these are in the same order
################################################################################
#prep a few values and output variables for converting ACES from hourly to
#annual
dir.create(destination_folder,showWarnings = F)
setwd(destination_folder)

Annual_ACES <- raster(nrow=2908,ncol=4634,xmn=-2300000,xmx=2334000,ymn=-1608000,
                      ymx=1300000,crs=ACES_crs,vals=0)
#copy pasted from an example ACES file

Urban_overlap <- cellFromPolygon(Annual_ACES, urban_outline, weights = TRUE)
#calculate the fraction of each pixel in ACES covered by each cities outline.
#List where each entry is 1 city, 1 column for pixel #, 1 for fraction covered
#by the polygon.

if(max(unlist(lapply(Urban_overlap,FUN=function(x){max(x[,'weight'])}))) > 0.01){
  stop('Check cellFromPolygon behaviour - this code assumes a bug that may now have been fixed')
}
# Add in a check to make sure the bug where 100% = 0.01 hasn't been fixed (if
# using a more recent version of raster)

cities_of_interest <- gsub(pattern = ",",replacement = "",cities_of_interest)
#can't save as csv later if there's commas in the headers, remove here. 

################################################################################
#ACES data is hourly, we want it to be the annual average instead.  Load layer 1
#of file 1, add it to the total, move to the next.  While doing so, sum the
#total for the urban outlines

Sector_breakdown <- seq(1,length(filenames)+1,by=12)
#12 per sector to the length of all files
Sector_count <- length(Sector_breakdown)-1

hourpermonth <- (cumsum(days_in_month(seq(as.Date(paste0(Year,"-01-01")),
                                          as.Date(paste0(Year,"-12-30")),by="month"))))*24

file.create(paste0("Annualize_",args,".txt"),overwrite=T)

options(timeout=60*20)
#default is to timeout if a download takes more than a minute.  Set that to 20
#minutes given these are large files
for(Sector_indx in 1:length(sectors)){
  out_indx <- 1
  first_hour_indx <- 1
  first_File_indx <- (Sector_breakdown[Sector_indx])
  #some indices
  Annual_ACES <- raster(nrow=2908,ncol=4634,xmn=-2300000,xmx=2334000,ymn=-1608000,
                        ymx=1300000,crs=ACES_crs,vals=0)
  writeRaster(Annual_ACES,
              paste0(Year,"_Annual_ACES_",sectors[Sector_indx],".nc"),
              overwrite=T,
              force_v4=TRUE,
              varname="flux_co2",
              varunit="kg km-2 h-1",
              longname=paste0(sectors[Sector_indx],"_sector_annual_average_combustion_CO2_emissions"))
  #load 1 layer of ACES, set to 0, just a base to add to with the right X,Y, etc.
  
  Urban_timestamp <- vector(length=8760)
  Urban_Emissions <- data.frame(matrix(ncol=length(cities_of_interest),nrow=8760))
  colnames(Urban_Emissions) <- cities_of_interest
  #1 year in hours, initialized output
  
  cat(sectors[Sector_indx],"started at",as.character(Sys.time()),"\n", file = paste0("Annualize_",args,".txt"),append=T)
  
  for(File_indx in first_File_indx:(Sector_breakdown[Sector_indx+1]-1)){
    #All 12 monthly files with hourly data for this sector
    download.file(url=paste0(download_url,filenames[File_indx]),
                  destfile <- paste0(destination_folder,filenames[File_indx]))
    
    monthly_file <- brick(filenames[File_indx],varname="flux_co2")
    #download and load in the monthly file
    for(hour_indx in first_hour_indx:nlayers(monthly_file)){
      hourly_file <- monthly_file[[hour_indx]]
      Urban_timestamp[out_indx] <- getZ(hourly_file)
      #save the timestamp
      Urban_Emissions[out_indx,] <- sapply(Urban_overlap,FUN = function(x){sum(hourly_file[x[,'cell']]*
                                                                                 x[,'weight']*100,
                                                                               na.rm=T)})
      #Calculate the total within each urban area and save to urban emissions,
      #all at once
      Annual_ACES <- Annual_ACES+hourly_file
      #add to the annual total
      cat("\rFinished hour ",out_indx," of 8760                   ", file = paste0("Annualize_",args,".txt"),append=T)
      #user update
      out_indx <- out_indx+1
    }
    file.remove(filenames[File_indx])
    #now get rid of the file
  }
  Annual_ACES <- Annual_ACES/8760
  writeRaster(Annual_ACES,
              paste0(Year,"_Annual_ACES_",sectors[Sector_indx],".nc"),
              overwrite=T,
              force_v4=TRUE,
              varname="flux_co2",
              varunit="kg km-2 h-1",
              longname=paste0(sectors[Sector_indx],"_sector_annual_average_combustion_CO2_emissions"))
  #save output after converting to avg per hr rather than annual sum
  
  output <- cbind(Urban_timestamp,Urban_Emissions)
  colnames(output) <- c("Time (UTC)",paste0(colnames(Urban_Emissions)," (kg/h)"))
  write.csv(output,file = paste0(Year,"_hourly_urban_ACES_",sectors[Sector_indx],".csv"),
            quote = F,row.names = F)
  #save the CSV data
  
  cat("\rFinished ",sectors[Sector_indx]," Sector,",Sector_count-Sector_indx," to go\n", file = paste0("Annualize_",args,".txt"),append=T)
}


options(timeout=60)
         ", file = paste0("Annualize_",args,".txt"),append=T)
      #   writeRaster(Annual_ACES,
      #               paste0(Year,"_Annual_ACES_",sectors[Sector_indx],".nc"),
      #               overwrite=T,
      #               force_v4=TRUE,
      #               varname="flux_co2",
      #               varunit="kg km-2 yr-1",
      #               longname=paste0(sectors[Sector_indx],"_sector_annual_average_combustion_CO2_emissions"))
      #   #save output after converting to avg per hr rather than annual sum
      #   cat("\rSaved Raster                                  ", file = paste0("Annualize_",args,".txt"),append=T)
      #   
      #   output <- cbind(Urban_timestamp,Urban_Emissions)
      #   colnames(output) <- c("Time (UTC)",paste0(colnames(Urban_Emissions)," (kg/h)"))
      #   write.csv(output,file = paste0(Year,"_hourly_urban_ACES_",sectors[Sector_indx],".csv"),
      #             quote = F,row.names = F)
      #   cat("\rSaved data                              ", file = paste0("Annualize_",args,".txt"),append=T)
      #   #save the CSV data
      # }
    }
    file.remove(filenames[File_indx])
    #now get rid of the file
  }
  Annual_ACES <- Annual_ACES/8760
  writeRaster(Annual_ACES,
              paste0(Year,"_Annual_ACES_",sectors[Sector_indx],".nc"),
              overwrite=T,
              force_v4=TRUE,
              varname="flux_co2",
              varunit="kg km-2 h-1",
              longname=paste0(sectors[Sector_indx],"_sector_annual_average_combustion_CO2_emissions"))
  #save output after converting to avg per hr rather than annual sum
  
  output <- cbind(Urban_timestamp,Urban_Emissions)
  colnames(output) <- c("Time (UTC)",paste0(colnames(Urban_Emissions)," (kg/h)"))
  write.csv(output,file = paste0(Year,"_hourly_urban_ACES_",sectors[Sector_indx],".csv"),
            quote = F,row.names = F)
  #save the CSV data
  
  cat("\rFinished ",sectors[Sector_indx]," Sector,",Sector_count-Sector_indx," to go\n", file = paste0("Annualize_",args,".txt"),append=T)
}


options(timeout=60)
