#'@title Download gridded CO2 emissions maps for the needed sectors from ACES
#'  and/or Vulcan and integrate them into monthly/annual files rather than the
#'  hourly ones available
#'
#'@description `Prepare_ACES_Vulcan` writes many netcdf files of gridded CO2
#'  emissions - one for every month and year for each sector.  Depending on
#'  config options, will run for ACES and/or Vulcan.
#'
#'@details This function downloads the most appropriate year of gridded CO2 data
#'  at 1 km by 1 km from the Anthropogenic Carbon Emission System (ACES) v2 from
#'  Gately et al. and/or Vulcan v3 from Gurney et al.  It downloads the total
#'  across all sectors and the sectors needed for other functions.  Then it
#'  takes the hourly data in daily (Vulcan) or monthly (ACES) files and
#'  aggregates into monthly and annual files.  They are cropped to the extent of
#'  the states within the domain with a slight buffer, but not reprojected at
#'  this point. The necessary sectors are
#' \itemize{
#'   \item Commercial
#'   \item Electric
#'   \item Industrial
#'   \item Residential
#'   \item Total
#'   }
#'
#'  ACES is available at \url{https://doi.org/10.3334/ORNLDAAC/1943} and annual
#'  Vulcan is available at \url{https://doi.org/10.3334/ORNLDAAC/1741} while
#'  hourly Vulcan is available at \url{https://doi.org/10.3334/ORNLDAAC/1810}.
#'  The monthly/annual output files will be saved to the input_directory to
#'  avoid re-downloading every run.
#'
#'  See references \href{https://doi.org/10.1002/2017JD027359}{Gately et
#'  al.} and \href{https://doi.org/10.1029/2020JD032974}{Gurney et al.}
#'@param input_directory Character providing the full filepath to save/load
#'  input data
#'@param Use_ACES Logical indicating whether or not to use ACES to disaggregate
#'  emissions in other functions.  Either ACES or Vulcan must be used, though
#'  both can be.
#'@param Use_Vulcan Logical indicating whether or not to use Vulcan to
#'  disaggregate emissions in other functions.  Either ACES or Vulcan must be
#'  used, though both can be.
#'@param ACES_year Numeric providing the year of ACES data to use
#'@param vulcan_band Numeric providing the band of Vulcan data to use (1-6 =
#'  2010 - 2015)
#'@param State_Tigerlines SpatVector.  United States Census Bureau county
#'  shapefile.  Available at
#'  \url{https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html}.
#'@returns Nothing is returned from the function, but the main outputs are many
#'  netcdf files of the CO2 emissions from ACES/Vulcan after converting to
#'  monthly and annual products.  They are titled "A_B_C_D_E.nc" where A is the
#'  inventory (ACES or Vulcan), B is the averaging window (monthly or annual), C
#'  is the sector, D is the year of inventory data, and E is the month.  The
#'  annual files are equivalent, but without a month.
#'@examples
#'  library(terra)
#'  Prepare_ACES_Vulcan(input_directory="~/../Desktop/",
#'                     Use_ACES=TRUE,
#'                     Use_Vulcan=TRUE,
#'                     ACES_year=2017,
#'                     vulcan_band=6,
#'                     State_Tigerlines=vect("~/../Desktop/State_Tigerlines/tl_2018_us_state.shp"))
#'@author Joe Pitt, \email{madeup@@wisc.edu}
#'@author Kris Hajny, \email{blank@@fake.edu}
#'@author Israel Lopez-Coto, \email{test@@test.edu}
#'@references \href{https://doi.org/10.1002/2017JD027359}{Gately et
#'  al.}
#'@references \href{https://doi.org/10.1029/2020JD032974}{Gurney et al.}
#'@export



#convert ACES/Vulcan from hourly to monthly/annual files

Prepare_ACES_Vulcan <- function(input_directory,
                                Use_ACES,
                                Use_Vulcan,
                                ACES_year,
                                vulcan_band,
                                State_Tigerlines){
  
  starttime <- Sys.time()
  
  
  #default is to timeout if a download takes more than a minute.  Set that to
  #1 hr per file given these are large files (shouldn't take that long, but
  #with a bad internet connection, the ~20 Gb files really might take even
  #longer than that...)
  default_timeout <- options("timeout")
  options(timeout=60*60)
  
  if(Use_ACES){
    cat("Downloading and preparing ACES: Prepare_ACES_Vulcan - this will take some time\n")
    ################################################################################
    #some initial variables that need defining
    
    #all sectors to be worked up
    sectors <- c("Air","Commercial","Elec","Industrial","Marine","Nonroad","Oilgas",
                 "Onroad","Rail","Residential","Total") #all sectors
    # sectors <- c("Commercial","Elec","Industrial","Residential","Total") #only the sectors needed
    
    #all months for 1 year, properly formatted
    Months <- sprintf("%02d",1:12)
    
    #filenames on the server to download.  Should match exactly from DAAC.
    filenames <- paste0("aces_",rep(sectors,each=12),"_",ACES_year,Months,".nc4")
    
    #main URL to pull files.  See 
    #https://thredds.daac.ornl.gov/thredds/catalog/ornldaac/1943/catalog.html?dataset=1943/aces_Air_201201.nc4
    download_url <- "https://thredds.daac.ornl.gov/thredds/fileServer/ornldaac/1943/"
    
    #output directory and names
    destination_folder <- file.path(input_directory,"ACES")
    monthly_output_list <- paste0("ACES_monthly_",rep(sectors,each=12),"_",ACES_year,"_",Months,".nc")
    annual_output_list <- paste0("ACES_annual_",sectors,"_",ACES_year,".nc")
    sectors <- tail(sectors,length(monthly_output_list)/12)
    
    #keep only those that haven't already been created (in case being rerun for
    #any reason).  Only if all 12 months were finished and the annual file was
    #created.
    filenames <- filenames[!rep(file.exists(file.path(input_directory,"ACES",annual_output_list)),each=12)]
    monthly_output_list <- monthly_output_list[!rep(file.exists(file.path(input_directory,"ACES",annual_output_list)),each=12)]
    annual_output_list <- annual_output_list[!file.exists(file.path(input_directory,"ACES",annual_output_list))]
    
    ################################################################################
    #prep a template to work with
    
    dir.create(destination_folder,showWarnings = F)
    
    #copy pasted from an example ACES file
    ACES_crs <- "+proj=lcc +lat_0=40 +lon_0=-97 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
    Annual_ACES <- rast(nrows=2908,ncols=4634,xmin=-2300000,xmax=2334000,ymin=-1608000,
                        ymax=1300000,crs=ACES_crs,vals=0)
    
    # Annual_ACES <- crop(Annual_ACES,ext(project(State_Tigerlines,crs(Annual_ACES)))*1.1)
    monthly_ACES <- Annual_ACES
    
    ################################################################################
    #Download and process the data
    
    #loop through each sector, and each monthly file
    for(Sector_indx in 0:(length(sectors)-1)){
      for(File_indx in (1+12*Sector_indx):(12+12*Sector_indx)){
        cat("\rDownloading",filenames[File_indx],"which is ACES file number",File_indx,"of",length(filenames),"                ")
        
        #download each monthly file with hourly data for this sector
        aces_file <- paste0(destination_folder,"/",filenames[File_indx])
        download.file(url=paste0(download_url,filenames[File_indx]),
                      destfile <- aces_file,quiet=T,method="curl")
        
        #compared against simply using sum/mean; this was slightly faster (1.05 min vs
        #1.16 min using ACES total)
        # start=Sys.time()
        monthly_data <- rast(aces_file)
        #go through each hour of the file and add to the monthly total.
        #Cropping like this is fast as it pulls from the file.  Cropping all at
        #outside a loop is much slower as it then is stored entirely in memory.
        for(hr_indx in 1:nlyr(monthly_data)){
          monthly_ACES <- monthly_ACES+monthly_data[[hr_indx]]
          # monthly_ACES <- monthly_ACES+crop(monthly_data[[hr_indx]],Annual_ACES)
        }
        #add this month to the annual total and convert from monthly total to
        #monthly average (in per hr units)
        Annual_ACES <- Annual_ACES+monthly_ACES
        monthly_ACES <- monthly_ACES/nlyr(monthly_data)
        # cat("longcode = ",Sys.time() - start)
        
        # start=Sys.time()
        # monthly_data <- rast(aces_file)
        # monthly_data <- crop(monthly_data,Annual_ACES)
        # monthly_data <- mean(monthly_data)
        # cat("shortcode = ",Sys.time() - start)
        
        #save and reset the monthly_aces raster
        writeCDF(monthly_ACES,
                 file.path(destination_folder,monthly_output_list[File_indx]),
                 force_v4=TRUE,
                 varname="flux_co2",
                 unit="kg km-2 hr-1",
                 longname=paste0(sectors[Sector_indx+1],"_sector_monthly_average_combustion_CO2_emissions"),
                 missval=-9999,
                 overwrite=TRUE)
        values(monthly_ACES) <- 0
        
        #delete the downloaded file to minimize how much storage space is needed
        #while running the code (each file is >1 Gb, so >100 Gb would be needed
        #otherwise)
        unlink(aces_file)
      }
      #now that every monthly file has been processed and summed into the annual
      #one, convert that to an average (per hr units)
      Annual_ACES <- Annual_ACES/8760
      
      #save and reset the annual_aces raster
      writeCDF(Annual_ACES,
               file.path(destination_folder,annual_output_list[Sector_indx+1]),
               force_v4=TRUE,
               varname="flux_co2",
               unit="kg km-2 hr-1",
               longname=paste0(sectors[Sector_indx+1],"_sector_annual_average_combustion_CO2_emissions"),
               missval=-9999,
               overwrite=TRUE)
      values(Annual_ACES) <- 0
      
    }
    cat("\nFinished preparing ACES data at",difftime(Sys.time(),starttime,units = "min"),"minutes since start\n")
  }
  ################################################################################
  #Repeat the process, but for Vulcan
  
  if(Use_Vulcan){
    cat("Downloading and preparing Vulcan: Prepare_ACES_Vulcan - this will take some time\n")
    ################################################################################
    #some initial variables that need defining
    
    vulcan_year <- (2010:2015)[vulcan_band]
    
    #all sectors to be worked up
    sectors <- c("airport","cement","cmv","commercial","elec_prod","industrial","nonroad",
                 "onroad","rail","residential","total") #all sectors
    # sectors <- c("commercial","elec_prod","industrial","residential","total") #only the sectors needed
    
    #filenames on the server to download.  Should match exactly from DAAC.
    filenames <- paste0("Vulcan.v3.US.hourly.1km.",rep(sectors,each=365),".mn.",vulcan_year,".d",sprintf("%03d",1:365),".nc4")
    
    #main URL to pull files.  See 
    #https://thredds.daac.ornl.gov/thredds/fileServer/ornldaac/1810/Contiguous_US/airport.2013.hourly_UTC/Vulcan.v3.US.hourly.1km.airport.mn.2013.d007.nc4
    sectoral_folder <- paste0(sectors,".",vulcan_year,".hourly_UTC/")
    download_url <- "https://thredds.daac.ornl.gov/thredds/fileServer/ornldaac/1810/Contiguous_US/"
    
    #output directory and names
    destination_folder <- file.path(input_directory,"Vulcan")
    # monthly_output_list <- paste0("Vulcan_monthly_",rep(sectors,each=12),"_",vulcan_year,"_",sprintf("%02d",1:12),".nc")
    monthly_output_list <- paste0("Vulcan_monthly_",rep(sectors,each=12),"_",vulcan_year,"_",1:12,".nc")
    annual_output_list <- paste0("Vulcan_annual_",sectors,"_",vulcan_year,".nc")
    
    #keep only those that haven't already been created (in case being rerun for
    #any reason).  Only if all 12 months were finished and the annual file was
    #created.
    filenames <- filenames[!rep(file.exists(file.path(input_directory,"Vulcan",annual_output_list)),each=365)]
    monthly_output_list <- monthly_output_list[!rep(file.exists(file.path(input_directory,"Vulcan",annual_output_list)),each=12)]
    annual_output_list <- annual_output_list[!file.exists(file.path(input_directory,"Vulcan",annual_output_list))]
    sectors <- tail(sectors,length(monthly_output_list)/12)
    
    #based on https://stackoverflow.com/a/6244503
    calculate_days_in_month <- function(x){
      as.numeric(as.Date(cut(x+34, "month")) - as.Date(cut(x, "month")))
    }
    days_per_month <- calculate_days_in_month(as.Date(paste0(vulcan_year,"-",1:12,"-01")))
    
    #calculate N days since year start/month rather than just N days/month
    cumulative_days_per_month <- diffinv(days_per_month)
    ################################################################################
    #prep a template to work with
    
    dir.create(destination_folder,showWarnings = F)
    
    #copy pasted from an example Vulcan file.  Same CRS as aces, slightly
    #different extent
    Vulcan_crs <- "+proj=lcc +lat_0=40 +lon_0=-97 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
    Annual_Vulcan <- rast(nrows=2900,ncols=4648,xmin=-2305363,xmax=2342637,ymin=-1624104,
                          ymax=1275896,crs=Vulcan_crs,vals=0)
    
    # Annual_Vulcan <- crop(Annual_Vulcan,ext(project(State_Tigerlines,crs(Annual_Vulcan)))*1.1)
    monthly_Vulcan <- Annual_Vulcan
    
    ################################################################################
    #Download and process the data
    
    #loop through each sector, each month separately (since we want to save
    #monthly files), and each daily file
    for(Sector_indx in 0:(length(sectors)-1)){
      for(monthly_indx in 1:12){
        for(File_indx in (cumulative_days_per_month[monthly_indx]+1+365*Sector_indx):(cumulative_days_per_month[monthly_indx+1]+365*Sector_indx)){
          cat("\rDownloading",filenames[File_indx],"which is Vulcan file number",File_indx,"of",length(filenames),"                ")
          
          #download each daily file with hourly data for this sector and month
          vulcan_file <- paste0(destination_folder,"/",filenames[File_indx])
          download.file(url=paste0(download_url,sectoral_folder[Sector_indx+1],filenames[File_indx]),
                        destfile <- vulcan_file,quiet=T,method="curl")
          
          daily_data <- rast(vulcan_file)
          for(hr_indx in 1:nlyr(daily_data)){
            #go through each hour of the file and add to the monthly total.
            #Cropping like this is fast as it pulls from the file.  Cropping all at
            #outside a loop is much slower as it then is stored entirely in memory.
            monthly_Vulcan <- monthly_Vulcan+daily_data[[hr_indx]]
            # monthly_Vulcan <- monthly_Vulcan+crop(daily_data[[hr_indx]],Annual_Vulcan)
          }
          
          #delete the downloaded file to minimize how much storage space is
          #needed while running the code (each daily file can be ~1 Gb, so >100
          #Gb would be needed otherwise)
          unlink(vulcan_file)
        }
        
        #Now that all daily files for this month have been processed, add this
        #month to the annual total and convert from monthly total to monthly
        #average (in per hr units)
        Annual_Vulcan <- Annual_Vulcan+monthly_Vulcan
        monthly_Vulcan <- monthly_Vulcan/(days_per_month[monthly_indx]*24)
        
        #save and reset the monthly_vulcan raster
        writeCDF(monthly_Vulcan,
                 file.path(destination_folder,monthly_output_list[Sector_indx*12+monthly_indx]),
                 force_v4=TRUE,
                 varname="flux_co2",
                 unit="Mg km-2 hr-1",
                 longname=paste0(sectors[Sector_indx+1],"_sector_monthly_average_combustion_CO2_emissions"),
                 missval=-9999,
                 overwrite=TRUE)
        values(monthly_Vulcan) <- 0
      }
      
      #now that every monthly file has been processed and summed into the annual
      #one, convert that to an average (per hr units)
      Annual_Vulcan <- Annual_Vulcan/8760
      
      #save and reset the annual_vulcan raster
      writeCDF(Annual_Vulcan,
               file.path(destination_folder,annual_output_list[Sector_indx+1]),
               force_v4=TRUE,
               varname="flux_co2",
               unit="Mg km-2 hr-1",
               longname=paste0(sectors[Sector_indx+1],"_sector_annual_average_combustion_CO2_emissions"),
               missval=-9999,
               overwrite=TRUE)
      values(Annual_Vulcan) <- 0
    }
    cat("\nFinished preparing Vulcan data at",difftime(Sys.time(),starttime,units = "min"),"minutes since start\n")
  }
  options(timeout=default_timeout)
}
