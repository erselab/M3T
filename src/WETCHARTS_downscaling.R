#code to disaggregate Wetcharts by a factor of ~5 using NALCMS or NLCD data.
#Assumes that NALCMS value 14 = wetlands and NLCD values > 89 = wetlands.

################################################################################
#User input

# d01_bounding_box <- cbind(c(-76.65,-73.65),c(38.97,40.97)) 
# resolution <- 0.01
#Will be used to generate a blank raster for the output to be built onto, WGS84
#CRS.  Resolution in deg, bounding box in lat/long.  Will be used as the grid
#for projectraster, would need to be defined as the grid to project to for
#XESMF.

#If blank, instead crop to the landcover extent, clipping part of AK with NALCMS
#to avoid crossing the antimeridian (longitude -180 to +180).  Will take a
#considerable amount of time to run.


Wetcharts_file <- "G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Inventories/WetCHARTs_v1_3_1_2019.nc"
# https://daac.ornl.gov/CMS/guides/MonthlyWetland_CH4_WetCHARTs.html
# landcover_file <- "G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Shapefiles/nlcd_2019_land_cover_l48_20210604/nlcd_2019_land_cover_l48_20210604.img"
# https://www.mrlc.gov/
landcover_file <- "G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Shapefiles/NALCMS_2020_land_cover/NA_NALCMS_landcover_2020_30m/data/NA_NALCMS_landcover_2020_30m.tif"
# http://www.cec.org/north-american-environmental-atlas/land-cover-30m-2015-landsat-and-rapideye/

#can use NALCMS or NLCD.

XESMF_check <- TRUE
#True/False whether you plan to use the XESMF regridder in python or use
#projectraster in R.

setwd("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed/Wetcharts")
#where to save output, including progressive output throughout processing

# aggregated_raster_name <- paste0(Sys.Date(),"_laea_0.1_deg_wetland_fraction.grd")
aggregated_raster_name <- "2023-06-02_laea_0.1_deg_wetland_fraction.grd"
#Name for partial output.  This is landcover after converting landcover to 0 for
#every type except wetlands, and wetlands to 1. The data is then aggregated from
#30 m native resolution to ~0.1 deg resolution (factor of 370) using sum and
#projected from the native landcover projection to lat/long.  Code will check if
#this file already exists and load it if so, otherwise it will calculate and
#save it. Saves processing time if rerunning only latter parts of the code.

################################################################################
#load packages
i <- 1
packagecheck <- c("raster","ncdf4","sp","rgdal","maps","fBasics","pracma","sf")

while(i<=length(packagecheck)){
  if(length(find.package(packagecheck[i],quiet = TRUE))<1){
    install.packages(packagecheck[i],repos="https://repo.miserver.it.umich.edu/cran/")
  }
  i <- i+1
}

invisible(suppressPackageStartupMessages(lapply(packagecheck, library, character.only=TRUE)))
rm(packagecheck,i)

################################################################################
#now quickly build the output raster matrix

if(exists("d01_bounding_box")){
  d01_rast <- raster(nrows=diff(range(d01_bounding_box[,2]))/resolution, 
                     ncols=diff(range(d01_bounding_box[,1]))/resolution,
                     xmn=min(d01_bounding_box[,1]), xmx=max(d01_bounding_box[,1]),
                     ymn=min(d01_bounding_box[,2]), ymx=max(d01_bounding_box[,2]), 
                     crs=4326)
  #4326 is the epsg code for lat/long projections with the WGS84 datum
  rm(d01_bounding_box,resolution)
}

################################################################################
#load in  wetcharts

Wetcharts <- list(brick(Wetcharts_file,level=1),
                  brick(Wetcharts_file,level=2),
                  brick(Wetcharts_file,level=3),
                  brick(Wetcharts_file,level=4),
                  brick(Wetcharts_file,level=5),
                  brick(Wetcharts_file,level=6),
                  brick(Wetcharts_file,level=7),
                  brick(Wetcharts_file,level=8),
                  brick(Wetcharts_file,level=9),
                  brick(Wetcharts_file,level=10),
                  brick(Wetcharts_file,level=11),
                  brick(Wetcharts_file,level=12))
#load each month separately, since each brick also has 18 layers, each a
#different modeled result

names(Wetcharts) <- sprintf("%02d",01:12)
#name the list levels appropriately.

rm(Wetcharts_file)
################################################################################
#load in the landcover - national land cover database or North
#American Land Change Monitoring System (NLCD and NALCMS)

landcover <- brick(landcover_file)

rm(landcover_file)
################################################################################
#set wetlands to a value of 1 and all other land cover to 0, and aggregate to
#0.1 deg

if(file.exists(aggregated_raster_name)){
  #this has already been done, so just load the output
  if(XESMF_check==T){
    landcover_0.1_deg_proj <- brick(gsub(".grd","_regridded.nc",
                                         aggregated_raster_name))
  }else{
    landcover_0.1_deg_proj <- brick(aggregated_raster_name)
  }
}else{
  if(any(grep("NALCMS",basename(landcover@file@name)))){
    #regex to confirm the landcover is NALCMS
    landcover_0.1_deg <- reclassify(landcover,matrix(c(0,13,0,
                                                       13.5,14.5,1,
                                                       14.6,5000,0),
                                                     ncol=3,byrow=T),
                                    include.lowest=T)
    #force all values between 0 and 13 or 14.6 to 5000 to 0.  values between 13.5
    #and 14.5 are 1.  14 = wetland land cover for NALCMS.
  }else{
    landcover_0.1_deg <- reclassify(landcover,matrix(c(0,89,0,
                                                       89,200,1),
                                                     ncol=3,byrow=T))
    #force all values between 0 and 89 to 0.  values between 89 and 200 are forced
    #to 1.  90 and 95 = wetland land cover for NLCD.
  }
  landcover_0.1_deg <- aggregate(x = landcover_0.1_deg,na.rm=T,
                                 fact = 370,
                                 fun = sum,expand=T)
  ################################################################################
  #Either save the data to project it using XESMF or regrid with projectraster
  #and save it to easily pick up mid-processing later
  
  if(XESMF_check){
    writeRaster(landcover_0.1_deg,aggregated_raster_name,
                overwrite=T)
    stop("Finished creating aggregated raster.  Regrid output with XESMF and rerun this script with the regridded file saved in the same location as aggregated_raster_name with the same name, except including _regridded.nc ")
  }else{
    #project with projectraster
    if(exists("d01_rast")){
      #user specified a domain
      template <- d01_rast
    }else{
      if(any(grep("NALCMS",basename(landcover@file@name)))){
        template <- raster(ext=extent(-179.5,-9.5,5,85),res=0.1,crs=crs(Wetcharts[[1]][[1]]))
      }else{
        template <- raster(ext=extent(-130.5,-63.5,21.5,49.5),res=0.1,crs=crs(Wetcharts[[1]][[1]]))
      }
      #NLCD is the entire US, NALCMS is the entirety of North America, excluding
      #a tiny portion of AK that crosses the antimeridian (longitude -180)
    }
    landcover_0.1_deg_proj <- projectRaster(landcover_0.1_deg,to=template,method="ngb")
    #project to a grid with the exact right resolution, extent and origin using
    #nearest neighbor, NOT interpolation, so that we do not change total
    #emissions.  This is why the data had to be aggregated to an ~0.1 deg grid
    #first.
    writeRaster(landcover_0.1_deg_proj,aggregated_raster_name,
                overwrite=T)
    rm(template)
  }#projectraster vs XESMF check
}#Load file from previous run vs run from scratch check

################################################################################
#define the domain of interest + a little buffer, crop wetcharts

for(A in 1:length(Wetcharts)){
  Wetcharts[[A]] <- crop(Wetcharts[[A]],extent(landcover_0.1_deg_proj))
}
#crop Wetcharts to the same region

rm(A)
################################################################################
#now calculate the wetland fraction

landcover_0.5_deg_proj <- aggregate(x = landcover_0.1_deg_proj,na.rm=T,
                                    fact = 5,
                                    fun = sum,expand=T)
#aggregate from 0.1 to 0.5 degrees.  Each 0.5 deg pixel = sum of 30 m wetland
#pixels that are wetlands (i.e., the fraction of the land in the pixel that is
#wetlands).

#this process was taken in part from
#https://gis.stackexchange.com/questions/262015/calculation-of-fractional-cover-for-each-vegetation-class-at-30-m-resolution-mat/262958#262958

landcover_0.5_deg_proj <- disaggregate(landcover_0.5_deg_proj,fact=5)
#convert the 0.5 deg version to a 0.1 deg version.  This is just so pixels
#align perfectly with the 0.1 version and does NOT change the values in each
#pixel.

wetland_fraction <- landcover_0.1_deg_proj/landcover_0.5_deg_proj
#now get the ratio of wetlands in each 0.1 deg pixel relative to the 0.5 deg
#pixels.  Note doing this and then projecting, rather than projecting first,
#will NOT conserve mass as the ratios within the 0.5 deg pixel will no longer
#sum exactly to 1.

wetland_fraction[is.na(wetland_fraction)] <- 1/25
#for any without a value, just distribute equally to the 25 pixels in each 0.5
#deg pixel (0/# or no data from landcover)

################################################################################
#average across months for Wetcharts.  We'll have an average for colder months
#and warmer months separately.  Cold = Oct - Apr

Cold_avg_Wetcharts <- (Wetcharts[[1]]+Wetcharts[[2]]+Wetcharts[[3]]+
                         Wetcharts[[4]]+
                         Wetcharts[[10]]+Wetcharts[[11]]+Wetcharts[[12]])/(4+3)
Cold_avg_Wetcharts <- mean(Cold_avg_Wetcharts)

Warm_avg_Wetcharts <- (Wetcharts[[5]]+Wetcharts[[6]]+Wetcharts[[7]]+
                         Wetcharts[[8]]+Wetcharts[[9]])/(5)
Warm_avg_Wetcharts <- mean(Warm_avg_Wetcharts)

################################################################################
#disaggregate Wetcharts using wetland fraction from NLCD

cold_Downscaled_Wetcharts <- disaggregate(Cold_avg_Wetcharts*area(Cold_avg_Wetcharts),fact=5)
cold_Downscaled_Wetcharts <- cold_Downscaled_Wetcharts*wetland_fraction/area(cold_Downscaled_Wetcharts)
#important - wetcharts is in flux units (which is per area).  Multiply by the
#area before downscaling, then divide by the new smaller area after downscaling.
#This conserves the total EMISSIONS, not the total FLUX.

warm_Downscaled_Wetcharts <- disaggregate(Warm_avg_Wetcharts*area(Warm_avg_Wetcharts),fact=5)
warm_Downscaled_Wetcharts <- warm_Downscaled_Wetcharts*wetland_fraction/area(warm_Downscaled_Wetcharts)
################################################################################
#write output

writeRaster(cold_Downscaled_Wetcharts,paste0(Sys.Date(),"_cold_Wetcharts"),overwrite=T)
writeRaster(warm_Downscaled_Wetcharts,paste0(Sys.Date(),"_warm_Wetcharts"),overwrite=T)
#this is the final desired output

################################################################################
#Several visuals

nice_plot <- function(input,title){
  converted_input <- input*1E9/(1000*16.043*24*60*60)
  #converting from mg/m2day (Wetcharts default) to nmol/m2s
  converted_input[converted_input<10^(min_plot)] <- 10^(min_plot)
  converted_input[converted_input>10^(max_plot)] <- 10^(max_plot)
  #max/min colorscale, assuming the input values are already log10(number)
  par(mar=c(c(5, 4, 4, 2) + 0.1 + c(0,0,2,0)))
  plot(log10(converted_input),col=timPalette(),
       main=title,
       zlim=c(min_plot,
              max_plot),
       cex.main=2,cex.axis=2,
       axis.args=list(cex.axis=2),legend.width=2)
  map("state",add=T)
  map("world",add=T)
  map("lakes",add=T)
  #add outlines for context
  if(exists("d01_rast")){
    lines(extent(d01_rast),lwd=3)
  }
}

zoomed_plot <- function(input,title){
  #identical to the above, but specifically for a small region where individual
  #pixels can be seen more clearly
  converted_input <- input*1E9/(1000*16.043*24*60*60)
  converted_input[converted_input<10^(min_plot)] <- 10^(min_plot)
  converted_input[converted_input>10^(max_plot)] <- 10^(max_plot)
  par(mar=c(c(5, 4, 4, 2) + 0.1 + c(0,0,2,0)))
  plot(log10(converted_input),col=timPalette(),
       main=title,
       zlim=c(min_plot,
              max_plot),
       cex.main=2,cex.axis=2,
       axis.args=list(cex.axis=2),legend.width=2,
       xlim=c(-80,-70),ylim=c(35,45))
  map("state",add=T)
  map("world",add=T)
  map("lakes",add=T)
}


min_plot <- -3
max_plot <- log10(max(Cold_avg_Wetcharts[Cold_avg_Wetcharts],
                      Warm_avg_Wetcharts[Warm_avg_Wetcharts],
                      cold_Downscaled_Wetcharts[cold_Downscaled_Wetcharts],
                      warm_Downscaled_Wetcharts[warm_Downscaled_Wetcharts])*
                    1E9/(1000*16.043*24*60*60))
#the minimum is a ~arbitrary value given the log scale can go quite negative.
#the max is the max across wetcharts pre and post downscaling.

png("Cold_wetcharts.png",width=480*2,height=480*2)
nice_plot(Cold_avg_Wetcharts,"Wetcharts CH4\nlog10(nmol/m2s), avg of Oct - Apr\nSaturated colorscale (high and low)")
dev.off()

png("Warm_wetcharts.png",width=480*2,height=480*2)
nice_plot(Warm_avg_Wetcharts,"Wetcharts CH4\nlog10(nmol/m2s), avg of May - Sept\nSaturated colorscale (high and low)")
dev.off()

png("Downscaled_Wetcharts_cold_months.png",width = 480*2,height = 480*2)
nice_plot(cold_Downscaled_Wetcharts,"Downscaled Wetcharts CH4\nlog10(nmol/m2s), avg of Oct - Apr\nSaturated colorscale (high and low)")
dev.off()

png("Downscaled_Wetcharts_warm_months.png",width = 480*2,height = 480*2)
nice_plot(warm_Downscaled_Wetcharts,"Downscaled Wetcharts CH4\nlog10(nmol/m2s), avg of May - Sept\nSaturated colorscale (high and low)")
dev.off()


if(!exists("d01_rast")){
  #probably won't need zoomed in plots if you're only working with a user set
  #domain
  png("Philly_Cold_wetcharts.png",width=480*2,height=480*2)
  zoomed_plot(Cold_avg_Wetcharts,"Wetcharts CH4\nlog10(nmol/m2s), avg of Oct - Apr\nSaturated colorscale (high and low)")
  dev.off()
  
  png("Philly_Warm_wetcharts.png",width=480*2,height=480*2)
  zoomed_plot(Warm_avg_Wetcharts,"Wetcharts CH4\nlog10(nmol/m2s), avg of May - Sept\nSaturated colorscale (high and low)")
  dev.off()
  
  png("Philly_Downscaled_Wetcharts_cold_months.png",width = 480*2,height = 480*2)
  zoomed_plot(cold_Downscaled_Wetcharts,"Downscaled Wetcharts CH4\nlog10(nmol/m2s), avg of Oct - Apr\nSaturated colorscale (high and low)")
  dev.off()
  
  png("Philly_Downscaled_Wetcharts_warm_months.png",width = 480*2,height = 480*2)
  zoomed_plot(warm_Downscaled_Wetcharts,"Downscaled Wetcharts CH4\nlog10(nmol/m2s), avg of May - Sept\nSaturated colorscale (high and low)")
  dev.off()
}


png("wetland_fraction.png",width = 480*2,height = 480*2)
plot(wetland_fraction,col=timPalette(),
     main="Wetland Fraction\n0.1deg pixel / 0.5deg pixel (0 to 1)",
     zlim=c(0,1),
     cex.main=2,cex.axis=2,
     axis.args=list(cex.axis=2),legend.width=2)
map("state",add=T)
map("world",add=T)
if(exists("d01_rast")){
  lines(extent(d01_rast),lwd=3)
}
dev.off()
################################################################################
#Quick sanity checks

domain_total <- data.frame("Cold"=cellStats(Cold_avg_Wetcharts*area(Cold_avg_Wetcharts)*1000*1000,sum)/1E9,
                           "Cold_downscaled"=cellStats(cold_Downscaled_Wetcharts*area(cold_Downscaled_Wetcharts)*1000*1000,sum)/1E9,
                           "Warm"=cellStats(Warm_avg_Wetcharts*area(Warm_avg_Wetcharts)*1000*1000,sum)/1E9,
                           "Warm_downscaled"=cellStats(warm_Downscaled_Wetcharts*area(warm_Downscaled_Wetcharts)*1000*1000,sum)/1E9)
#simple check.  The total emissions in the domain (converted from mg/m2day to
#Mega gram/day) should be the same pre and post downscaling.  Multiply by area
#to cancel out the per area unit (flux -> emission rate).  area is in km2, so
#1000*1000 to convert to m2.

if(!(domain_total[1]>=(domain_total[2]*0.95) & domain_total[1]<=(domain_total[2]*1.05) | 
     domain_total[3]>=(domain_total[4]*0.95) & domain_total[3]<=(domain_total[4]*1.05))){
  View(domain_total)
  stop("Something's gone wrong.  The total emissions (MT/day) across the domain differs between the original and downscaled wetcharts.")
}

cold_Downscaled_Wetcharts[is.na(cold_Downscaled_Wetcharts)]=0
Cold_avg_Wetcharts[is.na(Cold_avg_Wetcharts)]=0
warm_Downscaled_Wetcharts[is.na(warm_Downscaled_Wetcharts)]=0
Warm_avg_Wetcharts[is.na(Warm_avg_Wetcharts)]=0
#NA's can't be used in math or the result is also an NA.  Force all NA's across
#both wetcharts and downscaled wetcharts to 0 so we can be sure there aren't any
#cases where there's an NA in one product, not the other

if(max(abs(values(aggregate(cold_Downscaled_Wetcharts*area(cold_Downscaled_Wetcharts),fact=5,fun=sum) - 
                  Cold_avg_Wetcharts*area(Cold_avg_Wetcharts))),na.rm=T)>1E-5){
  check1=TRUE
}else{
  check1=FALSE
}
if(max(abs(values(aggregate(warm_Downscaled_Wetcharts*area(warm_Downscaled_Wetcharts),fact=5,fun=sum) - 
                  Warm_avg_Wetcharts*area(Warm_avg_Wetcharts))),na.rm=T)>1E-5){
  check2=TRUE
}else{
  check2=FALSE
}

if(check1 & check2){
  stop("Something's gone wrong.  There are pixels that differ between downscaled and original wetcharts (both warm and cold months) when aggregating the downscaled values back to the original resolution.")
}else if(check1){
  stop("Something's gone wrong.  There are pixels that differ between downscaled and original wetcharts (cold months) when aggregating the downscaled values back to the original resolution.")
}else if(check2){
  stop("Something's gone wrong.  There are pixels that differ between downscaled and original wetcharts (warm months) when aggregating the downscaled values back to the original resolution.")
}



