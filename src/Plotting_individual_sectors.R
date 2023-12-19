#build functions to plot up various sectors as they finish running.  
################################################################################
#Manually defined variables

Input_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed/"
Output_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Figures/"

# # load in the urban area and state outline data from US census
# state_outline_file <- "G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Shapefiles/tl_2022_us_state/tl_2022_us_state.shp"
# county_outline_file <- "G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Shapefiles/tl_2022_us_county/tl_2022_us_county.shp"
# include water areas - actual boundaries

state_outline_file <- "G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Shapefiles/cb_2021_us_state_500k/cb_2021_us_state_500k.shp"
county_outline_file <- "G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Shapefiles/cb_2021_us_county_500k/cb_2021_us_county_500k.shp"

city_shapefile <- "G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Shapefiles/tl_2022_us_uac10/tl_2022_us_uac10.shp"
#exclude water areas - cartographic boundaries (smaller, somewhat simplified)

city_name <- "Philadelphia, PA--NJ--DE--MD"
#must match exactly a value from us census (run city_bounds$NAME10 after loading
#it in to see all options).  Main focus region to highlight

state_FIPS <- data.frame("State"=c("DE", "MD", "NJ", "NY", "PA"),
                         "FIPS"=c(10,24,34,36,42))
#Data for all states in domain.  Found on census website
#https://www.census.gov/library/reference/code-lists/ansi.html

d03_bounding_box <- cbind(c(-76.65,-73.65),c(38.97,40.97))
resolution <- 0.01

################################################################################
#load packages
packagecheck <- c("raster","ncdf4","fBasics","sf")
for(i in length(packagecheck)){
  if(length(find.package(packagecheck[i],quiet = TRUE))<1){
    install.packages(packagecheck[i])
  }
}

invisible(suppressPackageStartupMessages(lapply(packagecheck, library, character.only=TRUE)))
rm(packagecheck,i)

#raster = raster filetype functionalities
#sf = spatial functionalities including OGR
#ncdf4 = .nc filetype functionalities
#fbasics = nice colorscale
################################################################################
#now quickly build the output raster matrix

d03_rast <- raster(nrows=diff(range(d03_bounding_box[,2]))/resolution, 
                   ncols=diff(range(d03_bounding_box[,1]))/resolution,
                   xmn=min(d03_bounding_box[,1]), xmx=max(d03_bounding_box[,1]),
                   ymn=min(d03_bounding_box[,2]), ymx=max(d03_bounding_box[,2]), 
                   crs=4326)

rm(d03_bounding_box,resolution)
################################################################################
#write functions to plot each up in a useful way
dir.create(Output_directory,showWarnings = F)
setwd(Output_directory)

city_bounds <- st_read(city_shapefile)
city_bounds <- city_bounds[city_bounds$NAME10==city_name,]

state_bounds <- st_read(state_outline_file)

county_outline <- st_read(county_outline_file)
county_outline <- county_outline[county_outline$STATEFP%in% state_FIPS[,2],]

prep_data <- function(input){
  output <- input
  output <- log10(output)
  output[is.infinite(output)] <- NA
  return(output)
}

log_plot <- function(input,title,zlim_min=NULL,zlim_max=NULL,
                     filename){
  if(missing(filename)){
    outputname <- substitute(input)
  }else{
    outputname <- filename
  }
  
  input <- prep_data(input)
  
  png(paste0(outputname,".png"),width = 480*2,height=480*2)
  par(mar=c(5, 4, 4, 2) + 0.1 + c(0,1,2,1))
  plot(input,col=timPalette(),colNA="black",
       main=title,
       legend.args=list(text="log10(nmol/m2/s)",side=2,line=0.5,cex=2),
       axis.args=list(cex.axis=2),legend.width=1.2*2,
       xlab="Longitude",ylab="Latitude",
       cex.main=2,cex.axis=2,cex.lab=2,
       smallplot= c(.9,.93,0.25,0.75),
       zlim=c(zlim_min,zlim_max))
  plot(county_outline[,1],add=T,border="dimgrey",col=NA)
  plot(state_bounds[,1],add=T,border="white",lwd=2,col=NA)
  plot(city_bounds[,1],add=T,border="darkgrey",col=NA)
  dev.off()
}
not_log_plot <- function(input,title,zlim_min=NULL,zlim_max=NULL,
                         filename){
  if(missing(filename)){
    outputname <- substitute(input)
  }else{
    outputname <- filename
  }
  
  input[values(input)==0] <- NA
  
  png(paste0(outputname,".png"),width = 480*2,height=480*2)
  par(mar=c(5, 4, 4, 2) + 0.1 + c(0,1,2,1))
  plot(input,col=timPalette(),colNA="black",
       main=title,
       legend.args=list(text="nmol/m2/s",side=2,line=0.5,cex=2),
       axis.args=list(cex.axis=2),legend.width=1.2*2,
       xlab="Longitude",ylab="Latitude",
       cex.main=2,cex.axis=2,cex.lab=2,
       smallplot= c(.9,.93,0.25,0.75),
       zlim=c(zlim_min,zlim_max))
  plot(county_outline[,1],add=T,border="dimgrey",col=NA)
  plot(state_bounds[,1],add=T,border="white",lwd=2,col=NA)
  plot(city_bounds[,1],add=T,border="darkgrey",col=NA)
  dev.off()
}
d01_plot <- function(input,title,zlim_min=NULL,zlim_max=NULL,
                     filename){
  if(missing(filename)){
    outputname <- substitute(input)
  }else{
    outputname <- filename
  }
  
  input <- prep_data(input)
  
  png(paste0(outputname,".png"),width = 480*2,height=480*2)
  par(mar=c(5, 4, 4, 2) + 0.1 + c(0,1,2,1))
  plot(input,col=timPalette(),colNA="black",
       main=title,
       legend.args=list(text="log10(nmol/m2/s)",side=2,line=0.5,cex=2),
       axis.args=list(cex.axis=2),legend.width=1.2*2,
       xlab="Longitude",ylab="Latitude",
       cex.main=2,cex.axis=2,cex.lab=2,
       smallplot= c(.9,.93,0.25,0.75),
       zlim=c(zlim_min,zlim_max))
  map("state",col="lightgrey",add=T,lwd=2)
  lines(city_bounds,col="darkgrey")
  dev.off()
}

