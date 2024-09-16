

################################################################################
#Manually defined variables

old_ACES_output <- "G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Inventories/ACES_v2.0"
new_ACES_output <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw_data_rewrite/ACES"
old_Vulcan_output <- "G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Inventories/Vulcan_v3.0"
new_Vulcan_output <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw_data_rewrite/Vulcan"

domain=as.data.frame(cbind(c(-76.65,-73.65),
                           c(38.97,40.97)))
domain_res=0.01
domain_crs="epsg:4326" #lat/long

################################################################################
#load packages
i <- 1
packagecheck <- c("raster","ncdf4","terra")
while(i<=length(packagecheck)){
  if(length(find.package(packagecheck[i],quiet = TRUE))<1){
    install.packages(packagecheck[i],repos="https://repo.miserver.it.umich.edu/cran/")
  }
  i <- i+1
}

invisible(suppressPackageStartupMessages(lapply(packagecheck, require, character.only=TRUE)))
rm(packagecheck,i)

#raster + ncdf4 = raster and .nc filetype functionalities
#sf = additional spatial functionalities
#readxl = ability to load more excel filetypes flexibly


################################################################################
#create the domain and set it to all NaN
if(length(domain_res)==1){
  domain_res <- rep(domain_res,2)
}

if(class(domain)=="SpatRaster"){
  values(domain) <- NaN
}else if(class(domain)=="data.frame"){
  domain <- rast(nrows=diff(range(domain[,2]))/domain_res[2], 
                 ncols=diff(range(domain[,1]))/domain_res[1],
                 xmin=min(domain[,1]), xmax=max(domain[,1]),
                 ymin=min(domain[,2]), ymax=max(domain[,2]), 
                 crs=domain_crs)
  rm(domain_res,domain_crs)
}

################################################################################
#first, let's just check that annual results match sum(monthly results)

divergent <- colorRampPalette(c("red","white","blue"))

monthly_ACES_data <- list.files(new_ACES_output,full.names = T,pattern = "monthly")
annual_ACES_data <- list.files(new_ACES_output,full.names = T,pattern="annual")

calculate_days_in_month <- function(x){
  as.numeric(as.Date(cut(x+34, "month")) - as.Date(cut(x, "month")))
}
days_per_month <- calculate_days_in_month(as.Date(paste0("2017-",1:12,"-01")))
hrs_per_month <- days_per_month*24

sectors <- c("Air","Commercial","Elec","Industrial","Marine","Nonroad","Oilgas",
             "Onroad","Rail","Residential","Total") #all sectors

for(Sector_indx in 1:(length(sectors))){
  monthly_data <- rast(monthly_ACES_data[grep(sectors[Sector_indx],monthly_ACES_data)])
  monthly_data <- crop(monthly_data,project(as.polygons(domain),crs(monthly_data)))
  monthly_data <- monthly_data*hrs_per_month
  monthly_data <- sum(monthly_data)/8760
  
  annual_data <- rast(annual_ACES_data[grep(sectors[Sector_indx],annual_ACES_data)])
  annual_data <- crop(annual_data,project(as.polygons(domain),crs(monthly_data)))
  
  delta <- annual_data - monthly_data
  plot(delta,main=paste0(sectors[Sector_indx],"\nannual - sum(monthly)"),
       range=unlist(global(abs(delta),max,na.rm=T))*c(-1,1),
       col=divergent(64),mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
  cat(sectors[Sector_indx],"max=",max(unlist(c(global(annual_data,max,na.rm=T),global(monthly_data,max,na.rm=T)))))
  cat("\nAnnual=",unlist(global(annual_data,sum,na.rm=T)),"monthly=",unlist(global(monthly_data,sum,na.rm=T)))
  cat("\nAnnual/monthly=",unlist(global(annual_data,sum,na.rm=T)/global(monthly_data,sum,na.rm=T)),"\n\n")
}
################################################################################
#repeat for Vulcan

monthly_Vulcan_data <- list.files(new_Vulcan_output,full.names = T,pattern = "monthly")
annual_Vulcan_data <- list.files(new_Vulcan_output,full.names = T,pattern="annual")

days_per_month <- calculate_days_in_month(as.Date(paste0("2015-",1:12,"-01")))
hrs_per_month <- days_per_month*24

sectors <- c("airport","cement","cmv","commercial","elec_prod","industrial","nonroad",
             "onroad","rail","residential","total") #all sectors

for(Sector_indx in 1:(length(sectors))){
  monthly_data <- rast(monthly_Vulcan_data[grep(sectors[Sector_indx],monthly_Vulcan_data)])
  monthly_data <- crop(monthly_data,project(as.polygons(domain),crs(monthly_data)))
  monthly_data <- monthly_data*hrs_per_month
  monthly_data <- sum(monthly_data)/8760
  
  annual_data <- rast(annual_Vulcan_data[grep(sectors[Sector_indx],annual_Vulcan_data)])
  annual_data <- crop(annual_data,project(as.polygons(domain),crs(monthly_data)))
  
  delta <- annual_data - monthly_data
  plot(delta,main=paste0(sectors[Sector_indx],"\nannual - sum(monthly)"),
       range=unlist(global(abs(delta),max,na.rm=T))*c(-1,1),
       col=divergent(64),mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
  cat(sectors[Sector_indx],"max=",max(unlist(c(global(annual_data,max,na.rm=T),global(monthly_data,max,na.rm=T)))))
  cat("\nAnnual=",unlist(global(annual_data,sum,na.rm=T)),"monthly=",unlist(global(monthly_data,sum,na.rm=T)))
  cat("\nAnnual/monthly=",unlist(global(annual_data,sum,na.rm=T)/global(monthly_data,sum,na.rm=T)),"\n\n")
}

################################################################################
#Next, let's check that the total sector matches the sum(sectors)

for(monthly_indx in sprintf("%02d",1:12)){
  monthly_data <- monthly_ACES_data[grep(paste0("_",monthly_indx,".nc"),monthly_ACES_data)]
  monthly_sector_data <- rast(monthly_data[-length(monthly_data)])
  monthly_total_data <- rast(tail(monthly_data,1))
  
  monthly_sector_data <- crop(monthly_sector_data,project(as.polygons(domain),crs(monthly_sector_data)))
  monthly_total_data <- crop(monthly_total_data,project(as.polygons(domain),crs(monthly_sector_data)))
  
  monthly_sector_data <- sum(monthly_sector_data)
  
  delta <- monthly_total_data - monthly_sector_data
  plot(delta,main=paste0(monthly_indx,"\ntotal - sum(sectors)"),
       range=unlist(global(abs(delta),max,na.rm=T))*c(-1,1),
       col=divergent(64),mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
  cat(monthly_indx,"max=",max(global(monthly_total_data,max,na.rm=T)))
  cat("\nTotal/sectoral=",unlist(global(monthly_total_data,sum,na.rm=T)/global(monthly_sector_data,sum,na.rm=T)),"\n\n")
}

################################################################################
#Repeat for Vulcan

# for(monthly_indx in sprintf("%02d",1:12)){
for(monthly_indx in 1:12){
  monthly_data <- monthly_Vulcan_data[grep(paste0("_",monthly_indx,".nc"),monthly_Vulcan_data)]
  monthly_sector_data <- rast(monthly_data[-length(monthly_data)])
  monthly_total_data <- rast(tail(monthly_data,1))
  
  monthly_sector_data <- crop(monthly_sector_data,project(as.polygons(domain),crs(monthly_sector_data)))
  monthly_total_data <- crop(monthly_total_data,project(as.polygons(domain),crs(monthly_sector_data)))
  
  monthly_sector_data <- sum(monthly_sector_data)
  
  delta <- monthly_total_data - monthly_sector_data
  plot(delta,main=paste0(monthly_indx,"\ntotal - sum(sectors)"),
       range=unlist(global(abs(delta),max,na.rm=T))*c(-1,1),
       col=divergent(64),mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
  cat(monthly_indx,"max=",max(global(monthly_total_data,max,na.rm=T)))
  cat("\nTotal/sectoral=",unlist(global(monthly_total_data,sum,na.rm=T)/global(monthly_sector_data,sum,na.rm=T)),"\n\n")
}


################################################################################
#Finally, let's compare to the old version of these outputs as calculated on the
#Seawulf

annual_ACES_data <- list.files(new_ACES_output,full.names = T,pattern="annual")
divergent <- colorRampPalette(c("red","white","blue"))

#old work didn't include the total
annual_ACES_data <- annual_ACES_data[-length(annual_ACES_data)]
old_annual_ACES_data <- list.files(file.path(old_ACES_output,"/Sectoral"),full.names = T,pattern="Annual")

sectors <- c("Air","Commercial","Elec","Industrial","Marine","Nonroad","Oilgas",
             "Onroad","Rail","Residential") #all sectors - total

for(sector_indx in 1:length(annual_ACES_data)){
  annual_ACES <- rast(annual_ACES_data[sector_indx])
  annual_ACES <- crop(annual_ACES,project(as.polygons(domain),crs(annual_ACES)))
  
  old_annual_ACES <- rast(old_annual_ACES_data[sector_indx])
  old_annual_ACES <- flip(old_annual_ACES)
  crs(old_annual_ACES) <- "+proj=lcc +lat_0=40 +lon_0=-97 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
  old_annual_ACES <- crop(old_annual_ACES,project(as.polygons(domain),crs(annual_ACES)))
  
  delta <- old_annual_ACES - annual_ACES
  plot(delta,main=paste0(sectors[sector_indx],"\nold - new"),
       range=unlist(global(abs(delta),max,na.rm=T))*c(-1,1),
       col=divergent(64),mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
  cat(sectors[sector_indx],"max=",max(unlist(c(global(annual_ACES,max,na.rm=T),global(old_annual_ACES,max,na.rm=T)))))
  cat("\nNew/old=",unlist(global(annual_ACES,sum,na.rm=T)/global(old_annual_ACES,sum,na.rm=T)),"\n\n")
}

################################################################################
#Repeat for Vulcan

sectors <- c("airport","cement","cmv","commercial","elec_prod","industrial","nonroad",
             "onroad","rail","residential","total") #all sectors

annual_Vulcan_data <- list.files(new_Vulcan_output,full.names = T,pattern="annual")
old_annual_Vulcan_data <- list.files(file.path(old_Vulcan_output,"/Sectoral"),full.names = T,pattern="2015_Annual_Vulcan")

#only a few can be compared at this point
annual_Vulcan_data <- annual_Vulcan_data[c(4,6)]
sectors <- sectors[c(4,6)]
old_annual_Vulcan_data <- old_annual_Vulcan_data[1:2]

for(sector_indx in 1:length(annual_Vulcan_data)){
  annual_Vulcan <- rast(annual_Vulcan_data[sector_indx])
  old_annual_Vulcan <- rast(old_annual_Vulcan_data[sector_indx])
  old_annual_Vulcan <- flip(old_annual_Vulcan)
  crs(old_annual_Vulcan) <- crs(annual_Vulcan)
  
  annual_Vulcan <- crop(annual_Vulcan,project(as.polygons(domain),crs(old_annual_Vulcan)))
  old_annual_Vulcan <- crop(old_annual_Vulcan,project(as.polygons(domain),crs(old_annual_Vulcan)))
  
  #old vulcan was the annual TOTAL, not annual average.  Fix for comparison.
  old_annual_Vulcan <- old_annual_Vulcan/8760
  
  delta <- old_annual_Vulcan - annual_Vulcan
  plot(delta,main=paste0(sectors[sector_indx],"\nold - new"),
       range=unlist(global(abs(delta),max,na.rm=T))*c(-1,1),
       col=divergent(64),mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
  cat(sectors[sector_indx],"max=",max(unlist(c(global(annual_Vulcan,max,na.rm=T),global(old_annual_Vulcan,max,na.rm=T)))))
  cat("\nNew/old=",unlist(global(annual_Vulcan,sum,na.rm=T)/global(old_annual_Vulcan,sum,na.rm=T)),"\n\n")
}



