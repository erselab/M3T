# Calculate NLCD fractions for the states in the d03 domain
## Finalized: 2023-02-03

################################################################################
#Manually defined variables

Input_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw data/"
Output_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed/"

#Will be used to generate a blank raster for the output to be built onto, WGS84
#CRS.  Resolution in deg, bounding box in lat/long.
domain=as.data.frame(cbind(c(-76.65,-73.65),
                           c(38.97,40.97)))
domain_res=0.01
domain_crs="epsg:4326" #lat/long

State_subset <- c("DE", "MD", "NJ", 'NY', "PA")
#state acronyms for the domain of interest

nlcd <- file.path("G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Shapefiles",
                  "nlcd_2019_land_cover_l48_20210604/nlcd_2019_land_cover_l48_20210604.img")
#national land cover data at 30 m (https://www.usgs.gov/centers/eros/science/national-land-cover-database)

state_shapefile <- "G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Shapefiles/tl_2022_us_state/tl_2022_us_state.shp"
#Census tigerline shapefiles for states

################################################################################
#load packages
i <- 1
packagecheck <- c("raster","sf","terra")
while(i<=length(packagecheck)){
  if(length(find.package(packagecheck[i],quiet = TRUE))<1){
    install.packages(packagecheck[i],repos="https://repo.miserver.it.umich.edu/cran/")
  }
  i <- i+1
}

invisible(suppressPackageStartupMessages(lapply(packagecheck, require, character.only=TRUE)))
rm(packagecheck,i)

#raster = raster and data functionalities
#sf = simple features, adds spatial functionalities
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
domain=raster(domain)

{
  ################################################################################
  #subset NLCD to the appropriate states
  
  nlcd <- brick(nlcd)
  
  # Make a polygon for d03 (with a small buffer just to be safe)
  area_x_coords <- c(xmin(domain),xmax(domain),xmax(domain),xmin(domain))+c(-0.5,0.5,0.5,-0.5)
  area_y_coords <- c(ymin(domain),ymin(domain),ymax(domain),ymax(domain))+c(-0.5,-0.5,0.5,0.5)
  xym <- cbind(area_x_coords,area_y_coords)
  p <- Polygon(xym)
  ps <- Polygons(list(p),1)
  crop_area <- SpatialPolygons(list(ps))
  proj4string(crop_area) <- CRS(SRS_string="EPSG:4326")  # WGS84
  crop_area_trans <- spTransform(crop_area, crs(nlcd))
  
  # Read state file and reproject
  states <- st_read(state_shapefile)
  states <- as(states,"Spatial")
  states_trans <- spTransform(states,crs(nlcd))
  
  #crop/mask the nlcd to the states in the domain so that it's a bit more
  #manageable in size from the get-go
  state_poly <- subset(states_trans, STUSPS %in% State_subset)
  # First crop to state polygon
  nlcd <- crop(nlcd,state_poly,snap='out')
  # Now mask
  nlcd <- mask(nlcd, state_poly, updatevalue=0)
  
  ################################################################################
  # Make new rasters for Developed, Open Space, and Developed, Low Intensity
  # Do everything one at a time to avoid memory issues
  nlcd_open <- nlcd
  nlcd_open[] <- 0
  nlcd_open[nlcd==21] <- 1
  nlcd_low_int <- nlcd
  nlcd_low_int[] <- 0
  nlcd_low_int[nlcd==22] <- 1
  nlcd_suburban <- nlcd_low_int + nlcd_open
  remove(nlcd,nlcd_low_int,nlcd_open);gc()
}
{
  ################################################################################
  #load in the landcover - national land cover database (NLCD)
  nlcd <- file.path("G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Shapefiles",
                    "nlcd_2019_land_cover_l48_20210604/nlcd_2019_land_cover_l48_20210604.img")
  
  NLCD <- rast(nlcd)
  # reproject states to proper CRS
  State_Tigerlines <- vect(state_shapefile)
  State_Tigerlines <- mask(State_Tigerlines,mask=as.polygons(rast(domain)))
  NLCD_states_trans <- project(State_Tigerlines,crs(NLCD))
  #crop to the states in the domain so that it's a bit more
  #manageable in size from the get-go.  Add a slight buffer.
  NLCD <- crop(NLCD,1.01*ext(NLCD_states_trans))
  
  rm(State_Tigerlines);gc()
  ################################################################################
  #Make a new raster for only Developed, Open Space, and Developed, Low
  #Intensity (21 and 22 of NLCD): summed
  
  NLCD_suburban <- NLCD
  NLCD_suburban <- classify(NLCD_suburban,matrix(ncol=3,c(0,20.5,0,
                                                          20.5,22.5,1,
                                                          22.5,1000,0),byrow=T))
  rm(NLCD);gc()
}

################################################################################
#Now compare the old and new processed input data
divergent <- colorRampPalette(c("red","white","blue"))

old_comparison <- rast(nlcd_suburban)
new_comparison <- mask(NLCD_suburban,NLCD_states_trans)
new_comparison <- crop(new_comparison,ext(old_comparison))

delta <- old_comparison - new_comparison
delta[delta==0] <- NA

plot(delta,main="old - new",range=c(-1,1),colNA="black",
     col="red",mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
lines(NLCD_states_trans,col="white")

#only 2-3 pixels ON the border of MD and VA - so just a minor mask difference
#that doesn't really matter right now as new doesn't mask until later anyway.

rm(old_comparison,new_comparison,delta);gc()

{
  ################################################################################
  #Finally calculate the sum total for each state and crop to d03 - do so in
  #both approaches simultaneously and compare stepwise, otherwise we'd need a
  #lot of memory to hold all of the data
  
  subset_tmpfile <- tempfile(fileext = ".tif")
  for(A in 1:length(State_subset)){
    #old names, but new process - the old approach does not produce identical
    #output and I need to compare further processing.  Just masking to 1 state.
    state_sub_poly <- subset(NLCD_states_trans, NLCD_states_trans$STUSPS==State_subset[A])
    suburban_d03 <- crop(rast(nlcd_suburban),state_sub_poly,snap='out')
    suburban_d03 <- mask(suburban_d03,state_sub_poly,touches=F)
    suburban_d03 <- raster(suburban_d03)

    #new - same process, slightly different names
    state_sub_poly_new <- subset(NLCD_states_trans, NLCD_states_trans$STUSPS==State_subset[A])
    NLCD_suburban_subset <- crop(NLCD_suburban,state_sub_poly_new,snap='out')
    NLCD_suburban_subset <- mask(NLCD_suburban_subset,state_sub_poly_new,touches=F)

    
    # #compare the masking/cropping of the individual state - commented out as it
    # #could not be made exactly identical, although it was quite similar.
    # 
    # #old - first just pull out 1 state
    # state_sub_poly <- subset(state_poly, STUSPS==State_subset[A])
    # suburban_d03 <- crop(nlcd_suburban,state_sub_poly,snap='out')
    # suburban_d03 <- mask(suburban_d03,state_sub_poly,snap='in')
    # 
    # #new - same process, slightly different code
    # state_sub_poly_new <- subset(NLCD_states_trans, NLCD_states_trans$STUSPS==State_subset[A])
    # NLCD_suburban_subset <- crop(NLCD_suburban,state_sub_poly_new,snap='out')
    # NLCD_suburban_subset <- mask(NLCD_suburban_subset,state_sub_poly_new,touches=F)
    # 
    # old_comparison <- rast(suburban_d03)
    # new_comparison <- NLCD_suburban_subset
    # ext(old_comparison) == ext(new_comparison)
    # 
    # old_comparison[is.na(old_comparison)] <- 0
    # new_comparison[is.na(new_comparison)] <- 0
    # 
    # delta <- old_comparison - new_comparison
    # delta[delta==0] <- NA
    # 
    # #odd, but plot claims the raster is empty, yet there are some non zero
    # #values (very few)
    # # plot(delta,main="old - new",range=c(-1,1),colNA="black",
    # #      col="red",mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
    # 
    # #Just compare how many septic pixels there are in each, slightly different
    # #(and some are in 1, but not the other, so there's both some missing and
    # #some added from new to old)
    # global(new_comparison,sum,na.rm=T)
    # global(old_comparison,sum,na.rm=T)
    # 
    # #a way to actually visualize, since delta can't be plotted.  Creates pixels
    # #area wide boxes around all pixels that aren't equal between the two.
    # #Always on the state border.
    # plot(new_comparison)
    # differences <- as.numeric(row.names(as.data.frame(delta)))
    # length(differences)
    # pixels <- 30
    # for(B in 1:length(differences)){
    #   lines(ext(c(crds(delta[differences[B],drop=F])[1]+30*pixels*c(-1,1),crds(delta[differences[B],drop=F])[2]+30*pixels*c(-1,1))))
    # }
    # 
    # #isolating a particular group in DE to investigate.  Can visually compare the 3 plots, add gridlines, and add state lines.
    # plot(new_comparison,xlim=c(1.7865E6,1.788E6),ylim=c(1.9095E6,1.9105E6))
    # plot(old_comparison,xlim=c(1.7865E6,1.788E6),ylim=c(1.9095E6,1.9105E6))
    # plot(delta,xlim=c(1.7865E6,1.788E6),ylim=c(1.9095E6,1.9105E6),main="old - new")
    # abline(h=seq(1.9095E6,1.9105E6,by=30))
    # abline(v=seq(1.7865E6,1.788E6,by=30))
    # lines(NLCD_states_trans,col="red")

    #crop to domain
    suburban_d03 <- rast(crop(suburban_d03,crop_area_trans,snap='out'))

    #project with projectraster
    template <- domain
    # reprojected_suburban <- aggregate(x = suburban_d03,na.rm=T,
    #                                   fact = 37,
    #                                   fun = mean,expand=T)
    # reprojected_suburban <- projectRaster(reprojected_suburban,to=template,method="ngb")
    suburban_d03[is.na(suburban_d03)] <- 0
    suburban_d03 <- extend(suburban_d03,fill=0,
                                   ext(suburban_d03)+(res(project(rast(domain),crs(suburban_d03)))*5))
    reprojected_suburban <- raster(project(suburban_d03,rast(domain),method="average"))
    #project to a grid with the exact right resolution, extent and origin using
    #nearest neighbor, NOT interpolation, so that we do not change totals.  This
    #is why the data have to be aggregated to an ~0.01 deg grid first.
    rm(template)
    
    
    

    
    NLCD_suburban_subset[is.na(NLCD_suburban_subset)] <- 0
    NLCD_suburban_subset <- extend(NLCD_suburban_subset,fill=0,
                                   ext(NLCD_suburban_subset)+(res(project(rast(domain),crs(NLCD_suburban)))*5))
    
    # writeRaster(NLCD_suburban_subset,subset_tmpfile)
    # NLCD_suburban_subset <- rast(subset_tmpfile)
    
    #project to the exact domain (resolution, origin, extent, etc.) using an
    #average.  Represents the fractional coverage of wetlands in each pixel (0 -
    #1).
    NLCD_suburban_reprojected <- project(NLCD_suburban_subset,rast(domain),method="average")
    
    
    
    
    old_comparison <- rast(reprojected_suburban)
    new_comparison <- NLCD_suburban_reprojected
    ext(old_comparison) == ext(new_comparison)

    old_comparison[is.na(old_comparison)] <- 0
    new_comparison[is.na(new_comparison)] <- 0

    delta <- old_comparison - new_comparison
    delta[delta==0] <- NA

    #odd, but plot claims the raster is empty, yet there are some non zero
    #values (very few)
    plot(delta,main="old - new",range=c(-1,1),colNA="black",
         col=divergent(64),mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))

    #Just compare how many septic pixels there are in each, slightly different
    #(and some are in 1, but not the other, so there's both some missing and
    #some added from new to old)
    global(new_comparison,sum,na.rm=T)
    global(old_comparison,sum,na.rm=T)

  }
  
}
################################################################################
#Same, but this time use the different input to check its impact
divergent <- colorRampPalette(c("red","white","blue"))

old_comparison <- rast(nlcd_suburban)
new_comparison <- mask(NLCD_suburban,NLCD_states_trans)
new_comparison <- crop(new_comparison,ext(old_comparison))

delta <- old_comparison - new_comparison
delta[delta==0] <- NA

plot(delta,main="old - new",range=c(-1,1),colNA="black",
     col="red",mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
lines(NLCD_states_trans,col="white")

#only 2-3 pixels ON the border of MD and VA - so just a minor mask difference
#that doesn't really matter right now as new doesn't mask until later anyway.

rm(old_comparison,new_comparison,delta);gc()

{
  ################################################################################
  #Finally calculate the sum total for each state and crop to d03 - do so in
  #both approaches simultaneously and compare stepwise, otherwise we'd need a
  #lot of memory to hold all of the data
  
  subset_tmpfile <- tempfile(fileext = ".tif")
  for(A in 1:length(State_subset)){
    #old - first just pull out 1 state
    state_sub_poly <- subset(state_poly, STUSPS==State_subset[A])
    suburban_d03 <- crop(nlcd_suburban,state_sub_poly,snap='out')
    suburban_d03 <- mask(suburban_d03,state_sub_poly,snap='in')

    #new - same process, slightly different code
    state_sub_poly_new <- subset(NLCD_states_trans, NLCD_states_trans$STUSPS==State_subset[A])
    NLCD_suburban_subset <- crop(NLCD_suburban,state_sub_poly_new,snap='out')
    NLCD_suburban_subset <- mask(NLCD_suburban_subset,state_sub_poly_new,touches=F)

    #crop to domain
    suburban_d03 <- rast(crop(suburban_d03,crop_area_trans,snap='out'))
    
    #project with projectraster
    template <- domain
    reprojected_suburban <- aggregate(x = suburban_d03,na.rm=T,
                                      fact = 37,
                                      fun = mean,expand=T)
    reprojected_suburban <- project(reprojected_suburban,rast(template),method="near")
    #project to a grid with the exact right resolution, extent and origin using
    #nearest neighbor, NOT interpolation, so that we do not change totals.  This
    #is why the data have to be aggregated to an ~0.01 deg grid first.

    

    NLCD_suburban_subset[is.na(NLCD_suburban_subset)] <- 0
    NLCD_suburban_subset <- extend(NLCD_suburban_subset,fill=0,
                                   ext(NLCD_suburban_subset)+(res(project(rast(domain),crs(NLCD_suburban)))*5))
    
    #to avoid memory issues with NY, sometimes PA
    writeRaster(NLCD_suburban_subset,subset_tmpfile)
    NLCD_suburban_subset <- rast(subset_tmpfile)
    
    #project to the exact domain (resolution, origin, extent, etc.) using an
    #average.  Represents the fractional coverage of wetlands in each pixel (0 -
    #1).
    NLCD_suburban_reprojected <- project(NLCD_suburban_subset,rast(domain),method="average")
    
    
    
    
    old_comparison <- reprojected_suburban
    new_comparison <- NLCD_suburban_reprojected
    ext(old_comparison) == ext(new_comparison)
    
    old_comparison[is.na(old_comparison)] <- 0
    new_comparison[is.na(new_comparison)] <- 0
    
    delta <- old_comparison - new_comparison
    delta[delta==0] <- NA
    
    #odd, but plot claims the raster is empty, yet there are some non zero
    #values (very few)
    plot(delta,main="old - new",range=c(-1,1),colNA="black",
         col=divergent(64),mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
    
    #Just compare how many septic pixels there are in each, slightly different
    #(and some are in 1, but not the other, so there's both some missing and
    #some added from new to old)
    cat("\n",unlist(global(new_comparison,sum,na.rm=T)))
    cat("\n",unlist(global(old_comparison,sum,na.rm=T)))
    unlink(subset_tmpfile)
  }
  
}
