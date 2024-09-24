
setwd("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_rewrite")
terradata <- list.files(pattern=".nc",full.names = T)
terradata <- terradata[-grep("MSW",terradata)]
testdata <- list.files("testing/",pattern=".nc",full.names = T)
oldmethoddata <- list.files("testing_oldmethod/",pattern=".nc",full.names = T)

temp <- rast(terradata[1])
County_Tigerlines <- vect(paste0("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw_data_rewrite/","County_Tigerlines/tl_",2019,"_us_county.shp"))
County_Tigerlines <- mask(County_Tigerlines,mask=as.polygons(temp))

divergent <- colorRampPalette(c("red","white","blue"))

default_scipen <- options("scipen")
options("scipen"=-10)

dir.create("test_figures",showWarnings = F)
for(A in 1:length(testdata)){
  real <- rast(terradata[A])
  part_raster <- rast(testdata[A])
  all_raster <- rast(oldmethoddata[A])
  
  name <-   gsub(".nc","",basename(terradata[A]))
  
  delta_old <- (part_raster - all_raster)
  peak <- max(abs(minmax(delta_old)))
  if(peak!=0){
    multiplier_A <- substr(peak,regexpr(as.character(peak),pattern="e")[1],nchar(peak))
    multiplier_A <- as.numeric(paste0("1",multiplier_A))
    delta_old <- delta_old/multiplier_A
  }else{multiplier_A=1}
  png(paste0("test_figures/",name,"_oldmethod.png"))
  plot(delta_old,col=divergent(101),main = "exact_raster - old_method",
       range=max(abs(terra::minmax(delta_old)))*c(-1,1),
       plg=list(title=multiplier_A))
  lines(shift(County_Tigerlines,dy=-0.05),col=rgb(0,0,0,0.075))
  dev.off()
  
  
  # delta <- (real - part_raster)
  # peak <- max(abs(minmax(delta)))
  # if(peak!=0){
  #   multiplier <- substr(peak,regexpr(as.character(peak),pattern="e")[1],nchar(peak))
  #   multiplier <- as.numeric(paste0("1",multiplier))
  #   delta <- delta/multiplier
  # }else{multiplier=1}
  
  # png(paste0("test_figures/",name,".png"))
  # plot(delta,col=divergent(101),main = "terra - exact_raster",
  #      range=max(abs(terra::minmax(delta)))*c(-1,1),
  #      plg=list(title=multiplier))
  # lines(shift(County_Tigerlines,dy=-0.05),col=rgb(0,0,0,0.075))
  # dev.off()
  
  peak <- unlist(global(real,max))
  multiplier_B <- substr(peak,regexpr(as.character(peak),pattern="e")[1],nchar(peak))
  multiplier_B <- as.numeric(paste0("1",multiplier_B))
  real <- real/multiplier_B

  # png(paste0("test_figures/",name,"_terra.png"))
  # plot(real,main = "terra",plg=list(title=multiplier))
  # lines(County_Tigerlines,col=rgb(0,0,0,0.075))
  # dev.off()
  # cat("Differences for  ",name,"  range  ",minmax(delta)*multiplier,
  #     "  compared to  ",unlist(global(real,mean))*multiplier_B,"\n")
  cat("Differences for  ",name,"  range  ",minmax(delta_old)*multiplier_A,
      "  compared to  ",unlist(global(real,mean))*multiplier_B,"\n")
}

options("scipen"=default_scipen)

