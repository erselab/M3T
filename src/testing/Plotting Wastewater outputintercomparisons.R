
setwd("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_rewrite")
newdata <- list.files(pattern=glob2rx("Waste*.nc"),"G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_rewrite",full.names = T)
olddata <- list.files(pattern=glob2rx("Waste*.nc"),"~/../../Kristian/Desktop/",full.names = T)
# olddata <- list.files(pattern=glob2rx("Waste*.nc"),"G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed",full.names = T)


divergent <- colorRampPalette(c("red","white","blue"))

default_scipen <- options("scipen")
options("scipen"=-10)

dir.create("WW_test_figures",showWarnings = F)
for(A in 1:length(newdata)){
  new <- rast(newdata[A])
  old <- rast(olddata[A])
  
  name <-   gsub(".nc","",basename(newdata[A]))
  
  delta <- (new - old)
  peak <- max(abs(minmax(delta)))
  if(peak!=0){
    multiplier_A <- substr(peak,regexpr(as.character(peak),pattern="e")[1],nchar(peak))
    multiplier_A <- as.numeric(paste0("1",multiplier_A))
    delta <- delta/multiplier_A
  }else{multiplier_A=1}
  png(paste0("WW_test_figures/",name,".png"))
  plot(delta,col=divergent(101),main = "new - old",
       range=max(abs(terra::minmax(delta)))*c(-1,1),
       plg=list(title=multiplier_A))
  dev.off()
  
  
  peak <- unlist(global(new,max))
  multiplier_B <- substr(peak,regexpr(as.character(peak),pattern="e")[1],nchar(peak))
  multiplier_B <- as.numeric(paste0("1",multiplier_B))
  new <- new/multiplier_B
  
  png(paste0("WW_test_figures/",name,"_new.png"))
  plot(new,main = "new",plg=list(title=multiplier_B))
  dev.off()
  
  
  cat("Differences for  ",name,"  range  ",minmax(delta)*multiplier_A,
      "  compared to  ",mean(new[new>1])*multiplier_B,"\n")
}

options("scipen"=default_scipen)

# newdata <- list.files(pattern=glob2rx("*NLCD*regridded.nc"),"G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_rewrite",full.names = T)
# olddata <- list.files(pattern=glob2rx("*NLCD*regridded.nc"),"G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed",full.names = T)
newdata <- list.files(pattern=glob2rx("test*NLCD*regridded.nc"),"G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_rewrite",full.names = T)
olddata <- list.files(pattern=glob2rx("raster*NLCD*regridded.nc"),"G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_rewrite",full.names = T)

options("scipen"=-10)

for(A in 1:length(newdata)){
  new <- rast(newdata[A])
  old <- rast(olddata[A])
  
  name <-   gsub(".nc","",basename(newdata[A]))
  
  delta <- (new - old)
  peak <- max(abs(minmax(delta)))
  if(peak!=0){
    multiplier_A <- substr(peak,regexpr(as.character(peak),pattern="e")[1],nchar(peak))
    multiplier_A <- as.numeric(paste0("1",multiplier_A))
    delta <- delta/multiplier_A
  }else{multiplier_A=1}
  png(paste0("WW_test_figures/",name,".png"))
  plot(delta,col=divergent(101),main = "new - old",
       range=max(abs(terra::minmax(delta)))*c(-1,1),
       plg=list(title=multiplier_A))
  dev.off()
  
  
  peak <- unlist(global(new,max,na.rm=T))
  multiplier_B <- substr(peak,regexpr(as.character(peak),pattern="e")[1],nchar(peak))
  multiplier_B <- as.numeric(paste0("1",multiplier_B))
  new <- new/multiplier_B
  
  png(paste0("WW_test_figures/",name,"_new.png"))
  plot(new,main = "new",plg=list(title=multiplier_B))
  dev.off()
  
  
  cat("Differences for  ",name,"  range  ",minmax(delta)*multiplier_A,
      "  compared to  ",mean(new[new>1])*multiplier_B,"\n")
}

options("scipen"=default_scipen)

