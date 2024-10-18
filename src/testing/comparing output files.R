#quickly compare output files to check if they differ/how much they differ.
#Only useful for testing after comparing to old raster based code as the raster
#functions result in some significant differences (effectively noise added each
#time an area, projection, etc. is applied).
#
#Comments are based on comparison with output as of Sept 10, 2024

library(terra)

#load in shapefiles to help with context when investigating if needed
Census_filenames <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw_data_rewrite/State_Tigerlines/tl_2019_us_state.shp"
State_Tigerlines <- vect(Census_filenames[1])


test <- function(oldfiles,newfiles){
  old_data <- rast(oldfiles)
  new_data <- rast(newfiles)
  
  # #testing resolution
  # # old_data <- aggregate(old_data,fact=10) - noticeably more inconsistent than the below version
  # old_data <- aggregate(old_data*cellSize(old_data,unit="m"),fact=10,sum)
  # old_data <- old_data/cellSize(old_data,unit="m")
  
  #testing projection
  old_data <- project(old_data,new_data,"near")
  
  delta <- new_data - old_data
  
  if(any(range(na.omit(values(range(delta))))!=0)){
    old_data[old_data==0] <- NA
    new_data[new_data==0] <- NA

    assign("old_data",old_data,envir = parent.env(environment()))
    assign("new_data",new_data,envir = parent.env(environment()))
    assign("delta",delta,envir = parent.env(environment()))
    plot(range(delta,na.rm=T),colNA="black")
    cat(basename(old[1]),"sector doesn't match old and new - see old_data, new_data, and delta")
    
    comparisons <- cbind(global(delta,sum),global(delta,range))
    colnames(comparisons) <- c("sum","min","max")
    rownames(comparisons) <- basename(new)
    View(comparisons)
  }else{
    cat(basename(old[1]),"sector PERFECTLY matches old and new")
  }
}

# old <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_rewrite/",pattern="MSW.*\\.nc",full.names = T)
# new <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_test",pattern="MSW.*\\.nc",full.names = T)
# old <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_rewrite/",pattern="MSW.*\\.nc",full.names = T)
# new <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_test2",pattern="MSW.*\\.nc",full.names = T)
old <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_test2/",pattern="MSW.*\\.nc",full.names = T)
new <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_test",pattern="MSW.*\\.nc",full.names = T)

test(old,new)
#MSW doesn't match, but that's expected.  When building CHECK files, modified to
#correct an error where facilities that stopped reporting without a valid reason
#were double counted (in both LMOP and GHGRP).  There is 1 pixel that's way too
#high in LMOP (missing in new).

State_Tigerlines <- project(State_Tigerlines,old_data)


# old <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_rewrite/",pattern="NG_dist.*\\.nc",full.names = T)
# new <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_test",pattern="NG_dist.*\\.nc",full.names = T)
# old <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_rewrite/",pattern="NG_dist.*\\.nc",full.names = T)
# new <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_test2",pattern="NG_dist.*\\.nc",full.names = T)
old <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_test2/",pattern="NG_dist.*\\.nc",full.names = T)
new <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_test",pattern="NG_dist.*\\.nc",full.names = T)

test(old,new)

# old <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_rewrite/",pattern="NG_trans.*\\.nc",full.names = T)
# new <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_test",pattern="NG_trans.*\\.nc",full.names = T)
# old <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_rewrite/",pattern="NG_trans.*\\.nc",full.names = T)
# new <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_test2",pattern="NG_trans.*\\.nc",full.names = T)
old <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_test2/",pattern="NG_trans.*\\.nc",full.names = T)
new <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_test",pattern="NG_trans.*\\.nc",full.names = T)

test(old,new)




# old <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_rewrite/",pattern="stat_comb.*\\.nc",full.names = T)
# new <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_test",pattern="stat_comb.*\\.nc",full.names = T)
# old <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_rewrite/",pattern="stat_comb.*\\.nc",full.names = T)
# new <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_test2",pattern="stat_comb.*\\.nc",full.names = T)
old <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_test2/",pattern="stat_comb.*\\.nc",full.names = T)
new <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_test",pattern="stat_comb.*\\.nc",full.names = T)

test(old,new)




# old <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_rewrite/",pattern="Wastewater.*\\.nc",full.names = T)
# new <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_test",pattern="Wastewater.*\\.nc",full.names = T)
# old <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_rewrite/",pattern="Wastewater.*\\.nc",full.names = T)
# new <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_test2",pattern="Wastewater.*\\.nc",full.names = T)
old <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_test2/",pattern="Wastewater.*\\.nc",full.names = T)
new <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_test",pattern="Wastewater.*\\.nc",full.names = T)

test(old,new)
#new septic by state is <6E-8 larger emissions for a few pixels (only for PA).
#I'm not sure why it's only PA, but this must be caused by saving and then
#reloading the NLCD by state data.  This is necessary given errors I was getting
#otherwise some runs.




# old <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_rewrite/",pattern="SOCCR.*\\.nc",full.names = T)
# new <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_test",pattern="SOCCR.*\\.nc",full.names = T)
# Freshwater_data <- rast(list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_test",pattern="Freshwater.*\\.nc",full.names = T))
# old <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_rewrite/",pattern="SOCCR.*\\.nc",full.names = T)
# new <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_test2",pattern="SOCCR.*\\.nc",full.names = T)
# Freshwater_data <- rast(list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_test2",pattern="Freshwater.*\\.nc",full.names = T))
# old_data <- rast(old)
# new_data <- rast(new)
# old_data <- c(sum(old_data[[1:10]]),sum(old_data[[11:20]]))
# new_data <- c(new_data[[1]]+Freshwater_data,new_data[[2]]+Freshwater_data)
# delta <- new_data - old_data
# plot(range(delta))
# global(delta,sum)
# #this one has to be more manual as the old approach treated freshwater as part
# #of SOCCR1 and 2 and the new approach saves only the sum across wetland types
# #for SOCCR1, SOCCR2, and Freshwater.  Summing across these before saving likely
# #avoids some rounding error when saving; that would be what's seen here perhaps?
# #Delta of <8E-6 per pixel (+ or -), summed across the domain it adds up to <8E-5
# #with new being higher. It's little in PA or NY, mostly DE and NJ - but that's
# #just where most emissions are.


old <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_test2/",pattern="SOCCR.*\\.nc",full.names = T)
new <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_test",pattern="SOCCR.*\\.nc",full.names = T)

test(old,new)

old <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_test2/",pattern="Freshwater.*\\.nc",full.names = T)
new <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_test",pattern="Freshwater.*\\.nc",full.names = T)

test(old,new)



# old <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_rewrite/",pattern="Wetcharts.*\\.nc",full.names = T)
# new <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_test",pattern="Wetcharts.*\\.nc",full.names = T)
# old <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_rewrite/",pattern="Wetcharts.*\\.nc",full.names = T)
# new <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_test2",pattern="Wetcharts.*\\.nc",full.names = T)
# old <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_test2/",pattern="Wetcharts.*\\.nc",full.names = T)
# new <- list.files("G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_test",pattern="Wetcharts.*\\.nc",full.names = T)
# 
# test(old,new)




