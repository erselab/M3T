# Load in Vulcan commercial and filter out large point sources, bearing in mind
# the range of values in Vulcan residential
################################################################################
#User input
Vulcan_inventory_folder <- "G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Inventories/Vulcan_v3.0/Sectoral/"
#load/save data here

state_outline_file <- "G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Shapefiles/tl_2022_us_state/tl_2022_us_state.shp"
#census outlines for states

focus_states <- c("DE", "MD", "NJ", "NY", "PA")
#which states to process

Output_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed/"
################################################################################
#load packages
i <- 1
packagecheck <- c("raster","ncdf4","sf")

while(i<=length(packagecheck)){
  if(length(find.package(packagecheck[i],quiet = TRUE))<1){
    install.packages(packagecheck[i],repos="https://repo.miserver.it.umich.edu/cran/")
  }
  i <- i+1
}

invisible(lapply(packagecheck, require, character.only=TRUE))
rm(packagecheck,i)

#raster = raster filetype functionalities
#ncdf4 = .nc filetype functionalities
#sf = simple features for spatial object functionalities
################################################################################
# Load inventory, remove NAs, and subset to the states of interest
Vulcan_com <- raster(file.path(Vulcan_inventory_folder,'Vulcan_v3_US_annual_1km_commercial_mn.nc4'), varname='carbon_emissions', band=6)
Vulcan_com[is.na(Vulcan_com)] <- 0
Vulcan_com_mol_per_s <- Vulcan_com*1e6/(12*365*24*60*60) #MT C/y to mol/s
remove(Vulcan_com)

Vulcan_res <- raster(file.path(Vulcan_inventory_folder,'Vulcan_v3_US_annual_1km_residential_mn.nc4'), varname='carbon_emissions', band=6)
Vulcan_res[is.na(Vulcan_res)] <- 0
Vulcan_res_mol_per_s <- Vulcan_res*1e6/(12*365*24*60*60) #MT C/y to mol/s
remove(Vulcan_res)

states <- st_read(state_outline_file)
states <- as(states,"Spatial")
states_lcc <- spTransform(states, crs(Vulcan_com_mol_per_s))

for(A in 1:length(focus_states)){
  #subset to the exact state outline
  State_subset <- subset(states_lcc, STUSPS==focus_states[A])
  State_subset_com_extract <- extract(Vulcan_com_mol_per_s, State_subset, weights=TRUE, normalizeWeights=FALSE, df=TRUE)
  State_subset_res_extract <- extract(Vulcan_res_mol_per_s, State_subset, weights=TRUE, normalizeWeights=FALSE, df=TRUE)
  
  #combine the subsets into 1 for the entire domain
  if(A>1){
    Vulcan_combined_states_com <- rbind(Vulcan_combined_states_com,State_subset_com_extract)
    Vulcan_combined_states_res <- rbind(Vulcan_combined_states_res,State_subset_res_extract)
  }else{
    Vulcan_combined_states_com <- State_subset_com_extract
    Vulcan_combined_states_res <- State_subset_res_extract
  }
  cat("\rFinished subsetting",A,"of",length(focus_states),"states           \n")
}
rm(State_subset,State_subset_com_extract,State_subset_res_extract,states,
   states_lcc,A,focus_states,state_outline_file)
################################################################################
#Set the maximum commercial to the maximum residential and save

#convert commercial and residential emissions to a vector
Vulcan_combined_states_com_emiss <- Vulcan_combined_states_com[,2]
Vulcan_combined_states_res_emiss <- Vulcan_combined_states_res[,2]

#ID the max res and the N of commercial pixels greater than that max
max_res <- max(Vulcan_combined_states_res_emiss)
com_abv_max_res <- length(which(Vulcan_combined_states_com_emiss>max_res))

#some visual checks of things.  May need to be edited for the region of interest.
hist(Vulcan_combined_states_com_emiss, breaks=seq(0,7000,100), ylim=c(0,10))
hist(Vulcan_combined_states_com_emiss, xlim=c(0,600), breaks=seq(0,7000,50), ylim=c(0,50))
hist(Vulcan_combined_states_com_emiss, xlim=c(0,10), breaks=seq(0,7000,1),ylim=c(0,100))

hist(Vulcan_combined_states_res_emiss, breaks=seq(0,7000,100), ylim=c(0,10))
hist(Vulcan_combined_states_res_emiss, xlim=c(0,600), breaks=seq(0,7000,50), ylim=c(0,50))
hist(Vulcan_combined_states_res_emiss, xlim=c(0,10), breaks=seq(0,7000,1), ylim=c(0,100))

# Saturate the commercial emissions at the max residential emission from the states
Vulcan_com_mol_per_s[Vulcan_com_mol_per_s>max_res] <- max_res
Vulcan_combined_states_com_emiss[Vulcan_combined_states_com_emiss>max_res] <- max_res
Vulcan_combined_states_com[[2]] <- Vulcan_combined_states_com_emiss
cat("\rOverwriting",com_abv_max_res,
    "values in the commercial sector that are above the maximum residential emissions of",
    max_res,"mol/s     \n")


# Write raster
writeRaster(Vulcan_com_mol_per_s,file.path(Output_directory,'Vulcan_v3_US_annual_1km_commercial_filt.grd'),overwrite=T)


