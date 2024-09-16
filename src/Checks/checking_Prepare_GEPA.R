## produce_combined_inventory_nc_files_r1.R
## In use 2022-04-21 09:00
#
# Original: Produce combined-sector level nc files for A1_WC_SN and V5_S1_SS
#
# r1: Updated for AL_AS1_WC_SS, VL_VS1_S1_SS, A1_VS1_WC_SN and V5_VS1_S1_SS

################################################################################
#User input
GEPA_file <- "G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Inventories/Gridded_EPA_2012/GEPA_Annual.nc"

domain=as.data.frame(cbind(c(-76.65,-73.65),
                           c(38.97,40.97)))
domain_res=0.01
domain_crs="epsg:4326" #lat/long

new_output_dir <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_rewrite"

################################################################################
#load packages
i <- 1
packagecheck <- c("raster","ncdf4","tools","terra")

while(i<=length(packagecheck)){
  if(length(find.package(packagecheck[i],quiet = TRUE))<1){
    install.packages(packagecheck[i],repos="https://repo.miserver.it.umich.edu/cran/")
  }
  i <- i+1
}

invisible(lapply(packagecheck, require, character.only=TRUE))
rm(packagecheck,i)

#raster + rgdal + sp + ncdf4 = raster and .nc filetype functionalities

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
#combine all the GEPA sectors into 1 large raster, cropped to d03 and res set to
#match

GEPA_sectors <- c("emissions_1A_Combustion_Mobile",
                  "emissions_1A_Combustion_Stationary",
                  "emissions_1B1a_Coal_Mining_Underground",
                  "emissions_1B1a_Coal_Mining_Surface",
                  "emissions_1B1a_Abandoned_Coal",
                  "emissions_1B2a_Petroleum",
                  "emissions_1B2b_Natural_Gas_Production",
                  "emissions_1B2b_Natural_Gas_Processing",
                  "emissions_1B2b_Natural_Gas_Transmission",
                  "emissions_1B2b_Natural_Gas_Distribution",
                  "emissions_2B5_Petrochemical_Production",
                  "emissions_2C2_Ferroalloy_Production",
                  "emissions_4A_Enteric_Fermentation",
                  "emissions_4B_Manure_Management",
                  "emissions_4C_Rice_Cultivation",
                  "emissions_4F_Field_Burning",
                  "emissions_5_Forest_Fires",
                  "emissions_6A_Landfills_Municipal",
                  "emissions_6A_Landfills_Industrial",
                  "emissions_6B_Wastewater_Treatment_Domestic",
                  "emissions_6B_Wastewater_Treatment_Industrial",
                  "emissions_6D_Composting")

# Load in the GEPA sectors
gepa <- brick(GEPA_file,varname=GEPA_sectors[1])
for(A in 2:length(GEPA_sectors)){
  gepa <- addLayer(gepa,brick(GEPA_file,varname=GEPA_sectors[A]))
}
gepa <- brick(gepa)
names(gepa) <- GEPA_sectors

#disaggregate to the proper res and crop to match hr_d03
gepa <- disaggregate(gepa,fact=5)
gepa <- crop(gepa,raster(domain))

#convert units
gepa <- gepa*(1e9*10^2^2)/(6.022141e+23)
#molec/cm2/s to nmol/m2/s

# rm(GEPA_sectors,A,GEPA_file)
################################################################################
# Collect the maps for the different combinations together

landfill <- (gepa$emissions_6A_Landfills_Industrial)

other_non_FF <- (gepa$emissions_6D_Composting +
                   gepa$emissions_4A_Enteric_Fermentation +
                   gepa$emissions_4B_Manure_Management +
                   gepa$emissions_4C_Rice_Cultivation +
                   gepa$emissions_4F_Field_Burning +
                   gepa$emissions_5_Forest_Fires)

other_FF <- (gepa$emissions_1A_Combustion_Mobile +
               gepa$emissions_1B1a_Abandoned_Coal +
               gepa$emissions_1B1a_Coal_Mining_Surface +
               gepa$emissions_1B1a_Coal_Mining_Underground +
               gepa$emissions_1B2a_Petroleum +
               gepa$emissions_1B2b_Natural_Gas_Processing +
               gepa$emissions_1B2b_Natural_Gas_Production +
               gepa$emissions_2B5_Petrochemical_Production +
               gepa$emissions_2C2_Ferroalloy_Production)

#convert to terra
other_FF <- rast(other_FF)
other_non_FF <- rast(other_non_FF)
landfill <- rast(landfill)

################################################################################
#Now compare to the newer GEPA.  Since it's for a totally different year, using
#updated methodologies, we don't expect perfect agreeement.  Just a reasonably
#similar value/distribution.

updated_FF <- rast(file.path(new_output_dir,"GEPA_thermo.nc"))
updated_non_FF <- rast(file.path(new_output_dir,"GEPA_non_thermo.nc"))
updated_landfill <- rast(file.path(new_output_dir,"GEPA_ind_landfill.nc"))

#regrid to match projection/res
updated_FF <- project(updated_FF,other_FF,method="average")
updated_non_FF <- project(updated_non_FF,other_FF,method="average")
updated_landfill <- project(updated_landfill,other_FF,method="average")

divergent <- colorRampPalette(c("red","white","blue"))

data_range <- range(c(global(other_FF,range),global(updated_FF,range)))
plot(other_FF,range=data_range)
plot(updated_FF,range=data_range)
delta <- other_FF - updated_FF
plot(delta,main="old - new",range=unlist(global(abs(delta),max))*c(-1,1),
     col=divergent(64),mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
global(other_FF,sum);global(updated_FF,sum)
global(updated_FF,sum)/global(other_FF,sum)

data_range <- range(c(global(other_non_FF,range),global(updated_non_FF,range)))
plot(other_non_FF,range=data_range)
plot(updated_non_FF,range=data_range)
delta <- other_non_FF - updated_non_FF
plot(delta,main="old - new",range=unlist(global(abs(delta),max))*c(-1,1),
     col=divergent(64),mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
global(other_non_FF,sum);global(updated_non_FF,sum)
global(updated_non_FF,sum)/global(other_non_FF,sum)

data_range <- range(c(global(landfill,range),global(updated_landfill,range)))
plot(landfill,range=data_range)
plot(updated_landfill,range=data_range)
delta <- landfill - updated_landfill
plot(delta,main="old - new",range=unlist(global(abs(delta),max))*c(-1,1),
     col=divergent(64),mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
global(landfill,sum);global(updated_landfill,sum)
global(updated_landfill,sum)/global(landfill,sum)


################################################################################
#Now instead do the same, but for the newer GEPA.  I.e., equivalent input data,
#but older approach to processing (which is little more than loading in and
#disagg)


GEPA_file <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw_data_rewrite/Express_Extension_Gridded_GHGI_Methane_v2_2019.nc"

GEPA_sectors <- c("emi_ch4_1A_Combustion_Mobile",
                  "emi_ch4_1A_Combustion_Stationary",
                  "emi_ch4_1B1a_Abandoned_Coal",
                  "emi_ch4_1B1a_Surface_Coal",
                  "emi_ch4_1B1a_Underground_Coal",
                  "emi_ch4_1B2a_Petroleum_Systems_Exploration",
                  "emi_ch4_1B2a_Petroleum_Systems_Production",
                  "emi_ch4_1B2a_Petroleum_Systems_Refining",
                  "emi_ch4_1B2a_Petroleum_Systems_Transport",
                  "emi_ch4_1B2ab_Abandoned_Oil_Gas",
                  "emi_ch4_1B2b_Natural_Gas_Distribution",
                  "emi_ch4_1B2b_Natural_Gas_Exploration",
                  "emi_ch4_1B2b_Natural_Gas_Processing",
                  "emi_ch4_1B2b_Natural_Gas_Production",
                  "emi_ch4_1B2b_Natural_Gas_TransmissionStorage",
                  "emi_ch4_2B8_Industry_Petrochemical",
                  "emi_ch4_2C2_Industry_Ferroalloy",
                  "emi_ch4_3A_Enteric_Fermentation",
                  "emi_ch4_3B_Manure_Management",
                  "emi_ch4_3C_Rice_Cultivation",
                  "emi_ch4_3F_Field_Burning",
                  "emi_ch4_5A1_Landfills_Industrial",
                  "emi_ch4_5A1_Landfills_MSW",
                  "emi_ch4_5B1_Composting",
                  "emi_ch4_5D_Wastewater_Treatment_Domestic",
                  "emi_ch4_5D_Wastewater_Treatment_Industrial",
                  "emi_ch4_Supp_1B2b_PostMeter")

# Load in the GEPA sectors
gepa <- brick(GEPA_file,varname=GEPA_sectors[1])
for(A in 2:length(GEPA_sectors)){
  gepa <- addLayer(gepa,brick(GEPA_file,varname=GEPA_sectors[A]))
}
gepa <- brick(gepa)
names(gepa) <- GEPA_sectors

#disaggregate to the proper res and crop to match hr_d03
gepa <- crop(gepa,extent(raster(domain))*1.2)
gepa <- disaggregate(gepa,fact=10)
gepa <- crop(gepa,raster(domain))

#convert units
gepa <- gepa*(1e9*10^2^2)/(6.022141e+23)
#molec/cm2/s to nmol/m2/s

# rm(GEPA_sectors,A,GEPA_file)
################################################################################
# Collect the maps for the different combinations together

landfill <- (gepa$emi_ch4_5A1_Landfills_Industrial)

other_non_FF <- (gepa$emi_ch4_5B1_Composting +
                   gepa$emi_ch4_3A_Enteric_Fermentation +
                   gepa$emi_ch4_3B_Manure_Management +
                   gepa$emi_ch4_3C_Rice_Cultivation +
                   gepa$emi_ch4_3F_Field_Burning)

other_FF <- (gepa$emi_ch4_1A_Combustion_Mobile +
             gepa$emi_ch4_1B1a_Abandoned_Coal +
             gepa$emi_ch4_1B1a_Surface_Coal +
             gepa$emi_ch4_1B1a_Underground_Coal +
             gepa$emi_ch4_1B2a_Petroleum_Systems_Exploration +
             gepa$emi_ch4_1B2a_Petroleum_Systems_Production +
             gepa$emi_ch4_1B2a_Petroleum_Systems_Refining +
             gepa$emi_ch4_1B2a_Petroleum_Systems_Transport +
             gepa$emi_ch4_1B2ab_Abandoned_Oil_Gas +
             gepa$emi_ch4_1B2b_Natural_Gas_Exploration +
             gepa$emi_ch4_1B2b_Natural_Gas_Processing +
             gepa$emi_ch4_1B2b_Natural_Gas_Production +
             gepa$emi_ch4_2B8_Industry_Petrochemical +
             gepa$emi_ch4_2C2_Industry_Ferroalloy)

#convert to terra
other_FF <- rast(other_FF)
other_non_FF <- rast(other_non_FF)
landfill <- rast(landfill)

################################################################################
#Now compare to the output via newer code.  Since the input and processing
#should match, the output should too.

updated_FF <- rast(file.path(new_output_dir,"GEPA_thermo.nc"))
updated_non_FF <- rast(file.path(new_output_dir,"GEPA_non_thermo.nc"))
updated_landfill <- rast(file.path(new_output_dir,"GEPA_ind_landfill.nc"))

divergent <- colorRampPalette(c("red","white","blue"))

plot(other_FF)
plot(updated_FF)
delta <- other_FF - updated_FF
plot(delta,main="old - new",range=unlist(global(abs(delta),max))*c(-1,1),
     col=divergent(64),mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
global(other_FF,sum);global(updated_FF,sum)

plot(other_non_FF)
plot(updated_non_FF)
delta <- other_non_FF - updated_non_FF
plot(delta,main="old - new",range=unlist(global(abs(delta),max))*c(-1,1),
     col=divergent(64),mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
global(other_non_FF,sum);global(updated_non_FF,sum)

plot(landfill)
plot(updated_landfill)
delta <- landfill - updated_landfill
plot(delta,main="old - new",range=unlist(global(abs(delta),max))*c(-1,1),
     col=divergent(64),mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,2))
global(landfill,sum);global(updated_landfill,sum)


