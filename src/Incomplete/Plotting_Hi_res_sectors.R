#plot up some of the sectors of the hi-resolution inventory


#set to just source the other piece

################################################################################
#Manually defined variables

Input_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed/"
Output_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Figures/"

prepared_Vulcan <- "G:/My Drive/Shepson Group Drive/Kris/Indy Inversion/Inventories/Prepared/2022-08-25_Vulcan_2015.grd"
prepared_GEPA <- "G:/My Drive/Shepson Group Drive/Kris/Indy Inversion/Inventories/Prepared/2022-08-25_GEPA_s_2012.grd"
prepared_composite <- "G:/My Drive/Shepson Group Drive/Kris/Indy Inversion/Inventories/Prepared/2022-08-25_composite.grd"
#all the other inventories as they're intended to be used, for d03

GEPA_file <- "G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Inventories/Gridded_EPA_2012/GEPA_Annual.nc"

d03_bounding_box <- cbind(c(-76.65,-73.65),c(38.97,40.97))
resolution <- 0.01

plotting_function <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Code and method/Scripts/Plotting_individual_sectors.R"
#the location of another script that just creates some functions for consistent,
#quick plotting

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
#load various sectors
MSW_GHGRP <- brick(file.path(Input_directory,"MSW_GHGRP.nc"))
MSW_LMOP <- brick(file.path(Input_directory,"MSW_LMOP.nc"))
NG_trans_compressors <- brick(file.path(Input_directory,"NG_trans_compressors.nc"))
NG_trans_pipes <- brick(file.path(Input_directory,"NG_trans_pipes.nc"))
Wastewater_dom_central <- brick(file.path(Input_directory,"Wastewater_dom_central.nc"))
Wastewater_ind <- brick(file.path(Input_directory,"Wastewater_ind.nc"))
Wastewater_dom_septic_bystate <- brick(file.path(Input_directory,"Wastewater_dom_septic_bystate.nc"))
Wastewater_dom_septic_national <- brick(file.path(Input_directory,"Wastewater_dom_septic_national.nc"))
NG_post_meter_res <- brick(file.path(Input_directory,"NG_post_meter_res_byLDC_vulcan.nc"))
NG_upsets_res <- brick(file.path(Input_directory,"NG_dist_upset_res_byLDC_vulcan.nc"))
NG_upsets_com <- brick(file.path(Input_directory,"NG_dist_upset_com_byLDC_vulcan.nc"))
NG_services_res <- brick(file.path(Input_directory,"NG_dist_serv_res_byLDC_vulcan.nc"))
NG_services_com <- brick(file.path(Input_directory,"NG_dist_serv_com_byLDC_vulcan.nc"))
NG_MnR_res <- brick(file.path(Input_directory,"NG_dist_MnR_res_byLDC_vulcan.nc"))
NG_MnR_com <- brick(file.path(Input_directory,"NG_dist_MnR_com_byLDC_vulcan.nc"))
NG_meter_res <- brick(file.path(Input_directory,"NG_dist_meter_res_byLDC_vulcan.nc"))
NG_meter_com <- brick(file.path(Input_directory,"NG_dist_meter_com_byLDC_vulcan.nc"))
NG_mains_res <- brick(file.path(Input_directory,"NG_dist_mains_res_byLDC_vulcan.nc"))
NG_mains_com <- brick(file.path(Input_directory,"NG_dist_mains_com_byLDC_vulcan.nc"))
stat_comb_res_petr <- brick(file.path(Input_directory,"stat_comb_res_petr_bystate_vulcan.nc"))
stat_comb_res_wood <- brick(file.path(Input_directory,"stat_comb_res_wood_bystate_vulcan.nc"))
stat_comb_ind_wood <- brick(file.path(Input_directory,"stat_comb_ind_wood_bystate_vulcan.nc"))
stat_comb_ind_petr <- brick(file.path(Input_directory,"stat_comb_ind_petr_bystate_vulcan.nc"))
stat_comb_ind_gas <- brick(file.path(Input_directory,"stat_comb_ind_gas_bystate_vulcan.nc"))
stat_comb_ind_coal <- brick(file.path(Input_directory,"stat_comb_ind_coal_bystate_vulcan.nc"))
stat_comb_elec_wood <- brick(file.path(Input_directory,"stat_comb_elec_wood_bystate_vulcan.nc"))
stat_comb_elec_petr <- brick(file.path(Input_directory,"stat_comb_elec_petr_bystate_vulcan.nc"))
stat_comb_elec_gas <- brick(file.path(Input_directory,"stat_comb_elec_gas_bystate_vulcan.nc"))
stat_comb_elec_coal <- brick(file.path(Input_directory,"stat_comb_elec_coal_bystate_vulcan.nc"))
stat_comb_com_wood <- brick(file.path(Input_directory,"stat_comb_com_wood_bystate_vulcan.nc"))
stat_comb_com_petr <- brick(file.path(Input_directory,"stat_comb_com_petr_bystate_vulcan.nc"))
stat_comb_com_gas <- brick(file.path(Input_directory,"stat_comb_com_gas_bystate_vulcan.nc"))
stat_comb_com_coal <- brick(file.path(Input_directory,"stat_comb_com_coal_bystate_vulcan.nc"))

Summed_landfill <- brick(file.path(Input_directory,"Summed_Sectors","Landfill.nc"))
Summed_NG_dist <- brick(file.path(Input_directory,"Summed_Sectors","NG_distribution.nc"))
Summed_NG_residential_postmeter <- brick(file.path(Input_directory,"Summed_Sectors","NG_residential_post_meter.nc"))
Summed_NG_transmission <- brick(file.path(Input_directory,"Summed_Sectors","NG_transmission.nc"))
Summed_other_FF <- brick(file.path(Input_directory,"Summed_Sectors","Other_FF.nc"))
Summed_other_nonFF <- brick(file.path(Input_directory,"Summed_Sectors","Other_nonFF.nc"))
Summed_SOCCR1 <- brick(file.path(Input_directory,"Summed_Sectors","SOCCR1.nc"))
Summed_SOCCR2 <- brick(file.path(Input_directory,"Summed_Sectors","SOCCR2.nc"))
Summed_stationary_combustion_FF <- brick(file.path(Input_directory,"Summed_Sectors","Stationary_combustion_FF.nc"))
Summed_stationary_combustion_wood <- brick(file.path(Input_directory,"Summed_Sectors","Stationary_combustion_wood.nc"))
Summed_wastewater_treatment <- brick(file.path(Input_directory,"Summed_Sectors","Wastewater_treatment.nc"))
Summed_wetchart <- brick(file.path(Input_directory,"Summed_Sectors","Wetcharts.nc"))

Total_no_wetland <- brick(file.path(Input_directory,"Summed_Sectors","d03_Total.nc"))

d01_SOCCR1 <- brick(file.path(Input_directory,"Wetlands","S1_natural_tot.nc"))
d01_SOCCR2 <- brick(file.path(Input_directory,"Wetlands","S2_natural_tot.nc"))
d01_Wetcharts <- brick(file.path(Input_directory,"Wetlands","WC_natural_tot.nc"))

################################################################################
#write functions to plot each up in a useful way
dir.create(Output_directory,showWarnings = F)
setwd(Output_directory)

source(plotting_function)

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

Extracted_emissions <- function(input,bounds){
  emission_extract <- extract(input,bounds,weights=TRUE,
                              normalizeWeights=FALSE, df=TRUE)
  area_extract <- extract(area(input),bounds,weights=TRUE,
                          normalizeWeights=FALSE, df=TRUE)
  emissions <- sum((area_extract[,2]*(1000*1000))*
                     (emission_extract[,2]/1e9)*area_extract[,3])
  #km2 area to m2 with 1000*1000, nmol/s to mol/s with 1/1e9
  return(emissions)
}

################################################################################
# Now do the actual plotting

#most simpler sectors
log_plot(MSW_GHGRP,
         "Municipal Solid Waste -\n GHGRP reporters")

log_plot(MSW_LMOP,
         "Municipal Solid Waste -\n (GHGI - GHGRP) distributed using \nLandfill Methane Outreach Program")

log_plot(NG_trans_compressors,
         "NG transmission - compressors\n GHGRP reporters + (average GHGI emissions distributed using\n Homeland Infrastructure Foundation-Level Database)")

not_log_plot(NG_trans_pipes,
             "NG transmission - pipelines\n EIA pipeline data * EPA EF")

not_log_plot(Wastewater_dom_septic_bystate,
             "Domestic Wastewater - Septic v2\n estimated state septic distributed using \ndeveloped open space/low intensity land cover",
             min(minValue(Wastewater_dom_septic_bystate),minValue(Wastewater_dom_septic_national)),
             max(maxValue(Wastewater_dom_septic_bystate),maxValue(Wastewater_dom_septic_national)))

not_log_plot(Wastewater_dom_septic_national,
             "Domestic Wastewater - Septic\n national EPA septic distributed using \ndeveloped open space/low intensity land cover",
             min(minValue(Wastewater_dom_septic_bystate),minValue(Wastewater_dom_septic_national)),
             max(maxValue(Wastewater_dom_septic_bystate),maxValue(Wastewater_dom_septic_national)))

log_plot(Wastewater_dom_central,
         "Domestic Wastewater -\n EPA total distributed using \nClean Watersheds Needs Survey")

log_plot(Wastewater_ind,
         "Industrial Wastewater -\n GHGRP Reporters")


































#more detailed sectors (stationary combustion and distribution)
not_log_plot(NG_mains_com,
             "NG Distribution - Mains\n local distribution data distributed using\nVulcan commercial CO2")

not_log_plot(NG_mains_res,
             "NG Distribution - Mains\n local distribution data distributed using\nVulcan residential CO2")

not_log_plot(NG_meter_com,
             "NG Distribution - Meters\n local distribution data distributed using\nVulcan commercial CO2")

not_log_plot(NG_meter_res,
             "NG Distribution - Meters\n local distribution data distributed using\nVulcan residential CO2")

not_log_plot(NG_MnR_com,
             "NG Distribution - metering and regulating stations\n local distribution data distributed using\nVulcan commercial CO2")

not_log_plot(NG_MnR_res,
             "NG Distribution - metering and regulating stations\n local distribution data distributed using\nVulcan residential CO2")

not_log_plot(NG_post_meter_res,
             "NG Distribution - post meter\n local distribution data distributed using\nVulcan residential CO2")

not_log_plot(NG_services_com,
             "NG Distribution - service pipelines\n local distribution data distributed using\nVulcan commercial CO2")

not_log_plot(NG_services_res,
             "NG Distribution - service pipelines\n local distribution data distributed using\nVulcan residential CO2")

not_log_plot(NG_upsets_com,
             "NG Distribution - upsets\n local distribution data distributed using\nVulcan commercial CO2")

not_log_plot(NG_upsets_res,
             "NG Distribution - upsets\n local distribution data distributed using\nVulcan residential CO2")

not_log_plot(stat_comb_com_coal,
             "Stationary Combustion Commercial - Coal\n state totals distributed using NEI CO emissions\n and Vulcan commercial CO2 emissions")

not_log_plot(stat_comb_com_gas,
             "Stationary Combustion Commercial - Gas\n state totals distributed using NEI CO emissions\n and Vulcan commercial CO2 emissions")

not_log_plot(stat_comb_com_petr,
             "Stationary Combustion Commercial - Petroleum\n state totals distributed using NEI CO emissions\n and Vulcan commercial CO2 emissions")

not_log_plot(stat_comb_com_wood,
             "Stationary Combustion Commercial - Wood\n state totals distributed using NEI CO emissions\n and Vulcan commercial CO2 emissions")

log_plot(stat_comb_elec_coal,
         "Stationary Combustion Electricity - Coal\n state totals distributed using NEI CO emissions\n and Vulcan electricity CO2 emissions")

log_plot(stat_comb_elec_gas,
         "Stationary Combustion Electricity - Gas\n state totals distributed using NEI CO emissions\n and Vulcan electricity CO2 emissions")

log_plot(stat_comb_elec_petr,
         "Stationary Combustion Electricity - Petroleum\n state totals distributed using NEI CO emissions\n and Vulcan electricity CO2 emissions")

log_plot(stat_comb_elec_wood,
         "Stationary Combustion Electricity - Wood\n state totals distributed using NEI CO emissions\n and Vulcan electricity CO2 emissions")

not_log_plot(stat_comb_ind_coal,
         "Stationary Combustion Industrial - Coal\n state totals distributed using NEI CO emissions\n and Vulcan electricity CO2 emissions")

not_log_plot(stat_comb_ind_gas,
             "Stationary Combustion Industrial - Gas\n state totals distributed using NEI CO emissions\n and Vulcan electricity CO2 emissions")

not_log_plot(stat_comb_ind_petr,
             "Stationary Combustion Industrial - Petroleum\n state totals distributed using NEI CO emissions\n and Vulcan electricity CO2 emissions")

not_log_plot(stat_comb_ind_wood,
             "Stationary Combustion Industrial - Wood\n state totals distributed using NEI CO emissions\n and Vulcan electricity CO2 emissions")

not_log_plot(stat_comb_res_petr,
             "Stationary Combustion Residential - Petroleum\n state totals distributed using NEI CO emissions\n and Vulcan electricity CO2 emissions")

not_log_plot(stat_comb_res_wood,
             "Stationary Combustion Residential - Wood\n state totals distributed using NEI CO emissions\n and Vulcan electricity CO2 emissions")

################################################################################
# Now sector-summed ones

dir.create("Summed_Sectors",showWarnings = F)
setwd("Summed_Sectors")

Summed_landfill = MSW_GHGRP+MSW_LMOP+GEPA???
Summed_NG_transmission = NG_trans_compressors+NG_trans_pipes
Summed_wastewater_treatment = Wastewater_dom_central+Wastewater_dom_septic_national+Wastewater_ind

log_plot(Summed_landfill,
         "Landfill Sector\nGHGRP + LMOP for municipal, GEPA for industrial")

not_log_plot(Summed_NG_dist,
             "NG Distribution Sector\nEIA local distribution data distributed using Vulcan commercial/residential CO2\nMetering and Regulating stations + mains + meters + service pipelines + upsets")

not_log_plot(Summed_NG_residential_postmeter,
             "NG Post-Meter Sector\nEIA local distribution data distributed using\nVulcan residential CO2")

log_plot(Summed_NG_transmission,
         "NG Transmission Sector\nEIA for pipelines + HFILD/GHGRP for compressors")

log_plot(Summed_other_FF,
         "Other FF Sector\nGEPA data - Mobile + abandoned coal + coal mining + petroleum + NG\nprocessing and production + petrochemical production + ferroalloy production")

not_log_plot(Summed_other_nonFF,
             "Other non-FF Sector\nGEPA data - Composting + Enteric Fermentation + Manure + Rice + Fires")

log_plot(Summed_SOCCR1,
             "Wetland Sector\nSOCCR1 data + Rosentreter Lake and River emissions",
         -4.5,
         log10(max(maxValue(Summed_SOCCR1),maxValue(Summed_SOCCR2),maxValue(Summed_wetchart))))
log_plot(Summed_SOCCR2,
             "Wetland Sector\nSOCCR2 data + Rosentreter Lake and River emissions",
         -4.5,
         log10(max(maxValue(Summed_SOCCR1),maxValue(Summed_SOCCR2),maxValue(Summed_wetchart))))
log_plot(Summed_wetchart,
             "Wetland Sector\nWetcharts data + Rosentreter Lake and River emissions",
         -4.5,
         log10(max(maxValue(Summed_SOCCR1),maxValue(Summed_SOCCR2),maxValue(Summed_wetchart))))

log_plot(Summed_stationary_combustion_FF,
         "Stationary Combustion FF Sector\nSEDS state data scaled to match GHGI national data distributed to the county\nlevel via NEI, then distributed using Vulcan.  coal + gas + petroleum")

log_plot(Summed_stationary_combustion_wood,
             "Stationary Combustion Wood Sector\nSEDS state data scaled to match GHGI national data distributed to the county\nlevel via NEI, then distributed using Vulcan.  wood")

log_plot(Summed_wastewater_treatment,
         "Wastewater Treatment Sector\nGHGI total distributed with CWNS (Domestic facilities) and GHGRP (industrial) and\n developed open space/low intensity NLCD land cover (Septic)")

################################################################################
# Now the biological ones for d01
setwd("G:/My Drive/Shepson Group Drive/Kris/Indy Inversion/Inventories/Hi-res_CH4_inventory_development/Figures/Summed_Sectors")

d01_plot(d01_Wetcharts,
         "Wetland Sector\nWetcharts data + Rosentreter Lake and River emissions",
         -6.5,
         log10(max(maxValue(d01_SOCCR1),maxValue(d01_SOCCR2),maxValue(d01_Wetcharts))))
d01_plot(d01_SOCCR1,
         "Wetland Sector\nSOCCR1 data + Rosentreter Lake and River emissions",
         -6.5,
         log10(max(maxValue(d01_SOCCR1),maxValue(d01_SOCCR2),maxValue(d01_Wetcharts))))
d01_plot(d01_SOCCR2,
         "Wetland Sector\nSOCCR2 data + Rosentreter Lake and River emissions",
         -6.5,
         log10(max(maxValue(d01_SOCCR1),maxValue(d01_SOCCR2),maxValue(d01_Wetcharts))))

################################################################################
# And finally the sector-summed total prior
setwd("G:/My Drive/Shepson Group Drive/Kris/Indy Inversion/Inventories/Hi-res_CH4_inventory_development/Figures/Summed_Sectors")

log_plot(Total_no_wetland,
         "All sectors combined, no wetlands")
################################################################################
# load in the GEPA

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

#disaggregate to the proper res and crop to the domain
gepa <- disaggregate(gepa,fact=10)
gepa <- crop(gepa,MSW_GHGRP)

#convert units
gepa <- gepa*(1e9*10^2^2)/(6.022141e+23)
#molec/cm2/s to nmol/m2/s

rm(GEPA_sectors,A,GEPA_file)
################################################################################
# Now calculate a few ER's and plot up a NG-non-NG figure
setwd("G:/My Drive/Shepson Group Drive/Kris/Indy Inversion/Inventories/Hi-res_CH4_inventory_development/Figures/Summed_Sectors")

Thermo_total <- (Summed_NG_dist+
                   Summed_NG_residential_postmeter+
                   Summed_NG_transmission+
                   Summed_stationary_combustion_FF+
                   Summed_other_FF)
non_Thermo_total <- (Summed_landfill+
                       Summed_other_nonFF+
                       Summed_stationary_combustion_wood+
                       Summed_wastewater_treatment+
                       Summed_wetchart)

GEPA_Thermo_total <- (gepa$emissions_1A_Combustion_Mobile+
                        gepa$emissions_1A_Combustion_Stationary+
                        gepa$emissions_1B1a_Coal_Mining_Underground+
                        gepa$emissions_1B1a_Coal_Mining_Surface+
                        gepa$emissions_1B1a_Abandoned_Coal+
                        gepa$emissions_1B2a_Petroleum+
                        gepa$emissions_1B2b_Natural_Gas_Production+
                        gepa$emissions_1B2b_Natural_Gas_Processing+
                        gepa$emissions_1B2b_Natural_Gas_Transmission+
                        gepa$emissions_1B2b_Natural_Gas_Distribution+
                        gepa$emissions_2B5_Petrochemical_Production+
                        gepa$emissions_2C2_Ferroalloy_Production)
GEPA_non_Thermo_total <- (gepa$emissions_4A_Enteric_Fermentation+
                            gepa$emissions_4B_Manure_Management+
                            gepa$emissions_4C_Rice_Cultivation+
                            gepa$emissions_4F_Field_Burning+
                            gepa$emissions_5_Forest_Fires+
                            gepa$emissions_6A_Landfills_Municipal+
                            gepa$emissions_6D_Composting+
                            gepa$emissions_6A_Landfills_Industrial+
                            gepa$emissions_6B_Wastewater_Treatment_Domestic+
                            gepa$emissions_6B_Wastewater_Treatment_Industrial+
                            Summed_wetchart)

low_res_total <- aggregate(Total_no_wetland+Summed_wetchart,fact=10)


log_plot(Thermo_total,"Thermogenic - Distribution + postmeter + transmission +\n FF stationary combustion + GEPA other FF",
         log10(min(minValue(Thermo_total),minValue(non_Thermo_total))),
         log10(max(maxValue(Thermo_total),maxValue(non_Thermo_total))))
log_plot(non_Thermo_total,"Non-Thermogenic - Landfill + non FF stationary combustion +\n wastewater + GEPA other non FF + Wetcharts",
         log10(min(minValue(Thermo_total),minValue(non_Thermo_total))),
         log10(max(maxValue(Thermo_total),maxValue(non_Thermo_total))))
log_plot(Thermo_total-non_Thermo_total,"Thermogenic - non-thermogenic",
         filename = "Thermo_minus_non_thermo")
log_plot(GEPA_Thermo_total-GEPA_non_Thermo_total,"GEPA Thermogenic - non-thermogenic",
         filename = "GEPA_Thermo_minus_non_thermo")

thermo_fraction <- Thermo_total/(Thermo_total+non_Thermo_total)
png("Thermogenic Fraction.png",width = 480*2,height=480*2)
par(mar=c(5, 4, 4, 2) + 0.1 + c(0,1,2,1))
plot(thermo_fraction,col=timPalette(),colNA="black",
     main="Thermogenic Fraction",
     legend.args=list(text="fraction",side=2,line=0.5,cex=2),
     axis.args=list(cex.axis=2),legend.width=1.2*2,
     xlab="Longitude",ylab="Latitude",
     cex.main=2,cex.axis=2,cex.lab=2,
     smallplot= c(.9,.93,0.25,0.75),
     zlim=c(0,1))
lines(county_outline,col="dimgrey")
lines(city_bounds,col="darkgrey")
dev.off()

png("Thermogenic Fraction lo-res.png",width = 480*2,height=480*2)
par(mar=c(5, 4, 4, 2) + 0.1 + c(0,1,2,1))
plot(aggregate(thermo_fraction,10),col=timPalette(),colNA="black",
     main="Thermogenic Fraction",
     legend.args=list(text="fraction",side=2,line=0.5,cex=2),
     axis.args=list(cex.axis=2),legend.width=1.2*2,
     xlab="Longitude",ylab="Latitude",
     cex.main=2,cex.axis=2,cex.lab=2,
     smallplot= c(.9,.93,0.25,0.75),
     zlim=c(0,1))
lines(county_outline,col="dimgrey")
lines(city_bounds,col="darkgrey")
dev.off()

GEPA_thermo_fraction <- GEPA_Thermo_total/(GEPA_Thermo_total+GEPA_non_Thermo_total)
png("GEPA Thermogenic Fraction.png",width = 480*2,height=480*2)
par(mar=c(5, 4, 4, 2) + 0.1 + c(0,1,2,1))
plot(aggregate(GEPA_thermo_fraction,10),col=timPalette(),colNA="black",
     main="GEPA Thermogenic Fraction",
     legend.args=list(text="fraction",side=2,line=0.5,cex=2),
     axis.args=list(cex.axis=2),legend.width=1.2*2,
     xlab="Longitude",ylab="Latitude",
     cex.main=2,cex.axis=2,cex.lab=2,
     smallplot= c(.9,.93,0.25,0.75),
     zlim=c(0,1))
lines(county_outline,col="dimgrey")
lines(city_bounds,col="darkgrey")
dev.off()


prepared_composite <- crop(brick(prepared_composite),low_res_total)*1000
prepared_GEPA <- crop(brick(prepared_GEPA),low_res_total)*1000
prepared_Vulcan <- crop(brick(prepared_Vulcan),low_res_total)*1000
#converting from micromol/m2s to nmol/m2s

prepared_composite <- prepared_composite+aggregate(Summed_wetchart,10)
prepared_GEPA <- prepared_GEPA+aggregate(Summed_wetchart,10)
#add wetlands to both CH4 ones

log_plot(prepared_Vulcan,"Vulcan CO2")
log_plot(prepared_GEPA,"GEPA",
         log10(min(minValue(prepared_GEPA),minValue(prepared_composite),minValue(low_res_total))),
         log10(max(maxValue(prepared_GEPA),maxValue(prepared_composite),maxValue(low_res_total))))
log_plot(prepared_composite,"Composite",
         log10(min(minValue(prepared_GEPA),minValue(prepared_composite),minValue(low_res_total))),
         log10(max(maxValue(prepared_GEPA),maxValue(prepared_composite),maxValue(low_res_total))))
log_plot(low_res_total,
         "Hi-Res Total, including Wetcharts",
         log10(min(minValue(prepared_GEPA),minValue(prepared_composite),minValue(low_res_total))),
         log10(max(maxValue(prepared_GEPA),maxValue(prepared_composite),maxValue(low_res_total))))


low_res_total_Indy_ER <- Extracted_emissions(low_res_total,city_bounds)
Composite_Indy_ER <- Extracted_emissions(prepared_composite,city_bounds)
GEPA_Indy_ER <- Extracted_emissions(prepared_GEPA,city_bounds)
Vulcan_Indy_ER <- Extracted_emissions(prepared_Vulcan,city_bounds)

Thermo_Indy_ER <- Extracted_emissions(Thermo_total,city_bounds)
non_thermo_Indy_ER <- Extracted_emissions(non_Thermo_total,city_bounds)
Hi_res_Indy_ER <- Thermo_Indy_ER+non_thermo_Indy_ER
GEPA_Thermo_Indy_ER <- Extracted_emissions(GEPA_Thermo_total,city_bounds)

Indy_totals_DF <- data.frame("Composite"=Composite_Indy_ER,"GEPA"=GEPA_Indy_ER,
                             "Hi_res"=Hi_res_Indy_ER,"Vulcan"=Vulcan_Indy_ER,
                             "HiRes_Thermo_Fraction"=Thermo_Indy_ER/Hi_res_Indy_ER*100,
                             "GEPA_Thermo_Fraction"=GEPA_Thermo_Indy_ER/GEPA_Indy_ER*100)

totals_DF <- data.frame("Composite"=cellStats(prepared_composite*area(prepared_composite)*1000*1000/1e9,sum,na.rm=T),
                        "GEPA"=cellStats(prepared_GEPA*area(prepared_GEPA)*1000*1000/1e9,sum,na.rm=T),
                        "Hi_res"=cellStats((Thermo_total+non_Thermo_total)*area(Thermo_total)*1000*1000/1e9,sum,na.rm=T),
                        "Vulcan"=cellStats(prepared_Vulcan*area(prepared_Vulcan)*1000*1000/1e9,sum,na.rm=T))
totals_DF$HiRes_Thermo_Fraction <- cellStats(Thermo_total*area(Thermo_total)*1000*1000/1e9,sum,na.rm=T)/totals_DF$Hi_res*100
totals_DF$GEPA_Thermo_Fraction <- cellStats(GEPA_Thermo_total*area(GEPA_Thermo_total)*1000*1000/1e9,sum,na.rm=T)/totals_DF$GEPA*100

totals_DF <- rbind(totals_DF,Indy_totals_DF)
rownames(totals_DF) <- c("D03","Indy")

SSLF <- rbind(maxValue(mask(Summed_landfill*area(Summed_landfill)*1000*1000/1e9,city_bounds)),
              maxValue(mask(aggregate(gepa$emissions_6A_Landfills_Municipal,10)*
                         area(aggregate(gepa$emissions_6A_Landfills_Municipal,10))*
                         1000*1000/1e9,city_bounds)))
SSLF <- cbind(SSLF,c(SSLF[1]/totals_DF$Hi_res[2]*100,SSLF[2]/totals_DF$GEPA[2]*100))
rownames(SSLF) <- c("Hi_res","GEPA")
colnames(SSLF) <- c("SSLF Emissions (mol/s)","Fraction of Indy")
################################################################################
# now compare hi-res sectors to GEPA ones
setwd("..")
dir.create("GEPA_Hi_res_differences",showWarnings = F)
setwd("GEPA_Hi_res_differences")

# Hi_res_sectors <- c(Summed_stationary_combustion_FF+Summed_stationary_combustion_wood,
#                     Summed_NG_transmission+Summed_NG_dist,MSW_GHGRP+MSW_LMOP,
#                     Wastewater_dom_central+Wastewater_dom_septic_national)
Hi_res_sectors <- c(Summed_stationary_combustion_FF+Summed_stationary_combustion_wood,
                    Summed_NG_transmission,Summed_NG_dist,MSW_GHGRP+MSW_LMOP,
                    Wastewater_dom_central+Wastewater_dom_septic_national)

GEPA_values <- cellStats(area(gepa)*gepa*1000*1000,sum,na.rm=T)/1e9
#km2 area to m2 with 1000*1000, nmol/s to mol/s with 1/1e9
names(GEPA_values) <- names(gepa)
GEPA_values <- GEPA_values[c("emissions_1A_Combustion_Stationary",
                             "emissions_1B2b_Natural_Gas_Transmission",
                             "emissions_1B2b_Natural_Gas_Distribution",
                             "emissions_6A_Landfills_Municipal",
                             "emissions_6B_Wastewater_Treatment_Domestic",
                             "emissions_6B_Wastewater_Treatment_Industrial")]
GEPA_values["emissions_6B_Wastewater_Treatment_Domestic"] <- 
  GEPA_values["emissions_6B_Wastewater_Treatment_Domestic"]+
  GEPA_values["emissions_6B_Wastewater_Treatment_Industrial"]
GEPA_values <- GEPA_values[-which(names(GEPA_values)=="emissions_6B_Wastewater_Treatment_Industrial")]
# GEPA_values["emissions_1B2b_Natural_Gas_Distribution"] <- 
#   GEPA_values["emissions_1B2b_Natural_Gas_Transmission"]+
#   GEPA_values["emissions_1B2b_Natural_Gas_Distribution"]
# GEPA_values <- GEPA_values[-which(names(GEPA_values)=="emissions_1B2b_Natural_Gas_Transmission")]

Hi_res_values <- sapply(c(Hi_res_sectors),
                        FUN=function(x){cellStats(area(x)*x*1000*1000,sum,na.rm=T)/1e9})
#km2 area to m2 with 1000*1000, nmol/s to mol/s with 1/1e9

Barplot_dataframe <- t(cbind(Hi_res_values,GEPA_values))
colnames(Barplot_dataframe) <- c("Stationary Comb",
                                 # "NG T&D",
                                 "NG Transmission",
                                 "NG Distribution",
                                 "Landfill",
                                 "Wastewater")

png("GEPA_Hi_Res_comparison_barplot.png",width = 480*2,height=480*2)
par(mar=c(5, 4, 4, 2) + 0.1 + c(3,4,0,0))
barplot(Barplot_dataframe,beside=T,horiz=T,legend.text=T,las=1,
        xlab="mol/s",
        xaxt="n")
xtickmarks <- c(seq(0,max(Barplot_dataframe)+0.05*max(Barplot_dataframe),
                    by=round(max(Barplot_dataframe)/10)),
                round(max(Barplot_dataframe)+0.05*max(Barplot_dataframe)))
axis(1,xtickmarks)
abline(v=xtickmarks[-1],lty=2,col="lightgrey")
dev.off()




GEPA_values <- sapply(as.list(gepa),FUN = function(x){Extracted_emissions(x,city_bounds)})
names(GEPA_values) <- names(gepa)
GEPA_values <- GEPA_values[c("emissions_1A_Combustion_Stationary",
                             "emissions_1B2b_Natural_Gas_Transmission",
                             "emissions_1B2b_Natural_Gas_Distribution",
                             "emissions_6A_Landfills_Municipal",
                             "emissions_6B_Wastewater_Treatment_Domestic",
                             "emissions_6B_Wastewater_Treatment_Industrial")]
GEPA_values["emissions_6B_Wastewater_Treatment_Domestic"] <- 
  GEPA_values["emissions_6B_Wastewater_Treatment_Domestic"]+
  GEPA_values["emissions_6B_Wastewater_Treatment_Industrial"]
GEPA_values <- GEPA_values[-which(names(GEPA_values)=="emissions_6B_Wastewater_Treatment_Industrial")]
# GEPA_values["emissions_1B2b_Natural_Gas_Distribution"] <- 
#   GEPA_values["emissions_1B2b_Natural_Gas_Transmission"]+
#   GEPA_values["emissions_1B2b_Natural_Gas_Distribution"]
# GEPA_values <- GEPA_values[-which(names(GEPA_values)=="emissions_1B2b_Natural_Gas_Transmission")]

Hi_res_values <- sapply(Hi_res_sectors,FUN=function(x){Extracted_emissions(x,city_bounds)})

Barplot_dataframe <- t(cbind(Hi_res_values,GEPA_values))
colnames(Barplot_dataframe) <- c("Stationary Comb",
                                 # "NG T&D",
                                 "NG Transmission",
                                 "NG Distribution",
                                 "Landfill",
                                 "Wastewater")

png("GEPA_Hi_Res_Indy_comparison_barplot.png",width = 480*2,height=480*2)
par(mar=c(5, 4, 4, 2) + 0.1 + c(3,4,0,0))
barplot(Barplot_dataframe,beside=T,horiz=T,legend.text=T,las=1,
        xlab="mol/s",
        xaxt="n")
xtickmarks <- c(seq(0,max(Barplot_dataframe)+0.05*max(Barplot_dataframe),
                    by=round(max(Barplot_dataframe)/10)),
                round(max(Barplot_dataframe)+0.05*max(Barplot_dataframe)))
axis(1,xtickmarks)
abline(v=xtickmarks[-1],lty=2,col="lightgrey")
dev.off()








GEPA_values <- sapply(as.list(gepa),FUN = function(x){Extracted_emissions(x,city_bounds)})
GEPA_Barplot_data <- log10(GEPA_values)
GEPA_Barplot_data[is.infinite(GEPA_Barplot_data)] <- NA
names(GEPA_Barplot_data) <- names(gepa)


Hi_res_sectors <- c(NG_MnR_com,NG_MnR_res,NG_mains_com,NG_mains_res,
                    NG_meter_com,NG_meter_res,NG_services_com,NG_services_res,
                    NG_upsets_com,NG_upsets_res,Summed_NG_dist,
                    
                    NG_trans_compressors,NG_trans_pipes,Summed_NG_transmission,
                    
                    NG_post_meter_res,
                    
                    stat_comb_com_coal,stat_comb_com_gas,stat_comb_com_petr,
                    stat_comb_elec_coal,stat_comb_elec_gas,stat_comb_elec_petr,
                    stat_comb_ind_coal,stat_comb_ind_gas,stat_comb_ind_petr,
                    stat_comb_res_petr,Summed_stationary_combustion_FF,
                    
                    stat_comb_com_wood,stat_comb_elec_wood,stat_comb_ind_wood,
                    stat_comb_res_wood,Summed_stationary_combustion_wood,
                    
                    Wastewater_dom_central,Wastewater_dom_septic_national,
                    Summed_wastewater_treatment,
                    
                    Summed_wetchart,
                    
                    MSW_GHGRP,MSW_LMOP,Summed_landfill)
Hi_res_values <- sapply(Hi_res_sectors,FUN=function(x){Extracted_emissions(x,city_bounds)})

Hi_res_Barplot_data <- c(Hi_res_values[1:11],NA,
                         Hi_res_values[12:14],NA,
                         Hi_res_values[15],NA,
                         Hi_res_values[16:26],NA,
                         Hi_res_values[27:31],NA,
                         Hi_res_values[32:34],NA,
                         Hi_res_values[35],NA,
                         Hi_res_values[36:38])
Hi_res_Barplot_data <- log10(Hi_res_Barplot_data)
Hi_res_Barplot_data[is.infinite(Hi_res_Barplot_data)] <- NA
names(Hi_res_Barplot_data) <- c("NG_MnR_com","NG_MnR_res","NG_mains_com","NG_mains_res",
                                "NG_meter_com","NG_meter_res","NG_services_com","NG_services_res",
                                "NG_upsets_com","NG_upsets_res","TOTAL_NG_dist","",
                                
                                "NG_trans_compressors","NG_trans_pipes","TOTAL_NG_trans","",

                                "NG_post_meter_res","",
                                
                                "stat_comb_com_coal","stat_comb_com_gas","stat_comb_com_petr",
                                "stat_comb_elec_coal","stat_comb_elec_gas","stat_comb_elec_petr",
                                "stat_comb_ind_coal","stat_comb_ind_gas","stat_comb_ind_petr",
                                "stat_comb_res_petr","TOTAL_stat_comb_FF","",
                                
                                
                                "stat_comb_com_wood","stat_comb_elec_wood","stat_comb_ind_wood",
                                "stat_comb_res_wood","TOTAL_stat_comb_wood","",
                                
                                "Wastewater_central","Wastewater_septic",
                                "TOTAL_wastewater","",
                                
                                "Wetcharts","",
                                
                                "GHGRP_landfills","LMOP_landfills","TOTAL_landfills")
Xtick_min <- min(Hi_res_Barplot_data,GEPA_Barplot_data,na.rm=T)
Xtick_max <- max(Hi_res_Barplot_data,GEPA_Barplot_data,na.rm=T)
Xtick_min <- round(Xtick_min+Xtick_min*0.05,1)
Xtick_max <- round(Xtick_max+Xtick_max*0.05,1)

xtickmarks <- c(seq(Xtick_min,Xtick_max,
                           by=round(Xtick_max-Xtick_min)/10))


png("GEPA_sectoral_barplot.png",width = 480*2,height=480*2)
par(mar=c(5, 4, 4, 2) + 0.1 + c(3,17,0,0))
barplot(GEPA_Barplot_data,beside=T,horiz=T,las=1,
        xlab="log10(mol/s)",
        xaxt="n",
        main="GEPA",
        col=c("green","grey",rep("green",6),rep("grey",2),rep("green",7),rep("grey",3),rep("green",2)),
        legend.text=c("Used as-is in Hi-Res","Recalculated"),
        xlim=c(Xtick_min,Xtick_max))
axis(1,xtickmarks)
abline(v=xtickmarks[-1],lty=2,col="lightgrey")
abline(v=0)
dev.off()

png("Hi_Res_sectoral_barplot.png",width = 480*2,height=480*2)
par(mar=c(5, 4, 4, 2) + 0.1 + c(3,7,0,0))
barplot(Hi_res_Barplot_data,beside=T,horiz=T,legend.text=F,las=1,
        xlab="log10(mol/s)",
        xaxt="n",
        main="Hi-Res",
        xlim=c(Xtick_min,Xtick_max))
axis(1,xtickmarks)
abline(v=xtickmarks[-1],lty=2,col="lightgrey")
abline(v=0)
dev.off()










not_log_plot(aggregate(gepa$emissions_6A_Landfills_Industrial+
                         gepa$emissions_6A_Landfills_Municipal,fact=10)-
               aggregate(Summed_landfill,fact=10),
             filename = "GEPA_Hi_Res_Landfill_comparison.png",
             title = "GEPA - Hi_res Landfills")


not_log_plot(aggregate(gepa$emissions_1B2b_Natural_Gas_Distribution+
                         gepa$emissions_1B2b_Natural_Gas_Transmission,fact=10)-
               aggregate(Summed_NG_dist+Summed_NG_transmission,fact=10),
             filename = "GEPA_Hi_Res_NG_T&D_comparison.png",
             title = "GEPA - Hi_res NG T&D")



#If you could do the barplot for the Indy polygon, that would be
# great, yes. Also, what's the NG/total fraction for the GEPA and HiRes in Indy
# and for d03?


