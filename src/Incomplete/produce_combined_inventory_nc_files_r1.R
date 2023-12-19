## produce_combined_inventory_nc_files_r1.R
## In use 2022-04-21 09:00
#
# Original: Produce combined-sector level nc files for A1_WC_SN and V5_S1_SS
#
# r1: Updated for AL_AS1_WC_SS, VL_VS1_S1_SS, A1_VS1_WC_SN and V5_VS1_S1_SS

################################################################################
#User input
GEPA_file <- "G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Inventories/Gridded_EPA_2012/GEPA_Annual.nc"
Input_folder <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Joe_rerun/Processed/redo/"
# WETCHARTS <- 
# SOCCR1 <- 'S1_natural_tot.nc')
# SOCCR2 <- file.path(Input_folder,"Wetlands",'S2_natural_tot.nc')
Output_folder <- file.path(Input_folder,"Summed_Sectors")

# #select 1 of the 3 wetland calculation methods
# wetlands <- c("Wetcharts","SOCCR1","SOCCR2")
# wetlands <- wetlands[1]

# d03_rast <- raster(nrows=160, ncols=230, xmn=-87.3, xmx=(0.01*230)+-87.3, ymn=39, ymx=39+(0.01*160), crs=4326)
################################################################################
#load packages
i <- 1
packagecheck <- c("raster","ncdf4","tools")

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
#combine all the d03 data into 1 large raster
d03_maps <- c(
  # file.path("Wetlands",'wetcharts_d03.nc'),
  # file.path("Wetlands",'warm_wetcharts_d03.nc'),
  # file.path("Wetlands",'cold_wetcharts_d03.nc'),
  # file.path("Wetlands",'PFO_flux_SOCCR1_d03.nc'),
  # file.path("Wetlands",'PFO_flux_SOCCR2_d03.nc'),
  # file.path("Wetlands",'PNF_flux_SOCCR1_d03.nc'),
  # file.path("Wetlands",'PNF_flux_SOCCR2_d03.nc'),
  # file.path("Wetlands",'E2_flux_SOCCR1_d03.nc'),
  # file.path("Wetlands",'E2_flux_SOCCR2_d03.nc'),
  # file.path("Wetlands",'M2_flux_SOCCR1_d03.nc'),
  # file.path("Wetlands",'M2_flux_SOCCR2_d03.nc'),
  # file.path("Wetlands",'R1_flux_d03.nc'),
  # file.path("Wetlands",'R2_flux_d03.nc'),
  # file.path("Wetlands",'R3_flux_d03.nc'),
  # file.path("Wetlands",'R4_flux_d03.nc'),
  # file.path("Wetlands",'L1_flux_d03.nc'),
  # file.path("Wetlands",'L2_flux_d03.nc'),
  'MSW_GHGRP.nc',
  'MSW_LMOP.nc',
  'NG_dist_MnR_com_byLDC_aces.nc',
  'NG_dist_MnR_res_byLDC_aces.nc',
  'NG_dist_mains_com_byLDC_aces.nc',
  'NG_dist_mains_res_byLDC_aces.nc',
  'NG_dist_meter_com_byLDC_aces.nc',
  'NG_dist_meter_res_byLDC_aces.nc',
  'NG_dist_serv_com_byLDC_aces.nc',
  'NG_dist_serv_res_byLDC_aces.nc',
  'NG_dist_upset_com_byLDC_aces.nc',
  'NG_dist_upset_res_byLDC_aces.nc',
  'NG_post_meter_res_byLDC_aces.nc',
  'NG_dist_MnR_com_byLDC_vulcan.nc',
  'NG_dist_MnR_res_byLDC_vulcan.nc',
  'NG_dist_mains_com_byLDC_vulcan.nc',
  'NG_dist_mains_res_byLDC_vulcan.nc',
  'NG_dist_meter_com_byLDC_vulcan.nc',
  'NG_dist_meter_res_byLDC_vulcan.nc',
  'NG_dist_serv_com_byLDC_vulcan.nc',
  'NG_dist_serv_res_byLDC_vulcan.nc',
  'NG_dist_upset_com_byLDC_vulcan.nc',
  'NG_dist_upset_res_byLDC_vulcan.nc',
  'NG_post_meter_res_byLDC_vulcan.nc',
  'NG_trans_compressors.nc',
  'NG_trans_pipes.nc',
  'Wastewater_dom_central.nc',
  'Wastewater_dom_septic_bystate.nc',
  'Wastewater_dom_septic_national.nc',
  'Wastewater_ind.nc',
  'stat_comb_com_coal_bystate_aces.nc',
  'stat_comb_com_gas_bystate_aces.nc',
  'stat_comb_com_petr_bystate_aces.nc',
  'stat_comb_com_wood_bystate_aces.nc',
  'stat_comb_elec_coal_bystate_aces.nc',
  'stat_comb_elec_gas_bystate_aces.nc',
  'stat_comb_elec_petr_bystate_aces.nc',
  'stat_comb_elec_wood_bystate_aces.nc',
  'stat_comb_ind_coal_bystate_aces.nc',
  'stat_comb_ind_gas_bystate_aces.nc',
  'stat_comb_ind_petr_bystate_aces.nc',
  'stat_comb_ind_wood_bystate_aces.nc',
  'stat_comb_res_petr_bystate_aces.nc',
  'stat_comb_res_wood_bystate_aces.nc',
  'stat_comb_com_coal_bystate_vulcan.nc',
  'stat_comb_com_gas_bystate_vulcan.nc',
  'stat_comb_com_petr_bystate_vulcan.nc',
  'stat_comb_com_wood_bystate_vulcan.nc',
  'stat_comb_elec_coal_bystate_vulcan.nc',
  'stat_comb_elec_gas_bystate_vulcan.nc',
  'stat_comb_elec_petr_bystate_vulcan.nc',
  'stat_comb_elec_wood_bystate_vulcan.nc',
  'stat_comb_ind_coal_bystate_vulcan.nc',
  'stat_comb_ind_gas_bystate_vulcan.nc',
  'stat_comb_ind_petr_bystate_vulcan.nc',
  'stat_comb_ind_wood_bystate_vulcan.nc',
  'stat_comb_res_petr_bystate_vulcan.nc',
  'stat_comb_res_wood_bystate_vulcan.nc')

# Load in new inventory d03
d03_files <- file.path(Input_folder, d03_maps)
HR_d03 <- stack(d03_files[1])

#disaggregate d03 wetcharts to the proper resolution (is just a cropped d03 for
#now)
# HR_d03 <- disaggregate(HR_d03,fact=10)

for(i in 2:length(d03_files)){
  HR_d03 <- stack(HR_d03, stack(d03_files[i]))
  cat("\rFinished loading",i,"of",length(d03_files),"               ")
}
HR_d03 <- HR_d03+0 #force each to be in memory, allowing writing later (any math)
names(HR_d03) <- file_path_sans_ext(basename(d03_files))
rm(d03_files,d03_maps,i)
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
gepa <- crop(gepa,HR_d03)

#convert units
gepa <- gepa*(1e9*10^2^2)/(6.022141e+23)
#molec/cm2/s to nmol/m2/s

rm(GEPA_sectors,A,GEPA_file)
################################################################################
# Collect the maps for the different combinations together
AC_VS1_S1_SS_NG_dist <- (HR_d03$NG_dist_MnR_com_byLDC_aces +
                           HR_d03$NG_dist_MnR_res_byLDC_aces +
                           HR_d03$NG_dist_mains_com_byLDC_aces +
                           HR_d03$NG_dist_mains_res_byLDC_aces +
                           HR_d03$NG_dist_meter_com_byLDC_aces +
                           HR_d03$NG_dist_meter_res_byLDC_aces +
                           HR_d03$NG_dist_serv_com_byLDC_aces +
                           HR_d03$NG_dist_serv_res_byLDC_aces +
                           HR_d03$NG_dist_upset_com_byLDC_aces +
                           HR_d03$NG_dist_upset_res_byLDC_aces)

VL_VS1_S1_SS_NG_dist <- (HR_d03$NG_dist_MnR_com_byLDC_vulcan +
                           HR_d03$NG_dist_MnR_res_byLDC_vulcan +
                           HR_d03$NG_dist_mains_com_byLDC_vulcan +
                           HR_d03$NG_dist_mains_res_byLDC_vulcan +
                           HR_d03$NG_dist_meter_com_byLDC_vulcan +
                           HR_d03$NG_dist_meter_res_byLDC_vulcan +
                           HR_d03$NG_dist_serv_com_byLDC_vulcan +
                           HR_d03$NG_dist_serv_res_byLDC_vulcan +
                           HR_d03$NG_dist_upset_com_byLDC_vulcan +
                           HR_d03$NG_dist_upset_res_byLDC_vulcan)

VL_VS1_S1_SS_post_meter <- HR_d03$NG_post_meter_res_byLDC_vulcan

AC_VS1_S1_SS_post_meter <- HR_d03$NG_post_meter_res_byLDC_aces

VL_VS1_S1_SS_stat_comb_FF <- (HR_d03$stat_comb_com_coal_bystate_vulcan +
                                HR_d03$stat_comb_com_gas_bystate_vulcan +
                                HR_d03$stat_comb_com_petr_bystate_vulcan +
                                HR_d03$stat_comb_elec_coal_bystate_vulcan +
                                HR_d03$stat_comb_elec_gas_bystate_vulcan +
                                HR_d03$stat_comb_elec_petr_bystate_vulcan +
                                HR_d03$stat_comb_ind_coal_bystate_vulcan +
                                HR_d03$stat_comb_ind_gas_bystate_vulcan +
                                HR_d03$stat_comb_ind_petr_bystate_vulcan +
                                HR_d03$stat_comb_res_petr_bystate_vulcan)

AC_VS1_S1_SS_stat_comb_FF <- (HR_d03$stat_comb_com_coal_bystate_aces +
                                HR_d03$stat_comb_com_gas_bystate_aces +
                                HR_d03$stat_comb_com_petr_bystate_aces +
                                HR_d03$stat_comb_elec_coal_bystate_aces +
                                HR_d03$stat_comb_elec_gas_bystate_aces +
                                HR_d03$stat_comb_elec_petr_bystate_aces +
                                HR_d03$stat_comb_ind_coal_bystate_aces +
                                HR_d03$stat_comb_ind_gas_bystate_aces +
                                HR_d03$stat_comb_ind_petr_bystate_aces +
                                HR_d03$stat_comb_res_petr_bystate_aces)

AC_VS1_S1_SS_stat_comb_wood <- (HR_d03$stat_comb_com_wood_bystate_aces +
                                  HR_d03$stat_comb_elec_wood_bystate_aces +
                                  HR_d03$stat_comb_ind_wood_bystate_aces +
                                  HR_d03$stat_comb_res_wood_bystate_aces)

VL_VS1_S1_SS_stat_comb_wood <- (HR_d03$stat_comb_com_wood_bystate_vulcan +
                                  HR_d03$stat_comb_elec_wood_bystate_vulcan +
                                  HR_d03$stat_comb_ind_wood_bystate_vulcan +
                                  HR_d03$stat_comb_res_wood_bystate_vulcan)

VL_VS1_S1_SS_WW <- (HR_d03$Wastewater_dom_central +
                      HR_d03$Wastewater_dom_septic_national +
                      HR_d03$Wastewater_ind)


# VL_wetcharts <- (
#   HR_d03$warm_wetcharts_d03 +
#     HR_d03$L1_flux_d03 +
#     HR_d03$L2_flux_d03 +
#     HR_d03$R1_flux_d03 +
#     HR_d03$R2_flux_d03 +
#     HR_d03$R3_flux_d03 +
#     HR_d03$R4_flux_d03)
# VL_wetcharts <- (
#   HR_d03$cold_wetcharts_d03 +
#     HR_d03$L1_flux_d03 +
#     HR_d03$L2_flux_d03 +
#     HR_d03$R1_flux_d03 +
#     HR_d03$R2_flux_d03 +
#     HR_d03$R3_flux_d03 +
#     HR_d03$R4_flux_d03)
# VL_wetcharts <- (
#   HR_d03$wetcharts_d03 +
#     HR_d03$L1_flux_d03 +
#     HR_d03$L2_flux_d03 +
#     HR_d03$R1_flux_d03 +
#     HR_d03$R2_flux_d03 +
#     HR_d03$R3_flux_d03 +
#     HR_d03$R4_flux_d03)

# VL_SOCCR1 <- (HR_d03$E2_flux_SOCCR1_d03 +
#                 HR_d03$M2_flux_SOCCR1_d03 +
#                 HR_d03$PFO_flux_SOCCR1_d03 +
#                 HR_d03$PNF_flux_SOCCR1_d03 +
#                 HR_d03$L1_flux_d03 +
#                 HR_d03$L2_flux_d03 +
#                 HR_d03$R1_flux_d03 +
#                 HR_d03$R2_flux_d03 +
#                 HR_d03$R3_flux_d03 +
#                 HR_d03$R4_flux_d03)
# VL_SOCCR2 <- (HR_d03$E2_flux_SOCCR2_d03 +
#                 HR_d03$M2_flux_SOCCR2_d03 +
#                 HR_d03$PFO_flux_SOCCR2_d03 +
#                 HR_d03$PNF_flux_SOCCR2_d03 +
#                 HR_d03$L1_flux_d03 +
#                 HR_d03$L2_flux_d03 +
#                 HR_d03$R1_flux_d03 +
#                 HR_d03$R2_flux_d03 +
#                 HR_d03$R3_flux_d03 +
#                 HR_d03$R4_flux_d03)


# These are the same in all four inventories
NG_trans <- (HR_d03$NG_trans_compressors +
               HR_d03$NG_trans_pipes)

landfill <- (HR_d03$MSW_GHGRP +
               HR_d03$MSW_LMOP +
               gepa$emissions_6A_Landfills_Industrial)

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

Total_Vulcan <- (landfill+
                   VL_VS1_S1_SS_NG_dist+
                   VL_VS1_S1_SS_post_meter+
                   NG_trans+
                   other_FF+
                   other_non_FF+
                   VL_VS1_S1_SS_stat_comb_FF+
                   VL_VS1_S1_SS_stat_comb_wood+
                   VL_VS1_S1_SS_WW)

Total_ACES <- (landfill+
                 AC_VS1_S1_SS_NG_dist+
                 AC_VS1_S1_SS_post_meter+
                 NG_trans+
                 other_FF+
                 other_non_FF+
                 AC_VS1_S1_SS_stat_comb_FF+
                 AC_VS1_S1_SS_stat_comb_wood+
                 VL_VS1_S1_SS_WW)
# HR_d03$Wetcharts)
# HR_d03$SOCCR1)
# HR_d03$SOCCR2)

# Thermo_total <- (VL_VS1_S1_SS_NG_dist+
#                    VL_VS1_S1_SS_post_meter+
#                    NG_trans+
#                    VL_VS1_S1_SS_stat_comb_FF+
#                    other_FF)
# non_Thermo_total <- (landfill+
#                        other_non_FF+
#                        VL_VS1_S1_SS_stat_comb_wood+
#                        VL_VS1_S1_SS_WW+
#                        VL_wetcharts)


################################################################################
# Now write the rasters as netcdf files

dir.create(Output_folder,showWarnings = F)

# writeRaster(Thermo_total,
#             file.path(Output_folder,'Thermogenic_total.nc'),
#             force_v4=TRUE,
#             varname='methane_emissions',
#             varunit='nmol/m2/s',
#             longname='Methane emissions from all thermogenic sources',
#             NAflag=-9999,
#             overwrite=TRUE)
# writeRaster(non_Thermo_total,
#             file.path(Output_folder,'Non_thermogenic_total.nc'),
#             force_v4=TRUE,
#             varname='methane_emissions',
#             varunit='nmol/m2/s',
#             longname='Methane emissions from all non-thermogenic sources',
#             NAflag=-9999,
#             overwrite=TRUE)
writeRaster(VL_VS1_S1_SS_NG_dist,
            file.path(Output_folder,'NG_distribution_vulcan.nc'),
            force_v4=TRUE,
            varname='methane_emissions',
            varunit='nmol/m2/s',
            longname='Methane emissions from the natural gas distribution network,
            spatially disaggregated from local distribution company totals using Vulcan residential and commercial CO2 emissions',
            NAflag=-9999,
            overwrite=TRUE)

writeRaster(AC_VS1_S1_SS_NG_dist,
            file.path(Output_folder,'NG_distribution_aces.nc'),
            force_v4=TRUE,
            varname='methane_emissions',
            varunit='nmol/m2/s',
            longname='Methane emissions from the natural gas distribution network,
            spatially disaggregated from local distribution company totals using aces residential and commercial CO2 emissions',
            NAflag=-9999,
            overwrite=TRUE)

writeRaster(VL_VS1_S1_SS_post_meter,
            file.path(Output_folder,'NG_residential_post_meter_vulcan.nc'),
            force_v4=TRUE,
            varname='methane_emissions',
            varunit='nmol/m2/s',
            longname='Methane emissions from downstream of residential consumer meters,
            spatially disaggregated from local distribution company totals using Vulcan residential CO2 emissions',
            NAflag=-9999,
            overwrite=TRUE)

writeRaster(AC_VS1_S1_SS_post_meter,
            file.path(Output_folder,'NG_residential_post_meter_aces.nc'),
            force_v4=TRUE,
            varname='methane_emissions',
            varunit='nmol/m2/s',
            longname='Methane emissions from downstream of residential consumer meters,
            spatially disaggregated from local distribution company totals using aces residential CO2 emissions',
            NAflag=-9999,
            overwrite=TRUE)

writeRaster(VL_VS1_S1_SS_stat_comb_FF,
            file.path(Output_folder,'Stationary_combustion_FF_vulcan.nc'),
            force_v4=TRUE,
            varname='methane_emissions',
            varunit='nmol/m2/s',
            longname='Methane emissions from stationary combustion of fossil fuels (excluding residential natural gas),
            spatially disaggregated from state totals using sector-specific Vulcan CO2 emissions',
            NAflag=-9999,
            overwrite=TRUE)

writeRaster(AC_VS1_S1_SS_stat_comb_FF,
            file.path(Output_folder,'Stationary_combustion_FF_aces.nc'),
            force_v4=TRUE,
            varname='methane_emissions',
            varunit='nmol/m2/s',
            longname='Methane emissions from stationary combustion of fossil fuels (excluding residential natural gas),
            spatially disaggregated from state totals using sector-specific aces CO2 emissions',
            NAflag=-9999,
            overwrite=TRUE)

writeRaster(VL_VS1_S1_SS_stat_comb_wood,
            file.path(Output_folder,'Stationary_combustion_wood_vulcan.nc'),
            force_v4=TRUE,
            varname='methane_emissions',
            varunit='nmol/m2/s',
            longname='Methane emissions from stationary combustion of wood,
            spatially disaggregated from state totals using county-level CO emissions (NEI) and sector-specific Vulcan CO2 emissions',
            NAflag=-9999,
            overwrite=TRUE)

writeRaster(AC_VS1_S1_SS_stat_comb_wood,
            file.path(Output_folder,'Stationary_combustion_wood_aces.nc'),
            force_v4=TRUE,
            varname='methane_emissions',
            varunit='nmol/m2/s',
            longname='Methane emissions from stationary combustion of wood,
            spatially disaggregated from state totals using county-level CO emissions (NEI) and sector-specific aces CO2 emissions',
            NAflag=-9999,
            overwrite=TRUE)

writeRaster(VL_VS1_S1_SS_WW,
            file.path(Output_folder,'Wastewater_treatment.nc'),
            force_v4=TRUE,
            varname='methane_emissions',
            varunit='nmol/m2/s',
            longname='Methane emissions from wastewater treatment, including municipal and industrial treatment plants,
            and onsite treatment (disaggregated from state totals according to land cover)',
            NAflag=-9999,
            overwrite=TRUE)

# writeRaster(VL_wetcharts,
#             file.path(Output_folder,'Wetcharts.nc'),
#             force_v4=TRUE,
#             varname='methane_emissions',
#             varunit='nmol/m2/s',
#             longname='Methane emissions from wetlands, rivers and lakes.
#             Wetland fluxes are calculated based on Wetcharts',
#             NAflag=-9999,
#             overwrite=TRUE)
# writeRaster(VL_wetcharts,
#             file.path(Output_folder,'Warm_Wetcharts.nc'),
#             force_v4=TRUE,
#             varname='methane_emissions',
#             varunit='nmol/m2/s',
#             longname='Methane emissions from wetlands, rivers and lakes.
#             Wetland fluxes are calculated based on Wetcharts',
#             NAflag=-9999,
#             overwrite=TRUE)
# writeRaster(VL_wetcharts,
#             file.path(Output_folder,'Cold_Wetcharts.nc'),
#             force_v4=TRUE,
#             varname='methane_emissions',
#             varunit='nmol/m2/s',
#             longname='Methane emissions from wetlands, rivers and lakes.
#             Wetland fluxes are calculated based on Wetcharts',
#             NAflag=-9999,
#             overwrite=TRUE)

# writeRaster(VL_SOCCR1,
#             file.path(Output_folder,'SOCCR1.nc'),
#             force_v4=TRUE,
#             varname='methane_emissions',
#             varunit='nmol/m2/s',
#             longname='Methane emissions from wetlands, rivers and lakes.
#             Wetland fluxes are calculated based on the first State of the Carbon Cycle Report',
#             NAflag=-9999,
#             overwrite=TRUE)
# 
# writeRaster(VL_SOCCR2,
#             file.path(Output_folder,'SOCCR2.nc'),
#             force_v4=TRUE,
#             varname='methane_emissions',
#             varunit='nmol/m2/s',
#             longname='Methane emissions from wetlands, rivers and lakes.
#             Wetland fluxes are calculated based on the second State of the Carbon Cycle Report',
#             NAflag=-9999,
#             overwrite=TRUE)
# 
writeRaster(NG_trans,
            file.path(Output_folder,'NG_transmission.nc'),
            force_v4=TRUE,
            varname='methane_emissions',
            varunit='nmol/m2/s',
            longname='Methane emissions from the natural gas transmission network',
            NAflag=-9999,
            overwrite=TRUE)

writeRaster(landfill,
            file.path(Output_folder,'Landfill.nc'),
            force_v4=TRUE,
            varname='methane_emissions',
            varunit='nmol/m2/s',
            longname='Methane emissions from municipal and industrial landfills',
            NAflag=-9999,
            overwrite=TRUE)

writeRaster(other_non_FF,
            file.path(Output_folder,'Other_nonFF.nc'),
            force_v4=TRUE,
            varname='methane_emissions',
            varunit='nmol/m2/s',
            longname='Methane emissions from other non-thermogenic sources (primarily composting and agriculture),
            taken straight from the GEPA inventory',
            NAflag=-9999,
            overwrite=TRUE)

writeRaster(other_FF,
            file.path(Output_folder,'Other_FF.nc'),
            force_v4=TRUE,
            varname='methane_emissions',
            varunit='nmol/m2/s',
            longname='Methane emissions from other thermogenic sources, taken straight from the GEPA inventory',
            NAflag=-9999,
            overwrite=TRUE)

writeRaster(Total_Vulcan,
            file.path(Output_folder,'d03_Total_vulcan.nc'),
            force_v4=TRUE,
            varname='methane_emissions',
            varunit='nmol/m2/s',
            longname='Methane emissions across all sectors, ignoring wetlands',
            NAflag=-9999,
            overwrite=TRUE)

writeRaster(Total_ACES,
            file.path(Output_folder,'d03_Total_aces.nc'),
            force_v4=TRUE,
            varname='methane_emissions',
            varunit='nmol/m2/s',
            longname='Methane emissions across all sectors, ignoring wetlands',
            NAflag=-9999,
            overwrite=TRUE)

################################################################################
#Lastly, save the GEPA sectors to a different folder for reference

# writeRaster(gepa$emissions_1A_Combustion_Mobile,
#             file.path(Input_folder,'d03_GEPA_Combustion_Mobile.nc'),
#             force_v4=TRUE,
#             varname='methane_emissions',
#             varunit='nmol/m2/s',
#             longname='Methane emissions straight from the GEPA inventory',
#             NAflag=-9999,
#             overwrite=TRUE)
# writeRaster(gepa$emissions_1B1a_Abandoned_Coal,
#             file.path(Input_folder,'d03_GEPA_Abandoned_Coal.nc'),
#             force_v4=TRUE,
#             varname='methane_emissions',
#             varunit='nmol/m2/s',
#             longname='Methane emissions straight from the GEPA inventory',
#             NAflag=-9999,
#             overwrite=TRUE)
# writeRaster(gepa$emissions_1B1a_Coal_Mining_Surface,
#             file.path(Input_folder,'d03_GEPA_Coal_Mining_Surface.nc'),
#             force_v4=TRUE,
#             varname='methane_emissions',
#             varunit='nmol/m2/s',
#             longname='Methane emissions straight from the GEPA inventory',
#             NAflag=-9999,
#             overwrite=TRUE)
# writeRaster(gepa$emissions_1B1a_Coal_Mining_Underground,
#             file.path(Input_folder,'d03_GEPA_Coal_Mining_Underground.nc'),
#             force_v4=TRUE,
#             varname='methane_emissions',
#             varunit='nmol/m2/s',
#             longname='Methane emissions straight from the GEPA inventory',
#             NAflag=-9999,
#             overwrite=TRUE)
# writeRaster(gepa$emissions_1B2a_Petroleum,
#             file.path(Input_folder,'d03_GEPA_Petroleum.nc'),
#             force_v4=TRUE,
#             varname='methane_emissions',
#             varunit='nmol/m2/s',
#             longname='Methane emissions straight from the GEPA inventory',
#             NAflag=-9999,
#             overwrite=TRUE)
# writeRaster(gepa$emissions_1B2b_Natural_Gas_Processing,
#             file.path(Input_folder,'d03_GEPA_Natural_Gas_Processing.nc'),
#             force_v4=TRUE,
#             varname='methane_emissions',
#             varunit='nmol/m2/s',
#             longname='Methane emissions straight from the GEPA inventory',
#             NAflag=-9999,
#             overwrite=TRUE)
# writeRaster(gepa$emissions_1B2b_Natural_Gas_Production,
#             file.path(Input_folder,'d03_GEPA_Natural_Gas_Production.nc'),
#             force_v4=TRUE,
#             varname='methane_emissions',
#             varunit='nmol/m2/s',
#             longname='Methane emissions straight from the GEPA inventory',
#             NAflag=-9999,
#             overwrite=TRUE)
# writeRaster(gepa$emissions_2B5_Petrochemical_Production,
#             file.path(Input_folder,'d03_GEPA_Petrochemical_Production.nc'),
#             force_v4=TRUE,
#             varname='methane_emissions',
#             varunit='nmol/m2/s',
#             longname='Methane emissions straight from the GEPA inventory',
#             NAflag=-9999,
#             overwrite=TRUE)
# writeRaster(gepa$emissions_2C2_Ferroalloy_Production,
#             file.path(Input_folder,'d03_GEPA_Ferroalloy_Production.nc'),
#             force_v4=TRUE,
#             varname='methane_emissions',
#             varunit='nmol/m2/s',
#             longname='Methane emissions straight from the GEPA inventory',
#             NAflag=-9999,
#             overwrite=TRUE)
# writeRaster(gepa$emissions_6D_Composting,
#             file.path(Input_folder,'d03_GEPA_Composting.nc'),
#             force_v4=TRUE,
#             varname='methane_emissions',
#             varunit='nmol/m2/s',
#             longname='Methane emissions straight from the GEPA inventory',
#             NAflag=-9999,
#             overwrite=TRUE)
# writeRaster(gepa$emissions_4A_Enteric_Fermentation,
#             file.path(Input_folder,'d03_GEPA_Enteric_Fermentation.nc'),
#             force_v4=TRUE,
#             varname='methane_emissions',
#             varunit='nmol/m2/s',
#             longname='Methane emissions straight from the GEPA inventory',
#             NAflag=-9999,
#             overwrite=TRUE)
# writeRaster(gepa$emissions_4B_Manure_Management,
#             file.path(Input_folder,'d03_GEPA_Manure_Management.nc'),
#             force_v4=TRUE,
#             varname='methane_emissions',
#             varunit='nmol/m2/s',
#             longname='Methane emissions straight from the GEPA inventory',
#             NAflag=-9999,
#             overwrite=TRUE)
# writeRaster(gepa$emissions_4C_Rice_Cultivation,
#             file.path(Input_folder,'d03_GEPA_Rice_Cultivation.nc'),
#             force_v4=TRUE,
#             varname='methane_emissions',
#             varunit='nmol/m2/s',
#             longname='Methane emissions straight from the GEPA inventory',
#             NAflag=-9999,
#             overwrite=TRUE)
# writeRaster(gepa$emissions_4F_Field_Burning,
#             file.path(Input_folder,'d03_GEPA_Field_Burning.nc'),
#             force_v4=TRUE,
#             varname='methane_emissions',
#             varunit='nmol/m2/s',
#             longname='Methane emissions straight from the GEPA inventory',
#             NAflag=-9999,
#             overwrite=TRUE)
# writeRaster(gepa$emissions_5_Forest_Fires,
#             file.path(Input_folder,'d03_GEPA_Forest_Fires.nc'),
#             force_v4=TRUE,
#             varname='methane_emissions',
#             varunit='nmol/m2/s',
#             longname='Methane emissions straight from the GEPA inventory',
#             NAflag=-9999,
#             overwrite=TRUE)
# writeRaster(gepa$emissions_6A_Landfills_Industrial,
#             file.path(Input_folder,'d03_GEPA_Landfills_Industrial.nc'),
#             force_v4=TRUE,
#             varname='methane_emissions',
#             varunit='nmol/m2/s',
#             longname='Methane emissions straight from the GEPA inventory',
#             NAflag=-9999,
#             overwrite=TRUE)






