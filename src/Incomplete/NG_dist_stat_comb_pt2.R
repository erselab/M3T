## NG_dist_stat_comb_pt2.R
## In use: 2022-03-31 07:30
#
# Convert the output from the xesmf regridder to the correct units/format
# This became necessary after switching to NG_distribution_emissions_r3.R and stationary_combustion_r3.R
# for the main calculation, as we now calculate CH4 emissions for each source category on the native
# ACES/Vulcan LCC grid, then regrid these CH4 emissions using xesmf (whereas previously we regridded
# the ACES/Vulcan CO2 emissions, then rescaled using CH4 totals for each source category)

################################################################################
#User input
filepath_in <- 'G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed/'
filepath_out <- 'G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed/'

subset_files <- c(12:25,48:61,73:86,109:122)
#subset the data to process from the section on line 42.  This one would be only
#stationary combustion.

# subset_files <- (1:122)[-c(12:25,48:61,73:86,109:122)]
#this one would be only NG distribution

plotting_function <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Code and method/Scripts/Plotting_individual_sectors.R"
#the location of another script that just creates some functions for consistent,
#quick plotting

################################################################################
#load packages
i <- 1
packagecheck <- c("raster","ncdf4")

while(i<=length(packagecheck)){
  if(length(find.package(packagecheck[i],quiet = TRUE))<1){
    install.packages(packagecheck[i],repos="https://repo.miserver.it.umich.edu/cran/")
  }
  i <- i+1
}

invisible(lapply(packagecheck, require, character.only=TRUE))
rm(packagecheck,i)

#raster + ncdf4 = raster and .nc filetype functionalities
################################################################################
#list out all the files.  Input names, output names, variable names.  Then
#subset them if there is a subset.

file_list_in <- c('aces_bydomain_NG_dist_MnR_ER_total_com_regridded.nc',
                  'aces_bydomain_NG_dist_MnR_ER_total_res_regridded.nc',
                  'aces_bydomain_NG_dist_mains_ER_total_com_regridded.nc',
                  'aces_bydomain_NG_dist_mains_ER_total_res_regridded.nc',
                  'aces_bydomain_NG_dist_meter_ER_total_com_regridded.nc',
                  'aces_bydomain_NG_dist_meter_ER_total_res_regridded.nc',
                  'aces_bydomain_NG_dist_post_meter_ER_total_res_regridded.nc',
                  'aces_bydomain_NG_dist_serv_ER_total_com_regridded.nc',
                  'aces_bydomain_NG_dist_serv_ER_total_res_regridded.nc',
                  'aces_bydomain_NG_dist_upset_ER_total_com_regridded.nc',
                  'aces_bydomain_NG_dist_upset_ER_total_res_regridded.nc',
                  'aces_bydomain_stat_comb_com_coal_ER_regridded.nc',
                  'aces_bydomain_stat_comb_com_gas_ER_regridded.nc',
                  'aces_bydomain_stat_comb_com_petr_ER_regridded.nc',
                  'aces_bydomain_stat_comb_com_wood_ER_regridded.nc',
                  'aces_bydomain_stat_comb_elec_coal_ER_regridded.nc',
                  'aces_bydomain_stat_comb_elec_gas_ER_regridded.nc',
                  'aces_bydomain_stat_comb_elec_petr_ER_regridded.nc',
                  'aces_bydomain_stat_comb_elec_wood_ER_regridded.nc',
                  'aces_bydomain_stat_comb_ind_coal_ER_regridded.nc',
                  'aces_bydomain_stat_comb_ind_gas_ER_regridded.nc',
                  'aces_bydomain_stat_comb_ind_petr_ER_regridded.nc',
                  'aces_bydomain_stat_comb_ind_wood_ER_regridded.nc',
                  'aces_bydomain_stat_comb_res_petr_ER_regridded.nc',
                  'aces_bydomain_stat_comb_res_wood_ER_regridded.nc',
                  'aces_byLDC_NG_dist_MnR_ER_total_com_regridded.nc',
                  'aces_byLDC_NG_dist_MnR_ER_total_res_regridded.nc',
                  'aces_byLDC_NG_dist_mains_ER_total_com_regridded.nc',
                  'aces_byLDC_NG_dist_mains_ER_total_res_regridded.nc',
                  'aces_byLDC_NG_dist_meter_ER_total_com_regridded.nc',
                  'aces_byLDC_NG_dist_meter_ER_total_res_regridded.nc',
                  'aces_byLDC_NG_dist_post_meter_ER_total_res_regridded.nc',
                  'aces_byLDC_NG_dist_serv_ER_total_com_regridded.nc',
                  'aces_byLDC_NG_dist_serv_ER_total_res_regridded.nc',
                  'aces_byLDC_NG_dist_upset_ER_total_com_regridded.nc',
                  'aces_byLDC_NG_dist_upset_ER_total_res_regridded.nc',
                  'aces_bystate_NG_dist_MnR_ER_total_com_regridded.nc',
                  'aces_bystate_NG_dist_MnR_ER_total_res_regridded.nc',
                  'aces_bystate_NG_dist_mains_ER_total_com_regridded.nc',
                  'aces_bystate_NG_dist_mains_ER_total_res_regridded.nc',
                  'aces_bystate_NG_dist_meter_ER_total_com_regridded.nc',
                  'aces_bystate_NG_dist_meter_ER_total_res_regridded.nc',
                  'aces_bystate_NG_dist_post_meter_ER_total_res_regridded.nc',
                  'aces_bystate_NG_dist_serv_ER_total_com_regridded.nc',
                  'aces_bystate_NG_dist_serv_ER_total_res_regridded.nc',
                  'aces_bystate_NG_dist_upset_ER_total_com_regridded.nc',
                  'aces_bystate_NG_dist_upset_ER_total_res_regridded.nc',
                  'aces_bystate_stat_comb_com_coal_ER_regridded.nc',
                  'aces_bystate_stat_comb_com_gas_ER_regridded.nc',
                  'aces_bystate_stat_comb_com_petr_ER_regridded.nc',
                  'aces_bystate_stat_comb_com_wood_ER_regridded.nc',
                  'aces_bystate_stat_comb_elec_coal_ER_regridded.nc',
                  'aces_bystate_stat_comb_elec_gas_ER_regridded.nc',
                  'aces_bystate_stat_comb_elec_petr_ER_regridded.nc',
                  'aces_bystate_stat_comb_elec_wood_ER_regridded.nc',
                  'aces_bystate_stat_comb_ind_coal_ER_regridded.nc',
                  'aces_bystate_stat_comb_ind_gas_ER_regridded.nc',
                  'aces_bystate_stat_comb_ind_petr_ER_regridded.nc',
                  'aces_bystate_stat_comb_ind_wood_ER_regridded.nc',
                  'aces_bystate_stat_comb_res_petr_ER_regridded.nc',
                  'aces_bystate_stat_comb_res_wood_ER_regridded.nc',
                  'vu_bydomain_NG_dist_MnR_ER_total_com_regridded.nc',
                  'vu_bydomain_NG_dist_MnR_ER_total_res_regridded.nc',
                  'vu_bydomain_NG_dist_mains_ER_total_com_regridded.nc',
                  'vu_bydomain_NG_dist_mains_ER_total_res_regridded.nc',
                  'vu_bydomain_NG_dist_meter_ER_total_com_regridded.nc',
                  'vu_bydomain_NG_dist_meter_ER_total_res_regridded.nc',
                  'vu_bydomain_NG_dist_post_meter_ER_total_res_regridded.nc',
                  'vu_bydomain_NG_dist_serv_ER_total_com_regridded.nc',
                  'vu_bydomain_NG_dist_serv_ER_total_res_regridded.nc',
                  'vu_bydomain_NG_dist_upset_ER_total_com_regridded.nc',
                  'vu_bydomain_NG_dist_upset_ER_total_res_regridded.nc',
                  'vu_bydomain_stat_comb_com_coal_ER_regridded.nc',
                  'vu_bydomain_stat_comb_com_gas_ER_regridded.nc',
                  'vu_bydomain_stat_comb_com_petr_ER_regridded.nc',
                  'vu_bydomain_stat_comb_com_wood_ER_regridded.nc',
                  'vu_bydomain_stat_comb_elec_coal_ER_regridded.nc',
                  'vu_bydomain_stat_comb_elec_gas_ER_regridded.nc',
                  'vu_bydomain_stat_comb_elec_petr_ER_regridded.nc',
                  'vu_bydomain_stat_comb_elec_wood_ER_regridded.nc',
                  'vu_bydomain_stat_comb_ind_coal_ER_regridded.nc',
                  'vu_bydomain_stat_comb_ind_gas_ER_regridded.nc',
                  'vu_bydomain_stat_comb_ind_petr_ER_regridded.nc',
                  'vu_bydomain_stat_comb_ind_wood_ER_regridded.nc',
                  'vu_bydomain_stat_comb_res_petr_ER_regridded.nc',
                  'vu_bydomain_stat_comb_res_wood_ER_regridded.nc',
                  'vu_byLDC_NG_dist_MnR_ER_total_com_regridded.nc',
                  'vu_byLDC_NG_dist_MnR_ER_total_res_regridded.nc',
                  'vu_byLDC_NG_dist_mains_ER_total_com_regridded.nc',
                  'vu_byLDC_NG_dist_mains_ER_total_res_regridded.nc',
                  'vu_byLDC_NG_dist_meter_ER_total_com_regridded.nc',
                  'vu_byLDC_NG_dist_meter_ER_total_res_regridded.nc',
                  'vu_byLDC_NG_dist_post_meter_ER_total_res_regridded.nc',
                  'vu_byLDC_NG_dist_serv_ER_total_com_regridded.nc',
                  'vu_byLDC_NG_dist_serv_ER_total_res_regridded.nc',
                  'vu_byLDC_NG_dist_upset_ER_total_com_regridded.nc',
                  'vu_byLDC_NG_dist_upset_ER_total_res_regridded.nc',
                  'vu_bystate_NG_dist_MnR_ER_total_com_regridded.nc',
                  'vu_bystate_NG_dist_MnR_ER_total_res_regridded.nc',
                  'vu_bystate_NG_dist_mains_ER_total_com_regridded.nc',
                  'vu_bystate_NG_dist_mains_ER_total_res_regridded.nc',
                  'vu_bystate_NG_dist_meter_ER_total_com_regridded.nc',
                  'vu_bystate_NG_dist_meter_ER_total_res_regridded.nc',
                  'vu_bystate_NG_dist_post_meter_ER_total_res_regridded.nc',
                  'vu_bystate_NG_dist_serv_ER_total_com_regridded.nc',
                  'vu_bystate_NG_dist_serv_ER_total_res_regridded.nc',
                  'vu_bystate_NG_dist_upset_ER_total_com_regridded.nc',
                  'vu_bystate_NG_dist_upset_ER_total_res_regridded.nc',
                  'vu_bystate_stat_comb_com_coal_ER_regridded.nc',
                  'vu_bystate_stat_comb_com_gas_ER_regridded.nc',
                  'vu_bystate_stat_comb_com_petr_ER_regridded.nc',
                  'vu_bystate_stat_comb_com_wood_ER_regridded.nc',
                  'vu_bystate_stat_comb_elec_coal_ER_regridded.nc',
                  'vu_bystate_stat_comb_elec_gas_ER_regridded.nc',
                  'vu_bystate_stat_comb_elec_petr_ER_regridded.nc',
                  'vu_bystate_stat_comb_elec_wood_ER_regridded.nc',
                  'vu_bystate_stat_comb_ind_coal_ER_regridded.nc',
                  'vu_bystate_stat_comb_ind_gas_ER_regridded.nc',
                  'vu_bystate_stat_comb_ind_petr_ER_regridded.nc',
                  'vu_bystate_stat_comb_ind_wood_ER_regridded.nc',
                  'vu_bystate_stat_comb_res_petr_ER_regridded.nc',
                  'vu_bystate_stat_comb_res_wood_ER_regridded.nc')

file_list_out <- c('NG_dist_MnR_com_bydomain_aces.nc',
                   'NG_dist_MnR_res_bydomain_aces.nc',
                   'NG_dist_mains_com_bydomain_aces.nc',
                   'NG_dist_mains_res_bydomain_aces.nc',
                   'NG_dist_meter_com_bydomain_aces.nc',
                   'NG_dist_meter_res_bydomain_aces.nc',
                   'NG_post_meter_res_bydomain_aces.nc',
                   'NG_dist_serv_com_bydomain_aces.nc',
                   'NG_dist_serv_res_bydomain_aces.nc',
                   'NG_dist_upset_com_bydomain_aces.nc',
                   'NG_dist_upset_res_bydomain_aces.nc',
                   'stat_comb_com_coal_bydomain_aces.nc',
                   'stat_comb_com_gas_bydomain_aces.nc',
                   'stat_comb_com_petr_bydomain_aces.nc',
                   'stat_comb_com_wood_bydomain_aces.nc',
                   'stat_comb_elec_coal_bydomain_aces.nc',
                   'stat_comb_elec_gas_bydomain_aces.nc',
                   'stat_comb_elec_petr_bydomain_aces.nc',
                   'stat_comb_elec_wood_bydomain_aces.nc',
                   'stat_comb_ind_coal_bydomain_aces.nc',
                   'stat_comb_ind_gas_bydomain_aces.nc',
                   'stat_comb_ind_petr_bydomain_aces.nc',
                   'stat_comb_ind_wood_bydomain_aces.nc',
                   'stat_comb_res_petr_bydomain_aces.nc',
                   'stat_comb_res_wood_bydomain_aces.nc',
                   'NG_dist_MnR_com_byLDC_aces.nc',
                   'NG_dist_MnR_res_byLDC_aces.nc',
                   'NG_dist_mains_com_byLDC_aces.nc',
                   'NG_dist_mains_res_byLDC_aces.nc',
                   'NG_dist_meter_com_byLDC_aces.nc',
                   'NG_dist_meter_res_byLDC_aces.nc',
                   'NG_post_meter_res_byLDC_aces.nc',
                   'NG_dist_serv_com_byLDC_aces.nc',
                   'NG_dist_serv_res_byLDC_aces.nc',
                   'NG_dist_upset_com_byLDC_aces.nc',
                   'NG_dist_upset_res_byLDC_aces.nc',
                   'NG_dist_MnR_com_bystate_aces.nc',
                   'NG_dist_MnR_res_bystate_aces.nc',
                   'NG_dist_mains_com_bystate_aces.nc',
                   'NG_dist_mains_res_bystate_aces.nc',
                   'NG_dist_meter_com_bystate_aces.nc',
                   'NG_dist_meter_res_bystate_aces.nc',
                   'NG_post_meter_res_bystate_aces.nc',
                   'NG_dist_serv_com_bystate_aces.nc',
                   'NG_dist_serv_res_bystate_aces.nc',
                   'NG_dist_upset_com_bystate_aces.nc',
                   'NG_dist_upset_res_bystate_aces.nc',
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
                   'NG_dist_MnR_com_bydomain_vulcan.nc',
                   'NG_dist_MnR_res_bydomain_vulcan.nc',
                   'NG_dist_mains_com_bydomain_vulcan.nc',
                   'NG_dist_mains_res_bydomain_vulcan.nc',
                   'NG_dist_meter_com_bydomain_vulcan.nc',
                   'NG_dist_meter_res_bydomain_vulcan.nc',
                   'NG_post_meter_res_bydomain_vulcan.nc',
                   'NG_dist_serv_com_bydomain_vulcan.nc',
                   'NG_dist_serv_res_bydomain_vulcan.nc',
                   'NG_dist_upset_com_bydomain_vulcan.nc',
                   'NG_dist_upset_res_bydomain_vulcan.nc',
                   'stat_comb_com_coal_bydomain_vulcan.nc',
                   'stat_comb_com_gas_bydomain_vulcan.nc',
                   'stat_comb_com_petr_bydomain_vulcan.nc',
                   'stat_comb_com_wood_bydomain_vulcan.nc',
                   'stat_comb_elec_coal_bydomain_vulcan.nc',
                   'stat_comb_elec_gas_bydomain_vulcan.nc',
                   'stat_comb_elec_petr_bydomain_vulcan.nc',
                   'stat_comb_elec_wood_bydomain_vulcan.nc',
                   'stat_comb_ind_coal_bydomain_vulcan.nc',
                   'stat_comb_ind_gas_bydomain_vulcan.nc',
                   'stat_comb_ind_petr_bydomain_vulcan.nc',
                   'stat_comb_ind_wood_bydomain_vulcan.nc',
                   'stat_comb_res_petr_bydomain_vulcan.nc',
                   'stat_comb_res_wood_bydomain_vulcan.nc',
                   'NG_dist_MnR_com_byLDC_vulcan.nc',
                   'NG_dist_MnR_res_byLDC_vulcan.nc',
                   'NG_dist_mains_com_byLDC_vulcan.nc',
                   'NG_dist_mains_res_byLDC_vulcan.nc',
                   'NG_dist_meter_com_byLDC_vulcan.nc',
                   'NG_dist_meter_res_byLDC_vulcan.nc',
                   'NG_post_meter_res_byLDC_vulcan.nc',
                   'NG_dist_serv_com_byLDC_vulcan.nc',
                   'NG_dist_serv_res_byLDC_vulcan.nc',
                   'NG_dist_upset_com_byLDC_vulcan.nc',
                   'NG_dist_upset_res_byLDC_vulcan.nc',
                   'NG_dist_MnR_com_bystate_vulcan.nc',
                   'NG_dist_MnR_res_bystate_vulcan.nc',
                   'NG_dist_mains_com_bystate_vulcan.nc',
                   'NG_dist_mains_res_bystate_vulcan.nc',
                   'NG_dist_meter_com_bystate_vulcan.nc',
                   'NG_dist_meter_res_bystate_vulcan.nc',
                   'NG_post_meter_res_bystate_vulcan.nc',
                   'NG_dist_serv_com_bystate_vulcan.nc',
                   'NG_dist_serv_res_bystate_vulcan.nc',
                   'NG_dist_upset_com_bystate_vulcan.nc',
                   'NG_dist_upset_res_bystate_vulcan.nc',
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

longname_out <- c('Methane emissions from natural gas distribution metering and regulating stations, spatially allocated from domain totals using ACES commercial CO2 emissions',
                  'Methane emissions from natural gas distribution metering and regulating stations, spatially allocated from domain totals using ACES residential CO2 emissions',
                  'Methane emissions from natural gas distribution mains pipelines, spatially allocated from domain totals using ACES commercial CO2 emissions',
                  'Methane emissions from natural gas distribution mains pipelines, spatially allocated from domain totals using ACES residential CO2 emissions',
                  'Methane emissions from natural gas consumer meters, spatially allocated from domain totals using ACES commercial CO2 emissions',
                  'Methane emissions from natural gas consumer meters, spatially allocated from domain totals using ACES residential CO2 emissions',
                  'Methane emissions from post-meter residential natural gas leakage and usage, spatially allocated from domain totals using ACES residential CO2 emissions',
                  'Methane emissions from natural gas distribution service pipelines, spatially allocated from domain totals using ACES commercial CO2 emissions',
                  'Methane emissions from natural gas distribution service pipelines, spatially allocated from domain totals using ACES residential CO2 emissions',
                  'Methane emissions from natural gas distribution upsets and maintenance, spatially allocated from domain totals using ACES commercial CO2 emissions',
                  'Methane emissions from natural gas distribution upsets and maintenance, spatially allocated from domain totals using ACES residential CO2 emissions',
                  'Methane emissions from commercial sector stationary combustion of coal, spatially allocated from domain totals using NEI CO emissions and ACES commercial CO2 emissions',
                  'Methane emissions from commercial sector stationary combustion of natural gas, spatially allocated from domain totals using NEI CO emissions and ACES commercial CO2 emissions',
                  'Methane emissions from commercial sector stationary combustion of petroleum products, spatially allocated from domain totals using NEI CO emissions and ACES commercial CO2 emissions',
                  'Methane emissions from commercial sector stationary combustion of wood, spatially allocated from domain totals using NEI CO emissions and ACES commercial CO2 emissions',
                  'Methane emissions from electricity production sector stationary combustion of coal, spatially allocated from domain totals using NEI CO emissions and ACES electricity production CO2 emissions',
                  'Methane emissions from electricity production sector stationary combustion of natural gas, spatially allocated from domain totals using NEI CO emissions and ACES electricity production CO2 emissions',
                  'Methane emissions from electricity production sector stationary combustion of petroleum products, spatially allocated from domain totals using NEI CO emissions and ACES electricity production CO2 emissions',
                  'Methane emissions from electricity production sector stationary combustion of wood, spatially allocated from domain totals using NEI CO emissions and ACES electricity production CO2 emissions',
                  'Methane emissions from industrial sector stationary combustion of coal, spatially allocated from domain totals using NEI CO emissions and ACES industrial CO2 emissions',
                  'Methane emissions from industrial sector stationary combustion of natural gas, spatially allocated from domain totals using NEI CO emissions and ACES industrial CO2 emissions',
                  'Methane emissions from industrial sector stationary combustion of petroleum products, spatially allocated from domain totals using NEI CO emissions and ACES industrial CO2 emissions',
                  'Methane emissions from industrial sector stationary combustion of wood, spatially allocated from domain totals using NEI CO emissions and ACES industrial CO2 emissions',
                  'Methane emissions from residential sector stationary combustion of petroleum products, spatially allocated from domain totals using NEI CO emissions and ACES residential CO2 emissions',
                  'Methane emissions from residential sector stationary combustion of wood, spatially allocated from domain totals using NEI CO emissions and ACES residential CO2 emissions',
                  'Methane emissions from natural gas distribution metering and regulating stations, spatially allocated from local distribution company totals using ACES commercial CO2 emissions',
                  'Methane emissions from natural gas distribution metering and regulating stations, spatially allocated from local distribution company totals using ACES residential CO2 emissions',
                  'Methane emissions from natural gas distribution mains pipelines, spatially allocated from local distribution company totals using ACES commercial CO2 emissions',
                  'Methane emissions from natural gas distribution mains pipelines, spatially allocated from local distribution company totals using ACES residential CO2 emissions',
                  'Methane emissions from natural gas consumer meters, spatially allocated from local distribution company totals using ACES commercial CO2 emissions',
                  'Methane emissions from natural gas consumer meters, spatially allocated from local distribution company totals using ACES residential CO2 emissions',
                  'Methane emissions from post-meter residential natural gas leakage and usage, spatially allocated from local distribution company totals using ACES residential CO2 emissions',
                  'Methane emissions from natural gas distribution service pipelines, spatially allocated from local distribution company totals using ACES commercial CO2 emissions',
                  'Methane emissions from natural gas distribution service pipelines, spatially allocated from local distribution company totals using ACES residential CO2 emissions',
                  'Methane emissions from natural gas distribution upsets and maintenance, spatially allocated from local distribution company totals using ACES commercial CO2 emissions',
                  'Methane emissions from natural gas distribution upsets and maintenance, spatially allocated from local distribution company totals using ACES residential CO2 emissions',
                  'Methane emissions from natural gas distribution metering and regulating stations, spatially allocated from individual-state totals using ACES commercial CO2 emissions',
                  'Methane emissions from natural gas distribution metering and regulating stations, spatially allocated from individual-state totals using ACES residential CO2 emissions',
                  'Methane emissions from natural gas distribution mains pipelines, spatially allocated from individual-state totals using ACES commercial CO2 emissions',
                  'Methane emissions from natural gas distribution mains pipelines, spatially allocated from individual-state totals using ACES residential CO2 emissions',
                  'Methane emissions from natural gas consumer meters, spatially allocated from individual-state totals using ACES commercial CO2 emissions',
                  'Methane emissions from natural gas consumer meters, spatially allocated from individual-state totals using ACES residential CO2 emissions',
                  'Methane emissions from post-meter residential natural gas leakage and usage, spatially allocated from individual-state totals using ACES residential CO2 emissions',
                  'Methane emissions from natural gas distribution service pipelines, spatially allocated from individual-state totals using ACES commercial CO2 emissions',
                  'Methane emissions from natural gas distribution service pipelines, spatially allocated from individual-state totals using ACES residential CO2 emissions',
                  'Methane emissions from natural gas distribution upsets and maintenance, spatially allocated from individual-state totals using ACES commercial CO2 emissions',
                  'Methane emissions from natural gas distribution upsets and maintenance, spatially allocated from individual-state totals using ACES residential CO2 emissions',
                  'Methane emissions from commercial sector stationary combustion of coal, spatially allocated from individual-state totals using NEI CO emissions and ACES commercial CO2 emissions',
                  'Methane emissions from commercial sector stationary combustion of natural gas, spatially allocated from individual-state totals using NEI CO emissions and ACES commercial CO2 emissions',
                  'Methane emissions from commercial sector stationary combustion of petroleum products, spatially allocated from individual-state totals using NEI CO emissions and ACES commercial CO2 emissions',
                  'Methane emissions from commercial sector stationary combustion of wood, spatially allocated from individual-state totals using NEI CO emissions and ACES commercial CO2 emissions',
                  'Methane emissions from electricity production sector stationary combustion of coal, spatially allocated from individual-state totals using NEI CO emissions and ACES electricity production CO2 emissions',
                  'Methane emissions from electricity production sector stationary combustion of natural gas, spatially allocated from individual-state totals using NEI CO emissions and ACES electricity production CO2 emissions',
                  'Methane emissions from electricity production sector stationary combustion of petroleum products, spatially allocated from individual-state totals using NEI CO emissions and ACES electricity production CO2 emissions',
                  'Methane emissions from electricity production sector stationary combustion of wood, spatially allocated from individual-state totals using NEI CO emissions and ACES electricity production CO2 emissions',
                  'Methane emissions from industrial sector stationary combustion of coal, spatially allocated from individual-state totals using NEI CO emissions and ACES industrial CO2 emissions',
                  'Methane emissions from industrial sector stationary combustion of natural gas, spatially allocated from individual-state totals using NEI CO emissions and ACES industrial CO2 emissions',
                  'Methane emissions from industrial sector stationary combustion of petroleum products, spatially allocated from individual-state totals using NEI CO emissions and ACES industrial CO2 emissions',
                  'Methane emissions from industrial sector stationary combustion of wood, spatially allocated from individual-state totals using NEI CO emissions and ACES industrial CO2 emissions',
                  'Methane emissions from residential sector stationary combustion of petroleum products, spatially allocated from individual-state totals using NEI CO emissions and ACES residential CO2 emissions',
                  'Methane emissions from residential sector stationary combustion of wood, spatially allocated from individual-state totals using NEI CO emissions and ACES residential CO2 emissions',
                  'Methane emissions from natural gas distribution metering and regulating stations, spatially allocated from domain totals using Vulcan commercial CO2 emissions',
                  'Methane emissions from natural gas distribution metering and regulating stations, spatially allocated from domain totals using Vulcan residential CO2 emissions',
                  'Methane emissions from natural gas distribution mains pipelines, spatially allocated from domain totals using Vulcan commercial CO2 emissions',
                  'Methane emissions from natural gas distribution mains pipelines, spatially allocated from domain totals using Vulcan residential CO2 emissions',
                  'Methane emissions from natural gas consumer meters, spatially allocated from domain totals using Vulcan commercial CO2 emissions',
                  'Methane emissions from natural gas consumer meters, spatially allocated from domain totals using Vulcan residential CO2 emissions',
                  'Methane emissions from post-meter residential natural gas leakage and usage, spatially allocated from domain totals using Vulcan residential CO2 emissions',
                  'Methane emissions from natural gas distribution service pipelines, spatially allocated from domain totals using Vulcan commercial CO2 emissions',
                  'Methane emissions from natural gas distribution service pipelines, spatially allocated from domain totals using Vulcan residential CO2 emissions',
                  'Methane emissions from natural gas distribution upsets and maintenance, spatially allocated from domain totals using Vulcan commercial CO2 emissions',
                  'Methane emissions from natural gas distribution upsets and maintenance, spatially allocated from domain totals using Vulcan residential CO2 emissions',
                  'Methane emissions from commercial sector stationary combustion of coal, spatially allocated from domain totals using NEI CO emissions and Vulcan commercial CO2 emissions',
                  'Methane emissions from commercial sector stationary combustion of natural gas, spatially allocated from domain totals using NEI CO emissions and Vulcan commercial CO2 emissions',
                  'Methane emissions from commercial sector stationary combustion of petroleum products, spatially allocated from domain totals using NEI CO emissions and Vulcan commercial CO2 emissions',
                  'Methane emissions from commercial sector stationary combustion of wood, spatially allocated from domain totals using NEI CO emissions and Vulcan commercial CO2 emissions',
                  'Methane emissions from electricity production sector stationary combustion of coal, spatially allocated from domain totals using NEI CO emissions and Vulcan electricity production CO2 emissions',
                  'Methane emissions from electricity production sector stationary combustion of natural gas, spatially allocated from domain totals using NEI CO emissions and Vulcan electricity production CO2 emissions',
                  'Methane emissions from electricity production sector stationary combustion of petroleum products, spatially allocated from domain totals using NEI CO emissions and Vulcan electricity production CO2 emissions',
                  'Methane emissions from electricity production sector stationary combustion of wood, spatially allocated from domain totals using NEI CO emissions and Vulcan electricity production CO2 emissions',
                  'Methane emissions from industrial sector stationary combustion of coal, spatially allocated from domain totals using NEI CO emissions and Vulcan industrial CO2 emissions',
                  'Methane emissions from industrial sector stationary combustion of natural gas, spatially allocated from domain totals using NEI CO emissions and Vulcan industrial CO2 emissions',
                  'Methane emissions from industrial sector stationary combustion of petroleum products, spatially allocated from domain totals using NEI CO emissions and Vulcan industrial CO2 emissions',
                  'Methane emissions from industrial sector stationary combustion of wood, spatially allocated from domain totals using NEI CO emissions and Vulcan industrial CO2 emissions',
                  'Methane emissions from residential sector stationary combustion of petroleum products, spatially allocated from domain totals using NEI CO emissions and Vulcan residential CO2 emissions',
                  'Methane emissions from residential sector stationary combustion of wood, spatially allocated from domain totals using NEI CO emissions and Vulcan residential CO2 emissions',
                  'Methane emissions from natural gas distribution metering and regulating stations, spatially allocated from local distribution company totals using Vulcan commercial CO2 emissions',
                  'Methane emissions from natural gas distribution metering and regulating stations, spatially allocated from local distribution company totals using Vulcan residential CO2 emissions',
                  'Methane emissions from natural gas distribution mains pipelines, spatially allocated from local distribution company totals using Vulcan commercial CO2 emissions',
                  'Methane emissions from natural gas distribution mains pipelines, spatially allocated from local distribution company totals using Vulcan residential CO2 emissions',
                  'Methane emissions from natural gas consumer meters, spatially allocated from local distribution company totals using Vulcan commercial CO2 emissions',
                  'Methane emissions from natural gas consumer meters, spatially allocated from local distribution company totals using Vulcan residential CO2 emissions',
                  'Methane emissions from post-meter residential natural gas leakage and usage, spatially allocated from local distribution company totals using Vulcan residential CO2 emissions',
                  'Methane emissions from natural gas distribution service pipelines, spatially allocated from local distribution company totals using Vulcan commercial CO2 emissions',
                  'Methane emissions from natural gas distribution service pipelines, spatially allocated from local distribution company totals using Vulcan residential CO2 emissions',
                  'Methane emissions from natural gas distribution upsets and maintenance, spatially allocated from local distribution company totals using Vulcan commercial CO2 emissions',
                  'Methane emissions from natural gas distribution upsets and maintenance, spatially allocated from local distribution company totals using Vulcan residential CO2 emissions',
                  'Methane emissions from natural gas distribution metering and regulating stations, spatially allocated from individual-state totals using Vulcan commercial CO2 emissions',
                  'Methane emissions from natural gas distribution metering and regulating stations, spatially allocated from individual-state totals using Vulcan residential CO2 emissions',
                  'Methane emissions from natural gas distribution mains pipelines, spatially allocated from individual-state totals using Vulcan commercial CO2 emissions',
                  'Methane emissions from natural gas distribution mains pipelines, spatially allocated from individual-state totals using Vulcan residential CO2 emissions',
                  'Methane emissions from natural gas consumer meters, spatially allocated from individual-state totals using Vulcan commercial CO2 emissions',
                  'Methane emissions from natural gas consumer meters, spatially allocated from individual-state totals using Vulcan residential CO2 emissions',
                  'Methane emissions from post-meter residential natural gas leakage and usage, spatially allocated from individual-state totals using Vulcan residential CO2 emissions',
                  'Methane emissions from natural gas distribution service pipelines, spatially allocated from individual-state totals using Vulcan commercial CO2 emissions',
                  'Methane emissions from natural gas distribution service pipelines, spatially allocated from individual-state totals using Vulcan residential CO2 emissions',
                  'Methane emissions from natural gas distribution upsets and maintenance, spatially allocated from individual-state totals using Vulcan commercial CO2 emissions',
                  'Methane emissions from natural gas distribution upsets and maintenance, spatially allocated from individual-state totals using Vulcan residential CO2 emissions',
                  'Methane emissions from commercial sector stationary combustion of coal, spatially allocated from individual-state totals using NEI CO emissions and Vulcan commercial CO2 emissions',
                  'Methane emissions from commercial sector stationary combustion of natural gas, spatially allocated from individual-state totals using NEI CO emissions and Vulcan commercial CO2 emissions',
                  'Methane emissions from commercial sector stationary combustion of petroleum products, spatially allocated from individual-state totals using NEI CO emissions and Vulcan commercial CO2 emissions',
                  'Methane emissions from commercial sector stationary combustion of wood, spatially allocated from individual-state totals using NEI CO emissions and Vulcan commercial CO2 emissions',
                  'Methane emissions from electricity production sector stationary combustion of coal, spatially allocated from individual-state totals using NEI CO emissions and Vulcan electricity production CO2 emissions',
                  'Methane emissions from electricity production sector stationary combustion of natural gas, spatially allocated from individual-state totals using NEI CO emissions and Vulcan electricity production CO2 emissions',
                  'Methane emissions from electricity production sector stationary combustion of petroleum products, spatially allocated from individual-state totals using NEI CO emissions and Vulcan electricity production CO2 emissions',
                  'Methane emissions from electricity production sector stationary combustion of wood, spatially allocated from individual-state totals using NEI CO emissions and Vulcan electricity production CO2 emissions',
                  'Methane emissions from industrial sector stationary combustion of coal, spatially allocated from individual-state totals using NEI CO emissions and Vulcan industrial CO2 emissions',
                  'Methane emissions from industrial sector stationary combustion of natural gas, spatially allocated from individual-state totals using NEI CO emissions and Vulcan industrial CO2 emissions',
                  'Methane emissions from industrial sector stationary combustion of petroleum products, spatially allocated from individual-state totals using NEI CO emissions and Vulcan industrial CO2 emissions',
                  'Methane emissions from industrial sector stationary combustion of wood, spatially allocated from individual-state totals using NEI CO emissions and Vulcan industrial CO2 emissions',
                  'Methane emissions from residential sector stationary combustion of petroleum products, spatially allocated from individual-state totals using NEI CO emissions and Vulcan residential CO2 emissions',
                  'Methane emissions from residential sector stationary combustion of wood, spatially allocated from individual-state totals using NEI CO emissions and Vulcan residential CO2 emissions')

if(exists("subset_files")){
  file_list_in <- file_list_in[subset_files]
  file_list_out <- file_list_out[subset_files]
  longname_out <- longname_out[subset_files]
}


################################################################################
#load up some functions/datasets and plot up this output nicely

source(plotting_function)

plot_title <- c("NG Distribution - metering and regulating stations\n domain data distributed using\nACES commercial CO2",
                "NG Distribution - metering and regulating stations\n domain data distributed using\nACES residential CO2",
                "NG Distribution - Mains\n domain data distributed using\nACES commercial CO2",
                "NG Distribution - Mains\n domain data distributed using\nACES residential CO2",
                "NG Distribution - Meters\n domain data distributed using\nACES commercial CO2",
                "NG Distribution - Meters\n domain data distributed using\nACES residential CO2",
                "NG Distribution - post meter\n domain data distributed using\nACES residential CO2",
                "NG Distribution - service pipelines\n domain data distributed using\nACES commercial CO2",
                "NG Distribution - service pipelines\n domain data distributed using\nACES residential CO2",
                "NG Distribution - upsets\n domain data distributed using\nVulcan commercial CO2",
                "NG Distribution - upsets\n domain data distributed using\nVulcan residential CO2",
                "Stationary Combustion Commercial - Coal\n domain totals distributed using NEI CO emissions\n and ACES commercial CO2 emissions",
                "Stationary Combustion Commercial - Gas\n domain totals distributed using NEI CO emissions\n and ACES commercial CO2 emissions",
                "Stationary Combustion Commercial - Petroleum\n domain totals distributed using NEI CO emissions\n and ACES commercial CO2 emissions",
                "Stationary Combustion Commercial - Wood\n domain totals distributed using NEI CO emissions\n and ACES commercial CO2 emissions",
                "Stationary Combustion Electricity - Coal\n domain totals distributed using NEI CO emissions\n and ACES electricity CO2 emissions",
                "Stationary Combustion Electricity - Gas\n domain totals distributed using NEI CO emissions\n and ACES electricity CO2 emissions",
                "Stationary Combustion Electricity - Petroleum\n domain totals distributed using NEI CO emissions\n and ACES electricity CO2 emissions",
                "Stationary Combustion Electricity - Wood\n domain totals distributed using NEI CO emissions\n and ACES electricity CO2 emissions",
                "Stationary Combustion Industrial - Coal\n domain totals distributed using NEI CO emissions\n and ACES electricity CO2 emissions",
                "Stationary Combustion Industrial - Gas\n domain totals distributed using NEI CO emissions\n and ACES electricity CO2 emissions",
                "Stationary Combustion Industrial - Petroleum\n domain totals distributed using NEI CO emissions\n and ACES electricity CO2 emissions",
                "Stationary Combustion Industrial - Wood\n domain totals distributed using NEI CO emissions\n and ACES electricity CO2 emissions",
                "Stationary Combustion Residential - Petroleum\n domain totals distributed using NEI CO emissions\n and ACES electricity CO2 emissions",
                "Stationary Combustion Residential - Wood\n domain totals distributed using NEI CO emissions\n and ACES electricity CO2 emissions",
                "NG Distribution - metering and regulating stations\n local distribution data distributed using\nACES commercial CO2",
                "NG Distribution - metering and regulating stations\n local distribution data distributed using\nACES residential CO2",
                "NG Distribution - Mains\n local distribution data distributed using\nACES commercial CO2",
                "NG Distribution - Mains\n local distribution data distributed using\nACES residential CO2",
                "NG Distribution - Meters\n local distribution data distributed using\nACES commercial CO2",
                "NG Distribution - Meters\n local distribution data distributed using\nACES residential CO2",
                "NG Distribution - post meter\n local distribution data distributed using\nACES residential CO2",
                "NG Distribution - service pipelines\n local distribution data distributed using\nACES commercial CO2",
                "NG Distribution - service pipelines\n local distribution data distributed using\nACES residential CO2",
                "NG Distribution - upsets\n local distribution data distributed using\nACES commercial CO2",
                "NG Distribution - upsets\n local distribution data distributed using\nACES residential CO2",
                "NG Distribution - metering and regulating stations\n state data distributed using\nACES commercial CO2",
                "NG Distribution - metering and regulating stations\n state data distributed using\nACES residential CO2",
                "NG Distribution - Mains\n state data distributed using\nACES commercial CO2",
                "NG Distribution - Mains\n state data distributed using\nACES residential CO2",
                "NG Distribution - Meters\n state data distributed using\nACES commercial CO2",
                "NG Distribution - Meters\n state data distributed using\nACES residential CO2",
                "NG Distribution - post meter\n state data distributed using\nACES residential CO2",
                "NG Distribution - service pipelines\n state data distributed using\nACES commercial CO2",
                "NG Distribution - service pipelines\n state data distributed using\nACES residential CO2",
                "NG Distribution - upsets\n state data distributed using\nVulcan commercial CO2",
                "NG Distribution - upsets\n state data distributed using\nVulcan residential CO2",
                "Stationary Combustion Commercial - Coal\n state totals distributed using NEI CO emissions\n and ACES commercial CO2 emissions",
                "Stationary Combustion Commercial - Gas\n state totals distributed using NEI CO emissions\n and ACES commercial CO2 emissions",
                "Stationary Combustion Commercial - Petroleum\n state totals distributed using NEI CO emissions\n and ACES commercial CO2 emissions",
                "Stationary Combustion Commercial - Wood\n state totals distributed using NEI CO emissions\n and ACES commercial CO2 emissions",
                "Stationary Combustion Electricity - Coal\n state totals distributed using NEI CO emissions\n and ACES electricity CO2 emissions",
                "Stationary Combustion Electricity - Gas\n state totals distributed using NEI CO emissions\n and ACES electricity CO2 emissions",
                "Stationary Combustion Electricity - Petroleum\n state totals distributed using NEI CO emissions\n and ACES electricity CO2 emissions",
                "Stationary Combustion Electricity - Wood\n state totals distributed using NEI CO emissions\n and ACES electricity CO2 emissions",
                "Stationary Combustion Industrial - Coal\n state totals distributed using NEI CO emissions\n and ACES electricity CO2 emissions",
                "Stationary Combustion Industrial - Gas\n state totals distributed using NEI CO emissions\n and ACES electricity CO2 emissions",
                "Stationary Combustion Industrial - Petroleum\n state totals distributed using NEI CO emissions\n and ACES electricity CO2 emissions",
                "Stationary Combustion Industrial - Wood\n state totals distributed using NEI CO emissions\n and ACES electricity CO2 emissions",
                "Stationary Combustion Residential - Petroleum\n state totals distributed using NEI CO emissions\n and ACES electricity CO2 emissions",
                "Stationary Combustion Residential - Wood\n state totals distributed using NEI CO emissions\n and ACES electricity CO2 emissions", 
                "NG Distribution - metering and regulating stations\n domain data distributed using\nVulcan commercial CO2",
                "NG Distribution - metering and regulating stations\n domain data distributed using\nVulcan residential CO2",
                "NG Distribution - Mains\n domain data distributed using\nVulcan commercial CO2",
                "NG Distribution - Mains\n domain data distributed using\nVulcan residential CO2",
                "NG Distribution - Meters\n domain data distributed using\nVulcan commercial CO2",
                "NG Distribution - Meters\n domain data distributed using\nVulcan residential CO2",
                "NG Distribution - post meter\n domain data distributed using\nVulcan residential CO2",
                "NG Distribution - service pipelines\n domain data distributed using\nVulcan commercial CO2",
                "NG Distribution - service pipelines\n domain data distributed using\nVulcan residential CO2",
                "NG Distribution - upsets\n domain data distributed using\nVulcan commercial CO2",
                "NG Distribution - upsets\n domain data distributed using\nVulcan residential CO2",
                "Stationary Combustion Commercial - Coal\n domain totals distributed using NEI CO emissions\n and Vulcan commercial CO2 emissions",
                "Stationary Combustion Commercial - Gas\n domain totals distributed using NEI CO emissions\n and Vulcan commercial CO2 emissions",
                "Stationary Combustion Commercial - Petroleum\n domain totals distributed using NEI CO emissions\n and Vulcan commercial CO2 emissions",
                "Stationary Combustion Commercial - Wood\n domain totals distributed using NEI CO emissions\n and Vulcan commercial CO2 emissions",
                "Stationary Combustion Electricity - Coal\n domain totals distributed using NEI CO emissions\n and Vulcan electricity CO2 emissions",
                "Stationary Combustion Electricity - Gas\n domain totals distributed using NEI CO emissions\n and Vulcan electricity CO2 emissions",
                "Stationary Combustion Electricity - Petroleum\n domain totals distributed using NEI CO emissions\n and Vulcan electricity CO2 emissions",
                "Stationary Combustion Electricity - Wood\n domain totals distributed using NEI CO emissions\n and Vulcan electricity CO2 emissions",
                "Stationary Combustion Industrial - Coal\n domain totals distributed using NEI CO emissions\n and Vulcan electricity CO2 emissions",
                "Stationary Combustion Industrial - Gas\n domain totals distributed using NEI CO emissions\n and Vulcan electricity CO2 emissions",
                "Stationary Combustion Industrial - Petroleum\n domain totals distributed using NEI CO emissions\n and Vulcan electricity CO2 emissions",
                "Stationary Combustion Industrial - Wood\n domain totals distributed using NEI CO emissions\n and Vulcan electricity CO2 emissions",
                "Stationary Combustion Residential - Petroleum\n domain totals distributed using NEI CO emissions\n and Vulcan electricity CO2 emissions",
                "Stationary Combustion Residential - Wood\n domain totals distributed using NEI CO emissions\n and Vulcan electricity CO2 emissions",
                "NG Distribution - metering and regulating stations\n local distribution data distributed using\nVulcan commercial CO2",
                "NG Distribution - metering and regulating stations\n local distribution data distributed using\nVulcan residential CO2",
                "NG Distribution - Mains\n local distribution data distributed using\nVulcan commercial CO2",
                "NG Distribution - Mains\n local distribution data distributed using\nVulcan residential CO2",
                "NG Distribution - Meters\n local distribution data distributed using\nVulcan commercial CO2",
                "NG Distribution - Meters\n local distribution data distributed using\nVulcan residential CO2",
                "NG Distribution - post meter\n local distribution data distributed using\nVulcan residential CO2",
                "NG Distribution - service pipelines\n local distribution data distributed using\nVulcan commercial CO2",
                "NG Distribution - service pipelines\n local distribution data distributed using\nVulcan residential CO2",
                "NG Distribution - upsets\n local distribution data distributed using\nVulcan commercial CO2",
                "NG Distribution - upsets\n local distribution data distributed using\nVulcan residential CO2",
                "NG Distribution - metering and regulating stations\n state data distributed using\nVulcan commercial CO2",
                "NG Distribution - metering and regulating stations\n state data distributed using\nVulcan residential CO2",
                "NG Distribution - Mains\n state data distributed using\nVulcan commercial CO2",
                "NG Distribution - Mains\n state data distributed using\nVulcan residential CO2",
                "NG Distribution - Meters\n state data distributed using\nVulcan commercial CO2",
                "NG Distribution - Meters\n state data distributed using\nVulcan residential CO2",
                "NG Distribution - post meter\n state data distributed using\nVulcan residential CO2",
                "NG Distribution - service pipelines\n state data distributed using\nVulcan commercial CO2",
                "NG Distribution - service pipelines\n state data distributed using\nVulcan residential CO2",
                "NG Distribution - upsets\n state data distributed using\nVulcan commercial CO2",
                "NG Distribution - upsets\n state data distributed using\nVulcan residential CO2",
                "Stationary Combustion Commercial - Coal\n state totals distributed using NEI CO emissions\n and Vulcan commercial CO2 emissions",
                "Stationary Combustion Commercial - Gas\n state totals distributed using NEI CO emissions\n and Vulcan commercial CO2 emissions",
                "Stationary Combustion Commercial - Petroleum\n state totals distributed using NEI CO emissions\n and Vulcan commercial CO2 emissions",
                "Stationary Combustion Commercial - Wood\n state totals distributed using NEI CO emissions\n and Vulcan commercial CO2 emissions",
                "Stationary Combustion Electricity - Coal\n state totals distributed using NEI CO emissions\n and Vulcan electricity CO2 emissions",
                "Stationary Combustion Electricity - Gas\n state totals distributed using NEI CO emissions\n and Vulcan electricity CO2 emissions",
                "Stationary Combustion Electricity - Petroleum\n state totals distributed using NEI CO emissions\n and Vulcan electricity CO2 emissions",
                "Stationary Combustion Electricity - Wood\n state totals distributed using NEI CO emissions\n and Vulcan electricity CO2 emissions",
                "Stationary Combustion Industrial - Coal\n state totals distributed using NEI CO emissions\n and Vulcan electricity CO2 emissions",
                "Stationary Combustion Industrial - Gas\n state totals distributed using NEI CO emissions\n and Vulcan electricity CO2 emissions",
                "Stationary Combustion Industrial - Petroleum\n state totals distributed using NEI CO emissions\n and Vulcan electricity CO2 emissions",
                "Stationary Combustion Industrial - Wood\n state totals distributed using NEI CO emissions\n and Vulcan electricity CO2 emissions",
                "Stationary Combustion Residential - Petroleum\n state totals distributed using NEI CO emissions\n and Vulcan electricity CO2 emissions",
                "Stationary Combustion Residential - Wood\n state totals distributed using NEI CO emissions\n and Vulcan electricity CO2 emissions")

if(exists("subset_files")){
  plot_title <- plot_title[subset_files]
}

################################################################################
#do the work and plot

for(i in 1:length(file_list_in)){
  emiss_mol_per_km2_per_s <- raster(file.path(filepath_in, file_list_in[i]))
  emiss_nmol_per_m2_per_s <- emiss_mol_per_km2_per_s*1000
  writeRaster(emiss_nmol_per_m2_per_s,
              paste0(filepath_out, file_list_out[i]),
              force_v4=TRUE,
              varname='methane_emissions',
              varunit='nmol/m2/s',
              longname=longname_out[i],
              NAflag=-9999,
              overwrite=TRUE)
  cat("\rFinished writing",i,"of",length(file_list_in),"           ")
}

plot_separation <- apply(expand.grid(c("com","elec","ind","res"),c("coal","gas","petr","wood")),1,
                         FUN=function(x){grep(paste0("stat_comb_",x[1],"_",x[2]),file_list_in)})
plot_separation <- plot_separation[-c(4,8)]
#indices for the various groups that should be on the same scale.  The ACES and
#Vulcan version and the bydomain and bystate version of each plot should be the
#same scale for easier comparisons.  There are no res_coal and res_gas here.

log_indices <- grep(file_list_in,pattern="elec")
for(i in 1:length(plot_separation)){
  plot_indx <- plot_separation[[i]]
  emiss_nmol_per_m2_per_s <- stack(paste0(filepath_out, file_list_out[plot_indx]))
  
  if(sum(plot_indx %in% log_indices)){
    plot_max <- max(maxValue(prep_data(emiss_nmol_per_m2_per_s)))
    plot_min <- min(minValue(prep_data(emiss_nmol_per_m2_per_s)))
    for(A in 1:length(plot_indx)){
      log_plot(emiss_nmol_per_m2_per_s[[A]],filename=file_list_out[plot_indx[A]],
               plot_title[plot_indx[A]],zlim_min = plot_min,zlim_max = plot_max)
    }
  }else{
    plot_max <- max(maxValue(emiss_nmol_per_m2_per_s))
    plot_min <- min(minValue(emiss_nmol_per_m2_per_s))
    for(A in 1:length(plot_indx)){
      not_log_plot(emiss_nmol_per_m2_per_s[[A]],filename=file_list_out[plot_indx[A]],
                 plot_title[plot_indx[A]],zlim_min = plot_min,zlim_max = plot_max)
    }
  }
  cat("\rFinished plotting",length(plot_indx)*i,"of",length(plot_indx)*length(plot_separation),"           ")
}



dir.create("Summed_Sectors",showWarnings = F)
setwd("Summed_Sectors")

#use regex to ID only those for each inventory-division combination and separate
#wood
Summed_stationary_combustion_FF_ACES_bydomain <- stack(paste0(filepath_out,file_list_out[grep("aces_bydomain_stat_comb_[[:alnum:]]+_[coal|gas|petr]",file_list_in)]))
Summed_stationary_combustion_wood_ACES_bydomain <- stack(paste0(filepath_out,file_list_out[grep("aces_bydomain_stat_comb_[[:alnum:]]+_wood",file_list_in)]))

Summed_stationary_combustion_FF_ACES_bydomain <- sum(Summed_stationary_combustion_FF_ACES_bydomain)
Summed_stationary_combustion_wood_ACES_bydomain <- sum(Summed_stationary_combustion_wood_ACES_bydomain)

log_plot(Summed_stationary_combustion_FF_ACES_bydomain,
         "Stationary Combustion FF Sector\nSEDS domain data scaled to match GHGI national data distributed to the county\nlevel via NEI, then distributed using ACES  coal + gas + petroleum")
log_plot(Summed_stationary_combustion_wood_ACES_bydomain,
         "Stationary Combustion Wood Sector\nSEDS domain data scaled to match GHGI national data distributed to the county\nlevel via NEI, then distributed using ACES  wood")

#repeat for aces-bystate
Summed_stationary_combustion_FF_ACES_bystate <- stack(paste0(filepath_out,file_list_out[grep("aces_bystate_stat_comb_[[:alnum:]]+_[coal|gas|petr]",file_list_in)]))
Summed_stationary_combustion_wood_ACES_bystate <- stack(paste0(filepath_out,file_list_out[grep("aces_bystate_stat_comb_[[:alnum:]]+_wood",file_list_in)]))
Summed_stationary_combustion_FF_ACES_bystate <- sum(Summed_stationary_combustion_FF_ACES_bystate)
Summed_stationary_combustion_wood_ACES_bystate <- sum(Summed_stationary_combustion_wood_ACES_bystate)
log_plot(Summed_stationary_combustion_FF_ACES_bystate,
         "Stationary Combustion FF Sector\nSEDS state data scaled to match GHGI national data distributed to the county\nlevel via NEI, then distributed using ACES  coal + gas + petroleum")
log_plot(Summed_stationary_combustion_wood_ACES_bystate,
         "Stationary Combustion Wood Sector\nSEDS state data scaled to match GHGI national data distributed to the county\nlevel via NEI, then distributed using ACES  wood")

#repeat for vu-bydomain
Summed_stationary_combustion_FF_Vulcan_bydomain <- stack(paste0(filepath_out,file_list_out[grep("vu_bydomain_stat_comb_[[:alnum:]]+_[coal|gas|petr]",file_list_in)]))
Summed_stationary_combustion_wood_Vulcan_bydomain <- stack(paste0(filepath_out,file_list_out[grep("vu_bydomain_stat_comb_[[:alnum:]]+_wood",file_list_in)]))
Summed_stationary_combustion_FF_Vulcan_bydomain <- sum(Summed_stationary_combustion_FF_Vulcan_bydomain)
Summed_stationary_combustion_wood_Vulcan_bydomain <- sum(Summed_stationary_combustion_wood_Vulcan_bydomain)
log_plot(Summed_stationary_combustion_FF_Vulcan_bydomain,
         "Stationary Combustion FF Sector\nSEDS domain data scaled to match GHGI national data distributed to the county\nlevel via NEI, then distributed using Vulcan  coal + gas + petroleum")
log_plot(Summed_stationary_combustion_wood_Vulcan_bydomain,
         "Stationary Combustion Wood Sector\nSEDS domain data scaled to match GHGI national data distributed to the county\nlevel via NEI, then distributed using Vulcan  wood")

#repeat for vu-bystate
Summed_stationary_combustion_FF_Vulcan_bystate <- stack(paste0(filepath_out,file_list_out[grep("vu_bystate_stat_comb_[[:alnum:]]+_[coal|gas|petr]",file_list_in)]))
Summed_stationary_combustion_wood_Vulcan_bystate <- stack(paste0(filepath_out,file_list_out[grep("vu_bystate_stat_comb_[[:alnum:]]+_wood",file_list_in)]))
Summed_stationary_combustion_FF_Vulcan_bystate <- sum(Summed_stationary_combustion_FF_Vulcan_bystate)
Summed_stationary_combustion_wood_Vulcan_bystate <- sum(Summed_stationary_combustion_wood_Vulcan_bystate)
log_plot(Summed_stationary_combustion_FF_Vulcan_bystate,
         "Stationary Combustion FF Sector\nSEDS state data scaled to match GHGI national data distributed to the county\nlevel via NEI, then distributed using Vulcan  coal + gas + petroleum")
log_plot(Summed_stationary_combustion_wood_Vulcan_bystate,
         "Stationary Combustion Wood Sector\nSEDS state data scaled to match GHGI national data distributed to the county\nlevel via NEI, then distributed using Vulcan  wood")


