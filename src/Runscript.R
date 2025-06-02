#simply run the main function based on current config setup.  Requires updating
#to point to files on local system.  Update the config first.  Recommend viewing
#main help file first to understand parameters.

source("~/../../Kristian/Desktop/methane_inventory/src/CH4_Inventory_Main.R")
CH4_inventory_build(input_directory="~/../../Kristian/Desktop/testrun/in/",
                    code_directory="~/../../Kristian/Desktop/methane_inventory/src/",
                    output_directory="~/../../Kristian/Desktop/testrun/out/",
                    plot_directory="~/../../Kristian/Desktop/testrun/plots/",
                    inventory_year=2019,
                    verbose=T,
                    domain = "DE",
                    # domain = as.data.frame(cbind(c(-75.7,-72.1),
                    #                              c(39.2,42))),
                    domain_res = 0.01,
                    domain_crs = "+proj=longlat",
                    NALCMS_file = "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Large inventory files/NA_NALCMS_landcover_2020_30m/data/NA_NALCMS_landcover_2020_30m.tif",
                    NLCD_file = "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Large inventory files/nlcd_2019_land_cover_l48_20210604/nlcd_2019_land_cover_l48_20210604.img",
                    ACES_directory="G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Large inventory files/ACES V2.0",
                    vulcan_directory="G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Large inventory files/Vulcan_v3.0",
                    DMR_file = "~/../../Kristian/Desktop/methane_inventory/src/Data/DMR_2019_from_11_1_2024.csv",
                    CWNS_file = "~/../../Kristian/Desktop/methane_inventory/src/Data/2022CWNS_NATIONAL_APR2024/",
                    EIA_file = "~/../../Kristian/Desktop/methane_inventory/src/Data/EIA_company_report_2019.xlsx",
                    PHMSA_file = "~/../../Kristian/Desktop/methane_inventory/src/Data/PHMSA_annual_gas_distribution_2010_present/annual_gas_distribution_2019.xlsx",
                    HIFLD_compressor_file="~/../../Kristian/Desktop/methane_inventory/src/Data/Natural_Gas_Compressor_Stations.csv",
                    watershed_shapefile="~/../../Kristian/Desktop/methane_inventory/src/Data/watersheds_shapefile/watershed_p_v2.shp",
                    Wetcharts_file = "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Large inventory files/WetCHARTs_v1_3_3_2019.nc")

