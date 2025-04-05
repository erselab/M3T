#simply run the main function based on current config setup.  Requires updating to point to files on local system.  Update the config first.  Recommend viewing main help file first to understand parameters.

source("~/../../Kristian/Desktop/methane_inventory/src/CH4_Inventory_Main.R")
CH4_inventory_build(input_directory="G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw_data_12_2024/",
                    code_directory="~/../../Kristian/Desktop/methane_inventory/src/",
                    output_directory="G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Processed_12_2024/",
                    plot_directory="G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Figures_12_2024/",
                    inventory_year=2019,
                    ACES_directory="G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Inventories/ACES_v2.0",
                    vulcan_directory="G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Inventories/Vulcan_v3.0",
                    verbose=T,
                    domain_res = 0.01,
                    domain = as.data.frame(cbind(c(-75.7,-72.1),
                                                 c(39.2,42))),
                    domain_crs = "+proj=longlat",
                    GHGI_file = "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw_data_CONUS/2024_ghgi_natural_gas_systems_annex36_tables.xlsx",
                    NALCMS_file = "G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Shapefiles/NALCMS_2020_land_cover/NA_NALCMS_landcover_2020_30m/data/NA_NALCMS_landcover_2020_30m.tif",
                    NLCD_file = "G:/My Drive/Shepson Group Drive/General Inventories and Shapefiles/Shapefiles/nlcd_2019_land_cover_l48_20210604/nlcd_2019_land_cover_l48_20210604.img",
                    DMR_file = "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw_data_CONUS/DMR_2019_from_11_1_2024.csv",
                    CWNS_file = "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw_data_rewrite/CWNS_merged_data_2012_KH.xlsx",
                    EIA_file = "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw_data_12_2024/EIA/EIA_company_report_2019.xlsx",
                    HIFLD_compressor_file="G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw_data_CONUS/Natural_Gas_Compressor_Stations.csv",
                    watershed_shapefile="G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Raw_data_CONUS/watersheds_shapefile/watershed_p_v2.shp")
