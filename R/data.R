#' State population data from the U.S. Census Bureau
#'
#' A subset of data from the Census Bureau
#'
#' The estimated state level population published by the U.S. Census Bureau
#'
#' @format `Census_state_population_M3T` A data frame with 49 rows and 16 columns:
#' \describe{
#'   \item{Name}{State name}
#'   \item{POPESTIMATE2010}{Estimated population for the year 2010}
#'   \item{POPESTIMATE2011}{Estimated population for the year 2011}
#'   \item{POPESTIMATE2012}{Estimated population for the year 2012}
#'   \item{POPESTIMATE2013}{Estimated population for the year 2013}
#'   \item{POPESTIMATE2014}{Estimated population for the year 2014}
#'   \item{POPESTIMATE2015}{Estimated population for the year 2015}
#'   \item{POPESTIMATE2016}{Estimated population for the year 2016}
#'   \item{POPESTIMATE2017}{Estimated population for the year 2017}
#'   \item{POPESTIMATE2018}{Estimated population for the year 2018}
#'   \item{POPESTIMATE2019}{Estimated population for the year 2019}
#'   \item{POPESTIMATE2020}{Estimated population for the year 2020}
#'   \item{POPESTIMATE2021}{Estimated population for the year 2021}
#'   \item{POPESTIMATE2022}{Estimated population for the year 2022}
#'   \item{POPESTIMATE2023}{Estimated population for the year 2023}
#'   \item{POPESTIMATE2024}{Estimated population for the year 2024}
#' }
#' @source
#'   <https://www.census.gov/data/tables/time-series/demo/popest/2020s-state-total.html>
'Census_state_population_M3T'






#' 2012 Clean Watershed Needs Survey (CWNS) data on waste water treatment plants
#'
#' A subset of data from the 2012 CWNS
#'
#' The 2012 CWNS is available as an access database.  The relevant data was
#' pulled and the file was converted to a csv file as described in detail in
#' \code{\link{M3T_config}}.
#'
#' @format `CWNS_2012` A data frame with 13,360 rows and 4 columns:
#' \describe{
#'   \item{LATITUDE}{Facility location coordinates}
#'   \item{LONGITUDE}{Facility location coordinates}
#'   \item{EXIST_MUNICIPAL}{Annual flow of municipal wastewater through a facility in millions of gallons per day}
#'   \item{HORIZONTAL_COORDINATE_DATUM}{Information regarding the mapping used in the coordinates}
#' }
#' @source
#'   <https://www.epa.gov/cwns/clean-watersheds-needs-survey-cwns-2012-report-and-data>
'CWNS_2012'






#' 2022 Clean Watershed Needs Survey (CWNS) data on waste water treatment plants
#'
#' A subset of data from the 2022 CWNS
#'
#' The 2022 CWNS is available as several CSVs by going to the data dashboard and
#' selecting data download in the top right.  The relevant data was pulled from
#' the various files, combined, and saved to a single CSV.
#'
#' @format `CWNS_2022` A data frame with 13,270 rows and 3 columns:
#' \describe{
#'   \item{LATITUDE}{Facility location coordinates}
#'   \item{LONGITUDE}{Facility location coordinates}
#'   \item{EXIST_MUNICIPAL}{Annual flow of municipal wastewater through a facility in millions of gallons per day}
#' }
#' @source
#'   <https://www.epa.gov/cwns/clean-watersheds-needs-survey-cwns-2022-report-and-data>
'CWNS_2022'






#' Data from EIA Form 176 - the Annual Report of Natural and supplemental Gas
#' Supply and Disposition
#'
#' A subset of data from EIA Form 176
#'
#' The volume of natural gas delivered and number of customers for natural gas
#' utilities, separated by customer type.  The data can be downloaded as a csv
#'
#' @format `EIA_NG_data` A data frame with 28,553 rows and 10 columns:
#' \describe{
#'   \item{Year}{Year of data}
#'   \item{State}{State the utility operates in}
#'   \item{Residential_Total_Volume_(Mcf)}{Volume of natural gas delivered to residential customers in thousand cubic feet}
#'   \item{Residential_Total_Customers}{Number of residential customers}
#'   \item{Commercial_Total_Volume_(Mcf)}{Volume of natural gas delivered to commercial customers in thousand cubic feet}
#'   \item{Commercial_Total_Customers}{Number of commercial customers}
#'   \item{Industrial_Total_Volume_(Mcf)}{Volume of natural gas delivered to industrial customers in thousand cubic feet}
#'   \item{Industrial_Total_Customers}{Number of industrial customers}
#'   \item{Electric_Total_Volume_(Mcf)}{Volume of natural gas delivered to electric production customers in thousand cubic feet}
#'   \item{Electric_Total_Customers}{Number of electric production customers}
#' }
#' @source
#' <https://www.eia.gov/naturalgas/ngqs/#?report=RP4&year1=2020&year2=2020&company=Name>
'EIA_NG_data'






#' Data from the EIA State Energy Data System (SEDS) for all continental US
#' states and all relevant years available at the time
#'
#' A subset of data from the EIA SEDS
#'
#' The amount of fuel consumed separated by fuel, state, year, and economic
#' sector
#'
#' @format `EIA_SEDS` A data frame with 8,918 rows and 5 columns:
#' \describe{
#'   \item{period}{Year of data}
#'   \item{seriesId}{ID code of the fuel and sector}
#'   \item{seriesDescription}{description of the seriesId}
#'   \item{stateId}{State abbreviation for the data}
#'   \item{value}{Consumed fuel for the corresponding state, year, fuel, and sector in billions of BTU}
#' }
#' @source
#' <https://www.eia.gov/naturalgas/ngqs/#?report=RP4&year1=2020&year2=2020&company=Name>
'EIA_SEDS'






#' Data from the Environmental Protection Agency's (EPA) Greenhouse Gas
#' Inventory (GHGI)
#'
#' A subset of data from the GHGI
#'
#' The national methane emissions estimated for municipal landfills in the GHGI
#'
#' @format `GHGI_landfill_total_M3T` A data frame with 12 rows and 2 columns:
#' \describe{
#'   \item{Year}{Year of data}
#'   \item{Emissions}{National total emissions in kilotons/year for municipal solid waste as reported in the GHGI}
#' }
#' @source
#' <https://www.epa.gov/ghgemissions/inventory-us-greenhouse-gas-emissions-and-sinks-1990-2022>
'GHGI_landfill_total_M3T'






#' Data from the Environmental Protection Agency's (EPA) Greenhouse Gas
#' Inventory (GHGI)
#'
#' A subset of data from the GHGI
#'
#' The national methane emission factors and activity data estimated for natural
#' gas distribution in the GHGI
#'
#' @format `GHGI_NG_distribution` A list of data frames:
#' \describe{
#'   \item{GHGI_MnR_EF}{Meter and regulating station emission factors in mol/s with columns representing years and rows representing type}
#'   \item{GHGI_MnR_Activity}{Meter and regulating station counts with columns representing years and rows representing type}
#'   \item{GHGI_services}{Service line emission factors in mol/s with columns representing years and rows representing material}
#'   \item{GHGI_meters}{Meter emission factors in mol/s with columns representing years and rows representing customer type}
#'   \item{GHGI_maintenance}{Maintenance/mishap emission factors in mol/s with columns representing years and rows representing type}
#' }
#' @source
#' <https://www.epa.gov/ghgemissions/natural-gas-and-petroleum-systems-ghg-inventory-additional-information-1990-2021-ghg>
'GHGI_NG_distribution'






#' Data from the Environmental Protection Agency's (EPA) Greenhouse Gas
#' Inventory (GHGI)
#'
#' A subset of data from the GHGI
#'
#' The national methane emission factors and activity data estimated for natural
#' gas distribution in the GHGI
#'
#' @format `GHGI_NG_transmission` A list of data frames:
#' \describe{
#'   \item{GHGI_Pipeline_Activity}{Pipeline equipment count or length of pipeline in meters with columns representing years and rows representing type}
#'   \item{GHGI_Pipeline_Emissions}{Pipeline equipment emissions in mol/s with columns representing years and rows representing type}
#'   \item{GHGI_transmission_compressors_Activity}{Transmission compressor counts with columns representing years and rows representing material}
#'   \item{GHGI_transmission_compressors_Emissions}{Transmission compressor emissions in mol/s with columns representing years and rows representing type}
#' }
#' @source
#' <https://www.epa.gov/ghgemissions/natural-gas-and-petroleum-systems-ghg-inventory-additional-information-1990-2021-ghg>
'GHGI_NG_transmission'






#' Data from the Environmental Protection Agency's (EPA) Greenhouse Gas
#' Inventory (GHGI)
#'
#' A subset of data from the GHGI
#'
#' The national fuel consumption data estimated in the GHGI
#'
#' @format `GHGI_stationary_combustion` A data frame with data year as
#'   rownames, 12 rows, and 15 columns:
#' \describe{
#'   \item{State}{US_EPA for all entries}
#'   \item{com_coal}{national coal use by the commercial sector in trillions of BTU}
#'   \item{ind_coal}{national coal use by the industrial sector in trillions of BTU}
#'   \item{elec_coal}{national coal use by the electric production sector in trillions of BTU}
#'   \item{res_petr}{national petroleum use by the residential sector in trillions of BTU}
#'   \item{com_petr}{national petroleum use by the commercial sector in trillions of BTU}
#'   \item{ind_petr}{national petroleum use by the industrial sector in trillions of BTU}
#'   \item{elec_petr}{national petroleum use by the electric production sector in trillions of BTU}
#'   \item{com_gas}{national natural gas use by the commercial sector in trillions of BTU}
#'   \item{ind_gas}{national natural gas use by the industrial sector in trillions of BTU}
#'   \item{elec_gas}{national natural gas use by the electric production sector in trillions of BTU}
#'   \item{res_wood}{national wood use by the residential sector in trillions of BTU}
#'   \item{com_wood}{national wood use by the commercial sector in trillions of BTU}
#'   \item{ind_wood}{national wood use by the industrial sector in trillions of BTU}
#'   \item{elec_wood}{national wood use by the electric production sector in trillions of BTU}
#' }
#' @source
#' <https://www.epa.gov/ghgemissions/inventory-us-greenhouse-gas-emissions-and-sinks-1990-2022>
'GHGI_stationary_combustion'






#' Data from the Environmental Protection Agency's (EPA) Greenhouse Gas
#' Reporting Program (GHGRP)
#'
#' A subset of data from the GHGRP
#'
#' Data for natural gas utilities
#'
#' @format `GHGRP_LDC` A data frame with 2,222 rows, and 7 columns:
#' \describe{
#'   \item{facility_id}{Unique GHGRP ID for each utility}
#'   \item{reporting_year}{Year of data}
#'   \item{Miles_of_Mains}{miles of natural gas mains pipelines}
#'   \item{N_of_above_grade_T_D_transfer_stations}{Count of above grade (meaning above the finished ground level of structures) transfer and distribution stations that switch gas from transmission lines to distribution lines}
#'   \item{N_of_above_grade_non_T_D_MR_stations}{Count of above grade (meaning above the finished ground level of structures) metering and regulating stations that measure gas flow and regulate the gas pressure in the line}
#'   \item{N_of_below_grade_T_D_transfer_stations}{Count of below grade (meaning below the finished ground level of structures) transfer and distribution stations that switch gas from transmission lines to distribution lines}
#'   \item{N_of_below_grade_non_T_D_MR_stations}{Count of below grade (meaning below the finished ground level of structures) metering and regulating stations that measure gas flow and regulate the gas pressure in the line}
#' }
#' @source
#' <https://enviro.epa.gov/envirofacts/metadata/table/ghg/w_local_dist_companies_details>
#' <https://enviro.epa.gov/envirofacts/metadata/table/ghg/ef_w_equip_leaks_ngdist_leaks>
#' <https://enviro.epa.gov/envirofacts/metadata/table/ghg/ef_w_equip_leaks_pop_count>
'GHGRP_LDC'






#' Data from the Environmental Protection Agency's (EPA) Greenhouse Gas
#' Reporting Program (GHGRP)
#'
#' A subset of data from the GHGRP
#'
#' Data for facilities with stationary combustion
#'
#' @format `GHGRP_combustion_emissions` A data frame with 68,122 rows, and 5 columns:
#' \describe{
#'   \item{facility_id}{Unique GHGRP ID for each facility}
#'   \item{facility_name}{Name of facility}
#'   \item{ghg_name}{greenhouse gas name - filtered to all methane}
#'   \item{ghg_quantity}{emissions of methane in metric tons of methane per year}
#'   \item{year}{Year of data}
#' }
#' @source
#' <https://enviro.epa.gov/envirofacts/metadata/table/ghg/c_subpart_level_information>
'GHGRP_combustion_emissions'







#' Data from the Environmental Protection Agency's (EPA) Greenhouse Gas
#' Reporting Program (GHGRP)
#'
#' A subset of data from the GHGRP
#'
#' Data for landfills
#'
#' @format `GHGRP_landfills` A data frame with 16,444 rows, and 7 columns:
#' \describe{
#'   \item{facility_id}{Unique GHGRP ID for each facility}
#'   \item{year}{Year of data}
#'   \item{facility_name}{Name of facility}
#'   \item{ghg_name}{greenhouse gas name - filtered to all methane}
#'   \item{ghg_quantity}{emissions of methane in metric tons of methane per year}
#'   \item{HH_modeled}{emissions of methane in metric tons of methane per year as estimated using a first order decay model (method HH-6), required for all reporting landfills}
#'   \item{HH_collection_efficiency}{emissions of methane in metric tons of methane per year as estimated using a back calculation with the amount of landfill gas captured and an estimated collection efficiency (method HH-8), required for all reporting landfills with a gas collection system}
#' }
#' @source
#' <https://enviro.epa.gov/envirofacts/metadata/table/ghg/hh_subpart_level_information>
#' <https://enviro.epa.gov/envirofacts/metadata/table/ghg/hh_gas_collection_system_detls>
'GHGRP_landfills'







#' Data from the Environmental Protection Agency's (EPA) Greenhouse Gas
#' Reporting Program (GHGRP)
#'
#' A subset of data from the GHGRP
#'
#' Data for industrial wastewater treatment facilities
#'
#' @format `GHGRP_wastewater` A data frame with 1,821 rows, and 5 columns:
#' \describe{
#'   \item{facility_id}{Unique GHGRP ID for each facility}
#'   \item{facility_name}{Name of facility}
#'   \item{ghg_name}{greenhouse gas name - filtered to all methane}
#'   \item{ghg_quantity}{emissions of methane in metric tons of methane per year}
#'   \item{reporting_year}{Year of data}
#' }
#' @source
#' <https://enviro.epa.gov/envirofacts/metadata/table/ghg/ii_subpart_level_information>
'GHGRP_wastewater'






#' Data formerly provided publicly by the Homeland Infrastructure
#' Foundation-Level Data (HIFLD) that has since been discontinued.  For this
#' work it has been processed to map to GHGRP facilities
#'
#' A subset of data from HIFLD
#'
#' Data for transmission compressor stations
#'
#' @format `HIFLD_NG_data` A data frame with 2,302 rows, and 5 columns:
#' \describe{
#'   \item{LATITUDE}{Facility location coordinates}
#'   \item{LONGITUDE}{Facility location coordinates}
#'   \item{GHGRP ID}{Unique GHGRP ID for each facility if there is a corresponding facility}
#'   \item{STATE}{State that the facility operates in}
#'   \item{NAME}{Facility name}
#' }
#' @source
#' <https://www.dhs.gov/gmo/hifld>
'HIFLD_NG_data'






#' Data from the Environmental Protection Agency's (EPA) Landfill Methane
#' Outreach Program (LMOP)
#'
#' A subset of data from LMOP
#'
#' Data for landfills, including those too small to report to the GHGRP
#'
#' @format `LMOP_data` A data frame with 2,323 rows, and 5 columns:
#' \describe{
#'   \item{GHGRP ID}{Unique GHGRP ID for each facility if there is a corresponding facility}
#'   \item{Latitude}{Facility location coordinates}
#'   \item{Longitude}{Facility location coordinates}
#'   \item{Landfill Name}{Facility name}
#'   \item{Year Landfill Opened}{Year the landfill opened}
#' }
#' @source <https://www.epa.gov/lmop/landfill-technical-data>
'LMOP_data'






#' Data from the Environmental Protection Agency's (EPA) National Emissions
#' Inventory (NEI)
#'
#' A subset of data from the NEI
#'
#' Data on CO emissions from a wide variety of sources separated by fuel,
#' county, year, and economic sector
#'
#' @format `NEI_all_years` A data frame with 2,323 rows, and 5 columns:
#' \describe{
#'   \item{COUNTY FIPS}{Federal Information Processing Standards (FIPS) code for the county}
#'   \item{EMISSIONS}{Emissions of carbon monoxide (CO) in short tons per year}
#'   \item{INVENTORY YEAR}{Year of data}
#'   \item{SECTOR}{Economic sector and fuel combination}
#'   \item{STATE}{State abbreviation}
#'   \item{STATE FIPS}{Federal Information Processing Standards (FIPS) code for the state}
#' }
#' @source
#' <https://www.epa.gov/air-emissions-inventories/national-emissions-inventory-nei>
'NEI_all_years'






#' Pipelines and Hazardous Materials Safety Administration (PHMSA) Gas
#' Distribution Annual Data
#'
#' A subset of tables from the PHMSA reports
#'
#' The PHMSA reports on natural gas distribution service and main pipelines by
#' utility
#'
#' @format `PHMSA_natural_gas_distribution` A data frame with 21,321 rows and
#'   14 columns:
#' \describe{
#'   \item{OPERATOR_NAME}{Utility name}
#'   \item{REPORT_YEAR}{Year the data was reported for}
#'   \item{NUM_SRVCS_TOTAL}{Total number of service lines (those connecting from a distribution main to an end user) in the system at the end of the year}
#'   \item{AVERAGE_LENGTH}{Average length of service lines in feet}
#'   \item{STOP}{State in which the system operates}
#'   \item{MMILES_TOTAL}{Total miles of pipe in the system at the end of the year}
#'   \item{MMILES_bare_steel}{Total miles of bare steel (cathodically protected or not) and copper pipe in the system at the end of the year}
#'   \item{MMILES_iron}{Total miles of cast iron, ductile iron, and reconditioned cast iron pipe in the system at the end of the year}
#'   \item{MMILES_coat_steel}{Total miles of coated steel (cathodically protected or not) and "other" pipe in the system at the end of the year}
#'   \item{MMILES_plastic}{Total miles of plastic pipe in the system at the end of the year}
#'   \item{NUM_SRVS_unp_steel}{Total number of not cathodically protected steel (bare or coated) service lines in the system at the end of the year}
#'   \item{NUM_SRVS_cp_steel}{Total number of cathodically protected steel (bare or coated) and "other" service lines in the system at the end of the year}
#'   \item{NUM_SRVS_plastic}{Total number of plastic service lines in the system at the end of the year}
#'   \item{NUM_SRVS_copper_iron}{Total number of copper, cast iron, ductile iron, and reconditioned cast iron service lines in the system at the end of the year}
#' }
#' @source
#' <https://www.phmsa.dot.gov/data-and-statistics/pipeline/gas-distribution-gas-gathering-gas-transmission-hazardous-liquids>
"PHMSA_natural_gas_distribution"

