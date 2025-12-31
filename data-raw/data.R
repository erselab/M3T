#' 2012 Clean Watershed Needs Survey (CWNS) data on waste water treatment plants
#'
#' A subset of tables from the 2012 CWNS
#' 
#' The 2012 CWNS is available as an access database.  The relevant data was pulled and the file was converted to a csv file by:
#' \describe{
#'    \item{Open mdb file in Microsoft Access}
#'    \item{Go to Create tab -> Query Wizard}
#'    \item{Select Simple Query Wizard}
#'    \item{Choose the first table you want (SUMMARY_FACILITY)}
#'    \item{Click the double right arrow to take all columns}
#'    \item{Repeat for other table (SUMMARY_FACILITY_FLOW)}
#'    \item{Click Finish}
#'    \item{In the left hand pane, make sure you have selected to view all Access objects}
#'    \item{Your query should be here at the bottom – right click on it and select to export to Excel (.xlsx)}
#'    \item{Note that Access seems to automatically save this query to the access file}
#' }
#'
#' @format ## `CWNS_2012`
#' A data frame with 15,359 rows and 4 columns:
#' \describe{
#'   \item{LONGITUDE, LATITUDE}{Facility location coordinates}
#'   \item{HORIZONTAL_COORDINATE_DATUM}{Information regarding the mapping used in the coordinates}
#'   \item{EXIST_MUNICIPAL}{Annual flow of municipal wastewater through a facility in millions of gallons per day}
#' }
#' @source <https://www.epa.gov/cwns/clean-watersheds-needs-survey-cwns-2012-report-and-data>
"CWNS_2012"










#' 2022 Clean Watershed Needs Survey (CWNS) data on waste water treatment plants
#'
#' A subset of tables from the 2022 CWNS
#' 
#' The 2022 CWNS is available as several CSVs by going to the data dashboard and selecting data download in the top right.  The relevant data was pulled from the various files, combined, and saved to a single CSV.
#'
#' @format ## `CWNS_2022`
#' A data frame with 16,477 rows and 3 columns:
#' \describe{
#'   \item{LONGITUDE, LATITUDE}{Facility location coordinates}
#'   \item{EXIST_MUNICIPAL}{Annual flow of municipal wastewater through a facility in millions of gallons per day}
#' }
#' @source <https://www.epa.gov/cwns/clean-watersheds-needs-survey-cwns-2022-report-and-data>
"CWNS_2022"










#' Pipelines and Hazardous Materials Safety Administration (PHMSA) Gas Distribution Annual Data (2010 - present)
#'
#' A subset of tables from the PHMSA reports including 2010 - present
#' 
#' The PHMSA reports are available as a zip file.
#'
#' @format ## `PHMSA_natural_gas_distribution`
#' A data frame with 21,321 rows and 26 columns:
#' \describe{
#'   \item{REPORT_YEAR}{Year the data was reported for}
#'   \item{NUM_SRVCS_TOTAL}{Total number of services in the system at the end of the year}
#'   \item{AVERAGE_LENGTH}{Average length of service in feet}
#'   \item{STOP}{State in which the system operates}
#'   \item{MMILES_TOTAL}{Total miles pipe in the system at the end of the year}
#'   \item{MMILES_X}{Total miles pipe in the system at the end of the year where X is the material}
#'   \item{NUM_SRVS_x}{Total number of services in the system at the end of the year where X is the material}
#' }
#' The materials for mains are:
#' \describe{
#'  \item{bare_steel}{bare steel (cathodically protected or not) and copper}
#'  \item{iron}{cast iron, ductile iron, reconditioned cast iron}
#'  \item{coat_steel}{coated steel (cathodically protected or not) and "other"}
#'  \item{plastic}{all plastics}
#'  \item{copper_iron}{}
#' }
#' The materials for services are:
#' \describe{
#'  \item{unp_steel}{not cathodically protected steel (bare or coated)}
#'  \item{cp_steel}{cathodically protected steel (bare or coated) and "other"}
#'  \item{plastic}{all plastics}
#'  \item{copper_iron}{copper, cast iron, ductile iron, reconditioned cast iron}
#' }
#' @source <https://www.phmsa.dot.gov/data-and-statistics/pipeline/gas-distribution-gas-gathering-gas-transmission-hazardous-liquids>
"PHMSA_natural_gas_distribution"


