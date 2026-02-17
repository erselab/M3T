## code to prepare `PHMSA_natural_gas_distribution` dataset.  This takes the
## folder from the Pipelines and Hazardous Materials Safety Administration
## (PHMSA) Gas Distribution Annual Data (2010 - present)
## https://www.phmsa.dot.gov/data-and-statistics/pipeline/gas-distribution-gas-gathering-gas-transmission-hazardous-liquids
## This zip file can be downloaded and extracted, though automating this process
## failed.  This code can then be run after setting the working directory to the
## unzipped folder.

input_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Manuscript/All inventory data/Prepared inventory data/"
################################################################################
#settings

#the subset variables needed for the package
PHMSA_cols_to_keep <- c("OPERATOR_NAME",
                        "REPORT_YEAR",
                        "NUM_SRVCS_TOTAL",
                        "AVERAGE_LENGTH",
                        "COMMODITY",
                        "STOP",
                        'MMILES_STEEL_UNP_BARE',
                        'MMILES_STEEL_UNP_COATED',
                        'MMILES_STEEL_CP_BARE',
                        'MMILES_STEEL_CP_COATED',
                        'MMILES_PLASTIC',
                        'MMILES_CI',
                        'MMILES_DI',
                        'MMILES_CU',
                        'MMILES_OTHER',
                        'MMILES_RCI',
                        'MMILES_TOTAL',
                        'NUM_SRVS_STEEL_UNP_BARE',
                        'NUM_SRVS_STEEL_UNP_COATED',
                        'NUM_SRVS_STEEL_CP_BARE',
                        'NUM_SRVS_STEEL_CP_COATED',
                        'NUM_SRVS_PLASTIC',
                        'NUM_SRVS_CI',
                        'NUM_SRVS_DI',
                        'NUM_SRVS_CU',
                        'NUM_SRVS_OTHER',
                        'NUM_SRVS_RCI')

################################################################################
#download

#initialize a blank data frame
combined_data <- data.frame(matrix(ncol=length(PHMSA_cols_to_keep),nrow=0))
colnames(combined_data) <- PHMSA_cols_to_keep

PHMSA_files <- list.files(path=input_directory,pattern="annual_gas_distribution_",full.names = T,recursive = T)

for(A in 1:length(PHMSA_files)){
  #read in 1 by 1, suppress messages as most files have rows before the header
  data <- suppressMessages(readxl::read_excel(PHMSA_files[A]))
  
  #read in with appropriate headers, suppress warnings as at least 1 file had an
  #unused column that flagged a class warning in read_excel
  if(colnames(data)[1]!="DATAFILE_AS_OF"){
    data <- suppressWarnings(readxl::read_excel(PHMSA_files[A],skip=which(data[,1]=="DATAFILE_AS_OF")))
  }
  
  #convert to data frame for space
  data <- as.data.frame(data)
  
  #commodity describes what gas a pipeline is moving (natural gas, propane,
  #synthetic gas, landfill gas, hydrogen, and other), but this variable is not
  #included before 2015.  However, in 2015, less than 0.2% of all pipelines were
  #not natural gas.  As such, we use earlier data even though a very small
  #fraction may be pipelines for gases other than natural gas.
  
  #RCI (reconditioned cast iron) variables also do not exist before 2015.  In
  #2015, RCI represented < 0.002% of all pipe, so it is similarly assumed
  #negligible for earlier years.
  
  if(all(PHMSA_cols_to_keep %in% colnames(data))){
    data <- data[,PHMSA_cols_to_keep]
    data <- data[which(data$COMMODITY == 'Natural Gas'),]
  }
  
  if(all(PHMSA_cols_to_keep[!PHMSA_cols_to_keep %in% colnames(data)] == c("COMMODITY","MMILES_RCI","NUM_SRVS_RCI"))){
    data <- data[,PHMSA_cols_to_keep[!PHMSA_cols_to_keep %in% c("COMMODITY","MMILES_RCI","NUM_SRVS_RCI")]]
    #create all 0 columns for these so they align with other years
    data$MMILES_RCI <- 0
    data$NUM_SRVS_RCI <- 0
  }
  
  #combine and user update
  combined_data <- rbind(combined_data,data)
  cat("\rFinished file",A,"of",length(PHMSA_files),"                       ")
}
################################################################################
#combine and cleanup

PHMSA_natural_gas_distribution <- data.frame(matrix(ncol=0,nrow=nrow(combined_data)))

#combine to just the columns and combination of columns needed
PHMSA_natural_gas_distribution[,c("OPERATOR_NAME","REPORT_YEAR","NUM_SRVCS_TOTAL","AVERAGE_LENGTH","STOP",'MMILES_TOTAL')] <- combined_data[,c("OPERATOR_NAME","REPORT_YEAR","NUM_SRVCS_TOTAL","AVERAGE_LENGTH","STOP",'MMILES_TOTAL')]

PHMSA_natural_gas_distribution$MMILES_bare_steel <- rowSums(combined_data[,c("MMILES_STEEL_UNP_BARE","MMILES_STEEL_CP_BARE","MMILES_CU")],na.rm=T)
PHMSA_natural_gas_distribution$MMILES_iron <- rowSums(combined_data[,c("MMILES_CI","MMILES_DI","MMILES_RCI")],na.rm=T)
PHMSA_natural_gas_distribution$MMILES_coat_steel <- rowSums(combined_data[,c("MMILES_STEEL_UNP_COATED","MMILES_STEEL_CP_COATED","MMILES_OTHER")],na.rm=T)
PHMSA_natural_gas_distribution$MMILES_plastic <- combined_data[,"MMILES_PLASTIC"]

PHMSA_natural_gas_distribution$NUM_SRVS_unp_steel <- rowSums(combined_data[,c("NUM_SRVS_STEEL_UNP_COATED","NUM_SRVS_STEEL_UNP_BARE")],na.rm=T)
PHMSA_natural_gas_distribution$NUM_SRVS_cp_steel <- rowSums(combined_data[,c("NUM_SRVS_STEEL_CP_BARE","NUM_SRVS_STEEL_CP_COATED","NUM_SRVS_OTHER")],na.rm=T)
PHMSA_natural_gas_distribution$NUM_SRVS_plastic <- combined_data[,c("NUM_SRVS_PLASTIC")]
PHMSA_natural_gas_distribution$NUM_SRVS_copper_iron <- rowSums(combined_data[,c("NUM_SRVS_CU","NUM_SRVS_CI","NUM_SRVS_DI","NUM_SRVS_RCI")],na.rm=T)

################################################################################
#save output
usethis::use_data(PHMSA_natural_gas_distribution, overwrite = TRUE)

