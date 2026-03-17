## code to prepare `EIA_SEDS` dataset.

################################################################################
#download SEDS data

#CONUS subset including DC
SEDS_state_name_list <- datasets::state.abb[!datasets::state.abb %in% c("AK","AS","MP","PR","HI","GU","VI")]
SEDS_state_name_list <- sort(c(SEDS_state_name_list,"DC"))
SEDS_state_name_list <- c(paste0("USA-",SEDS_state_name_list),"USA\"")

temp_file <- tempfile(fileext = ".zip")
temp_dir <- tempdir()

download.file("https://www.eia.gov/opendata/bulk/SEDS.zip",temp_file)
unzip(temp_file,exdir=temp_dir,overwrite = T)

SEDS_filename <- list.files(temp_dir,full.names = T,pattern = ".txt")

#see https://www.eia.gov/opendata/browser/seds.  Filtered to only sectors,
#states, and years of interest here.  All in billion BTU/yr units (last
#digit B instead of P - short tons)
EIA_raw_json <- readLines(SEDS_filename)
EIA_raw_json <- EIA_raw_json[grep("CLCCB|CLEIB|CLICB|NGCCB|NGEIB|NGICB|PACCB|PAEIB|PAICB|PARCB|WDRCB|WWCCB|WWEIB|WWICB",EIA_raw_json)]
EIA_raw_json <- EIA_raw_json[grep(paste0(SEDS_state_name_list,collapse="|"),EIA_raw_json)]

#load data from 1 entry and format to align with the API download format
subset_data <- jsonlite::fromJSON(EIA_raw_json[1])
EIA_raw_data <- data.frame("period"=as.numeric(subset_data$data[,1]),
                           "seriesId"=strsplit(subset_data$series_id,"\\.")[[1]][2],
                           "seriesDescription"=subset_data$name,
                           "stateId"=strsplit(subset_data$series_id,"\\.")[[1]][3],
                           "value"=as.numeric(subset_data$data[,2]))
for(A in 2:length(EIA_raw_json)){
  subset_data <- jsonlite::fromJSON(EIA_raw_json[A])
  temp <- data.frame("period"=as.numeric(subset_data$data[,1]),
                     "seriesId"=strsplit(subset_data$series_id,"\\.")[[1]][2],
                     "seriesDescription"=subset_data$name,
                     "stateId"=strsplit(subset_data$series_id,"\\.")[[1]][3],
                     "value"=as.numeric(subset_data$data[,2]))
  EIA_raw_data <- rbind(EIA_raw_data,temp)
}

#limit to only relevant years
EIA_raw_data <- EIA_raw_data[EIA_raw_data$period>=2011,]

#organize to be equivalent to past API dataset
EIA_raw_data <- EIA_raw_data[order(EIA_raw_data$period,EIA_raw_data$seriesId,EIA_raw_data$stateId),]

#remove temp files
unlink(temp_dir,recursive = T)
unlink(temp_file)
################################################################################
#save

EIA_SEDS <- EIA_raw_data
usethis::use_data(EIA_SEDS, overwrite = TRUE)


