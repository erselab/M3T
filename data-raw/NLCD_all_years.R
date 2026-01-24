## code to prepare `NLCD_all_years` data.  Downloads every relevant year of NLCD
## data available and combines them to a single file. Annual NLCD files are
## available at 

input_directory <- "G:/My Drive/Shepson Group Drive/Kris/Philly Inventory/Manuscript/All inventory data/Prepared inventory data/"

#these are large files and may take longer than the default timeout
options("timeout"=60*10)

################################################################################
#save partial data given the significant processing time/memory needed

NLCD_output_directory <- file.path(input_directory,"NLCD_data")
dir.create(NLCD_output_directory,showWarnings = F,recursive = T)

################################################################################
#Download annual NLCD v1.1 data that's available from 2011 and up

#identify the years available from the data catalog
main_page_URL <- "https://www.sciencebase.gov/catalog/item/6810c1a4d4be022940554075?format=json"
NLCD_filenames <- jsonlite::fromJSON(main_page_URL)
NLCD_filenames <- NLCD_filenames$files$name

#determine years and filter to those relevant for M3T
Data_Yr <- sapply(strsplit(NLCD_filenames,"_"),"[[",4)

NLCD_filenames <- NLCD_filenames[which(as.numeric(Data_Yr)>2010)]
Data_Yr <- Data_Yr[which(as.numeric(Data_Yr)>2010)]

download_location <- tempfile(fileext = ".zip")

#loop to download and unzip each 1 in sequence
for(A in 1:length(NLCD_filenames)){
  NLCD_URL <- paste0("https://www.mrlc.gov/downloads/sciweb1/shared/mrlc/data-bundles/",NLCD_filenames[A])
  utils::download.file(NLCD_URL,download_location,method = "curl",quiet = T)
  utils::unzip(download_location,exdir=file.path(NLCD_output_directory,Data_Yr[A]))
  
  #delete the temp file
  unlink(download_location)
  cat("\rFinished downloading",A,"of",length(NLCD_filenames),"                    ")
}

################################################################################
#check the output

# test=rast(file.path(NLCD_output_directory,"2020","Annual_NLCD_LndCov_2020_CU_C1V1.tif"))
# NLCD_key <- data.frame("Value"=c(11,12,21:24,31,41:43,52,71,81:82,90,95),
#                        "Land_Class"=levels(test)[[1]][,2])
# levels(test) <- NLCD_key
