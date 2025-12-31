## code to prepare `GHGRP_MR_TD_stations` dataset goes here

#Reporting changed for subpart W after 2014, content is the same, format is
#different

################################################################################
#get additional variables from GHGRP that are more detailed in other tables

ghgrp_LDC_file <- tempfile(fileext = ".csv")
ghgrp_ngdist_file <- tempfile(fileext = ".csv")
ghgrp_pop_count_file <- tempfile(fileext = ".csv")

################################################################################
#pre 2015
data_URL <- "https://data.epa.gov/dmapservice/ghg.w_local_dist_companies_details/csv"
download.file(data_URL,ghgrp_LDC_file,quiet=T)

#grab the needed data and merge with the past section
old_subpartW <- read.csv(ghgrp_LDC_file)
old_subpartW <- old_subpartW[!is.na(old_subpartW$reporting_year),]
#Calculate the total miles of pipeline across materials
old_subpartW$total_miles <- rowSums(old_subpartW[,c("miles_of_cast_iron_dist_mains","miles_of_plstic_dist_mains","miles_of_prot_steel_dist_mains","miles_of_unpr_steel_dist_mains")],na.rm=T)

old_subpartW <- old_subpartW[,c("facility_id","reporting_year","total_miles","above_grade_transfer_stations","above_grade_metering_stations","below_grade_transfer_stations","below_grade_metering_stations")]
#rename for consistency
colnames(old_subpartW) <- c("facility_id","reporting_year","Miles_of_Mains","N_of_above_grade_T_D_transfer_stations","N_of_above_grade_non_T_D_MR_stations","N_of_below_grade_T_D_transfer_stations","N_of_below_grade_non_T_D_MR_stations")

################################################################################
#2015 and post

data_URL <- "https://data.epa.gov/dmapservice/ghg.ef_w_equip_leaks_ngdist_leaks/csv"
utils::download.file(data_URL,ghgrp_ngdist_file,quiet=T)

data_URL <- "https://data.epa.gov/dmapservice/ghg.ef_w_equip_leaks_pop_count//csv"
utils::download.file(data_URL,ghgrp_pop_count_file,quiet=T)

ghgrp_ngdist <- utils::read.csv(ghgrp_ngdist_file)
ghgrp_ngdist <- ghgrp_ngdist[order(ghgrp_ngdist$facility_id),c("facility_id","reporting_year","total_td_facility_stations","total_non_td_facility_stations")]
#rename for consistency
colnames(ghgrp_ngdist) <- c("facility_id","reporting_year","N_of_above_grade_T_D_transfer_stations","N_of_above_grade_non_T_D_MR_stations")


ghgrp_pop_count <- utils::read.csv(ghgrp_pop_count_file)
ghgrp_pop_count <- ghgrp_pop_count[order(ghgrp_pop_count$facility_id),]

facility_id <- ghgrp_pop_count[ghgrp_pop_count$emission_src_type=="Distribution Mains, Gas Service - Cast Iron","facility_id"]
reporting_year <- ghgrp_pop_count[ghgrp_pop_count$emission_src_type=="Distribution Mains, Gas Service - Cast Iron","reporting_year"]
total_miles <- rowSums(cbind(ghgrp_pop_count[ghgrp_pop_count$emission_src_type=="Distribution Mains, Gas Service - Cast Iron","source_type_count"],
                             ghgrp_pop_count[ghgrp_pop_count$emission_src_type=="Distribution Mains, Gas Service - Plastic","source_type_count"],
                             ghgrp_pop_count[ghgrp_pop_count$emission_src_type=="Distribution Mains, Gas Service - Protected Steel","source_type_count"],
                             ghgrp_pop_count[ghgrp_pop_count$emission_src_type=="Distribution Mains, Gas Service - Unprotected Steel","source_type_count"]))
N_of_below_grade_T_D_transfer_stations <- rowSums(cbind(ghgrp_pop_count[ghgrp_pop_count$emission_src_type=="Below Grade T-D Station, Gas Service, Inlet Pressure < 100 psig","source_type_count"],
                                                        ghgrp_pop_count[ghgrp_pop_count$emission_src_type=="Below Grade T-D Station, Gas Service, Inlet Pressure 100 to 300 psig","source_type_count"],
                                                        ghgrp_pop_count[ghgrp_pop_count$emission_src_type=="Below Grade T-D Station, Gas Service, Inlet Pressure > 300 psig","source_type_count"]))
N_of_below_grade_non_T_D_MR_stations <- rowSums(cbind(ghgrp_pop_count[ghgrp_pop_count$emission_src_type=="Below Grade M-R Station, Gas Service, Inlet Pressure < 100 psig","source_type_count"],
                                                      ghgrp_pop_count[ghgrp_pop_count$emission_src_type=="Below Grade M-R Station, Gas Service, Inlet Pressure 100 to 300 psig","source_type_count"],
                                                      ghgrp_pop_count[ghgrp_pop_count$emission_src_type=="Below Grade M-R Station, Gas Service, Inlet Pressure > 300 psig","source_type_count"]))
ghgrp_pop_count_data <- data.frame(facility_id,reporting_year,total_miles,N_of_below_grade_T_D_transfer_stations,N_of_below_grade_non_T_D_MR_stations)
colnames(ghgrp_pop_count_data) <- c("facility_id","reporting_year","Miles_of_Mains","N_of_below_grade_T_D_transfer_stations","N_of_below_grade_non_T_D_MR_stations")

################################################################################
#combine pre and post 2015

new_subpartW <- merge(ghgrp_ngdist,ghgrp_pop_count_data,by = c("facility_id","reporting_year"),all = T)

GHGRP_LDC <- rbind(old_subpartW,new_subpartW)

################################################################################

unlink(c(ghgrp_LDC_file,ghgrp_ngdist_file,ghgrp_pop_count_file))

usethis::use_data(GHGRP_LDC, overwrite = TRUE)
