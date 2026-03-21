## code to prepare `Neighbored_states` dataset. Small matrix needed for natural
## gas distribution, which states are neighbors to each.  This is used to
## spatially average data on metering and regulating stations for states with no
## data.

library(terra)
################################################################################
#download state tigerlines from the Census

#year of data is ~irrelevant for this, getting 2022 data
data_URL <- "https://www2.census.gov/geo/tiger/TIGER2022/STATE/tl_2022_us_state.zip"

temp_file_name <- tempfile(fileext = ".zip")
temp_dir_name <- tempdir()

download.file(data_URL,temp_file_name)

unzip(temp_file_name,exdir = temp_dir_name)

#load in shapefile
State_Tigerlines <- vect(file.path(temp_dir_name,"tl_2022_us_state.shp"))

#filter out states outside of the continental US
State_Tigerlines <- State_Tigerlines[!State_Tigerlines$STUSPS %in% c("AK","AS","PR","HI","MP","GU","VI"),]

#organize to simplify processing elsewhere
State_Tigerlines <- State_Tigerlines[order(State_Tigerlines$STUSPS),]

################################################################################
#process tigerlines to get neighboring states

#matrix where every row and column is a logical indicating states that border
#each other
Neighboring_states <- relate(State_Tigerlines,State_Tigerlines,relation="touches")

#add row and column names
colnames(Neighboring_states) <- rownames(Neighboring_states) <- State_Tigerlines$STUSPS

################################################################################
#save

usethis::use_data(Neighboring_states, overwrite = TRUE)
