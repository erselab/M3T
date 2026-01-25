## code to prepare `state_population` dataset goes here

unlink(c(Old_State_pop_file,))
################################################################################
#download

#first load in the state population dataset - new each decade as new census'
#are done
Old_State_pop_file <- tempfile(fileext = ".csv")

data_URL <- "https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/state/totals/nst-est2020-alldata.csv"
utils::download.file(data_URL,Old_State_pop_file,quiet=T)

#for every year after the most recent census they output a new file with the
#most recent estimates.  Iteratively test down to 2024 (most recent available
#when code was written) to find the newest one available.
for(A in 2029:2024){
  data_URL <- paste0("https://www2.census.gov/programs-surveys/popest/datasets/2020-",A,"/state/totals/NST-EST",A,"-ALLDATA.csv")
  test_url <- suppressWarnings(try(download.file(data_URL,tempfile(".csv"),quiet = T),silent = T))
  if(test_url==0){
    break
  }
}
New_State_pop_file <- tempfile(fileext = ".csv")
utils::download.file(data_URL,New_State_pop_file,quiet=T)

################################################################################
#Clean and combine
Old_State_population <- read.csv(Old_State_pop_file)
New_State_population <- read.csv(New_State_pop_file)

#remove unneeded data - all columns other than pop, regional data, states
#outside of CONUS, and the old version of 2020 estimates
New_State_population <- New_State_population[,c("STATE","NAME",colnames(New_State_population)[grep("POPESTIMATE",colnames(New_State_population))])]
Old_State_population <- Old_State_population[,c("STATE","NAME",colnames(Old_State_population)[grep("POPESTIMATE",colnames(Old_State_population))])]

Old_State_population <- Old_State_population[,-grep("POPESTIMATE2020",colnames(Old_State_population))]

New_State_population <- New_State_population[!(New_State_population$STATE==0),]
Old_State_population <- Old_State_population[!(Old_State_population$STATE==0),]

New_State_population <- New_State_population[!(New_State_population$STATE %in% c(2,15,72)),-1]
Old_State_population <- Old_State_population[!(Old_State_population$STATE %in% c(2,15,72)),-1]

Census_state_population <- merge(Old_State_population,New_State_population,by="NAME")
################################################################################
#save

usethis::use_data(Census_state_population, overwrite = TRUE)

