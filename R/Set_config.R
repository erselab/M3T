#'@title Create the config file utilized by `CH4_inventory_build`
#'
#'@description This function is a wrapper to build the config that provides
#'  `CH4_inventory_build` with all emission factors and similar user-edited
#'  input data as well as options for which sectors to run and, when applicable,
#'  which variations to run.
#'
#'@details This function creates the config, which is intended to be the
#'  location for all user edited variables other than inputs to the main
#'  `CH4_inventory_build` function.
#'
#'@inheritParams CH4_inventory_build
#'
#'@returns A config file is created and opened for editing in the user set
#'  directory and that directory is saved to the global environment.  These
#'  user-edited settings are utilized throughout.
#'
#'@inherit CH4_inventory_build author
#'
#'@examples
#'Set_config("~/../Desktop/CONUS_2020_MMMT_run/")
#'
#'@export
#'
#'@seealso [CH4_inventory_build()] Calculates methane inventory using settings
#'  provided in config.

Set_config <- function(run_directory){
  
  #create the directory if needed
  dir.create(file.path(run_directory,"in"),showWarnings = F,recursive = T)
  
  #output file name
  output_file <- file.path(run_directory,"in","MMMT_config.R")
  
  #load in the default config built into the package and update the directory
  #with the run directory
  txtdata <- readLines(system.file(package="MMMT","R/MMMT_config.txt"))
  directory_line <- grep("run_directory <- ",txtdata)
  txtdata[directory_line] <- gsub("<- ",paste0("<- \"",run_directory,"\""),
                                  txtdata[directory_line])
  
  #if the file already exists, prompt user and rely on past file or overwrite.
  #readline default = "" so only overwrite if user specifies yes exactly. If no
  #file exists, create.
  if(file.exists(output_file)){
    response <- readline("Overwrite existing config file in this directory with the default (yes/no)?")
    if(response=="yes"){
      writeLines(txtdata,output_file)
    }
  }else{
    writeLines(txtdata,output_file)
  }
  
  #user alert + open for editing.
  cat("The config file should be open for editing.  Edit settings as desired.\nBe sure to save changes (Ctrl + S or Cmd + S) before proceeding!")
  file.edit(output_file)
  assign("run_directory",run_directory,pos = .GlobalEnv)
}



