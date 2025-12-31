#'@title Create gridded methane emissions maps that include all sectors
#'
#'@description `Combine_inventories` writes a netcdf for every unique
#'  combination of sectoral emissions estimates.  Optionally writes additional
#'  netcdf with the thermogenic and nonthermogenic sectors separately.
#'
#'@details This function considers every possible output across all sectors and
#'  filters to keep those that have been saved to the output folder.  From there
#'  it creates a table of every possible unique combination of sectors.  If many
#'  options have been enabled, this can be over 1,000 combinations, so caution
#'  is warranted, though processing is generally fast and file sizes small -
#'  this will depend strongly on the domain size. Additionally, if the config
#'  option is set, three inventories will be created for each unique combination
#'  instead:
#' \itemize{
#'   \item One that is the total across all sectors
#'   \item One that is the total across only thermogenic sources
#'   \itemize{
#'     \item stationary combustion of wood
#'     \item natural gas distribution
#'     \item natural gas transmission
#'     \item Gridded EPA mobile combustion
#'     \item Gridded EPA abandoned coal
#'     \item Gridded EPA surface coal
#'     \item Gridded EPA underground coal
#'     \item Gridded EPA petroleum systems exploration
#'     \item Gridded EPA petroleum systems production
#'     \item Gridded EPA petroleum systems refining
#'     \item Gridded EPA petroleum systems transport
#'     \item Gridded EPA abandoned oil gas
#'     \item Gridded EPA natural gas exploration
#'     \item Gridded EPA natural gas processing
#'     \item Gridded EPA natural gas production
#'     \item Gridded EPA industry petrochemical
#'     \item Gridded EPA industry ferroalloy
#'   }
#'   \item One that is the total across only non-thermogenic sources
#'   \itemize{
#'     \item landfills
#'     \item stationary combustion of fossil fuels
#'     \item wastewater
#'     \item wetlands
#'     \item Gridded EPA composting
#'     \item Gridded EPA enteric fermentation
#'     \item Gridded EPA manure management
#'     \item Gridded EPA rice cultivation
#'     \item Gridded EPA field burning
#'   }
#' }
#'
#'@param output_directory Character providing the full filepath to save
#'  processed data
#'@param separate_thermo Logical.  Pulled from config file.  Indicating whether
#'  or not to save partial inventories: one that is only thermogenic sources,
#'  and one that is only non-thermogenic sources.
#'
#'@returns Nothing is returned from the function, but the main outputs are many
#'  netcdf files of the methane emissions across all sectors.  Given the large
#'  number of possible files and variations, they are titled
#'  "Combined_inventory_combination_1.nc" increasing numerically.  A csv is also
#'  saved that details what variations were used for each sector for each
#'  inventory.
#'
#'@examples
#'library(terra)
#'Combine_inventories <- function(output_directory="~/../Desktop/out/",
#'                                separate_thermo=T)
#'@author Joe Pitt, \email{madeup@@wisc.edu}
#'@author Kris Hajny, \email{blank@@fake.edu}
#'@author Israel Lopez-Coto, \email{test@@test.edu}
#'@export
#'@seealso [CH4_inventory_build()] Calculates methane inventory using settings provided in config.







#edit this so any number of sectors can be included, e.g., someone wants to use
#a not wetcharts biogenic CH4 or in general just wants anthropogenic










Combine_inventories <- function(output_directory,
                                separate_thermo,
                                plot_directory,
                                County_Tigerlines,
                                State_CB,
                                domain,
                                domain_template,
                                verbose){
  
  starttime <- Sys.time()
  cat("Starting the process of combining emissions across all sectors: Combine_inventories\n")
  
  Combined_output_directory <- file.path(output_directory,"Combined_files")
  dir.create(Combined_output_directory,showWarnings = F)
  ################################################################################
  #tiny helper function as get fails in an apply otherwise
  local_get <- function(x){get(x,envir=environment())}
  
  ################################################################################
  #list all possible output variations
  
  set_output <- c("GEPA_ind_landfill.nc","GEPA_non_thermo.nc","GEPA_thermo.nc",
                  "NG_transmission_sector_total.nc")
  Landfill_options <- c("GHGRP_reported","GHGRP_modeled","GHGRP_collection_efficiency")
  Wetland_options <- c("SOCCR1","SOCCR2","Wetcharts_NLCD")
  
  #expand.grid to combine multiple variations in 1 sector - just simpler 
  NG_dist_options <- paste0("NG_distribution_sector_total_",
                            apply(expand.grid(c("ACES","Vulcan"),
                                              c("byLDC","bystate","bydomain")),
                                  MARGIN = 1,FUN=function(x){paste0(x,collapse="_")}))
  Wastewater_options <- apply(expand.grid(c("CWNS","DMR"),
                                          c("GHGI","Moore"),
                                          c("state","national")),
                              MARGIN = 1,FUN=function(x){paste0(x,collapse="_")})
  
  #note here the .* for regular expression searching later
  stat_comb_options <- paste0("Stationary_combustion_sector_.*",
                              apply(expand.grid(c("ACES","Vulcan"),
                                                c("bystate","bydomain")),
                                    MARGIN = 1,FUN=function(x){paste0(x,collapse="_")}))
  
  #1 character vector with all options that can be referenced
  all_options <- ls(pattern="options")
  ################################################################################
  #look at the output that actually exists and filter out options that weren't
  #used
  
  output_files <- list.files(output_directory,pattern="*.nc")
  
  for(A in 1:length(all_options)){
    subset_options <- local_get(all_options[A])
    #this any(grepl()) subset keeps only those that are in the output files list
    subset_options <- subset_options[sapply(subset_options,function(x){any(grepl(x,output_files))})]
    #update the variables
    assign(all_options[A],subset_options)
    
    #grep here replaces the values with the exact filename instead, save
    #separately
    subset_options <- sapply(subset_options,function(x){output_files[grep(x,output_files)]})
    assign(paste0(all_options[A],"_filenames"),subset_options)
  }
  
  ################################################################################
  #load in all of the options - will pull from these to get the unique
  #combinations
  
  #these don't vary, we will always use them
  set_rast <- terra::rast(file.path(output_directory,set_output))
  
  #any variations that were run
  all_filename_options <- ls(pattern="options_filenames")
  variable_rast <- terra::rast(file.path(output_directory,unlist(sapply(all_filename_options,local_get))))
  ################################################################################
  #prepare lists for the thermogenic and nonthermogenic if that option was set
  
  if(separate_thermo){
    nonthermo_options <- unlist(sapply(all_filename_options[!all_filename_options %in% "NG_dist_options_filenames"],local_get))
    thermo_options <- NG_dist_options_filenames
    
    thermo_options <- c(thermo_options,nonthermo_options[grepl("fossil_fuel",nonthermo_options)])
    nonthermo_options <- nonthermo_options[!grepl("fossil_fuel",nonthermo_options)]
  }
  
  ################################################################################
  #for naming later, adjust these slightly
  
  NG_dist_options <- gsub("NG_distribution_sector_total_","",NG_dist_options)
  stat_comb_options <- gsub("\\.\\*","",gsub("Stationary_combustion_sector_","",stat_comb_options))
  
  #since the wood and fossil fuel are 2 files for 1 variant, this will be a
  #matrix if there are more than 2.  Need to combine into a single entry for
  #later
  if(class(stat_comb_options_filenames)[1]=="matrix"){
    stat_comb_options_filenames <- apply(stat_comb_options_filenames,2,FUN=function(x){paste0(x,collapse = ",")})
  }
  
  #wetcharts can be multiple files per type if more than 1 wetcharts model
  #subset was set.  These are each unique variations, so just unlist.  Dealing
  #with the subset number here in this way simplifies things compared to
  #alternatives (e.g., save as input to this function)
  if(class(Wetland_options_filenames)=="list"){
    Wetland_options_filenames <- unlist(Wetland_options_filenames)
    Wetland_options <- gsub("Wetland_sector_total_","",
                            gsub(".nc","",Wetland_options_filenames))
  }
  
  ################################################################################
  #combine into total inventories
  
  #expand.grid does the heavy lifting - run separately for the filenames and
  #variation descriptions - those are just for a key in excel
  Possible_combinations <- expand.grid(lapply(all_options,local_get),stringsAsFactors = F)
  Possible_combination_filenames <- expand.grid(lapply(all_filename_options,local_get),stringsAsFactors = F)
  
  all_combinations_rast <- domain_template
  for(A in 1:nrow(Possible_combinations)){
    #For the each unique combination, identify the relevant variable_rast
    #layers.  The as.vector... is simply to split stationary combustion into 2
    #files since they were 1 entry before.
    indx <- basename(terra::sources(variable_rast)) %in% as.vector(unlist(strsplit((unlist(Possible_combination_filenames[A,])),",")))
    
    #sum across sectors, include the sectors that don't have options, save
    out_rast <- sum(c(variable_rast[[indx]],set_rast))
    if(verbose){
      all_combinations_rast <- sum(all_combinations_rast,out_rast)
    }
    
    writeCDF_no_newline(out_rast,file.path(Combined_output_directory,
                                           paste0("Combined_inventory_combination_",
                                                  sprintf(paste0("%0",nchar(nrow(Possible_combination_filenames)),"d"),A),
                                                  ".nc")),overwrite=T)
    cat("\rFinished creating unique inventory",A,"of",nrow(Possible_combinations),"   ")
    out_rast <- NULL;gc()
  }
  
  ################################################################################
  #create and save a key
  
  #add useful column names and inventory numbers
  Possible_combinations <- cbind(1:nrow(Possible_combinations),Possible_combinations)
  colnames(Possible_combinations) <- c("Inventory_Number",gsub("NG_dist","Natural_Gas_Distribution",
                                                               gsub("stat_comb","Stationary_Combustion",all_options)))
  utils::write.csv(Possible_combinations,file = file.path(Combined_output_directory,"Combined_inventory_key.csv"),
                   quote = F,row.names = F)
  
  
  ################################################################################
  #repeat for thermogenic and non-thermogenic if the option was set
  
  if(separate_thermo){
    #save these in their own folders
    thermo_output_directory <- file.path(Combined_output_directory,"thermogenic")
    nonthermo_output_directory <- file.path(Combined_output_directory,"non_thermogenic")
    dir.create(thermo_output_directory,showWarnings = F)
    dir.create(nonthermo_output_directory,showWarnings = F)
    
    for(A in 1:nrow(Possible_combinations)){
      #all files in the first unique combination
      filename_subset <- as.vector(unlist(strsplit((unlist(Possible_combination_filenames[A,])),",")))
      
      #pull only the thermo ones, otherwise process in the same way.  Note
      #set_rast is subset by index.
      thermo_files <- filename_subset[filename_subset %in% thermo_options]
      indx <- basename(terra::sources(variable_rast)) %in% thermo_files
      thermo_rast <- sum(c(variable_rast[[indx]],set_rast[[3:4]]))
      writeCDF_no_newline(thermo_rast,file.path(thermo_output_directory,
                                                paste0("Thermogenic_combined_inventory_combination_",
                                                       sprintf(paste0("%0",nchar(nrow(Possible_combination_filenames)),"d"),A),
                                                       ".nc")),overwrite=T)
      
      nonthermo_files <- filename_subset[filename_subset %in% nonthermo_options]
      indx <- basename(terra::sources(variable_rast)) %in% nonthermo_files
      nonthermo_rast <- sum(c(variable_rast[[indx]],set_rast[[1:2]]))
      writeCDF_no_newline(nonthermo_rast,file.path(nonthermo_output_directory,
                                                   paste0("Non_thermogenic_combined_inventory_combination_",
                                                          sprintf(paste0("%0",nchar(nrow(Possible_combination_filenames)),"d"),A),
                                                          ".nc")),overwrite=T)
      
      #sanity check - all sectors should be either thermogenic or
      #non-thermogenic
      if(length(filename_subset)!=sum(length(nonthermo_files),length(thermo_files))){
        stop("Splitting between thermogenic and nonthermogenic sectors encountered an error")
      }
      
      cat("\rFinished creating unique thermogenic and non-thermogenic inventory",A,"of",nrow(Possible_combinations),"   ")
    }
  }
  ################################################################################
  #plot average across all combinations
  
  if(verbose){
    Summed_final_inventory <- all_combinations_rast/nrow(Possible_combinations)
    log_plot(Summed_final_inventory,
             "Final Inventory -\nAveraged across all variations\nSaturated low end",
             plot_directory=plot_directory,
             domain=domain,County_Tigerlines=County_Tigerlines,
             zlim_min=-4,
             State_CB=State_CB)
    
  }
  cat("\nFinished the process of combining emissions across all sectors: Combine_inventories in",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes")
}
