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
#'@inheritParams Municipal_solid_waste
#'
#'@param Separate_thermo Logical.  Pulled from \code{\link{M3T_config}}.
#'@param verbose Logical indicating whether to save visuals. This is a single
#'  plot of the average gridded methane emissions across all variations on a log
#'  scale
#'@returns Nothing is returned from the function, but the main outputs are many
#'  netcdf files of the methane emissions across all sectors.  Given the large
#'  number of possible files and variations, they are titled
#'  "Combined_inventory_combination_#.nc" with # increasing numerically.  A csv
#'  titled "Combined_inventory_key.csv" is also saved that details what
#'  variations were used for each sector for each inventory.
#'
#'@inherit Municipal_solid_waste seealso
#'@keywords internal

#@examples
#library(terra)
#Combine_inventories <- function(output_directory="~/../Desktop/out/",
#                                Separate_thermo=T)

Combine_inventories <- function(output_directory,
                                Separate_thermo,
                                Create_summary_combinations,
                                Create_individual_combinations,
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
  Summary_combination_output_directory <- file.path(Combined_output_directory,"summary_combinations")
  dir.create(Summary_combination_output_directory,showWarnings = F)
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
  all_options <- ls(pattern="options$")
  
  ################################################################################
  #these are assigned in the below section, but R doesn't see them being created
  #explicitly, so do so here just to make usethis::check() happy for package
  #building.
  Landfill_options_filenames <- NG_dist_options_filenames <- 
    stat_comb_options_filenames <- Wastewater_options_filenames <- 
    Wetland_options_filenames <- NULL
  
  ################################################################################
  #look at the output that actually exists and filter out options that weren't
  #used
  
  output_files <- list.files(output_directory,pattern="*.nc")
  
  set_output <- set_output[set_output %in% output_files]
  
  for(A in 1:length(all_options)){
    subset_options <- local_get(all_options[A])
    
    #this any(grepl()) subset keeps only those that are in the output files list
    subset_options <- subset_options[sapply(subset_options,function(x){any(grepl(x,output_files))})]
    if(length(subset_options)==0){
      #remove the "filenames" variable, remove the "options" variable, and remove it from all options
      rm(list = paste0(all_options[A],"_filenames"))
      rm(list=all_options[A])
      all_options[A] <- NA
      next
    }else{
      #update the variables
      assign(all_options[A],subset_options)
      
      #grep here replaces the values with the exact filename instead, save
      #separately
      subset_options <- sapply(subset_options,function(x){output_files[grep(x,output_files)]})
      assign(paste0(all_options[A],"_filenames"),subset_options)
    }
  }
  
  all_options <- all_options[!is.na(all_options)]
  ################################################################################
  #load in all of the options - will pull from these to get the unique
  #combinations
  
  #these don't vary
  set_rast <- terra::rast(file.path(output_directory,set_output))
  
  #any variations that were run
  all_filename_options <- ls(pattern="options_filenames")
  variable_rast <- terra::rast(file.path(output_directory,unlist(sapply(all_filename_options,local_get))))
  ################################################################################
  #prepare lists for the thermogenic and nonthermogenic if that option was set
  
  if(Separate_thermo){
    nonthermo_options <- unlist(sapply(all_filename_options[!all_filename_options %in% "NG_dist_options_filenames"],local_get))
    thermo_options <- NG_dist_options_filenames
    
    thermo_options <- c(thermo_options,nonthermo_options[grepl("fossil_fuel",nonthermo_options)])
    nonthermo_options <- nonthermo_options[!grepl("fossil_fuel",nonthermo_options)]
  }
  
  ################################################################################
  #for naming later, adjust these slightly
  
  if(exists("NG_dist_options")){
    NG_dist_options <- gsub("NG_distribution_sector_total_","",NG_dist_options)
  }
  if(exists("stat_comb_options")){
    stat_comb_options <- gsub("\\.\\*","",gsub("Stationary_combustion_sector_","",stat_comb_options))
  }
  
  #since the wood and fossil fuel are 2 files for 1 variant, this will be a
  #matrix if there are more than 2.  Need to combine into a single entry for
  #later
  if(exists("stat_comb_options_filenames")){
    if(isa(stat_comb_options_filenames[1],"matrix")){
      stat_comb_options_filenames <- apply(stat_comb_options_filenames,2,FUN=function(x){paste0(x,collapse = ",")})
    }
  }
  
  #wetcharts can be multiple files per type if more than 1 wetcharts model
  #subset was set.  These are each unique variations, so just unlist.  Dealing
  #with the subset number here in this way simplifies things compared to
  #alternatives (e.g., save as input to this function)
  if(exists("Wetland_options_filenames")){
    if(isa(Wetland_options_filenames,"list")){
      Wetland_options_filenames <- unlist(Wetland_options_filenames)
      Wetland_options <- gsub("Wetland_sector_total_","",
                              gsub(".nc","",Wetland_options_filenames))
    }
  }
  
  ################################################################################
  #combine into total inventories
  
  list_all_options <- lapply(all_options,local_get)
  list_all_filename_options <- lapply(all_filename_options,local_get)
  
  #expand.grid does the heavy lifting - run separately for the filenames and
  #variation descriptions - those are just for a key in excel
  Possible_combinations <- expand.grid(list_all_options,stringsAsFactors = F)
  Possible_combination_filenames <- expand.grid(list_all_filename_options,stringsAsFactors = F)
  
  
  
  #summary versions only - substantially faster
  if(Create_summary_combinations){
    summary_combinations_rast <- terra::rast(set_rast,nlyrs=3,vals=NA)
    for(A in 1:3){summary_combinations_rast[[A]]=sum(set_rast,na.rm=T)}
    names(summary_combinations_rast) <- c("min","mean","max")
    
    for(A in 1:length(list_all_options)){
      #For each sector, identify the relevant variable_rast layers.
      #The as.vector... is simply to split stationary combustion into 2 files
      #since they were 1 entry before.
      indx <- basename(terra::sources(variable_rast)) %in% as.vector(unlist(strsplit((list_all_filename_options[[A]]),",")))
      sub_rast <- variable_rast[[indx]]
      
      #calculate mean, max, and min across combinations
      summary_combinations_rast$mean <- sum(c(summary_combinations_rast$mean,terra::mean(sub_rast,na.rm=T)),na.rm=T)
      summary_combinations_rast$max <- sum(c(summary_combinations_rast$max,max(sub_rast,na.rm=T)),na.rm=T)
      summary_combinations_rast$min <- sum(c(summary_combinations_rast$min,min(sub_rast,na.rm=T)),na.rm=T)
      cat("\rFinished adding sector",A,"of",length(list_all_options),"to the summary combinations        ")
      rm(sub_rast);gc()
    }
    
    writeCDF_no_newline(summary_combinations_rast,
                        file.path(Summary_combination_output_directory,"Summary_combination_inventories.nc"),
                        force_v4=TRUE,
                        varname='methane_emissions',
                        unit='nmol/m2/s',
                        longname="The sum of the min, mean, max for each sector across all variations considered",
                        missval=-9999,
                        overwrite=TRUE)
    rm(indx);gc()
  }
  
  
  
  #all unique combinations
  if(Create_individual_combinations){
    for(A in 1:nrow(Possible_combinations)){
      #For each unique combination, identify the relevant variable_rast layers.
      #The as.vector... is simply to split stationary combustion into 2 files
      #since they were 1 entry before.
      indx <- basename(terra::sources(variable_rast)) %in% as.vector(unlist(strsplit((unlist(Possible_combination_filenames[A,])),",")))
      
      #sum across sectors, include the sectors that don't have options, save
      out_rast <- sum(c(variable_rast[[indx]],set_rast),na.rm=T)
      
      writeCDF_no_newline(out_rast,file.path(Combined_output_directory,
                                             paste0("Combined_inventory_combination_",
                                                    sprintf(paste0("%0",nchar(nrow(Possible_combination_filenames)),"d"),A),
                                                    ".nc")),overwrite=T)
      cat("\rFinished creating unique inventory",A,"of",nrow(Possible_combinations),"   ")
      rm(out_rast);gc()
    }
  }
  
  ################################################################################
  #create and save a key
  
  if(Create_individual_combinations){
    #add useful column names and inventory numbers
    Possible_combinations <- cbind(1:nrow(Possible_combinations),Possible_combinations)
    colnames(Possible_combinations) <- c("Inventory_Number",gsub("NG_dist","Natural_Gas_Distribution",
                                                                 gsub("stat_comb","Stationary_Combustion",all_options)))
    for(A in 1:length(set_output)){
      Possible_combinations[,paste0("Nonvarying_sector_",A)] <- gsub(".nc","",set_output[A])
    }
    utils::write.csv(Possible_combinations,file = file.path(Combined_output_directory,"Combined_inventory_key.csv"),
                     quote = F,row.names = F)
  }
  
  ################################################################################
  #repeat for thermogenic and non-thermogenic if the option was set
  
  if(Separate_thermo){
    #save these in their own folders
    thermo_output_directory <- file.path(Combined_output_directory,"thermogenic")
    nonthermo_output_directory <- file.path(Combined_output_directory,"non_thermogenic")
    dir.create(thermo_output_directory,showWarnings = F)
    dir.create(nonthermo_output_directory,showWarnings = F)
    
    thermo_set_indx <- grep("GEPA_thermo.nc|NG_transmission_sector_total.nc",terra::sources(set_rast))
    non_thermo_set_indx <- grep("GEPA_non_thermo.nc|GEPA_ind_landfill.nc",terra::sources(set_rast))
    
    
    
    
    #summary combinations
    if(Create_summary_combinations){
      thermo_summary_combinations_rast <- terra::rast(domain_template,nlyrs=3)
      for(A in 1:3){thermo_summary_combinations_rast[[A]]=domain_template}
      names(thermo_summary_combinations_rast) <- c("min","mean","max")
      non_thermo_summary_combinations_rast <- thermo_summary_combinations_rast
      
      for(A in 1:length(list_all_options)){
        #For each sector, identify the relevant variable_rast layers.
        #The as.vector... is simply to split stationary combustion into 2 files
        #since they were 1 entry before.
        indx <- basename(terra::sources(variable_rast)) %in% as.vector(unlist(strsplit((list_all_filename_options[[A]]),",")))
        sub_rast <- variable_rast[[indx]]
        
        if(any(basename(terra::sources(sub_rast)) %in% thermo_options)){
          #calculate mean, max, and min across combinations
          thermo_summary_combinations_rast$mean <- sum(c(thermo_summary_combinations_rast$mean,terra::mean(sub_rast,na.rm=T)),na.rm=T)
          thermo_summary_combinations_rast$max <- sum(c(thermo_summary_combinations_rast$max,max(sub_rast,na.rm=T)),na.rm=T)
          thermo_summary_combinations_rast$min <- sum(c(thermo_summary_combinations_rast$min,min(sub_rast,na.rm=T)),na.rm=T)
        }else{
          non_thermo_summary_combinations_rast$mean <- sum(c(non_thermo_summary_combinations_rast$mean,terra::mean(sub_rast,na.rm=T)),na.rm=T)
          non_thermo_summary_combinations_rast$max <- sum(c(non_thermo_summary_combinations_rast$max,max(sub_rast,na.rm=T)),na.rm=T)
          non_thermo_summary_combinations_rast$min <- sum(c(non_thermo_summary_combinations_rast$min,min(sub_rast,na.rm=T)),na.rm=T)
        }
        
        cat("\rFinished adding sector",A,"of",length(list_all_options),"to the summary thermogenic/non-thermogenic combinations          ")
        rm(sub_rast);gc()
      }
      
      if(length(thermo_set_indx)!=0){
        for(A in 1:3){thermo_summary_combinations_rast[[A]]=sum(c(thermo_summary_combinations_rast[[A]],sum(set_rast[[thermo_set_indx]],na.rm=T)),na.rm=T)}
      }
      if(length(non_thermo_set_indx)!=0){
        for(A in 1:3){non_thermo_summary_combinations_rast[[A]]=sum(c(non_thermo_summary_combinations_rast[[A]],sum(set_rast[[non_thermo_set_indx]],na.rm=T)),na.rm=T)}
      }
      names(thermo_summary_combinations_rast) <- c("min","mean","max")
      names(non_thermo_summary_combinations_rast) <- c("min","mean","max")

      writeCDF_no_newline(thermo_summary_combinations_rast,
                          file.path(Summary_combination_output_directory,"Summary_combination_thermogenic_inventories.nc"),
                          force_v4=TRUE,
                          varname='methane_emissions',
                          unit='nmol/m2/s',
                          longname="The sum of the min, mean, max for each thermogenic sector across all variations considered",
                          missval=-9999,
                          overwrite=TRUE)
      writeCDF_no_newline(non_thermo_summary_combinations_rast,
                          file.path(Summary_combination_output_directory,"Summary_combination_non_thermogenic_inventories.nc"),
                          force_v4=TRUE,
                          varname='methane_emissions',
                          unit='nmol/m2/s',
                          longname="The sum of the min, mean, max for each non-thermogenic sector across all variations considered",
                          missval=-9999,
                          overwrite=TRUE)
      rm(indx);gc()
    }
    
    
    
    
    
    
    #individual combinations
    if(Create_individual_combinations){
      for(A in 1:nrow(Possible_combinations)){
        #all files in the first unique combination
        filename_subset <- as.vector(unlist(strsplit((unlist(Possible_combination_filenames[A,])),",")))
        
        #pull only the thermo ones, otherwise process in the same way.  Note
        #set_rast is subset by index.
        thermo_files <- filename_subset[filename_subset %in% thermo_options]
        indx <- basename(terra::sources(variable_rast)) %in% thermo_files
        if(length(thermo_set_indx)==0){
          thermo_rast <- sum(variable_rast[[indx]])
        }else{
          thermo_rast <- sum(c(variable_rast[[indx]],set_rast[[thermo_set_indx]]))
        }
        writeCDF_no_newline(thermo_rast,file.path(thermo_output_directory,
                                                  paste0("Thermogenic_combined_inventory_combination_",
                                                         sprintf(paste0("%0",nchar(nrow(Possible_combination_filenames)),"d"),A),
                                                         ".nc")),overwrite=T)
        
        
        
        nonthermo_files <- filename_subset[filename_subset %in% nonthermo_options]
        indx <- basename(terra::sources(variable_rast)) %in% nonthermo_files
        if(length(non_thermo_set_indx)==0){
          nonthermo_rast <- sum(variable_rast[[indx]])
        }else{
          nonthermo_rast <- sum(c(variable_rast[[indx]],set_rast[[non_thermo_set_indx]]))
        }
        writeCDF_no_newline(nonthermo_rast,file.path(nonthermo_output_directory,
                                                     paste0("Non_thermogenic_combined_inventory_combination_",
                                                            sprintf(paste0("%0",nchar(nrow(Possible_combination_filenames)),"d"),A),
                                                            ".nc")),overwrite=T)
        
        cat("\rFinished creating unique thermogenic and non-thermogenic inventory",A,"of",nrow(Possible_combinations),"        ")
      }
    }
  }
  ################################################################################
  #plot average across all combinations.  Name = sum instead of mean as this
  #triggers log_plot to save to the proper folder and it is sum across sectors,
  #average across variations.
  
  if(verbose & Create_summary_combinations){
    Summed_final_inventory <- summary_combinations_rast$mean
    log_plot(Summed_final_inventory,
             "Final Inventory -\nAveraged across all variations\nSaturated low end",
             plot_directory=plot_directory,
             domain=domain,County_Tigerlines=County_Tigerlines,
             zlim_min=-4,
             State_CB=State_CB)
    if(Separate_thermo){
      Summed_thermogenic_sources <- thermo_summary_combinations_rast$mean
      log_plot(Summed_thermogenic_sources,
               "Final Inventory thermogenic sources -\nAveraged across all variations\nSaturated low end",
               plot_directory=plot_directory,
               domain=domain,County_Tigerlines=County_Tigerlines,
               zlim_min=-4,
               State_CB=State_CB)
      Summed_non_thermogenic_sources <- non_thermo_summary_combinations_rast$mean
      log_plot(Summed_non_thermogenic_sources,
               "Final Inventory non-thermogenic sources-\nAveraged across all variations\nSaturated low end",
               plot_directory=plot_directory,
               domain=domain,County_Tigerlines=County_Tigerlines,
               zlim_min=-4,
               State_CB=State_CB)
    }
  }
  cat("\nFinished the process of combining emissions across all sectors: Combine_inventories at",format(Sys.time(),"%H:%M"),"with a total runtime of",round(difftime(Sys.time(),starttime,units = "min"),2),"minutes\n\n")
}


