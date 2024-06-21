#'@title Disaggregate emissions to the pixel scale using ACES or Vulcan CO2
#'  inventories
#'
#'@description This is a function utilized by `stationary_combustion` and
#'  `NG_distribution`.  It distributes emissions from a larger scale (state or
#'  county) to the pixel scale using gridded CO2 emissions.
#'
#'@details This function is intended to be used by `stationary_combustion` and
#'  `NG_distribution`.  For each pixel it calculates the ratio of CO2 from that
#'  pixel to the state/county it is in.  This is combined with the state/county
#'  total CH4 emissions to disaggregate the emissions to the pixel-scale.  If
#'  there are no emissions for the state/county then the entire area is assigned
#'  an equal fraction of emissions.  This is done using sectoral (residential,
#'  commercial, industrial, electric) CO2 inventories.
#'
#'  ACES is available at \url{https://doi.org/10.3334/ORNLDAAC/1943} and Vulcan
#'  is available at \url{https://doi.org/10.3334/ORNLDAAC/1741}.
#'
#'  See references \href{https://doi.org/10.1029/2020JD032974}{Vulcan} and,
#'  \href{https://doi.org/10.1002/2017JD027359}{ACES}
#'@param input_inventory SpatRaster.  Either a Vulcan or ACES sectoral
#'  inventory.  ACES is available at \url{https://doi.org/10.3334/ORNLDAAC/1943}
#'  and Vulcan is available at \url{https://doi.org/10.3334/ORNLDAAC/1741}.
#'  Annual mean files should be used for Vulcan.  The hourly ACES file should be
#'  averaged across hours to create an annually averaged inventory. Code to do
#'  this on a linux-based HPC system is available as the script
#'  "Annualize_ACES_seawulf.R" and the accompanying batch script
#'  "Annualize_ACES.sh".
#'@param totals Character vector.  Various subsectors to run through the
#'  disaggregation process.
#'@param agg_level Character.  The scale of the data before disaggregation,
#'  solely used for naming.
#'@param NEI_input SpatVector.  Includes attributes for each spatial feature
#'  that detail the emissions at the current agg_level separated by totals.
#'@param cover_all List of matrices.  Each entry in the list is a separate
#'  state/county and the matrices are the output of the function
#'  terra::extract(weights=T,exact=T,cells=T).  These provide the fraction of
#'  each pixel contained within the state/county.  This allows for better
#'  accounting for pixels that are on the borders of states/counties.
#'@param out_envir Environment.  Where to assign output data.
#'@returns Nothing is returned.  The disaggregated methane data is assigned to
#'  the specified out_envir.  This will be a SpatRaster list with one for each
#'  of the totals.
#'@author Joe Pitt, \email{madeup@@wisc.edu}
#'@author Kris Hajny, \email{blank@@fake.edu}
#'@author Israel Lopez-Coto, \email{test@@test.edu}
#'@reference \href{https://doi.org/10.1029/2020JD032974}{Vulcan}
#'@reference \href{https://doi.org/10.1002/2017JD027359}{ACES}
#'@examples
#' aces_res <- rast(paste0(ACES_directory,"/Sectoral/",ACES_year,'_Annual_ACES_Residential.nc'))
#' res_totals <- c('mains_ER_total_res',
#'                 'serv_ER_total_res',
#'                 'MnR_ER_total_res',
#'                 'meter_ER_total_res',
#'                 'upset_ER_total_res',
#'                 'post_meter_ER_total_res')
#' cover_all <- list(extract(aces_res,all_merge_LCC_domain,weights=T,exact=T,cells=T))
#' disaggregation(aces_res,
#'                res_totals,
#'                agg_level="state",
#'                NEI_input=all_merge_LCC_state,
#'                cover_all,
#'                out_envir=environment())
#'@export


#Write a function to disaggregate total emissions using ACES/Vulcan or both
#within sub domain bounds (states, Local Distribution Companies, Counties)

disaggregation <- function(input_inventory,totals,agg_level,NEI_input,cover_all,out_envir){
  input_name <- deparse(substitute(input_inventory))
  #pull the input name (aces_res/com or vu_res/com) for naming later.  Note -
  #this must be done before input_inventory is edited
  #(https://stackoverflow.com/a/23563587)
  
  input_inventory[is.na(input_inventory)] <- 0
  # Change nans to zeros otherwise they could mess with the regridding later
  
  template <- input_inventory
  template[] <- 0
  
  input_inventory_ch4 <- replicate(length(totals), template)
  names(input_inventory_ch4) <- totals
  # Set up lists of rasters for calculating ch4 emissions, with one raster for
  # each subsector
  
  for(i in 1:length(cover_all)){
    cover <- cover_all[[i]]
    
    input_inventory_temp <- template
    input_inventory_temp[cover[,'cell']] <- input_inventory[cover[,'cell']]*cover[,'weight']
    # Starting with a raster of zeros, set cells within polygon i equal to their cover-weighted CO2 emission
    
    if(global(input_inventory_temp,sum) == 0){
      input_inventory_temp[cover[,'cell']] <- cover[,'weight']
      input_inventory_frac <- input_inventory_temp/unlist(global(input_inventory_temp, sum))
      #if there are no inventory emissions in this polygon, set the entire
      #polygon to 1 (i.e., equally distribute emissions across the polygon)
    }else{
      input_inventory_frac <- input_inventory_temp/unlist(global(input_inventory_temp, sum))
      # Calculate the fraction of the polygon-total CO2 emissions within each cell
    }
    
    for(total in totals){
      new_addition <- as.numeric(as.data.frame(NEI_input[i,total]))
      if(!is.na(new_addition)){
        input_inventory_ch4[[total]] <- input_inventory_ch4[[total]] + input_inventory_frac*new_addition
      }
    }
    # Loop through the different subsectors, and add the CH4 emissions map to
    # the relevant raster
    
    cat("\rFinished mapping",input_name,agg_level,"level entry",i,"of",length(cover_all),"        ")
  }#cover loop
  assign(x=paste0(input_name,"_ch4_by",agg_level),input_inventory_ch4,envir = out_envir)
  #save this output to the global environment, as it'll be needed later.
  #E.g., aces_res_ch4_state or vu_com_ch4_domain
  
}#function
