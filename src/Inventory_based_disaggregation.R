#Write a function to disaggregate total emissions using ACES/Vulcan or both
#within sub domain bounds (states, Local Distribution Companies, Counties)

disaggregation <- function(input_inventory,totals,agg_level,sf_input){
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
  
  assign(x=gsub("res|com|ind|elec","template",input_name),template,envir = .GlobalEnv)
  #save the template to the global environment, as it may be needed later.
  
  if(max(do.call(rbind,cover_all)[,'weight']) > 0.01){
    stop('Check cellFromPolygon behaviour - this code assumes a bug that may now have been fixed')
  }
  # Add in a check to make sure the bug hasn't been fixed (if using a more recent version of raster)
  
  for(i in 1:length(cover_all)){
    cover <- cover_all[[i]]
    
    input_inventory_temp <- template
    input_inventory_temp[cover[,'cell']] <- input_inventory[cover[,'cell']]*cover[,'weight']*100
    # Starting with a raster of zeros, set cells within polygon i equal to their cover-weighted CO2 emission
    
    if(cellStats(input_inventory_temp, sum) == 0){
      input_inventory_temp[cover[,'cell']] <- cover[,'weight']*100
      input_inventory_frac <- input_inventory_temp/cellStats(input_inventory_temp, sum)
      #if there are no inventory emissions in this polygon, set the entire
      #polygon to 1 (i.e., equally distribute emissions across the polygon)
    }else{
      input_inventory_frac <- input_inventory_temp/cellStats(input_inventory_temp, sum)
      # Calculate the fraction of the polygon-total CO2 emission within each cell
    }
    
    for(total in totals){
      new_addition <- as.numeric(st_drop_geometry(sf_input[i, total]))
      if(!is.na(new_addition)){
        input_inventory_ch4[[total]][] <- input_inventory_ch4[[total]][] + input_inventory_frac[]*new_addition
      }
    }
    # Loop through the different subsectors, and add the CH4 emissions map to
    # the relevant raster Note that we need to use the sf object here, because
    # the merge function has changed the row order relative to all_merge_clean
    
    cat("\rFinished mapping",input_name,agg_level,"level entry",i,"of",length(cover_all),"        ")
  }#cover loop
  
  assign(x=paste0(input_name,"_ch4_by",agg_level),input_inventory_ch4,envir = .GlobalEnv)
  #save this output to the global environment, as it'll be needed later.
  #E.g., aces_res_ch4_state or vu_com_ch4_domain
  
}#function
