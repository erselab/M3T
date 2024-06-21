#'@title Prepare input data for a logarithmic plot
#'
#'@description This is a simple helper function for ease of use.  It just takes
#'  the log10 of the data and sets infinite values to NA.
#'
#'@details This function is intended to be used when verbose = TRUE for
#'  CH4_inventory_build.  It is called by log_plot to help build plots for each
#'  individual sector/subsector.  Some of these, particular those for point
#'  sources, are more useful on a log scale.
#'@param input SpatRaster.  Intended to be a SpatRaster of gridded methane
#'  emissions.
#'@returns The log-scaled input data is returned, after removing infinite
#'  values.
#'@author Joe Pitt, \email{madeup@@wisc.edu}
#'@author Kris Hajny, \email{blank@@fake.edu}
#'@author Israel Lopez-Coto, \email{test@@test.edu}
#'@examples
#'prep_plot_data(central_flux)
#'@export
#'


#build functions to plot up most sectors as they finish running.  

#take log and remove 0 or negative values
prep_plot_data <- function(input){
  output <- input
  output <- log10(output)
  output[is.infinite(output)] <- NA
  return(output)
}

#'@title Create a log10 visual
#'
#'@description This is a simple helper function for ease of use.  It runs
#'  prep_plot_data and builds useful map visuals of the input data.
#'
#'@details This function is intended to be used when verbose = TRUE for
#'  CH4_inventory_build.  It allows for reasonable default scales/titles or set
#'  ones, e.g., if wanting to hold the colorscale constant across multiple
#'  similar sectors/subsectors.
#'@param input SpatRaster.  Intended to be a SpatRaster of gridded methane
#'  emissions.
#'@param title \strong{Optional} character providing the main title for the
#'  plot.  The plot will have no main title if not provided.
#'@param zlim_min \strong{Optional} numeric providing the minimum value for the
#'  colorscale of the plot, representing the log-scale gridded methane in
#'  nmol/m2/s.  Will default to the minimum of the data if not provided.
#'@param zlim_max \strong{Optional} numeric equivalent to zlim_min, but for the
#'  maximum value.
#'@param filename \strong{Optional} character providing the output filename for
#'  the plot.  Will default to a png with the name of the input if not provided,
#'  unless the plot is for an entire sector, rather than subsector.  This will
#'  use the same default filename, but place it in a separate subfolder.
#'@returns This function returns nothing, but does produce a plot with the
#'  gridded methane data colored on a log scale with NA values set to black. All
#'  axes are clearly labeled and state, county, and, if relevant, focus city
#'  boundaries are overlaid in greys and white.
#'@author Joe Pitt, \email{madeup@@wisc.edu}
#'@author Kris Hajny, \email{blank@@fake.edu}
#'@author Israel Lopez-Coto, \email{test@@test.edu}
#'@examples
#'log_plot(central_flux,filename="Wastewater_dom_central",
#'title="Domestic Wastewater -\n EPA total distributed using \nClean Watersheds Needs Survey")
#'@export

#plot for log scale
log_plot <- function(input,title,zlim_min=NULL,zlim_max=NULL,
                     filename){
  
  #set filename to the proper path and use input data as filename if none was
  #provided
  if(missing(filename)){
    #save to a separate folder if the input is a summed_sector
    if(grepl(pattern="Summed",x=substitute(input))){
      outputname <- paste0(plot_directory,"Summed_Sectors/",substitute(input))
    }else{
      outputname <- paste0(plot_directory,substitute(input))
    }
  }else{
    outputname <- paste0(plot_directory,filename)
  }
  
  input <- prep_plot_data(input)
  
  png(paste0(outputname,".png"),width = 480*2,height=480*2)
  plot(input,mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,1),
       # col=timPalette(),
       colNA="black",
       main=title,
       plg=list(cex=2,title="log10(nmol/m2/s)",title.cex=2),
       pax=list(cex.axis=2),
       xlab="Longitude",ylab="Latitude",
       cex.main=2,cex.axis=2,cex.lab=2,
       zlim=c(zlim_min,zlim_max))
  plot(County_Tigerlines,add=T,border="dimgrey",col=NA)
  plot(State_Tigerlines,add=T,border="white",lwd=2,col=NA)
  if(class(focus_city_tigerlines)=="SpatVector"){
    plot(focus_city_tigerlines,add=T,border="darkgrey",col=NA)
  }
  dev.off()
  
}

#'@title Create a visual
#'
#'@description This is a simple helper function for ease of use.  It builds
#'  useful map visuals of the input data.
#'
#'@details This function is intended to be used when verbose = TRUE for
#'  CH4_inventory_build.  It allows for reasonable default scales/titles or set
#'  ones, e.g., if wanting to hold the colorscale constant across multiple
#'  similar sectors/subsectors.
#'@param input SpatRaster.  Intended to be a SpatRaster of gridded methane
#'  emissions.
#'@param title \strong{Optional} character providing the main title for the
#'  plot.  The plot will have no main title if not provided.
#'@param zlim_min \strong{Optional} numeric providing the minimum value for the
#'  colorscale of the plot, representing the gridded methane in nmol/m2/s.  Will
#'  default to the minimum of the data if not provided.
#'@param zlim_max \strong{Optional} numeric equivalent to zlim_min, but for the
#'  maximum value.
#'@param filename \strong{Optional} character providing the output filename for
#'  the plot.  Will default to a png with the name of the input if not provided,
#'  unless the plot is for an entire sector, rather than subsector.  This will
#'  use the same default filename, but place it in a separate subfolder.
#'@returns This function returns nothing, but does produce a plot with the
#'  gridded methane data colored with NA values set to black. All axes are
#'  clearly labeled and state, county, and, if relevant, focus city boundaries
#'  are overlaid in greys and white.
#'@author Joe Pitt, \email{madeup@@wisc.edu}
#'@author Kris Hajny, \email{blank@@fake.edu}
#'@author Israel Lopez-Coto, \email{test@@test.edu}
#'@examples
#' not_log_plot(septic_flux,filename="Wastewater_dom_septic_national",
#'              "Domestic Wastewater - Septic\n national EPA septic distributed using \ndeveloped open space/low intensity land cover",
#'              global(min(septic_flux,septic_flux2),min),
#'              global(max(septic_flux,septic_flux2),max))
#'@export

#plot for linear scale - mostly identical
not_log_plot <- function(input,title,zlim_min=NULL,zlim_max=NULL,
                         filename){
  if(missing(filename)){
    if(grepl(pattern="Summed",x=substitute(input))){
      outputname <- paste0(plot_directory,"Summed_Sectors/",substitute(input))
    }else{
      outputname <- paste0(plot_directory,substitute(input))
    }
  }else{
    outputname <- paste0(plot_directory,filename)
  }
  
  #Here just set 0 values to NA so that colNA applies
  input[values(input)==0] <- NA
  
  png(paste0(outputname,".png"),width = 480*2,height=480*2)
  par(mar=c(5, 4, 4, 2) + 0.1 + c(0,1,2,1))
  plot(input,mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,1),
       # col=timPalette(),
       colNA="black",
       main=title,
       plg=list(cex=2,title="nmol/m2/s",title.cex=2),
       pax=list(cex.axis=2),
       xlab="Longitude",ylab="Latitude",
       cex.main=2,cex.axis=2,cex.lab=2,
       zlim=c(zlim_min,zlim_max))
  plot(County_Tigerlines,add=T,border="dimgrey",col=NA)
  plot(State_Tigerlines,add=T,border="white",lwd=2,col=NA)
  if(class(focus_city_tigerlines)=="SpatVector"){
    plot(focus_city_tigerlines,add=T,border="darkgrey",col=NA)
  }
  dev.off()
}

