#'@title Interactively define the domain for 'CH4_inventory_build'
#'
#'@description This function uses functions from the terra package to allow a
#'  user to build a domain area using a state map of the continental US.
#'
#'@details The polygon is saved for potential reuse and reference.  Be aware
#'  that nonsensical polygons can be built (e.g., those with holes, odd shapes,
#'  etc.) so the user check at the end is quite important to make sure this
#'  polygon is useful.
#'
#'@param input_directory Character providing the full filepath to save/load
#'  input data.  Automatically defined in 'CH4_inventory_build'.
#'@param State_CB SpatVector. US Census Cartographic Boundary files for
#'  visualization
#'  \url{https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html}.
#'
#'@returns SpatVector
#'
#'@inherit CH4_inventory_build author
#'
#'@examples
#'define_custom_domain("~/../Desktop/CONUS_2020_MMMT_run/in")
#'
#'@export
#'
#'@seealso [CH4_inventory_build()] Calculates methane inventory using settings
#'  provided in config.

define_custom_domain <- function(input_directory,State_CB){

  #user update, new window for plotting (terra documentation recommends this for
  #interactive functions like the below)
  cat("See new plot window\n")
  grDevices::dev.new(noRStudioGD = TRUE)
  
  #state map visual, CONUS extent (including offshore emissions)
  terra::plot(State_CB,main="State Map\nSelect topleft/bottomright corners to zoom into a region",
              xlim=c(-130,-60),ylim=c(20,55),col="antiquewhite",background="aliceblue",buffer=F)
  terra::text(State_CB,"stusps")
  
  #zoom as many times as needed
  repeat{
    cat("Select topleft/bottomright corners to zoom into a region")
    suppressWarnings(terra::zoom(State_CB,main="State Map",col="antiquewhite",background="aliceblue",buffer=F))
    terra::text(State_CB,"stusps")
    input <- readline("Zoom more (yes/no)?")
    if(input=="no"){
      break
    }
  }
  cat("Draw domain by selecting points along an outline, hit stop in the topleft when done - a line to the start point will be added to close the polygon.")
  
  #user interactively draws the domain
  domain <- suppressMessages(terra::draw("polygon"))
  
  #plot and confirm it is correct before further processsing - stop run if not
  terra::plot(domain,add=T,col="red")
  input <- readline("Carefully review - does this look as desired (yes/no)?")
  if(input=="no"){
    invisible(dev.off())
    stop("incorrectly drawn domain, stopping run")
  }
  
  #close new window of domain plot
  invisible(dev.off())
  
  crs(domain) <- crs(State_CB)
  
  #save to input directory for future reference
  terra::writeVector(domain,file.path(input_directory,"custom_domain.gpkg"),overwrite=T)
  return(domain)
}



