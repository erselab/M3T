#' Wrapper around terra's writeCDF to prevent newline.
#'
#' @param input_raster SpatRaster.
#'
#' @description `writeCDF_no_newline` runs terra's writeCDF in a way that avoids
#'   outputting a new line after running.  Improves flow of periodic user
#'   updates within the package.
#'
#' @export
#'
#' @examples
#'
#' f <- system.file("ex/elev.tif", package = "terra")
#' r <- rast(f)
#' fname <- paste0(tempfile(), ".nc")
#' rr <- writeCDF(r, fname,
#'   overwrite = TRUE, varname = "alt",
#'   longname = "elevation in m above sea level", unit = "m"
#' )
writeCDF_no_newline <- function(input_raster,...) {
  invisible(utils::capture.output(terra::writeCDF(input_raster,...)))
}





#' Helper function for downloading data
#'
#' @param URL Character. The URL for the data
#' @param output_location Character.  The path and filename for the data to be
#'   saved to, if it is being saved.
#' @param method Character.  One of save, JSON, or vect.  Save will save it as
#'   a file using output_location, JSON will direct load it into R using the
#'   jsonlite package, and vect will directly load it into R using terra's vect
#'   function (points or polygons, e.g., .shp or .gpkg)
#' @param error_message Character.  Message to output should download fail.

#'
#' @details This wrapper will use the proper function to download the data and
#'   uses a repeat and trycatch loop in case there is poor internet
#'   connectivity.  It will attempt the download up to 5 times before failing.
#'   Includes user updates throughout as well.
#'
#' @export
#'
#' @examples
#'
#' data_URL <- "https://www2.census.gov/geo/tiger/TIGER2022/STATE/2022/tl_2022_us_state20.zip"
#' out_file <- tempfile(fileext = ".zip")
#' Trycatch_downloader(
#'   URL = data_URL, output_location = out_file, method = "save",
#'   error_message = paste("Census tigerlines could not be downloaded using link:", data_URL)
#' )
# Based on https://stackoverflow.com/a/60880960
Trycatch_downloader <- function(URL, output_location = NULL, method, error_message = "") {
  counter <- 0

  # user update - as some downloads can take a while
  cat("Attempting to download", URL, "at", format(Sys.time(), "%H:%M:%S"), "   ...")

  repeat{
    counter <- counter + 1
    if (counter > 1) {
      cat("\n", URL, "Download failed, retrying up to 5x")
    }
    info <- tryCatch(
      # save to file
      if (method == "save") {
        utils::download.file(URL, destfile = output_location, quiet = T, method = "curl")
        # load in as JSON
      } else if (method == "JSON") {
        jsonlite::fromJSON(URL)
        # load in as SpatVector
      } else if (method == "vect") {
        terra::vect(URL)
      },

      # download failed, try again with 2 second delay
      warning = function(w) {
        Sys.sleep(2)
        NA
      },
      error = function(e) {
        Sys.sleep(2)
        NA
      }
    )

    # blank out the "attempting to download" line from the console
    cat("\r", rep(" ", 1000), "\r")

    # return the data for method = vect or json
    if (!all(is.na(info)) | length(info) > 1) {
      if (method != "save") {
        return(info)
      }
      break
    }

    # download failed repeatedly, stop function
    if (counter >= 5) {
      stop(error_message)
    }
  }
}



#' Helper function for making GHGRP data consistent across sectors
#'

#' @param input Dataframe. A GHGRP dataframe built from a "subpart information"
#'   table such as
#'   \url{https://enviro.epa.gov/envirofacts/metadata/table/ghg/hh_subpart_level_information}
#' @details This function will change "ghg_gas_name" to "ghg_name" and
#'   "reporting_year" to "year" as these column names are not consistent across
#'   tables.  It will also force the "ghg_name" and "facility_name" data to be
#'   lower case and remove any data for a ghg besides methane.
#' @examples
#' GHGRP_combustion <- data.frame("facility_id"=c(1,2,3,3),
#'                                "facility_name"=c("ONE","TWO","THREE","FOUR"),
#'                                "ghg_gas_name"=c(rep("METHANE",3),"CARBON DIOXIDE"),
#'                                "ghg_quantity"=c(rep(100,3),500),
#'                                "reporting_year"=rep(2015,4))
#' make_consistent(GHGRP_combustion)
#' @inherit CH4_inventory_build author
#' @export

make_consistent <- function(input){
  colnames(input) <- gsub("ghg_gas_name","ghg_name",colnames(input))
  colnames(input) <- gsub("reporting_year","year",colnames(input))
  input$ghg_name <- tolower(input$ghg_name)
  input$facility_name <- tolower(input$facility_name)
  input <- input[input$ghg_name=="methane",]
  return(input)
}
