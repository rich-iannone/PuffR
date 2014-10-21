#' Add point sources to a list for later use in CALPUFF
#' @description Add point sources to a list for later use in CALPUFF
#' @param src_name
#' @param species_name
#' @param lat
#' @param lon
#' @param x_coord_km
#' @param y_coord_km
#' @param UTM_zone
#' @param stack_height
#' @param base_elev
#' @param stack_diam
#' @param exit_velocity
#' @param exit_temp
#' @param emission_rate
#' @param emission_units
#' @export calpuff_add_point_sources

calpuff_add_point_sources <- function(src_name,
                                      species_name,
                                      lat_dec_deg = NULL,
                                      lon_dec_deg = NULL,
                                      x_coord_km = NULL,
                                      y_coord_km = NULL,
                                      UTM_zone = NULL,
                                      stack_height,
                                      base_elev,
                                      stack_diam,
                                      exit_velocity,
                                      exit_temp,
                                      emission_rate,
                                      emission_units){
  
  # Add require statements
  require(rgdal)
  require(raster)
  require(stringr)
  require(plyr)
  
  # Get expected filename for point sources
  pt_sources_filename <-
    paste(unlist(str_split(getwd(),
                           pattern = "/"))[length(unlist(str_split(getwd(),
                                                                   pattern = "/")))],
          "--point_sources.txt", sep = '')
  
  # Create point sources text file with header if it doesn't exist
  if (file.exists(pt_sources_filename) == FALSE){
    
    # Create empty file in working folder
    file.create(pt_sources_filename)
    
    # Add header row to new point sources file
    cat(paste("src_name", ",",
              "species_name", ",",
              "lat_dec_deg", ",",
              "lon_dec_deg", ",",
              "x_coord_km", ",",
              "y_coord_km", ",",
              "UTM_zone", ",",
              "stack_height", ",",
              "base_elev", ",",
              "stack_diam", ",",
              "exit_velocity", ",",
              "exit_temp", ",",
              "emission_rate", ",",
              "emission_units", sep = ''),
        file = pt_sources_filename,
        append = TRUE)
  }
  
  # Determine whether lon/lat provided
  if (!is.null(lat) & !is.null(lon)){
    lon_lat_provided <- TRUE
  } else {
    lon_lat_provided <- FALSE
  }
  
  # Determine whether UTM coordinates provided
  if (!is.null(x_coord_km) & !is.null(y_coord_km) & !is.null(UTM_zone)){
    UTM_provided <- TRUE
  } else {
    UTM_provided <- FALSE
  }  
  
  
  
}
