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
  if (!is.null(lat_dec_deg) & !is.null(lon_dec_deg)){
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
  
  # If both lon/lat provided, convert to UTM
  if (lon_lat_provided == TRUE & UTM_provided == FALSE){
    
    # Get matrix of longitude and latitude for source location
    lat_lon_dec_deg <- cbind(lon_dec_deg, lat_dec_deg)
    
    # Determine the UTM zone
    UTM_zone <- (floor((lon_dec_deg + 180)/6) %% 60) + 1
    
    # Determine whether source is in the Northern Hemisphere or the Southern Hemisphere
    UTM_hemisphere <- ifelse(lat_dec_deg >= 0, "N", "S")
    
    # Define a PROJ.4 projection string for a lat/lon projection
    proj_string_longlat <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    
    # Define a PROJ.4 projection string for a UTM projection
    proj_string_UTM <- paste("+proj=utm +zone=",
                             UTM_zone,
                             " +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
                             sep = '')
    
    # Project as UTM coordinates from the determined UTM zone
    UTM_location <- project(lat_lon_dec_deg, proj_string_UTM)
    
  
  
}
