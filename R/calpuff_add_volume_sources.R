#' Add volume sources to a list for later use in CALPUFF
#' @description Add volume sources to a list for later use in CALPUFF
#' @param src_name the name of the source emitting the species.
#' @param species_name the name of the species undergoing emissions.
#' @param lat_dec_deg the latitude of the volume source in decimal degrees.
#' @param lon_dec_deg the longitude of the volume source in decimal degrees.
#' @param x_coord_km the UTM easting value of the volume source in km units.
#' @param y_coord_km the UTM northing value of the volume source in km units.
#' @param UTM_zone the UTM zone for the volume source.
#' @param UTM_hemisphere the UTM hemisphere for the volume source.
#' @param UTM_zone the UTM zone for the volume source.
#' @param UTM_hemisphere the UTM hemisphere for the volume source.
#' @param effective_height the effective height of the volume source in meters above ground level (m AGL).
#' @param base_elev the ground elevation at the location of the volume source in meters above sea level (m ASL).
#' @param init_sigma_y the initial sigma y value for the volume source in meters.
#' @param init_sigma_z the initial sigma z value for the volume source in meters.
#' @param emission_rate the rate of constant emissions from the volume source; units are defined in the 'emission_units' argument.
#' @param emission_units the units applied to the value defined in the 'emission_rate' argument. The possible selections are: (1) "g/s", (2) "kg/hr", (3) "lb/hr", (4) "tons/yr", (5) "Odour Unit * m3/s", (6) "Odour Unit * m3/min", (7) "metric tons/yr", (8) "Bq/s", and (9) "GBq/yr".
#' @export calpuff_add_volume_sources

calpuff_add_volume_sources <- function(src_name,
                                       species_name,
                                       lat_dec_deg = NULL,
                                       lon_dec_deg = NULL,
                                       x_coord_km = NULL,
                                       y_coord_km = NULL,
                                       UTM_zone = NULL,
                                       UTM_hemisphere = NULL,
                                       effective_height,
                                       base_elev,
                                       init_sigma_y,
                                       init_sigma_z,
                                       emission_rate,
                                       emission_units){
 
  # Add require statements
  require(rgdal)
  require(raster)
  require(stringr)
  require(plyr)
  
  # Get expected filename for volume sources
  vol_sources_filename <-
    paste0(unlist(str_split(getwd(),
                            pattern = "/"))[length(unlist(str_split(getwd(),
                                                                    pattern = "/")))],
           "--volume_sources.txt")
  
  # Create volume sources text file with header if it doesn't exist
  if (file.exists(vol_sources_filename) == FALSE){
    
    # Create empty file in working folder
    file.create(vol_sources_filename)
    
    # Add header row to new volume sources file
    cat(paste0("src_name", ",",
               "species_name", ",",
               "lat_dec_deg", ",",
               "lon_dec_deg", ",",
               "x_coord_km", ",",
               "y_coord_km", ",",
               "UTM_zone", ",",
               "UTM_hemisphere", ",",
               "effective_height", ",",
               "base_elev", ",",
               "init_sigma_y", ",",
               "init_sigma_z", ",",
               "emission_rate", ",",
               "emission_units"),
        sep = "\n",
        file = vol_sources_filename,
        append = TRUE)
  }
  
  # Determine whether lon/lat provided
  if (!is.null(lat_dec_deg) & !is.null(lon_dec_deg)){
    lon_lat_provided <- TRUE
  } else {
    lon_lat_provided <- FALSE
  }
  
  # Determine whether UTM coordinates and zone information provided
  if (!is.null(x_coord_km) & !is.null(y_coord_km)
      & !is.null(UTM_zone) & !is.null(UTM_hemisphere)){
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
    
    # Define the UTM x coordinate in km units
    x_coord_km <- UTM_location[1,1] / 1000
    
    # Define the UTM y coordinate in km units
    y_coord_km <- UTM_location[1,2] / 1000
    
  }
  
  # If UTM coordinates provided, convert to lon/lat
  if (lon_lat_provided == FALSE & UTM_provided == TRUE){
    
    # Define a PROJ.4 projection string for a lat/lon projection
    proj_string_longlat <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    
    # Define a PROJ.4 projection string for a UTM projection
    proj_string_UTM <- paste("+proj=utm +zone=",
                             UTM_zone,
                             " +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
                             sep = '')
    
    # Create a SpatialPoints object for the UTM coordinates
    UTM_m_SP <- SpatialPoints(matrix(c(x_coord_km * 1000,
                                       y_coord_km * 1000),
                                     nrow = 1,
                                     ncol = 2),
                              proj4string = CRS(proj_string_UTM))
    
    # Project as UTM coordinates from the determined UTM zone
    latlon_SP <- spTransform(UTM_m_SP, CRS(proj_string_longlat))
    
    # Extract the latitude in decimal degrees from the SpatialPoints object
    lat_dec_deg <- latlon_SP@coords[[2]]
    
    # Extract the longitude in decimal degrees from the SpatialPoints object
    lon_dec_deg <- latlon_SP@coords[[1]]
    
  }
    
  # Write the values to the file
  cat(paste(src_name, ",",
            species_name, ",",
            format(lat_dec_deg, small.interval = 6), ",",
            format(lon_dec_deg, small.interval = 6), ",",
            format(x_coord_km, small.interval = 3), ",",
            format(y_coord_km, small.interval = 3), ",",
            UTM_zone, ",",
            UTM_hemisphere, ",",
            effective_height, ",",
            base_elev, ",",
            init_sigma_y, ",",
            init_sigma_z, ",",
            emission_rate, ",",
            emission_units, sep = ''),
      sep = "\n",
      file = vol_sources_filename,
      append = TRUE)
  
}
