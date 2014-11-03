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
    paste(unlist(str_split(getwd(),
                           pattern = "/"))[length(unlist(str_split(getwd(),
                                                                   pattern = "/")))],
          "--volume_sources.txt", sep = '')
  
  # Create volume sources text file with header if it doesn't exist
  if (file.exists(vol_sources_filename) == FALSE){
    
    # Create empty file in working folder
    file.create(vol_sources_filename)
    
}
