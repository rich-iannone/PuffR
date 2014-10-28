#' Add line sources to a list for later use in CALPUFF
#' @description Add line sources to a list for later use in CALPUFF
#' @param src_name the name of the source emitting the species.
#' @param species_name the name of the species undergoing emissions.
#' @param lat_dec_deg_1 the latitude of point 1 of the line source in decimal degrees.
#' @param lat_dec_deg_2 the latitude of point 2 of the line source in decimal degrees.
#' @param lon_dec_deg_1 the longitude of point 1 of the line source in decimal degrees.
#' @param lon_dec_deg_2 the longitude of point 2 of the line source in decimal degrees.
#' @param x_coord_km_1 the UTM easting value of point 1 of the line source in km units.
#' @param x_coord_km_2 the UTM easting value of point 2 of the line source in km units.
#' @param y_coord_km_1 the UTM northing value of point 1 of the line source in km units.
#' @param y_coord_km_2 the UTM northing value of point 2 of the line source in km units.
#' @param UTM_zone the UTM zone for the line source.
#' @param UTM_hemisphere the UTM hemisphere for the line source.
#' @param beg_x_coord 
#' @param beg_y_coord 
#' @param end_x_coord 
#' @param end_y_coord 
#' @param release_hgt 
#' @param base_elev the ground elevation at the location of the area source in meters above sea level (m ASL).
#' @param emission_rate the rate of constant emissions from the line source; units are defined in the 'emission_units' argument.
#' @param emission_units the units applied to the value defined in the 'emission_rate' argument. The possible selections are: (1) "g/s", (2) "kg/hr", (3) "lb/hr", (4) "tons/yr", (5) "Odour Unit * m3/s", (6) "Odour Unit * m3/min", (7) "metric tons/yr", (8) "Bq/s", and (9) "GBq/yr".
#' @export calpuff_add_line_sources

calpuff_add_line_sources <- function(src_name,
                                     species_name,
                                     lat_dec_deg = NULL,
                                     lon_dec_deg = NULL,
                                     x_coord_km = NULL,
                                     y_coord_km = NULL,
                                     UTM_zone = NULL,
                                     UTM_hemisphere = NULL,
                                     beg_x_coord,
                                     beg_y_coord,
                                     end_x_coord,
                                     end_y_coord,
                                     release_hgt,
                                     base_elev,
                                     emission_rate,
                                     emission_units){
  
  # Add require statements
  require(rgdal)
  require(raster)
  require(stringr)
  require(plyr)
 
  # Get expected filename for line sources
  line_sources_filename <-
    paste(unlist(str_split(getwd(),
                           pattern = "/"))[length(unlist(str_split(getwd(),
                                                                   pattern = "/")))],
          "--line_sources.txt", sep = '')
  
  
}
