#' Add line sources to a list for later use in CALPUFF
#' @description Add line sources to a list for later use in CALPUFF
#' @param src_name the name of the source emitting the species.
#' @param species_name the name of the species undergoing emissions.
#' @param lat_dec_deg the latitude of the line source in decimal degrees.
#' @param lon_dec_deg the longitude of the line source in decimal degrees.
#' @param x_coord_km the UTM easting value of the line source in km units.
#' @param y_coord_km the UTM northing value of the line source in km units.
#' @param UTM_zone the UTM zone for the line source.
#' @param UTM_hemisphere the UTM hemisphere for the line source.
#' @export calpuff_add_line_sources

calpuff_add_line_sources <- function(src_name,
                                     species_name,
                                     lat_dec_deg = NULL,
                                     lon_dec_deg = NULL,
                                     x_coord_km = NULL,
                                     y_coord_km = NULL,
                                     UTM_zone = NULL,
                                     UTM_hemisphere = NULL){
  
}
