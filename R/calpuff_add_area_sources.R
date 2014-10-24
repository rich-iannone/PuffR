#' Add area sources to a list for later use in CALPUFF
#' @description Add area sources to a list for later use in CALPUFF
#' @param src_name the name of the source emitting the species.
#' @param species_name the name of the species undergoing emissions.
#' @param lat_dec_deg the latitude of the area source in decimal degrees.
#' @param lon_dec_deg the longitude of the area source in decimal degrees.
#' @param x_coord_km the UTM easting value of the area source in km units.
#' @param y_coord_km the UTM northing value of the area source in km units.
#' @param UTM_zone the UTM zone for the area source.
#' @param UTM_hemisphere the UTM hemisphere for the area source.
#' @param effective_height
#' @param base_elev the ground elevation at the location of the point source stack in meters above sea level (m ASL).
#' @param init_sigma_z
#' @param emission_rate the rate of constant emissions from the stack; units are defined in the 'emission_units' argument.
#' @param emission_units the units applied to the value defined in the 'emission_rate' argument. The possible selections are: (1) "g/m2/s", (2) "kg/m2/hr", (3) "lb/m2/hr", (4) "tons/m2/yr", (5) "Odour Unit * m/s", (6) "Odour Unit * m/min", (7) "metric tons/m2/yr", (8) "Bq/m2/s", and (9) "GBq/m2/yr".
#' @export calpuff_add_area_sources

calpuff_add_area_sources <- function(){
  
  # Add require statements
  require(rgdal)
  require(raster)
  require(stringr)
  require(plyr)
  
}
