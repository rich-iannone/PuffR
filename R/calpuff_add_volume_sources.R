#' Add volume sources to a list for later use in CALPUFF
#' @description Add volume sources to a list for later use in CALPUFF
#' @param src_name the name of the source emitting the species.
#' @param species_name the name of the species undergoing emissions.
#' @param emission_rate the rate of constant emissions from the line source; units are defined in the 'emission_units' argument.
#' @param emission_units the units applied to the value defined in the 'emission_rate' argument. The possible selections are: (1) "g/s", (2) "kg/hr", (3) "lb/hr", (4) "tons/yr", (5) "Odour Unit * m3/s", (6) "Odour Unit * m3/min", (7) "metric tons/yr", (8) "Bq/s", and (9) "GBq/yr".
#' @export calpuff_add_volume_sources

calpuff_add_volume_sources <- function(src_name,
                                       species_name,
                                       emission_rate,
                                       emission_units){
 
  # Add require statements
  require(rgdal)
  require(raster)
  require(stringr)
  require(plyr)
  
}
