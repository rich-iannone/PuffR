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
                                      lat = NULL,
                                      lon = NULL,
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
  require(stringr)
  
  # Get expected filename for point sources
  pt_sources_filename <-
    paste(unlist(str_split(getwd(),
                           pattern = "/"))[length(unlist(str_split(getwd(),
                                                                   pattern = "/")))],
          "--point_sources.txt", sep = '')
  
  
  
  
}
