#' Define the CALMET domain and determine gridded terrain heights
#' @description Define the CALMET domain and determine gridded terrain heights for use in the GEO.DAT CALMET input file
#' @param lat_dec_deg the latitude of the CALMET domain in decimal degrees. The location of this point is defined in the lat_long_grid_loc argument.
#' @param long_dec_deg the longitude of the CALMET domain in decimal degrees. The location of this point is defined in the lat_long_grid_loc argument.
#' @param lat_long_grid_loc the location of the lat/long inputs in relation to the domain. Choices are: 1 (center), 2 (lower left), 3 (lower right), 4 (upper left), 5 (upper right).
#' @param domain_width_m the desired width of the meteorological domain in meters.
#' @param domain_height_m the desired height of the meteorological domain in meters.
#' @export calmet_define_domain
#' @examples
#' \dontrun{
#' # Create a CALMET domain of 100 by 100 km in the Los Angeles area.
#' # Chosen lat/long coordinates are for the center of the domain. 
#' calmet_define_domain(lat_dec_deg = 34.050184,
#'                      long_dec_deg = -118.253959,
#'                      lat_long_grid_loc = 1,
#'                      domain_width_m = 100000,
#'                      domain_height_m = 100000)
#'}

calmet_define_domain <- function(lat_dec_deg = NULL,
                                 long_dec_deg = NULL,
                                 lat_long_grid_loc = 1,
                                 domain_width_m = NULL,
                                 domain_height_m = NULL) {
  
  # Add require statements
  require(rgdal)
  require(plyr)
  require(sp)
  require(raster)
  
  # Where is this point located on the grid?
  # Choices are: 1 (center), 2 (lower left), 3 (lower right), 4 (upper left), 5 (upper right)
  lat_long_grid_loc <- 1
  
  # Define the cell resolution (square cells) as 250 m
  cell_resolution_m <- 250
  
  # Round the provided width and the height of the met domain to the resolution of the cell
  domain_width_m <- round_any(domain_width_m, 250, round)
  domain_height_m <- round_any(domain_height_m, 250, round)
  
  # Get matrix of longitude and latitude for chosen point
  lat_long_dec_deg <- cbind(long_dec_deg, lat_dec_deg)
  
  # Determine the UTM zone
  UTM_zone <- (floor((long_dec_deg + 180)/6) %% 60) + 1
    
  # Define the PROJ.4 projection strings for long/lat and UTM projections
  proj_string_longlat <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

  proj_string_UTM <- paste("+proj=utm +zone=",
                           UTM_zone,
                           " +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
                           sep = '')
  
  # Project as UTM coordinates from the determined UTM zone, round to nearest 250 m using the
  # 'round_any' function from the 'plyr' package
  UTM_location <- project(lat_long_dec_deg, proj_string_UTM)
  UTM_location <- round_any(UTM_location, 250, round)
  
  # Do these length and width values accomodate an integer number of cells of the specified resolution?
  # These checks will be later part of a function in setting domain width and height
  is_number_cells_across_x_an_int <- ifelse(domain_width_m %% cell_resolution_m != 0, FALSE, TRUE)
  is_number_cells_across_y_an_int <- ifelse(domain_height_m %% cell_resolution_m != 0, FALSE, TRUE)
  
  # Get the number of cells in the x direction
  number_cells_across_x <- ifelse(is_number_cells_across_x_an_int == TRUE,
                                  domain_width_m/cell_resolution_m, NULL)
  
}
