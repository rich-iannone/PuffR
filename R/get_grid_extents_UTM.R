#' Get either of the four grid extents in UTM values 
#' @description This provides grid extents for a given side of a bounding box depending on how the grid is defined and given a width and height in meters.
#' @param side the requested side of the bounding box. Choices are 'left', 'right', 'bottom', or 'top'.
#' @param lat_lon_grid_loc the location of the lat/long inputs in relation to the domain. Choices are: 1 (center), 2 (lower left), 3 (lower right), 4 (upper left), 5 (upper right).
#' @param UTM_location the UTM location from which the extents will be determined.
#' @param domain_width_m the width of the meteorological domain in meters.
#' @param domain_height_m the height of the meteorological domain in meters.
#' @export get_grid_extents_UTM

get_grid_extents_UTM <- function(side,
                                 lat_lon_grid_loc,
                                 UTM_location,
                                 domain_width_m,
                                 domain_height_m){
  
  # Get extents of UTM grid (left, right, bottom, top) in meters
  
  if (side == 'left'){
    
    left_UTM <- if(lat_lon_grid_loc == 1) {
      UTM_location[1,1] - (0.5 * domain_width_m)
    } else if (lat_lon_grid_loc == 2) {
      UTM_location[1,1]
    } else if (lat_lon_grid_loc == 3) {
      UTM_location[1,1] - domain_width_m
    } else if (lat_lon_grid_loc == 4) {
      UTM_location[1,1]
    } else if (lat_lon_grid_loc == 5) {
      UTM_location[1,1] - domain_width_m
    } else {
      NULL
    }
    
    return(left_UTM)
    
  }
  
  if (side == 'right'){
    
    right_UTM <- if(lat_lon_grid_loc == 1) {
      UTM_location[1,1] + (0.5 * domain_width_m)
    } else if (lat_lon_grid_loc == 2) {
      UTM_location[1,1] + domain_width_m
    } else if (lat_lon_grid_loc == 3) {
      UTM_location[1,1]
    } else if (lat_lon_grid_loc == 4) {
      UTM_location[1,1] + domain_width_m
    } else if (lat_lon_grid_loc == 5) {
      UTM_location[1,1]
    } else {
      NULL
    }
    
    return(right_UTM)
    
  }
  
  if (side == 'bottom'){
    
    bottom_UTM <- if(lat_lon_grid_loc == 1) {
      UTM_location[1,2] - (0.5 * domain_height_m)
    } else if (lat_lon_grid_loc == 2) {
      UTM_location[1,2]
    } else if (lat_lon_grid_loc == 3) {
      UTM_location[1,2]
    } else if (lat_lon_grid_loc == 4) {
      UTM_location[1,2] - domain_height_m
    } else if (lat_lon_grid_loc == 5) {
      UTM_location[1,2] - domain_height_m
    } else {
      NULL
    }
    
    return(bottom_UTM)
    
  }
  
  if (side == 'top'){
    
    top_UTM <- if(lat_lon_grid_loc == 1) {
      UTM_location[1,2] + (0.5 * domain_height_m)
    } else if (lat_lon_grid_loc == 2) {
      UTM_location[1,2] + domain_height_m
    } else if (lat_lon_grid_loc == 3) {
      UTM_location[1,2] + domain_height_m
    } else if (lat_lon_grid_loc == 4) {
      UTM_location[1,2]
    } else if (lat_lon_grid_loc == 5) {
      UTM_location[1,2]
    } else {
      NULL
    }
    
    return(top_UTM)
    
  }
  
}
