#' Define the CALMET domain and determine gridded terrain heights
#' @description Define the CALMET domain and determine gridded terrain heights for use in the GEO.DAT CALMET input file
#' @param lat_dec_deg the latitude of the CALMET domain in decimal degrees. The location of this point is defined in the lat_lon_grid_loc argument.
#' @param lon_dec_deg the longitude of the CALMET domain in decimal degrees. The location of this point is defined in the lat_lon_grid_loc argument.
#' @param lat_lon_grid_loc the location of the lat/long inputs in relation to the domain. Choices are: 1 (center), 2 (lower left), 3 (lower right), 4 (upper left), 5 (upper right).
#' @param domain_width_m the desired width of the meteorological domain in meters.
#' @param domain_height_m the desired height of the meteorological domain in meters.
#' @export calmet_define_domain
#' @examples
#' \dontrun{
#' # Create a CALMET domain of 100 by 100 km in the Los Angeles area.
#' # Chosen lat/lon coordinates are for the center of the domain. 
#' calmet_define_domain(lat_dec_deg = 34.050184,
#'                      lon_dec_deg = -118.253959,
#'                      lat_lon_grid_loc = 1,
#'                      domain_width_m = 8000,
#'                      domain_height_m = 8000)
#'}

calmet_define_domain <- function(lat_dec_deg = NULL,
                                 lon_dec_deg = NULL,
                                 lat_lon_grid_loc = 1,
                                 domain_width_m = NULL,
                                 domain_height_m = NULL,
                                 output_file = "heights.txt"){
  
  # Add require statements
  require(rgdal)
  require(plyr)
  require(sp)
  require(raster)
  
  # Where is this point located on the grid?
  # Choices are: 1 (center), 2 (lower left), 3 (lower right), 4 (upper left), 5 (upper right)
  lat_lon_grid_loc <- 1
  
  # Define the cell resolution (square cells) as 250 m
  cell_resolution_m <- 250
  
  # Round the provided width and the height of the met domain to the resolution of the cell
  domain_width_m <- round_any(domain_width_m, 250, round)
  domain_height_m <- round_any(domain_height_m, 250, round)
  
  # Get matrix of longitude and latitude for chosen point
  lat_lon_dec_deg <- cbind(lon_dec_deg, lat_dec_deg)
  
  # Determine the UTM zone
  UTM_zone <- (floor((lon_dec_deg + 180)/6) %% 60) + 1
  
  # Define a PROJ.4 projection string for a lat/lon projection
  proj_string_longlat <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  
  # Define a PROJ.4 projection string for a UTM projection
  proj_string_UTM <- paste("+proj=utm +zone=",
                           UTM_zone,
                           " +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
                           sep = '')
  
  # Project as UTM coordinates from the determined UTM zone, round to nearest 250 m using the
  # 'round_any' function from the 'plyr' package
  UTM_location <- project(lat_lon_dec_deg, proj_string_UTM)
  UTM_location <- round_any(UTM_location, 250, round)
  
  # Do these length and width values accomodate an integer number of cells of the specified resolution?
  # These checks will be later part of a function in setting domain width and height
  is_number_cells_across_x_an_int <- ifelse(domain_width_m %% cell_resolution_m != 0, FALSE, TRUE)
  is_number_cells_across_y_an_int <- ifelse(domain_height_m %% cell_resolution_m != 0, FALSE, TRUE)
  
  # Get the number of cells in the x direction
  number_cells_across_x <- ifelse(is_number_cells_across_x_an_int == TRUE,
                                  domain_width_m/cell_resolution_m, NULL)
  
  # Get the number of cells in the y direction
  number_cells_across_y <- ifelse(is_number_cells_across_y_an_int == TRUE,
                                  domain_height_m/cell_resolution_m, NULL)
  
  # Get the total number of cells
  total_cells <- number_cells_across_x * number_cells_across_y
  
  # Get extents of UTM grid (left, right, bottom, top) in meters
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
  
  # Create a data frame object for UTM values of LL, LR, UL, and UR
  LL_LR_UL_UR_UTM_m_DF <- data.frame("x" = c(left_UTM, right_UTM, left_UTM, right_UTM), 
                                     "y" = c(bottom_UTM, bottom_UTM, top_UTM, top_UTM))
  
  # Create a SpatialPoints object for UTM values of LL, LR, UL, and UR
  LL_LR_UL_UR_UTM_m_SP <- SpatialPoints(as.matrix(LL_LR_UL_UR_UTM_m_DF),
                                        proj4string = CRS(proj_string_UTM))
  
  # Generate Extent object in UTM
  bbox_UTM <- extent(LL_LR_UL_UR_UTM_m_SP)
  
  # Create a RasterLayer object for UTM values
  LL_LR_UL_UR_UTM_m_RL <- raster(nrows = number_cells_across_x,
                                 ncols = number_cells_across_x,
                                 ext = bbox_UTM,
                                 crs = proj_string_UTM)
  
  # Create a SpatialPoints object for lat/lon values of LL, LR, UL, and UR through a
  # spatial transform
  LL_LR_UL_UR_longlat_SP <- spTransform(LL_LR_UL_UR_UTM_m_SP, CRS("+proj=longlat +ellps=GRS80"))
  
  # Obtain DEM data projected as long/lat for the domain as a RasterLayer object
  srtm <- getData('SRTM',
                  lon = floor(lon_dec_deg),
                  lat = floor(lat_dec_deg))
  
  # Generate Extents object in long/lat projection for cropping
  bbox_longlat <- extent(LL_LR_UL_UR_longlat_SP)
  
  # Crop DEM data using 'bbox' Extent object in lat/lon projection
  srtm_cropped <- crop(srtm, bbox_longlat)
  
  # Reproject cropped RasterLayer object from lat/lon to UTM
  srtm_UTM <- projectRaster(srtm_cropped,
                            crs = paste("+proj=utm +zone=",
                                        UTM_zone,
                                        " +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
                                        sep = ''))
  
  # Crop DEM data again using 'bbox' Extent object in UTM projection
  srtm_UTM_resampled <- resample(srtm_UTM, LL_LR_UL_UR_UTM_m_RL)
  
  # Create a SpatialPixelsDataFrame from the resampled data
  srtm_UTM_resampled.SPDF <- as(srtm_UTM_resampled, "SpatialPixelsDataFrame")
  
  # Create a data frame for plotting in ggplot
  srtm_UTM_resampled.df <- as.data.frame(srtm_UTM_resampled.SPDF)
  
  # Plot the grid using ggplot
  g <- ggplot(srtm_UTM_resampled.df, aes(x = x, y = y, fill = srtm_UTM_resampled.df[,3])) +
       geom_tile(aes(fill = srtm_UTM_resampled.df[,3])) +
       scale_fill_gradient(low = "green", high = "red",
                           guide = guide_legend(title = "Heights")) +
       coord_equal() +
       theme_bw(base_size = 12, base_family = "") +
       labs(x = "UTM Easting, m") +
       labs(y = "UTM Northing, m") +
       theme(axis.text = element_text(size = rel(1.2)),
             axis.title = element_text(size = rel(1.2)),
             legend.title = element_text(size = rel(1.2)))
  
  # Extract heights from the resampled DEM in UTM
  gridded_heights_UTM_m_vector <- srtm_UTM_resampled@data@values
  
  # Create a data frame for the extracted heights in UTM, in row-major order
  gridded_heights_UTM_m_df <- as.data.frame(matrix(gridded_heights_UTM_m_vector,
                                                   nrow = number_cells_across_y,
                                                   ncol = number_cells_across_x))
  
  # Replace NA values with 0 values
  gridded_heights_UTM_m_df[is.na(gridded_heights_UTM_m_df)] <- 0
  
  # Generate a vector of comma-delimited strings containing heights of every row of cells;
  # this is for writing to a file and eventual inclusion in the GEO.DAT file
  for (i in 1:nrow(gridded_heights_UTM_m_df)){
    
    if (i == 1) gridded_heights_UTM_m_row_major_strings <- vector(mode = "character", length = 0)
    
    string <- paste(round(gridded_heights_UTM_m_df[i, ], digits = 2), collapse = ", ")
    
    gridded_heights_UTM_m_row_major_strings <- c(gridded_heights_UTM_m_row_major_strings, string)
    
  }
  
  # Write the formatted text to disk
  cat(gridded_heights_UTM_m_row_major_strings, file = output_file, sep = "\n")
  
}
