#' Generate a grid of receptors from a CALMET geophysical input file
#' @description Generate a grid of receptors from a pre-existing CALMET geophysical input file.
#' @param geophys_file a path to a geophysical data file generated using the 'calmet_define_geophys" function.
#' @param resolution_scale_factor a factor by which the resolution of the receptors relates to the resolution of the CALMET domain.
#' @param download_SRTM a choice of whether to download the SRTM GeoTIFF height data from a server or read the identical files from a local folder.
#' @param SRTM_file_path path to a folder containing a collection of SRTM V4 zip archive files.
#' @export calpuff_define_receptors_from_geophys_file

calpuff_define_receptors_from_geophys_file <- function(geophys_file = NULL,
                                                       resolution_scale_factor = 0.5,
                                                       download_SRTM = TRUE,
                                                       SRTM_file_path = NULL){
  
  # Add require statements
  require(rgdal)
  require(plyr)
  require(sp)
  require(raster)
  require(ggplot2)
  require(stringr)
   
  # From file contents, read in the grid information into a data frame
  grid_info <- as.data.frame(t(matrix(as.numeric(unlist(str_split(readLines(geophys_file)[
    (as.numeric(readLines(geophys_file)[2]) + 6)], " ")))[
      which(!is.na(as.numeric(unlist(str_split(readLines(geophys_file)[
        (as.numeric(readLines(geophys_file)[2]) + 6)], " ")))))][1:5])))
  
  
  # Get the UTM zone from the geophysical data file
  UTM_zone <- str_trim(readLines(geophys_file)[as.numeric(readLines(geophys_file)[2]) + 4])
  
  proj_string_UTM <- paste("+proj=utm +zone=",
                           UTM_zone,
                           " +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
                           sep = '')
  
  # Provide column names for the 'grid_info' data frame
  colnames(grid_info) <- c("nx", "ny", "xorigkm", "yorigkm", "dgridkm")
  
  # Get extents of UTM grid (left, right, bottom, top) in meters
  left_UTM <- grid_info[1, 3] * 1000
  
  bottom_UTM <- grid_info[1, 4] * 1000
    
  right_UTM <- left_UTM + (grid_info[1, 1] * (grid_info[1, 5] * 1000))
  
  top_UTM <- bottom_UTM + (grid_info[1, 2] * (grid_info[1, 5] * 1000))
  
  # Create a data frame object for UTM values of LL, LR, UL, and UR
  LL_LR_UL_UR_UTM_m_DF <- data.frame("x" = c(left_UTM, right_UTM, left_UTM, right_UTM), 
                                     "y" = c(bottom_UTM, bottom_UTM, top_UTM, top_UTM))
  
  # Create a SpatialPoints object for UTM values of LL, LR, UL, and UR
  LL_LR_UL_UR_UTM_m_SP <- SpatialPoints(as.matrix(LL_LR_UL_UR_UTM_m_DF),
                                        proj4string = CRS(proj_string_UTM))
  
  # Generate Extent object in UTM
  bbox_UTM <- extent(LL_LR_UL_UR_UTM_m_SP)
  
  
}

