#' Generate a grid of receptors from a CALMET geophysical input file
#' @description Generate a grid of receptors from a pre-existing CALMET geophysical input file.
#' @param geophys_file a path to a geophysical data file generated using the 'calmet_define_geophys" function.
#' @param resolution_scale_factor a factor by which the resolution of the receptors relates to the resolution of the CALMET domain.
#' @param download_SRTM a choice of whether to download the SRTM GeoTIFF height data from a server or read the identical files from a local folder.
#' @param SRTM_file_path path to a folder containing a collection of SRTM V4 zip archive files.
#' @export calpuff_define_receptors_from_geophys_file

calpuff_define_receptors_from_geophys_file <- function(geophys_file = NULL,
                                                       resolution_scale_factor = 0.5,
                                                       height_above_ground = 2.5,
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
  
  # Provide column names for the 'grid_info' data frame
  colnames(grid_info) <- c("nx", "ny", "xorigkm", "yorigkm", "dgridkm")
  
  # Get the UTM zone from the geophysical data file
  UTM_zone <- str_trim(readLines(geophys_file)[as.numeric(readLines(geophys_file)[2]) + 4])
  
  # Create a PROJ.4 string for the UTM zone
  proj_string_UTM <- paste("+proj=utm +zone=",
                           UTM_zone,
                           " +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
                           sep = '')
  
  # Get extents of UTM grid (left, right, bottom, top) in meters
  left_UTM <- grid_info$xorigkm * 1000
  bottom_UTM <- grid_info$yorigkm * 1000
  right_UTM <- left_UTM + (grid_info$nx * grid_info$dgridkm * 1000)
  top_UTM <- bottom_UTM + (grid_info$ny * grid_info$dgridkm * 1000)
  
  # Create a data frame object for UTM values of LL, LR, UL, and UR
  LL_LR_UL_UR_UTM_m_DF <- data.frame("x" = c(left_UTM, right_UTM, left_UTM, right_UTM), 
                                     "y" = c(bottom_UTM, bottom_UTM, top_UTM, top_UTM))
  
  # Create a SpatialPoints object for UTM values of LL, LR, UL, and UR
  LL_LR_UL_UR_UTM_m_SP <- SpatialPoints(as.matrix(LL_LR_UL_UR_UTM_m_DF),
                                        proj4string = CRS(proj_string_UTM))
  
  # Generate Extent object in UTM
  bbox_UTM <- extent(LL_LR_UL_UR_UTM_m_SP)
  
  # Create a RasterLayer object for UTM values
  LL_LR_UL_UR_UTM_m_RL <- raster(nrows = (1 / resolution_scale_factor) * grid_info$ny,
                                 ncols = (1 / resolution_scale_factor) * grid_info$nx,
                                 ext = bbox_UTM,
                                 crs = proj_string_UTM)
  
  # Create a SpatialPoints object for lat/lon values of LL, LR, UL, and UR through a
  # spatial transform
  LL_LR_UL_UR_longlat_SP <- spTransform(LL_LR_UL_UR_UTM_m_SP, CRS("+proj=longlat +ellps=GRS80"))
  
  # Determine the latitude and logitude of the LL of grid
  lon_dec_deg <- LL_LR_UL_UR_longlat_SP@coords[[1,1]]  
  lat_dec_deg <- LL_LR_UL_UR_longlat_SP@coords[[1,2]]
  
  # Obtain DEM data projected as long/lat for the domain as a RasterLayer object
  srtm <- download_SRTMV4_GeoTIFF(lon = floor(lon_dec_deg),
                                  lat = floor(lat_dec_deg),
                                  download = download_SRTM,
                                  SRTM_file_path = SRTM_file_path)
  
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
  
  # Create a copy of the RasterLayer object for subsituting NA values with 0
  srtm_UTM_resampled_no_NA <- srtm_UTM_resampled
  
  # Substitute NA values with 0 values in RasterLayer copy
  srtm_UTM_resampled_no_NA@data@values[is.na(srtm_UTM_resampled_no_NA@data@values)] <- 0
  
  # Create a SpatialPixelsDataFrame from the resampled data with no NA values in the data/values slot
  srtm_UTM_resampled_no_NA.SPDF <- as(srtm_UTM_resampled_no_NA, "SpatialPixelsDataFrame")
  
  # Create a data frame for plotting in ggplot
  srtm_UTM_resampled.df <- as.data.frame(srtm_UTM_resampled.SPDF)
  
  # Change the column names to a standard set of labels
  colnames(srtm_UTM_resampled.df) <- c("x", "y", "z")
  
  # Plot the grid of heights using ggplot
  g <- ggplot(srtm_UTM_resampled.df, aes(x = x/1000, y = y/1000, fill = z)) +
    geom_tile(aes(fill = z)) +
    scale_fill_gradient(low = "green", high = "red",
                        guide = guide_legend(title = "Heights")) +
    coord_equal() +
    theme_bw(base_size = 12, base_family = "") +
    labs(x = paste("UTM (Zone ", UTM_zone, ") Easting, km", sep = '')) +
    labs(y = paste("UTM (Zone ", UTM_zone, ") Northing, km", sep = '')) +
    theme(axis.text = element_text(size = rel(1.2)),
          axis.title = element_text(size = rel(1.2)),
          legend.title = element_text(size = rel(1.2)))
  
  # Save terrain plot as a pdf file
  ggsave(filename = "receptors.pdf",
         width = 8, height = 8, units = "in")
  
  # Extract heights from the resampled DEM in UTM
  gridded_heights_UTM_m_vector <- srtm_UTM_resampled@data@values
  
  # Create a matrix of the extracted heights in UTM, in row-major order
  gridded_heights_UTM_m_mat <- t(matrix(gridded_heights_UTM_m_vector,
                                        ncol = grid_info$ny * (1/resolution_scale_factor)))
  
  # Replace NA values with 0 values
  gridded_heights_UTM_m_mat[is.na(gridded_heights_UTM_m_mat)] <- 0
  
  # Obtain data frame of UTM coordinates and heights for entire set of receptors
  srtm_UTM_resampled_no_NA.df <- as.data.frame(srtm_UTM_resampled_no_NA.SPDF)
  colnames(srtm_UTM_resampled_no_NA.df) <- c("x", "y", "z")
  
}

