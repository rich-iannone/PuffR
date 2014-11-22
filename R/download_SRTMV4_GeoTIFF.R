#' Download and rasterize SRTM V4 GeoTIFF files
#' @description Download one or more SRTM V4 GeoTIFF files and merge files as necessary to encompass modelling domain
#' @param SP_object a SpatialPoints object with a longlat projection.
#' @export download_SRTMV4_GeoTIFF

download_SRTMV4_GeoTIFF <- function(SP_object){
  
  # Add require statement
  require(raster)
  
  # Create RasterLayer object representative of available SRTM tiles
  rs <- raster(nrows = 24, ncols = 72, xmn = -180, xmx = 180, 
               ymn = -60, ymx = 60)
  
  # Stop function if 'SP_object' not of class 'SpatialPoints'
  stopifnot(class(SP_object)[1] == "SpatialPoints")
  
  # Stop function for disallowed extents
  stopifnot(all(SP_object@coords[,1] >= -180) == TRUE)
  stopifnot(all(SP_object@coords[,1] <= 180) == TRUE)
  stopifnot(all(SP_object@coords[,2] >= -60) == TRUE)
  stopifnot(all(SP_object@coords[,2] <= 60) == TRUE)
  
  # Get vector of longitude coordinates
  lon_coords <- c(SP_object@bbox[1,1],
                  seq(from = ceiling(SP_object@bbox[1,1]), to = floor(SP_object@bbox[1,2]), by = 1),
                  SP_object@bbox[1,2])
  
  # Get vector of latitude coordinates
  lat_coords <- c(SP_object@bbox[2,1],
                  seq(from = ceiling(SP_object@bbox[2,1]), to = floor(SP_object@bbox[2,2]), by = 1),
                  SP_object@bbox[2,2])
  
  # Create data frame with coordinates in row-major order
  lat_lon_coords <- data.frame(mat.or.vec(nr = 0, nc = 2))
  colnames(lat_lon_coords) <- c("lon", "lat")
  
  for (i in 1:length(lon_coords)){
    for (j in 1:length(lat_coords)){
      lat_lon_coord <- data.frame(lon = lon_coords[i], lat = lat_coords[j])
      lat_lon_coords <- rbind(lat_lon_coords, lat_lon_coord) 
    }
  }
  
  # Get vector of GeoTIFF column numbers
  colTile_vector <- colFromX(rs, lat_lon_coords$lon)
  
  # Get vector of GeoTIFF row numbers
  rowTile_vector <- rowFromY(rs, lat_lon_coords$lat)
  
  # Create data frame with unique combinations of GeoTIFF column and row numbers 
  unique_tile_df <- unique(data.frame(rowTile = rowTile_vector, colTile = colTile_vector))
  
  # Format values 'unique_tile_df' so leading zeros are provided
  for (i in 1:nrow(unique_tile_df)){
    if (i == 1){
      unique_tile_df$rowTile <- as.character(unique_tile_df$rowTile)
      unique_tile_df$colTile <- as.character(unique_tile_df$colTile)
    }
    
    if (as.numeric(unique_tile_df$rowTile[i]) < 10){
      unique_tile_df$rowTile[i] <- paste0("0", unique_tile_df$rowTile[i])
    }
    
    if (as.numeric(unique_tile_df$colTile[i]) < 10){
      unique_tile_df$colTile[i] <- paste0("0", unique_tile_df$colTile[i])
    }     
  }
  
  # Construct filename list for SRTM data downloads
  for (i in 1:nrow(unique_tile_df)){
    if (i == 1) file_list <- vector(mode = "character", length = 0)
    
    a_file <- paste0("srtm_", unique_tile_df$colTile[i],
                     "_", unique_tile_df$rowTile[i])
    
    file_list <- c(file_list, a_file)
  }
  
  # Assign a temporary directory path to 'temp_dir'
  temp_dir <- tempdir()
  
  for (i in 1:length(file_list)){
    
    # Construct expected filename for zip file
    zipfilename <- paste0(temp_dir, "/", file_list[i], ".zip")
    
    # Construct expected filename for tif file
    tiffilename <- paste0(temp_dir, "/", file_list[i], ".tif")
    
    # Download the GeoTIFF file from the FTP server
    download.file(url = paste0("http://gis-lab.info/data/srtm-tif/", 
                               file_list[i], ".zip"),
                  destfile = zipfilename, method = "auto", 
                  quiet = FALSE, mode = "wb", cacheOK = TRUE)
    
    # Unzip the downloaded file and remove unnecessary files
    if (file.exists(zipfilename)){
      unzip(zipfilename, exdir = dirname(zipfilename))
      file.remove(zipfilename)
      file.remove(gsub(".zip", ".hdr", zipfilename))
      file.remove(gsub(".zip", ".tfw", zipfilename))
      file.remove(paste(temp_dir, "/readme.txt", sep = ''))
    }
    
    # If the expected tif file exists, create a RasterLayer object from it and
    # assign it as an object with the same name as the basename of the file
    if (file.exists(tiffilename)){
      assign(file_list[i], raster(tiffilename))
    }
  }
  
  # Return a RasterLayer object if only one file downloaded
  if (length(file_list) == 1){
    
    # Assign the single RasterLayer object to 'the_raster'
    the_raster <- get(file_list)
    
    # Return the RasterLayer produced from a single SRTM file
    return(the_raster)
  }
  
  # Merge multiple SRTM files together into single raster
  if (length(file_list) > 1){
    for (i in 1:length(file_list)){
      if (i == 1) raster_list <- vector(mode = "list", length = 0)
      
      # Generate a list of multiple RasterLayer objects
      raster_list <- c(raster_list, get(file_list[i]))
    }
    
    # Merge multiple RasterLayer objects from 'raster_list'
  }
}


