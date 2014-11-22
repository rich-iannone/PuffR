#' Download an SRTM V4 GeoTIFF file
#' @description Download an SRTM V4 GeoTIFF file based on lat/lon coordinates
#' @param SP_object a SpatialPoints object with a longlat projection.
#' @export download_SRTMV4_GeoTIFF

download_SRTMV4_GeoTIFF <- function(SP_object = NULL){
  
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
  
    
    stopifnot(all(SP_object@coords[,2] >= -60) == TRUE)
    stopifnot(all(SP_object@coords[,2] <= 60) == TRUE)
  }
  
  
  stopifnot(lon >= -180 & lon <= 180)
  stopifnot(lat >= -60 & lat <= 60)
  

  
  rowTile <- rowFromY(rs, lat)
  colTile <- colFromX(rs, lon)
  
  if (rowTile < 10) {
    rowTile <- paste("0", rowTile, sep = "")
  }
  if (colTile < 10) {
    colTile <- paste("0", colTile, sep = "")
  }
  
  # Construct filename for SRTM data
  f <- paste("srtm_", colTile, "_", rowTile, sep = "")
  
  # If a download is requested, get the file
  if (download == TRUE){
    
    temp_dir <- tempdir()
    
    zipfilename <- paste(temp_dir, "/", f, ".zip", sep = "")
    tiffilename <- paste(temp_dir, "/", f, ".tif", sep = "")
    
    download.file(url = paste("http://gis-lab.info/data/srtm-tif/", 
                              f, ".zip", sep = ""),
                  destfile = zipfilename, method = "auto", 
                  quiet = FALSE, mode = "wb", cacheOK = TRUE)
    
    if (file.exists(zipfilename)){
      unzip(zipfilename, exdir = dirname(zipfilename))
      file.remove(zipfilename)
      file.remove(gsub(".zip", ".hdr", zipfilename))
      file.remove(gsub(".zip", ".tfw", zipfilename))
      file.remove(paste(temp_dir, "/readme.txt", sep = ''))
    }
    
    if (file.exists(tiffilename)){
      rs <- raster(tiffilename)
      projection(rs) <- "+proj=longlat +datum=WGS84"
      return(rs)
    }
  }
  
  # If the file is known to exist on a supplied path, read in that file
  if (!is.null(SRTM_file_path)){
    
    zipfilename <- paste(SRTM_file_path, "/", f, ".zip", sep = "")
    tiffilename <- paste(SRTM_file_path, "/", f, ".tif", sep = "")
    
    if (!file.exists(zipfilename) & !file.exists(tiffilename)){
      stop("The file doesn't exist.")
    }
    
    if (file.exists(zipfilename)){
      unzip(zipfilename, exdir = dirname(zipfilename))
      file.remove(zipfilename)
      file.remove(gsub(".zip", ".hdr", zipfilename))
      file.remove(gsub(".zip", ".tfw", zipfilename))
      file.remove(paste(SRTM_file_path, "/readme.txt", sep = ''))
    }
    
    if (file.exists(tiffilename)){
      rs <- raster(tiffilename)
      projection(rs) <- "+proj=longlat +datum=WGS84"
      return(rs)
    }
    
  }
  
}
