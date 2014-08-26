#' Download an SRTM V4 GeoTIFF file
#' @description Download an SRTM V4 GeoTIFF file based on lat/lon coordinates
#' @param lon a longitude supplied as decimal degrees.
#' @param lat a latitude supplied as decimal degrees.
#' @param download a choice of whether to download the file if it already exists in the specified local path.
#' @param path a path for which GeoTIFF files may exist locally.
#' @export download_SRTMV4_GeoTIFF

download_SRTMV4_GeoTIFF <- function(lon, lat, download, path){
  
  stopifnot(lon >= -180 & lon <= 180)
  stopifnot(lat >= -60 & lat <= 60)
  rs <- raster(nrows = 24, ncols = 72, xmn = -180, xmx = 180, 
               ymn = -60, ymx = 60)
  rowTile <- rowFromY(rs, lat)
  colTile <- colFromX(rs, lon)
  if (rowTile < 10) {
    rowTile <- paste("0", rowTile, sep = "")
  }
  if (colTile < 10) {
    colTile <- paste("0", colTile, sep = "")
  }
  f <- paste("srtm_", colTile, "_", rowTile, sep = "")
  zipfilename <- paste(path, "/", f, ".ZIP", sep = "")
  tiffilename <- paste(path, "/", f, ".TIF", sep = "")
  if (!file.exists(tiffilename)) {
    if (!file.exists(zipfilename)) {
      if (download) {
        theurl <- paste("http://gis-lab.info/data/srtm-tif/", 
                        f, ".zip", sep = "")
        test <- try(.download(theurl, zipfilename), silent = TRUE)
        if (class(test) == "try-error") {
          theurl <- paste("http://hypersphere.telascience.org/elevation/cgiar_srtm_v4/tiff/zip/", 
                          f, ".ZIP", sep = "")
          test <- try(.download(theurl, zipfilename), 
                      silent = TRUE)
          if (class(test) == "try-error") {
            theurl <- paste("http://srtm.csi.cgiar.org/SRT-ZIP/SRTM_V41/SRTM_Data_GeoTiff/", 
                            f, ".ZIP", sep = "")
            .download(theurl, zipfilename)
          }
        }
      }
      
      else {
        cat("file not available locally, use download=TRUE\n")
      }
    }
    
    if (file.exists(zipfilename)){
      unzip(zipfilename, exdir = dirname(zipfilename))
      file.remove(zipfilename)
    }
  }
  
  if (file.exists(tiffilename)){
    rs <- raster(tiffilename)
    projection(rs) <- "+proj=longlat +datum=WGS84"
    return(rs)
  }
  
  else {
    stop("file not found")
  }
}
