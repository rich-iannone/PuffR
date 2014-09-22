#' Generate a vector of CALMET landuse categories from BC shapefile data
#' @description Generate a vector of CALMET landuse categories from BC shapefile data.
#' @param location_name an assigned name for the CALMET domain.
#' @param bbox_longlat a bounding box with latitude and longitude coordinates.
#' @param bbox_UTM a bounding box with UTM coordinates.
#' @param UTM_zone the UTM zone in which the domain resides.
#' @param UTM_hemisphere the hemisphere, either north ("N") or south ("S") in which the domain resides. 
#' @param proj_string_longlat the chosen PROJ.4 string for the lat/lon coordinate system.
#' @param proj_string_UTM the chosen PROJ.4 string for the UTM coordinate system.
#' @param shapefile_dir the directory in which the BC BTM V1 landuse shapefile is stored.
#' @export calmet_landuse_BTM_BC

calmet_landuse_BTM_BC <- function(location_name,
                                  bbox_longlat,
                                  bbox_UTM,
                                  UTM_zone,
                                  UTM_hemisphere,
                                  proj_string_longlat,
                                  proj_string_UTM,
                                  shapefile_dir){
  
  # Add package requirements
  require(rgdal)
  require(ggplot2)
  require(sp)
  require(raster)
  require(plyr)
    
  # Load in shapefile
  layers <- ogrListLayers(shapefile_dir)
  shapefile <- readOGR(dsn = shapefile_dir, layer = layers[1])
  
  # Reproject the shapefile  
  new_shape <- spTransform(shapefile,
                           CRS = CRS(proj_string_longlat))
  
  extents <- raster(xmn = bbox_longlat@xmin, xmx = bbox_longlat@xmax,
                    ymn = bbox_longlat@ymin, ymx = bbox_longlat@ymax, 
                    crs = proj_string_longlat)
  
  # Crop the shapefile
  cropped <- crop(new_shape, extents)
  
  cropped_UTM <- spTransform(cropped, CRS = CRS(proj_string_UTM))
  
  # For the reclassification of PLU_label with CALMET categories, a data frame could be used
  # to facilitate the transformation
  BC_PLU_labels <- c("Agriculture", "Alpine", "Barren Surfaces", "Estuaries", "Fresh Water",
                     "Glaciers and Snow", "Mining", "Old Forest", "Outside B.C.", "Range Lands",
                     "Recently Burned", "Recently Logged", "Recreation Activities",
                     "Residential Agriculture Mixtures", "Salt Water", "Selectively Logged",
                     "Shrubs", "Sub alpine Avalanche Chutes", "Urban", "Wetlands", "Young Forest")
  
  CALMET_categories <- c(20, 90, 70, 60, 50, 90, 70, 40, NA, 30,
                         70, 30, 30, 20, 50, 40, 40, 90, 10, 60, 40)
  
  BC_categories <- seq(1,21)
  
  LU_classification <- data.frame(BC_PLU_labels, BC_categories, CALMET_categories,
                                  stringsAsFactors = FALSE)
  
  # Add extra classification columns to the shapefile
  metadata <- cropped_UTM@data
  
  metadata_plus <- merge(metadata, LU_classification,
               by.x = "PLU_LABEL", by.y = "BC_PLU_labels",
               sort = FALSE, all.x = TRUE, all.y = FALSE)
  
  cropped_UTM@data <- metadata_plus
  
  # Create new raster object to fit desired target domain
  raster_obj <- raster(xmn = bbox_UTM@xmin, xmx = bbox_UTM@xmax,
                       ymn = bbox_UTM@ymin, ymx = bbox_UTM@ymax, 
                       crs = proj_string_UTM, resolution = cell_resolution_m,
                       vals = NULL)
  
  # Get the landuse characteristics
  raster_LU <- rasterize(cropped_UTM, raster_obj, field = "CALMET_categories")
  
  # Convert any NA values to CALMET code 50
  raster_LU@data@values[is.na(raster_LU@data@values)] <- 50
  
  # Create a data frame for the LU categories, in row-major order
  raster_LU_df <- as.data.frame(rasterToPoints(raster_LU))
  
  return(raster_LU_df)
  
}
