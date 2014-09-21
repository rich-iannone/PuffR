#' Generate a vector of CALMET landuse categories from BC shapefile data
#' @description Generate a vector of CALMET landuse categories from BC shapefile data.
#' @param location_name an assigned name for the CALMET domain.
#' @param bbox_longlat 
#' @param bbox_UTM 
#' @param UTM_zone 
#' @param UTM_hemisphere 
#' @param proj_string_longlat 
#' @param proj_string_UTM 
#' @param shapefile_dir 
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
  shapefile <- readOGR(dsn = dsn, layer = layers[1])
  
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
  
  # Create a data frame for the LU categories, in row-major order
  raster_LU_df <- as.data.frame(rasterToPoints(raster_LU))
  
  # Define the colours for each of the CALMET land use categories using a named vector
  cols <- c("10" = "gold2",
            "20" = "olivedrab2",
            "30" = "springgreen",
            "40" = "forestgreen",
            "50" = "deepskyblue2",
            "60" = "orchid",
            "70" = "lightsalmon",
            "80" = "moccasin",
            "90" = "honeydew")
  
  # Create ggplot object for the gridded landuse categories
  graphics.off()
  
  g <- ggplot(raster_LU_df, aes(x = x/1000, y = y/1000, fill = as.character(layer))) +
    geom_tile() + 
    scale_fill_manual(values = cols,breaks = c(as.numeric(names(cols)), 100),
                      name = "Land Use\nCategories") + 
    coord_equal() +
    theme_bw(base_size = 12, base_family = "") +
    labs(x = paste("UTM (Zone ", UTM_zone, UTM_hemisphere, ") Easting, km", sep = '')) +
    labs(y = paste("UTM (Zone ", UTM_zone, UTM_hemisphere, ") Northing, km", sep = '')) +
    theme(axis.text = element_text(size = rel(1.2)),
          axis.title = element_text(size = rel(1.2)),
          legend.title = element_text(size = rel(1.2)))
  
  # Save as land use plot as a pdf file
  ggsave(filename = paste(location_name, "_landuse_raster.pdf", sep = ''), 
         plot = g, device = pdf, width = 8, height = 8, units = "in")
  
  # Add ID field to prepare data within polygon for ggplot use
  cropped_UTM@data$id <- rownames(cropped_UTM@data)
  
  cropped_UTM_points <- fortify(cropped_UTM, region = "id")
  
  cropped_UTM_df <- join(cropped_UTM_points, cropped_UTM@data, by = "id")
  
  # Create ggplot object for the landuse shapefile
  h <- ggplot(cropped_UTM_df, aes(x = long/1000, y = lat/1000,
                                  group = group, fill = as.character(CALMET_categories))) +
    geom_polygon() +
    scale_fill_manual(values = cols, breaks = c(as.numeric(names(cols)), 100),
                      name = "Land Use\nCategories") +
    coord_equal() +
    theme_bw(base_size = 12, base_family = "") +
    labs(x = paste("UTM (Zone ", UTM_zone, UTM_hemisphere, ") Easting, km", sep = '')) +
    labs(y = paste("UTM (Zone ", UTM_zone, UTM_hemisphere, ") Northing, km", sep = '')) +
    theme(axis.text = element_text(size = rel(1.2)),
          axis.title = element_text(size = rel(1.2)),
          legend.title = element_text(size = rel(1.2)))
  
  # Save as land use plot as a PDF file
  ggsave(filename = paste(location_name,"_landuse_shape.pdf", sep = ''), 
         plot = h, device = pdf, width = 8, height = 8, units = "in")
  
  # Extract the 'layer' column from 'raster_LU_df' and create a vector object
  CALMET_categories <- raster_LU_df$layer
  
  # Return the 'CALMET_categories' vector object
  return(CALMET_categories)
  
}
