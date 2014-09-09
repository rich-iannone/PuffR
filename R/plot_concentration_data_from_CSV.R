#' Plot concentration data from one or more CSV files
#' @description Plot concentration data from one or more CSV files.
#' @param CSV_file_pattern a regex pattern for matching one or more files
#' @export plot_concentration_data_from_CSV

plot_concentration_data_from_CSV <- function(CSV_file_pattern,
                                             UTM_zone){
  
  # Add require statements
  require(ggplot2)
  require(ggmap)
  require(sp)
    
  # Obtain a file list from the supplied pattern
  file_list <- list.files(pattern = CSV_file_pattern)
  
  # Open loop to process 'file_list' CSV files
  for (i in 1:length(file_list)){
    
    # Read in a CSV file from the 'file_list' vector object
    concentration_data <- read.csv(file_list[i], header = TRUE, stringsAsFactors = FALSE)
    
    # Determine the number of receptors in the x and y directions
    nx <- length(unique(concentration_data$recep_x_km))
    ny <- length(unique(concentration_data$recep_y_km))
    
    # Get the UTM coordinate extents for both x and y
    xx <- c(min(concentration_data$recep_x_km * 1000), min(concentration_data$recep_x_km* 1000),
            max(concentration_data$recep_x_km * 1000), max(concentration_data$recep_x_km * 1000))
    yy <- c(min(concentration_data$recep_y_km * 1000), max(concentration_data$recep_y_km * 1000),
            max(concentration_data$recep_y_km * 1000), min(concentration_data$recep_y_km * 1000))
    
    # Bind the x and y extents into a matrix
    xxyy <- cbind(xx,yy)
    
    # Create a PROJ.4 string for the UTM zone
    proj_string_UTM <- paste("+proj=utm +zone=",
                             UTM_zone,
                             " +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
                             sep = '')  
    
    # Create a SpatialPoints object for the bounding box that is projected as UTM coordinates
    SP_UTM <- SpatialPoints(xxyy, proj4string = CRS(proj_string_UTM))
    
    # Apply spatial transform to reproject the four UTM coordinates into lat/lon
    SP_lat_lon <- spTransform(SP_UTM, CRS("+proj=longlat +ellps=GRS80"))
    
    # Create a matrix object that is a bounding box in lat/lon coordinates
    bbox_lat_lon <- bbox(SP_lat_lon)
    
    # Create a SpatialPoints object in UTM projection for all x and y values
    xxyy_SP_UTM <- SpatialPoints(cbind(concentration_data$recep_x_km * 1000,
                                       concentration_data$recep_y_km * 1000),
                                 proj4string = CRS(proj_string_UTM))
    
    # Apply spatial transform to reproject UTM x and y values into lat/lon coordinates
    xxyy_SP_lat_lon <- spTransform(xxyy_SP_UTM, CRS("+proj=longlat +ellps=GRS80"))
    
    # Extract a data frame from the SpatialPixels lat/lon coordinates object
    xxyy_DF_lat_lon <- as.data.frame(xxyy_SP_lat_lon@coords)
    
    # Bind concentration values to the data frame containing lat/lon coordinates for all receptors
    xxyy_DF_lat_lon_conc <- cbind(xxyy_DF_lat_lon, concentration_data$concentration)
    
  
}

