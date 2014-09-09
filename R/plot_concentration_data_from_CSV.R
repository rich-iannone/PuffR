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
  
  file_list <- list.files(pattern = CSV_file_pattern)
  
  concentration_data <- read.csv(file_list, header = TRUE, stringsAsFactors = FALSE)
  
  nx <- length(unique(concentration_data$recep_x_km))
  ny <- length(unique(concentration_data$recep_y_km))
  
  concentration_colours <- c("#5E4FA2", "#3288BD", "#66C2A5", "#ABDDA4", "#E6F598",
                             "#FEE08B", "#FDAE61", "#F46D43", "#D53E4F", "#9E0142")
  
  # Create a PROJ.4 string for the UTM zone
  proj_string_UTM <- paste("+proj=utm +zone=",
                           UTM_zone,
                           " +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
                           sep = '')  
  
  # Get a bbox object
  xx <- c(min(concentration_data$recep_x_km * 1000), min(concentration_data$recep_x_km* 1000),
          max(concentration_data$recep_x_km * 1000), max(concentration_data$recep_x_km * 1000))
  yy <- c(min(concentration_data$recep_y_km * 1000), max(concentration_data$recep_y_km * 1000),
          max(concentration_data$recep_y_km * 1000), min(concentration_data$recep_y_km * 1000))
  xxyy <- cbind(xx,yy)
  SP_UTM <- SpatialPoints(xxyy,
                          proj4string = CRS(proj_string_UTM))
  
  SP_lat_lon <- spTransform(SP_UTM, CRS("+proj=longlat +ellps=GRS80"))
  
  bbox_UTM <- bbox(SP_lat_lon)
  
  
  xxyy_SP_UTM <- SpatialPoints(cbind(concentration_data$recep_x_km * 1000, concentration_data$recep_y_km * 1000),
                               proj4string = CRS(proj_string_UTM))
  
  xxyy_SP_lat_lon <- spTransform(xxyy_SP_UTM, CRS("+proj=longlat +ellps=GRS80"))
  
  xxyy_DF_lat_lon <- as.data.frame(xxyy_SP_lat_lon@coords)
  
  xxyy_DF_lat_lon_conc <- cbind(xxyy_DF_lat_lon, concentration_data$concentration)
  
  colnames(xxyy_DF_lat_lon_conc) <- c("x", "y", "conc")
  
  # For purpose of plotting, cut 'conc' column into factor levels
  xxyy_DF_lat_lon_conc$conc <- cut(xxyy_DF_lat_lon_conc$conc,
                                   c(1E-5, 1E-4, 1E-3, 1E-2, 1E-1, 1E-0, 1E1, 1E2, 1E3, 1E4),
                                   include.lowest = TRUE)
  
  # Get map tile
  map <- get_map(location = bbox_UTM,
                 maptype = "toner",
                 source = "stamen")
  
  # Prepare a ggplot graphic
  gg <- ggmap(ggmap = map) +
    geom_point(data = xxyy_DF_lat_lon_conc, aes(x = x, y = y, colour = conc, alpha = conc)) +
    scale_colour_gradientn(colours = concentration_colours,
                           values = c(1E-6, 1E-4, 1E-3, 1E-2, 1E-1, 1E-0, 1E1, 1E2, 1E3, 1E4)) +
    theme(legend.position = "none",
          axis.line = element_blank(), axis.ticks = element_blank(), 
          axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), 
          axis.text.x = element_blank(), axis.text.y = element_blank(), axis.text.y = element_blank()) +
    guides(alpha = FALSE)
  gg 
  
  # Save plot as a pdf file
  ggsave(filename = paste(gsub(".csv", "", file_list), ".pdf", sep = ''),
         width = 8, height = 8, units = "in")
  
}

