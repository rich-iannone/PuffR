#' Plot concentration data from one or more CSV files
#' @description Plot concentration data from one or more CSV files.
#' @param CSV_file_pattern a regex pattern for matching one or more files
#' @export plot_concentration_data_from_CSV

plot_concentration_data_from_CSV <- function(CSV_file_pattern,
                                             UTM_zone,
                                             create_movie = FALSE,
                                             frame_rate = 6,
                                             IM_path,
                                             ffmpeg_path){
  
  # Add require statements
  require(ggplot2)
  require(ggmap)
  require(raster)
  require(rgdal)
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
    
    # Create a simplified set of column names
    colnames(xxyy_DF_lat_lon_conc) <- c("x", "y", "conc")
    
    # For purpose of plotting, cut 'conc' column into factor levels
    xxyy_DF_lat_lon_conc$conc <- cut(xxyy_DF_lat_lon_conc$conc,
                                     c(1E-5, 1E-4, 1E-3, 1E-2, 1E-1, 0, 1E0, 1E1, 1E2, 1E3, 1E4),
                                     include.lowest = TRUE)
    
    # Get map tile from Stamen Maps
    if (!exists("map")){
      map <- get_map(location = bbox_UTM,
                     maptype = "toner",
                     source = "stamen")
    }
    
    # Create a named vector with levels and colour values
    cols <- c("[0,1e-05]" = "#5E4FA2",
              "(1e-05,0.0001]" = "#3288BD",
              "(0.0001,0.001]" = "#66C2A5",
              "(0.001,0.01]" = "#ABDDA4",
              "(0.01,0.1]" = "#E6F598",
              "(0.1,1]" = "#FEE08B", 
              "(1,10]" = "#FDAE61",
              "(10,100]" = "#F46D43",
              "(100,1e+03]" = "#D53E4F",
              "(1e+03,1e+04]" = "#9E0142")
    
    # Prepare a ggplot graphic
    gg <- ggmap(ggmap = map) +
      geom_point(data = xxyy_DF_lat_lon_conc, aes(x = x, y = y, colour = conc, alpha = conc)) +
      scale_fill_manual(values = cols) +
      theme(legend.position = "none",
            axis.line = element_blank(), axis.ticks = element_blank(), 
            axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), 
            axis.text.x = element_blank(), axis.text.y = element_blank(), axis.text.y = element_blank()) +
      guides(alpha = FALSE)
    
    # Save plot as a pdf file
    ggsave(filename = paste(gsub(".csv", "", file_list[i]), ".pdf", sep = ''),
           width = 8, height = 8, units = "in")
        
  } 
  
  # If requested, create a movie from file list
  if (create_movie == TRUE){
    
    # Generate a list of PDF files that were created
    for (i in 1:length(file_list)){
      if (i == 1) PDF_list <- vector(mode = "character", length = 0)
      
      # Obtain a file name for each PDF that was to be generated
      a_PDF <- paste(gsub(".csv", "", file_list[i]), ".pdf", sep = '')
      
      # Generate a string vector of PDF file names
      PDF_list <- c(PDF_list, a_PDF)
    }
    
    # Determine which of the PDF files in 'PDF_list' that reside in the working folder
    
    # Begin loop for processing PDF files in the 'PDF_list' object
    for (i in 1:length(PDF_list)){
    
      # Convert PDF files to JPEG files using ImageMagick, cropping whitespace
      system(paste("cd ", getwd(), " ; ",
                   IM_path, "/convert",
                   " -verbose -density 150 -trim ",
                   PDF_list[i],
                   " -quality 100 -sharpen 0x1.0 ",
                   formatC(i, width = 4, flag = "0"),
                   ".jpg",
                   sep = ''))
    }
    
    # Construct the movie output name
    movie_output_name <- paste("movie__",
                               format(Sys.time(), "%Y-%m-%d--%H-%M-%S"),
                               sep = "")
    
    # Generate the movie file using ffmpeg
    system(paste("cd ", getwd(), " ; ", ffmpeg_path, "/ffmpeg -f image2 -start_number 1 -i '",
                 "%04d.jpg", "' -r ", frame_rate, " ",
                 "-vcodec libx264 -pix_fmt yuv420p ",
                 movie_output_name, ".mov",
                 sep = ''))
    
  }
  
}
