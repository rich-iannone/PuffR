#' Obtain surface meteorology and generate input files
#' @description Obtain surface meteorology and generate input files
#' @param start_year the starting year for the SURF.DAT file.
#' @param end_year the ending year for the SURF.DAT file.
#' @param lat_dec_deg the latitude of the CALMET domain in decimal degrees. The location of this point is defined in the lat_long_grid_loc argument.
#' @param lon_dec_deg the longitude of the CALMET domain in decimal degrees. The location of this point is defined in the lat_long_grid_loc argument.
#' @param lat_lon_grid_loc the location of the lat/long inputs in relation to the domain. Choices are: 1 (center), 2 (lower left), 3 (lower right), 4 (upper left), 5 (upper right).
#' @param domain_width_m the width of the meteorological domain in meters.
#' @param domain_height_m the height of the meteorological domain in meters.
#' @param output_file the specified filename for the SURF.DAT file. Defaults to "surf.dat".
#' @export calmet_surface_met
#' @examples
#' \dontrun{
#' # Generate "surf.dat" from previously defined period
#' calmet_surface_met()
#'}

calmet_surface_met <- function(start_year,
                               end_year,
                               lat_dec_deg = NULL,
                               lon_dec_deg = NULL,
                               lat_lon_grid_loc = 1,
                               domain_width_m = NULL,
                               domain_height_m = NULL
                               output_file = "surf.dat"){
  
  # Add require statements
  require(lubridate)
  
  # Define the cell resolution (square cells) as 250 m
  cell_resolution_m <- 250
  
  # Determine the UTM zone
  UTM_zone <- (floor((lon_dec_deg + 180)/6) %% 60) + 1
  
  # Define a PROJ.4 projection string for a lat/lon projection
  proj_string_longlat <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  
  # Get extents of UTM grid (left, right, bottom, top) in meters
  left_UTM <- if(lat_lon_grid_loc == 1) {
    UTM_location[1,1] - (0.5 * domain_width_m)
  } else if (lat_lon_grid_loc == 2) {
    UTM_location[1,1]
  } else if (lat_lon_grid_loc == 3) {
    UTM_location[1,1] - domain_width_m
  } else if (lat_lon_grid_loc == 4) {
    UTM_location[1,1]
  } else if (lat_lon_grid_loc == 5) {
    UTM_location[1,1] - domain_width_m
  } else {
    NULL
  }
  
  right_UTM <- if(lat_lon_grid_loc == 1) {
    UTM_location[1,1] + (0.5 * domain_width_m)
  } else if (lat_lon_grid_loc == 2) {
    UTM_location[1,1] + domain_width_m
  } else if (lat_lon_grid_loc == 3) {
    UTM_location[1,1]
  } else if (lat_lon_grid_loc == 4) {
    UTM_location[1,1] + domain_width_m
  } else if (lat_lon_grid_loc == 5) {
    UTM_location[1,1]
  } else {
    NULL
  }
  
  bottom_UTM <- if(lat_lon_grid_loc == 1) {
    UTM_location[1,2] - (0.5 * domain_height_m)
  } else if (lat_lon_grid_loc == 2) {
    UTM_location[1,2]
  } else if (lat_lon_grid_loc == 3) {
    UTM_location[1,2]
  } else if (lat_lon_grid_loc == 4) {
    UTM_location[1,2] - domain_height_m
  } else if (lat_lon_grid_loc == 5) {
    UTM_location[1,2] - domain_height_m
  } else {
    NULL
  }
  
  top_UTM <- if(lat_lon_grid_loc == 1) {
    UTM_location[1,2] + (0.5 * domain_height_m)
  } else if (lat_lon_grid_loc == 2) {
    UTM_location[1,2] + domain_height_m
  } else if (lat_lon_grid_loc == 3) {
    UTM_location[1,2] + domain_height_m
  } else if (lat_lon_grid_loc == 4) {
    UTM_location[1,2]
  } else if (lat_lon_grid_loc == 5) {
    UTM_location[1,2]
  } else {
    NULL
  }
  
  # Create a data frame object for UTM values of LL, LR, UL, and UR
  LL_LR_UL_UR_UTM_m_DF <- data.frame("x" = c(left_UTM, right_UTM, left_UTM, right_UTM), 
                                     "y" = c(bottom_UTM, bottom_UTM, top_UTM, top_UTM))
  
  # Create a SpatialPoints object for UTM values of LL, LR, UL, and UR
  LL_LR_UL_UR_UTM_m_SP <- SpatialPoints(as.matrix(LL_LR_UL_UR_UTM_m_DF),
                                        proj4string = CRS(proj_string_UTM))
  
  # Generate Extent object in UTM
  bbox_UTM <- extent(LL_LR_UL_UR_UTM_m_SP)
  
  # Create a RasterLayer object for UTM values
  LL_LR_UL_UR_UTM_m_RL <- raster(nrows = number_cells_across_x,
                                 ncols = number_cells_across_x,
                                 ext = bbox_UTM,
                                 crs = proj_string_UTM)
  
  # Create a SpatialPoints object for lat/lon values of LL, LR, UL, and UR through a
  # spatial transform
  LL_LR_UL_UR_longlat_SP <- spTransform(LL_LR_UL_UR_UTM_m_SP, CRS("+proj=longlat +ellps=GRS80"))
  
  # Generate Extents object in long/lat projection
  bbox_lat_lon <- extent(LL_LR_UL_UR_longlat_SP)  
  
  # Define the start and end times and determine number of hours in each year
  start_time <- ISOdatetime(startyear, 1, 1, hour = 0, min = 0, sec = 0, tz = "GMT")
  end_time <- ISOdatetime(endyear, 12, 31, hour = 24, min = 0, sec = 0, tz = "GMT")
  time_difference <- difftime(end_time, start_time, units='hours')
  total_hours <- time_difference[[1]]
  
  # Create the hourly time series as a list of POSIXlt time objects
  time_series <- as.list(c(1:total_hours))
  for (i in 1:total_hours) {
    time_series[[i]] <- start_time + (3600 * (i - 1))
  }
  
  # Use selected_synthetic_id to extract data from specified stations
  station_data_frames <- as.list(c(1:nrow(selected_synthetic_id)))
  
  for (i in 1:nrow(selected_synthetic_id)) {
    station_data_frames[[i]] <- list(read.csv(paste(selected_synthetic_id$CSV[i]), header = TRUE))
    
  }
  
  # Write out the SURF.DAT file
  #
  # The following parameters are used:
  #
  # Name      Description
  # --------  -----------
  # IYR       Year of data
  # IJUL      Julian day
  # IHR       Hour (00-23 LST)
  # WS        Wind Speed (m/s)
  # WD        Wind Direction (degrees)
  # ICEIL     Ceiling Height (hundreds of feet)
  # ICC       Opaque Sky Cover (tenths)
  # TEMPK     Air temperature (K)
  # IRH       Relative humidity (percent)
  # PRES      Station Pressure
  # IPCODE    Precipitation code
  #           (0=no precipitation, 1-18=liquid precipitation, 19-45=frozen precipitation)
  
  # Construct the file header records of the SURF.DAT file
  
  # Initialize file for writing
  cat("", file = paste(outputfile))
  
  # Add line 1 to file header (dataset name [SURF.DAT], dataset version [2.1], dataset message field)
  cat("SURF.DAT        2.1             Hour Start and End Times with Seconds",
      file = paste(outputfile), sep = "\n", append = TRUE)
  
  # Add line 2 to file header (number of comment lines to follow)
  cat("1",
      file = paste(outputfile), sep = "\n", append = TRUE)
  
  # Add line 3 to file header (single comment line)
  cat("Produced using R",
      file = paste(outputfile), sep = "\n", append = TRUE)
  
  # Add line 4 to file header (map projection [NONE])
  cat("NONE",
      file = paste(outputfile), sep = "\n", append = TRUE)
  
  # Add line 5 to file header (time zone)
  cat("UTC-0800",
      file = paste(outputfile), sep = "\n", append = TRUE)
  
  # Add line 6 to file header (Beginning and end times for file, number of met stations)
  cat(year(time_series[[1]]),
      "  ",
      yday(time_series[[1]]),
      "  ",
      hour(time_series[[1]]),
      "  ",
      year(time_series[[total_hours]] + 3600),
      "  ",
      yday(time_series[[total_hours]] + 3600),
      "  ",
      hour(time_series[[total_hours]] + 3600),
      "  ",
      nrow(selected_synthetic_id),
      file = paste(outputfile), sep = '', append = TRUE)
  
  cat("", file = paste(outputfile), sep = "\n", append = TRUE)
  
  # Add line 7- to file (5-digit identifiers for met stations used in file)
  for (i in 1:length(station_data_frames)) {
    cat(paste(as.character(i),
              as.character(i),
              as.character(i),
              as.character(i),
              as.character(i), sep = ''), file = paste(outputfile), sep = "\n", append = TRUE)
  }
  
  # Construct the body of the SURF.DAT file using a nested loops that provide grouped time interval
  # headers with ordered surface station parameters
  
  for (i in 1:total_hours) {
    cat(year(time_series[[i]]),
        "  ",
        yday(time_series[[i]]),
        "  ",
        hour(time_series[[i]]),
        "  ",
        year(time_series[[i]] + 3600),
        "  ", 
        yday(time_series[[i]] + 3600),
        "  ",
        hour(time_series[[i]] + 3600), file = paste(outputfile), append = TRUE)
    cat("", file = paste(outputfile), sep = "\n", append = TRUE)
    for (j in 1:length(station_data_frames)) {
      cat("  ",
          # Wind speed, m/s (WS)
          station_data_frames[[j]][[1]]$WIND.SPD[i],
          "  ",
          # Wind direction, degrees (WD)
          station_data_frames[[j]][[1]]$WIND.DIR[i],
          "  ",
          # Ceiling height, hundreds of feet (ICEIL)
          station_data_frames[[j]][[1]]$CEIL.HGT[i],
          "  ",
          # Opaque sky cover, tenths (ICC)
          # (still to prepare)
          "  ",
          # Air temperature, K (TEMPK)
          station_data_frames[[j]][[1]]$TEMP[i],
          "  ",
          # Relative humidity, % (IRH)
          station_data_frames[[j]][[1]]$RH[i],
          "  ",
          # Station pressure, mb (PRES)
          station_data_frames[[j]][[1]]$ATM.PRES[i],
          "  ",
          # Precipitation code (IPCODE) 
          #   0 = no precipitation
          #   1-18 = liquid precipitation
          #   19-45 = frozen precipitation
          station_data_frames[[j]][[1]]$PRECIP.CODE[i], file = paste(outputfile), append = TRUE)
      
      cat("", file = paste(outputfile), sep = "\n", append = TRUE) } }
  #
  #
}
