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
                               domain_height_m = NULL,
                               time_offset,
                               output_file = "surf.dat"){
  
  # Add require statements
  require(lubridate)
  require(plyr)
  require(raster)
  
  # Define the cell resolution (square cells) as 250 m
  cell_resolution_m <- 250
  
  # Round the provided width and the height of the met domain to the resolution of the cell
  domain_width_m <- round_any(domain_width_m, cell_resolution_m, round)
  domain_height_m <- round_any(domain_height_m, cell_resolution_m, round)
  
  # Get matrix of longitude and latitude for chosen point
  lat_lon_dec_deg <- cbind(lon_dec_deg, lat_dec_deg)
  
  # Determine the UTM zone
  UTM_zone <- (floor((lon_dec_deg + 180)/6) %% 60) + 1
  
  # Determine whether domain is in Northern Hemisphere or Southern Hemisphere
  UTM_hemisphere <- ifelse(lat_dec_deg >= 0, "N", "S")
  
  # Define a PROJ.4 projection string for a lat/lon projection
  proj_string_longlat <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  
  # Define a PROJ.4 projection string for a UTM projection
  proj_string_UTM <- paste("+proj=utm +zone=",
                           UTM_zone,
                           " +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
                           sep = '')
  
  # Project as UTM coordinates from the determined UTM zone, round to nearest 250 m using the
  # 'round_any' function from the 'plyr' package
  UTM_location <- project(lat_lon_dec_deg, proj_string_UTM)
  UTM_location <- round_any(UTM_location, cell_resolution_m, round)
  
  # Do these length and width values accomodate an integer number of cells of the specified resolution?
  # These checks will be later part of a function in setting domain width and height
  is_number_cells_across_x_an_int <- ifelse(domain_width_m %% cell_resolution_m != 0, FALSE, TRUE)
  is_number_cells_across_y_an_int <- ifelse(domain_height_m %% cell_resolution_m != 0, FALSE, TRUE)
  
  # Get the number of cells in the x direction
  number_cells_across_x <- ifelse(is_number_cells_across_x_an_int == TRUE,
                                  domain_width_m/cell_resolution_m, NULL)
  
  # Get the number of cells in the y direction
  number_cells_across_y <- ifelse(is_number_cells_across_y_an_int == TRUE,
                                  domain_height_m/cell_resolution_m, NULL)
  
  # Get the total number of cells
  total_cells <- number_cells_across_x * number_cells_across_y
  
  # Get extents of UTM grid (left, right, bottom, top) in meters
  left_UTM <- get_grid_extents_UTM(side = "left",
                                   lat_lon_grid_loc = lat_lon_grid_loc,
                                   UTM_location = UTM_location,
                                   domain_width_m = domain_width_m,
                                   domain_height_m = domain_height_m)
  
  right_UTM <- get_grid_extents_UTM(side = "right",
                                    lat_lon_grid_loc = lat_lon_grid_loc,
                                    UTM_location = UTM_location,
                                    domain_width_m = domain_width_m,
                                    domain_height_m = domain_height_m)
  
  bottom_UTM <- get_grid_extents_UTM(side = "bottom",
                                     lat_lon_grid_loc = lat_lon_grid_loc,
                                     UTM_location = UTM_location,
                                     domain_width_m = domain_width_m,
                                     domain_height_m = domain_height_m)
  
  top_UTM <- get_grid_extents_UTM(side = "top",
                                  lat_lon_grid_loc = lat_lon_grid_loc,
                                  UTM_location = UTM_location,
                                  domain_width_m = domain_width_m,
                                  domain_height_m = domain_height_m)
  
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
  
  # Generate Extents object in lat/lon projection
  bbox_lat_lon <- extent(LL_LR_UL_UR_longlat_SP)
  
  # Get all surface met data and write CSV files to the working folder
  stations <- calmet_get_ncdc_station_data(start_year = start_year,
                                           end_year = end_year,
                                           bbox_lat_lon = bbox_lat_lon)
  
  if (is.logical(stations) & stations == FALSE){
    stop("There are no stations in the selected domain")
  }
  
  # Generate a file list for the newly-generated CSV files
  CSV_files <- list.files(path = ".", pattern = "[0-9]*-[0-9]*-[0-9]*.csv")
  
  # Get lists of files, grouped by station identifier
  CSV_files_unique_stations <- unique(gsub("([0-9]*-[0-9]*)-[0-9]*.csv", "\\1", CSV_files))
  
  # Join years of station data together
  for (i in 1:length(CSV_files_unique_stations)){
    CSV_station_years <- list.files(path = ".",
                                    pattern = paste(CSV_files_unique_stations[i],
                                                    "-.*.csv", sep = ''))
    for (j in 1:length(CSV_station_years)){
      if (j == 1){
        CSV_all_years_at_station <- read.csv(CSV_station_years[j],
                                             header = TRUE,
                                             stringsAsFactors = FALSE)
      }
      if (j > 1){
        CSV_single_year_at_station <- read.csv(CSV_station_years[j],
                                               header = TRUE,
                                               stringsAsFactors = FALSE)
        CSV_all_years_at_station <- rbind(CSV_all_years_at_station,
                                          CSV_single_year_at_station)
      }
      if (j == length(CSV_station_years))
        write.csv(CSV_all_years_at_station,
                  file = paste(CSV_files_unique_stations[i], ".csv", sep = ""),
                  row.names = FALSE)
    }
  }
  
  # Clean up folder
  file.remove(CSV_files, gsub(".csv", "", CSV_files))
  
  # Generate a new list of CSV files
  CSV_files <- list.files(path = ".", pattern = "[0-9]*-[0-9]*.csv")
  
  # Define the start and end times and determine number of hours in each year
  start_time <- ISOdatetime(start_year, 1, 1, hour = 0, min = 0, sec = 0, tz = "GMT")
  end_time <- ISOdatetime(end_year, 12, 31, hour = 24, min = 0, sec = 0, tz = "GMT")
  time_difference <- difftime(end_time, start_time, units = 'hours')
  total_hours <- time_difference[[1]]
  
  # Create the hourly time series as a list of POSIXct time objects
  time_series <- as.list(c(1:total_hours))
  for (i in 1:total_hours) {
    time_series[[i]] <- start_time + (3600 * (i - 1))
  }
  
  # Use CSV_files to extract data from specified stations
  station_data_frames <- as.list(c(1:length(CSV_files)))
  
  for (i in 1:length(CSV_files)){
    station_data_frames[[i]] <- list(read.csv(CSV_files[i], header = TRUE))
  }
  
  # For every station, change UTC times to local times using the specified time offset
  for (i in 1:length(station_data_frames)){
   
    POSIXdatetime <- ISOdatetime(year = station_data_frames[[i]][[1]]$YR,
                                 month = station_data_frames[[i]][[1]]$M,
                                 day = station_data_frames[[i]][[1]]$D,
                                 hour = station_data_frames[[i]][[1]]$HR,
                                 min = station_data_frames[[i]][[1]]$MIN,
                                 sec = 0,
                                 tz = "GMT")
    
    # Shift POSIXct timestamps by specified time offset
    POSIXdatetime <- POSIXdatetime + (time_offset * 3600)
    
    # Extract time elements from 'POSIXdatetime' object and replace in df
    station_data_frames[[i]][[1]]$YR <- year(POSIXdatetime)
    station_data_frames[[i]][[1]]$M <- month(POSIXdatetime)
    station_data_frames[[i]][[1]]$D <- day(POSIXdatetime)
    station_data_frames[[i]][[1]]$HR <- hour(POSIXdatetime)
    station_data_frames[[i]][[1]]$MIN <- minute(POSIXdatetime)
    
    # Trim data frame to only contain target times
    station_data_frames[[i]][[1]] <- 
      subset(station_data_frames[[i]][[1]], YR >= start_year & YR <= end_year) # 8741
    
    # Obtain new vector of POSIXct timestamps
    POSIXdatetime <- ISOdatetime(year = station_data_frames[[i]][[1]]$YR,
                                 month = station_data_frames[[i]][[1]]$M,
                                 day = station_data_frames[[i]][[1]]$D,
                                 hour = station_data_frames[[i]][[1]]$HR,
                                 min = station_data_frames[[i]][[1]]$MIN,
                                 sec = 0,
                                 tz = "GMT")
    
    # Scan for missing hours by comparing against all available hours in year
    time_series_vector <- as.POSIXct(unlist(time_series),
                                     origin = "1970-01-01",
                                     tz = "GMT")
    
    station_data_frames[[i]][[1]]$time_series <- POSIXdatetime
    
    missing_times <- time_series_vector[which(!(time_series_vector %in% POSIXdatetime))]
        
    # Create data frame with missing values
    missing_usafid <- rep(unique(station_data_frames[[i]][[1]][,1]), length(missing_times))
    missing_wban <- rep(unique(station_data_frames[[i]][[1]][,2]), length(missing_times))
    missing_yr <- year(missing_times)
    missing_m <- month(missing_times)
    missing_d <- day(missing_times)
    missing_hr <- hour(missing_times)
    missing_min <- rep(0, length(missing_times))
    missing_lat <- rep(station_data_frames[[i]][[1]][1,8], length(missing_times))
    missing_long <- rep(station_data_frames[[i]][[1]][1,9], length(missing_times))
    missing_elev <- rep(station_data_frames[[i]][[1]][1,10], length(missing_times))
    missing_wind.dir <- rep(999, length(missing_times))
    missing_wind.spd <- rep(999.9, length(missing_times))
    missing_ceil.hgt <- rep(999.9, length(missing_times))
    missing_temp <- rep(999.9, length(missing_times))
    missing_dewpoint <- rep(999.9, length(missing_times))
    missing_atm.pres <- rep(999.9, length(missing_times))
    missing_sky.cover <- rep(999, length(missing_times))
    missing_precip.rate <- rep(999, length(missing_times))
    missing_precip.rh <- rep(999, length(missing_times))
    missing_precip.code <- rep(999, length(missing_times))
    
    # Create vector of vector objects
    missing_vectors <- c(missing_usafid, missing_wban, missing_yr, missing_m,
                         missing_d, missing_hr, missing_min, missing_lat, missing_long,
                         missing_elev, missing_wind.dir, missing_wind.spd, missing_ceil.hgt,
                         missing_temp, missing_dewpoint, missing_atm.pres, missing_precip.rate,
                         missing_precip.rh, missing_precip.code, missing_times)
    
    # Create matrix of vector objects
    missing_df <- as.data.frame(matrix(missing_vectors,
                                       nrow = length(missing_times),
                                       ncol = 20),
                                stringsAsFactors = FALSE)
    
    
    colnames(missing_df) <- colnames(station_data_frames[[i]][[1]])
    
    missing_df$time_series <- as.POSIXct(missing_df$time_series,
                                         origin = "1970-01-01",
                                         tz = "GMT")
    
    #     for (j in 1:ncol(station_data_frames[[i]][[1]])){
    #       missing_df[,j] <- missing_vectors[j]
    #     }
    
    # Row bind missing data frame with the available data frame
    station_data_frames[[i]][[1]] <- rbind(station_data_frames[[i]][[1]],
                                           missing_df)
    
    # Reorder data frame by POSIXct timestamp
    station_data_frames[[i]][[1]] <-
      station_data_frames[[i]][[1]][order(station_data_frames[[i]][[1]]$time_series),]
  }

  # Validate the individual CSV files, present a table several pieces of
  # information related to the data availablity and the data quality
  # 1. Elements where data is available
  # 2. Data completeness for each of the elements
  # 3. Whether the station has cloud opacity and ceiling height fields
  # 4. A data completeness and data quality score (from 1 to 10)
  
  # Save the data frame to disk as a CSV file
  
  # Ask the user which files to use, or, ask what DC/DQ is the minimum for
  # excluding stations
  
  # Generate a list of CSV files that are accepted by the user
  
  # Determine whether the selected stations, when taken together, provide
  # the minimum data completeness for the SURF.DAT file
  
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
  cat("", file = paste(output_file))
  
  # Add line 1 to file header (dataset name [SURF.DAT], dataset version [2.1], dataset message field)
  cat("SURF.DAT        2.1             Hour Start and End Times with Seconds",
      file = paste(output_file), sep = "\n", append = TRUE)
  
  # Add line 2 to file header (number of comment lines to follow)
  cat("1",
      file = paste(output_file), sep = "\n", append = TRUE)
  
  # Add line 3 to file header (single comment line)
  cat("Produced using PuffR",
      file = paste(output_file), sep = "\n", append = TRUE)
  
  # Add line 4 to file header (map projection [NONE])
  cat("NONE",
      file = paste(output_file), sep = "\n", append = TRUE)
  
  # Add line 5 to file header (time zone)
  cat(paste("UTC", ifelse(time_offset >=0, "+", "-"),
            formatC(abs(time_offset), width = 2, flag = "0"), "00", sep = ''),
      file = paste(output_file), sep = "\n", append = TRUE)
  
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
      length(CSV_files),
      file = paste(output_file), sep = '', append = TRUE)
  
  cat("", file = paste(output_file), sep = "\n", append = TRUE)
  
  # Add line 7- to file (5-digit identifiers for met stations used in file)
  for (i in 1:length(station_data_frames)) {
    
    cat(paste(formatC(i, digits = 5, flag = "0")),
        file = output_file, sep = "\n", append = TRUE)
    
  }
  
  # Construct the body of the SURF.DAT file using a nested loops that provide grouped time interval
  # headers with ordered surface station parameters
  for (i in 1:total_hours){
    
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
        hour(time_series[[i]] + 3600), file = paste(output_file), append = TRUE)
    
    cat("", file = paste(output_file), sep = "\n", append = TRUE)
    
    for (j in 1:length(station_data_frames)){
      
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
          station_data_frames[[j]][[1]]$PRECIP.CODE[i],
          file = paste(output_file), append = TRUE)
      
      cat("", file = output_file, sep = "\n", append = TRUE)
      
    } 
    
  }
  
}
