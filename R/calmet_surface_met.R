#' Obtain surface meteorology and generate input files
#' @description Obtain surface meteorology and generate input files
#' @param location_name an assigned name for the CALMET domain.
#' @param year the year for the SURF.DAT file.
#' @param lat_dec_deg the latitude of the CALMET domain in decimal degrees. The location of this point is defined in the lat_long_grid_loc argument.
#' @param lon_dec_deg the longitude of the CALMET domain in decimal degrees. The location of this point is defined in the lat_long_grid_loc argument.
#' @param lat_lon_grid_loc the location of the lat/long inputs in relation to the domain. Choices are: 1 (center), 2 (lower left), 3 (lower right), 4 (upper left), 5 (upper right).
#' @param domain_width_m the width of the meteorological domain in meters.
#' @param domain_height_m the height of the meteorological domain in meters.
#' @param cell_resolution_m the grid cell resolution in meters.
#' @param time_offset the offset from UTC-00:00 in hours.
#' @param local_archive_dir a local path containing an archive of gzipped NCDC station data files.
#' @param use_CSV_files option to use local, pre-processed CSV files.
#' @export calmet_surface_met
#' @examples
#' \dontrun{
#' # Generate a surface station file
#' calmet_surface_met(location_name = 'test',
#'                    year = 2005,
#'                    lat_dec_deg = 49.196116,
#'                    lon_dec_deg = -122.505866,
#'                    lat_lon_grid_loc = 1,
#'                    domain_width_m = 117000,
#'                    domain_height_m = 43250,
#'                    cell_resolution_m = 500,
#'                    time_offset = -8)
#'}

calmet_surface_met <- function(location_name,
                               year,
                               lat_dec_deg = NULL,
                               lon_dec_deg = NULL,
                               lat_lon_grid_loc = 1,
                               domain_width_m = NULL,
                               domain_height_m = NULL,
                               cell_resolution_m = 250,
                               time_offset,
                               local_archive_dir = NULL,
                               use_CSV_files = NULL){
  
  # Add require statements
  require(lubridate)
  require(plyr)
  require(raster)
  require(rgdal)
  
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
  
  # Project as UTM coordinates from the determined UTM zone, round to cell resolution using the
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
  
  # Generate an output file name for the SURF.DAT file
  output_file <- paste("surf--", location_name, "-",
                       number_cells_across_x, "x",
                       number_cells_across_y, "x",
                       cell_resolution_m, "--",
                       year, ".txt", sep = '')
  
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
  
  if (is.null(use_CSV_files)){
    
    # Get all surface met data, write CSV files to the working folder, get a vector
    # list of the generate CSV files
    CSV_files <- calmet_get_ncdc_station_data(year = year,
                                              bbox_lat_lon = bbox_lat_lon,
                                              local_archive_dir = local_archive_dir)
        
    # Get additional files to compensate for missing data at either the beginning or end of
    # the year (since datasets are standardized to UTC-00:00)
    if (time_offset < 0 & year < as.numeric(format(Sys.time(), "%Y"))){
      
        for (i in 1:length(CSV_files)){
          calmet_get_ncdc_station_data(data_filename = gsub("([0-9]*-[0-9]*-)[0-9]*.csv",
                                                            paste("\\1", year + 1, ".gz", sep = ''),
                                                            CSV_files[i]),
                                       local_archive_dir = local_archive_dir)
        }
    }
    
    #     if (time_offset > 0){
    #       for (i in seq((start_year - 1), end_year, 1)){
    #         if (i == start_year) CSV_files <- vector(mode = "character", length = 0)
    #         CSV_files_year <- list.files(path = ".", pattern = paste("[0-9]*-[0-9]*-", i, ".csv", sep = ''))
    #         CSV_files <- c(CSV_files, CSV_files_year)
    #       }
    #     }
    
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
    CSV_files <- list.files(path = ".", pattern = "^[0-9]*-[0-9]*.csv")
    
    # Define the start and end times and determine number of hours in each year
    start_time <- ISOdatetime(year, 1, 1, hour = 0, min = 0, sec = 0, tz = "GMT")
    end_time <- ISOdatetime(year, 12, 31, hour = 24, min = 0, sec = 0, tz = "GMT")
    time_difference <- difftime(end_time, start_time, units = 'hours')
    total_hours <- time_difference[[1]]
    
    # Get key station information as a character vector
    for (i in 1:length(CSV_files)){
      
      # Initialize the vector object
      if (i == 1) station_information_strings <- vector(mode = "character", length = 0)
      
      # Create new identifiers for the stations
      identifier_1 <- paste(formatC(i, digits = 3, flag = "0"))
      identifier_2 <- paste(formatC(i, digits = 5, flag = "0"))
      
      # Obtain the station's latitude and longitude
      station_lat <- read.csv(CSV_files[i], header = TRUE, stringsAsFactors = FALSE)[1,8]
      station_lon <- read.csv(CSV_files[i], header = TRUE, stringsAsFactors = FALSE)[1,9]
      
      # Reproject the lat/lon coordinates as UTM coordinates
      station_lat_lon_dec_deg <- cbind(station_lon, station_lat)
      station_UTM <- project(station_lat_lon_dec_deg, proj_string_UTM)
      station_UTM_easting_km <- round((station_UTM[1,1] / 1000), digits = 3)
      station_UTM_northing_km <- round((station_UTM[1,2] / 1000), digits = 3) 
      
      # Get the station's time offset and format the value for use in CALMET
      station_time_offset <- -(time_offset)
      
      # Set a fixed anenometer height of 20 m AGL
      station_anenometer_height <- 20
      
      # Format the values into a character string
      station_values_string <- paste(identifier_1, " ",
                                     identifier_2, " ",
                                     station_UTM_easting_km, " ",
                                     station_UTM_northing_km, " ",
                                     station_time_offset, " ",
                                     station_anenometer_height,
                                     sep = "")
      
      station_information_strings <- c(station_information_strings,
                                       station_values_string)
      
    }
    
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
        subset(station_data_frames[[i]][[1]], YR >= year & YR <= year) # 8741
      
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
                           missing_temp, missing_dewpoint, missing_atm.pres, missing_sky.cover,
                           missing_precip.rate, missing_precip.rh, missing_precip.code,
                           missing_times)
      
      # Create matrix of vector objects
      missing_df <- as.data.frame(matrix(missing_vectors,
                                         nrow = length(missing_times),
                                         ncol = 21),
                                  stringsAsFactors = FALSE)
      
      
      colnames(missing_df) <- c("USAFID", "WBAN", "YR", "M", "D", "HR", "MIN",
                                "LAT", "LONG", "ELEV", "WIND.DIR", "WIND.SPD",
                                "CEIL.HGT", "TEMP", "DEW.POINT", "ATM.PRES",
                                "SKY.COVER", "PRECIP.RATE", "RH", "PRECIP.CODE",
                                "time_series")
            
      missing_df$time_series <- as.POSIXct(missing_df$time_series,
                                           origin = "1970-01-01",
                                           tz = "GMT")
      
      # Row bind missing data frame with the available data frame
      station_data_frames[[i]][[1]] <- rbind(station_data_frames[[i]][[1]],
                                             missing_df)
      
      # Reorder data frame by POSIXct timestamp
      station_data_frames[[i]][[1]] <-
        station_data_frames[[i]][[1]][order(station_data_frames[[i]][[1]]$time_series),]
    }
    
  }
  
  if (!is.null(use_CSV_files)){
    print("Not NULL.")
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
  
  # Add line 1 to file header (dataset name [SURF.DAT], dataset version [2.1],
  # dataset message field)
  cat("SURF.DAT        2.1             Hour Start and End Times with Seconds",
      file = paste(output_file), sep = "\n", append = TRUE)
  
  # Add line 2 to file header (number of comment lines to follow)
  cat(as.character(length(station_information_strings) + 2),
      file = paste(output_file), sep = "\n", append = TRUE)
  
  # Add line 3 to file header
  cat("Produced by PuffR !Do not edit by hand!",
      file = paste(output_file), sep = "\n", append = TRUE)

  # Add line 4 to file header
  cat("Station Information:",
      file = paste(output_file), sep = "\n", append = TRUE)
  
  # Add station information to file header
  cat(station_information_strings,
      file = paste(output_file), sep = "\n", append = TRUE)
  
  # Add map projection keyword [NONE] to file header
  cat("NONE",
      file = paste(output_file), sep = "\n", append = TRUE)
  
  # Add time zone to file header
  cat(paste("UTC", ifelse(time_offset >=0, "+", "-"),
            formatC(abs(time_offset), width = 2, flag = "0"), "00", sep = ''),
      file = paste(output_file), sep = "\n", append = TRUE)
  
  # Add time and station info to file header (Beginning and end times for
  # file, number of met stations)
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
  
  # Add 5-digit identifiers for met stations used in file
  for (i in 1:length(station_data_frames)){
    
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
      
      cat(# Wind speed, m/s (WS)
          format(station_data_frames[[j]][[1]]$WIND.SPD[i],
                 width = 6, justify = "right"),
          " ",
          # Wind direction, degrees (WD)
          format(station_data_frames[[j]][[1]]$WIND.DIR[i],
                 width = 6, justify = "right"),
          " ",
          # Ceiling height, hundreds of feet (ICEIL)
          format(station_data_frames[[j]][[1]]$CEIL.HGT[i],
                 width = 6, justify = "right"),
          " ",
          # Opaque sky cover, tenths (ICC)
          format(station_data_frames[[j]][[1]]$SKY.COVER[i],
                 width = 6, justify = "right"),
          " ",
          # Air temperature, K (TEMPK)
          format(station_data_frames[[j]][[1]]$TEMP[i],
                 width = 6, justify = "right"),
          " ",
          # Relative humidity, % (IRH)
          format(station_data_frames[[j]][[1]]$RH[i],
                 width = 6, justify = "right"),
          " ",
          # Station pressure, mb (PRES)
          format(station_data_frames[[j]][[1]]$ATM.PRES[i],
                 width = 6, justify = "right"),
          " ",
          # Precipitation code (IPCODE) 
          #   0 = no precipitation
          #   1-18 = liquid precipitation
          #   19-45 = frozen precipitation
          format(station_data_frames[[j]][[1]]$PRECIP.CODE[i],
                 width = 6, justify = "right"),
          file = paste(output_file), append = TRUE)
      
      cat("", file = output_file, sep = "\n", append = TRUE)
      
    } 
    
  }
  
}
