#' Create an upper air data file for CALMET
#' @description This function retrieves upper air sounding data and processes it to generate an upper air input file for CALMET.
#' @param location_name an assigned name for the CALMET domain.
#' @param year the year for the UP.DAT file.
#' @param lat_dec_deg the latitude of the CALMET domain in decimal degrees. The location of this point is defined in the lat_long_grid_loc argument.
#' @param lon_dec_deg the longitude of the CALMET domain in decimal degrees. The location of this point is defined in the lat_long_grid_loc argument.
#' @param lat_lon_grid_loc the location of the lat/long inputs in relation to the domain. Choices are: 1 (center), 2 (lower left), 3 (lower right), 4 (upper left), 5 (upper right).
#' @param domain_width_m the width of the meteorological domain in meters.
#' @param domain_height_m the height of the meteorological domain in meters.
#' @param cell_resolution_m the grid cell resolution in meters.
#' @param time_offset the offset from UTC-00:00 in hours.
#' @export calmet_upper_air
#' @examples
#' \dontrun{
#' # Generate an upper air station file
#' calmet_upper_air(location_name = 'test',
#'                  year = 2005,
#'                  lat_dec_deg = 49.196116,
#'                  lon_dec_deg = -122.505866,
#'                  lat_lon_grid_loc = 1,
#'                  domain_width_m = 117000,
#'                  domain_height_m = 43250,
#'                  cell_resolution_m = 500,
#'                  time_offset = -8)
#'}

calmet_upper_air <- function(location_name,
                             year,
                             lat_dec_deg = NULL,
                             lon_dec_deg = NULL,
                             lat_lon_grid_loc = 1,
                             domain_width_m = NULL,
                             domain_height_m = NULL,
                             cell_resolution_m = 250,
                             time_offset){
  
  # Include require statements
  require(RCurl)
  require(stringr)
  require(plyr)
  require(raster)
  require(rgdal)
  require(lubridate)
  
  ####
  # Get bounding box for domain
  ####
  
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
  
  # Generate an output file name for the UP.DAT file
  output_file <- paste("up--", location_name, "-",
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
  
  ####
  # Get dataframe with all sounding stations
  ####
  
  # Obtain the HTML source from a URI containing a query
  URI <- getURL(paste("http://www.esrl.noaa.gov/raobs/intl/GetRaobs.cgi?",
                      "shour=All+Times&ltype=All+Levels&wunits=Tenths+of+Meters%2FSecond",
                      "&bdate=1990010100&edate=2013122523&access=All+Sites&view=YES&",
                      "osort=Station+Series+Sort&oformat=FSL+format+%28ASCII+text%29",
                      sep = ''))
  
  if(grepl("Service Temporarily Unavailable", URI) == TRUE) {
    stop("The NOAA DB server is reporting that it's temporarily unavailable.")
  }
  
  # Create a 'pattern' string object cdontaining the regex pattern for extracting
  # sounding data strings from the URI
  pattern <- paste("<OPTION> [0-9A-Z]*[ ]*[0-9]* [0-9]{5} [0-9/.-]*",
                   "[0-9/.-]* [0-9-]{5,6}  [.]*  [0-9A-Z]{2} [0-9A-Z]{2}",
                   sep = '')
  
  # Generate vector list of strings from URI page source
  sounding_lines <- gsub(pattern = pattern, replacement = "\\1",
                         x = URI)
  
  sounding_lines <- gsub(pattern = ".*MULTIPLE SIZE=\"10\">\n", replacement = "",
                         x = sounding_lines)
  
  sounding_lines <- gsub(pattern = "\n\n</SELECT>.*", replacement = "",
                         x = sounding_lines)
  
  sounding_lines <- gsub(pattern = "<OPTION> ", replacement = "",
                         x = sounding_lines)
  
  sounding_lines <- unlist(str_split(sounding_lines, "\n\n"))
  
  # Initialize the data objects
  # Loop through list of strings, extract and clean the substrings corresponding to data elements
  # Create a data frame with vector lists and coerce some objects into numeric objects
  for (i in 1:length(sounding_lines)){
    if (i == 1) {
      init <- mat.or.vec(nr = length(sounding_lines), nc = 1)
      wban <- mat.or.vec(nr = length(sounding_lines), nc = 1)
      wmo <- mat.or.vec(nr = length(sounding_lines), nc = 1)
      lat <- mat.or.vec(nr = length(sounding_lines), nc = 1)
      lon <- mat.or.vec(nr = length(sounding_lines), nc = 1)
      elev <- mat.or.vec(nr = length(sounding_lines), nc = 1)
      station_name <- mat.or.vec(nr = length(sounding_lines), nc = 1)
      prov_state <- mat.or.vec(nr = length(sounding_lines), nc = 1)
      country <- mat.or.vec(nr = length(sounding_lines), nc = 1)
    }
    
    init[i] <- 
      str_match(string = sounding_lines[i],
                pattern = "^([0-9A-Z]*)")[1,2]
    
    wban[i] <- 
      str_match(string = sounding_lines[i],
                pattern = "^[0-9A-Z]+[ ]+([0-9]*)")[1,2]
    
    wmo[i] <- 
      str_match(string = sounding_lines[i],
                pattern = "^[0-9A-Z]+[ ]+[0-9]* ([0-9]{5})")[1,2]
    
    lat[i] <- 
      as.numeric(str_match(string = sounding_lines[i],
                           pattern = paste("^[0-9A-Z]+[ ]+[0-9]* ",
                                           "[0-9]{5} ([0-9/.-]*)", sep = ''))[1,2])
    
    lon[i] <- 
      as.numeric(str_match(string = sounding_lines[i],
                           pattern = paste("^[0-9A-Z]+[ ]+[0-9]* [0-9]{5} ",
                                           "[0-9/.-]* ([0-9/.-]*)", sep = ''))[1,2])
    
    elev[i] <- 
      as.numeric(str_match(string = sounding_lines[i],
                           pattern = paste("^[0-9A-Z]+[ ]+[0-9]* ",
                                           "[0-9]{5} [0-9/.-]* [0-9/.-]* ",
                                           "([0-9-]{5,6})", sep = ''))[1,2])
    
    station_name[i] <- 
      str_trim(str_match(string = sounding_lines[i],
                         pattern = paste("^[0-9A-Z]+[ ]+[0-9]* ",
                                         "[0-9]{5} [0-9/.-]* [0-9/.-]* ",
                                         "[0-9-]{5,6}  (.+) [0-9A-Z]{2} ",
                                         "[0-9A-Z]{2}$", sep = ''))[1,2],
               side = "both")
    
    prov_state[i] <- 
      str_match(string = sounding_lines[i],
                pattern = paste("^[0-9A-Z]+[ ]+[0-9]* ",
                                "[0-9]{5} [0-9/.-]* [0-9/.-]* ",
                                "[0-9-]{5,6}  .+ ([0-9A-Z]{2}) ",
                                "[0-9A-Z]{2}$", sep = ''))[1,2]
    
    country[i] <- 
      str_match(string = sounding_lines[i],
                pattern = paste("^[0-9A-Z]+[ ]+[0-9]* ",
                                "[0-9]{5} [0-9/.-]* [0-9/.-]* ",
                                "[0-9-]{5,6}  .+ [0-9A-Z]{2} ",
                                "([0-9A-Z]{2})$", sep = ''))[1,2]
    
    if (i == length(sounding_lines)) {
      # Create data frame with vector objects of equal length 
      df_soundings <- as.data.frame(cbind(init, wban, wmo, lat, lon,
                                          elev, station_name, prov_state,
                                          country), stringsAsFactors = FALSE)
      
      # Change object class for lat, lon, and elev in 'df_soundings' data frame
      df_soundings[,4] <- as.numeric(df_soundings[,4])
      df_soundings[,5] <- as.numeric(df_soundings[,5])
      df_soundings[,6] <- as.numeric(df_soundings[,6])
      
      # Remove objects
      rm(i, init, wban, wmo, lat, lon, elev, station_name, prov_state,
         country, URI, pattern, sounding_lines)
    }
  }
  
  ####
  # Get the sounding data for both the primary and secondary stations
  ####
  
  # Get start date and end dates
  start_date <- paste(year, "-01-01", sep = "")
  end_date <- paste(year, "-12-31", sep = "")
  
  # Set 'shour' as "0z%2C+12z+ONLY"
  shour <- "0z%2C+12z+ONLY"
  
  # Set 'ltype' as "All+Levels"
  ltype <- "All+Levels"
  
  # Set 'wunits' as "Tenths+of+Meter%2FSecond"
  wunits <- "Tenths+of+Meter%2FSecond"
  
  # Get formatted beginning date
  bdate <- paste(str_replace_all(start_date, "-", ""), "00", sep = '')
  
  # Get formatted ending date
  edate <- paste(str_replace_all(end_date, "-", ""), "23", sep = '')
  
  # Generate a data frame containing WBAN-WMO keys
  wban_wmo_list <- as.vector(paste(df_soundings$wban, "-", df_soundings$wmo, sep = ''))
  
  # Create a subset of the df_sounding data frame through constraining by the
  # bounding box
  df_soundings_domain <- subset(df_soundings,
                                df_soundings$lat >= bbox_lat_lon@ymin &
                                  df_soundings$lat <= bbox_lat_lon@ymax &
                                  df_soundings$lon >= bbox_lat_lon@xmin &
                                  df_soundings$lon <= bbox_lat_lon@xmax)
  
  # Obtain the primary sounding station for the domain by expanding the bounding
  # box until a single station is captured
  if (nrow(df_soundings_domain) < 1){
    
    # Set the initial degree increment to 0
    deg_increment <- 0
    
    # Set the initial 'station_counter' value to be 1
    station_counter <- 1
    
    # Set the initial values for 'primary_file_valid' and 'secondary_file_valid' to FALSE
    primary_file_valid <- FALSE
    secondary_file_valid <- FALSE
    
    # Initialize the list of sounding stations that will be excluded if they don't
    # contain any valid data
    excluded_wban_wmo <- vector(mode = "character", length = 0)
    
    # Create repeat loop to capture and validate sounding station data 
    repeat {
      
      # Increment the bounding box dimensions by 0.1 degrees
      deg_increment <- deg_increment + 0.1
      
      # Obtain subset of 'df_soundings_domain' data frame
      df_soundings_domain <-
        subset(df_soundings,
               df_soundings$lat >= (bbox_lat_lon@ymin - deg_increment) &
                 df_soundings$lat <= (bbox_lat_lon@ymax + deg_increment) &
                 df_soundings$lon >= (bbox_lat_lon@xmin - deg_increment) &
                 df_soundings$lon <= (bbox_lat_lon@xmax + deg_increment))
      
      # When an entry exists in the data frame, download the file and
      # determine whether it contains sounding data
      if (nrow(df_soundings_domain) == station_counter){
        
        # Assign the captured sounding stations as candidates for the primary
        # sounding station
        primary_station_wban_wmo <- paste(df_soundings_domain[,2], "-",
                                          df_soundings_domain[,3], sep = '')
        
        # Exclude the stations that have been determined to contain no valid data
        primary_station_wban_wmo <- 
          primary_station_wban_wmo[which(!(primary_station_wban_wmo %in%
                                             excluded_wban_wmo))[1]]
        
        # Obtain the position of the primary sounding station in 'df_soundings'
        primary_station_wban_wmo_position <- match(primary_station_wban_wmo,
                                                   wban_wmo_list)
        
        # Download the FSL data file for the primary sounding station
        downloaded_primary_sounding_file <- 
          download_FSL_sounding_data(sounding_priority = "primary",
                                     df_soundings = df_soundings,
                                     station_list_position = primary_station_wban_wmo_position,
                                     starting_hour = shour,
                                     level_type = ltype,
                                     wind_units = wunits,
                                     beginning_date = bdate,
                                     ending_date = edate)
        
        # Determine if the file size is above 5000 bytes (valid data file);
        # add file
        if (file.info(downloaded_primary_sounding_file)$size > 5000){
          
          # Set the 'primary_file_valid' boolean value as TRUE
          primary_file_valid <- TRUE
          
        } else {
          
          # Set the 'primary_file_valid' boolean value as FALSE
          primary_file_valid <- FALSE
          
          # Add this sounding to the vector list of excluded soundings
          excluded_wban_wmo <- c(excluded_wban_wmo, primary_station_wban_wmo)
          
          # Increment the 'station_counter' value by 1
          station_counter <- station_counter + 1
          
          # Delete the invalid sounding file from the working directory
          file.remove(downloaded_primary_sounding_file)
        
        }
        
      }
      
      # Break from the repeat loop if a sounding file was downloaded and it contained
      # valid sounding data
      if (primary_file_valid == TRUE){
        print(paste("The sounding file for ", primary_station_wban_wmo, " was downloaded",
                    sep = ''))
        break
      }
      
    }
    
  }
  
  # Add the 'primary_station_wban_wmo' string to the 'excluded_wban_wmo' vector such
  # that the primary sounding station won't be used automatically as the
  # secondary station
  excluded_wban_wmo <- c(excluded_wban_wmo, primary_station_wban_wmo)
  
  repeat {
    
    # Use the retained 'deg_increment' value and continue incrementing by 0.1
    deg_increment <- deg_increment + 0.1
    
    # Obtain subset of 'df_soundings_domain' data frame
    df_soundings_domain <-
      subset(df_soundings,
             df_soundings$lat >= (bbox_lat_lon@ymin - deg_increment) &
               df_soundings$lat <= (bbox_lat_lon@ymax + deg_increment) &
               df_soundings$lon >= (bbox_lat_lon@xmin - deg_increment) &
               df_soundings$lon <= (bbox_lat_lon@xmax + deg_increment))
    
    # When an entry exists in the data frame, download the file and
    # determine whether it contains sounding data
    if (nrow(df_soundings_domain) == station_counter + 1){
      
      # Assign the captured sounding stations as candidates for the secondary
      # sounding station
      secondary_station_wban_wmo <- paste(df_soundings_domain[,2], "-",
                                          df_soundings_domain[,3], sep = '')
      
      # Assign the captured sounding station as the secondary sounding station,
      # excluding the stations that are either the primary sounding station or have
      # no valid data
      secondary_station_wban_wmo <- 
        secondary_station_wban_wmo[which(!(secondary_station_wban_wmo %in%
                                             excluded_wban_wmo))[1]]
      
      # Obtain the position of the primary sounding station in 'df_soundings'
      secondary_station_wban_wmo_position <- match(secondary_station_wban_wmo,
                                                   wban_wmo_list)
      
      # Download the FSL data file for the primary sounding station
      downloaded_secondary_sounding_file <- 
        download_FSL_sounding_data(sounding_priority = "secondary",
                                   df_soundings = df_soundings,
                                   station_list_position = secondary_station_wban_wmo_position,
                                   starting_hour = shour,
                                   level_type = ltype,
                                   wind_units = wunits,
                                   beginning_date = bdate,
                                   ending_date = edate)
      
      # Determine if the file size is above 5000 bytes (valid data file);
      # add file
      if (file.info(downloaded_secondary_sounding_file)$size > 5000){
        
        # Set the 'secondary_file_valid' boolean value as TRUE
        secondary_file_valid <- TRUE
        
      } else {
        
        # Set the 'secondary_file_valid' boolean value as FALSE
        secondary_file_valid <- FALSE
        
        # Add this sounding to the vector list of excluded soundings
        excluded_wban_wmo <- c(excluded_wban_wmo, secondary_station_wban_wmo)
        
        # Increment the 'station_counter' value by 1
        station_counter <- station_counter + 1
        
        # Delete the invalid sounding file from the working directory
        file.remove(downloaded_secondary_sounding_file)
      }
      
    }
    
    # Break from the repeat loop if a sounding file was downloaded and it contained
    # valid sounding data
    if (secondary_file_valid == TRUE){
      print(paste("The sounding file for ", secondary_station_wban_wmo, " was downloaded",
                  sep = ''))
      break
    }
    
  }
  
  # Generate the READ62 input file template in the working folder
  read62_inp_generate_template()
  
  # Add run control parameters to READ62 file, using defaults
  read62_01_run_control_params()
  
#   ####
#   # Process the upper air sounding data
#   ####
#   
#   # Determine number of soundings in file, necessary for initialization of list object
#   # Check for equal numbers of header lines (254, 1, 2, and 3)
#   soundings_254 <- length(grep(pattern = "^    254", x = sounding_data))
#   soundings_1 <- length(grep(pattern = "^      1", x = sounding_data))
#   soundings_2 <- length(grep(pattern = "^      2", x = sounding_data))
#   soundings_3 <- length(grep(pattern = "^      3", x = sounding_data))
#   
#   # The condition of full headers (having line types 254, 1, 2, and 3) is verified
#   # by seeing if the counts are all equal
#   full_headers <- ifelse(soundings_254 == soundings_1 &
#                            soundings_1 == soundings_2 &
#                            soundings_2 == soundings_3, TRUE, FALSE)
#   
#   correct_first_line <- ifelse(1 %in% grep(pattern = "^    254",
#                                            x = sounding_data),
#                                TRUE, FALSE)
#   
#   # If the full headers condition passes and the first line contains the first header line,
#   # the number of soundings is passed to the 'soundings' object
#   if (full_headers == TRUE & correct_first_line == TRUE) soundings <- soundings_254
#   
#   # Remove 'soundings_*' objects
#   rm(soundings_254, soundings_1, soundings_2, soundings_3)
#   
#   # Remove other objects from global workspace
#   rm(full_headers, correct_first_line)
#   
#   # Create list of soundings
#   sounding_list <- vector("list", soundings)
#   
#   # Each list element in sounding_list will have two data frames:
#   # (1) header information (e.g., time, sounding launch coordinates, etc.), and
#   # (2) sounding data (e.g., pressure, height, temperature, DPT, WD, WS, and
#   # sounding type)
#   
#   # Set iterator index (i) to 1; this corresponds the line number in 'sounding_data'
#   # vector list
#   i <- 1
#   
#   # Set iterator index (list_item) to 0; this corresponds to the sounding number (and the
#   # list number)
#   list_item <- 0
#   
#   # Use a while loop to cycle through the 'sounding_data' object and extract elements
#   while (i < length(sounding_data)){
#     
#     header_254 <- as.data.frame(do.call('rbind', 
#                                         str_split(gsub("^[ ]{1,3}", "",
#                                                        gsub("[ ]{2,6}", " ",
#                                                             sounding_data[i])), " ")),
#                                 stringsAsFactors = FALSE)
#     
#     colnames(header_254) <- c("lintyp_254", "hour", "day", "month", "year")
#     
#     # Recode month from 3-letter char object to a month number
#     header_254$month <- switch(header_254$month,
#                                "JAN" = 1,
#                                "FEB" = 2,
#                                "MAR" = 3,
#                                "APR" = 4,
#                                "MAY" = 5,
#                                "JUN" = 6,
#                                "JUL" = 7,
#                                "AUG" = 8,
#                                "SEP" = 9,
#                                "OCT" = 10,
#                                "NOV" = 11,
#                                "DEC" = 12)
#     
#     header_1 <- as.data.frame(do.call('rbind',
#                                       str_split(gsub("^[ ]{1,3}", "",
#                                                      gsub("[ ]{2,6}", " ",
#                                                           sounding_data[i + 1])), " ")),
#                               stringsAsFactors = FALSE)
#     
#     # Need to check and correct for those situations where the lat value is merged with
#     # the lon value
#     if (grepl("W", header_1$V4) | grepl("E", header_1$V4)){
#       
#       # Get regex pattern using look-around assertions
#       if (grepl("N", header_1$V4)) split_pattern <- "(?<=N)(?=[0-9])"
#       if (grepl("S", header_1$V4)) split_pattern <- "(?<=S)(?=[0-9])"
#       
#       # Apply 'strsplit' to vector item and obtain the separated lat/lon values
#       lat <- unlist(strsplit(header_1$V4, split_pattern, perl = TRUE))[1]
#       lon <- unlist(strsplit(header_1$V4, split_pattern, perl = TRUE))[2]
#       
#       # Add column 7 to 'header_1'
#       V7 <- 0
#       header_1 <- cbind(header_1, V7)
#       
#       # Move columns 5 & 6 to columns 6 & 7
#       header_1$V7 <- header_1$V6
#       header_1$V6 <- header_1$V5
#       
#       # Copy lat and lon values to columns 4 & 5
#       header_1$V4 <- lat
#       header_1$V5 <- lon
#       
#       # Remove variables
#       rm(split_pattern, lat, lon, V7)
#     }
#     
#     colnames(header_1) <- c("lintyp_1", "wban", "wmo", "lat",
#                             "lon", "elev", "rtime")
#     
#     header_2 <- as.data.frame(do.call('rbind',
#                                       str_split(gsub("^[ ]{1,3}", "",
#                                                      gsub("[ ]{2,6}", " ",
#                                                           sounding_data[i + 2])), " ")),
#                               stringsAsFactors = FALSE)
#     
#     colnames(header_2) <- c("lintyp_2", "hydro", "mxwd", "tropl",
#                             "lines", "tindex", "source")
#     
#     header_3 <- as.data.frame(do.call('rbind',
#                                       str_split(gsub("^[ ]{1,3}", "",
#                                                      gsub("[ ]{2,20}", " ",
#                                                           sounding_data[i + 3])), " ")),
#                               stringsAsFactors = FALSE)
#     
#     colnames(header_3) <- c("lintyp_3", "staid", "sonde", "wsunits")
#     
#     header <- cbind(header_254, header_1, header_2, header_3)
#     
#     header[, c(1:4,9:16)] <- sapply(header[, c(1:4,9:16)], as.numeric)
#     
#     header$lintyp_254 <- NULL
#     header$lintyp_1 <- NULL
#     header$lintyp_2 <- NULL
#     header$lintyp_3 <- NULL
#     
#     # Increment the 'list_item' vector, corresponding to the sounding number
#     list_item <- list_item + 1
#     
#     # Store the header information as a data frame in the first list index
#     sounding_list[[list_item]][[1]] <- header
#     
#     for (j in (i + 4):(i + as.numeric(header$lines) - 1)) {
#       
#       # Initialize the data frame for the sounding data
#       if (j == (i + 4)){
#         data <- as.data.frame(mat.or.vec(nr = as.numeric(header$lines) - 4,
#                                          nc = 7))
#       }
#       
#       data[(j - 3 - i),] <- 
#         as.data.frame(do.call('rbind',
#                               str_split(gsub("^[ ]{1,3}", "",
#                                              gsub("[ ]{2,20}", " ",
#                                                   sounding_data[j])), " ")),
#                       stringsAsFactors = FALSE)
#       
#       if (j == (i + as.numeric(header$lines) - 1)) {
#         colnames(data) <- c("lintyp", "pressure", "height", "temp",
#                             "dewpt", "wind_dir", "wind_speed")
#         data$temp <- as.numeric(data$temp) / 10
#         data$dewpt <- as.numeric(data$dewpt) / 10
#         data$wind_speed <- as.numeric(data$wind_speed) / 10
#       }
#     }
#     
#     # Store the sounding data information as a data frame in the
#     # second index of the list item
#     sounding_list[[list_item]][[2]] <- data
#     
#     # increment the index of i to the first header line of the next sounding
#     i <- i + as.numeric(header$lines)
#     
#     # Remove the 'header' and 'header_*' items
#     rm(header, header_254, header_1, header_2, header_3)
#     
#     # Remove the 'data' object
#     rm(data)
#     
#     # Create a progress bar
#     pb <- txtProgressBar(min = 1, max = length(sounding_data), style = 3)
#     setTxtProgressBar(pb, i)
#     flush.console()
#     Sys.sleep(1)
#     close(pb)
#     
#     # Close while loop
#   }
#   
#   ####
#   # Create the UP.DAT file
#   ####
#   
#   # Generate requested start and end POSIXct time objects
#   req_start_date_time <-
#     as.POSIXct(paste(year, "-01-01", sep = ''),
#                origin = "1970-01-01", tz = "GMT") +
#     (0 * 3600)
#   
#   req_end_date_time <-
#     as.POSIXct(paste(year, "-12-31", sep = ''),
#                origin = "1970-01-01", tz = "GMT") +
#     (12 * 3600)
#   
#   top_pressure_level <- 500
#   
#   # Generate header for UP.DAT file
#   header_1 <- "UP.DAT          2.0             Header structure with coordinate parameters"
#   header_2 <- "3"
#   header_3 <- "Produced by PuffR !Do not edit by hand!"
#   header_4 <- "Sounding Information"
#   header_5 <- paste(df_soundings_domain$init, " ",
#                     df_soundings_domain$wban, " ",
#                     "520.0 6205.0", " ",
#                     -(time_offset), sep = '')
#   header_6 <- "NONE"
#   header_7 <- paste(year(as.POSIXct(start_date, origin = "1970-01-01", tz = "GMT")),
#                     "  ",
#                     yday(as.POSIXct(start_date, origin = "1970-01-01", tz = "GMT")),
#                     "    ",
#                     "1 ",
#                     year(as.POSIXct(end_date, origin = "1970-01-01", tz = "GMT")),
#                     "    ",
#                     yday(as.POSIXct(end_date, origin = "1970-01-01", tz = "GMT")),
#                     "    ",
#                     "1 ",
#                     top_pressure_level, ".",
#                     "    2    2",
#                     sep = '')
#   header_8 <- "F    F    F    F"
#   
#   # Determine whether the selected time interval is available in the
#   # 'processed_sounding_data' list of data frames; if not, stop function with message
#   processed_sounding_data_start_date_time <- 
#     ISOdatetime(year = sounding_list[[1]][[1]][[4]],
#                 month = sounding_list[[1]][[1]][[3]],
#                 day = sounding_list[[1]][[1]][[2]],
#                 hour = sounding_list[[1]][[1]][[1]],
#                 min = 0, sec = 0, tz = "GMT")
#   
#   processed_sounding_data_end_date_time <-
#     ISOdatetime(year = sounding_list[[length(sounding_list)]][[1]][[4]],
#                 month = sounding_list[[length(sounding_list)]][[1]][[3]],
#                 day = sounding_list[[length(sounding_list)]][[1]][[2]],
#                 hour = sounding_list[[length(sounding_list)]][[1]][[1]],
#                 min = 0, sec = 0, tz = "GMT")
#   
#   #   if (req_start_date_time <= processed_sounding_data_start_date_time |
#   #         req_end_date_time >= processed_sounding_data_end_date_time  ) {
#   #     stop("Requested time frame for data is not entirely available in processed dataset.")
#   #   }
#   
#   ####
#   # Subset the list object
#   ####
#   
#   # Find how many values of the list need to be trimmed from the beginning then
#   # trim those list items and save as a new list object
#   for (i in 1:length(sounding_list)) {
#     if (i == 1) above_req_date_time <- mat.or.vec(nr = length(sounding_list), nc = 1)
#     above_req_date_time[i] <- 
#       ISOdatetime(year = sounding_list[[i]][[1]][[4]],
#                   month = sounding_list[[i]][[1]][[3]],
#                   day = sounding_list[[i]][[1]][[2]],
#                   hour = sounding_list[[i]][[1]][[1]],
#                   min = 0, sec = 0, tz = "GMT") >= req_start_date_time
#     trim_number_from_left <- length(above_req_date_time) -
#       sum(above_req_date_time, na.rm = TRUE)   
#   }
#   
#   # Find how many values of the list need to be trimmed from the end   
#   for (i in 1:length(sounding_list)) {
#     if (i == 1) below_req_date_time <- mat.or.vec(nr = length(sounding_list), nc = 1)
#     below_req_date_time[i] <- 
#       ISOdatetime(year = sounding_list[[i]][[1]][[4]],
#                   month = sounding_list[[i]][[1]][[3]],
#                   day = sounding_list[[i]][[1]][[2]],
#                   hour = sounding_list[[i]][[1]][[1]],
#                   min = 0, sec = 0, tz = "GMT") <= req_end_date_time
#     trim_number_from_right <- length(below_req_date_time) -
#       sum(below_req_date_time, na.rm = TRUE)
#   }
#   
#   # Make copy of processed_sounding_data before trimming it
#   trimmed_processed_sounding_data <- sounding_list
#   
#   # Trim the 'trimmed_processed_sounding_data' object at the beginning
#   if (trim_number_from_left > 0) {
#     for (i in 1:trim_number_from_left) {
#       trimmed_processed_sounding_data[1] <- NULL
#     }
#   }
#   
#   # Trim the 'trimmed_processed_sounding_data' object at the end
#   if (trim_number_from_right > 0) {
#     for (i in 1:trim_number_from_right) {
#       trimmed_processed_sounding_data[length(trimmed_processed_sounding_data)] <- NULL
#     }
#   }
#   
#   # Remove objects from global environment
#   rm(req_start_date_time, req_end_date_time,
#      processed_sounding_data_start_date_time, processed_sounding_data_end_date_time,
#      trim_number_from_left, trim_number_from_right,
#      above_req_date_time, below_req_date_time)
#   
#   # Construct the first portion of header line, which is constant throughout
#   header_line_constant <- paste("   6201     94240   ")
#   
#   ####
#   # Loop through the list, outputting lines at 0z and at 12z
#   # Check that the next item in the list is within 10-14 h of the previous
#   # If there is no list item available, then get the sounding data from the
#   # previous period ~24 h earlier
#   ####
#   
#   # Generate a file for writing
#   cat(file = output_file)
#   
#   # Add header to top of output file
#   cat(header_1, header_2, header_3, header_4, header_5, header_6,
#       sep = "\n", file = output_file, append = TRUE)
#   
#   # Remove objects from global environment
#   rm(header_1, header_2, header_3, header_4, header_5, header_6)
#   
#   # Start loop for header line
#   for (i in 1:length(trimmed_processed_sounding_data)) {
#     header_line <- 
#       paste(header_line_constant,
#             trimmed_processed_sounding_data[[i]][[1]][[4]],
#             formatC(trimmed_processed_sounding_data[[i]][[1]][[3]], # month
#                     width = 2, flag = " "),
#             formatC(trimmed_processed_sounding_data[[i]][[1]][[2]], # day
#                     width = 2, flag = " "),
#             formatC(trimmed_processed_sounding_data[[i]][[1]][[1]], # hour
#                     width = 2, flag = " "),
#             formatC(as.numeric(trimmed_processed_sounding_data[[i]][[1]][[14]]) - 3, # lines
#                     width = 7, flag = " "),
#             formatC(as.numeric(trimmed_processed_sounding_data[[i]][[1]][[14]]) - 3, # lines
#                     width = 33, flag = " "),
#             sep = '')
#     
#     # Write header_line to file
#     cat(header_line, sep = "\n", file = output_file, append = TRUE)
#     
#     # Start loop for data lines
#     for (j in 1:nrow(trimmed_processed_sounding_data[[i]][[2]])) {
#       if (j == 1) data_line <- mat.or.vec(nr = nrow(trimmed_processed_sounding_data[[i]][[2]]),
#                                           nc = 1) 
#       data_line[j] <-
#         paste(formatC(trimmed_processed_sounding_data[[i]][[2]][[j, 2]], # pressure
#                       width = 9, format = "f", digits = 1, flag = " "),
#               ",",
#               formatC(trimmed_processed_sounding_data[[i]][[2]][[j, 3]], # height
#                       width = 6, format = "f", digits = 0, flag = " "),
#               ",",
#               ifelse(trimmed_processed_sounding_data[[i]][[2]][[j, 4]] > 900, 
#                      formatC(999.9, width = 5, format = "f",
#                              digits = 1, flag = " "),
#                      formatC(trimmed_processed_sounding_data[[i]][[2]][[j, 4]] + 273, # temp
#                              width = 2, format = "f", digits = 1, flag = " ")),
#               ",",
#               ifelse(trimmed_processed_sounding_data[[i]][[2]][[j, 6]] > 900,
#                      formatC(999, width = 3, format = "f",
#                              digits = 0, flag = " "),
#                      formatC(trimmed_processed_sounding_data[[i]][[2]][[j, 6]], # WD
#                              width = 3, format = "f", digits = 0, flag = " ")),
#               ",",
#               ifelse(trimmed_processed_sounding_data[[i]][[2]][[j, 7]] > 900,
#                      formatC(999.9, width = 5, format = "f",
#                              digits = 1, flag = " "),                
#                      formatC(trimmed_processed_sounding_data[[i]][[2]][[j, 7]], # WS
#                              width = 5, format = "f", 
#                              digits = 1, flag = " ")),
#               ifelse(j == nrow(trimmed_processed_sounding_data[[i]][[2]]), "", ","),
#               sep = '')
#       
#       # Close loop for data lines
#     }
#     
#     # Scan for entries with negative height and delete such records
#     for (k in 1:length(data_line)) {
#       if (trimmed_processed_sounding_data[[i]][[2]][[k, 3]] < 0) data_line <- data_line[-k]
#     }
#     
#     # Write data lines to file
#     cat(data_line, sep = "\n", file = output_file, append = TRUE)
#     
#     # Close loop for header line
#   }
  
  
}
