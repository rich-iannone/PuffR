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
#' @param READ62_exec if the READ62 executable is in the system path, provide the name of the executable only; otherwise, provide the full path and name of the READ62 executable.
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
#'                  time_offset = -8,
#'                  READ62_exec = "read62")
#'}

calmet_upper_air <- function(location_name,
                             year,
                             lat_dec_deg = NULL,
                             lon_dec_deg = NULL,
                             lat_lon_grid_loc = 1,
                             domain_width_m = NULL,
                             domain_height_m = NULL,
                             cell_resolution_m = 250,
                             time_offset,
                             READ62_exec_location){
  
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
  proj_string_UTM <- paste0("+proj=utm +zone=",
                           UTM_zone,
                           " +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  
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
  URI <- getURL(paste0("http://www.esrl.noaa.gov/raobs/intl/GetRaobs.cgi?",
                      "shour=All+Times&ltype=All+Levels&wunits=Tenths+of+Meters%2FSecond",
                      "&bdate=1990010100&edate=2013122523&access=All+Sites&view=YES&",
                      "osort=Station+Series+Sort&oformat=FSL+format+%28ASCII+text%29"))
  
  if(grepl("Service Temporarily Unavailable", URI) == TRUE) {
    stop("The NOAA DB server is reporting that it's temporarily unavailable.")
  }
  
  # Create a 'pattern' string object cdontaining the regex pattern for extracting
  # sounding data strings from the URI
  pattern <- paste0("<OPTION> [0-9A-Z]*[ ]*[0-9]* [0-9]{5} [0-9/.-]*",
                   "[0-9/.-]* [0-9-]{5,6}  [.]*  [0-9A-Z]{2} [0-9A-Z]{2}")
  
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
                           pattern = paste0("^[0-9A-Z]+[ ]+[0-9]* ",
                                           "[0-9]{5} ([0-9/.-]*)"))[1,2])
    
    lon[i] <- 
      as.numeric(str_match(string = sounding_lines[i],
                           pattern = paste0("^[0-9A-Z]+[ ]+[0-9]* [0-9]{5} ",
                                           "[0-9/.-]* ([0-9/.-]*)"))[1,2])
    
    elev[i] <- 
      as.numeric(str_match(string = sounding_lines[i],
                           pattern = paste0("^[0-9A-Z]+[ ]+[0-9]* ",
                                           "[0-9]{5} [0-9/.-]* [0-9/.-]* ",
                                           "([0-9-]{5,6})"))[1,2])
    
    station_name[i] <- 
      str_trim(str_match(string = sounding_lines[i],
                         pattern = paste0("^[0-9A-Z]+[ ]+[0-9]* ",
                                         "[0-9]{5} [0-9/.-]* [0-9/.-]* ",
                                         "[0-9-]{5,6}  (.+) [0-9A-Z]{2} ",
                                         "[0-9A-Z]{2}$"))[1,2],
               side = "both")
    
    prov_state[i] <- 
      str_match(string = sounding_lines[i],
                pattern = paste0("^[0-9A-Z]+[ ]+[0-9]* ",
                                "[0-9]{5} [0-9/.-]* [0-9/.-]* ",
                                "[0-9-]{5,6}  .+ ([0-9A-Z]{2}) ",
                                "[0-9A-Z]{2}$"))[1,2]
    
    country[i] <- 
      str_match(string = sounding_lines[i],
                pattern = paste0("^[0-9A-Z]+[ ]+[0-9]* ",
                                "[0-9]{5} [0-9/.-]* [0-9/.-]* ",
                                "[0-9-]{5,6}  .+ [0-9A-Z]{2} ",
                                "([0-9A-Z]{2})$"))[1,2]
    
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
  start_date <- paste0(year, "-01-01")
  end_date <- paste0(year, "-12-31")
  
  # Set 'shour' as "0z%2C+12z+ONLY"
  shour <- "0z%2C+12z+ONLY"
  
  # Set 'ltype' as "All+Levels"
  ltype <- "All+Levels"
  
  # Set 'wunits' as "Tenths+of+Meter%2FSecond"
  wunits <- "Tenths+of+Meter%2FSecond"
  
  # Get formatted beginning date
  bdate <- paste0(str_replace_all(start_date, "-", ""), "00")
  
  # Get formatted ending date
  edate <- paste0(str_replace_all(end_date, "-", ""), "23")
  
  # Generate a data frame containing WBAN-WMO keys
  wban_wmo_list <- as.vector(paste0(df_soundings$wban, "-", df_soundings$wmo))
  
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
        primary_station_wban_wmo <- paste0(df_soundings_domain[,2], "-",
                                          df_soundings_domain[,3])
        
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
        print(paste0("The sounding file for ", primary_station_wban_wmo, " was downloaded"))
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
      secondary_station_wban_wmo <- paste0(df_soundings_domain[,2], "-",
                                          df_soundings_domain[,3])
      
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
      print(paste0("The sounding file for ", secondary_station_wban_wmo, " was downloaded"))
      break
    }
  }
  
  ####
  # Generate two READ62 input files and obtain two UP.DAT files
  ####
  
  # Generate an output file name for the primary UP.DAT file
  output_file_primary <- paste0("up--", location_name, "-",
                               number_cells_across_x, "x",
                               number_cells_across_y, "x",
                               cell_resolution_m, "--",
                               year, ".txt")
  
  # Generate an output file name for the secondary UP.DAT file
  output_file_secondary <- paste0("up--secondary--", location_name, "-",
                                 number_cells_across_x, "x",
                                 number_cells_across_y, "x",
                                 cell_resolution_m, "--",
                                 year, ".txt")  
  
  # Generate the READ62 input file template for the secondary station
  read62_inp_generate_template()
  
  # Add run control parameters to READ62 file for the secondary station
  read62_01_run_control_params(isub = 0, lht = TRUE, lxtop = FALSE, lxsfc = FALSE)
  
  # Read in the working READ62 input file for the secondary station
  read62_inp_working <- readLines("read62_template.txt", warn = FALSE)
  
  # Define the READ62 'INDAT' parameter as the secondary sounding file
  read62_inp_working <- replace_in_inp(inp_file_working = read62_inp_working,
                                       keyword = "INDAT",
                                       replacement = downloaded_secondary_sounding_file)
  
  # Comment out the 'SUBDAT' parameter
  read62_inp_working[which(grepl("SUBDAT", read62_inp_working))] <-
    gsub("!", "\\*", read62_inp_working[which(grepl("SUBDAT", read62_inp_working))])
  
  # Define the READ62 'UPDAT' parameter as the name of the UP.DAT file
  read62_inp_working <- replace_in_inp(inp_file_working = read62_inp_working,
                                       keyword = "UPDAT",
                                       replacement = output_file_secondary)
  
  # Define the READ62 'RUNLST' parameter as filename related to the UP.DAT file
  read62_inp_working <- replace_in_inp(inp_file_working = read62_inp_working,
                                       keyword = "RUNLST",
                                       replacement = gsub(".txt", ".lst", output_file_secondary))
  
  # Define the READ62 'LCFILES' parameter as 'T', meaning that all filenames should
  # be lowercase
  read62_inp_working <- replace_in_inp(inp_file_working = read62_inp_working,
                                       keyword = "LCFILES",
                                       replacement = "T")
  
  # Construct a filename for the finalized READ62 input file
  read62_final_secondary_filename <- gsub("^up--secondary(.*)", "read62\\1", output_file_secondary)
  
  # Write the READ62 input file to a finalized filename
  writeLines(read62_inp_working, con = read62_final_secondary_filename)
  
  # Delete the temporary READ62 file from the working folder
  file.remove("read62_template.txt")
  
  # Execute the READ62 run
  READ62_exec(READ62_exec = READ62_exec_location,
              READ62_file = read62_final_secondary_filename)
  
  # Delete the READ62 input file
  file.remove(read62_final_secondary_filename)
  
  # Generate the READ62 input file template for the primary station
  read62_inp_generate_template()
  
  # Add run control parameters to READ62 file for the primary station
  read62_01_run_control_params(lht = TRUE, lxtop = FALSE, lxsfc = FALSE)
  
  # Read in the working READ62 input file for the primary station
  read62_inp_working <- readLines("read62_template.txt", warn = FALSE)
  
  # Define the READ62 'INDAT' parameter as the primary sounding file
  read62_inp_working <- replace_in_inp(inp_file_working = read62_inp_working,
                                       keyword = "INDAT",
                                       replacement = downloaded_primary_sounding_file)
  
  # Define the READ62 'SUBDAT' parameter as the secondary UP.DAT file
  read62_inp_working <- replace_in_inp(inp_file_working = read62_inp_working,
                                       keyword = "SUBDAT",
                                       replacement = output_file_secondary)
  
  # Define the READ62 'UPDAT' parameter as the name of the UP.DAT file
  read62_inp_working <- replace_in_inp(inp_file_working = read62_inp_working,
                                       keyword = "UPDAT",
                                       replacement = output_file_primary)
  
  # Define the READ62 'RUNLST' parameter as filename related to the UP.DAT file
  read62_inp_working <- replace_in_inp(inp_file_working = read62_inp_working,
                                       keyword = "RUNLST",
                                       replacement = gsub(".txt", ".lst", output_file_primary))
  
  # Define the READ62 'LCFILES' parameter as 'T', meaning that all filenames should
  # be lowercase
  read62_inp_working <- replace_in_inp(inp_file_working = read62_inp_working,
                                       keyword = "LCFILES",
                                       replacement = "T")
  
  # Construct a filename for the finalized READ62 input file
  read62_final_primary_filename <- gsub("^up--(.*)", "read62--\\1", output_file_primary)
  
  # Write the READ62 input file to a finalized filename
  writeLines(read62_inp_working, con = read62_final_primary_filename)
  
  # Delete the temporary READ62 file from the working folder
  file.remove("read62_template.txt")
  
  # Execute the READ62 run
  READ62_exec(READ62_exec = READ62_exec_location,
              READ62_file = read62_final_primary_filename)
}
