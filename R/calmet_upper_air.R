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
  
  sounding_lines <- str_split(sounding_lines, "\n\n")
  sounding_lines <- unlist(sounding_lines)
  
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
  # Locate the nearest sounding station 
  ####
  
  
  df_soundings_domain <- subset(df_soundings,
                                df_soundings$lat >= bbox_lat_lon@ymin &
                                  df_soundings$lat <= bbox_lat_lon@ymax &
                                  df_soundings$lon >= bbox_lat_lon@xmin &
                                  df_soundings$lon <= bbox_lat_lon@xmax)
  
  ####
  # Get the sounding data from the nearest station
  ####
  
  # Get start date and end dates
  start_date <- paste(year, "-01-01", sep = "")
  end_date <- paste(year, "-12-31", sep = "")
  
  # Get formatted starting date
  bdate <- paste(str_replace_all(start_date, "-", ""), "00", sep = '') 
  
  # Get formatted ending date
  edate <- paste(str_replace_all(end_date, "-", ""), "23", sep = '') 
  
  # Get formatted 'shour' string
  if (hour_type == "all") shour <- "All+Times"
  if (hour_type == "0z") shour <- "0z+ONLY"
  if (hour_type == "12z") shour <- "12z+ONLY"
  if (hour_type == "0z,12z") shour <- "0z%2C+12z+ONLY"
  
  # Get formatted 'ltype' string
  if (level_type == "all") ltype <- "All+Levels"
  if (level_type == "mandatory") ltype <- "Mandatory"
  if (level_type == "mandatory_and_significant") ltype <- "Mand+%26+Sigs"
  
  # Get formatted 'wunits' string
  if (wind_units == "tenths_ms") wunits <- "Tenths+of+Meter%2FSecond" 
  
  # Resolve the output file path based on whether "working" is set (setting absolute path to
  # current working directory of the R process) and an absolute file path is specified
  resolved_output_file_path <- ifelse(output_file_path == "working",
                                      paste(getwd(), "/", sep = ''),
                                      output_file_path)
  
  # Combine resolved output file path with protocol
  output_file_path_with_protocol <- paste("file://", 
                                          resolved_output_file_path, 
                                          sep = '')
  
  # Get Station information
  if (is.null(station_number) & is.null(station_wban_wmo)) {
    if (exists("target_station")) station_list_position <- as.numeric(row.names(target_station))
  } else if (!is.null(station_number) & is.null(station_wban_wmo)) {
    # If a 'target_station' was set using the 'select.sounding.station' function,
    # get the 'station_list_position' value from that
    if (exists("target_station")) station_list_position <- as.numeric(row.names(target_station))
    # If no 'target_station' set, defer to using the 'station_number' value
    if (!exists("target_station")) station_list_position <- station_number
  } else if (is.null(station_number) & !is.null(station_wban_wmo)) {
    wban_wmo_list <- as.data.frame(cbind(df_soundings$wban, df_soundings$wmo))
    wban_wmo_list$V3 <- do.call(paste, c(wban_wmo_list[c("V1", "V2")], sep = "-"))
    wban_wmo_list$V1 <- NULL
    wban_wmo_list$V2 <- NULL
    station_list_position <- match(station_wban_wmo,wban_wmo_list$V3)
    rm(wban_wmo_list)
  }
  
  # Construct 'station_list' string based on requested station
  station_list <- paste(df_soundings[station_list_position,1],
                        df_soundings[station_list_position,2],
                        df_soundings[station_list_position,3],
                        df_soundings[station_list_position,4],
                        df_soundings[station_list_position,5],
                        sprintf("%05s", df_soundings[station_list_position,6]),
                        str_replace_all(df_soundings[station_list_position,7], " ", "+"),
                        df_soundings[station_list_position,8],
                        df_soundings[station_list_position,9], sep = '+')
  
  # Construct request for data from NOAA
  noaa_cgi_message <- getURL(paste(
    "http://www.esrl.noaa.gov/raobs/intl/GetRaobs.cgi?",
    "bdate=", bdate,
    "&",
    "edate=", edate,
    "&",
    "access=All+Sites",
    "&",
    "view=NO",
    "&",
    "States=States",
    "&",
    "Countries=Countries",
    "&",
    "shour=", shour,
    "&",
    "ltype=", ltype,
    "&",
    "wunits=", wunits,
    "&",
    "stationlist=YES",
    "&",
    "station_list=", station_list,
    "&",
    "osort=Station+Series+Sort",
    "&",
    "oformat=FSL+format+%28ASCII+text%29", sep = ''))
  
  # Parse message and construct URI for data
  data_URI <- paste("http://www.esrl.noaa.gov/raobs/temp",
                    str_match(string = noaa_cgi_message,
                              pattern = "temp(.*)(tmp)")[1,2], "tmp", sep = '')
  
  # Get the data as a large character object
  sounding_data <- getURL(data_URI)
  
  # Append additional details to the output file name if request for such details is TRUE
  if (details_in_file_name == TRUE) {
    if (str_detect(output_file_name, fixed(".txt"))) {
      output_file_name <- str_replace(output_file_name, fixed(".txt"), "")
    }
    output_file_name <-
      paste(output_file_name, "__",
            "WBAN-", df_soundings[station_list_position,2], "_",
            "WMO-", df_soundings[station_list_position,3], "_",
            "Station-", strtrim(df_soundings[station_list_position,7], 10),
            ifelse(nchar(df_soundings[station_list_position,7]) > 10, "...", "_"),
            bdate, "-",
            edate,
            ".txt",
            sep = "")
  }
  
  # Write the data to the output file
  writeLines(sounding_data,
             con = paste(output_file_path_with_protocol,
                         output_file_name, sep = ''),
             sep = "\n")
  
  # Read back the file as lines
  sounding_data <- readLines(con = paste(output_file_path_with_protocol,
                                         output_file_name, sep = ''))
  ####
  # Process the upper air sounding data
  ####
  
  ####
  # Subset the list object
  ####
  
  
}
