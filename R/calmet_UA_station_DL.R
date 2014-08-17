#' Download the upper air data and produce a data frame.
#' @description This function allows for the downloading and processing of upper air sounding data.
#' @param start_date the starting date of the sounding data request in the format "YYYY-MM-DD". The earliest sounding for the specified date will be included in the request.
#' @param end_date the end date of the sounding data request in the format "YYYY-MM-DD". The last sounding for the specified date will be included in the request.
#' @param hour_type the types of hours to be supplied in the returned dataset. The default is "0z,12z", which includes sounding data from launches near 12:00 UTC and those near 24:00 UTC. The option "all" provides all of the soundings available from the dataset, and the options "12z" and "0z" provide data from those soundings launched near 12:00 UTC or 24:00 UTC, respectively.
#' @param level_type the types of levels to be supplied in the returned dataset. The default is "all", which includes all sounding types. Other options are "mandatory" (includes only the mandatory-type levels) and "mandatory_and_significant" (includes only the mandatory- and significant-type levels).
#' @param wind_units the desired wind units for the sounding data request. Options are tenths of meters per second ("tenths_ms", the default) and "knots".
#' @param station_number the selection of the desired sounding site can be performed by identifying a line number from the dataframe supplied by the 'get.sounding.stations' function.
#' @param station_wban_wmo the selection of the desired sounding site for the data request can be achieved by supplying WBAN and WMO numbers in the form of "XXXXX-YYYYY" (WBAN first, WMO second).
#' @param output_file_path the output file path for the retrieved sounding data can be specified here by using "working", which sets the path to the current working directory, or, by specifying an absolute path.
#' @param output_file_name the desired filename for the retrieved sounding data, including an extension such as ".txt" or ".dat". The default is "FSL-Sounding.txt", so, not modifying this argument over several uses (especially in conjunction with a "FALSE" setting for the 'details_in_file_name' argument) may result in overwriting previously generated files.
#' @param details_in_file_name incorporates details about the sounding data into the filename. Set to "TRUE" by default to provide self-describing filenames and reducing the risk of overwriting files. Setting to "FALSE" strictly uses the filename specified in the 'output_file_name' argument.
#' @export calmet_UA_station_DL

calmet_UA_station_DL <- function(start_date,
                                 end_date,
                                 hour_type = "0z,12z",
                                 level_type = "all",
                                 wind_units = "tenths_ms",
                                 station_number = NULL,
                                 station_wban_wmo = NULL,
                                 output_file_path = "working",
                                 output_file_name = "FSL-Sounding.txt",
                                 details_in_file_name = TRUE){
  
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
  lines <- gsub(pattern = pattern, replacement = "\\1", x = URI)
  lines <- gsub(pattern = ".*MULTIPLE SIZE=\"10\">\n", replacement = "", x = lines)
  lines <- gsub(pattern = "\n\n</SELECT>.*", replacement = "", x = lines)
  lines <- gsub(pattern = "<OPTION> ", replacement = "", x = lines)
  lines <- str_split(lines, "\n\n")
  lines <- unlist(lines)
  
  # Initialize the data objects
  # Loop through list of strings, extract and clean the substrings corresponding to data elements
  # Create a data frame with vector lists and coerce some objects into numeric objects
  for (i in 1:length(lines)){
    if (i == 1) {
      init <- mat.or.vec(nr = length(lines), nc = 1)
      wban <- mat.or.vec(nr = length(lines), nc = 1)
      wmo <- mat.or.vec(nr = length(lines), nc = 1)
      lat <- mat.or.vec(nr = length(lines), nc = 1)
      lon <- mat.or.vec(nr = length(lines), nc = 1)
      elev <- mat.or.vec(nr = length(lines), nc = 1)
      station_name <- mat.or.vec(nr = length(lines), nc = 1)
      prov_state <- mat.or.vec(nr = length(lines), nc = 1)
      country <- mat.or.vec(nr = length(lines), nc = 1)
    }
    init[i] <- 
      str_match(string = lines[i],
                pattern = "^([0-9A-Z]*)")[1,2]
    wban[i] <- 
      str_match(string = lines[i],
                pattern = "^[0-9A-Z]+[ ]+([0-9]*)")[1,2]
    wmo[i] <- 
      str_match(string = lines[i],
                pattern = "^[0-9A-Z]+[ ]+[0-9]* ([0-9]{5})")[1,2]
    lat[i] <- 
      as.numeric(str_match(string = lines[i],
                           pattern = paste("^[0-9A-Z]+[ ]+[0-9]* ",
                                           "[0-9]{5} ([0-9/.-]*)", sep = ''))[1,2])
    lon[i] <- 
      as.numeric(str_match(string = lines[i],
                           pattern = paste("^[0-9A-Z]+[ ]+[0-9]* [0-9]{5} ",
                                           "[0-9/.-]* ([0-9/.-]*)", sep = ''))[1,2])
    elev[i] <- 
      as.numeric(str_match(string = lines[i],
                           pattern = paste("^[0-9A-Z]+[ ]+[0-9]* ",
                                           "[0-9]{5} [0-9/.-]* [0-9/.-]* ",
                                           "([0-9-]{5,6})", sep = ''))[1,2])
    station_name[i] <- 
      str_trim(str_match(string = lines[i],
                         pattern = paste("^[0-9A-Z]+[ ]+[0-9]* ",
                                         "[0-9]{5} [0-9/.-]* [0-9/.-]* ",
                                         "[0-9-]{5,6}  (.+) [0-9A-Z]{2} ",
                                         "[0-9A-Z]{2}$", sep = ''))[1,2],
               side = "both")
    prov_state[i] <- 
      str_match(string = lines[i],
                pattern = paste("^[0-9A-Z]+[ ]+[0-9]* ",
                                "[0-9]{5} [0-9/.-]* [0-9/.-]* ",
                                "[0-9-]{5,6}  .+ ([0-9A-Z]{2}) ",
                                "[0-9A-Z]{2}$", sep = ''))[1,2]
    country[i] <- 
      str_match(string = lines[i],
                pattern = paste("^[0-9A-Z]+[ ]+[0-9]* ",
                                "[0-9]{5} [0-9/.-]* [0-9/.-]* ",
                                "[0-9-]{5,6}  .+ [0-9A-Z]{2} ",
                                "([0-9A-Z]{2})$", sep = ''))[1,2]
    
    if (i == length(lines)) {
      # Create data frame with vector objects of equal length 
      df_soundings <- as.data.frame(cbind(init, wban, wmo, lat, lon,
                                          elev, station_name, prov_state,
                                          country), stringsAsFactors = FALSE)
      
      # Change object class for lat, lon, and elev in 'df_soundings' data frame
      df_soundings[,4] <- as.numeric(df_soundings[,4])
      df_soundings[,5] <- as.numeric(df_soundings[,5])
      df_soundings[,6] <- as.numeric(df_soundings[,6])
      
      # Remove objects from global environment
      rm(i, init, wban, wmo, lat, lon, elev, station_name, prov_state,
         country, URI, pattern, lines)
    }
  }
  
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
  
}
