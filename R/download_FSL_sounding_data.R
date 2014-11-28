#' Download an upper air data file in the FSL format
#' @description Download an upper air data file from NOAA in the FSL format.
#' @param sounding_priority a character string that determines the priority of the sounding; choices are either "primary" or "secondary".
#' @param df_soundings a data frame containing a complete database of global sounding sites.
#' @param station_list_position the row of interest from the 'df_soundings' data frame.
#' @param starting_hour the starting hour for the sounding data.
#' @param level_type the type of levels to obtain from the FSL upper air sounding data.
#' @param wind_units the type of wind units to obtain from the FSL upper air sounding data.
#' @param beginning_date the beginning date of the requested sounding data.
#' @param ending_date the ending date of the requested sounding data.
#' @export download_FSL_sounding_data

download_FSL_sounding_data <- function(sounding_priority,
                                       df_soundings,
                                       station_list_position,
                                       starting_hour,
                                       level_type,
                                       wind_units,
                                       beginning_date,
                                       ending_date){

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
  noaa_cgi_message <- getURL(paste0(
    "http://www.esrl.noaa.gov/raobs/intl/GetRaobs.cgi?",
    "bdate=", beginning_date,
    "&",
    "edate=", ending_date,
    "&",
    "access=All+Sites",
    "&",
    "view=NO",
    "&",
    "States=States",
    "&",
    "Countries=Countries",
    "&",
    "shour=", starting_hour,
    "&",
    "ltype=", level_type,
    "&",
    "wunits=", wind_units,
    "&",
    "stationlist=YES",
    "&",
    "station_list=", station_list,
    "&",
    "osort=Station+Series+Sort",
    "&",
    "oformat=FSL+format+%28ASCII+text%29"))
  
  # Parse message and construct URI for data
  data_URI <- paste0("http://www.esrl.noaa.gov/raobs/temp",
                    str_match(string = noaa_cgi_message,
                              pattern = "temp(.*)(tmp)")[1,2], "tmp")
  
  # Get the sounding data as a large character object
  sounding_data <- getURL(data_URI)
  
  # Write the data for a primary sounding to a text file
  if (sounding_priority == "primary"){
  
    # Construct the name of the primary sounding file
    primary_sounding_filename <- paste0("primary_sounding--",
                                        df_soundings[station_list_position,1], "--",
                                        df_soundings[station_list_position,9], "--",
                                        gsub("^([0-9]{4}).*", "\\1", beginning_date),
                                        ".txt")
    
    # Write the primary sounding file
    writeLines(sounding_data,
               con = primary_sounding_filename,
               sep = "\n")
    
    # Invisibly return the filename of the secondary sounding
    return(primary_sounding_filename)
    
  }
  
  # Write the data for a secondary sounding to a text file
  if (sounding_priority == "secondary"){
    
    # Construct the name of the secondary sounding file
    secondary_sounding_filename <- paste("secondary_sounding--",
                                         df_soundings[station_list_position,1], "--",
                                         df_soundings[station_list_position,9], "--",
                                         gsub("^([0-9]{4}).*", "\\1", beginning_date),
                                         ".txt",
                                         sep = '')
    
    # Write the secondary sounding file
    writeLines(sounding_data,
               con = secondary_sounding_filename,
               sep = "\n")
    
    # Invisibly return the filename of the secondary sounding
    return(secondary_sounding_filename)
    
  }
    
}
