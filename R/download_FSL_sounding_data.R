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
  noaa_cgi_message <- getURL(paste(
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
    "oformat=FSL+format+%28ASCII+text%29", sep = ''))
  
  # Parse message and construct URI for data
  data_URI <- paste("http://www.esrl.noaa.gov/raobs/temp",
                    str_match(string = noaa_cgi_message,
                              pattern = "temp(.*)(tmp)")[1,2], "tmp", sep = '')
  
}
