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
  
}
