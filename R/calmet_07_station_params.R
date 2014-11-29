#' Set CALMET surface station, precipitation station, and upper air parameters
#' @description This function validates and writes CALMET parameters for surface stations, precipitation stations, and upper air soundings to the working CALMET.INP file.
#' @param calmet_inp the absolute path and filename for the working CALMET input file.
#' @param read_data_from_files an option to read data from GEO.DAT and SURF.DAT residing the the working folder.
#' @param surf_station_vars_name a vector containing names for each of the surface stations. If 'read_data_from_files' is set to TRUE, values aren't required.
#' @param surf_station_vars_ID a vector containing ID strings for each of the surface stations. If 'read_data_from_files' is set to TRUE, values aren't required.
#' @param surf_station_vars_x_coord a vector containing X direction coordinates for each of the surface stations. If 'read_data_from_files' is set to TRUE, values aren't required.
#' @param surf_station_vars_y_coord a vector containing Y direction coordinates for each of the surface stations. If 'read_data_from_files' is set to TRUE, values aren't required.
#' @param surf_station_vars_tz a vector containing time zone values for each of the surface stations. If 'read_data_from_files' is set to TRUE, values aren't required.
#' @param surf_station_vars_anem_hgt a vector containing anemometer heights for each of the surface stations. If 'read_data_from_files' is set to TRUE, values aren't required.
#' @param upper_air_params_name the name of the upper air sounding. If 'read_data_from_files' is set to TRUE, a value isn't required.
#' @param upper_air_params_ID the ID string of the upper air sounding. If 'read_data_from_files' is set to TRUE, a value isn't required.
#' @param upper_air_params_x_coord the X direction of the upper air sounding. If 'read_data_from_files' is set to TRUE, a value isn't required.
#' @param upper_air_params_y_coord the Y direction of the upper air sounding. If 'read_data_from_files' is set to TRUE, a value isn't required.
#' @param upper_air_params_tz the time zone value of the upper air sounding. If 'read_data_from_files' is set to TRUE, a value isn't required.
#' @param precip_station_params_name a vector containing anemometer heights for each of the precipitation stations. If 'read_data_from_files' is set to TRUE, values aren't required.
#' @param precip_station_params_stn_code a vector containing station codes for each of the precipitation stations. If 'read_data_from_files' is set to TRUE, values aren't required. 
#' @param precip_station_params_x_coord a vector containing X direction values for each of the precipitation stations. If 'read_data_from_files' is set to TRUE, values aren't required.
#' @param precip_station_params_y_coord a vector containing Y direction values for each of the precipitation stations. If 'read_data_from_files' is set to TRUE, values aren't required.
#' @export calmet_07_station_params

calmet_07_station_params <- function(calmet_inp = "calmet_template.txt",
                                     read_data_from_files = TRUE,
                                     surf_station_vars_name = NULL,
                                     surf_station_vars_ID = NULL,
                                     surf_station_vars_x_coord = NULL,
                                     surf_station_vars_y_coord = NULL,
                                     surf_station_vars_tz = NULL,
                                     surf_station_vars_anem_hgt = NULL,
                                     upper_air_params_name = NULL,
                                     upper_air_params_ID = NULL,
                                     upper_air_params_x_coord = NULL,
                                     upper_air_params_y_coord = NULL,
                                     upper_air_params_tz = NULL,
                                     precip_station_params_name = NULL,
                                     precip_station_params_stn_code = NULL,
                                     precip_station_params_x_coord = NULL,
                                     precip_station_params_y_coord = NULL){
  
  # Read in the working calmet.inp file as a character vector
  calmet_inp_working <- readLines(calmet_inp, warn = FALSE)
  
  # If option set to read data from files, determine which files are available
  # in the working folder and process that data
  if (read_data_from_files == TRUE){
    
    # Determine whether one or several surface station files are available
    surf_dat_file <- list.files(pattern = "surf--.*")
    
    # If there are multiple SURF.DAT files in the working folder, choose only
    # the first of the set
    if (length(surf_dat_file > 1)) surf_dat_file <- surf_dat_file[1]
    
    # Determine whether one or several upper air data files are available
    up_dat_file <- list.files(pattern = "up--.*")
    
    # If there are multiple SURF.DAT files in the working folder, choose only
    # the first of the set
    if (length(up_dat_file > 1)) up_dat_file <- up_dat_file[1]
    
    # Determine whether one or several precipitation data files are available
    precip_dat_file <- list.files(pattern = "precip--.*")
    
    # If there are multiple PRECIP.DAT files in the working folder, choose only
    # the first of the set
    if (length(precip_dat_file > 1)) precip_dat_file <- precip_dat_file[1]
    
    # Obtain the number of precipitation stations
    npsta <- length(precip_dat_file)
    
    # Insert number of precipitation stations for keyword 'NPSTA' in Section 4
    calmet_inp_working <- replace_in_inp(inp_file_working = calmet_inp_working,
                                         keyword = "NPSTA",
                                         replacement = npsta)
    
    # If there are no precipitation stations, surround 'PS1' keyword with asterisks
    calmet_inp_working[grep("PS1", calmet_inp_working)] <-
      gsub("!", "\\*", calmet_inp_working[grep("PS1", calmet_inp_working)])
    
    # Write the output to the same working calmet.inp file
    writeLines(calmet_inp_working, con = calmet_inp)
    
    # Read in the working calmet.inp file as a character vector
    calmet_inp_working <- readLines(calmet_inp, warn = FALSE)
    
    # Obtain key lines from the header portion of the SURF.DAT file
    surf_dat_station_info <-
      readLines(surf_dat_file,
                warn = FALSE)[5:(as.numeric(readLines(surf_dat_file,
                                                      warn = FALSE)[2]) + 2)]
    
    for (i in 1:length(surf_dat_station_info)){
      if (i == 1) surf_dat_station_strings <- vector(mode = "character", length = 0)
      a_string <-
        paste0("! SS", i, ifelse(i < 10, "  = ", " = "),
               gsub("^([a-zA-Z0-9]*).*", "'\\1' ",
                    surf_dat_station_info[i]),
               gsub("^[a-zA-Z0-9]* ([a-zA-Z0-9]*).*", "\\1 ",
                    surf_dat_station_info[i]),
               gsub("^[a-zA-Z0-9]* [a-zA-Z0-9]* ([\\.0-9]*) .*", "\\1 ",
                    surf_dat_station_info[i]),
               gsub("^[a-zA-Z0-9]* [a-zA-Z0-9]* [\\.0-9]* ([\\.0-9]*) .*", "\\1 ",
                    surf_dat_station_info[i]),
               gsub("^[a-zA-Z0-9]* [a-zA-Z0-9]* [\\.0-9]* [\\.0-9]* ([0-9]*) .*", "\\1 ",
                    surf_dat_station_info[i]),
               gsub("^[a-zA-Z0-9]* [a-zA-Z0-9]* [\\.0-9]* [\\.0-9]* [0-9]* ([0-9]*)", "\\1 !",
                    surf_dat_station_info[i]))
      surf_dat_station_strings <- c(surf_dat_station_strings, a_string)
    }
    
    # Obtain the number of surface met stations
    nssta <- length(surf_dat_station_strings)
    
    # Insert number of surface met stations for keyword 'NSSTA' in Section 4
    calmet_inp_working <- replace_in_inp(inp_file_working = calmet_inp_working,
                                         keyword = "NSSTA",
                                         replacement = nssta)
    
    # Write the output to the same working calmet.inp file
    writeLines(calmet_inp_working, con = calmet_inp)
    
    # Read in the working calmet.inp file as a character vector
    calmet_inp_working <- readLines(calmet_inp, warn = FALSE)
    
    # Obtain key lines from the header portion of the UP.DAT file
    up_dat_station_info <-
      readLines(up_dat_file)[5:(as.numeric(readLines(up_dat_file, warn = FALSE)[2]) + 2)]
    
    up_dat_station_string <-
      paste0("! US1  = ",
             gsub("^([a-zA-Z0-9]*).*", "'\\1' ",
                  up_dat_station_info),
             gsub("^[a-zA-Z0-9]* ([a-zA-Z0-9]*).*", "\\1 ",
                  up_dat_station_info),
             gsub("^[a-zA-Z0-9]* [a-zA-Z0-9]* ([\\.0-9]*) .*", "\\1 ",
                  up_dat_station_info),
             gsub("^[a-zA-Z0-9]* [a-zA-Z0-9]* [\\.0-9]* ([\\.0-9]*) .*", "\\1 ",
                  up_dat_station_info),
             gsub("^[a-zA-Z0-9]* [a-zA-Z0-9]* [\\.0-9]* [\\.0-9]* ([0-9]*)", "\\1",
                  up_dat_station_info)) 
  }
  
  # Write the surface station parameters to 'calmet_inp_working'
  calmet_inp_working <- 
    c(calmet_inp_working[1:(grep("! SS1.*", calmet_inp_working) - 1)],
      surf_dat_station_strings,
      calmet_inp_working[(grep("END", calmet_inp_working[grep(
        "! SS1.*", calmet_inp_working):length(calmet_inp_working)]) +
          grep("! SS1.*", calmet_inp_working) - 1)[1]:length(calmet_inp_working)])
  
  # Write the upper air station parameters to 'calmet_inp_working'
  calmet_inp_working <- 
    c(calmet_inp_working[1:(grep("! US1.*", calmet_inp_working) - 1)],
      up_dat_station_string,
      calmet_inp_working[(grep("END", calmet_inp_working[grep(
        "! US1.*", calmet_inp_working):length(calmet_inp_working)]) +
          grep("! US1.*", calmet_inp_working) - 1)[1]:length(calmet_inp_working)])
  
  # Write the output to the same working calmet.inp file
  writeLines(calmet_inp_working, con = calmet_inp)
  
}
