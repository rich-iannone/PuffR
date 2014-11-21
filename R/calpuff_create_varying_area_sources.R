#' Create CALPUFF input file with area sources that vary in location and time
#' @description Create CALPUFF input file with area sources that vary in location and time.
#' @param CSV_input a path to a CSV file containing hourly area source data.
#' @param df_input a data frame containing hourly area source data.
#' @param src_name the name of the source emitting the species.
#' @param species_name the name of the species undergoing emissions.
#' @export calpuff_create_varying_area_sources

calpuff_create_varying_area_sources <- function(CSV_input = NULL,
                                                df_input = NULL,
                                                src_name,
                                                species_name){
 
  # Add require statement
  require(lubridate)
  
  # Obtain domain dimensions from CALMET output files
  calmet_out_files <- list.files(pattern = "calmet_out--.*")
  domain_dimensions <- unique(gsub("^calmet_out--.*?-([x0-9]*).*", "\\1", calmet_out_files))[1]
  
  # Use 'domain_dimensions' to get the first item from a vector list of GEO.DAT files
  geo_dat_file <- list.files(pattern = paste0("geo--.*?-", domain_dimensions, ".*"))[1]
  
  # Obtain text lines of GEO.DAT file as a vector object
  geo_dat_lines <- readLines(geo_dat_file, warn = FALSE)
  
  # Obtain UTM zone and hemisphere text from 'geo_dat_lines'
  UTM_zone <- gsub(" ", "", geo_dat_lines[grep("UTM", geo_dat_lines) + 1])
  
  # Use 'domain_dimensions' to get the first item from a vector list of SURF.DAT files
  surf_dat_file <- list.files(pattern = paste0("surf--.*?-", domain_dimensions, ".*"))[1]
  
  # Change 'date_time' column to 'POSIXct' class
  if (class(area_sources_df$date_time)[1] == "POSIXct"){
    NULL
  } else if (class(area_sources_df$date_time) == "factor"){
    area_sources_df$date_time <- as.POSIXct(as.numeric(as.character(area_sources_df[,2])),
                                             origin = "1970-01-01", tz = "GMT")  
  } else if (class(area_sources_df$date_time) == "numeric"){
    area_sources_df$date_time <- as.POSIXct(area_sources_df[,2],
                                             origin = "1970-01-01", tz = "GMT") 
  }
  
  # Change 'character'/'factor' classes to numeric class
  for (i in 3:16){
    if (class(area_sources_df[,i]) == "character"){
      area_sources_df[,i] <- as.numeric(area_sources_df[,i])
    } else if (class(area_sources_df[,i]) == "factor"){
      area_sources_df[,i] <- as.numeric(as.character(area_sources_df[,i]))
    }
  }
  
  # Change 'q_emit_series' column to 'character' class
  area_sources_df[,17] <- as.character(area_sources_df[,17])
  
  
  # Example of a nicely-formatted BAEMARB.DAT file
  
#   BAEMARB.DAT     2.1             Comments, times with seconds, time zone, coord info
#   2
#   Prepared by user
#   NOX_FIRE_RUN
#   LCC     
#   40.5N           90.0W           30.0N           60.0N           
#   0.00000000E+00 0.00000000E+00
#   NWS-84  02-21-2003  
#   KM
#   UTC-0500
#   1994 365  23  0000  1995 142  12 0000
#   3   3  
#   'SO2'  'NO'   'NO2' 
#   64.0000        30.0000        46.0000    
#   'Fire_Number_1'   'g/s'       0.0        0.0
#   'Fire_Number_2'   'g/s'       0.0        0.0
#   'Fire_Number_3'   'g/s'       0.0        0.0
#   1994  365 23  0000        1995  142  09  3600
#   'Fire_Number_1'   -84.8600       -84.7423       -84.7423       -84.8600    
#   -254.620       -254.620       -254.502       -254.502        1.00000    
#   2259.00        1126.35        5.76000        7.98000        10.0000    
#   0.000000       0.000000       0.000000    
#   'Fire_Number_2'   -167.190       -167.029       -167.029       -167.190    
#   29.0900        29.0900        29.2514        29.2514        1.00000    
#   1545.00        1126.35        2.97000        20.5500        10.0000    
#   0.000000       0.000000       0.000000    
#   'Fire_Number_3'   -65.0600       -64.9668       -64.9668       -65.0600    
#   -88.9500       -88.9500       -88.8568       -88.8568        1.00000    
#   1527.00        1126.35        19.1300        3.92000        10.0000    
#   0.000000       0.000000       0.000000 
  
}
