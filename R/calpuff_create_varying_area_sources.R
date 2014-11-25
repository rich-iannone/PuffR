#' Create CALPUFF input file with area sources that vary in location and time
#' @description Create CALPUFF input file with area sources that vary in location and time.
#' @param CSV_input a path to a CSV file containing hourly area source data.
#' @param df_input a data frame containing hourly area source data.
#' @param pollutant_MW a vector containing molecular weights for all pollutants in the supplied dataset.
#' @export calpuff_create_varying_area_sources

calpuff_create_varying_area_sources <- function(CSV_input = NULL,
                                                df_input = NULL,
                                                pollutant_MW){
  
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
  
  # Obtain text lines of SURF.DAT file as a vector object
  surf_dat_lines <- readLines(surf_dat_file, warn = FALSE)
  
  # Obtain time zone text from 'surf_dat_lines'
  time_zone <- gsub(" ", "", surf_dat_lines[grep("UTC([+|-])", surf_dat_lines)])
  
  # Read in CSV file if it is provided
  if (!is.null(CSV_input)){
    
  # Change 'source_names' column to 'character' class
  df_input$src_name <- as.character(df_input$src_name)
  
  # Change 'date_time' column to 'POSIXct' class
  if (class(df_input$date_time)[1] == "POSIXct"){
    NULL
  } else if (class(df_input$date_time) == "factor"){
    df_input$date_time <- as.POSIXct(as.numeric(as.character(df_input[,2])),
                                     origin = "1970-01-01", tz = "GMT")  
  } else if (class(df_input$date_time) == "numeric"){
    df_input$date_time <- as.POSIXct(df_input[,2],
                                     origin = "1970-01-01", tz = "GMT") 
  }
  
  # Change 'character'/'factor' classes to numeric class
  for (i in 3:16){
    if (class(df_input[,i]) == "character"){
      df_input[,i] <- as.numeric(df_input[,i])
    } else if (class(df_input[,i]) == "factor"){
      df_input[,i] <- as.numeric(as.character(df_input[,i]))
    }
  }
  
  # Change 'q_emit_series' column to 'character' class
  df_input[,17] <- as.character(df_input[,17])
  
  # Get beginning date and time    
  beginning_date_time <- min(df_input$date_time)
  
  # Get ending date and time
  ending_date_time <- max(df_input$date_time)
  
  # Get sorted list of unique dates and times
  sorted_date_time <- sort(unique(df_input$date_time))
  
  # Get vector list of sources
  source_names <- sort(unique(df_input$src_name))
  
  # Get vector list of pollutants
  pollutant_names <- colnames(df_input)[17:length(df_input)]
  
  # Construct header lines for file
  header_1 <- paste0("BAEMARB.DAT     2.1             ",
                     "Comments, times with seconds, time zone, coord info")
  
  # Format number of comment lines
  header_2 <- "1"
  
  # Format a comment line
  header_3 <- "Produced by PuffR !Do not edit by hand!"
  
  # Format the coordinate system in use
  header_4 <- "UTM"
  
  # Format the UTM zone
  header_5 <- UTM_zone
  
  # Format the datum in use
  header_6 <- "WGS-84"
  
  # Format units used for distances
  header_7 <- "  KM"
  
  # Format time zone
  header_8 <- time_zone
  
  # Format beginning and ending dates and times
  header_9 <- paste0(year(beginning_date_time), "  ",
                     yday(beginning_date_time), "  ",
                     hour(beginning_date_time), " ",
                     "0000", "  ",
                     year(ending_date_time), "  ",
                     yday(ending_date_time), "  ",
                     hour(ending_date_time), " ",
                     "0000")
  
  # Format number of sources and number of pollutants
  header_10 <- paste0(length(source_names), "  ", length(pollutant_names))
  
  # Format names of pollutants
  header_11 <- gsub("^", "'",
                    gsub("$", "'",
                         gsub("  ", "' '",
                              paste(pollutant_names, collapse = '  '))))
  
  # Format molecular weights of pollutants
  header_12 <- paste(pollutant_MW, collapse = '  ')
  
  # Format sources with unit definitions
  for (i in 1:length(source_names)){
    if (i == 1) header_13 <- vector(mode = "character", length = 0)
    
    header_13_item <- paste0("'", source_names[i], "'   'g/s'       0.0        0.0")
    header_13 <- c(header_13, header_13_item)
  }
  
  # Loop through dates and obtain blocks of changing emissions
  for (i in 1:length(unique(df_input$date_time))){
    
    if (i == 1) date_time_blocks <- vector(mode = "character", length = 0)
    
    date_time_subset <- subset(df_input, date_time == sorted_date_time[i])
    
    date_header <- paste0("       ",
                          year(date_time_subset$date_time)[1], "  ",
                          yday(date_time_subset$date_time)[1], "  ",
                          hour(date_time_subset$date_time)[1], "  ",
                          "0000", "  ",
                          year(date_time_subset$date_time)[1], "  ",
                          yday(date_time_subset$date_time)[1], "  ",
                          hour(date_time_subset$date_time)[1], "  ",
                          "3600")
    
    date_time_blocks <- c(date_time_blocks, date_header)
    
    for (j in 1:nrow(date_time_subset)){
      if (j == 1) date_time_block <- vector(mode = "character", length = 0)
      
      date_time_block_source <- 
        paste0("'", date_time_subset[j,1], "'", " ",
               date_time_subset[j,3], "   ",
               date_time_subset[j,4], "   ",
               date_time_subset[j,5], "   ",
               date_time_subset[j,6], "\n",
               gsub(".", " ", date_time_subset[j,1]), "   ",
               date_time_subset[j,7], "   ",
               date_time_subset[j,8], "   ",
               date_time_subset[j,9], "   ",
               date_time_subset[j,10], "\n",
               gsub(".", " ", date_time_subset[j,1]), "   ",
               date_time_subset[j,11], "   ",
               date_time_subset[j,12], "   ",
               date_time_subset[j,13], "\n",
               gsub(".", " ", date_time_subset[j,1]), "   ",
               date_time_subset[j,14], "   ",
               date_time_subset[j,15], "   ",
               date_time_subset[j,16], "\n",
               gsub(".", " ", date_time_subset[j,1]), "   ",
               paste(date_time_subset[j,17:18], collapse = "   "))
      
      date_time_block <- c(date_time_block, date_time_block_source)
    }
    
    date_time_blocks <- c(date_time_blocks, date_time_block)    
  }
  
  # Write all lines to BAEMARB.DAT file
  cat(header_1, header_2, header_3, header_4, header_5, header_6,
      header_7, header_8, header_9, header_10, header_11, header_12, header_13,
      date_time_blocks, "", sep = "\n",
      file = paste0("baemarb--", gsub("^.*?--(.*?)--.*", "\\1", geo_dat_file), ".txt"),
      append = FALSE)
  
}
