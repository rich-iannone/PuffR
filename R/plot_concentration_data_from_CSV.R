#' Plot concentration data from one or more CSV files
#' @description Plot concentration data from one or more CSV files.
#' @param CSV_file_pattern a regex pattern for matching one or more files
#' @export plot_concentration_data_from_CSV

plot_concentration_data_from_CSV <- function(CSV_file_pattern,
                                             UTM_zone){
  
  # Add require statements
  require(ggplot2)
  require(ggmap)
  require(sp)
    
  # Obtain a file list from the supplied pattern
  file_list <- list.files(pattern = CSV_file_pattern)
  
  # Open loop to process 'file_list' CSV files
  for (i in 1:length(file_list)){
    
    # Read in a CSV file from the 'file_list' vector object
    concentration_data <- read.csv(file_list[i], header = TRUE, stringsAsFactors = FALSE)
    
  
}

