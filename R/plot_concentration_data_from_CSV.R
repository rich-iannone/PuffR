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
  
  
}

