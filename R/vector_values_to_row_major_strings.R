#' Transform vector values into strings formatted for GEO.DAT files
#' @description Transform vector values into strings formatted for GEO.DAT files
#' @param values_vector a numeric vector
#' @param number_cells_across_x the number of grid cells horizontally across the CALMET domain.
#' @param number_cells_across_y the number of grid cells vertically across the CALMET domain.
#' @export vector_values_to_row_major_strings

vector_values_to_row_major_strings <- function(values_vector,
                                               number_cells_across_y){
  
  # Create formatted GEO.DAT fields
  values_df <- as.data.frame(t(matrix(values_vector,
                                      ncol = number_cells_across_y)))
  
  # Generate a vector of comma-delimited strings containing LU categories of every row of cells;
  # this is for writing to a file and eventual inclusion in the GEO.DAT file
  for (i in 1:nrow(values_df)){
    
    if (i == 1) values_strings <- vector(mode = "character", length = 0)
    
    string <- paste(values_df[i, ], collapse = ", ")
    
    values_strings <- c(values_strings, string)
    
  }
  
  return(values_strings)
  
}
