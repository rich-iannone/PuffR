vector_values_to_row_major_strings <- function(values_vector,
                                               number_cells_across_x,
                                               number_cells_across_y){
  
  # Create formatted GEO.DAT fields
  values_df <- as.data.frame(matrix(values_vector,
                                    nrow = number_cells_across_y,
                                    ncol = number_cells_across_x))
  
  # Generate a vector of comma-delimited strings containing LU categories of every row of cells;
  # this is for writing to a file and eventual inclusion in the GEO.DAT file
  for (i in 1:nrow(values_df)){
    
    if (i == 1) values_strings <- vector(mode = "character", length = 0)
    
    string <- paste(values_df[i, ], collapse = ", ")
    
    values_strings <- c(values_strings, string)
    
  }
  
  return(values_strings)
  
}
