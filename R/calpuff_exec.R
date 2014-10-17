#' Run the CALPUFF model
#' @description Run the CALPUFF model.
#' @param calpuff_exec if the CALPUFF executable is in the system path, provide the name of the executable only; otherwise, provide the full path and name of the CALPUFF executable.
#' @export calpuff_exec

calpuff_exec <- function(calpuff_exec){
  
  # Obtain list of CALPUFF input files
  calpuff_in_files <- list.files(pattern = "^calpuff_in")
  
  # Process each input file with CALPUFF
  for (i in 1:length(calpuff_in_files)){
    
    # Run the input file with CALPUFF and capture output as lines to 'console_log'
    console_log <- system(command = paste("cd ", getwd(), " ; '",
                                          calpuff_exec, "' ", calpuff_in_files[i],
                                          sep = ""),
                          intern = TRUE)
    
    # Write 'console_log' to a text file
    writeLines(console_log, con = paste("log_for_", calpuff_in_files[i], sep = '' ))
    
  }
  
}

