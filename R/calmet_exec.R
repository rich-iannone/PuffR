#' Run the CALMET model
#' @description Run the CALMET model.
#' @param calmet_exec the path and name of the CALMET executable.
#' @export calmet_exec

calmet_exec <- function(calmet_exec){
  
  # Obtain list of CALMET input files
  calmet_in_files <- list.files(pattern = "^calmet_in.*.txt")
  
  # Process each input file with CALMET
  for (i in 1:length(calmet_in_files)){
    
    # Run the input file with CALMET and capture output as lines to 'console_log'
    console_log <- system(command = paste("cd ", getwd(), " ; '",
                                          calmet_exec, "' ", calmet_in_files[i],
                                          sep = ""),
                          intern = TRUE)
    
    # Write 'console_log' to a text file
    writeLines(console_log, con = paste("log_for_", calmet_in_files[i], sep = '' ))

  }
  
}
