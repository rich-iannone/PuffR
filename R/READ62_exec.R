#' Run the READ62 executable
#' @description Run the READ62 executable.
#' @param READ62_exec if the READ62 executable is in the system path, provide the name of the executable only; otherwise, provide the full path and name of the READ62 executable.
#' @param READ62_file the READ62 input file to execute.
#' @export READ62_exec

READ62_exec <- function(READ62_exec,
                        READ62_file){
  
  # Run the input file with READ62
  console_log <- system(command = paste("cd '", getwd(), "' ; '",
                                        READ62_exec, "' ", READ62_file,
                                        sep = ""),
                        intern = TRUE)
  
  # Return the 'console_log' object
  return(console_log)
  
}
