#' Run the CALPUFF model
#' @description Run the CALPUFF model.
#' @param calpuff_executable if the CALPUFF executable is in the system path, provide the name of the executable only; otherwise, provide the full path and name of the CALPUFF executable.
#' @param calpuff_exec_file the CALPUFF input file to execute.
#' @export calpuff_exec

calpuff_exec <- function(calpuff_executable,
                         calpuff_exec_file){
  
  # Run the input file with CALPUFF and capture output as lines to 'console_log'
  console_log <- system(command = paste0("cd '", getwd(), "' ; '",
                                         calpuff_executable, "' ", calpuff_exec_file),
                        intern = TRUE)
  
  # Return the 'console_log' object
  return(console_log)
  
}
