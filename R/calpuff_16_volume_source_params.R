#' Set one or more CALPUFF volume source parameters
#' @description This function validates and writes CALPUFF volume source parameters.
#' @export calpuff_16_volume_source_params

calpuff_16_volume_source_params <- function(calpuff_inp = "calpuff_template.txt"){
  
  # Read in the working calpuff.inp file as a character vector
  calpuff_inp_working <- readLines(calpuff_inp, warn = FALSE)
}
                                          