#' Set one or more CALPUFF point source parameters
#' @description This function validates and writes CALPUFF point source parameters.
#' @param calpuff_inp 
#' @param npt1 
#' @param iptu 
#' @param nspt1
#' @param npt2
#' @export calpuff_13_pt_source_params

calpuff_13_pt_source_params <- function(calpuff_inp,
                                        npt1 = NULL,
                                        iptu = NULL,
                                        nspt1 = NULL,
                                        npt2 = NULL,
                                        ptsource_name = NULL,
                                        ptsource_params = NULL,
                                        ptsource_zpltfm = NULL,
                                        ptsource_fmfac = NULL,
                                        downwash_output = NULL){

  # Generate a vector list of calmet.inp keywords
  keywords <- c("NPT1", "RGR", "REACTR", "NINT", "IVEG")
  
  # Generate a vector list of the formatted replacements
  replacements <- c(rcutr, rgr, reactr, nint, iveg)
  
  # Modify all parameters in working calmet.inp vector
  calpuff_inp_working <- replace_in_inp(inp_file_working = calpuff_inp_working,
                                        keyword = keywords,
                                        replacement = replacements)
  
  # Write the output to the same working calmet.inp file
  writeLines(calpuff_inp_working, con = calpuff_inp)
  
}