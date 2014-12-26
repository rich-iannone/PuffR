#' Set one or more CALPUFF point source parameters
#' @description This function validates and writes CALPUFF point source parameters.
#' @param calpuff_inp the absolute path and filename for the working CALPUFF input file.
#' @param npt1 the number of point sources.
#' @param iptu the units used for line source emissions, where the possible selections are: (1) "g/s", (2) "kg/hr", (3) "lb/hr", (4) "tons/yr", (5) "Odour Unit * m^3/s", (6) "Odour Unit * m^3/min", and (7) "metric tons/yr".
#' @param nspt1 the number of source-species combinations with variable emissions scaling factors.
#' @param npt2 the number of point sources with variable location and emission parameters.
#' @param ptsource_name a vector of assigned names for each point source.
#' @param ptsource_params a list of point source parameters for each source in the order of: (1) x UTM coordinate (km), (2) y UTM coordinate (km), (3) stack height (m), (4) base elevation (m), (5) stack diameter (m), (6) exit velocity (m/s), (7) exit temperature (K), (8) building downwash computed (where 1 is yes, 0 is no), (9) emission rates for each species emitted from point source in units specified in the 'iptu' argument. 
#' @param ptsource_zpltfm 
#' @param ptsource_fmfac 
#' @param downwash_output 
#' @export calpuff_13_pt_source_params

calpuff_13_pt_source_params <- function(calpuff_inp = "calpuff_template.txt",
                                        npt1 = NULL,
                                        iptu = NULL,
                                        nspt1 = NULL,
                                        npt2 = NULL,
                                        ptsource_name = NULL,
                                        ptsource_params = NULL,
                                        ptsource_zpltfm = NULL,
                                        ptsource_fmfac = NULL,
                                        downwash_output = NULL){

  # Generate a vector list of calpuff.inp keywords
  keywords <- c("NPT1", "IPTU", "NSPT1", "NPT2")
  
  # Generate a vector list of the formatted replacements
  replacements <- c(npt1, iptu, nspt1, npt2)
  
  # Modify all parameters in working calpuff.inp vector
  calpuff_inp_working <- replace_in_inp(inp_file_working = calpuff_inp_working,
                                        keyword = keywords,
                                        replacement = replacements)
  
  # Write the output to the same working calpuff.inp file
  writeLines(calpuff_inp_working, con = calpuff_inp)
  
}
