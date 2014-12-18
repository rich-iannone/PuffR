#' Set the CALPUFF output options
#' @description This function validates and writes CALPUFF output options.
#' @param calpuff_inp the absolute path and filename for the working CALPUFF input file.
#' @param icon 
#' @param idry 
#' @param iwet 
#' @param it2d 
#' @param irho 
#' @param ivis 
#' @param lcomprs 
#' @param iqaplot 
#' @param ipftrak 
#' @param imflx 
#' @param imbal 
#' @param inrise 
#' @param icprt 
#' @param idprt 
#' @param iwprt 
#' @param icfrq 
#' @param idfrq 
#' @param iwfrq 
#' @param iprtu 
#' @param imesg 
#' @param ldebug 
#' @param ipfdeb 
#' @param npfdeb 
#' @param nn1 
#' @param nn2 
#' @export calpuff_05_output_opts

calpuff_05_output_opts <- function(calpuff_inp = "calpuff_template.txt",
                                   icon = TRUE,
                                   idry = TRUE,
                                   iwet = TRUE,
                                   it2d = FALSE,
                                   irho = FALSE,
                                   ivis = TRUE,
                                   lcomprs = TRUE,
                                   iqaplot = TRUE,
                                   ipftrak = 0,
                                   imflx = FALSE,
                                   imbal = FALSE,
                                   inrise = FALSE,
                                   icprt = FALSE,
                                   idprt = FALSE,
                                   iwprt = FALSE,
                                   icfrq = 1,
                                   idfrq = 1,
                                   iwfrq = 1,
                                   iprtu = 1,
                                   imesg = 2,
                                   ldebug = FALSE,
                                   ipfdeb = 1,
                                   npfdeb = 1,
                                   nn1 = 1,
                                   nn2 = 10){
  
  # Generate a vector list of calpuff.inp keywords
  keywords <- c("ICON", "IDRY", "IWET", "IT2D", "IRHO", "IVIS", "LCOMPRS", "IQAPLOT",
                "IPFTRAK", "IMFLX", "IMBAL", "INRISE", "ICPRT", "IDPRT", "IWPRT",
                "ICFRQ", "IDFRQ", "IWFRQ", "IPRTU", "IMESG", "LDEBUG",
                "IPFDEB", "NPFDEB", "NN1", "NN2")
  
  # Generate a vector list of the formatted replacements
  replacements <- c(icon, idry, iwet, it2d, irho, ivis, lcomprs, iqaplot,
                    ipftrak, imflx, imbal, inrise, icprt, idprt, iwprt,
                    icfrq, idfrq, iwfrq, iprtu, imesg, ldebug,
                    ipfdeb, npfdeb, nn1, nn2)
  
  # Modify all parameters in working calpuff.inp vector
  calpuff_inp_working <- replace_in_inp(inp_file_working = calpuff_inp_working,
                                        keyword = keywords,
                                        replacement = replacements)
  
  # Write the output to the same working calpuff.inp file
  writeLines(calpuff_inp_working, con = calpuff_inp)
  
}
