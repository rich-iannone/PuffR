#' Set the CALPUFF output options
#' @description This function validates and writes CALPUFF output options.
#' @param calpuff_inp 
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

calpuff_05_output_opts <- function(calpuff_inp,
                                   icon,
                                   idry,
                                   iwet,
                                   it2d,
                                   irho,
                                   ivis,
                                   lcomprs,
                                   iqaplot,
                                   ipftrak,
                                   imflx,
                                   imbal,
                                   inrise,
                                   icprt,
                                   idprt,
                                   iwprt,
                                   icfrq,
                                   idfrq,
                                   iwfrq,
                                   iprtu,
                                   imesg,
                                   ldebug,
                                   ipfdeb,
                                   npfdeb,
                                   nn1,
                                   nn2){
  
  # Generate a vector list of calmet.inp keywords
  keywords <- c("ICON", "IDRY", "IWET", "IT2D", "IRHO", "IVIS", "LCOMPRS", "IQAPLOT",
                "IPFTRAK", "IMFLX", "IMBAL", "INRISE", "ICPRT", "IDPRT", "IWPRT",
                "ICFRQ", "IDFRQ", "IWFRQ", "IPRTU", "IMESG", "LDEBUG",
                "IPFDEB", "NPFDEB", "NN1", "NN2")
  
  # Generate a vector list of the formatted replacements
  replacements <- c(icon, idry, iwet, it2d, irho, ivis, lcomprs, iqaplot,
                    ipftrak, imflx, imbal, inrise, icprt, idprt, iwprt,
                    icfrq, idfrq, iwfrq, iprtu, imesg, ldebug,
                    ipfdeb, npfdeb, nn1, nn2)
  
  # Modify all parameters in working calmet.inp vector
  calpuff_inp_working <- replace_in_inp(inp_file_working = calpuff_inp_working,
                                        keyword = keywords,
                                        replacement = replacements)
  
  # Write the output to the same working calmet.inp file
  writeLines(calpuff_inp_working, con = calpuff_inp)
  
}
