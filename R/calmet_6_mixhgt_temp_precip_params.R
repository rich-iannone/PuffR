#' Set CALMET parameters for mixing height, temperature, and precipitation
#' @description This function validates and writes CALMET parameters for wind field options and parameters to the working CALMET.INP file.
#' @param calmet_inp the absolute path and filename for the working CALMET input file.
#' @param constb 
#' @param conste 
#' @param constn 
#' @param constw 
#' @param fcoriol 
#' @param iavezi 
#' @param mnmdav 
#' @param hafang 
#' @param ilevzi 
#' @param imixh 
#' @param threshl 
#' @param threshw 
#' @param itwprog 
#' @param iluoc3d 
#' @param dptmin 
#' @param dzzi 
#' @param zimin 
#' @param zimax 
#' @param ziminw 
#' @param zimaxw 
#' @param icoare 
#' @param dshelf 
#' @param iwarm 
#' @param icool 
#' @param itprog 
#' @param irad 
#' @param tradkm 
#' @param numts 
#' @param iavet 
#' @param tgdefb 
#' @param tgdefa 
#' @param jwat1 
#' @param jwat2 
#' @param nflagp 
#' @param sigmap 
#' @param cutp 
#' @export calmet_6_mixhgt_temp_precip_params

calmet_6_mixhgt_temp_precip_params <- function(calmet_inp,
                                               constb,
                                               conste,
                                               constn,
                                               constw,
                                               fcoriol,
                                               iavezi,
                                               mnmdav,
                                               hafang,
                                               ilevzi,
                                               imixh,
                                               threshl,
                                               threshw,
                                               itwprog,
                                               iluoc3d,
                                               dptmin,
                                               dzzi,
                                               zimin,
                                               zimax,
                                               ziminw,
                                               zimaxw,
                                               icoare,
                                               dshelf,
                                               iwarm,
                                               icool,
                                               itprog,
                                               irad,
                                               tradkm,
                                               numts,
                                               iavet,
                                               tgdefb,
                                               tgdefa,
                                               jwat1,
                                               jwat2,
                                               nflagp,
                                               sigmap,
                                               cutp){
  
  # Generate a vector list of calmet.inp keywords
  keywords <- c("CONSTB", "CONSTE", "CONSTN", "CONSTW", "FCORIOL", "IAVEZI", "MNMDAV",
                "HAFANG", "ILEVZI", "IMIXH", "THRESHL", "THRESHW", "ITWPROG", "ILUOC3D",
                "DPTMIN", "DZZI", "ZIMIN", "ZIMAX", "ZIMINW", "ZIMAXW", "ICOARE", "DSHELF",
                "IWARM", "ICOOL", "ITPROG", "IRAD", "TRADKM", "NUMTS", "IAVET", "TGDEFB",
                "TGDEFA", "JWAT1", "JWAT2", "NFLAGP", "SIGMAP", "CUTP")
  
  # Generate a vector list of the formatted replacements
  replacements <- c(constb, conste, constn, constw, fcoriol, iavezi, mnmdav, hafang,
                    ilevzi, imixh, threshl, threshw, itwprog, iluoc3d, dptmin, dzzi,
                    zimin, zimax, ziminw, zimaxw, icoare, dshelf, iwarm, icool, itprog,
                    irad, tradkm, numts, iavet, tgdefb, tgdefa, jwat1, jwat2, nflagp,
                    sigmap, cutp)
  
  # Modify all parameters in working calmet.inp vector
  calmet_inp_working <- replace_in_inp(calmet_inp_working = calmet_inp_working,
                                       keyword = keywords,
                                       replacement = replacements)  
  
}
