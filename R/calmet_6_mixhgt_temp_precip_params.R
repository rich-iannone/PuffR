#' Set CALMET parameters for mixing height, temperature, and precipitation
#' @description This function validates and writes CALMET parameters for wind field options and parameters to the working CALMET.INP file.
#' @param calmet_inp the absolute path and filename for the working CALMET input file.
#' 
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
