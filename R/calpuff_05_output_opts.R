#' Set the CALPUFF output options
#' @description This function validates and writes CALPUFF output options.
#' @param calpuff_inp the absolute path and filename for the working CALPUFF input file.
#' @param icon the choice of whether to create a concentration output file.
#' @param idry the choice of whether to create a dry deposition flux output file.
#' @param iwet the choice of whether to create a wet deposition flux output file.
#' @param it2d the choice of whether to create a 2D temperature output file.
#' @param irho the choice of whether to create a 2D density output file.
#' @param ivis the choice of whether to create a relative humidity output file (required for visibility analysis).
#' @param lcomprs the choice of whether to compress the output data files.
#' @param iqaplot the choice of whether to create a standard series of output files (e.g. locations of sources, receptors, grids, etc.) for QA purposes.
#' @param ipftrak the choice of method for tracking puff locations and properties to the PFTRAK.DAT file: (0) no puff tracking, (1) track puffs at the end of each timestep, or (2) track puffs at the end of each sampling step.
#' @param imflx the choice of whether to output data on mass flux across specified boundaries for selected species.
#' @param imbal the choice of whether to output mass balance data for each species to the MASSBAL.DAT file.
#' @param inrise the choice of whether to output plume properties for each rise increment, for each model timestep, for a single species to the RISE.DAT file.
#' @param icprt the choice of whether to print concentrations.
#' @param idprt the choice of whether to print dry flux data.
#' @param iwprt the choice of whether to print wet flux data.
#' @param icfrq the number of time intervals for which the concentration is printed.
#' @param idfrq the number of time intervals for which the dry flux data is printed.
#' @param iwfrq the number of time intervals for which the wet flux data is printed.
#' @param iprtu the choice of units used when printing: (1) g/m3 for concentration, g/m2/s for deposition, (2) mg/m3 for concentration, mg/m2/s for deposition, (3) ug/m3 for concentration, ug/m2/s for deposition, (4) ng/m3 for concentration, ng/m2/s for deposition, or (5) odour units for concentration.
#' @param imesg the choice of method for displaying messages during model execution: (0) no messages, (1) messages with advection step and puff ID, or (2) messages with date-time, number of old puffs, and number of emitted puffs.
#' @param ldebug the choice of whether to store debugging information for the model run.
#' @param ipfdeb the first puff to track when gathering debugging information.
#' @param npfdeb the number of puffs to track when gathering debugging information.
#' @param nn1 the meteorological period to beginning gathering output.
#' @param nn2 the meteorological period to end gathering output.
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
  
  # Transform TRUE or FALSE value for 'icon' to a numeric value
  if (icon == TRUE){
    icon <- 1
  } else if (icon == FALSE){
    icon <- 0
  }

  # Transform TRUE or FALSE value for 'idry' to a numeric value
  if (idry == TRUE){
    idry <- 1
  } else if (idry == FALSE){
    idry <- 0
  }

  # Transform TRUE or FALSE value for 'iwet' to a numeric value
  if (iwet == TRUE){
    iwet <- 1
  } else if (iwet == FALSE){
    iwet <- 0
  }
  
  # Transform TRUE or FALSE value for 'it2d' to a numeric value
  if (it2d == TRUE){
    it2d <- 1
  } else if (it2d == FALSE){
    it2d <- 0
  }
  
  # Transform TRUE or FALSE value for 'irho' to a numeric value
  if (irho == TRUE){
    irho <- 1
  } else if (irho == FALSE){
    irho <- 0
  }
  
  # Transform TRUE or FALSE value for 'ivis' to a numeric value
  if (ivis == TRUE){
    ivis <- 1
  } else if (ivis == FALSE){
    ivis <- 0
  }
  
  # Transform TRUE or FALSE value for 'lcomprs' to a numeric value
  if (lcomprs == TRUE){
    lcomprs <- 1
  } else if (lcomprs == FALSE){
    lcomprs <- 0
  }
  
  # Transform TRUE or FALSE value for 'iqaplot' to a numeric value
  if (iqaplot == TRUE){
    iqaplot <- 1
  } else if (iqaplot == FALSE){
    iqaplot <- 0
  }
  
  # Transform TRUE or FALSE value for 'imflx' to a numeric value
  if (imflx == TRUE){
    imflx <- 1
  } else if (imflx == FALSE){
    imflx <- 0
  }  
  
  # Transform TRUE or FALSE value for 'imbal' to a numeric value
  if (imbal == TRUE){
    imbal <- 1
  } else if (imbal == FALSE){
    imbal <- 0
  }
  
  # Transform TRUE or FALSE value for 'inrise' to a numeric value
  if (inrise == TRUE){
    inrise <- 1
  } else if (inrise == FALSE){
    inrise <- 0
  }
  
  # Transform TRUE or FALSE value for 'icprt' to a numeric value
  if (icprt == TRUE){
    icprt <- 1
  } else if (icprt == FALSE){
    icprt <- 0
  }
  
  # Transform TRUE or FALSE value for 'idprt' to a numeric value
  if (idprt == TRUE){
    idprt <- 1
  } else if (idprt == FALSE){
    idprt <- 0
  }
  
  # Transform TRUE or FALSE value for 'iwprt' to a numeric value
  if (iwprt == TRUE){
    iwprt <- 1
  } else if (iwprt == FALSE){
    iwprt <- 0
  }

  # Transform TRUE or FALSE value for 'ldebug' to a numeric value
  if (ldebug == TRUE){
    ldebug <- 1
  } else if (ldebug == FALSE){
    ldebug <- 0
  }
  
  # Read in the working calpuff.inp file as a character vector
  calpuff_inp_working <- readLines(calpuff_inp, warn = FALSE)
  
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
