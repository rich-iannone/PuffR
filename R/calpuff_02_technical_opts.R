#' Set CALPUFF technical options
#' @description This function validates and writes CALPUFF parameters for the model's technical options.
#' @param calpuff_inp the absolute path and filename for the working CALPUFF input file.
#' @param mgauss vertical distribution used in the near field: (0) uniform, and (1) Gaussian.
#' @param mctadj the type of terrain adjustment method to use: (0) none, (1) ISC-type, (2) CALPUFF-type, and (3) partial plume path adjustment.
#' @param mctsg the choice of whether to model subgrid-scale complex terrain.
#' @param mslug the choice of whether to model near field puffs as elongated slugs.
#' @param mtrans the choice of whether to model transitional plume rise. 
#' @param mtip the choice of whether to model stack tip downwash. 
#' @param mrise the choice of method for calculating plume rise for point sources not subject to building downwash: (1) Briggs plume rise, and (2) numerical plume rise.
#' @param mbdw the choice of method for simulating building downwash: (1) ISC method, and (2) PRIME method.
#' @param mshear the choice of whether to model vertical wind shear above the stack top using modified Briggs plume rise.
#' @param msplit the choice of whether to allow splitting of puffs.
#' @param mchem the choice of method for simulating chemical transformation: (0) none, (1) internally calculate using MESOPUFF II, (2) use specified transformation rates, (3) internally calculate using RIVAD/ARM3 scheme, (4) calculate secondary organic aerosol (with the MESOPUFF II scheme for OH), (5) using a specific half, possibly with transfer to child species, (6) using transformation rates calculated internally (updated RIVAD scheme with ISORROPIA equilibrium), and (7) using transformation rates calculated internally (updated RIVAD scheme with ISORROPIA equilibrium and CalTech SOA).         
#' @param maqchem the choice of whether to model aqueous phase chemistry using transformation rates and wet scavenging coefficients adjusted for in-cloud aqueous phase reactions (ignored unless mchem set to either 6 or 7).
#' @param mlwc the method for determining liquid water content (ignored if maqchem is FALSE): (0) water content estimated from cloud cover and presence of precipitation, and (1) gridded cloud water data read from CALMET water content output files.
#' @param mwet the choice of whether to model wet removal of pollutants.
#' @param mdry the choice of whether to model dry deposition of pollutants.
#' @param mtilt the choice of whether to model gravitational settling (i.e., plume tilt).
#' @param mdisp the choice of method for calculating dispersion coefficients: (1) dispersion coefficients computed from measured values of turbulence, sigma v, sigma w, (2) = dispersion coefficients from internally calculated sigma v, sigma w using micrometeorological variables (u*, w*, L, etc.), (3) use PG dispersion coefficients for rural areas (computed using the ISCST multi-segment approximation) and MP coefficients in urban areas, (4) same as option 3 except PG coefficients computed using the MESOPUFF II equations, and (5) CTDM sigmas used for stable and neutral conditions.
#' @param mturbvw the selection of method for sigma-v/sigma-theta, sigma-w measurements used (ignored unless mdisp is either 1 or 5): (1) use sigma-v or sigma-theta measurements from PROFILE.DAT to compute sigma-y (valid for METFM = 1, 2, 3, 4, 5), (2) use sigma-w measurements from PROFILE.DAT to compute sigma-z (valid for METFM = 1, 2, 3, 4, 5), (3) use both sigma-(v/theta) and sigma-w from PROFILE.DAT to compute sigma-y and sigma-z (valid for METFM = 1, 2, 3, 4, 5), and (4) use sigma-theta measurements from PLMMET.DAT to compute sigma-y (valid only if METFM = 3).
#' @param mdisp2 the choice of back-up method used to compute dispersion when measured turbulence data are missing (ignored unless mdisp is either 1 or 5): (2) use dispersion coefficients from internally calculated sigma v, sigma w using micrometeorological variables, (3) use PG dispersion coefficients for rural areas (computed using the ISCST multi-segment approximation) and MP coefficients in urban areas, and (4) same as option 3 except PG coefficients computed using the MESOPUFF II equations.
#' @param mcturb the choice of method used to compute turbulence sigma-v & sigma-w using micrometeorological variables (ignored unless mdisp or mdisp2 is 2): (1) use standard CALPUFF subroutines, and (2) use AERMOD subroutines.
#' @param mrough the choice of whether to use PG sigma-y,z to adjust for roughness.
#' @param mpartl the choice of whether to model partial plume penetration of elevated inversions for point sources.
#' @param mpartlba the choice of whether to model partial plume penetration of elevated inversions for buoyant area sources.
#' @param mtinv confirmation that the strength of temperature inversion is provided in PROFILE.DAT extended records.
#' @param mpdf the choice of whether to use a PDF for dispersion under convective conditions.
#' @param msgtibl the choice of whether to use the subgrid TIBL module for the shoreline.
#' @param mbcon the choice of whether to use boundary concentrations in the model calculations.
#' @param msource the choice of whether to save individual source contributions.
#' @param mfog the choice of reporting methods for analyses of fogging and icing impacts due to emissions from arrays of mechanically-forced cooling towers: (0) none, (1) report results in plume-mode format, and (2) report results in receptor-mode format.
#' @param mreg the choice of whether to enable options to determine whether they conform to EPA regulatory values.
#' @export calpuff_02_technical_opts

calpuff_02_technical_opts <- function(calpuff_inp = "calpuff_template.txt",
                                      mgauss = 1,
                                      mctadj = 3,
                                      mctsg = FALSE,
                                      mslug = FALSE,
                                      mtrans = TRUE,
                                      mtip = TRUE,
                                      mrise = 1,
                                      mbdw = 1,
                                      mshear = FALSE,
                                      msplit = FALSE,
                                      mchem = 0,
                                      maqchem = FALSE,
                                      mlwc = 1,
                                      mwet = TRUE,
                                      mdry = TRUE,
                                      mtilt = FALSE,
                                      mdisp = 3,
                                      mturbvw = 3,
                                      mdisp2 = 3,
                                      mcturb = 1,
                                      mrough = FALSE,
                                      mpartl = TRUE,
                                      mpartlba = TRUE,
                                      mtinv = FALSE,
                                      mpdf = FALSE,
                                      msgtibl = FALSE,
                                      mbcon = FALSE,
                                      msource = FALSE,
                                      mfog = 0,
                                      mreg = 0){
  
  # Transform TRUE or FALSE value for 'mctsg' to a numeric value
  if (mctsg == TRUE){
    mctsg <- 1
  } else if (mctsg == FALSE){
    mctsg <- 0
  }
  
  # Transform TRUE or FALSE value for 'mslug' to a numeric value
  if (mslug == TRUE){
    mslug <- 1
  } else if (mslug == FALSE){
    mslug <- 0
  }
  
  # Transform TRUE or FALSE value for 'mtrans' to a numeric value
  if (mtrans == TRUE){
    mtrans <- 1
  } else if (mtrans == FALSE){
    mtrans <- 0
  }
  
  # Transform TRUE or FALSE value for 'mtip' to a numeric value
  if (mtip == TRUE){
    mtip <- 1
  } else if (mtip == FALSE){
    mtip <- 0
  }
  
  # Transform TRUE or FALSE value for 'mshear' to a numeric value
  if (mshear == TRUE){
    mshear <- 1
  } else if (mshear == FALSE){
    mshear <- 0
  }
  
  # Transform TRUE or FALSE value for 'msplit' to a numeric value
  if (msplit == TRUE){
    msplit <- 1
  } else if (msplit == FALSE){
    msplit <- 0
  }
  
  # Transform TRUE or FALSE value for 'maqchem' to a numeric value
  if (maqchem == TRUE){
    maqchem <- 1
  } else if (maqchem == FALSE){
    maqchem <- 0
  }
  
  # Transform TRUE or FALSE value for 'mwet' to a numeric value
  if (mwet == TRUE){
    mwet <- 1
  } else if (mwet == FALSE){
    mwet <- 0
  }
  
  # Transform TRUE or FALSE value for 'mdry' to a numeric value
  if (mdry == TRUE){
    mdry <- 1
  } else if (mdry == FALSE){
    mdry <- 0
  }
  
  # Transform TRUE or FALSE value for 'mtilt' to a numeric value
  if (mtilt == TRUE){
    mtilt <- 1
  } else if (mtilt == FALSE){
    mtilt <- 0
  }
  
  # Transform TRUE or FALSE value for 'mrough' to a numeric value
  if (mrough == TRUE){
    mrough <- 1
  } else if (mrough == FALSE){
    mrough <- 0
  }
  
  # Transform TRUE or FALSE value for 'mpartl' to a numeric value
  if (mpartl == TRUE){
    mpartl <- 1
  } else if (mpartl == FALSE){
    mpartl <- 0
  }
  
  # Transform TRUE or FALSE value for 'mpartlba' to a numeric value
  if (mpartlba == TRUE){
    mpartlba <- 1
  } else if (mpartlba == FALSE){
    mpartlba <- 0
  }
  
  # Transform TRUE or FALSE value for 'mtinv' to a numeric value
  if (mtinv == TRUE){
    mtinv <- 1
  } else if (mtinv == FALSE){
    mtinv <- 0
  }
  
  # Transform TRUE or FALSE value for 'mpdf' to a numeric value
  if (mpdf == TRUE){
    mpdf <- 1
  } else if (mpdf == FALSE){
    mpdf <- 0
  }
  
  # Transform TRUE or FALSE value for 'msgtibl' to a numeric value
  if (msgtibl == TRUE){
    msgtibl <- 1
  } else if (msgtibl == FALSE){
    msgtibl <- 0
  }  
  
  # Transform TRUE or FALSE value for 'mbcon' to a numeric value
  if (mbcon == TRUE){
    mbcon <- 1
  } else if (mbcon == FALSE){
    mbcon <- 0
  }  
  
  # Transform TRUE or FALSE value for 'msource' to a numeric value
  if (msource == TRUE){
    msource <- 1
  } else if (msource == FALSE){
    msource <- 0
  }  
  
  # Generate a vector list of calpuff.inp keywords
  keywords <- c("MGAUSS", "MCTADJ", "MCTSG", "MSLUG", "MTRANS", "MTIP", "MRISE", 
                "MBDW", "MSHEAR", "MSPLIT", "MCHEM", "MAQCHEM", "MLWC", 
                "MWET", "MDRY", "MTILT", "MDISP", "MTURBVW", "MDISP2", 
                "MTAULY", "MTAUADV", "MCTURB", "MROUGH", "MPARTL", "MPARTLBA", 
                "MTINV", "MPDF", "MSGTIBL", "MBCON", "MSOURCE", "MFOG", "MREG")
  
  # Generate a vector list of the formatted replacements
  replacements <- c(mgauss, mctadj, mctsg, mslug, mtrans, mtip, mrise,
                    mbdw, mshear, msplit, mchem, maqchem, mlwc,
                    mwet, mdry, mtilt, mdisp, mturbvw, mdisp2,
                    mtauly, mtauadv, mcturb, mrough, mpartl, mpartlba,
                    mtinv, mpdf, msgtibl, mbcon, msource, mfog, mreg)
  
  # Modify all parameters in working calpuff.inp vector
  calpuff_inp_working <- replace_in_inp(inp_file_working = calpuff_inp_working,
                                        keyword = keywords,
                                        replacement = replacements)
  
  # Write the output to the same working calpuff.inp file
  writeLines(calpuff_inp_working, con = calpuff_inp)
  
}
