#' Set CALMET parameters for mixing height, temperature, and precipitation
#' @description This function validates and writes CALMET parameters for wind field options and parameters to the working CALMET.INP file.
#' @param calmet_inp the absolute path and filename for the working CALMET input file.
#' @param constb empirical mixing height constant for the neutral, mechanical equation.
#' @param conste empirical mixing height constant for the convective mixing height equation.
#' @param constn empirical mixing height constant for the stable mixing height equation.
#' @param constw empirical mixing height constant for the overwater mixing height equation.
#' @param fcoriol absolute value of the Coriolis parameter.
#' @param iavezi conduct spatial averaging of mixing heights?
#' @param mnmdav maximum search radius (in grid cell units) for averaging of mixing heights.
#' @param hafang half-angle of upwind looking cone for averaging of mixing heights.
#' @param ilevzi layer of winds used in upwind averaging of mixing heights.
#' @param imixh method for computation of the convective mixing height: (1) Maul-Carson for land and water cells, (-1) Maul-Carson for land and water cells and OCD mixing heights overwater, (2) Batchvarova and Gryning for land and water cells, and (-2) Batchvarova and Gryning for land and water cells and OCD mixing heights overwater. 
#' @param threshl threshold buoyancy flux required to sustain convective mixing height growth overland. Expressed as a heat flux per meter of boundary layer in units of W/m3.
#' @param threshw threshold buoyancy flux required to sustain convective mixing height growth overwater. Expressed as a heat flux per meter of boundary layer in units of W/m3.
#' @param itwprog option for overwater lapse rates used in convective mixing height growth: (0) use SEA.DAT lapse rates and deltaT (or assume neutral conditions if missing), (1) use prognostic lapse rates (only if IPROG > 2) and SEA.DAT deltaT (or neutral if missing), (2) use prognostic lapse rates and prognostic delta T (only if IPROG > 12 and 3D.DAT version 2.0 or higher).
#' @param iluoc3d land use category for ocean in 3D.DAT datasets.
#' @param dptmin minimum potential temperature lapse rate in the stable layer above the current convective mixing height (in units of K/m).
#' @param dzzi depth of layer (in meters) above current convective mixing height through which lapse rate is computed.
#' @param zimin minimum overland mixing height (in meters).
#' @param zimax maximum overland mixing height (in meters).
#' @param ziminw minimum overwater mixing height (in meters).
#' @param zimaxw maximum overwater mixing height (in meters).
#' @param icoare method for computation of overwater surface fluxes: (0) original deltaT method, (10) COARE with no wave parameterization (jwave = 0), (11) COARE with wave option (jwave = 1) and default wave properties, (-11) COARE with wave option (jwave = 1) and observed wave properties (must be in SEA.DAT files), (12) COARE with wave option 2 (Taylor and Yelland) and default wave properties, and (-12) COARE with wave option 2 (Taylor and Yelland) and observed wave properties (must be in SEA.DAT files). 
#' @param dshelf coastal/shallow water length scale (in kilometers) for modified z0 in shallow water.
#' @param iwarm COARE warm layer computation: (0) off, (1) on.
#' @param icool COARE cool skin layer computation: (0) off, (1) on.
#' @param itprog selection of temperature data from observations or prognostic data: (0) use both surface and upper air stations, (1) use both surface stations and MM5/3D files for upper air data (no upper air observations), (2) MM5/3D files for both the surface and upper air data (no surface or upper air observations).
#' @param irad interpolation type: (1) 1/R, (2) 1/R^2.
#' @param tradkm radius of influence (in kilometers) for temperature interpolation.
#' @param numts maximum number of stations to include in temperature interpolation.
#' @param iavet conduct spatial averaging of temperatures?
#' @param tgdefb default temperature gradient (in units of K/m) below the mixing height over water.
#' @param tgdefa default temperature gradient (in units of K/m) above the mixing height over water.
#' @param jwat1 beginning land use category value for temperature interpolation over water.
#' @param jwat2 ending land use category value for temperature interpolation over water.
#' @param nflagp method of interpolation for precipitation: (1) 1/R, (2) 1/R^2, (3) exp/R^2.
#' @param sigmap radius of influence (in kilometers) for interpolation of precipitation.
#' @param cutp minimum precipitation rate cutoff (in units of mm/h).
#' @export calmet_6_mixhgt_temp_precip_params

calmet_6_mixhgt_temp_precip_params <- function(calmet_inp,
                                               constb = 1.41,
                                               conste = 0.15,
                                               constn = 2400,
                                               constw = 0.16,
                                               fcoriol = 1E-4,
                                               iavezi = 1,
                                               mnmdav = 10,
                                               hafang = 30,
                                               ilevzi = 1,
                                               imixh = 1,
                                               threshl = 0.05,
                                               threshw = 0.05,
                                               itwprog = 0,
                                               iluoc3d = 16,
                                               dptmin = 0.001,
                                               dzzi = 200,
                                               zimin = 50,
                                               zimax = 3000,
                                               ziminw = 50,
                                               zimaxw = 3000,
                                               icoare = 10,
                                               dshelf = 0,
                                               iwarm = 0,
                                               icool = 0,
                                               itprog = 0,
                                               irad = 1,
                                               tradkm = 500,
                                               numts = 5,
                                               iavet = 1,
                                               tgdefb = -0.0098,
                                               tgdefa = -0.0045,
                                               jwat1 = 55,
                                               jwat2 = 55,
                                               nflagp = 2,
                                               sigmap = 100,
                                               cutp = 0.01){
  
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
  calmet_inp_working <- replace_in_inp(inp_file_working = calmet_inp_working,
                                       keyword = keywords,
                                       replacement = replacements)    
  
  # Write the output to the same working calmet.inp file
  writeLines(calmet_inp_working, con = calmet_inp)
  
}
