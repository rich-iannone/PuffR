#' Set CALMET parameters for wind field options and other options
#' @description This function validates and writes CALMET parameters for wind field options and parameters to the working CALMET.INP file.
#' @param calmet_inp the absolute path and filename for the working CALMET input file.
#' @param iwfcod for model, choose betwen: (0) objective analysis only, or (1) diagnostic wind module.
#' @param ifradj compute Froude number adjustment effects.
#' @param ikine compute kinematic effects.
#' @param iobr use O'Brien procedure for adjustment of the vertical velocity.
#' @param islope compute slope flow effects.
#' @param iextrp extrapolate surface wind observations to upper layers.
#' @param icalm extrapolate surface winds even if calm?
#' @param bias layer-dependent biases modifying the weights of surface and upper air stations
#' @param rmin2 minimum distance from nearest upper air station to surface station for which extrapolation of surface winds at surface station will be allowed.
#' @param iprog use gridded prognostic wind field model output fields as input to the diagnostic wind field model.
#' @param isteppg timestep in hours of the prognostic model input data.
#' @param igfmet use coarse CALMET fields as initial guess fields.
#' @param lvary use varying radius of influence.
#' @param rmax1 maximum radius of influence over land in the surface layer.
#' @param rmax2 maximum radius of influence over land aloft.
#' @param rmax3 maximum radius of influence over water.
#' @param rmin minimum radius of influence used in the wind field interpolation.
#' @param terrad radius of influence of terrain features.
#' @param r1 the relative weighting of the first guess field and observations in the surface layer.
#' @param r2 the relative weighting of the first guess field and observations in layers aloft.
#' @param rprog relative weighting parameter of the prognostic wind field data.
#' @param divlim maximum acceptable divergence in the divergence minimization procedure.
#' @param niter maximum number of iterations in the divergence min. procedure.
#' @param nsmth number of passes in the smoothing procedure.
#' @param nintr2 maximum number of stations used in each layer for the interpolation of data to a grid point.
#' @param critfn the critical Froude number.
#' @param alpha an empirical factor controlling the influence of kinematic effects.
#' @param fextr2 multiplicative scaling factors for extrapolation of surface observations to upper layers.
#' @param nbar number of barriers to interpolation of the wind fields.
#' @param kbar level up to which barriers apply (must be value in the range of 1 to NZ).
#' @param xbbar the x coordinates for the beginning of each barrier.
#' @param ybbar the y coordinates for the beginning of each barrier.
#' @param xebar the x coordinates for the ending of each barrier.
#' @param yebar the y coordinates for the ending of each barrier.
#' @param idiopt1 method for computation of surface temperatures: (0) compute internally from hourly surface observations, (1) read preprocessed values from a data file (DIAG.DAT).
#' @param isurft surface meteorological station to use for the surface temperature (must be value in the range of 1 to nssta).
#' @param idiopt2 method for computation of domain-averaged temperature lapse rate: (0) compute internally from twice-daily upper air observations, (1) read hourly preprocessed values from a data file (DIAG.DAT).
#' @param iupt upper air station to use for the domain-scale lapse rate (must be a value in the range of 1 to NUSTA).
#' @param zupt Depth in meters through which the domain-scale lapse rate is computed.
#' @param idiopt3 method for computation of domain-averaged wind components: (0) compute internally from twice-daily upper air observations, (1) read hourly preprocessed values a data file (DIAG.DAT).
#' @param iupwnd upper air station to use for the domain-scale winds (must be a value in the range of -1 to NUSTA).
#' @param zupwnd bottom and top of layer through which the domain-scale winds are computed.
#' @param idiopt4 selection of observed surface wind components for wind field module: (0) read WS and WD values from a surface data file (SURF.DAT), (1) read hourly preprocessed U and V values from a data file (DIAG.DAT).
#' @param idiopt5 selection of observed upper air wind components for wind field module: (0) read WS and WD values from an upper air data file (UP.DAT), (1) read hourly preprocessed U and V values from a data file (DIAG.DAT).
#' @param llbreze use lake breeze module?
#' @param nbox number of lake breeze regions.
#' @param xg1 the x direction grid line 1 defining the region of interest for the lake breeze module.
#' @param xg2 the x direction grid line 2 defining the region of interest for the lake breeze module.
#' @param yg1 the y direction grid line 1 defining the region of interest for the lake breeze module.
#' @param yg2 the y direction grid line 2 defining the region of interest for the lake breeze module.
#' @param xbcst the beginning x point (in kilometers) defining the coastline (straight line).
#' @param ybcst the beginning y point (in kilometers) defining the coastline (straight line).
#' @param xecst the ending x point (in kilometers) defining the coastline (straight line).
#' @param yecst the ending y point (in kilometers) defining the coastline (straight line).
#' @param nlb the combined number of meteorological and upper air stations in the region.
#' @param metbxid station identifiers for the region; include surface stations first, then upper air stations.
#' @export calmet_05_wind_field_opts_params

calmet_05_wind_field_opts_params <- function(calmet_inp = "calmet_template.txt",
                                             iwfcod = 1,
                                             ifradj = 1,
                                             ikine = 0,
                                             iobr = 0,
                                             islope = 1,
                                             iextrp = -4,
                                             icalm = 0,
                                             bias = c(-1, -1, -0.5, -0.3, 0, 1, 1, 1, 1, 1, 1, 1),
                                             rmin2 = -1.0,
                                             iprog = 0,
                                             isteppg = 1,
                                             igfmet = 0,
                                             lvary = TRUE,
                                             rmax1 = 5.0,
                                             rmax2 = 10.0,
                                             rmax3 = 20.0,
                                             rmin = 0.1,
                                             terrad = 5.0,
                                             r1 = 2.0,
                                             r2 = 1.0,
                                             rprog = 0.0,
                                             divlim = 5.0E-6,
                                             niter = 50,
                                             nsmth = c(2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4),
                                             nintr2 = 99,
                                             critfn = 1.0,
                                             alpha = 0.1,
                                             fextr2 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                                             nbar = 0,
                                             kbar = 10,
                                             xbbar = 0,
                                             ybbar = 0,
                                             xebar = 0,
                                             yebar = 0,
                                             idiopt1 = 0,
                                             isurft = 1,
                                             idiopt2 = 0,
                                             iupt = 1,
                                             zupt = 200,
                                             idiopt3 = 0,
                                             iupwnd = -1,
                                             zupwnd = c(1, 1000),
                                             idiopt4 = 0,
                                             idiopt5 = 0,
                                             llbreze = FALSE,
                                             nbox = 0,
                                             xg1 = 0,
                                             xg2 = 0,
                                             yg1 = 0,
                                             yg2 = 0,
                                             xbcst = 0,
                                             ybcst = 0,
                                             xecst = 0,
                                             yecst = 0,
                                             nlb = 0,
                                             metbxid = 0){
  
  # Read in the working calmet.inp file as a character vector
  calmet_inp_working <- readLines(calmet_inp, warn = FALSE)
  
  # Generate a formatted character string for 'bias'
  bias <- paste(bias, collapse = ", ")
  
  # Generate a formatted character string for 'nsmth'
  nsmth <- paste(nsmth, collapse = ", ")
  
  # Generate a formatted character string for 'fextr2'
  fextr2 <- paste(fextr2, collapse = ", ")
  
  # Generate a formatted character string for 'zupwnd'
  zupwnd <- paste(zupwnd, collapse = ", ")
  
  # Generate a vector list of calmet.inp keywords that require single values
  keywords <- c("IWFCOD", "IFRADJ", "IKINE", "IOBR", "ISLOPE", "IEXTRP", "ICALM",
                "RMIN2", "IPROG", "ISTEPPG", "IGFMET", "LVARY", "RMAX1", "RMAX2", "RMAX3",
                "RMIN", "TERRAD", "R1", "R2", "RPROG", "DIVLIM", "NITER", "NINTR2", "CRITFN",
                "ALPHA", "NBAR", "KBAR", "XBBAR", "YBBAR", "XEBAR", "YEBAR", "IDIOPT1",
                "ISURFT", "IDIOPT2", "IUPT", "ZUPT", "IDIOPT3", "IUPWND", "IDIOPT4", "IDIOPT5",
                "LLBREZE", "NBOX", "XG1", "XG2", "YG1", "YG2", "XBCST", "YBCST", "XECST", "YECST",
                "NLB", "METBXID")
  
  # Generate a vector list of the formatted single-value replacements
  replacements <- c(iwfcod, ifradj, ikine, iobr, islope, iextrp, icalm,
                    rmin2, iprog, isteppg, igfmet, lvary, rmax1, rmax2, rmax3,
                    rmin, terrad, r1, r2, rprog, divlim, niter, nintr2, critfn,
                    alpha, nbar, kbar, xbbar, ybbar, xebar, yebar, idiopt1,
                    isurft, idiopt2, iupt, zupt, idiopt3, iupwnd, idiopt4, idiopt5,
                    llbreze, nbox, xg1, xg2, yg1, yg2, xbcst, ybcst, xecst, yecst,
                    nlb, metbxid)
  
  # Modify all parameters that require single values in working calmet.inp vector
  calmet_inp_working <- replace_in_inp(inp_file_working = calmet_inp_working,
                                       keyword = keywords,
                                       replacement = replacements)
  
  # Add formatted 'bias' character string to the working calmet.inp vector
  calmet_inp_working <- replace_in_inp(inp_file_working = calmet_inp_working,
                                       keyword = "BIAS",
                                       replacement = bias)
  
  # Add formatted 'nsmth' character string to the working calmet.inp vector
  calmet_inp_working <- replace_in_inp(inp_file_working = calmet_inp_working,
                                       keyword = "NSMTH",
                                       replacement = nsmth)
  
  # Add formatted 'fextr2' character string to the working calmet.inp vector
  calmet_inp_working <- replace_in_inp(inp_file_working = calmet_inp_working,
                                       keyword = "FEXTR2",
                                       replacement = fextr2)
  
  # Add formatted 'zupwnd' character string to the working calmet.inp vector
  calmet_inp_working <- replace_in_inp(inp_file_working = calmet_inp_working,
                                       keyword = "ZUPWND",
                                       replacement = zupwnd)
  
  # Write the output to the same working calmet.inp file
  writeLines(calmet_inp_working, con = calmet_inp)
  
}
