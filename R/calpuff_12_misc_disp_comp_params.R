#' Set miscellaneous CALPUFF dispersion and computational parameters
#' @description This function validates and writes miscellaneous CALPUFF dispersion and computational parameters.
#' @param calpuff_inp the absolute path and filename for the working CALPUFF input file.
#' @param sytdep the horizontal size of puff (in units of meters) beyond which the time-dependent Heffter dispersion equations are used to determine sigma-y and sigma-z.
#' @param mhftsz the choice of whether to use the Heffter equation to calculate sigma z values.
#' @param jsup the stability class used to determine plume growth rates for puffs above the boundary layer.
#' @param conk1 the vertical dispersion constant for stable conditions.
#' @param conk2 the vertical dispersion constant for neutral and unstable conditions.
#' @param tbd the factor for determining the transition-point from Schulman-Scire to Huber-Snyder Building downwash schemes.
#' @param iurb1 the lower range of land use categories for which urban dispersion is assumed.
#' @param iurb2 the upper range of land use categories for which urban dispersion is assumed.
#' @param ilanduin the land use category for the modeling domain. Used for site characterization parameters with single-point Met data files and ignored unless metfm is either 2, 3, 4, or 5.
#' @param z0in the roughness length (in units of meters) for the modeling domain. Used for site characterization parameters with single-point Met data files and ignored unless metfm is either 2, 3, 4, or 5.
#' @param xlaiin the leaf area index for the modeling domain. Used for site characterization parameters with single-point Met data files and ignored unless metfm is either 2, 3, 4, or 5.
#' @param elevin the elevation of the site (in units of meters ASL). Used for site characterization parameters with single-point Met data files and ignored unless metfm is either 2, 3, 4, or 5.
#' @param xlatin the latitude of the site (in decimal degrees). Used for site characterization parameters with single-point Met data files and ignored unless metfm is either 2, 3, 4, or 5.
#' @param xlonin the longitude of the site (in decimal degrees). Used for site characterization parameters with single-point Met data files and ignored unless metfm is either 2, 3, 4, or 5.
#' @param anemht the anemometer height at the site (in units of meters ASL). Used for site characterization parameters with single-point Met data files and ignored unless metfm is either 2 or 3.
#' @param isigmav the choice of which lateral turbulance data in PROFILE.DAT file to use: (0) read sigma-theta, or (1) read sigma-v. Used for site characterization parameters with single-point Met data files and ignored unless metfm is either 4 or 5, and mturbvw is either 1 or 3.
#' @param imixctdm the choice of method for reading mixing heights: (0) read predicted, or (1) read observed. Used for site characterization parameters with single-point Met data files and ignored unless metfm is 4.
#' @param xmxlen the maximum length of a slug (in meteorological grid cell units).
#' @param xsamlen the maximum travel distance of a slug or puff (in meteorological grid cell units) during each time step.
#' @param mxnew the maximum number of slugs or puffs released from one source during each time step.
#' @param mxsam the maximum number of sampling steps for one puff or slug during each time step.
#' @param ncount the number of iterations used when computing the transport wind for a sampling step that includes gradual rise.
#' @param symin the minimum sigma y (in units of meters) for a new puff or slug.
#' @param szmin the minimum sigma z (in units of meters) for a new puff or slug.
#' @param szcap_m the maximum sigma z (in units of meters) that is permitted.
#' @param svmin the default minimum turbulence velocities sigma v (in units of meters per second) for each stability class over land and over water.
#' @param swmin the default minimum turbulence velocities sigma w (in units of meters per second) for each stability class over land and over water.
#' @param cdiv the divergence criterion for dw/dz across the puff; used to initiate adjustment for horizontal convergence.
#' @param nlutibl the search radius (in meteorological grid cell units) for nearest land and water cells.
#' @param wscalm the minimum wind speed (in units of meters per second) allowed for non-calm conditions.
#' @param xmaxzi the maximum mixing height (in units of meters).
#' @param xminzi the minimum mixing height (in units of meters).
#' @param wscat the upper-bound wind-speeds (in units of meters per second) for the the default wind speed classes.
#' @param plx0 the default wind speed profile power-law exponent values for stability classes 1 through 6.
#' @param ptg0 the default potential temperature gradient values for stable classes E and F.
#' @param ppc the default plume path coefficients for each stability class (ignored unless the option for partial plume height terrain adjustment, mctadj, is selected by using the value 3).
#' @param sl2pf the slug-to-puff transition criterion factor equal to sigma-y or length of a slug.
#' @param nsplit the number of puffs that result every time a puff is split vertically.
#' @param iresplit the time(s) of a day when split puffs are eligible to be split vertically once again.
#' @param zisplit the minimum mixing height (in units of meters) for which vertical puff-splitting is allowed in the subsequent hour.
#' @param roldmax the maximum value of ratio of the mixing height to the maximum mixing height experienced by the puff for which vertical puff-splitting is allowed in the subsequent hour.
#' @param nsplith the number of puffs that result every time a puff is split horizontally.
#' @param sysplith the minimum sigma-y (in meteorological grid cell units) of puff before horizontal puff-splitting is allowed.
#' @param shsplith the minimum puff elongation rate (sysplith units per hour) due to wind shear before horizontal puff-splitting is allowed.
#' @param cnsplith the minimum concentration (in units of grams per cubic meter) of each species within a puff before it may be split horizontally. Provide vector of length corresponding to the number of species; if a single value is provided, it will be applied to all species.
#' @param epsslug the fractional convergence criterion for numerical slug sampling integration.
#' @param epsarea the fractional convergence criterion for numerical area source integration.
#' @param dsrise the trajectory step-length (in units of meters) used for numerical rise integration.
#' @param htminbc the minimum height (in units of meters) to which boundary condition puffs are mixed as they are emitted (ignored unless mbcon is 2).
#' @param rsampbc the search radious (in units of km) about a receptor for sampling the nearest boundary condition puff.
#' @param mdepbc the choice of whether to perform a near-surface depletion adjustment to the concentration profile when sampling boundary condition puffs.
#' @export calpuff_12_misc_disp_comp_params

calpuff_12_misc_disp_comp_params <- function(calpuff_inp = "calpuff_template.txt",
                                             sytdep = 550.0,
                                             mhftsz = FALSE,
                                             jsup = 5,
                                             conk1 = 0.01,
                                             conk2 = 0.1,
                                             tbd = 0.5,
                                             iurb1 = 10,
                                             iurb2 = 19,
                                             ilanduin = 20,
                                             z0in = 0.25,
                                             xlaiin = 0.0,
                                             elevin = 0.0,
                                             xlatin = 0.0,
                                             xlonin = 0.0,
                                             anemht = 10.0,
                                             isigmav = 1,
                                             imixctdm = 0,
                                             xmxlen = 1.0,
                                             xsamlen = 1.0,
                                             mxnew = 99,
                                             mxsam = 99,
                                             ncount = 2,
                                             symin = 1.0,
                                             szmin = 1.0,
                                             szcap_m = 5.0e6,
                                             svmin = c(0.50, 0.50, 0.50, 0.50, 0.50, 0.50,
                                                       0.37, 0.37, 0.37, 0.37, 0.37, 0.37),
                                             swmin = c(0.20, 0.12, 0.08, 0.06, 0.03, 0.016,
                                                       0.20, 0.12, 0.08, 0.06, 0.03, 0.016),
                                             cdiv = c(0.0, 0.0),
                                             nlutibl = 4,
                                             wscalm = 0.5,
                                             xmaxzi = 3000.0,
                                             xminzi = 20.0,
                                             wscat = c(1.54, 3.09, 5.14, 8.23, 10.80),
                                             plx0 = c(0.07, 0.07, 0.10, 0.15, 0.35, 0.55),
                                             ptg0 = c(0.020, 0.035),
                                             ppc = c(0.50, 0.50, 0.50, 0.50, 0.35, 0.35),
                                             sl2pf = 10.0,
                                             nsplit = 3,
                                             iresplit = c(0, 0, 0, 0, 0, 0,
                                                          0, 0, 0, 0, 0, 0,
                                                          0, 0, 0, 0, 0, 1,
                                                          0, 0, 0, 0, 0, 0),
                                             zisplit = 100.0,
                                             roldmax = 0.25,
                                             nsplith = 5,
                                             sysplith = 1.0,
                                             shsplith = 2.0,
                                             cnsplith = 1.0e-7,
                                             epsslug = 1.0e-4,
                                             epsarea = 1.0e-6,
                                             dsrise = 1.0,
                                             htminbc = 500.0,
                                             rsampbc = 10.0,
                                             mdepbc = TRUE){
 
  # Read in the working calpuff.inp file as a character vector
  calpuff_inp_working <- readLines(calpuff_inp, warn = FALSE)
  
  # Generate a vector list of calpuff.inp keywords
  keywords <- c("SYTDEP", "MHFTSZ", "JSUP", "CONK1", "CONK2", "TBD", "IURB1", "IURB2",
                "ILANDUIN", "Z0IN", "XLAIIN", "ELEVIN", "XLATIN", "XLONIN", "ANEMHT",
                "ISIGMAV", "IMIXCTDM", "XMXLEN", "XSAMLEN", "MXNEW", "MXSAM", "NCOUNT",
                "SYMIN", "SZMIN", "SZCAP_M", "SVMIN", "SWMIN", "CDIV", "NLUTIBL", "WSCALM",
                "XMAXZI", "XMINZI", "WSCAT", "PLX0", "PTG0", "PPC", "SL2PF", "NSPLIT",
                "IRESPLIT", "ZISPLIT", "ROLDMAX", "NSPLITH", "SYSPLITH", "SHSPLITH",
                "CNSPLITH", "EPSSLUG", "EPSAREA", "DSRISE", "HTMINBC", "RSAMPBC", "MDEPBC")
  
  # Generate a vector list of the formatted replacements
  replacements <- c(sytdep, mhftsz, jsup, conk1, conk2, tbd, iurb1, iurb2,
                    ilanduin, z0in, xlaiin, elevin, xlatin, xlonin, anemht,
                    isigmav, imixctdm, xmxlen, xsamlen, mxnew, mxsam, ncount,
                    symin, szmin, szcap_m, svmin, swmin, cdiv, nlutibl, wscalm,
                    xmaxzi, xminzi, wscat, plx0, ptg0, ppc, sl2pf, nsplit,
                    iresplit, zisplit, roldmax, nsplith, sysplith, shsplith,
                    cnsplith, epsslug, epsarea, dsrise, htminbc, rsampbc, mdepbc)
  
  # Modify all parameters in working calpuff.inp vector
  calpuff_inp_working <- replace_in_inp(inp_file_working = calpuff_inp_working,
                                        keyword = keywords,
                                        replacement = replacements)
  
  # Write the output to the same working calpuff.inp file
  writeLines(calpuff_inp_working, con = calpuff_inp)
  
}
