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
#' @param critfn 
#' @param alpha 
#' @param fextr2 
#' @param nbar 
#' @param kbar 
#' @param xbbar 
#' @param ybbar 
#' @param xebar 
#' @param yebar 
#' @param idiopt1 
#' @param isurft 
#' @param idiopt2 
#' @param iupt 
#' @param zupt 
#' @param idiopt3 
#' @param iupwnd 
#' @param zupwnd 
#' @param idiopt4 
#' @param idiopt5 
#' @param llbreze 
#' @param nbox 
#' @param xg1 
#' @param xg2 
#' @param yg1 
#' @param yg2 
#' @param xbcst 
#' @param ybcst 
#' @param xecst 
#' @param yecst 
#' @param nlb 
#' @param metbxid 
#' @export calmet_5_wind_field_opts_params

calmet_5_wind_field_opts_params <- function(calmet_inp,
                                            iwfcod,
                                            ifradj,
                                            ikine,
                                            iobr,
                                            islope,
                                            iextrp,
                                            icalm,
                                            bias,
                                            rmin2,
                                            iprog,
                                            isteppg,
                                            igfmet,
                                            lvary,
                                            rmax1,
                                            rmax2,
                                            rmax3,
                                            rmin,
                                            terrad,
                                            r1,
                                            r2,
                                            rprog,
                                            divlim,
                                            niter,
                                            nsmth,
                                            nintr2,
                                            critfn,
                                            alpha,
                                            fextr2,
                                            nbar,
                                            kbar,
                                            xbbar,
                                            ybbar,
                                            xebar,
                                            yebar,
                                            idiopt1,
                                            isurft,
                                            idiopt2,
                                            iupt,
                                            zupt,
                                            idiopt3,
                                            iupwnd,
                                            zupwnd,
                                            idiopt4,
                                            idiopt5,
                                            llbreze,
                                            nbox,
                                            xg1,
                                            xg2,
                                            yg1,
                                            yg2,
                                            xbcst,
                                            ybcst,
                                            xecst,
                                            yecst,
                                            nlb,
                                            metbxid){
  
  # Generate a vector list of calmet.inp keywords
  keywords <- c("IWFCOD", "IFRADJ", "IKINE", "IOBR", "ISLOPE", "IEXTRP", "ICALM",
                "BIAS", "RMIN2", "IPROG", "ISTEPPG", "IGFMET", "LVARY", "RMAX1", "RMAX2", "RMAX3",
                "RMIN", "TERRAD", "R1", "R2", "RPROG", "DIVLIM", "NITER", "NSMTH", "NINTR2", "CRITFN",
                "ALPHA", "FEXTR2", "NBAR", "KBAR", "XBBAR", "YBBAR", "XEBAR", "YEBAR", "IDIOPT1",
                "ISURFT", "IDIOPT2", "IUPT", "ZUPT", "IDIOPT3", "IUPWND", "ZUPWND", "IDIOPT4", "IDIOPT5",
                "LLBREZE", "NBOX", "XG1", "XG2", "YG1", "YG2", "XBCST", "YBCST", "XECST", "YECST",
                "NLB", "METBXID")
  
  # Generate a vector list of the formatted replacements
  replacements <- c(iwfcod, ifradj, ikine, iobr, islope, iextrp, icalm, bias, rmin2, iprog,
                    isteppg, igfmet, lvary, rmax1, rmax2, rmax3, rmin, terrad, r1, r2, rprog,
                    divlim, niter, nsmth, nintr2, critfn, alpha, fextr2, nbar, kbar, xbbar,
                    ybbar, xebar, yebar, idiopt1, isurft, idiopt2, iupt, zupt, idiopt3,
                    iupwnd, zupwnd, idiopt4, idiopt5, llbreze, nbox, xg1, xg2, yg1, yg2,
                    xbcst, ybcst, xecst, yecst, nlb, metbxid)
  
  # Modify all parameters in working calmet.inp vector
  calmet_inp_working <- replace_in_inp(calmet_inp_working = calmet_inp_working,
                                       keyword = keywords,
                                       replacement = replacements)
  
}
