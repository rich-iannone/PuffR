#' Set CALMET parameters for wind field options and parameters
#' @description This function validates and writes CALMET parameters for wind field options and parameters to the working CALMET.INP file.
#' @param calmet_inp the absolute path and filename for the working CALMET input file.
#' @param iwfcod 
#' @param ifradj 
#' @param ikine 
#' @param iobr 
#' @param islope 
#' @param iextrp 
#' @param icalm 
#' @param bias 
#' @param rmin2 
#' @param iprog 
#' @param isteppg 
#' @param igfmet 
#' @param lvary 
#' @param rmax1 
#' @param rmax2 
#' @param rmax3 
#' @param rmin 
#' @param terrad 
#' @param r1 
#' @param r2 
#' @param rprog 
#' @param divlim 
#' @param niter 
#' @param nsmth 
#' @param nintr2 
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
                "RMIN", "TERRAD" "R1", "R2", "RPROG", "DIVLIM", "NITER", "NSMTH", "NINTR2", "CRITFN",
                "ALPHA", "FEXTR2", "NBAR", "KBAR", "XBBAR", "YBBAR", "XEBAR", "YEBAR", "IDIOPT1",
                "ISURFT", "IDIOPT2", "IUPT", "ZUPT", "IDIOPT3", "IUPWND", "ZUPWND", "IDIOPT4", "IDIOPT5",
                "LLBREZE", "NBOX", "XG1", "XG2", "YG1", "YG2", "XBCST", "YBCST", "XECST", "YECST",
                "NLB", "METBXID")
