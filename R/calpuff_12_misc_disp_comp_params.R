#' Set miscellaneous CALPUFF dispersion and computational parameters
#' @description This function validates and writes miscellaneous CALPUFF dispersion and computational parameters.
#' @param calpuff_inp the absolute path and filename for the working CALPUFF input file.
#' @param sytdep 
#' @param mhftsz 
#' @param jsup 
#' @param conk1 
#' @param conk2 
#' @param tbd 
#' @param iurb1 
#' @param iurb2 
#' @param ilanduin 
#' @param z0in 
#' @param xlaiin 
#' @param elevin 
#' @param xlatin 
#' @param xlonin 
#' @param anemht 
#' @param isigmav 
#' @param imixctdm 
#' @param xmxlen 
#' @param xsamlen 
#' @param mxnew 
#' @param mxsam 
#' @param ncount 
#' @param symin 
#' @param szmin 
#' @param szcap_m 
#' @param svmin 
#' @param swmin 
#' @param cdiv 
#' @param nlutibl 
#' @param wscalm 
#' @param xmaxzi 
#' @param xminzi 
#' @param wscat 
#' @param plx0 
#' @param ptg0 
#' @param ppc 
#' @param sl2pf 
#' @param nsplit 
#' @param iresplit 
#' @param zisplit 
#' @param roldmax 
#' @param nsplith 
#' @param sysplith 
#' @param shsplith 
#' @param cnsplith 
#' @param epsslug 
#' @param epsarea 
#' @param dsrise 
#' @param htminbc 
#' @param rsampbc 
#' @param mdepbc 
#' @export calpuff_12_misc_disp_comp_params

calpuff_12_misc_disp_comp_params <- function(calpuff_inp = "calpuff_template.txt",
                                             sytdep,
                                             mhftsz,
                                             jsup,
                                             conk1,
                                             conk2,
                                             tbd,
                                             iurb1,
                                             iurb2,
                                             ilanduin,
                                             z0in,
                                             xlaiin,
                                             elevin,
                                             xlatin,
                                             xlonin,
                                             anemht,
                                             isigmav,
                                             imixctdm,
                                             xmxlen,
                                             xsamlen,
                                             mxnew,
                                             mxsam,
                                             ncount,
                                             symin,
                                             szmin,
                                             szcap_m,
                                             svmin,
                                             swmin,
                                             cdiv,
                                             nlutibl,
                                             wscalm,
                                             xmaxzi,
                                             xminzi,
                                             wscat,
                                             plx0,
                                             ptg0,
                                             ppc,
                                             sl2pf,
                                             nsplit,
                                             iresplit,
                                             zisplit,
                                             roldmax,
                                             nsplith,
                                             sysplith,
                                             shsplith,
                                             cnsplith,
                                             epsslug,
                                             epsarea,
                                             dsrise,
                                             htminbc,
                                             rsampbc,
                                             mdepbc){
 
  # Read in the working calpuff.inp file as a character vector
  calpuff_inp_working <- readLines(calpuff_inp, warn = FALSE)
  
  # Generate a vector list of calmet.inp keywords
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
  
  # Modify all parameters in working calmet.inp vector
  calpuff_inp_working <- replace_in_inp(inp_file_working = calpuff_inp_working,
                                        keyword = keywords,
                                        replacement = replacements)
  
  # Write the output to the same working calmet.inp file
  writeLines(calpuff_inp_working, con = calpuff_inp)
  
}
