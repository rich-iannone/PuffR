#' Set miscellaneous CALPUFF dispersion and computational parameters
#' @description This function validates and writes miscellaneous CALPUFF dispersion and computational parameters.
#' @param calpuff_inp the absolute path and filename for the working CALPUFF input file.
#' @param sytdep Horizontal size of puff (in units of meters) beyond which the time-dependent Heffter dispersion equations are used to determine sigma-y and sigma-z.
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
