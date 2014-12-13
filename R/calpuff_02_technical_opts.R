#' Set CALPUFF technical options
#' @description This function validates and writes CALPUFF parameters for the model's technical options.
#' @param calpuff_inp the absolute path and filename for the working CALPUFF input file.
#' @param mgauss 
#' @param mctadj 
#' @param mctsg 
#' @param mslug 
#' @param mtrans 
#' @param mtip 
#' @param mrise 
#' @param mbdw 
#' @param mshear 
#' @param msplit 
#' @param mchem 
#' @param maqchem 
#' @param mlwc 
#' @param mwet 
#' @param mdry 
#' @param mtilt 
#' @param mdisp 
#' @param mturbvw 
#' @param mdisp2 
#' @param mtauly 
#' @param mtauadv 
#' @param mcturb 
#' @param mrough 
#' @param mpartl 
#' @param mpartlba 
#' @param mtinv 
#' @param mpdf 
#' @param msgtibl 
#' @param mbcon 
#' @param msource 
#' @param mfog 
#' @param mreg 
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
                                      mtauly = 0,
                                      mtauadv = 0,
                                      mcturb = 1,
                                      mrough = 0,
                                      mpartl = 1,
                                      mpartlba = 1,
                                      mtinv = 0,
                                      mpdf = 0,
                                      msgtibl = 0,
                                      mbcon = 0,
                                      msource = 0,
                                      mfog = 0,
                                      mreg = 0){
  
  # Generate a vector list of calmet.inp keywords
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
  
  # Modify all parameters in working calmet.inp vector
  calpuff_inp_working <- replace_in_inp(inp_file_working = calpuff_inp_working,
                                        keyword = keywords,
                                        replacement = replacements)
  
  # Write the output to the same working calmet.inp file
  writeLines(calpuff_inp_working, con = calpuff_inp)
  
}
