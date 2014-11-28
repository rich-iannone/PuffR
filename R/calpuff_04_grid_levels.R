#' Set CALPUFF parameters for the map projection, datum, and grid definitions
#' @description This function validates and writes CALPUFF parameters for the map projection, datum, and grid definitions to the working CALPUFF.INP file.
#' @param calpuff_inp 
#' @param pmap 
#' @param feast 
#' @param fnorth 
#' @param iutmzn 
#' @param utmhem 
#' @param rlat0 
#' @param rlon0 
#' @param xlat1 
#' @param xlat2 
#' @param datum 
#' @param nx 
#' @param ny 
#' @param dgridkm 
#' @param xorigkm 
#' @param yorigkm 
#' @param nz 
#' @param zface 
#' @param ibcomp 
#' @param jbcomp 
#' @param iecomp 
#' @param jecomp 
#' @param lsamp 
#' @param ibsamp 
#' @param jbsamp 
#' @param iesamp 
#' @param jesamp 
#' @param meshdn 
#' @export calpuff_04_grid_levels

calpuff_04_grid_levels <- function(calpuff_inp,
                                   pmap = "UTM",
                                   feast = 0.0,
                                   fnorth = 0.0,
                                   iutmzn = NULL,
                                   utmhem = NULL,
                                   rlat0 = "40N",
                                   rlon0 = "90W",
                                   xlat1 = "30N",
                                   xlat2 = "60N",
                                   datum = "WGS-84",
                                   nx = NULL,
                                   ny = NULL,
                                   dgridkm = NULL,
                                   xorigkm = NULL,
                                   yorigkm = NULL,
                                   nz = 12,
                                   zface = c(0,20,40,80,100,150,200,300,400,800,1400,2000,3000),
                                   ibcomp,
                                   jbcomp,
                                   iecomp,
                                   jecomp,
                                   lsamp,
                                   ibsamp,
                                   jbsamp,
                                   iesamp,
                                   jesamp,
                                   meshdn){
  
  # Generate a vector list of calmet.inp keywords
  keywords <- c("PMAP", "FEAST", "FNORTH", "IUTMZN", "UTMHEM", "RLAT0", "RLON0", 
                "XLAT1", "XLAT2", "DATUM", "NX", "NY", "DGRIDKM", "XORIGKM", "YORIGKM", 
                "NZ", "ZFACE", "IBCOMP", "JBCOMP", "IECOMP", "JECOMP", "LSAMP",
                "IBSAMP", "JBSAMP", "IESAMP", "JESAMP", "MESHDN")
  
  # Generate a vector list of the formatted replacements
  replacements <- c(pmap, feast, fnorth, iutmzn, utmhem, rlat0, rlon0, 
                    xlat1, xlat2, datum, nx, ny, dgridkm, xorigkm, yorigkm, 
                    nz, zface, ibcomp, jbcomp, iecomp, jecomp, lsamp,
                    ibsamp, jbsamp, iesamp, jesamp, meshdn)
  
  # Modify all parameters in working calmet.inp vector
  calpuff_inp_working <- replace_in_inp(inp_file_working = calpuff_inp_working,
                                        keyword = keywords,
                                        replacement = replacements)
  
  # Write the output to the same working calmet.inp file
  writeLines(calpuff_inp_working, con = calpuff_inp)
  
}
