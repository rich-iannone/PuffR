#' Generate a template for a READ62 input file
#' @description Generate a template for a READ62 input file.
#' @export read62_inp_generate_template

read62_inp_generate_template <- function(){
  
  writeLines(
    c("READ62.INP      2.1             Hour Start and End Times with Seconds",
      "",
      "0 -- Input and Output Files",
      "",
      "! INDAT  = !",
      "! SUBDAT = !",
      "! UPDAT  = !",
      "! RUNLST = !",
      "",
      "! LCFILES = T !",
      "",
      "!END!",
      "",
      "--------------------------------------------------------------------------------",
      "",
      "1 -- Run control parameters",
      "",
      "Starting date/time",
      "! IBYR  = !",
      "! IBMO  = !",
      "! IBDY  = !",
      "! IBHR  = !",
      "! IBSEC = !",
      "",
      "Ending date/time",
      "! IEYR  = !",
      "! IEMO  = !",
      "! IEDY  = !",
      "! IEHR  = !",
      "! IESEC = !",
      "",
      "File Options",
      "! JDAT = 2 !",
      "! ISUB = 2 !",
      "! IFMT = 2 !",
      "",
      "Processing Options",
      "! PSTOP = 500 !",
      "! LXTOP = T   !",
      "! PVTOP = 850 !",
      "! LXSFC = T   !",
      "! ZVSFC = 200 !",
      "",
      "!END!",
      ""), "read62_template.txt", sep = "\n")
  
}
