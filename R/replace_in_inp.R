#' Replace keyword values in CALPUFF system input files
#' @description This function allows for the replacement of values in a CALPUFF system input file.
#' @param inp_file_working the string vector that represents the working input file.
#' @param keyword a string vector of input file keywords for the appropriate section.
#' @param replacement a vector of objects that represent formatted strings (for placement in the input file).
#' @export replace_in_inp

# Create function to replace parameter in input file
replace_in_inp <- function(inp_file_working,
                           keyword,
                           replacement){
  
  for (i in 1:length(keyword)){
    inp_file_working[grep(paste0(keyword[i], "(?![[:alpha:]])"),
                          inp_file_working, perl = TRUE)] <-
      gsub("=.*!", paste0("= ", replacement[i], " !"),
           inp_file_working[grep(paste0(keyword[i], "(?![[:alpha:]])"),
                                 inp_file_working, perl = TRUE)])
  }
  
  # Remove space characters in lines with no text characters
  inp_file_working <- gsub("^[ ]*$", "", inp_file_working)
  
  return(inp_file_working)
  
}
