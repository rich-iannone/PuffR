#' Replace keyword values in CALPUFF system input files
#' @description This function allows for the replacement of values in a CALPUFF system input file.
#' @param calmet_inp_working the string vector that represents the working CALMET input file.
#' @param keyword a string vector of input file keywords for the appropriate section.
#' @param replacement a vector of objects that represent formatted strings (for placement in the input file).
#' @export replace_in_inp

# Create function to replace parameter in input file
replace_in_inp <- function(calmet_inp_working,
                           keyword,
                           replacement){
  
  for (i in 1:length(keyword)){
    
    calmet_inp_working[grep(keyword[i], calmet_inp_working)] <-
      gsub("=.*!", paste("= ", replacement[i], " !", sep = ''),
           calmet_inp_working[grep(keyword[i], calmet_inp_working)])
    
  }
  
  # Remove space characters in lines with no text characters
  calmet_inp_working <- gsub("^[ ]*$", "", calmet_inp_working)
  
  return(calmet_inp_working)
  
}