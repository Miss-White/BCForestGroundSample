lookup_sp_type <- function(){
  lookuptable <- data.table(speciesCode = c(c("AC", "AT", "B", "C", "D", "E", "F", "H", "L", "MB", "PA", 
                                              "PL", "PW", "PY", "S", "Y")),
                            speciesType = c("D", "D", "C", "C", "D", "D", "C", "C", "C", "D", "C", "C", 
                                            "C", "C", "C", "C"))
  return(lookuptable)
}