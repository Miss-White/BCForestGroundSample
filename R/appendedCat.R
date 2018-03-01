#' Prints first text file and appends into second file
#' 
#' 
#' @description This function is a generic function to print the first text and appends into second
#'              file if it exists.
#'              
#'
#' @param firstText character, First text.
#' @param secondText character, Second text.
#'                            
#'                                                                     
#' @return Appended text file.
#' 
#' 
#' @export
#' @docType methods
#' @rdname appendedCat
#'
#' @author Yong Luo
#'
setGeneric("appendedCat",
           function(firstText, secondText){
             standardGeneric("appendedCat")
           })

#' @rdname appendedCat
setMethod(
  "appendedCat",
  signature = c(firstText = "character",
                secondText = "character"),
  definition = function(firstText, secondText){
    if(is.na(secondText)){
      cat(paste(firstText, "\n\n", sep = ""))
      output <- firstText
    } else {
      cat(paste(firstText, "\n\n", sep = ""))
      output <- paste(secondText, "\n\n", firstText, sep = "")
    }
    return(output)
  })

#' @rdname appendedCat
setMethod(
  "appendedCat",
  signature = c(firstText = "character",
                secondText = "missing"),
  definition = function(firstText){
    return(appendedCat(firstText, secondText = as.character(NA)))
  })