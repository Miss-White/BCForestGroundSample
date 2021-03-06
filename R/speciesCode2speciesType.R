#' Group species into deciduous and coniferous species group 
#' 
#' 
#' @description This function is to group species into deciduous and 
#'              coniferous species types based on BC species code and \code{sp_type} lookup table.
#'              This function uses hardcoded \code{sp_type} lookup table.
#' @param species character, Tree basic species code, which is SP0 in VRI original data.                             
#' 
#'                   
#' @return Species type: \code{D}-deciduous species and \code{C}-coniferous species. 
#'         \code{NA}, with a warning message, is given if a species fails to be grouped.
#' 
#' @importFrom data.table ':='
#' 
#' @export
#' @docType methods
#' @rdname speciesCode2speciesType
#'
#' @author Yong Luo
#'
setGeneric("speciesCode2speciesType",
           function(species) {
             standardGeneric("speciesCode2speciesType")
           })

#' @rdname speciesCode2speciesType
setMethod(
  "speciesCode2speciesType",
  signature = c(species = "character"),
  definition = function(species){
    ## hardcoded lookup table
    tempdata <- data.table(uniObs = 1:length(species),
                           speciesCode = species)
    sp_type <- lookup_sp_type()
    tempdata <- merge(tempdata, sp_type, by = "speciesCode", all.x = TRUE)
    if(nrow(tempdata[is.na(speciesType)]) > 0){
      warning("Species code can not find a species type, NA is produced.")
    }
    return(tempdata[order(uniObs)]$speciesType)
  })

