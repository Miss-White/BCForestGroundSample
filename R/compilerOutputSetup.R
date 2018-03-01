#' Setup an output path of the compiler
#' 
#' @description This function does two things: 1. create a folder that will store compiled data;
#'              2. return a path that directs the compiled folder.
#'
#' @param outputPath character, Specifies an output path.
#'                              If missing, the current work directory will be used.
#'
#' @return An output path that will be used to store your compiled data
#' 
#' @note Could overwrite the existing output folder, depending on user's choise, i.e., yes or no.
#'
#' @export
#' @docType methods
#' @rdname compilerOutputSetup
#'
#' @author Yong Luo
#'
setGeneric("compilerOutputSetup",
           function(outputPath) {
             standardGeneric("compilerOutputSetup")
           })

#' @rdname compilerOutputSetup
setMethod(
  "compilerOutputSetup",
  signature = c(outputPath = "character"),
  definition = function(outputPath){
    if(dir.exists(file.path(outputPath))){
      if(dir.exists(file.path(outputPath, "CompiledOutputs"))){
        YN <- readline("The output folder exists, do you intend to overwrite it? (Yes or No) : ")
        if(toupper(YN) == "YES"){
          unlink(file.path(outputPath, "CompiledOutputs"), recursive = TRUE)
          dir.create(file.path(outputPath, "CompiledOutputs"), recursive = TRUE)
        } else if (toupper(YN) == "NO") {
          stop("Your compilation is terminated by yourself.")
        } else {
          stop("Sorry, I can not regonize your input.")
        }
        rm(YN)
      } else {
        dir.create(file.path(outputPath, "CompiledOutputs"))
      }
    } else {
      dir.create(file.path(outputPath))
      dir.create(file.path(outputPath, "CompiledOutputs"))
    }
    return(file.path(outputPath, "CompiledOutputs"))
  })

#' @export
#' @rdname compilerOutputSetup
setMethod("compilerOutputSetup",
          signature = signature(outputPath = "missing"),
          definition = function(){
            return(compilerOutputSetup(outputPath = "."))
          })
