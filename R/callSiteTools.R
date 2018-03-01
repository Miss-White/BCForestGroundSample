#' Calculate site index reference by species and region
#' 
#' @description This function is to extract site index reference for a given species and region in siteTools program.
#'
#' @param species character, Species code, must be consistent with the species code in site tools.
#' @param ICRegion character, Must be either \code{I} (interior) and \code{C} (coastal). In VRI compiler,
#'                 IC regions are derived using \code{\link{BEC2IC}} function.
#' @param siteToolsDLLPath character, Path to \code{SINDEX33.DLL}
#' @param sasExePath character, Path to sas executable, i.e., \code{sas.exe}. If missing, the function takes
#'                   \code{C:/Program Files/SASHome/x86/SASFoundation/9.3} as default. However, it will cause crush
#'                   if sas executable is not located in default path.
#'  
#' @return Site index reference that can be recognized by sitetools
#' 
#' @importFrom data.table ':=' data.table
#' @importFrom foreign write.dbf
#'
#' @seealso \code{\link{ST_DefCurve}}, \code{\link{ST_DefGICurve}}, \code{\link{ST_HTAgeToSI}} and \code{\link{ST_YrsToBH}}
#' @export
#' @docType methods
#' @rdname ST_SpecRemap
#'
#' @author Yong Luo
#'
setGeneric("ST_SpecRemap",
           function(species, ICRegion, 
                    siteToolsDLLPath, sasExePath) {
             standardGeneric("ST_SpecRemap")
           })

#' @rdname ST_SpecRemap
setMethod(
  "ST_SpecRemap",
  signature = c(species = "character", 
                ICRegion = "character",
                siteToolsDLLPath = "character",
                sasExePath = "character"),
  definition = function(species, ICRegion, 
                        siteToolsDLLPath, sasExePath){
    tempD <- file.path(tempdir(), "sasrun")
    dir.create(tempD)
    #1. create a data frame and save it to temp dir
    workdata <- data.frame(uniObs = 1:length(species),
                           sp_ST = species,
                           bec_i_c = ICRegion)
    foreign::write.dbf(workdata, file.path(tempD, "tempdata.dbf"))
    #2. generate sas file and sas config file and sascbtbl file save them to temp dir
    sascbtblGenerator(savePath = tempD, moduleName = "Sindex_SpecRemap")
    sasFileGenerator(savePath = tempD, sascbtblPath = tempD,
                     moduleName = "Sindex_SpecRemap")
    sasconfigGenerator(savePath = tempD, 
                       siteToolsDLLPath = siteToolsDLLPath,
                       sasExePath = sasExePath)
    
    #3. call sas file from R 
    orgDir <- getwd()
    setwd(sasExePath)
    shell(paste("sas.exe -batch -log ", file.path(tempD, "mylog.log"), 
                " -CONFIG ", file.path(tempD, "sasconfig.cfg"),
                " -SYSIN ",file.path(tempD, "Sindex_SpecRemap.sas"), sep = ""))
    #4. load output into R
    output <- read_sas(file.path(tempD, "tempdatacpted.sas7bdat")) %>% data.table
    setwd(orgDir)
    unlink(tempD, recursive = TRUE)
    return(output[order(uniObs),]$si_sp)
  })

#' @export
#' @rdname ST_SpecRemap
setMethod(
  "ST_SpecRemap",
  signature = c(species = "character", 
                ICRegion = "character",
                siteToolsDLLPath = "character",
                sasExePath = "missing"),
  definition = function(species, ICRegion, 
                        siteToolsDLLPath){
    return(ST_SpecRemap(species, ICRegion, siteToolsDLLPath, 
                        sasExePath = "C:/Program Files/SASHome/x86/SASFoundation/9.3"))
  })

# siteToolsDLLPath <- "C:/SiteToolsdll"
# savePath <- "F:/sitetools"
# sasExePath <- "C:/Program Files/SASHome/x86/SASFoundation/9.3"
# 
# sasFileGenerator(savePath, sascbtblPath, moduleName = "Sindex_DefGICurve")



#' Assign site index curve reference in site tools
#' 
#' @description Assign site index curve reference that can be recognized by site tools for a given site index reference.
#'              The site index can be generated using \code{\link{ST_SpecRemap}} function.
#'
#' @param siteIndexRef numeric, Site index reference. It can be derived using \code{\link{ST_SpecRemap}} function.
#' @param siteToolsDLLPath character, Path to \code{SINDEX33.DLL}
#' @param sasExePath character, Path to sas executable, i.e., \code{sas.exe}. If missing, the function takes
#'                   \code{C:/Program Files/SASHome/x86/SASFoundation/9.3} as default. However, it will cause crush
#'                   if sas executable does not located in default folder.
#' 
#' @return Site index curve reference that can be recognized by site tools
#' 
#' @importFrom data.table ':=' data.table
#' @importFrom foreign write.dbf
#' 
#' @seealso \code{\link{ST_SpecRemap}}, \code{\link{ST_DefGICurve}}, \code{\link{ST_HTAgeToSI}} and \code{\link{ST_YrsToBH}}
#' @export
#' @docType methods
#' @rdname ST_DefCurve
#'
#' @author Yong Luo
#'
setGeneric("ST_DefCurve",
           function(siteIndexRef, siteToolsDLLPath, sasExePath) {
             standardGeneric("ST_DefCurve")
           })

#' @rdname ST_DefCurve
setMethod(
  "ST_DefCurve",
  signature = c(siteIndexRef = "numeric",
                siteToolsDLLPath = "character",
                sasExePath = "character"),
  definition = function(siteIndexRef, siteToolsDLLPath, sasExePath){
    tempD <- file.path(tempdir(), "sasrun")
    dir.create(tempD)
    #1. create a data frame and save it to temp dir
    workdata <- data.frame(uniObs = 1:length(siteIndexRef),
                           si_sp = siteIndexRef)
    foreign::write.dbf(workdata, file.path(tempD, "tempdata.dbf"))
    #2. generate sas file and sas config file and sascbtbl file save them to temp dir
    sascbtblGenerator(savePath = tempD, moduleName = "Sindex_DefCurve")
    sasFileGenerator(savePath = tempD, sascbtblPath = tempD,
                     moduleName = "Sindex_DefCurve")
    sasconfigGenerator(savePath = tempD, 
                       siteToolsDLLPath = siteToolsDLLPath,
                       sasExePath = sasExePath)
    
    #3. call sas file from R 
    orgDir <- getwd()
    setwd(sasExePath)
    shell(paste("sas.exe -batch -log ", file.path(tempD, "mylog.log"), 
                " -CONFIG ", file.path(tempD, "sasconfig.cfg"),
                " -SYSIN ",file.path(tempD, "Sindex_DefCurve.sas"), sep = ""))
    #4. load output into R
    workdata <- read_sas(file.path(tempD, "tempdatacpted.sas7bdat")) %>% data.table
    setwd(orgDir)
    unlink(tempD, recursive = TRUE)
    return(workdata[order(uniObs),]$curveOutput)
  })

#' @export
#' @rdname ST_DefCurve
setMethod(
  "ST_DefCurve",
  signature = c(siteIndexRef = "numeric",
                siteToolsDLLPath = "character",
                sasExePath = "missing"),
  definition = function(siteIndexRef, siteToolsDLLPath){
    return(ST_DefCurve(siteIndexRef, siteToolsDLLPath, 
                       sasExePath = "C:/Program Files/SASHome/x86/SASFoundation/9.3"))
  })


#' Assign growth intercept curve reference by site index reference in sitetools
#' 
#' @description Assign growth intercept curve reference that can be recognized by site tools for a given site index reference.
#'              
#'
#' @param siteIndexRef numeric, Site index reference. It can be derived using \code{\link{ST_SpecRemap}} function.
#' @param siteToolsDLLPath character, Path to \code{SINDEX33.DLL}
#' @param sasExePath character, Path to sas executable, i.e., \code{sas.exe}. If missing, the function takes
#'                   \code{C:/Program Files/SASHome/x86/SASFoundation/9.3} as default. However, it will cause crush
#'                   if sas executable does not located in default folder.
#'  
#' 
#' 
#' @return Growth intercept curve reference that can be recognized by sitetools
#' 
#' @importFrom data.table ':=' data.table
#' @importFrom foreign write.dbf
#' 
#' @seealso \code{\link{ST_SpecRemap}}, \code{\link{ST_DefCurve}}, \code{\link{ST_HTAgeToSI}} and \code{\link{ST_YrsToBH}}
#' @export
#' @docType methods
#' @rdname ST_DefGICurve
#'
#' @author Yong Luo
#'
setGeneric("ST_DefGICurve",
           function(siteIndexRef, siteToolsDLLPath, sasExePath) {
             standardGeneric("ST_DefGICurve")
           })

#' @rdname ST_DefGICurve
setMethod(
  "ST_DefGICurve",
  signature = c(siteIndexRef = "numeric", 
                siteToolsDLLPath = "character",
                sasExePath = "character"),
  definition = function(siteIndexRef, siteToolsDLLPath, sasExePath){
    tempD <- file.path(tempdir(), "sasrun")
    dir.create(tempD)
    #1. create a data frame and save it to temp dir
    workdata <- data.frame(uniObs = 1:length(siteIndexRef),
                           si_sp = siteIndexRef)
    foreign::write.dbf(workdata, file.path(tempD, "tempdata.dbf"))
    #2. generate sas file and sas config file and sascbtbl file save them to temp dir
    sascbtblGenerator(savePath = tempD, moduleName = "Sindex_DefGICurve")
    sasFileGenerator(savePath = tempD, sascbtblPath = tempD,
                     moduleName = "Sindex_DefGICurve")
    sasconfigGenerator(savePath = tempD, 
                       siteToolsDLLPath = siteToolsDLLPath,
                       sasExePath = sasExePath)
    
    #3. call sas file from R 
    orgDir <- getwd()
    setwd(sasExePath)
    shell(paste("sas.exe -batch -log ", file.path(tempD, "mylog.log"), 
                " -CONFIG ", file.path(tempD, "sasconfig.cfg"),
                " -SYSIN ",file.path(tempD, "Sindex_DefGICurve.sas"), sep = ""))
    #4. load output into R
    workdata <- read_sas(file.path(tempD, "tempdatacpted.sas7bdat")) %>% data.table
    setwd(orgDir)
    unlink(tempD, recursive = TRUE)
    return(workdata[order(uniObs),]$curveOutput)
  })

#' @rdname ST_DefGICurve
setMethod(
  "ST_DefGICurve",
  signature = c(siteIndexRef = "numeric", 
                siteToolsDLLPath = "character",
                sasExePath = "missing"),
  definition = function(siteIndexRef, siteToolsDLLPath){
    return(ST_DefGICurve(siteIndexRef, siteToolsDLLPath,
                         sasExePath = "C:/Program Files/SASHome/x86/SASFoundation/9.3"))
  })


#' Calculate site index in site tools
#' 
#' @description Calculate site index based on bored age (\code{boredAge}), height (\code{height}) and curve reference (\code{curveRef})
#'              in site tools. The site index is defined as height at 50 years old. Curve reference can be either site index curve 
#'              reference or growth intercept curve reference. Site
#'              index curve reference can be derived using \code{ST_DefCurve} function, while growth intercept curve reference can be derived
#'              using \code{ST_DefGICurve} function.
#'
#' @param curveRef numeric, Either site index curve reference or growth intercept curve reference 
#' @param boredAge numeric, Tree age at the bored height
#' @param ageType numeric, Must be either \code{0} or \code{1}. \code{0} stands for total age, for which site index is 
#'                         calculated for 50 years of total tree age. While \code{1} stands for breast height age, for which 
#'                         site index is calculated for 50 year old at breast height.
#' @param height numeric, Tree height
#' @param estimateMethod numeric, Defines how the site tools estimate site index. Valued as \code{0} and \code{1}, 
#'                                \code{0} is interative and while \code{1} is directive. Default is \code{1}, which is directive.
#' @param siteToolsDLLPath character, Path to \code{SINDEX33.DLL}
#' @param sasExePath character, Path to sas executable, i.e., \code{sas.exe}. If missing, the function takes
#'                   \code{C:/Program Files/SASHome/x86/SASFoundation/9.3} as default. However, it will cause crush
#'                   if sas executable does not located in default folder.
#'  
#' 
#' @return A list of \code{output} and \code{error}. \code{output} is the site index. 
#'         \code{error} the flag in calculation, with a negative value represents failure.
#' 
#' @importFrom data.table ':=' data.table
#' @importFrom foreign write.dbf
#' @seealso \code{\link{ST_SpecRemap}}, \code{\link{ST_DefCurve}}, \code{\link{ST_DefGICurve}} and \code{\link{ST_YrsToBH}}
#' @export
#' @docType methods
#' @rdname ST_HTAgeToSI
#'
#' @author Yong Luo
#'
setGeneric("ST_HTAgeToSI",
           function(curveRef, boredAge, ageType, height, estimateMethod,
                    siteToolsDLLPath, sasExePath) {
             standardGeneric("ST_HTAgeToSI")
           })

#' @rdname ST_HTAgeToSI
setMethod(
  "ST_HTAgeToSI",
  signature = c(curveRef = "numeric",
                boredAge = "numeric",
                ageType = "numeric",
                height = "numeric",
                estimateMethod = "numeric",
                siteToolsDLLPath = "character",
                sasExePath = "character"),
  definition = function(curveRef, boredAge, ageType, height, 
                        estimateMethod, siteToolsDLLPath, sasExePath){
    tempD <- file.path(tempdir(), "sasrun")
    dir.create(tempD)
    #1. create a data frame and save it to temp dir
    workdata <- data.frame(uniObs = 1:length(curveRef),
                           site_curve = curveRef,
                           age = boredAge, ageType,
                           height, estimateM = estimateMethod, si_tree = 0)
    foreign::write.dbf(workdata, file.path(tempD, "tempdata.dbf"))
    #2. generate sas file and sas config file and sascbtbl file save them to temp dir
    sascbtblGenerator(savePath = tempD, moduleName = "Sindex_HtAgeToSI")
    sasFileGenerator(savePath = tempD, sascbtblPath = tempD,
                     moduleName = "Sindex_HtAgeToSI")
    sasconfigGenerator(savePath = tempD, 
                       siteToolsDLLPath = siteToolsDLLPath,
                       sasExePath = sasExePath)
    
    #3. call sas file from R 
    orgDir <- getwd()
    setwd(sasExePath)
    shell(paste("sas.exe -batch -log ", file.path(tempD, "mylog.log"), 
                " -CONFIG ", file.path(tempD, "sasconfig.cfg"),
                " -SYSIN ",file.path(tempD, "Sindex_HtAgeToSI.sas"), sep = ""))
    #4. load output into R
    workdata <- read_sas(file.path(tempD, "tempdatacpted.sas7bdat")) %>% data.table
    setwd(orgDir)
    unlink(tempD, recursive = TRUE)
    return(list(output = workdata[order(uniObs),]$si_tree,
                error = workdata[order(uniObs),]$sierr))
  })


#' @export
#' @rdname ST_HTAgeToSI
setMethod(
  "ST_HTAgeToSI",
  signature = c(curveRef = "numeric",
                boredAge = "numeric",
                ageType = "numeric",
                height = "numeric",
                estimateMethod = "missing",
                siteToolsDLLPath = "character",
                sasExePath = "missing"),
  definition = function(curveRef, boredAge, ageType, height, 
                        siteToolsDLLPath){
    return(ST_HTAgeToSI(curveRef, boredAge, ageType, height, estimateMethod = 1,
                        siteToolsDLLPath, 
                        sasExePath = "C:/Program Files/SASHome/x86/SASFoundation/9.3"))
  })

#' @export
#' @rdname ST_HTAgeToSI
setMethod(
  "ST_HTAgeToSI",
  signature = c(curveRef = "numeric",
                boredAge = "numeric",
                ageType = "numeric",
                height = "numeric",
                estimateMethod = "numeric",
                siteToolsDLLPath = "character",
                sasExePath = "missing"),
  definition = function(curveRef, boredAge, ageType, height, 
                        estimateMethod, siteToolsDLLPath){
    return(ST_HTAgeToSI(curveRef, boredAge, ageType, height, estimateMethod,
                        siteToolsDLLPath, 
                        sasExePath = "C:/Program Files/SASHome/x86/SASFoundation/9.3"))
  })


#' Derive years between ground and breast height in site tools
#' 
#' @description Derive years between ground and breast height based on site index curve reference (\code{SICurveRef}) and 
#'              site index (\code{siteIndex}) in site tools.
#'
#' @param SICurveRef numeric, Site index curve reference in site tools. It can be derived using \code{ST_DefCurve} function.
#' @param siteIndex numeric, Site index. Defined as tree height at 50 years old.
#' @param siteToolsDLLPath character, Path to \code{SINDEX33.DLL}
#' @param sasExePath character, Path to sas executable, i.e., \code{sas.exe}. If missing, the function takes
#'                   \code{C:/Program Files/SASHome/x86/SASFoundation/9.3} as default. However, it will cause crush
#'                   if sas executable does not located in default folder.
#'  
#' 
#' 
#' @return A list of \code{output} and \code{error}. \code{output} is the derived years to breast height. 
#'        \code{error} is the flag in calculation, with negative value represents failure.
#' 
#' @importFrom data.table ':=' data.table
#' @importFrom foreign write.dbf
#' @seealso \code{\link{ST_SpecRemap}}, \code{\link{ST_DefCurve}}, \code{\link{ST_DefGICurve}} and \code{\link{ST_HTAgeToSI}}
#' @export
#' @docType methods
#' @rdname ST_YrsToBH
#'
#' @author Yong Luo
#'
setGeneric("ST_YrsToBH",
           function(SICurveRef, siteIndex, siteToolsDLLPath, sasExePath) {
             standardGeneric("ST_YrsToBH")
           })

#' @rdname ST_YrsToBH
setMethod(
  "ST_YrsToBH",
  signature = c(SICurveRef = "numeric",
                siteIndex = "numeric",
                siteToolsDLLPath = "character",
                sasExePath = "character"),
  definition = function(SICurveRef, siteIndex, siteToolsDLLPath, sasExePath){
    tempD <- file.path(tempdir(), "sasrun")
    dir.create(tempD)
    #1. create a data frame and save it to temp dir
    workdata <- data.frame(uniObs = 1:length(SICurveRef),
                           site_curve = SICurveRef,
                           si_tree = siteIndex,
                           corr = 0)
    foreign::write.dbf(workdata, file.path(tempD, "tempdata.dbf"))
    #2. generate sas file and sas config file and sascbtbl file save them to temp dir
    sascbtblGenerator(savePath = tempD, moduleName = "Sindex_Y2BH")
    sasFileGenerator(savePath = tempD, sascbtblPath = tempD,
                     moduleName = "Sindex_Y2BH")
    sasconfigGenerator(savePath = tempD, 
                       siteToolsDLLPath = siteToolsDLLPath,
                       sasExePath = sasExePath)
    
    #3. call sas file from R 
    orgDir <- getwd()
    setwd(sasExePath)
    shell(paste("sas.exe -batch -log ", file.path(tempD, "mylog.log"), 
                " -CONFIG ", file.path(tempD, "sasconfig.cfg"),
                " -SYSIN ",file.path(tempD, "Sindex_Y2BH.sas"), sep = ""))
    #4. load output into R
    workdata <- read_sas(file.path(tempD, "tempdatacpted.sas7bdat")) %>% data.table
    setwd(orgDir)
    unlink(tempD, recursive = TRUE)
    return(list(output = workdata[order(uniObs),]$corr,
                error = workdata[order(uniObs), ]$ybherr))
  })

#' @export
#' @rdname ST_YrsToBH
setMethod(
  "ST_YrsToBH",
  signature = c(SICurveRef = "numeric",
                siteIndex = "numeric",
                siteToolsDLLPath = "character",
                sasExePath = "missing"),
  definition = function(SICurveRef, siteIndex, siteToolsDLLPath){
    return(ST_YrsToBH(SICurveRef, siteIndex, siteToolsDLLPath, 
                      sasExePath = "C:/Program Files/SASHome/x86/SASFoundation/9.3"))
  })



################################################################################################
################################################################################################
#' Derive site index curve name based on site index curve reference
#' 
#' @description Derive site index curve name based on site index curve reference (\code{SICurveRef}) in site tools.
#'
#' @param SICurveRef numeric, Site index curve reference in site tools. It can be derived using \code{ST_DefCurve} function.
#' @param siteToolsDLLPath character, Path to \code{SINDEX33.DLL}
#' @param sasExePath character, Path to sas executable, i.e., \code{sas.exe}. If missing, the function takes
#'                   \code{C:/Program Files/SASHome/x86/SASFoundation/9.3} as default. However, it will cause crush
#'                   if sas executable does not located in default folder.

#'  
#' 
#' 
#' @return site index curve name
#' 
#' @importFrom data.table ':=' data.table
#'
#' 
#' @export
#' @docType methods
#' @rdname ST_CurveName
#'
#' @author Yong Luo
#'
setGeneric("ST_CurveName",
           function(SICurveRef, siteToolsDLLPath, sasExePath) {
             standardGeneric("ST_CurveName")
           })

#' @rdname ST_CurveName
setMethod(
  "ST_CurveName",
  signature = c(SICurveRef = "numeric",
                siteToolsDLLPath = "character",
                sasExePath = "character"),
  definition = function(SICurveRef, siteToolsDLLPath, sasExePath){
    tempD <- file.path(tempdir(), "sasrun")
    dir.create(tempD)
    #1. create a data frame and save it to temp dir
    workdata <- data.frame(uniObs = 1:length(SICurveRef),
                           site_curve = SICurveRef)
    foreign::write.dbf(workdata, file.path(tempD, "tempdata.dbf"))
    #2. generate sas file and sas config file and sascbtbl file save them to temp dir
    sascbtblGenerator(savePath = tempD, moduleName = "Sindex_CurveName")
    sasFileGenerator(savePath = tempD, sascbtblPath = tempD,
                     moduleName = "Sindex_CurveName")
    sasconfigGenerator(savePath = tempD, 
                       siteToolsDLLPath = siteToolsDLLPath,
                       sasExePath = sasExePath)
    
    #3. call sas file from R 
    orgDir <- getwd()
    setwd(sasExePath)
    shell(paste("sas.exe -batch -log ", file.path(tempD, "mylog.log"), 
                " -CONFIG ", file.path(tempD, "sasconfig.cfg"),
                " -SYSIN ",file.path(tempD, "Sindex_CurveName.sas"), sep = ""))
    
    #4. load output into R
    workdata <- read_sas(file.path(tempD, "tempdatacpted.sas7bdat")) %>% data.table
    setwd(orgDir)
    unlink(tempD, recursive = TRUE)
    return(workdata$curveID)
  })

#' @export
#' @rdname ST_CurveName
setMethod(
  "ST_CurveName",
  signature = c(SICurveRef = "numeric",
                siteToolsDLLPath = "character",
                sasExePath = "missing"),
  definition = function(SICurveRef, siteToolsDLLPath){
    return(ST_CurveName(SICurveRef, siteToolsDLLPath,
                        sasExePath = "C:/Program Files/SASHome/x86/SASFoundation/9.3"))
  })



sasFileGenerator <- function(savePath, sascbtblPath, moduleName){
  cat("FILENAME SASCBTBL (\"", gsub("/", "\\\\", file.path(sascbtblPath, "sascbtbl.sas")), "\");
libname thisistl \'", savePath, "\';
run;
proc import datafile=\"", file.path(savePath, "tempdata.dbf"), "\"
out=tempdata dbms=dbf;
run;
data si22;
set tempdata;",
      sep = "", file = file.path(savePath, paste(moduleName, ".sas", sep = "")))
  if(moduleName == "Sindex_SpecRemap"){
  cat("\nsi_sp = 0;
si_sp =  MODULEN(\'", moduleName, "\', sp_ST, bec_i_c );",
      sep = "", file = file.path(savePath, paste(moduleName, ".sas", sep = "")),
      append = TRUE)
         
  } else if (moduleName %in% c("Sindex_DefCurve", "Sindex_DefGICurve")){
    cat("\ncurveOutput = 0;
curveOutput =  MODULEN(\'", moduleName, "\', si_sp);",
        sep = "", file = file.path(savePath, paste(moduleName, ".sas", sep = "")),
        append = TRUE)
  } else if(moduleName == "Sindex_HtAgeToSI"){
    cat("\nsierr = 0;
sierr =  MODULEN(\'", moduleName, "\', site_curve , age , agetype, height, estimateM, si_tree);",
        sep = "", file = file.path(savePath, paste(moduleName, ".sas", sep = "")),
        append = TRUE)
  } else if(moduleName == "Sindex_Y2BH"){
    cat("\nybherr = 0;
ybherr =  MODULEN(\'", moduleName, "\', site_curve , si_tree, corr);",
        sep = "", file = file.path(savePath, paste(moduleName, ".sas", sep = "")),
        append = TRUE)
  } else if(moduleName == "Sindex_CurveName"){
    cat("\nformat curveID $30.;
curveID =  MODULEC(\'", moduleName, "\', site_curve);",
        sep = "", file = file.path(savePath, paste(moduleName, ".sas", sep = "")),
        append = TRUE)
  }
  cat("\nrun;
data thisistl.tempdataCpted;
set si22;
run;
libname thisistl clear;
run;", sep = "", file = file.path(savePath, paste(moduleName, ".sas", sep = "")),
      append = TRUE)
  
}


sasconfigGenerator <- function(savePath, siteToolsDLLPath,
                               sasExePath = "C:/Program Files/SASHome/x86/SASFoundation/9.3"){
  cat("-PATH \"", siteToolsDLLPath, "\"
-SET SASROOT \"", sasExePath, "\"
-RESOURCESLOC (
        \"!SASROOT\\nls\\en\\resource\"
        \"!SASROOT\\core\\resource\"
         )
-SASHELP (
      \"!SASCFG\\SASCFG\"
      \"!SASROOT\\core\\sashelp\"
      \"!SASROOT\\connect\\sashelp\"
      \"!SASROOT\\ets\\sashelp\"
      \"!SASROOT\\graph\\sashelp\"
      \"!SASROOT\\iml\\sashelp\"
      \"!SASROOT\\or\\sashelp\"
      \"!SASROOT\\qc\\sashelp\"
      \"!SASROOT\\stat\\sashelp\"
      )
-MSG (
      \"!SASROOT\\core\\sasmsg\"
      \"!SASROOT\\accelmva\\sasmsg\"
      \"!SASROOT\\access\\sasmsg\"
      \"!SASROOT\\dmscore\\sasmsg\"
      \"!SASROOT\\ets\\sasmsg\"
      \"!SASROOT\\etscomp\\sasmsg\"
      \"!SASROOT\\graph\\sasmsg\"
      \"!SASROOT\\iml\\sasmsg\"
      \"!SASROOT\\or\\sasmsg\"
      \"!SASROOT\\qc\\sasmsg\"
      \"!SASROOT\\spdsclient\\sasmsg\"
      \"!SASROOT\\stat\\sasmsg\"
      )
-PATH (
      \"!SASROOT\\core\\sasexe\"
      \"!SASROOT\\aacomp\\sasexe\"
      \"!SASROOT\\accelmva\\sasexe\"
      \"!SASROOT\\access\\sasexe\"
      \"!SASROOT\\connect\\sasexe\"
      \"!SASROOT\\dmscore\\sasexe\"
      \"!SASROOT\\ets\\sasexe\"
      \"!SASROOT\\etscomp\\sasexe\"
      \"!SASROOT\\graph\\sasexe\"
      \"!SASROOT\\iml\\sasexe\"
      \"!SASROOT\\or\\sasexe\"
      \"!SASROOT\\qc\\sasexe\"
      \"!SASROOT\\share\\sasexe\"
      \"!SASROOT\\spdsclient\\sasexe\"
      \"!SASROOT\\stat\\sasexe\"
      \"!SASROOT\\statcomp\\sasexe\"
      )
-SASUSER \"?CSIDL_PERSONAL\\My SAS Files(32)\\9.3\"
-WORK \"!TEMP\\SAS Temporary Files\"", 
      file = file.path(savePath, "sasconfig.cfg"), sep = "")
}


sascbtblGenerator <- function(moduleName, savePath){
  if(moduleName == "Sindex_SpecRemap"){
  cat("ROUTINE Sindex_SpecRemap
      MINARG = 2
      MAXARG = 2
      STACKPOP = CALLED
      MODULE = SINDEX33
      RETURNS = SHORT;
      ARG 1 CHAR BYADDR FORMAT = $CSTR4.;
      ARG 2 CHAR BYVALUE FORMAT = $BYVAL4.;",
      file = file.path(savePath, "sascbtbl.sas"))
  } else if(moduleName == "Sindex_DefCurve"){
    cat("ROUTINE Sindex_DefCurve
      MINARG = 1
      MAXARG = 1
      STACKPOP = CALLED
      MODULE = SINDEX33
      RETURNS = SHORT;
      ARG 1 NUM BYVALUE FORMAT = IB4.;",
        file = file.path(savePath, "sascbtbl.sas"))
  } else if(moduleName == "Sindex_DefGICurve"){
    cat("ROUTINE Sindex_DefGICurve
        MINARG = 1
        MAXARG = 1
        STACKPOP = CALLED
        MODULE = SINDEX33
        RETURNS = SHORT;
        ARG 1 NUM BYVALUE FORMAT = IB4.;",
        file = file.path(savePath, "sascbtbl.sas"))
  } else if(moduleName == "Sindex_HtAgeToSI"){
    cat("ROUTINE Sindex_HtAgeToSI
        MINARG = 6
        MAXARG = 6
        STACKPOP = CALLED
        MODULE = SINDEX33
        RETURNS = SHORT;
        ARG 1 NUM BYVALUE FORMAT = IB4.;
        ARG 2 NUM BYVALUE FORMAT = RB8.;
        ARG 3 NUM BYVALUE FORMAT = IB4.;
        ARG 4 NUM BYVALUE FORMAT = RB8.;
        ARG 5 NUM BYVALUE FORMAT = IB4.;
        ARG 6 NUM UPDATE BYADDR FORMAT = RB8.;",
        file = file.path(savePath, "sascbtbl.sas"))
  } else if(moduleName == "Sindex_Y2BH"){
    cat("ROUTINE Sindex_Y2BH
        MINARG = 3
        MAXARG = 3
        STACKPOP = CALLED
        MODULE = SINDEX33
        RETURNS = SHORT;
        ARG 1 NUM BYVALUE FORMAT = IB4.;
        ARG 2 NUM BYVALUE FORMAT = RB8.;
        ARG 3 NUM UPDATE BYADDR FORMAT = RB8.;",
        file = file.path(savePath, "sascbtbl.sas"))
  } else if(moduleName == "Sindex_AgeSIToHt"){
    cat("ROUTINE Sindex_AgeSIToHt
        MINARG = 6
        MAXARG = 6
        STACKPOP = CALLED
        MODULE = SINDEX33
        RETURNS = SHORT;
        ARG 1 NUM BYVALUE FORMAT = IB4.;
        ARG 2 NUM BYVALUE FORMAT = RB8.;
        ARG 3 NUM BYVALUE FORMAT = IB4.;
        ARG 4 NUM BYVALUE FORMAT = RB8.;
        ARG 5 NUM BYVALUE FORMAT = RB8.;
        ARG 6 NUM UPDATE BYADDR FORMAT = RB8.;",
        file = file.path(savePath, "sascbtbl.sas"))
  } else if(moduleName == "Sindex_CurveName"){
    cat("ROUTINE Sindex_CurveName
        MINARG = 1
        MAXARG = 1
        STACKPOP = CALLED
        MODULE = SINDEX33
        RETURNS = CHAR40;
        ARG 1 NUM BYVALUE FORMAT = IB4.;",
        file = file.path(savePath, "sascbtbl.sas"))
  }
}
