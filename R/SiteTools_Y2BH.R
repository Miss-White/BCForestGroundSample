#' Derive years to breast height using site tools
#' 
#' @description Derive years to breast height based on species (\code{species}), region (\code{ICRegion}) and site index (\code{siteIndex})
#'              using site tools. This function is equivalent to \code{sindex_httoage.sas}.
#'
#' @param species character, Species code, must be consistent with the species code in site tools, which can be converted 
#'                           from the original species code by using \code{\link{siteToolsSpeciesConvertor}}.
#' @param ICRegion character, Must be either \code{I} (interior) and \code{C} (coastal). 
#'                            IC regions can be derived using \code{\link{BEC2IC}} function.
#' @param siteIndex numeric, Site index. Defined as tree height at 50 years old.
#' @param siteToolsDLLPath character, Path to \code{SINDEX33.DLL}
#' @param sasExePath character, Path to sas executable, i.e., \code{sas.exe}. If missing, the function takes
#'                   \code{C:/Program Files/SASHome/x86/SASFoundation/9.3} as default. However, it will cause crush
#'                   if sas executable is not located in default path. 
#' 
#' @return Years to breast height
#' 
#' @importFrom data.table ':=' data.table
#'
#' 
#' @export
#' @docType methods
#' @rdname SiteTools_Y2BH
#'
#' @author Yong Luo
#'
setGeneric("SiteTools_Y2BH",
           function(species, ICRegion, siteIndex,
                    siteToolsDLLPath, sasExePath) {
             standardGeneric("SiteTools_Y2BH")
           })

#' @rdname SiteTools_Y2BH
setMethod(
  "SiteTools_Y2BH",
  signature = c(species = "character", 
                ICRegion = "character",
                siteIndex = "numeric",
                siteToolsDLLPath = "character",
                sasExePath = "character"),
  definition = function(species, ICRegion, siteIndex,
                        siteToolsDLLPath, sasExePath){
    worktable <- data.table(uniObs = 1:length(siteIndex), speciesFRED = species, 
                            BEC_I_C = ICRegion, SI_TREE = siteIndex)
    worktable[, SI_SP := ST_SpecRemap(species = speciesFRED, ICRegion = BEC_I_C,
                                      siteToolsDLLPath = siteToolsDLLPath,
                                      sasExePath = sasExePath)]
    worktable[SI_SP >= 0, SITE_CURVE := ST_DefCurve(siteIndexRef = SI_SP,
                                                    siteToolsDLLPath = siteToolsDLLPath,
                                                    sasExePath = sasExePath)]
    worktable[SI_SP >= 0, ':='(crtedAge = ST_YrsToBH(SICurveRef = SITE_CURVE, siteIndex = SI_TREE,
                                                   siteToolsDLLPath = siteToolsDLLPath,
                                                   sasExePath = sasExePath)$output,
                               YBHErr = ST_YrsToBH(SICurveRef = SITE_CURVE, siteIndex = SI_TREE,
                                                   siteToolsDLLPath = siteToolsDLLPath,
                                                   sasExePath = sasExePath)$error)]
    worktable[SI_SP >= 0 & YBHErr < 0, crtedAge := 0]
    return(worktable[order(uniObs)]$crtedAge)
  })

#' @export
#' @rdname SiteTools_Y2BH
setMethod(
  "SiteTools_Y2BH",
  signature = c(species = "character", 
                ICRegion = "character",
                siteIndex = "numeric",
                siteToolsDLLPath = "character",
                sasExePath = "missing"),
  definition = function(species, ICRegion, siteIndex,
                        siteToolsDLLPath){
    return(SiteTools_Y2BH(species, ICRegion, siteIndex, siteToolsDLLPath,
                          sasExePath = "C:/Program Files/SASHome/x86/SASFoundation/9.3"))
  })
