#' Calculate site index using site tools
#' 
#' @description This function calculates site index based on bored age (\code{boredAge}), tree height (\code{height}),
#'              species (\code{species}) and region (\code{ICRegion}) using site tools program. This function
#'              is equivalent to sindex_httoage.sas.
#'              
#' @param boredAge numeric, Age at bored height.
#' @param height numeric, Total tree height.
#' @param species character, Species code, must be consistent with the species code in site tools, which can be converted 
#'                           from the original species code by using \code{\link{siteToolsSpeciesConvertor}}.
#' @param ICRegion character, Must be either \code{I} (interior) and \code{C} (coastal). 
#'                            IC regions can be derived using \code{\link{BEC2IC}}.
#' @param ageType numeric, Must be either \code{0} or \code{1}. \code{0} stands for total age, for which site index is 
#'                         calculated for 50 years of total tree age. While \code{1} stands for breast height age, for which 
#'                         site index is calculated for 50 year old at breast height.
#' @param estimateMethod numeric, Defines how the site tools estimate site index. Valued as \code{0} and \code{1}, 
#'                                \code{0} is interative and while \code{1} is directive. Default is \code{1}, which is directive.
#' @param siteToolsDLLPath character, Path to \code{SINDEX33.DLL}
#' @param sasExePath character, Path to sas executable, i.e., \code{sas.exe}.
#'                   
#' 
#' @return Site index 
#' 
#' @importFrom data.table ':=' data.table
#'
#' 
#' @export
#' @docType methods
#' @rdname SiteTools_HTBoredAge2SI
#'
#' @author Yong Luo
#'
setGeneric("SiteTools_HTBoredAge2SI",
           function(boredAge, height, species, ICRegion, ageType, 
                    estimateMethod, siteToolsDLLPath, sasExePath) {
             standardGeneric("SiteTools_HTBoredAge2SI")
           })

#' @rdname SiteTools_HTBoredAge2SI
setMethod(
  "SiteTools_HTBoredAge2SI",
  signature = c(boredAge = "numeric", 
                height = "numeric", 
                species = "character", 
                ICRegion = "character",
                ageType = "numeric",
                estimateMethod = "numeric",
                siteToolsDLLPath = "character",
                sasExePath = "character"),
  definition = function(boredAge, height, species, ICRegion, ageType, 
                        estimateMethod, siteToolsDLLPath, sasExePath){
    worktable <- data.table(uniObs = 1:max(length(boredAge), length(height)),
                            age = boredAge, height, ageType, 
                            speciesFRED = species, BEC_I_C = ICRegion)
    worktable[, SI_SP := ST_SpecRemap(species = speciesFRED, ICRegion = BEC_I_C,
                                      siteToolsDLLPath = siteToolsDLLPath,
                                      sasExePath = sasExePath)]
    worktable[SI_SP >= 0, ':='(SITE_CURVE = ST_DefCurve(siteIndexRef = SI_SP,
                                                        siteToolsDLLPath = siteToolsDLLPath,
                                                        sasExePath = sasExePath),
                               GRTH_CURVE = ST_DefGICurve(siteIndexRef = SI_SP,
                                                          siteToolsDLLPath = siteToolsDLLPath,
                                                          sasExePath = sasExePath))]
    worktable[SI_SP >= 0, ':='(SI_ERR = ST_HTAgeToSI(curveRef = SITE_CURVE,
                                                     boredAge = age,
                                                     ageType = ageType,
                                                     height = height,
                                                     estimateMethod = estimateMethod,
                                                     siteToolsDLLPath = siteToolsDLLPath,
                                                     sasExePath = sasExePath)$error,
                               SI_TREE = ST_HTAgeToSI(curveRef = SITE_CURVE,
                                                      boredAge = age,
                                                      ageType = ageType,
                                                      height = height,
                                                      estimateMethod = estimateMethod,
                                                      siteToolsDLLPath = siteToolsDLLPath,
                                                      sasExePath = sasExePath)$output)]
    worktable[SI_SP >= 0 & SI_ERR < 0, SI_TREE := as.numeric(NA)]
    worktable[SI_SP >= 0 & age <= 50, ':='(GI_ERR = ST_HTAgeToSI(curveRef = GRTH_CURVE,
                                                                 boredAge = age,
                                                                 ageType = ageType,
                                                                 height = height,
                                                                 estimateMethod = estimateMethod,
                                                                 siteToolsDLLPath = siteToolsDLLPath,
                                                                 sasExePath = sasExePath)$error,
                                           SI_GI = ST_HTAgeToSI(curveRef = GRTH_CURVE,
                                                                boredAge = age,
                                                                ageType = ageType,
                                                                height = height,
                                                                estimateMethod = estimateMethod,
                                                                siteToolsDLLPath = siteToolsDLLPath,
                                                                sasExePath = sasExePath)$output)]
    worktable[SI_SP >= 0 & age <= 50 & GI_ERR >= 0,
              SI_TREE := SI_GI]
    return(worktable[order(uniObs),]$SI_TREE)
  })