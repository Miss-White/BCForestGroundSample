#' Derive volume components for H-enhanced and non-enhanced trees-VRI specific
#' 
#' 
#' @description Estimates volume components for H-enhanced and non-enhanced trees using regression and ratio methods.
#'              For H-enhanced trees, the whole stem volume and gross merchantable volume are already calculated directly using
#'              taper equations; and rest of volume components will be calculated using ratio method in this function.
#'              For non-enhanced trees, the whole stem volume is derived using regression equation between basal area
#'              and whole stem volume and the rest of volume components will be computed using ratio method in this function.
#'              The function is part of \code{vol_ha_2017.sas}.
#'
#' @param fullMeasuredTrees Compiled tree-level data in vi_c, which contains full measured trees, enhanced trees
#'                          and H-enhanced trees. This data is output of \code{\link{DWBCompiler}}
#'                                                                     
#' @param auxiTrees data.table, Non-enhanced trees in anxilirary plots, however, it may have enhanced trees and H-enhanced trees.
#'                              An output from \code{\link{VRIInit_auxTree}}.
#' 
#' @param clusterPlotHeader data.table, Cluster and plot-level information. An output of \code{\link{VRIInit_clusterplot}}.                                               
#'                           
#' @return A list of four tables: 1. fullenhancedtrees: full and enhanced trees; 
#'         2. HnonenhancedTrees: Height enhanced and non-enhanced trees;
#'         3. regression table; 4. ratio table.
#' 
#' @importFrom data.table ':='
#' @importFrom fpCompare '%<=%' '%==%' '%>=%' '%!=%' '%>>%' '%<<%'
#'
#' 
#' @export
#' @docType methods
#' @rdname auxiTreeCompiler
#'
#' @author Yong Luo
#'
setGeneric("auxiTreeCompiler",
           function(fullMeasuredTrees, auxiTrees, clusterPlotHeader) {
             standardGeneric("auxiTreeCompiler")
           })

#' @rdname auxiTreeCompiler
setMethod(
  "auxiTreeCompiler",
  signature = c(fullMeasuredTrees = "data.table",
                auxiTrees = "data.table",
                clusterPlotHeader = "data.table"),
  definition = function(fullMeasuredTrees, auxiTrees, clusterPlotHeader){
    ## prepare regression based on prj_grp, lv_d and sp0
    ## obtain regression data
    compileLog <- appendedCat("Start to compile tree volumes for H-enhanced/nonenhanced trees.")
    compileLog <- appendedCat("Merge full/enhanced/H-enhanced trees and non-enhanced trees.",
                              compileLog)
    tree_vb <- mergeAllVolTrees(treeMS = data.table::copy(fullMeasuredTrees),
                               treeAX = data.table::copy(auxiTrees))
    rm(fullMeasuredTrees, auxiTrees)
    compileLog <- appendedCat(paste("There are total ", nrow(tree_vb), " trees.",
                                    sep = ""),
                              compileLog)
    
    compileLog <- appendedCat(paste("   ", nrow(tree_vb[MEAS_INTENSE == "FULL",]),
                                    " trees are fully measured trees in IPC.", sep = ""),
                              compileLog)
    compileLog <- appendedCat(paste("   ", nrow(tree_vb[MEAS_INTENSE == "ENHANCED",]),
                                    " trees are enhanced trees in auxi plots.", sep = ""),
                              compileLog)
    compileLog <- appendedCat(paste("   ", nrow(tree_vb[MEAS_INTENSE == "H-ENHANCED",]),
                                    " trees are H-enhanced trees in auxi plots.", sep = ""),
                              compileLog)
    compileLog <- appendedCat(paste("   ", nrow(tree_vb[MEAS_INTENSE == "NON-ENHANCED",]),
                                    " trees are non-enhanced trees in auxi plots.", sep = ""),
                              compileLog)
    compileLog <- appendedCat(paste("   ", nrow(tree_vb[MEAS_INTENSE == "B-SAMPLE",]),
                                    " trees are B sample trees.", sep = ""),
                              compileLog)
    tree_vb <- merge_dupUpdate(tree_vb,
                               unique(clusterPlotHeader[,.(CLSTR_ID, TYPE_CD)], by = "CLSTR_ID"),
                               by = "CLSTR_ID", all.x = TRUE)
    nonvoltrees <- tree_vb[MEAS_INTENSE %in% c("H-ENHANCED", "NON-ENHANCED"),] ## tree with H-enhanced and non-enhanced
    voltrees <- tree_vb[MEAS_INTENSE %in% c("FULL", "ENHANCED", "B-SAMPLE"), ] ## tree are fully and enhanced measured
    fullMeasuredTrees <- tree_vb[MEAS_INTENSE %in% c("FULL", "ENHANCED", "H-ENHANCED"), ] ## for calibrating regression and ratio
    fullMeasuredTrees[, uniobsid := 1:nrow(fullMeasuredTrees)]
    rm(tree_vb)
    if(nrow(nonvoltrees) > 0){
      ## go to the non-enhanced trees routine to calculate volume components
      compileLog <- appendedCat(paste("Start to derive volumes for H-enhanced & non-enhanced trees: ", 
                                      nrow(nonvoltrees), " observations.", sep = ""),
                                compileLog)
      compileLog <- appendedCat("Develop BA-WSV regressions using FULL, ENHANCED and H-ENHANCED trees.",
                                compileLog)
      regressionData <- merge_dupUpdate(fullMeasuredTrees[VOL_WSV %>=% 0 & 
                                                            ST_FL != "F", # all standing trees
                                                          .(CLSTR_ID, PLOT, TREE_NO, ST_FL, SPECIES, SP0,
                                                            LV_D, VOL_WSV, BA_TREE, LOG_G_1, uniobsid)], 
                                        unique(clusterPlotHeader[,.(CLSTR_ID, PROJ_ID, MEAS_DT, SAMP_NO, TYPE_CD)], by = "CLSTR_ID"),
                                        by = "CLSTR_ID", all.x = TRUE)
      regressionData <- regressionData[(substr(TYPE_CD, 1, 1) != "A" & # all type_cd does not include A 
                                          substr(TYPE_CD, 2, 2) != "A" & # which is an audit plot
                                          substr(TYPE_CD, 3, 3) != "A"), ]
      regressionData[, ':='(L10WSV = log10(VOL_WSV),
                            L10BA = log10(BA_TREE))]
      regressionData[, ':='(PRJ_GRP = prj_ID2Grp(PROJ_ID),
                            TYPE = speciesCode2speciesType(SP0),
                            SPECIES = substr(SPECIES, 1, 2),
                            MEASYR = as.numeric(substr(MEAS_DT, 1, 4)))]
      regressionData[substr(SPECIES, 1, 1) %in% c("X", "Z"), SP0 := "X"]
      regressionData <- unique(regressionData[order(PROJ_ID, SAMP_NO, PLOT, TREE_NO, LV_D, -MEASYR),],
                               by = c("PROJ_ID", "SAMP_NO", "PLOT", "TREE_NO", "LV_D"))
      # the below codes are consistent with reg_wsv_2017.sas
      reg_all <- WSV_BARegression(regressionData = regressionData)
      compileLog <- appendedCat(paste("    Developed BA-WSV regressions using ", 
                                      nrow(regressionData), " observations.", sep = ""),
                                compileLog)
      
      ## prepare toWSV ratio 
      ## ratios can only be calcualted for live and full measured/enhanced trees that are not in NFI
      ## inner join here
      ratiodata <- merge_dupUpdate(x = data.table::copy(fullMeasuredTrees)[, ':='(SP0 = NULL)],
                                   y = regressionData[,.(uniobsid, SP0, TYPE, PRJ_GRP)], 
                                   by = c("uniobsid"))
      useFM <- FALSE
      if(!useFM){
        ratiodata <- ratiodata[!(substr(TYPE_CD, 1, 1) %in% c("F", "M", "L", "Y")) & MEAS_INTENSE %in% c("FULL", "ENHANCED"),]
      } else {
        ratiodata <- ratiodata[!(substr(TYPE_CD, 1, 1) %in% c("L", "Y")) & MEAS_INTENSE %in% c("FULL", "ENHANCED"),]
      }
      ## call calc_ratio_2017.sas
      ratios_by_group <- toWSVRatio(ratioData = ratiodata)
      compileLog <- appendedCat(paste("Derived to-WSV ratios using : ", 
                                      nrow(ratiodata), " observations.", sep = ""),
                                compileLog)
      rm(regressionData, ratiodata, fullMeasuredTrees)
      nonvoltrees <- merge_dupUpdate(nonvoltrees, 
                                     unique(clusterPlotHeader[,.(CLSTR_ID, PROJ_ID)], by = "CLSTR_ID"),
                                     by = "CLSTR_ID", all.x = TRUE)
      compileLog <- appendedCat("Apply regression and ratio methods to calculate volumes for H-enhanced/non-enhanced trees.",
                                compileLog)
      nonvoltrees <- treeVolEst_RegRatioMethod(nonVolTrees = nonvoltrees,
                                               regressionTable = reg_all,
                                               ratioTable = ratios_by_group)
      compileLog <- appendedCat("End of volume compilations for H-enhanced/non-enhanced trees.",
                                compileLog)
      return(list(fullenhancedtrees = voltrees,
                  HnonenhancedTrees = nonvoltrees,
                  regressionTable = reg_all,
                  ratioTable = ratios_by_group,
                  compileLog = compileLog))
    } else {
      compileLog <- appendedCat(paste("Calculating volumes for H-enhanced/non-enhanced trees is skipped.", sep = ""),
                                compileLog)
      return(list(fullenhancedtrees = voltrees,
                  HnonenhancedTrees = NA,
                  regressionTable = NA,
                  ratioTable = NA,
                  compileLog = compileLog))
    }
  })