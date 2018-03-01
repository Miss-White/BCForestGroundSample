#' Estimate volume for H-enhanced and non-enhanced trees-VRI specific
#' 
#' 
#' @description This function estimates the volumes for JH-enhanced and non-enhanced trees using \code{BA-WSV} equation and 
#'              \code{toWSV} ratio methods. For H-enhanced trees, the whole stem volume and gross merchantable volume are already calculated directly using
#'              taper equations; and rest of volume components will be calculated using ratio method in this function.
#'              For non-enhanced trees, the whole stem volume is derived using regression equation between basal area
#'              and whole stem volume and the rest of volume components will be computed using ratio method in this function.
#'
#' @param nonVolTrees data.table, H-enhanced trees and non-enhanced trees.
#' @param regressionTable data.table, Specifies the WSV-BA equations by project group \code{PRJ_GRP}, live and dead status \code{LV_D},
#'                                   stand and falling status \code{SF_COMPILE} and species code \code{SP0}. The table can be 
#'                                   generated using \code{\link{WSV_BARegression}}.
#' @param ratioTable data.table, Specifies \code{toWSV} ratio by project group \code{PRJ_GRP}, live and dead status \code{LV_D},
#'                                   stand and falling status \code{SF_COMPILE} and species code \code{SP0}.
#'                                   The table can be generated using \code{\link{toWSVRatio}}.                           
#'                                                                     
#' @return A data table that has compiled non volume trees.
#' 
#' @importFrom data.table ':='
#' @importFrom fpCompare '%<=%' '%==%' '%>=%' '%!=%' '%>>%' '%<<%'
#'
#' 
#' @export
#' @docType methods
#' @rdname treeVolEst_RegRatioMethod
#'
#' @author Yong Luo
#'
setGeneric("treeVolEst_RegRatioMethod",
           function(nonVolTrees, regressionTable, ratioTable) {
             standardGeneric("treeVolEst_RegRatioMethod")
           })

#' @rdname treeVolEst_RegRatioMethod
setMethod(
  "treeVolEst_RegRatioMethod",
  signature = c(nonVolTrees = "data.table",
                regressionTable = "data.table",
                ratioTable = "data.table"),
  definition = function(nonVolTrees, regressionTable, ratioTable){
    ## standardize the species code
    nonVolTrees[substr(SPECIES, 1, 1) %in% c("X", "Z"), SP0 := "X"]
    nonVolTrees[, PRJ_GRP := prj_ID2Grp(PROJ_ID),]
    volVariables <- c(paste("VOL_",c("WSV", "NET", "MER", "NETM", "NTW2",
                                     "NTWB", "D", "DW", "DWB"),
                            sep = ""), "VAL_MER")
    ## why the last one is included?? val_mer is value not volume
    
    ratioVariables <- paste("RATIO_",c("WSV", "NET", "MER", "NETM", "NTW2",
                                       "NTWB", "D", "DW", "DWB", "VAL"),
                            sep = "")
    nonVolTrees <- merge_dupUpdate(nonVolTrees, regressionTable,
                     by = c("PRJ_GRP", "LV_D", "SF_COMPILE", "SP0"),
                     all.x = TRUE)
    nonVolTrees[!is.na(SOURCE), METHOD := "reg_log10"]
    nonVolTrees[is.na(SOURCE), METHOD := "not computed"]
    nonVolTrees[MEAS_INTENSE == "H-ENHANCED", METHOD := "computed"] ## FOR THE HEIGHT ENHANCED TREES
                                                                    ## VOL_WSV IS COMPUTED
    nonVolTrees[MEAS_INTENSE != "H-ENHANCED" & (DBH %<=% 1 | DBH %>>% 1000), METHOD := "not computed"]
    
    nonVolTrees[METHOD == "reg_log10", VOL_WSV := 10^(INTERCEPT + L10BA * log10(BA_TREE) + MSE/2)]
    nonVolTrees[METHOD == "reg_log10" & VOL_WSV %<<% 0, VOL_WSV := 0]
    set(nonVolTrees, ,c("TYPE", "COUNT", "MODEL", "INTERCEPT", "L10BA",
                         "EDF", "RSQ", "MSE", "SOURCE"), NULL)
    nonVolTrees <- merge_dupUpdate(nonVolTrees, ratioTable,
                     by = c("PRJ_GRP", "LV_D", "SF_COMPILE", "SP0"),
                     all.x = TRUE)
    # nonVolTrees[, NET_FCT_METHOD := "Ratio"]
    nonVolTrees[is.na(VOL_WSV), VOL_WSV := 0]
    ### estimate volumn components for tree with dbh >= 10cm
    output <- nonVolTrees[DBH %<<% 10 | is.na(DBH),] # add is.na to make the whole dataset
                                                     ## is covered
    output[, NET_FCT_METHOD := as.character(NA)]
    nonVolTrees <- nonVolTrees[DBH %>=% 10,]
    nonVolTrees[, NET_FCT_METHOD := "Ratio"]
    ### for trees with height information in the recent inventoried data
    ### vol_mer can be estimated directly using kozak equations, and do not need to apply ratio method.
    ### for the trees without height (I.E., NON-ENHANCED), assign vol_net using ratio method
    nonVolTrees[MEAS_INTENSE == "NON-ENHANCED", 
                volVariables[3] := unlist(nonVolTrees[MEAS_INTENSE == "NON-ENHANCED",
                                                      ratioVariables[3], with = FALSE]) * VOL_WSV]
    ### for all trees, i.e., with and without height information, estimate the rest of volume components 
    ### using ratio method
    for(i in c(2, 4:length(volVariables))){
      nonVolTrees[, volVariables[i] := unlist(nonVolTrees[, ratioVariables[i],
                                           with = FALSE]) * VOL_WSV]
    }
    output <- rbindlist(list(nonVolTrees, output), fill = TRUE)
    set(output, ,c(ratioVariables, "COUNT", "SOURCE", "TYPE", "PRJ_GRP", "PROJ_ID"), NULL)
    rm(nonVolTrees)
    output[VOL_WSV %<<% VOL_NTWB, VOL_NTWB := VOL_WSV]
    return(output)
  })




