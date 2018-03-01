#' Load and select auxiliary plot trees-VRI specific
#' 
#' @description This function loads and selects auxiliary data (\code{vi_i}, cardi) based on cluster/plot header.
#'              
#' 
#' @param clusterplotHeader data.table, Cluster and plot level attributes, an output from \code{\link{VRIInit_clusterplot}}.
#'              
#' @param dataSourcePath character, Specifies the path that directs to the VRI original data soruce, i.e.,
#'                                  \code{//Mayhem/GIS_TIB/RDW/RDW_Data2/Work_Areas/VRI_ASCII_PROD/vri_sa}.
#'
#'                            
#' @return A data table that contains auxiliary plot tree data. A log file documents the detailed process
#' 
#' 
#' @importFrom data.table data.table ':=' set rbindlist setnames setkey
#' @importFrom dplyr '%>%'
#' @importFrom haven read_sas
#' @export
#' @docType methods
#' @rdname VRIInit_auxTree
#'  
#' @author Yong Luo
#'   
#' 
#' 
setGeneric("VRIInit_auxTree",
           function(clusterplotHeader, 
                    dataSourcePath){
             standardGeneric("VRIInit_auxTree")   
           })       

#' @rdname VRIInit_auxTree
setMethod(
  "VRIInit_auxTree",
  signature = c(clusterplotHeader = "data.table", 
                dataSourcePath = "character"),
  definition = function(clusterplotHeader, 
                        dataSourcePath){
    compileLog <- appendedCat("Start to load and select auxiliary plot tree vi_i.")
    displayColumn <- c("CLSTR_ID", "PLOT", "TREE_NO")
    vi_i <- read_sas(file.path(dataSourcePath, "vi_i.sas7bdat")) %>% data.table
    names(vi_i) <- toupper(names(vi_i))
    
    compileLog <- appendedCat(paste("vi_i:", nrow(vi_i), "rows."),
                              compileLog)
    
    clusterplotHeader[, clusterplot := paste(CLSTR_ID, PLOT, sep = "_")]
    vi_i[, clusterplot := paste(CLSTR_ID, PLOT, sep = "_")]
    
    compileLog <- appendedCat(logFileProducer(reason = "Auxiliary plot trees are not in selected cluster/plot",
                                              action = "removed",
                                              displayTable = vi_i[!(clusterplot %in% clusterplotHeader$clusterplot),],
                                              displayColumn = displayColumn),
                              compileLog)
    
    vi_i <- vi_i[clusterplot %in% clusterplotHeader$clusterplot,]
    if(nrow(vi_i) > 0){
      vi_i[, lendup := length(DBH), by = displayColumn]
      compileLog <- appendedCat(logFileProducer(reason = "Duplicate auxiliary tree observations at cluster/plot/tree level",
                                                action = "removed",
                                                displayTable = vi_i[lendup > 1,],
                                                displayColumn = displayColumn),
                                compileLog)
      vi_i <- unique(vi_i, by = displayColumn)
      vi_i[ ,':='(PROJ_ID = NULL,
                  SAMP_NO = NULL,
                  TYPE_CD = NULL)]
      vi_i[, orgLV_D := LV_D]
      vi_i[is.na(LV_D), LV_D := "L"]
      compileLog <- appendedCat(logFileProducer(reason = "Missing live/dead information",
                                                action = "changed",
                                                displayTable = vi_i[is.na(orgLV_D),],
                                                displayColumn = displayColumn,
                                                changedVariable = "LV_D",
                                                fromTo = c("orgLV_D", "LV_D")),
                                compileLog)
      vi_i[, orgLV_D := NULL]
      
      vi_i[, TREE_WT := 1]
      vi_i[DBH != 0, BA_TREE := pi * ((DBH/200)^2)]
      compileLog <- appendedCat("Assinged TREE_WT as 1. \nCalculated tree basal area (BA_TREE) when DBH was available", 
                                compileLog)
      compileLog <- appendedCat(logFileProducer(reason = "Failed to calculate tree basal area (BA_TREE)",
                                                action = "no", 
                                                displayTable = vi_i[is.na(BA_TREE)],
                                                displayColumn = displayColumn),
                                compileLog)
      vi_i <- merge_dupUpdate(vi_i, clusterplotHeader[, .(clusterplot, SAMP_TYP, BLOWUP, PLOT_WT)],
                              by = "clusterplot", all.x = TRUE)
      vi_i[, PHF_TREE := PHFCalculator(sampleType = SAMP_TYP, blowUp = BLOWUP, treeWeight = TREE_WT,
                                       plotWeight = PLOT_WT, treeBasalArea = BA_TREE)]
      compileLog <- appendedCat("Calculated tree per ha factor (PHF_TREE).",
                                compileLog)
      compileLog <- appendedCat(logFileProducer(reason = "Failed to calculate tree per ha factor (PHF_TREE)",
                                                action = "no", 
                                                displayTable = vi_i[is.na(PHF_TREE)],
                                                displayColumn = displayColumn),
                                compileLog)
      compileLog <- appendedCat(paste("After selection, vi_i has", nrow(vi_i), "observatons."),
                                compileLog)
      return(list(auxiPlotTree = vi_i[,.(CLSTR_ID, PLOT, SPECIES, TREE_NO, 
                                         DBH, SP0, BA_TREE,
                                         PHF_TREE, LV_D)],
                  compileLog = compileLog))
    } else {
      compileLog <- appendedCat(paste("After selection, vi_i has", nrow(vi_i), "observatons."),
                                compileLog)
      return(list(auxiPlotTree = vi_i[,.(CLSTR_ID, PLOT, SPECIES, TREE_NO)],
                  compileLog = compileLog))
    }
    ## please note that for auxiliary plots, they are part of VRI design, therefore, there no small tree plot (DBH<9),
    ## no need for adjusting tree per ha factor

  })

