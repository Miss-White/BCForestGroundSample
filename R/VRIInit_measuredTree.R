#' Load and select fully measured tree data-VRI specific
#' 
#' @description This function selects the tree-level data from vi_c (cardc) based on selected cluster/plot headers.
#'              Additonally, the function calculates basal area and tree per ha factor.
#'              
#' 
#' @param clusterplotHeader data.table, Cluster and plot-level attributes, an output from \code{\link{VRIInit_clusterplot}}.
#'              
#' 
#' @param dataSourcePath character, Specifies the path that directs to the VRI original data soruce, i.e.,
#'                                  \code{//Mayhem/GIS_TIB/RDW/RDW_Data2/Work_Areas/VRI_ASCII_PROD/vri_sa}.
#'
#' @param walkThru logical, Indicates whether walkthrough sampling protocal is used,
#'                          Tree weight is determined by walkthrough method. In walkthrough 
#'                          method, a tree is identified as \code{NA} (no walkthrough applied),
#'                           \code{O} for out tree (not counted), and \code{W} for double counted tree.
#'                                                    
#'                            
#'                              
#'                            
#' @return A data table that contains tree-level information. A log file that describes the detailed process.
#' 
#' 
#' @importFrom data.table data.table ':=' set rbindlist setnames setkey
#' @importFrom dplyr '%>%'
#' @importFrom haven read_sas
#' @export
#' @docType methods
#' @rdname VRIInit_measuredTree
#'  
#' @author Yong Luo
#'   
#' 
#' 
setGeneric("VRIInit_measuredTree",
           function(clusterplotHeader, 
                    dataSourcePath,
                    walkThru){
             standardGeneric("VRIInit_measuredTree")   
           })       

#' @rdname VRIInit_measuredTree
setMethod(
  "VRIInit_measuredTree",
  signature = c(clusterplotHeader = "data.table", 
                dataSourcePath = "character", 
                walkThru = "logical"),
  definition = function(clusterplotHeader, 
                        dataSourcePath,
                        walkThru){
    compileLog <- appendedCat("Start to load and select measured tree data vi_c.")
    displayColumn <- c("CLSTR_ID", "PLOT", "TREE_NO")
    vi_c <- read_sas(file.path(dataSourcePath, "vi_c.sas7bdat")) %>% data.table
    names(vi_c) <- toupper(names(vi_c))
    
    compileLog <- appendedCat(paste("vi_c: ", nrow(vi_c), "rows."))
    vi_c[, clusterPlot := paste(CLSTR_ID, PLOT, sep = "_")]
    clusterplotHeader[, clusterPlot := paste(CLSTR_ID, PLOT, sep = "_")]
    
    compileLog <- appendedCat(logFileProducer(reason = "Trees that do not have cluster/plot",
                                              action = "removed",
                                              displayTable = vi_c[!(clusterPlot %in% unique(clusterplotHeader$clusterPlot)), ],
                                              displayColumn = displayColumn),
                              compileLog)
    vi_c <- vi_c[clusterPlot %in% unique(clusterplotHeader$clusterPlot), ]
    vi_c[, uniLength := length(DBH), by = displayColumn]
    compileLog <- appendedCat(logFileProducer(reason = "Duplicate observations at cluster/plot/tree level",
                                              action = "removed",
                                              displayTable = vi_c[uniLength > 1, ],
                                              displayColumn = displayColumn),
                              compileLog)
    setnames(vi_c, paste("LOG", 1:9, "_GRD", sep = ""), paste("LOG_G_", 1:9, sep = ""))
    setnames(vi_c, paste("LOG", 1:9, "_LEN", sep = ""), paste("LOG_L_", 1:9, sep = ""))
    setnames(vi_c, paste("LOG", 1:9, "_SND", sep = ""), paste("LOG_S_", 1:9, sep = ""))
    
    vi_c[DBH != 0, BA_TREE := pi * ((DBH/200)^2)]
    compileLog <- appendedCat(logFileProducer(reason = "Basal area is not calculated",
                                              action = "no",
                                              displayTable = vi_c[is.na(BA_TREE),],
                                              displayColumn = displayColumn),
                              compileLog)
    
    # remove get_vars function as SP0 remains same as that in vi_pc table
    vi_c[DBH == 0, ':='(SP0 = NA, BA_TREE = NA)]
    vi_c[, TREE_WT := 1]
    if(walkThru){
      vi_c[toupper(WALKTHRU_STATUS) == "O", TREE_WT := 0] # tree is out and is not used
      vi_c[toupper(WALKTHRU_STATUS) == "W", TREE_WT := 2] # tree is 
    }
    vi_c <- merge_dupUpdate(vi_c, clusterplotHeader[,.(clusterPlot, SAMP_TYP, PLOT_WT, BLOWUP)],
                            by = "clusterPlot", all.x = TRUE)
    vi_c[, PHF_TREE := PHFCalculator(sampleType = SAMP_TYP, blowUp = BLOWUP,
                                     treeWeight = TREE_WT, plotWeight = PLOT_WT,
                                     treeBasalArea = BA_TREE)]
    
    
    # for NFI (F), CMI and YSMI, the plots use a 100 m2 subplot for
    # trees with a dbh < 9, therefore should be extrapolate to 400 m2 (size of large tree plot)
    vi_c[, orgphf := PHF_TREE]
    vi_c[substr(TYPE_CD, 1, 1) %in% c("F", "M", "Y") & DBH < 9,
         PHF_TREE := PHF_TREE*4]
    compileLog <- appendedCat(logFileProducer(reason = "For NFI, CMI and YSMI, trees with DBH < 9",
                                              action = "changed",
                                              displayTable = vi_c[substr(TYPE_CD, 1, 1) %in% c("F", "M", "Y") &
                                                                    DBH < 9,],
                                              displayColumn = displayColumn,
                                              changedVariable = "Tree per ha factor (PHF_TREE)",
                                              fromTo = c("orgphf", "PHF_TREE")),
                              compileLog)
    vi_c <- vi_c[order(CLSTR_ID, PLOT, TREE_NO),]
    compileLog <- appendedCat(paste("After selection, vi_c has", nrow(vi_c), "trees."),
                              compileLog)
    return(list(treeData = vi_c[,.(CLSTR_ID, PLOT, SPECIES, TREE_NO, 
                                   LV_D,ST_FL,NO_LOGS, 
                                   TREE_WT, DBH, SP0, BA_TREE, PHF_TREE, 
                                   HEIGHT, BARK_PER,
                                   HT_PROJ, DIAM_BTP, LOG_G_1,  LOG_G_2,  LOG_G_3,  LOG_G_4,  LOG_G_5,
                                   LOG_G_6,  LOG_G_7, LOG_G_8,  LOG_G_9, LOG_L_1,  LOG_L_2,  LOG_L_3,
                                   LOG_L_4,  LOG_L_5, LOG_L_6,  LOG_L_7,  LOG_L_8,  LOG_L_9, LOG_S_1, 
                                   LOG_S_2,  LOG_S_3,  LOG_S_4,  LOG_S_5,
                                   LOG_S_6,  LOG_S_7,  LOG_S_8,  LOG_S_9)],
                compileLog = compileLog))
  })


#' @export
#' @rdname VRIInit_measuredTree
setMethod(
  "VRIInit_measuredTree",
  signature = c(clusterplotHeader = "data.table", 
                dataSourcePath = "character", 
                walkThru = "missing"),
  definition = function(clusterplotHeader, 
                        dataSourcePath){
    return(VRIInit_measuredTree(clusterplotHeader, dataSourcePath, walkThru = TRUE))
  })


