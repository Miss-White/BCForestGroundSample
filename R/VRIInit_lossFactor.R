#' Load and select trees that have loss factor information-VRI specific
#' 
#' @description This function loads and selects trees that have loss factor information (\code{vi_d}, cardd) based on 
#'              selected trees from vi_c.
#' 
#' @param fullMeasuredTrees data.table, Selected trees in vi_c, which includes full, enhanced and H-enhanced trees.
#'                                      An output of \code{\link{VRIInit_measuredTree}}.
#'              
#' @param dataSourcePath character, Specifies the path that directs to the VRI original data soruce, i.e.,
#'                                  \code{//Mayhem/GIS_TIB/RDW/RDW_Data2/Work_Areas/VRI_ASCII_PROD/vri_sa}.
#'
#'                            
#' @return A data table that contains loss factor data. A log file documents the detailed process
#' 
#' 
#' @importFrom data.table data.table ':=' set rbindlist setnames setkey
#' @importFrom dplyr '%>%'
#' @importFrom haven read_sas
#' @export
#' @docType methods
#' @rdname VRIInit_lossFactor
#'  
#' @author Yong Luo
#'   
#' 
#' 
setGeneric("VRIInit_lossFactor",
           function(fullMeasuredTrees, 
                    dataSourcePath){
             standardGeneric("VRIInit_lossFactor")   
           })       

#' @rdname VRIInit_lossFactor
setMethod(
  "VRIInit_lossFactor",
  signature = c(fullMeasuredTrees = "data.table", 
                dataSourcePath = "character"),
  definition = function(fullMeasuredTrees, 
                        dataSourcePath){
    compileLog <- appendedCat("Start to load tree-level loss factor information vi_d.")
    lossfactors <- haven::read_sas(file.path(dataSourcePath, "vi_d.sas7bdat")) %>% data.table
    compileLog <- appendedCat(paste("vi_d has ", nrow(lossfactors), " rows.", sep = ""),
                              compileLog)
    names(lossfactors) <- toupper(names(lossfactors))
    lossfactors <- lossfactors[, c("CLSTR_ID", "PLOT", "TREE_NO", paste("LOSS", 1:8, "_IN", sep = ""),
                                   paste("LOC", 1:8, "_FRO", sep = "")), with = FALSE]
    lossfactors[, obslength := length(LOSS1_IN), by = c("CLSTR_ID", "PLOT", "TREE_NO")]
    compileLog <- appendedCat(logFileProducer(reason = "Duplicate observations",
                                              action = "removed",
                                              displayTable = lossfactors[obslength > 1,],
                                              displayColumn = c("CLSTR_ID", "PLOT", "TREE_NO")),
                              compileLog)
    lossfactors <- unique(lossfactors, by = c("CLSTR_ID", "PLOT", "TREE_NO"))
    fullMeasuredTrees[, clusterplottree := paste(CLSTR_ID, "_", PLOT, "_", TREE_NO, sep = "")]
    lossfactors[, clusterplottree := paste(CLSTR_ID, "_", PLOT, "_", TREE_NO, sep = "")]
    compileLog <- appendedCat(logFileProducer(reason = "Trees are not in vi_c",
                                              action = "removed",
                                              displayTable = lossfactors[!(clusterplottree %in% fullMeasuredTrees$clusterplottree),],
                                              displayColumn = c("CLSTR_ID", "PLOT", "TREE_NO")),
                              compileLog)
    lossfactors <- lossfactors[clusterplottree %in% fullMeasuredTrees$clusterplottree,]
    lossfactors[, ':='(clusterplottree = NULL,
                       obslength = NULL)]
    compileLog <- appendedCat(paste("After selection, vi_d has", nrow(lossfactors), "observatons."),
                              compileLog)
    return(list(treeLossFactor = lossfactors, compileLog = compileLog))
  })