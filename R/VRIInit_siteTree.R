#' Load and select site trees-VRI specific
#' 
#' @description This function connects site tree data (vi_h, cardh) to selected cluster/plot-level data.
#'              Site tree data is located in  \code{//Mayhem/GIS_TIB/RDW/RDW_Data2/Work_Areas/VRI_ASCII_PROD/vri_sa}
#' 
#' @param clusterplotHeader data.table, contains cluster/plot-level attributes, an output from \code{\link{VRIInit_clusterplot}}.
#'              
#' @param dataSourcePath character, Specifies the path that directs to the VRI original data soruce, i.e.,
#'                                  \code{//Mayhem/GIS_TIB/RDW/RDW_Data2/Work_Areas/VRI_ASCII_PROD/vri_sa}.
#' 
#'                            
#'                              
#'                            
#' @return A data table that contains site tree data information. A log file documents the detailed process
#' 
#' @note VRI specific
#' 
#' @importFrom data.table data.table ':=' set rbindlist setkey
#' @importFrom haven read_sas
#' @importFrom dplyr '%>%'
#' @export
#' @docType methods
#' @rdname VRIInit_siteTree
#'  
#' @author Yong Luo
#'   
#' 
#' 
setGeneric("VRIInit_siteTree",
           function(clusterplotHeader, 
                    dataSourcePath){
             standardGeneric("VRIInit_siteTree")   
           })       

#' @rdname VRIInit_siteTree
setMethod(
  "VRIInit_siteTree",
  signature = c(clusterplotHeader = "data.table", 
                dataSourcePath = "character"),
  definition = function(clusterplotHeader, 
                        dataSourcePath){
    compileLog <- appendedCat("Start to load and select site tree.")
    displayColumn <- c("CLSTR_ID", "PLOT", "TREE_NO")
    vi_h <- read_sas(file.path(dataSourcePath, "vi_h.sas7bdat")) %>% data.table
    names(vi_h) <- toupper(names(vi_h))
    compileLog <- appendedCat(paste("vi_h (site tree data) contains ", nrow(vi_h), " rows.", sep = ""),
                              compileLog)
    
    clusterplotHeader[, clusterplot := paste(CLSTR_ID, PLOT, sep = "_")]
    vi_h[, clusterplot := paste(CLSTR_ID, PLOT, sep = "_")]
    compileLog <- appendedCat(logFileProducer(reason = "Site trees are not in selected cluster/plot",
                                              action = "removed",
                                              displayTable = vi_h[!(clusterplot %in% clusterplotHeader$clusterplot),],
                                              displayColumn = displayColumn),
                              compileLog)
    
    vi_h <- vi_h[clusterplot %in% clusterplotHeader$clusterplot,]
    vi_h[, lendup := length(TREE_LEN), by = displayColumn]
    
    compileLog <- appendedCat(logFileProducer(reason = "Duplicate site tree observations at cluster/plot/tree level",
                                              action = "removed",
                                              displayTable = vi_h[lendup > 1,],
                                              displayColumn = displayColumn),
                              compileLog)
    vi_h <- unique(vi_h, by = displayColumn)
    
    # range(vi_h$BNG_DIAM, na.rm = TRUE) # from 0.1 to 999.9, is 999.9 correct?
    vi_h[(!is.na(BNG_DIAM) | BNG_DIAM != 0) & (!is.na(BARK_THK) | BARK_THK != 0),
         DBH := BNG_DIAM + 2*BARK_THK/10]
    compileLog <- appendedCat("DBH was calculated using diameter at bore (BNG_DIAM) and bark thickness (BARK_THK).",
                              compileLog)
    
    compileLog <- appendedCat(logFileProducer(reason = "Failed to calculate DBH",
                                              action = "no",
                                              displayTable = vi_h[is.na(DBH)],
                                              displayColumn = displayColumn),
                              compileLog)
    
    compileLog <- appendedCat(paste("After selection, vi_h has", nrow(vi_h), "trees."),
                              compileLog)
    vi_h[, ':='(PROJ_ID = NULL,
                SAMP_NO = NULL,
                TYPE_CD = NULL,
                clusterplot = NULL,
                lendup = NULL)]
    ##### sas codes 
    # IF UPCASE(WALKTHRU_STATUS) = 'W' THEN TREE_WT = 2;
    # ELSE IF UPCASE(WALKTHRU_STATUS) = 'O' THEN TREE_WT = 0;
    # ELSE WALKTHRU_STATUS = 1;
    vi_h <- vi_h[order(CLSTR_ID, PLOT, TREE_NO),]
    return(list(siteTreeData = vi_h,
                compileLog = compileLog))
    
  })
