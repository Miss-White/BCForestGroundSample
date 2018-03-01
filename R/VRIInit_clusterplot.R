#' Load and select cluster and plot level data- VRI specific
#' 
#' 
#' @description This function prepares the cluster/plot-level inputs for VRI compiler. Specifically, it standardizes names for
#'              the variables; reports and removes the duplicate observations at cluster, cluster/plot.
#'              
#'
#' @param dataSourcePath character, Specifies the path that directs to the VRI original data soruce, i.e.,
#'                                  \code{//Mayhem/GIS_TIB/RDW/RDW_Data2/Work_Areas/VRI_ASCII_PROD/vri_sa}.
#'                            
#' @return A data table that contains key information at cluster/plot level and compiler log file. 
#' 
#' @importFrom data.table ':='
#' @importFrom haven read_sas
#' 
#' @export
#' @docType methods
#' @rdname VRIInit_clusterplot
#'
#' @author Yong Luo
#'
setGeneric("VRIInit_clusterplot",
           function(dataSourcePath){
             standardGeneric("VRIInit_clusterplot")
           })

#' @rdname VRIInit_clusterplot
setMethod(
  "VRIInit_clusterplot",
  signature = c(dataSourcePath = "character"),
  definition = function(dataSourcePath){
    compilerLog <- appendedCat("Load and select cluster-level data vi_a.")
    ## for vi_a
    vi_a <- read_sas(file.path(dataSourcePath, "vi_a.sas7bdat")) %>% data.table
    names(vi_a) <- toupper(names(vi_a))
    totalNrow <- nrow(vi_a)
    uniqueNrow <- length(unique(vi_a$CLSTR_ID))
    vi_a[, dupLength := length(SAMP_NO), by = "CLSTR_ID"]
    compilerLog <- appendedCat(paste("vi_a:", totalNrow, "rows and ", uniqueNrow, "clusters."),
                               compilerLog)
    
    compilerLog <- appendedCat(logFileProducer(reason = "Duplicate cluster in vi_a file",
                                               action = "removed",
                                               displayTable = vi_a[dupLength > 1,],
                                               displayColumn = "CLSTR_ID"),
                               compilerLog)
    vi_a <- unique(vi_a, by = "CLSTR_ID")
    vi_a[, dupLength := NULL]
    
    compilerLog <- appendedCat(logFileProducer(reason = "DEV plot",
                                               action = "removed",
                                               displayTable = vi_a[substr(PROJ_ID, 1, 3) == "DEV",],
                                               displayColumn = "CLSTR_ID"),
                               compilerLog)
    vi_a <- vi_a[substr(PROJ_ID, 1, 3) != "DEV",]
    
    compilerLog <- appendedCat(logFileProducer(reason = "Ecological plot",
                                               action = "removed",
                                               displayTable = vi_a[substr(TYPE_CD, 1, 1) == "E",],
                                               displayColumn = "CLSTR_ID"),
                               compilerLog)
    
    vi_a <- vi_a[substr(TYPE_CD, 1, 1) != "E", ]
    
    
    # The plots belong to LGMW project, which samples each polygon (a unit of sample)
    # that has one or more plots, however, the plot identity is not unique
    # remove these plot from further compilation 
    compilerLog <- appendedCat(logFileProducer(reason = "LGMW project",
                                               action = "removed",
                                               displayTable = vi_a[substr(TYPE_CD, 1, 1) == "W",],
                                               displayColumn = "CLSTR_ID"),
                               compilerLog)
    vi_a <- vi_a[substr(TYPE_CD, 1, 1) != "W",] # double check with Bob and Rene
    
    vi_a[, PRJ_GRP := prj_ID2Grp(PROJ_ID)]
    vi_a[, orgBgc_zone := BGC_ZONE]
    compilerLog <- appendedCat(logFileProducer(reason = "Unknown project group",
                                               action = "no",
                                               displayTable = vi_a[PRJ_GRP == "Unknown",],
                                               displayColumn = "CLSTR_ID"),
                               compilerLog)
    
    vi_a[!(orgBgc_zone %in% c("AT","BWBS","CDF","CWH","ESSF","ICH","IDF","MH",
                              "MS","PP","SBPS","SBS","SWB","BG","BAFA","CMA","IMA")),
         BGC_ZONE := prj_ID2BEC(PROJ_ID)]
    
    compilerLog <- appendedCat(logFileProducer(reason = "BGC_ZONE is not available and assigned based on project id",
                                               action = "changed",
                                               displayTable = vi_a[BGC_ZONE != orgBgc_zone,],
                                               displayColumn = "CLSTR_ID",
                                               changedVariable = "BGC_ZONE",
                                               fromTo = c("orgBgc_zone", "BGC_ZONE")),
                               compilerLog)
    
    vi_a[, org_fiz := FIZ]
    vi_a[is.na(FIZ) | FIZ == " ", FIZ := "E"]
    compilerLog <- appendedCat(logFileProducer(reason = "FIZ is not available",
                                               action = "changed",
                                               displayTable = vi_a[is.na(org_fiz) | org_fiz == " ",],
                                               displayColumn = "CLSTR_ID",
                                               changedVariable = "FIZ",
                                               fromTo = c("org_fiz", "FIZ")),
                               compilerLog)
    
    ## keep key columns for further compilation
    vi_a <- vi_a[,.(CLSTR_ID, PROJ_ID, SAMP_NO, TYPE_CD, MEAS_DT, TSA, TFL, FIZ, BGC_ZONE,
                    BGC_SBZN, BGC_VAR, PRJ_GRP)]
    compilerLog <- appendedCat(paste("vi_a:", nrow(vi_a), "clusters left."),
                               compilerLog)
    
    compilerLog <- appendedCat("Load and select cluster/plot-level data vi_b.",
                               compilerLog)
    vi_b <- read_sas(file.path(dataSourcePath, "vi_b.sas7bdat")) %>% data.table
    names(vi_b) <- toupper(names(vi_b))
    totalNrow <- nrow(vi_b)
    uniqueNrow <- nrow(unique(vi_b, by = c("CLSTR_ID", "PLOT")))
    compilerLog <- appendedCat(paste("vi_b :", totalNrow, "rows and ", uniqueNrow, "cluster/plots."),
                               compilerLog)
    compilerLog <- appendedCat(logFileProducer(reason = "Plots not in selected cluster",
                                               action = "removed",
                                               displayTable = vi_b[!(CLSTR_ID %in% vi_a$CLSTR_ID)],
                                               displayColumn = c("CLSTR_ID", "PLOT")),
                               compilerLog)
    vi_b <- vi_b[CLSTR_ID %in% vi_a$CLSTR_ID,]
    vi_b[, dupLength := length(SAMP_NO), by = c("CLSTR_ID", "PLOT")]
    compilerLog <- appendedCat(logFileProducer(reason = "Duplicate cluster/plot",
                                               action = "removed",
                                               displayTable = vi_b[dupLength > 1,],
                                               displayColumn = c("CLSTR_ID", "PLOT")),
                               compilerLog)
    vi_b <- unique(vi_b, by = c("CLSTR_ID", "PLOT"))
    vi_b[V_BAF > 0 & V_FULL != "", PLOT_WT := 1]
    vi_b[V_BAF > 0 & V_HALF != "", PLOT_WT := 2]
    vi_b[V_BAF > 0 & V_QRTR != "", PLOT_WT := 4]
    vi_b[V_BAF > 0, ':='(SAMP_TYP = "V", BLOWUP = V_BAF)]
    # for fixed area plot
    vi_b[is.na(V_BAF) & F_FULL != "", PLOT_WT := 1]
    vi_b[is.na(V_BAF) & F_HALF != "", PLOT_WT := 2]
    vi_b[is.na(V_BAF) & F_QRTR != "", PLOT_WT := 4]
    vi_b[is.na(V_BAF), ':='(SAMP_TYP = "F", 
                            BLOWUP = 1 / (3.1415926* F_RAD^2) * 10000)]
    compilerLog <- appendedCat(logFileProducer(reason = "PLOT_WT can not be derived",
                                               action = "no",
                                               displayTable = vi_b[is.na(PLOT_WT),],
                                               displayColumn = c("CLSTR_ID", "PLOT")),
                               compilerLog)
    compilerLog <- appendedCat(logFileProducer(reason = "BLOWUP can not be derived",
                                               action = "no",
                                               displayTable = vi_b[is.na(BLOWUP),],
                                               displayColumn = c("CLSTR_ID", "PLOT")),
                               compilerLog)
    vi_b[, NO_PLOTS := length(PLOT), by = CLSTR_ID]
    vi_b[, PLOT_DED := 1L]
    vi_b[substr(TYPE_CD, 1, 1) == "N" | (as.numeric(substr(MEAS_DT, 1, 4)) >= 2008) | (as.numeric(substr(MEAS_DT, 1, 4)) == 2007 & PROJ_ID %in% c("0141", "014M", "0091")),
         PLOT_DED := NO_PLOTS]
    vi_b <- vi_b[,.(CLSTR_ID, PLOT, SAMP_TYP, PLOT_WT, BLOWUP, NO_PLOTS, PLOT_DED)]
    clusterplot <- merge_dupUpdate(vi_b, vi_a, by = "CLSTR_ID", all.x = TRUE)
    compilerLog <- appendedCat(paste("After selection, cluster/plot header table has", length(unique(clusterplot$CLSTR_ID)), "clusters and ",
                                     nrow(vi_b), "plots."),
                               compilerLog)
    return(list(clusterPlotHeader = clusterplot,
                compilerLog = compilerLog))
  })

