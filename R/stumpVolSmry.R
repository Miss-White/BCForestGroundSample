#' Summarizes the volume for stumps - VRI specific
#' 
#' 
#' @description Calculates stump volume at cluster/species level and cluster.
#'              This function is equivalent to \code{stmpvol.sas} in original compiler. 
#'
#' @param stumpData data.table, Stump data. This data is from card g, i.e., \code{vi_g}.
#' @param stumpPlotHeader data.table, Plot header data for stump and small tree data. The data is from card e, i.e.,
#'                                    \code{vi_e}.                              
#'                           
#'                                                                     
#' @return Two tables: stmp_c is summarized volume at cluster level; stmp_cs is summarized volume at cluster/species level.
#' 
#' @importFrom data.table ':='
#' @importFrom fpCompare '%<=%' '%==%' '%>=%' '%!=%' '%>>%' '%<<%'
#'
#' @note 
#' 
#' @export
#' @docType methods
#' @rdname stumpVolSmry
#'
#' @author Yong Luo
#'
setGeneric("stumpVolSmry",
           function(stumpData, stumpPlotHeader){
             standardGeneric("stumpVolSmry")
           })

#' @rdname stumpVolSmry
setMethod(
  "stumpVolSmry",
  signature = c(stumpData = "data.table",
                stumpPlotHeader = "data.table"),
  definition = function(stumpData, stumpPlotHeader){
    stmp <- merge_dupUpdate(stumpData, stumpPlotHeader, 
                            by = "CLSTR_ID", all.x = TRUE)
    stmp[is.na(PLOT_WT), PLOT_WT := 1]
    stmp[, ':='(STMP_VOL = FREQ*((DIB/200)^2)*HEIGHT*pi)]
    stmp[, STMP_VNT := STMP_VOL * PCT_SND/100]
    stmp[, ':='(STP_VLHA = 10000*STMP_VOL/(PLOT_WT * pi * (F_RAD^2)),
                STP_VNHA = 10000*STMP_VNT/(PLOT_WT * pi * (F_RAD^2)))]
    stmp_cs <- stmp[,.(PROJ_ID = unique(PROJ_ID),
                       SAMP_NO = unique(SAMP_NO),
                       TYPE_CD = unique(TYPE_CD),
                       STP_VHT = sum(STP_VLHA),
                       STP_VHTN = sum(STP_VNHA)),
                    by = c("CLSTR_ID", "SPECIES")]
    stmp_c <- stmp[,.(PROJ_ID = unique(PROJ_ID),
                       SAMP_NO = unique(SAMP_NO),
                       TYPE_CD = unique(TYPE_CD),
                       STP_VHT = sum(STP_VLHA),
                       STP_VHTN = sum(STP_VNHA)),
                    by = c("CLSTR_ID")]
    return(list(stmp_c = stmp_c, stmp_cs = stmp_cs))
  })