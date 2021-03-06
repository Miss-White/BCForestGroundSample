#' Summarize the tree-level data at cluster or cluster/species level-VRI specific
#' 
#' 
#' @description Summarizes the compiled tree data (including both enhanced tree data and non-enhanced tree data) at
#'              cluster level. This function is equevalent to the summary part in sas compiler in \code{cp_vegi_2017.sas}.
#'              Different from the original compiler, this function outputs the summaries by summarized components,
#'              rather than putting all together.
#'              
#'              
#'
#' @param allVolumeTrees data.table, All tree data from vi_c and vi_i that have been compiled with
#'                                   tree volume.
#' @param clusterPlotHeader data.table, Cluster and plot-level information. An output of \code{\link{VRIInit_clusterplot}}.                                               
#' @param utilLevel numeric, Utilization levels.
#' @param weirdUtil character, Weird util. Default is No. Otherwise need to be specified as a number.
#' @param equation character, Specifies whether the compiler is based on \code{KBEC} or \code{KFIZ}.
#'                          
#' @return Cluster and species-level volume summaries; cluster-level volume summaries; cluster-level height summaries;
#'         cluster-level species composition summaries and log file.
#'         
#' 
#' @importFrom data.table ':='
#' @importFrom fpCompare '%<=%' '%==%' '%>=%' '%!=%' '%>>%' '%<<%'
#'
#' 
#' @export
#' @docType methods
#' @rdname VRISummaries
#'
#' @author Yong Luo
#'
setGeneric("VRISummaries",
           function(allVolumeTrees, clusterPlotHeader, utilLevel, weirdUtil, equation) {
             standardGeneric("VRISummaries")
           })

#' @rdname VRISummaries
setMethod(
  "VRISummaries",
  signature = c(allVolumeTrees = "data.table",
                clusterPlotHeader = "data.table",
                utilLevel = "numeric",
                weirdUtil = "character",
                equation = "character"),
  definition = function(allVolumeTrees, clusterPlotHeader, utilLevel, weirdUtil, equation){
    compileLog <- appendedCat("Summarize key attributes at cluster and cluster/species levels.")
    compileLog <- appendedCat("Summarize volume at cluster/species level.",
                              compileLog)
    allVolumeTrees <- merge_dupUpdate(allVolumeTrees,
                                      clusterPlotHeader[,.(CLSTR_ID, PLOT, PROJ_ID, NO_PLOTS, PLOT_DED,
                                                           BGC_ZONE, PLOT_WT, SAMP_TYP)],
                                      by = c("CLSTR_ID", "PLOT"))
    volsmy_cs <- volSmry_byCS(treeMC = data.table::copy(allVolumeTrees),
                              utilLevel = utilLevel,
                              weirdUtil = weirdUtil,
                              equation = equation)
    # write.csv(volsmy_cs, file.path(r_path, "smy_cs.csv"), row.names = F)
    
    ## volume summary by cluster
    compileLog <- appendedCat("Summarize volume at cluster level.",
                              compileLog)
    volsmy_c <- volSmry_byC(volSmryByCS = volsmy_cs)  
    volsmy_cs[, ':='(VPT_WSV = sum(VHA_WSV, na.rm = TRUE),
                  VPT_NETM = sum(VHA_NETM, na.rm = TRUE),
                  VPT_MER = sum(VHA_MER, na.rm = TRUE),
                  BA_PT = sum(BA_HA, na.rm = TRUE),
                  VPT_WSVDS = sum(VHA_WSVDS, na.rm = TRUE)),
           by = c("CLSTR_ID", "UTIL")]
    volsmy_cs[VPT_WSV %>>% 0, VPC_WSV := 100*VHA_WSV/VPT_WSV]
    volsmy_cs[VPT_NETM %>>% 0, VPC_NETM := 100*VHA_NETM/VPT_NETM]
    volsmy_cs[VPT_MER %>>% 0, VPC_MER := 100*VHA_MER/VPT_MER]
    volsmy_cs[VPT_WSVDS %>>% 0, VPC_WSVDS := 100*VHA_WSVDS/VPT_WSVDS]
    volsmy_cs[BA_PT %>>% 0, BA_PC := 100*BA_HA/BA_PT]
    volsmy_cs[,c("VPT_WSV", "VPT_NETM", "VPT_MER", "VPT_WSVDS", "BA_PT") := NULL]
    formatcols <- c("VPC_WSV", "VPC_NETM", "VPC_MER", "BA_PC", "VPC_WSVDS")
    volsmy_cs[, c(formatcols) := lapply(.SD, function(s) round(s, 1)), 
           .SDcols = formatcols]
    
    ## height summary by cluster
    compileLog <- appendedCat("Summarize tree height at cluster level.",
                              compileLog)
    heightsmry_c <- heightSmry_byC(treeMC = allVolumeTrees)
    compileLog <- appendedCat("Summarize species composition at cluster level.",
                              compileLog)
    cl_spc <- speciesComp_byC(CSSmryTable = volsmy_cs, basedOn = "BA_HA", speciesMaxNO = 12)
    compileLog <- appendedCat("End of volume, height and species composition summarization.",
                              compileLog)
    return(list(vol_bycs = volsmy_cs, vol_byc = volsmy_c, heightsmry_byc = heightsmry_c, 
                compositionsmry_byc = cl_spc, compileLog = compileLog))
  })



