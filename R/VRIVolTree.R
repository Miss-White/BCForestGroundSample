#' Calcualte tree volume-VRI specific
#' 
#' @description This function use BEC(or FIZ) and species-specific taper equation to
#'              calculate tree volume. Before calculation, the function adjusts height 
#'              for broken top trees: scenario 1 (D scenario): availability of DBH, DIB at broken height; 
#'              scenario 2 (H scenario): availability of projected tree height in the field.  
#'              This functions also assigns the volume multiplier adjustment.
#'              The function is equivalent to \code{vir_vol_tree_2011} macro in orignal SAS compiler.
#'
#' @param treeData data.table, An output from \code{\link{VRIInit_measuredTree}} function, i.e., vi_c data.
#' 
#'                      
#' @param equation character, Specifies which taper equation form will be used to calculate
#'                                     diameter inside bark for a given height.
#'                                     Must be either KBEC or KFIZ3. If missing, default is KBEC
#' 
#' @param logMinLength numeric, Specifies a minimum length for a log.
#'                                                               
#' @param stumpHeight numeric, Specifies stump height. If missing, 0.3 m will be used.
#' @param breastHeight numeric, Specifies breast height. 1.3 m will be used when this arguement is missing.
#' @param UTOPDIB numeric, Specifies minimum merchantable inside bark diameter. 10 cm is used as a default. 
#' @return A data table
#' 
#' @importFrom data.table ':='
#' @importFrom dplyr left_join
#' @importFrom fpCompare %>>% %<<% %==% %!=% %<=% %>=%
#'
#' 
#' @export
#' @docType methods
#' @rdname VRIVolTree
#'
#' @author Yong Luo
#'
setGeneric("VRIVolTree",
           function(treeData, equation, logMinLength, 
                    stumpHeight, breastHeight, UTOPDIB) {
             standardGeneric("VRIVolTree")
           })

#' @rdname VRIVolTree
setMethod(
  "VRIVolTree",
  signature = c(treeData = "data.table",
                equation = "character",
                logMinLength = "numeric",
                stumpHeight = "numeric",
                breastHeight = "numeric",
                UTOPDIB = "numeric"),
  definition = function(treeData, equation, logMinLength,
                        stumpHeight, breastHeight, UTOPDIB){
    #### please note that this function reorder the process from the original macro
    ## before apply taper equation to calculate tree volume, 
    ## 1. check species, FIZ and BEC zone
    equation <- toupper(equation)
    compileLog <- appendedCat(paste("Start to calculate tree volume based on taper equation = ", 
                                    equation, ".", sep = ""))
    beczones <- c('AT','BWBS','CDF','CWH','ESSF','ICH','IDF','MH',
                  'MS','PP','SBPS','SBS','SWB','BG','BAFA','CMA','IMA')
    if(equation == "FIZ"){
      treeData[, ':='(ADJ_ID = " ",
                      VOL_MULT = 1, BEC = BGC_ZONE,
                      BGC_VAR = gsub(" ", "", BGC_VAR))]
      treeData[BGC_ZONE %in% c("BAFA", "CMA", "IMA"), BEC := "AT"]
      treeData[BGC_ZONE %in% c("BG"), BEC := "PP"]
    } else if (equation == "KBEC") {
      treeData[BGC_ZONE %in% beczones, ':='(ADJ_ID = " ",
                                            VOL_MULT = 1, BEC = BGC_ZONE,
                                            BGC_VAR = gsub(" ", "", BGC_VAR))]
      treeData[BGC_ZONE %in% c("BAFA", "CMA", "IMA"), BEC := "AT"]
      treeData[BGC_ZONE %in% c("BG"), BEC := "PP"]
      ##### please check the adjustment_id_2011 macro
      ### it just simply assign adj_id as empty space
      ADJIDData <- treeData[ADJ_ID == " ",]
      ADJIDnaData <- treeData[ADJ_ID != " ",]
      if(nrow(ADJIDnaData) > 0){
        dcy_v3x <- dcy_v3lookuptables(type = "dcy_v3x")
        ADJIDnaData <- dplyr::left_join(ADJIDnaData, dcyV3xTable, by = c("ADJ_ID", "SP0"))
      }
      treeData <- rbind(ADJIDData, ADJIDnaData)
      rm(ADJIDData, ADJIDnaData)
    } else {
      stop("equation is not specified correctly. must be one of KFIZ or KBEC")
    }
    compileLog <- appendedCat(logFileProducer(reason = "Volume multiplier is too big (>1.2) or too small (<0.9), and reset to 1",
                                              action = "no",
                                              displayTable = treeData[VOL_MULT > 1.2 | VOL_MULT < 0.9,],
                                              displayColumn = c("CLSTR_ID", "PLOT", "TREE_NO")), 
                              compileLog)
    treeData[VOL_MULT > 1.2 | VOL_MULT < 0.9, VOL_MULT := 1]
    
    compileLog <- appendedCat(logFileProducer(reason = paste("Plot does not belong to 17 BEC zones, i.e.,", paste(beczones, collapse = ", ")),
                                              action = "no",
                                              displayTable = unique(treeData[!(BGC_ZONE %in% beczones), .(CLSTR_ID, PLOT)], by = c("CLSTR_ID", "PLOT")),
                                              displayColumn = c("CLSTR_ID", "PLOT")),
                              compileLog)
    
    compileLog <- appendedCat(logFileProducer(reason = "For plots in the BEC zone of BAFA, CNA and IMA, BEC was changed to AT",
                                              action = "no",
                                              displayTable = unique(treeData[BGC_ZONE %in% c("BAFA", "CMA", "IMA"),
                                                                             .(CLSTR_ID, PLOT)], by = c("CLSTR_ID", "PLOT")),
                                              displayColumn = c("CLSTR_ID", "PLOT")),
                              compileLog)
    compileLog <- appendedCat(logFileProducer(reason = "For plots in the BEC zone of BG, BEC was changed to PP",
                                              action = "no",
                                              displayTable = unique(treeData[BGC_ZONE %in% c("BG"),
                                                                             .(CLSTR_ID, PLOT)], by = c("CLSTR_ID", "PLOT")),
                                              displayColumn = c("CLSTR_ID", "PLOT")),
                              compileLog)
    
    ## 2. estimate tree height for broken top trees
    treeData[DIAM_BTP > 0, BTOP := "D"] # diameter at broken top
    treeData[HT_PROJ > 0, BTOP := "H"] ## projected height for broken top tree
    compileLog <- appendedCat(paste("Estimate tree height for  broken top trees", 
                                    nrow(treeData[!is.na(BTOP)]), "observations."),
                              compileLog)
    compileLog <- appendedCat(paste("    ", 
                                    nrow(treeData[BTOP == "H",]), "observations have projected height."),
                              compileLog)
    compileLog <- appendedCat(paste("    ", 
                                    nrow(treeData[BTOP == "D",]), "observations have diamter at broken height."),
                              compileLog)
    
    treeData[is.na(BTOP), HT := round(HEIGHT, 1)]
    treeData[!is.na(BTOP), HT_BTOP := round(HEIGHT, 1)]
    compileLog <- appendedCat(logFileProducer(reason = "Estimated tree height for BTOPs with projected height",
                                              action = "no",
                                              displayTable = treeData[BTOP == "H",],
                                              displayColumn = c("CLSTR_ID", "PLOT", "TREE_NO")),
                              compileLog)
    
    treeData[BTOP == "H", HT := round(heightEstimateForBTOP_H(HT_PROJ), 1)]
    treeData[BTOP == "H" & HT_BTOP > HT, HT_BTOP := round(HT, 1)]
    
    treeData[BTOP == "D", DOB_BTOP := round(DIAM_BTP, 2)]
    treeData[DOB_BTOP %>=% DBH, DOB_BTOP := round(DOB_BTOP*0.9, 2)]
    
    
    treeData[!is.na(BTOP) & HT_BTOP %==% 1.3, HT_BTOP := 1.4]
    treeData[!is.na(DOB_BTOP), DIB_BTOP := round(DOB_BTOP/1.07, 2)]
    treeData[, DOB_BTOP := NULL]
    
    treeData[DIB_BTOP %<<% 1.1 & DIB_BTOP %>>% 0, DIB_BTOP := 1.1]
    compileLog <- appendedCat(logFileProducer(reason = "Estimated tree height for BTOPs with diameter at broken",
                                              action = "no",
                                              displayTable = treeData[BTOP == "D",],
                                              displayColumn = c("CLSTR_ID", "PLOT", "TREE_NO")),
                              compileLog)
    treeData[BTOP == "D", HT := round(heightEstimateForBTOP_D(heightBTOP = HT_BTOP,
                                                              taperEquationForm = equation,
                                                              DIBBTOP = DIB_BTOP,
                                                              DBH = DBH,
                                                              FIZorBEC = BEC,
                                                              species = SP0,
                                                              volMultiplier = VOL_MULT),
                                      1)]
    treeData[BTOP == "D" & is.na(HT), BTOP_ESTIMATE_TYPE := 0] # D TREES THAT FAILED TO ESTIMATE TREE HEIGHT
    treeData[BTOP == "D" & !is.na(HT), BTOP_ESTIMATE_TYPE := 1] # D TREES THAT SUCCESS TO ESTIMATE TREE HEIGHT
    treeData[BTOP == "H" & DIB_BTOP > 0, BTOP_ESTIMATE_TYPE := 2] # h TREES THAT HAVE DIAMETER AT BROKEN HEIGHT INFORMATION
    treeData[BTOP == "H" & HT_BTOP > 0, BTOP_ESTIMATE_TYPE := 3] # H TREES THAT HAVE PROJECTED HEIGHT INFORMATION
    
    compileLog <- appendedCat(logFileProducer(reason = "Height can not be estimated for broken top trees",
                                              action = "no",
                                              displayTable = treeData[!is.na(BTOP) & is.na(HT), ],
                                              displayColumn = c("CLSTR_ID", "PLOT", "TREE_NO")),
                              compileLog)
    
    # 3. adjust log for fully-measured trees and enhanced trees
    
    compileLog <- appendedCat("Adjust log length.",
                              compileLog)
    treeData <- logAdjustment(treeData = data.table::copy(treeData), 
                               stumpHeight = stumpHeight)
    compileLog <- appendedCat(logFileProducer(reason = "Log length can not be adjusted",
                                              action = "no",
                                              displayTable = treeData[LOGADJUST == "FAIL", ],
                                              displayColumn = c("CLSTR_ID", "PLOT", "TREE_NO")),
                              compileLog)
    ## adjust log matrix by tree height, minimum log length and default log length
    ## not sure whether this part can be incorporated into logAjustment function, as they are functionally same
    adjustedLogLength <- suppressWarnings(logMatrixAdjustment(logLengthMatrix = treeData[, paste("LOG_L_", 1:9, sep = ""), with = FALSE],
                                             height = treeData$HT,
                                             logMinLength = logMinLength))
    
    ## replace the original log length matrix with adjusted one
    treeData <- cbind(treeData[, paste("LOG_L_", 0:9, sep = "") := NULL],
                       adjustedLogLength)
    

    compileLog <- appendedCat("Calculate tree volume.",
                              compileLog)
    treeVolumes <- treeVolCalculator(FIZorBEC = treeData$BEC,
                                     species = treeData$SP0,
                                     height = treeData$HT,
                                     DBH = treeData$DBH,
                                     taperEquationForm = equation,
                                     volMultiplier = as.numeric(treeData$VOL_MULT),
                                     stumpHeight = stumpHeight, 
                                     breastHeight = breastHeight,
                                     UTOPDIB = UTOPDIB, 
                                     BTOPEstimateType = as.integer(treeData$BTOP_ESTIMATE_TYPE),
                                     BTOPHeight = treeData$HT_BTOP,
                                     BTOPDIB = treeData$DIB_BTOP,
                                     logLengthMatrix = treeData[, paste("LOG_L_", 0:9, sep = ""),
                                                                with = FALSE],
                                     logMinLength = logMinLength)

    treeData <- cbind(treeData, treeVolumes)
    volumenames <- names(treeVolumes)
    rm(treeVolumes)
    NAvolumesForHenhancedtrees <- volumenames[!(volumenames %in% c("HT_STUMP", "DIB_STUMP", "VOL_STUMP", "HT_BH",
                                                  "DIB_BH", "HT_UTOP", "DIB_UTOP", "VOL_WSV",
                                                  "VOL_BELOW_UTOP", "VOL_ABOVE_UTOP", "VOL_BELOW_BTOP",
                                                  "VOL_ABOVE_BTOP"))]
    treeData[MEAS_INTENSE == "H-ENHANCED", c(NAvolumesForHenhancedtrees) := NA]
    compileLog <- appendedCat(logFileProducer(reason = "Volume can not be calculated",
                                              action = "no",
                                              displayTable = treeData[is.na(VOL_WSV), ],
                                              displayColumn = c("CLSTR_ID", "PLOT", "TREE_NO")),
                              compileLog)
    treeData[,':='(VOL_TOP = VOL_ABOVE_UTOP,
                   VOL_MER = VOL_BELOW_UTOP,
                   VOL_BKT = 0,
                   H_MERCH = HT_UTOP)]
    ## the break is above the utop
    treeData[!is.na(BTOP) & VOL_BELOW_UTOP %<<% VOL_BELOW_BTOP, ':='(VOL_BKT = VOL_ABOVE_BTOP,
                                                                  VOL_TOP = VOL_ABOVE_UTOP-VOL_ABOVE_BTOP)]
    ## the break is below the utop
    treeData[!is.na(BTOP) & VOL_BELOW_UTOP %>=% VOL_BELOW_BTOP, ':='(VOL_MER = VOL_BELOW_BTOP,
                                                                     VOL_BKT = VOL_ABOVE_BTOP,
                                                                     VOL_TOP = 0,
                                                                     H_MERCH = HT_BTOP)]
    sasadjust <- FALSE
    if(sasadjust){
      treeData[is.na(LOG_VM_1) & MEAS_INTENSE %in% c("FULL", "ENHANCED"), LOG_VM_1 := 0]
      
      compileLog <- appendedCat(logFileProducer(reason = "Adjust tree volume for BTOP and full/enhanced trees ",
                                                action = "no",
                                                displayTable = treeData[!is.na(BTOP) & MEAS_INTENSE %in% c("FULL", "ENHANCED"), ],
                                                displayColumn = c("CLSTR_ID", "PLOT", "TREE_NO")),
                                compileLog)
      for(i in unique(treeData[!is.na(BTOP) & MEAS_INTENSE %in% c("FULL", "ENHANCED"),]$NO_LOGS)){ ## for btop trees, last log is above btop, therefore log_v and log_vm should be 0
        treeData[!is.na(BTOP) & NO_LOGS == i & MEAS_INTENSE %in% c("FULL", "ENHANCED"), paste("LOG_V_", i, sep = "") := 0]
        treeData[!is.na(BTOP) & NO_LOGS == i & MEAS_INTENSE %in% c("FULL", "ENHANCED"), paste("LOG_VM_", i, sep = "") := 0]
      }
      treeData[!is.na(BTOP) & MEAS_INTENSE %in% c("FULL", "ENHANCED"),
               ':='(ADJUST = "Y",
                    VOL_WSV = rowSums(treeData[!is.na(BTOP) & MEAS_INTENSE %in% c("FULL", "ENHANCED"), paste("LOG_V_", 0:9, sep = ""), 
                                               with = FALSE], na.rm = TRUE),
                    VOL_MER = rowSums(treeData[!is.na(BTOP) & MEAS_INTENSE %in% c("FULL", "ENHANCED"), paste("LOG_VM_", 1:9, sep = ""), 
                                               with = FALSE], na.rm = TRUE))]
      
      
      treeData[!is.na(BTOP) & VOL_BELOW_UTOP %>=% VOL_BELOW_BTOP, 
               tempvolmer := rowSums(treeData[!is.na(BTOP) & VOL_BELOW_UTOP %>=% VOL_BELOW_BTOP,
                                              paste("LOG_VM_", 1:9, sep = ""), with = FALSE], na.rm = TRUE)]
      
      treeData[!is.na(BTOP) & VOL_BELOW_UTOP %>=% VOL_BELOW_BTOP, BT_VOLDIFF := VOL_MER - tempvolmer]
      treeData[, tempvolmer := NULL]
      for(indilogbtop in unique(treeData[!is.na(BTOP) & VOL_BELOW_UTOP %>=% VOL_BELOW_BTOP & MEAS_INTENSE %in% c("FULL", "ENHANCED"),]$LOG_BTOP)){
        treeData[!is.na(BTOP) & VOL_BELOW_UTOP %>=% VOL_BELOW_BTOP & LOG_BTOP == indilogbtop & MEAS_INTENSE %in% c("FULL", "ENHANCED"),
                 paste("LOG_VM_", indilogbtop, sep = "") := treeData[!is.na(BTOP) & VOL_BELOW_UTOP %>=% VOL_BELOW_BTOP & LOG_BTOP == indilogbtop & MEAS_INTENSE %in% c("FULL", "ENHANCED"),
                                                                     paste("LOG_VM_", indilogbtop, sep = ""), with = FALSE]+BT_VOLDIFF]
      }
    } else {
      ## for whole stem volume adjustment for btop trees
      treeData[!is.na(BTOP), VOL_WSV := VOL_BELOW_BTOP + VOL_STUMP]
      treeData[, uniobs := 1:nrow(treeData)]
      ## adjust log volume for btop trees
      needadjustdata <- treeData[!is.na(BTOP) & MEAS_INTENSE %in% c("FULL", "ENHANCED"), ]
      treeData <- treeData[!(uniobs %in% needadjustdata$uniobs), ]
      for(indilogbtop in unique(needadjustdata$LOG_BTOP)){
        needadjustdata[LOG_BTOP == indilogbtop, paste("LOG_V_", (indilogbtop+1):9, sep = "") := as.numeric(NA)]
        needadjustdata[LOG_BTOP == indilogbtop, lowerlogs := rowSums(needadjustdata[LOG_BTOP == indilogbtop, paste("LOG_V_", 1:(indilogbtop-1), sep = ""), 
                                                                                    with = FALSE], na.rm = TRUE)]
        needadjustdata[LOG_BTOP == indilogbtop, paste("LOG_V_", indilogbtop, sep = "") := VOL_BELOW_BTOP - lowerlogs]
        needadjustdata[, lowerlogs := NULL]
      }
      treeData <- rbindlist(list(treeData, needadjustdata))
      rm(needadjustdata)

      ## adjust the merchantable volume for the log for btop trees
      ## for btop higher than utop, nonthing need to do
      ## for btop lower than utop
      needadjustdata <- treeData[!is.na(BTOP) & VOL_BELOW_UTOP %>>% VOL_BELOW_BTOP & 
                                   MEAS_INTENSE %in% c("FULL", "ENHANCED"), ]
      treeData <- treeData[!(uniobs %in% needadjustdata$uniobs), ]
      for(indilogbtop in unique(needadjustdata$LOG_BTOP)){
        needadjustdata[LOG_BTOP == indilogbtop, paste("LOG_VM_", (indilogbtop+1):9, sep = "") := as.numeric(NA)]
        needadjustdata[LOG_BTOP == indilogbtop, lowerlogs := rowSums(needadjustdata[LOG_BTOP == indilogbtop, paste("LOG_VM_", 0:(indilogbtop-1), sep = ""), 
                                                                                    with = FALSE], na.rm = TRUE)]
        needadjustdata[LOG_BTOP == indilogbtop, paste("LOG_VM_", indilogbtop, sep = "") := VOL_MER - lowerlogs]
        needadjustdata[, lowerlogs := NULL]
      }
      treeData <- rbindlist(list(treeData, needadjustdata))
      treeData[, uniobs := NULL]
      rm(needadjustdata)
    }
    
    netFacteredTree <- treeData[MEAS_INTENSE %in% c("FULL", "ENHANCED"),]
    nonnetFacteredTree <- treeData[MEAS_INTENSE %in% c("H-ENHANCED"),]
    compileLog <- appendedCat(paste("Calculate net volume for FULL and ENHANCED trees ", 
                                    nrow(netFacteredTree), " observations.", sep = ""),
                              compileLog)
    netvols <- netVolumeCalculator(grossVolMatrix = netFacteredTree[, paste("LOG_V_", 0:9, sep = ""), with = FALSE],
                                   grossMerchVolMatrix = netFacteredTree[, paste("LOG_VM_", 1:9, sep = ""), with = FALSE],
                                   netFactorMatrix = netFacteredTree[, paste("LOG_S_", 1:9, sep = ""), with = FALSE])
    netFacteredTree <- cbind(netFacteredTree, netvols)
    treeData <- rbindlist(list(netFacteredTree, nonnetFacteredTree), fill = TRUE)
    rm(netFacteredTree, nonnetFacteredTree, netvols)
    
    treeData[, uniObsID := 1:nrow(treeData)]
    gradedTree <- treeData[MEAS_INTENSE %in% c("FULL", "ENHANCED") & !(LOG_G_1 %in% c("*", NA)),]
    nonGradedTree <- treeData[!(uniObsID %in% gradedTree$uniObsID),]
    compileLog <- appendedCat(paste("Calculate value for graded trees ", 
                                    nrow(gradedTree), " observations.", sep = ""),
                              compileLog)
    treevalue <- valueCalculator(species = gradedTree$SPECIES,
                                 grossVolMatrix = gradedTree[, paste("LOG_V_", 0:9, sep = ""), with = FALSE],
                                 grossMerchVolMatrix = gradedTree[, paste("LOG_VM_", 1:9, sep = ""), with = FALSE],
                                 callGradeMatrix = gradedTree[, paste("LOG_G_", 1:9, sep = ""), with = FALSE])
    gradedTree <- cbind(gradedTree, treevalue)
    treeData <- rbindlist(list(gradedTree, nonGradedTree), fill = TRUE)
    compileLog <- appendedCat("End of tree volume calculation for vi_c trees.",
                              compileLog)
    return(list(treeData = treeData[, uniObsID := NULL],
                compileLog = compileLog))
  })


#' @export
#' @rdname VRIVolTree
setMethod(
  "VRIVolTree",
  signature = c(treeData = "data.table",
                equation = "missing",
                logMinLength = "missing",
                stumpHeight = "missing",
                breastHeight = "missing",
                UTOPDIB = "missing"),
  definition = function(treeData){
    return(VRIVolTree(treeData, equation = "KBEC", logMinLength = 0.1,
                      stumpHeight = 0.3, breastHeight = 1.3, UTOPDIB = 10))
  })

