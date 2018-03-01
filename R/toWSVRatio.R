#' Calculate to whole stem volume ratios
#' 
#' 
#' @description This function calculates a bunch of ratio to adjust volume components based on whole stem volume \code{WSV}.
#'              For each combination of project group, live and dead status and species. Specifically,
#'              the function calculates the ratios of whole stem volume (\code{WSV}), net stem volume (\code{NET}), 
#'              merchantable volume (\code{MER}), net merchantable volume (\code{NETM}), 
#'              net volume after waste 2 (\code{NTW2}), 
#'              volume of less top, stemp, crurser decay, waste and breakage (\code{NTWB}),
#'              volume of less top, stump and decay (\code{D}), volume of less top, stump, waste and decay (\code{DW}),
#'              volume of less top, stump, waste, decay and breakage (\code{DWB})
#'              to whole stem volume. The ratios are calculated as the mean of tree with DBH >= 10.
#'              The function is equivalent to \code{calc_ratio_2017.sas}.
#'
#' @param ratioData data.table, The data used to calculated for deriving toWSV ratios.
#' @param minDBH numeric, Defines minimum DBH threshold to select trees in deriving the ratios. If missing 10 cm
#'                        is used.
#' @param minObs numeric, Defined minimum of observations that for each conbination that used in deriving ratios.
#'                        If missing, 3 is used.                               
#'                                                                     
#' @return A ratio table
#' 
#' @importFrom data.table ':='
#' @importFrom fpCompare '%<=%' '%==%' '%>=%' '%!=%' '%>>%' '%<<%'
#'
#' 
#' @export
#' @docType methods
#' @rdname toWSVRatio
#'
#' @author Yong Luo
#'
setGeneric("toWSVRatio",
           function(ratioData, minDBH, minObs) {
             standardGeneric("toWSVRatio")
           })

#' @rdname toWSVRatio
setMethod(
  "toWSVRatio",
  signature = c(ratioData = "data.table",
                minDBH = "numeric",
                minObs = "numeric"),
  definition = function(ratioData, minDBH, minObs){
    # ## i can not see why the ratio adjustment is needed to be associated with regression equations.
    # regressionData <- data.table::copy(reg_all)
    # regressionData <- regressionData[,.(PRJ_GRP, LV_D, SP0, SF_COMPILE, SOURCE, TYPE)]
    volVariables <- c(paste("VOL_",c("WSV", "NET", "MER", "NETM", "NTW2",
                                     "NTWB", "D", "DW", "DWB"),
                            sep = ""), "VAL_MER")
    ## why the last one is included?? val_mer is value not volume
    
    ratioVariables <- paste("RATIO_",c("WSV", "NET", "MER", "NETM", "NTW2",
                                     "NTWB", "D", "DW", "DWB", "VAL"),
                          sep = "")
    
    
    ## after several tests the original sas codes just want to get all the conbinations of 
    ## prj_grp*type*lv_d*sp0*sf_compile, as the count variable will be replaced soon
    ## the below table should be the master table for the final output
    ## is this correct? as this header table is for all trees, while DBH>=10 is small proportion of
    ## whole data
    outputMaster <- unique(ratioData[,.(PRJ_GRP, LV_D, SP0, SF_COMPILE, TYPE)], 
                           by = c("PRJ_GRP", "LV_D", "SP0", "SF_COMPILE"))
    
    ## similar to wsv-ba regression, the first attempt is at prj_grp, lv_d and sp0 level
    
    treeMSdbh10 <- ratioData[DBH %>=% minDBH, 
                          c(volVariables, "PRJ_GRP", "TYPE", "LV_D", "SP0", "SF_COMPILE"),
                          with = FALSE]
    rm(ratioData)
    mean_vol_sp0 <- treeMSdbh10[,c(.N, lapply(.SD, mean)),
                                .SDcols = volVariables,
                                by = c("PRJ_GRP",  "LV_D", "SF_COMPILE", "SP0")]
    setnames(mean_vol_sp0, "N", "COUNT")
    ratio_sp0 <- mean_vol_sp0[COUNT %>>% minObs, ]
    outputMaster <- merge_dupUpdate(x = outputMaster,
                                     y = ratio_sp0,
                                     by = c("PRJ_GRP",  "LV_D", "SF_COMPILE", "SP0"),
                                     all.x = TRUE)
    need_ratio <- outputMaster[is.na(COUNT),]
    outputMaster <- outputMaster[!is.na(COUNT),][, SOURCE := "Sp0"]
    rm(mean_vol_sp0)
    
    if(nrow(need_ratio) > 0){
      # second priority is given to type if number of obs is less than or equal 2 at species level
      mean_vol_type <- treeMSdbh10[,c(.N, lapply(.SD, mean)),
                                   .SDcols = volVariables,
                                   by = c("PRJ_GRP",  "LV_D", "SF_COMPILE", "TYPE")]
      setnames(mean_vol_type, "N", "COUNT")
      ratio_type <- mean_vol_type[COUNT %>>% minObs, ]
      tempoutput <- merge_dupUpdate(x = need_ratio[,.(PRJ_GRP, TYPE, LV_D, SP0, SF_COMPILE)],
                                     y = ratio_type,
                                     by = c("PRJ_GRP",  "LV_D", "SF_COMPILE", "TYPE"),
                                     all.x = TRUE)
      need_ratio <- tempoutput[is.na(COUNT),]
      ratio_type <- tempoutput[!is.na(COUNT),][, SOURCE := "Type"]
      outputMaster <- rbind(outputMaster,
                            ratio_type[, names(outputMaster), with = FALSE])
      rm(mean_vol_type, ratio_type, tempoutput)
      
      if(nrow(need_ratio) > 0){
        ## one more attempt if the ratio have not been derived for all the rows in master table
        ## using combination of prj_grp lv_d and sf_compile
        mean_vol_lv_d <- treeMSdbh10[,c(.N, lapply(.SD, mean)),
                                     .SDcols = volVariables,
                                     by = c("PRJ_GRP",  "LV_D", "SF_COMPILE")]
        setnames(mean_vol_lv_d, "N", "COUNT") 
        ratio_lv_d <- mean_vol_lv_d[COUNT %>>% minObs, ]
        tempoutput <- merge_dupUpdate(x = need_ratio[,.(PRJ_GRP, TYPE, LV_D, SP0, SF_COMPILE)],
                                       y = ratio_lv_d,
                                       by = c("PRJ_GRP",  "LV_D", "SF_COMPILE"),
                                       all.x = TRUE)
        need_ratio <- tempoutput[is.na(COUNT),]
        ratio_lv_d <- tempoutput[!is.na(COUNT),][, SOURCE := "Lv_D"]
        if(nrow(need_ratio) > 0){
          need_ratio[, SOURCE := as.character(NA)]
          outputMaster <- rbind(outputMaster,
                                ratio_lv_d[, names(outputMaster), with = FALSE],
                                need_ratio[, names(outputMaster), with = FALSE])
          warning("After three attempts in deriving toWSV ratio, still some combination failed.")
        } else {
          outputMaster <- rbind(outputMaster,
                                ratio_lv_d[, names(outputMaster), with = FALSE])
        }
        rm(mean_vol_lv_d, ratio_lv_d, tempoutput)
      }
    }
    
    for(i in 1:length(ratioVariables)){
      outputMaster[, ratioVariables[i] := unlist(outputMaster[, volVariables[i], with = FALSE])/VOL_WSV]
    }
    outputMaster <- unique(outputMaster, 
                           by = c("PRJ_GRP", "LV_D", "SF_COMPILE", "SP0", "TYPE"))
    outputMaster[RATIO_NET %>>% 1, RATIO_NET := 1]
    if(nrow(outputMaster[RATIO_WSV %!=% 1,]) > 0){
      stop("RATIO_WSV must be 1.")
    }
    if(nrow(outputMaster[RATIO_WSV %<<% RATIO_NET,]) > 0){
      stop("RATIO_WSV must be bigger than or equal to RATIO_NET.")
    }
    for(i in 3:6){ ## for ratio_wsv, ratio_mer, ratio_netm, ratio_ntw2 and ratio_ntwb check
      if(i == 3){
        outputMaster[, ':='(checkvalue1 = RATIO_WSV,
                            checkvalue2 = unlist(outputMaster[, ratioVariables[i], with = FALSE]))]
      } else {
        outputMaster[, ':='(checkvalue1 = unlist(outputMaster[, ratioVariables[i-1], with = FALSE]),
                            checkvalue2 = unlist(outputMaster[, ratioVariables[i], with = FALSE]))]
      }
      if(nrow(outputMaster[checkvalue2 %>>% checkvalue1]) > 0){
        if(i == 3){
          stop("Must be RATIO_WSV >= RATIO_MER.")
        } else {
          stop(paste("Must be ", ratioVariables[i-1], " >= ", ratioVariables[i], ".", sep = ""))
        }
      }
    }
    for(i in 7:9){ ## for ratio_mer, ratio_d, ratio_dw and ratio_dwb check
      if(i == 7){
        outputMaster[, ':='(checkvalue1 = unlist(outputMaster[, ratioVariables[3], with = FALSE]), # ratio_mer
                            checkvalue2 = unlist(outputMaster[, ratioVariables[i], with = FALSE]))]
      } else {
        outputMaster[, ':='(checkvalue1 = unlist(outputMaster[, ratioVariables[i-1], with = FALSE]),
                            checkvalue2 = unlist(outputMaster[, ratioVariables[i], with = FALSE]))]
      }
      if(nrow(outputMaster[checkvalue2 %>>% checkvalue1]) > 0){
        if(i == 7){
          stop("Must be RATIO_MER >= RATIO_D.")
          
        } else {
          stop(paste("Must be ", ratioVariables[i-1], " >= ", ratioVariables[i], ".", sep = ""))
        }
      }
    }
    # ## CHECK AND ADJUST RATIO
    # 
    # outputMaster[]
    # 
    # outputMaster[RATIO_NETM %>>% RATIO_MER, RATIO_NETM := RATIO_MER]
    # outputMaster[RATIO_NTW2 %>>% RATIO_NETM, RATIO_NTW2 := RATIO_NETM]
    return(outputMaster[,c("PRJ_GRP", "LV_D", "SF_COMPILE", "SP0",
                           "TYPE", "COUNT", "SOURCE", ratioVariables),
                        with = FALSE])
  })


#' @export 
#' @rdname toWSVRatio
setMethod(
  "toWSVRatio",
  signature = c(ratioData = "data.table",
                minDBH = "missing",
                minObs = "numeric"),
  definition = function(ratioData, minObs){
    return(toWSVRatio(ratioData, minDBH = 10, minObs))
  })

#' @export 
#' @rdname toWSVRatio
setMethod(
  "toWSVRatio",
  signature = c(ratioData = "data.table",
                minDBH = "numeric",
                minObs = "missing"),
  definition = function(ratioData, minDBH){
    return(toWSVRatio(ratioData, minDBH, minObs = 3))
  })

#' @export 
#' @rdname toWSVRatio
setMethod(
  "toWSVRatio",
  signature = c(ratioData = "data.table",
                minDBH = "missing",
                minObs = "missing"),
  definition = function(ratioData){
    return(toWSVRatio(ratioData, minDBH = 10, minObs = 3))
  })