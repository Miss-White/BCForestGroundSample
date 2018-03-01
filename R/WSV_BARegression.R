#' Create regression equation to link whole stem volume and basal area in VRI data-VRI specific
#' 
#' 
#' @description This function develops regression equations between whole stem volume (\code{WSV}) and basal area (\code{BA})
#'              for both alive and dead trees. The equation is derived at project group and species level. When sample size is small 
#'              or the model's performance is poor for a given species, the species type, i.e., deciduous or coniferious species, is
#'              used as this species in developing the model. The function is equivalent to \code{reg_wsv_2017.sas}.
#'              To estimate tree volume for non-enhanced trees, the resultant equations will be used. 
#'
#' @param regressionData numeric, Specifies whole stem volume.
#' @param minObs numeric, Defines the minimum number of observations (included) that used for the regression.
#'                        If missing, 3 is used to be consistent with the compilation manual.
#' @param minR2 numeric, Defines the minimum R square to identify the models goodness of fit.
#'                       If missing, 0.3 is used.
#'                                                                     
#' @return A data table that contains key statistics of whole stem volume and basal area regression.
#' 
#' @importFrom data.table ':='
#' @importFrom fpCompare '%<=%' '%==%' '%>=%' '%!=%' '%>>%' '%<<%'
#'
#' 
#' @export
#' @docType methods
#' @rdname WSV_BARegression
#'
#' @author Yong Luo
#'
setGeneric("WSV_BARegression",
           function(regressionData, minObs, minR2) {
             standardGeneric("WSV_BARegression")
           })

#' @rdname WSV_BARegression
setMethod(
  "WSV_BARegression",
  signature = c(regressionData = "data.table",
                minObs = "numeric",
                minR2 = "numeric"),
  definition = function(regressionData, minObs, minR2){
    outputHeader <- unique(regressionData[,.(PRJ_GRP, LV_D, SP0, TYPE)],
                           by = c("PRJ_GRP", "LV_D", "SP0"))
    outputHeader[, combination := paste(PRJ_GRP, LV_D, SP0, sep = " & ")]
    ## first attempt at prj_grp lv_d and sp0 level 
    allmodels <- lm_group(formula = "L10WSV~L10BA", 
                          data = regressionData, 
                          groupBy = c("PRJ_GRP", "LV_D", "SP0"))
    reg_sp0 <- wsv_baRegSummary(allmodels)
    # select models that fullfills the conditions
    reg_sp0 <- reg_sp0[COUNT %>>% minObs & EDF > 0 & RSQ %>=% minR2, ][, SOURCE := "Sp0"]
    rm(allmodels)
    outputHeader <- merge_dupUpdate(outputHeader, reg_sp0, by = "combination", all.x = TRUE)
    output <- outputHeader[!is.na(RSQ), ]
    outputHeader <- outputHeader[is.na(RSQ),.(PRJ_GRP, LV_D, SP0, TYPE)]
    
    
    if(nrow(outputHeader) > 0){
      ## second attempt at prj_grp lv_d and type level
      outputHeader[, combination := paste(PRJ_GRP, LV_D, TYPE, sep = " & ")]
      allmodels <- lm_group(formula = "L10WSV~L10BA", 
                            data = regressionData, 
                            groupBy = c("PRJ_GRP", "LV_D", "TYPE"))
      reg_type <- wsv_baRegSummary(allmodels)
      ## select models
      rep_type <- reg_type[COUNT %>>% minObs & EDF > 0 & RSQ %>=% minR2, ][, SOURCE := "Type"]
      outputHeader <- merge_dupUpdate(outputHeader, rep_type, by = "combination",
                            all.x = TRUE)
      tempoutput <- outputHeader[!is.na(RSQ),]
      output <- rbindlist(list(output, 
                               tempoutput[,names(output), with = FALSE]))
      outputHeader <- outputHeader[is.na(RSQ),]
      rm(allmodels, tempoutput)
      if(nrow(outputHeader) > 0){
        ## one more attempt to derive regression based on prj_grp and lv_d
        outputHeader[, combination := paste(PRJ_GRP, LV_D, sep = " & ")]
        allmodels <- lm_group(formula = "L10WSV~L10BA", 
                              data = regressionData, 
                              groupBy = c("PRJ_GRP", "LV_D"))
        reg_lv_d <- wsv_baRegSummary(allmodels)
        ## select models
        reg_lv_d <- reg_lv_d[COUNT %>>% minObs & EDF > 0 & RSQ %>=% minR2, ][, SOURCE := "Lv_D"]
        outputHeader <- merge_dupUpdate(outputHeader, reg_lv_d, by = "combination",
                              all.x = TRUE)
        tempoutput <- outputHeader[!is.na(RSQ),]
        output <- rbindlist(list(output, 
                                 tempoutput[,names(output), with = FALSE]))
        outputHeader <- outputHeader[is.na(RSQ),]
        rm(allmodels, tempoutput)
      }
    }
    output[, ':='(combination = NULL)]
    output[LV_D %in% c("L", "D"), SF_COMPILE := "S"]
    return(output[order(PRJ_GRP, LV_D, SP0)])
  })

#' @export
#' @rdname WSV_BARegression
setMethod(
  "WSV_BARegression",
  signature = c(regressionData = "data.table",
                minObs = "missing",
                minR2 = "numeric"),
  definition = function(regressionData, minR2){
    return(WSV_BARegression(regressionData, minObs = 3, minR2))
  })

#' @export
#' @rdname WSV_BARegression
setMethod(
  "WSV_BARegression",
  signature = c(regressionData = "data.table",
                minObs = "numeric",
                minR2 = "missing"),
  definition = function(regressionData, minObs){
    return(WSV_BARegression(regressionData, minObs, minR2 = 0.3))
  })

#' @export
#' @rdname WSV_BARegression
setMethod(
  "WSV_BARegression",
  signature = c(regressionData = "data.table",
                minObs = "missing",
                minR2 = "missing"),
  definition = function(regressionData){
    return(WSV_BARegression(regressionData, minObs = 3, minR2 = 0.3))
  })


wsv_baRegSummary <- function(modellist){
  modeloutputlist <- lapply(modellist, function(s) data.table(COUNT = length(s$residuals), 
                                                              MODEL = "WSV", 
                                                              INTERCEPT = s$coefficients[1],
                                                              L10BA = s$coefficients[2], 
                                                              EDF = s$df.residual, 
                                                              RSQ = summary(s)$r.squared,
                                                              MSE = mean(s$residuals^2)))
  for(i in names(modeloutputlist)){
    if(i == names(modeloutputlist)[1]){
      output <- cbind(data.table(combination = i),
                      modeloutputlist[[i]])
    } else {
      output <- rbind(output,
                      cbind(data.table(combination = i),
                            modeloutputlist[[i]]))
    }
  }
  return(output)
}

