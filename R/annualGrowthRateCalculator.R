#' Calculate annual growth rate
#' 
#' 
#' @description This function is to calcualte annual growth rate. 
#'
#' @param boredDiameter numeric, Diameter at bored height in cm.
#' @param growthIncrement numeric, Growth increment in mm over a time period.
#' @param growthYear numeric, Number of years over which growth increment is measured.
#' @param barkThickness numeric, Bark thickness in mm. If missing, 0.05 will be used.
#' 
#' 
#' @return Calculated annual growth rate.
#' 
#' @importFrom data.table ':='
#'
#' 
#' @export
#' @docType methods
#' @rdname annualGrowthRateCalculator
#'
#' @author Yong Luo
#'
setGeneric("annualGrowthRateCalculator",
           function(boredDiameter, growthIncrement, growthYear, barkThickness) {
             standardGeneric("annualGrowthRateCalculator")
           })

#' @rdname annualGrowthRateCalculator
setMethod(
  "annualGrowthRateCalculator",
  signature = c(boredDiameter = "numeric", 
                growthIncrement = "numeric", 
                growthYear = "numeric",
                barkThickness = "numeric"),
  definition = function(boredDiameter, growthIncrement, growthYear, barkThickness){
    worktable <- data.table(uniObs = 1:length(boredDiameter),
                            boredDiameter, 
                            barkThickness, 
                            growthIncrement, 
                            growthYear)
    if(nrow(worktable[is.na(barkThickness)]) > 0){
      worktable[is.na(barkThickness), barkThickness := 0.05]
      warning("Bark thickness is missing, 0.05 was used.")
    }
    if(nrow(worktable[barkThickness < 0 | growthIncrement < 0,]) > 0){
      stop("Bark thickness and growth increment must be positive values.")
    }
    worktable[, radius := boredDiameter/2-barkThickness/10]
    worktable[, R := radius - growthIncrement/10]
    worktable[R > -0.001, R := abs(R)]
    worktable[R > 0 & growthIncrement > 0, growthRate := exp(log((radius^2)/(R^2))/growthYear) - 1]
    return(worktable[order(uniObs),]$growthRate)
  })


#' @export
#' @rdname annualGrowthRateCalculator
setMethod(
  "annualGrowthRateCalculator",
  signature = c(boredDiameter = "numeric", 
                growthIncrement = "numeric", 
                growthYear = "numeric",
                barkThickness = "missing"),
  definition = function(boredDiameter, growthIncrement, growthYear){
    warning("Bark thickness is missing, 0.05 was used.")
    return(annualGrowthRateCalculator(boredDiameter, growthIncrement, growthYear, barkThickness = 0.05))
  })
