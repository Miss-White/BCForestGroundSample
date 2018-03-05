#' Compile breast age, total age, and site index where possible-VRI specific
#'
#'
#' @description This function takes site age tree data ie., \code{vi_h}, an output of \code{\link{VRIInit_siteTree}} to
#'              compute the breast height age, total age, and site index where possible. This function is
#'              equivalent to site_age.sas. The function heavily depends on site tools program.
#'
#' @param siteAgeData data.table, Site age data with plot header information.
#'                                An output from \code{\link{VRIInit_siteTree}} function.
#' @param siteToolsDLLPath character, Path to \code{SINDEX33.DLL}.
#' @param sasExePath character, Path to sas executable, i.e., \code{sas.exe}. If missing, the function takes
#'                   \code{C:/Program Files/SASHome/x86/SASFoundation/9.3} as default. However, it will cause crush
#'                   if sas executable is not located in default path.
#' @return A data table and a log file.
#'
#' @importFrom data.table ':='
#' @importFrom dplyr left_join
#' @importFrom fpCompare '%<=%' '%==%' '%>=%' '%!=%' '%>>%' '%<<%'
#' @importFrom SIndexR SIndexR_SpecRemap
#'
#' @export
#' @docType methods
#' @rdname siteAgeCompiler
#'
#' @author Yong Luo
#'
setGeneric("siteAgeCompiler",
           function(siteAgeData,
                    siteToolsDLLPath, sasExePath) {
             standardGeneric("siteAgeCompiler")
           })

#' @rdname siteAgeCompiler
setMethod(
  "siteAgeCompiler",
  signature = c(siteAgeData = "data.table",
                siteToolsDLLPath = "character",
                sasExePath = "character"),
  definition = function(siteAgeData,
                        siteToolsDLLPath, sasExePath){
    displaycol <- c("CLSTR_ID", "PLOT", "TREE_NO")
    siteAgeData <- cbind(data.table(uniObs = 1:nrow(siteAgeData)),
                         siteAgeData)
    compileLog <- appendedCat("Start to compile site age tree data.")
    siteAgeData[, BARK_TEMP := as.numeric(BARK_THK)]
    siteAgeData[is.na(BARK_THK), BARK_TEMP := BNG_DIAM/20]
    compileLog <- appendedCat(logFileProducer(reason = "Bark thickness was missing, was calculated based on diameter at bore height",
                                              action = "no",
                                              displayTable = siteAgeData[is.na(BARK_THK)],
                                              displayColumn = displaycol),
                              compileLog)
    siteAgeData[BARK_THK %>>% 0 & BNG_DIAM %>>% 0, BARK_PCT := 100*((BARK_THK*0.2)/BNG_DIAM)]
    ## CALL annualGrowthRataCalculator function
    siteAgeData[!is.na(BNG_DIAM), ':='(RATE_5 = annualGrowthRateCalculator(boredDiameter = BNG_DIAM,
                                                                           growthIncrement = GROW_5YR,
                                                                           growthYear = 5,
                                                                           barkThickness = BARK_TEMP),
                                       RATE_10 = annualGrowthRateCalculator(boredDiameter = BNG_DIAM,
                                                                            growthIncrement = GROW_10Y,
                                                                            growthYear = 10,
                                                                            barkThickness = BARK_TEMP),
                                       RATE_20 = annualGrowthRateCalculator(boredDiameter = BNG_DIAM,
                                                                            growthIncrement = GROW_20Y,
                                                                            growthYear = 20,
                                                                            barkThickness = BARK_TEMP))]
    siteAgeData[BARK_THK %>>% 0 & BNG_DIAM %>>% 0,
                BARK_PCT := 100*(BARK_THK*0.2)/BNG_DIAM]

    siteAgeData[, BEC_I_C := BEC2IC(BEC = BGC_ZONE)]
    siteAgeData[BEC_I_C == "?", BEC_I_C := "I"]
    siteAgeData[, SP_FRED := siteToolsSpeciesConvertor(species = SPECIES)]

    compileLog <- appendedCat("Derive species/FIZ-specific site index using siteTools.",
                              compileLog)
    siteAgeData[, SI_SP := SIndexR::SIndexR_SpecRemap(SP_FRED, BEC_I_C)]
    compileLog <- appendedCat(logFileProducer(reason = "Species/FIZ site index problem",
                                              action = "no",
                                              displayTable = siteAgeData[SI_SP < 0,],
                                              displayColumn = displaycol),
                              compileLog)

    siteAgeData[, ':='(HT_OLD = HEIGHT)]
    siteAgeData[HEIGHT %<=% 1.3, ':='(HEIGHT = 1.31)]

    ## assign bored age, first part of the age-ind.sas

    ## need check with rene to make sure the order makes sense
    compileLog <- appendedCat("Derive bored age for each tree.",
                              compileLog)
    siteAgeData[, HT_CALC := BORED_HT]
    siteAgeData[!(BORE_AGE %in% c(NA, 0)),
                ':='(AGE_BOR = boredAgeCalculator_Bore(officeBoredAge = as.numeric(BORE_AGE)),
                     AGE_BASE = "Bore")]
    siteAgeData[is.na(AGE_BASE) & !(BORAG_FL %in% c(NA, 0)),
                ':='(AGE_BOR = boredAgeCalculator_Bore(fieldBoredAge = as.numeric(BORAG_FL)),
                     AGE_BASE = "Bore")]
    siteAgeData[is.na(AGE_BASE) & !(TOTAL_AG %in% c(NA, 0)) & is.na(AGE_BASE),
                ':='(AGE_BOR = boredAgeCalculator_Total(as.numeric(TOTAL_AG)),
                     AGE_BASE = "Total")]
    siteAgeData[is.na(AGE_BASE) & !(PHYS_AGE %in% c(NA, 0)),
                ':='(AGE_BOR = boredAgeCalculator_Phys(as.numeric(PHYS_AGE)),
                     AGE_BASE = "Phys")]
    siteAgeData[PRO_LEN %>>% 0 & PRO_RING %>>% 0,
                ':='(AGE_BOR = boredAgeCalculator_Prorated(ringLength_prorated = PRO_LEN,
                                                           ringCount_prorated = PRO_RING,
                                                           boreDiameter = BNG_DIAM,
                                                           barkThickness = BARK_TEMP),
                     AGE_BASE = "Pro")]
    compileLog <- appendedCat(logFileProducer(reason = " Office/field bored age is used",
                                              action = "no",
                                              displayTable = siteAgeData[AGE_BASE == "Bore",],
                                              displayColumn = displaycol),
                              compileLog)
    compileLog <- appendedCat(logFileProducer(reason = " Total age is used",
                                              action = "no",
                                              displayTable = siteAgeData[AGE_BASE == "Total",],
                                              displayColumn = displaycol),
                              compileLog)
    compileLog <- appendedCat(logFileProducer(reason = " Physiological age is used",
                                              action = "no",
                                              displayTable = siteAgeData[AGE_BASE == "Phys",],
                                              displayColumn = displaycol),
                              compileLog)
    compileLog <- appendedCat(logFileProducer(reason = " Pro-rated age is used",
                                              action = "no",
                                              displayTable = siteAgeData[AGE_BASE == "Pro",],
                                              displayColumn = displaycol),
                              compileLog)

    compileLog <- appendedCat(logFileProducer(reason = "Adjust bored age for bored height is not at breast height",
                                              action = "no",
                                              displayTable = siteAgeData[HT_CALC %!=% 1.3 & HT_CALC %!=% 0,],
                                              displayColumn = displaycol),
                              compileLog)
    ## call boredagecalculator_crted
    siteAgeData[HT_CALC %!=% 1.3 & HT_CALC %!=% 0,
                ':='(AGE_BOR = boredAgeCalculator_Crted(boredAge = AGE_BOR,
                                                        boredHeight = HT_CALC,
                                                        treeHeight = HEIGHT,
                                                        species = SP_FRED,
                                                        ICRegion = BEC_I_C,
                                                        siteToolsDLLPath = siteToolsDLLPath,
                                                        sasExePath = sasExePath),
                     HT_CALC = 1.3,
                     AGE_BASE = "Bh_cr")]

    siteAgeData[, ':='(CORR = 0,
                       AGE_TOT = as.numeric(NA),
                       AGE_BH = as.numeric(NA),
                       SI_TREE = as.numeric(NA))]
    treatdata <- siteAgeData[AGE_BOR %>>% 0 & HEIGHT %>>% 0 & SI_SP %>=% 0, ]
    untreatdata <- siteAgeData[!(uniObs %in% treatdata$uniObs),]
    compileLog <- appendedCat(logFileProducer(reason = "Derive total age, breast age and site index for each tree using siteTools",
                                              action = "no",
                                              displayTable = treatdata,
                                              displayColumn = displaycol),
                              compileLog)
    treatdata[, AGE_TP := 1]
    treatdata[, SI_TREE := SiteTools_HTBoredAge2SI(boredAge = AGE_BOR, height = HEIGHT,
                                                   species = SP_FRED, ICRegion = BEC_I_C,
                                                   ageType = AGE_TP, estimateMethod = 1,
                                                   siteToolsDLLPath = siteToolsDLLPath,
                                                   sasExePath = sasExePath)]
    treatdata[, CORR := SiteTools_Y2BH(species = SP_FRED, ICRegion = BEC_I_C, siteIndex = SI_TREE,
                                       siteToolsDLLPath = siteToolsDLLPath,
                                       sasExePath = sasExePath)]
    treatdata[HT_CALC %==% 0, ':='(AGE_TOT = AGE_BOR)]
    treatdata[HT_CALC %==% 0, ':='(AGE_BH = AGE_TOT - CORR)]
    treatdata[HT_CALC %==% 0 & AGE_BH %<<% 0, ':='(AGE_BH = 0)]
    treatdata[HT_CALC %!=% 0, ':='(AGE_BH = AGE_BOR)]
    treatdata[HT_CALC %!=% 0, ':='(AGE_TOT = CORR + AGE_BH)]
    siteAgeData <- rbindlist(list(treatdata[, ':='(AGE_TP = NULL)], untreatdata))
    rm(treatdata, untreatdata)
    siteAgeData[, HEIGHT := HT_OLD]
    siteAgeData[, ':='(HT_OLD = NULL)]
    siteAgeData[SUIT_HT == "N" | SUIT_TR == "N", SI_TREE := as.numeric(NA)]
    siteAgeData <- siteAgeData[order(uniObs),.(CLSTR_ID, PLOT, TREE_NO, BGC_ZONE, SPECIES,
                                               TH_TREE, RA_TREE, TP_TREE, SUIT_TR, SUIT_HT,
                                               MEAS_COD, AGE_BASE, SP0,
                                               GROW_5YR, GROW_10Y, GROW_20Y, AGE_CORR, TOTAL_AG, PHYS_AGE,
                                               CR_CL, TREE_LEN, HEIGHT, BNG_DIAM, BARK_THK, PRO_LEN,
                                               PRO_RING, BORE_AGE, BORED_HT, SI_SP, AGE_BH, AGE_TOT, SI_TREE,
                                               BARK_PCT, RATE_5, RATE_10, RATE_20, SP_FRED)]
    compileLog <- appendedCat("End of site age tree compilation.",
                              compileLog)
    return(list(compiledData = siteAgeData, compileLog = compileLog))
  })


#' @export
#' @rdname siteAgeCompiler
setMethod(
  "siteAgeCompiler",
  signature = c(siteAgeData = "data.table",
                siteToolsDLLPath = "character",
                sasExePath = "missing"),
  definition = function(siteAgeData, siteToolsDLLPath){
    return(siteAgeCompiler(siteAgeData, siteToolsDLLPath,
                           sasExePath = "C:/Program Files/SASHome/x86/SASFoundation/9.3"))
  })
