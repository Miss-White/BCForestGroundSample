#' VRI compiler - VRI specific
#'
#'
#' @description This function compiles VRI data by calling specific VRI functions. Unlike the original
#'              compiler (i.e., SAS compiler), the R version compiler hardcodes all the lookup tables in
#'              the compilation process. Please refer the descriptions for lookup table to see whether
#'              they are same as the original lookup table.
#'
#' @param dataSourcePath character, Specifies the path that directs to the VRI original data soruce.
#' @param outputPath character, Specifies the folder to save all the outputs.
#' @param equation character, Specifies the taper equation that is used for compiler. Currently supports
#'                            BEC-based (\code{KBEC}) and FIZ-based (\code{KFIZ}).
#' @param walkThru logical, Speciefies whether the data had been collected using work through method. Default is \code{TRUE},
#'                          if it is not specified.
#' @param logMinLength numeric, Specifies minimum length of log when doing log length adjustment,
#'                              see \code{\link{logMatrixAdjustment}} for details. If missing 0.1 is used.
#' @param stumpHeight numeric, Stump height. If missing 0.3 is used.
#' @param breastHeight numeric, Breast height. If missing 1.3 is used.
#' @param UTOPDIB numeric, Threshold inside-bark diameter for merchantable volume. If missing, UTOPDIB is 10.
#' @param utilLevel numeric, Specifies utilization level in summrizing tree volumes at cluster and species level. Default is 4.
#' @param weirdUtil character, Specifies weird utilization in summarizing tree volumes at cluster and species level.
#'                             Default is \code{no}, if missing. Otherwise, a number should be provided.
#' @param siteToolsDLLPath character, Path to SINDEX33.DLL.
#' @param sasExePath character, Path to sas executable, i.e., sas.exe. If missing,
#'                              the function takes \code{C:/Program Files/SASHome/x86/SASFoundation/9.3 as default}.
#'                              However, it will cause crush if sas executable is not located in default path.
#'
#' @return This function compiles data and save outputs in \code{outputPath} and no file is returned.
#'
#' @importFrom data.table ':='
#' @importFrom fpCompare '%<=%' '%==%' '%>=%' '%!=%' '%>>%' '%<<%'
#' @importFrom haven read_sas
#' @references VRI compiler manual
#' @note
#'  Improvements include:
#'  \enumerate{
#'  \item specifies trees in B plots as height enhanced trees
#'  \item removes the sas-dependent sindex functions
#'  \item introduce the SIndexR package
#'  }
#'  Currently, the compiler supports compilation for the below sample types:
#'  \itemize{
#'  \item{\code{Q: }} {Regular VRI sample with five point clusters design}
#'  \item{\code{T: }} {??, sample protocol and sample design are same as Q samples}
#'  \item{\code{B: }} {??, same plot layout as Q sample, with height is measured for all trees and
#'  no call grading information}
#'  \item{\code{M: }} {CMI sample, with all trees have call grading information in the field}
#'  \item{\code{L: }} {LiDAR project, same plot layout and same design but without call grading information}
#'  \item{\code{Y: }} {YSM plots, population between 15 and 50 years}
#'  \item{\code{F: }} {NFI plots, trees measured all DBH, height and call grading}
#'  \item{\code{N: }} {NVAF plots}
#'  \item{\code{A: }} {VRI audit plots}
#'  }
#' @export
#' @docType methods
#' @rdname VRICompiler
#'
#' @author Yong Luo
#'

VRICompiler <- function(dataSourcePath,
                        outputPath = ".",
                        equation = "KBEC",
                        walkThru = TRUE,
                        logMinLength = 0.1,
                        stumpHeight = 0.3,
                        breastHeight = 1.3,
                        UTOPDIB = 10,
                        utilLevel = 4,
                        weirdUtil = "No",
                        siteToolsDLLPath,
                        sasExePath = "C:/Program Files/SASHome/x86/SASFoundation/9.3"){
  ### 1. setup output folder
  outputPath <- compilerOutputSetup(outputPath)
  compileLog <- appendedCat(paste("VRI compiler will save all outputs in ", outputPath, sep = ""))

  ### 2. load all data for compilation
  ### 2.0 lookup table check
  lookuptables <- c("vri_grp", "vri_bec", "spv_spc", "sp_cost", "spv_frd", "sp_type",
                    "dcy_v3", "dcy_v3x", "brk_99", "wst_v3")
  for(inditable in lookuptables){
    indilog <- lookupCheck(inditable)
    cat("\n\n", indilog,
        file = file.path(outputPath, "compilerLog.txt"), sep = "")
  }
  rm(lookuptables, indilog, inditable)
  ### 2.1 load cluster/plot header
  compileLog <- appendedCat(paste(rep("*", 60), collapse = ""),
                            compileLog)
  compileLog <- appendedCat("VRI compiler starts to load all the input data.",
                            compileLog)
  cat(compileLog,
      file = file.path(outputPath, "compilerLog.txt"), append = TRUE, sep = "")
  clusterplotheader_VRI <- VRIInit_clusterplot(dataSourcePath = dataSourcePath)
  samples <- data.table::copy(clusterplotheader_VRI$clusterPlotHeader)
  haidaproj <- FALSE
  if(haidaproj){
    samples <- samples[(PRJ_GRP == "Haida Gwaii" & PROJ_ID %in% c("0251", "0252", "025Y")),]
  }

  compileLog <- appendedCat("Save cluster/plot information to samples.csv",
                            clusterplotheader_VRI$compilerLog)
  write.csv(samples, file.path(outputPath, "samples.csv"), row.names = FALSE)
  cat("\n\n", compileLog,
      file = file.path(outputPath, "compilerLog.txt"), append = TRUE, sep = "")
  rm(clusterplotheader_VRI)
  ### 2.2 load vi_c data
  ## vi_c contains the trees of: 1) fully measured trees in IPC (trees have dbh, height and call grading)
  ##                             2) enhanced trees in auxi plots (trees have dbh, height and call grading)
  ##                             3) H-enhanced trees in auxi plots (trees have dbh, height)
  ##                             4) B-sample trees in fixed area lidar projects (trees have dbh, height)
  vi_c <- VRIInit_measuredTree(data.table::copy(samples),
                                           dataSourcePath,
                                           walkThru)
  tree_ms1 <- vi_c$treeData
  cat("\n\n", vi_c$compileLog,
      file = file.path(outputPath, "compilerLog.txt"), append = TRUE, sep = "")
  rm(vi_c)

  ### 2.3 load vi_d data
  ## vi_d contains call grading data for fully measured trees and enhanced trees
  vi_d <- VRIInit_lossFactor(fullMeasuredTrees = tree_ms1[,.(CLSTR_ID, PLOT, TREE_NO)],
                             dataSourcePath = dataSourcePath)
  cat("\n\n", vi_d$compileLog,
      file = file.path(outputPath, "compilerLog.txt"), append = TRUE, sep = "")
  vi_d <- vi_d$treeLossFactor

  ### 2.4 load vi_i data
  ## vi_i has trees in auxi plots without height information (mostly), however, some of these trees are also in vi_c
  vi_i <- VRIInit_auxTree(data.table::copy(samples),
                                 dataSourcePath)
  tree_ax1 <- vi_i$auxiPlotTree
  cat("\n\n",   vi_i$compileLog,
      file = file.path(outputPath, "compilerLog.txt"), append = TRUE, sep = "")
  rm(vi_i)

  ### 2.5 load vi_h data
  ## vi_h data is the site age trees
  vi_h <- VRIInit_siteTree(data.table::copy(samples),
                                   dataSourcePath)
  tree_ah1 <- vi_h$siteTreeData
  cat("\n\n", vi_h$compileLog,
      file = file.path(outputPath, "compilerLog.txt"), append = TRUE, sep = "")
  rm(vi_h)



  ### 3. vi_c compilation
  compileLog <- appendedCat(paste(rep("*", 60), collapse = ""))
  compileLog <- appendedCat("Start to compile vi_c.",
                            compileLog)
  compileLog <- appendedCat(paste("Assign measurement intensity for trees in vi_c (",
                                  nrow(tree_ms1), " observations).", sep = ""),
                            compileLog)
  tree_ms1 <- merge_dupUpdate(tree_ms1, samples[,.(CLSTR_ID, PLOT, BGC_ZONE, BGC_VAR, TYPE_CD)],
                              by = c("CLSTR_ID", "PLOT"), all.x = TRUE)
  treemeasintensity <- vi_d[,.(CLSTR_ID, PLOT, TREE_NO)]
  treemeasintensity[PLOT == "I", MEAS_INTENSE := "FULL"] ## fully measured trees
  treemeasintensity[is.na(MEAS_INTENSE), MEAS_INTENSE := "ENHANCED"] ## fully measured trees
  tree_ms1 <- merge_dupUpdate(tree_ms1, treemeasintensity, by = c("CLSTR_ID", "PLOT", "TREE_NO"),
                              all.x = TRUE)
  tree_ms1[is.na(MEAS_INTENSE) | substr(TYPE_CD, 1, 1) == "B",
           MEAS_INTENSE := "H-ENHANCED"]
  rm(treemeasintensity)
  compileLog <- appendedCat(paste("   ", nrow(tree_ms1[MEAS_INTENSE == "FULL",]),
                                  " observations are fully measured trees", sep = ""),
                            compileLog)
  compileLog <- appendedCat(paste("   ", nrow(tree_ms1[MEAS_INTENSE == "ENHANCED",]),
                                  " observations are enhanced trees in auxi plots", sep = ""),
                            compileLog)
  compileLog <- appendedCat(paste("   ", nrow(tree_ms1[MEAS_INTENSE == "H-ENHANCED",]),
                                  " observations are height enhanced trees", sep = ""),
                            compileLog)
  cat("\n\n", compileLog,
      file = file.path(outputPath, "compilerLog.txt"), append = TRUE, sep = "")
  tree_ms6 <- VRIVolTree(treeData = data.table::copy(tree_ms1),
                         equation = equation, logMinLength = logMinLength,
                         stumpHeight = stumpHeight, breastHeight = breastHeight, UTOPDIB = UTOPDIB)
  cat("\n\n",   tree_ms6$compileLog,
      file = file.path(outputPath, "compilerLog.txt"), append = TRUE, sep = "")
  tree_ms6 <- tree_ms6$treeData
  rm(tree_ms1)


  ######################
  ###################### start the site age compilation
  ### 4. vi_h site age compilation
  compileLog <- appendedCat(paste(rep("*", 60), collapse = ""))
  cat("\n\n",   compileLog,
      file = file.path(outputPath, "compilerLog.txt"), append = TRUE, sep = "")
  tree_ah1 <- merge_dupUpdate(tree_ah1, samples[,.(CLSTR_ID, PLOT, BGC_ZONE)],
                              by = c("CLSTR_ID", "PLOT"), all.x = TRUE)
  tree_ah2 <- siteAgeCompiler(siteAgeData = data.table::copy(tree_ah1),
                              siteToolsDLLPath = siteToolsDLLPath,
                              sasExePath = sasExePath)
  rm(tree_ah1)
  compileLog <- appendedCat("Save compiled site age data to compiled_vi_h.csv.",
                            tree_ah2$compileLog)
  write.csv(tree_ah2$compiledData, file.path(outputPath, "compiled_vi_h.csv"), row.names = FALSE)

  siteAgeSummaries <- siteAgeSummary(tree_ah2$compiledData,
                                     siteToolsDLLPath, sasExePath)
  compileLog <- appendedCat("Summaried compiled site age data at cluster and cluster/species level.",
                            compileLog)
  cl_ah <- siteAgeSummaries$cl_ah
  compileLog <- appendedCat("   Save site age summaries at cluster level to Smries_siteAge_byCL.csv.",
                            compileLog)
  write.csv(cl_ah, file.path(outputPath, "Smries_siteAge_byCL.csv"), row.names = FALSE)
  compileLog <- appendedCat("   Save site age summaries at cluster/species level to Smries_siteAge_byCLSP.csv.",
                            compileLog)
  write.csv(siteAgeSummaries$spc_ah, file.path(outputPath, "Smries_siteAge_byCLSP.csv"), row.names = FALSE)
  cat("\n", compileLog,
      file = file.path(outputPath, "compilerLog.txt"), append = TRUE)
  rm(siteAgeSummaries, tree_ah2)

  ######################
  ######################
  ### 5. start the decay, waste and breakage calculation for full/enhanced trees in vi_c
  compileLog <- appendedCat(paste(rep("*", 60), collapse = ""))
  cat("\n\n",   compileLog,
      file = file.path(outputPath, "compilerLog.txt"), append = TRUE, sep = "")
  siteAgeTable <- merge_dupUpdate(cl_ah[,.(CLSTR_ID, AT_M_TLS, AT_M_TXO)],
                                  unique(samples[,.(CLSTR_ID, PROJ_ID, SAMP_NO, TYPE_CD)],
                                         by = "CLSTR_ID"),
                                  by = "CLSTR_ID",
                                  all.x = TRUE)
  tree_ms6 <- merge_dupUpdate(tree_ms6,
                              unique(samples[,.(CLSTR_ID, PROJ_ID, BGC_ZONE, BGC_SBZN, BGC_VAR,
                                                TSA, TYPE_CD)],
                                     by = "CLSTR_ID"),
                              by = "CLSTR_ID",
                              all.x = TRUE)
  tree_ms7 <- DWBCompiler(treeMS = tree_ms6, siteAge = siteAgeTable,
                          treeLossFactors = vi_d, equation = "KBEC")
  compileLog <- appendedCat("Save compiled fully-measured trees: compilted_vi_c.csv",
                            tree_ms7$compileLog)
  write.csv(tree_ms7$treeMS, file.path(outputPath, "compiled_vi_c.csv"), row.names = FALSE)
  cat("\n\n", compileLog,
      file = file.path(outputPath, "compilerLog.txt"), append = TRUE, sep = "")
  tree_ms7 <- tree_ms7$treeMS
  rm(vi_d, siteAgeTable, tree_ms6)



  #######
  ### 6. start to calculate tree volume components for H-enhanced and non-enhanced trees in auxi plots
  # call vol_ha_2017 macro
  compileLog <- appendedCat(paste(rep("*", 60), collapse = ""))
  cat("\n\n",   compileLog,
      file = file.path(outputPath, "compilerLog.txt"), append = TRUE, sep = "")
  auxtreecompilation <- auxiTreeCompiler(fullMeasuredTrees = data.table::copy(tree_ms7),
                                         auxiTrees = data.table::copy(tree_ax1),
                                         clusterPlotHeader = samples)
  rm(tree_ms7)
  compileLog <- appendedCat("Save regression table into output folder: regressiontable.csv.",
                            auxtreecompilation$compileLog)
  write.csv(auxtreecompilation$regressionTable,
            file.path(outputPath, "regressiontable.csv"), row.names = FALSE)
  compileLog <- appendedCat("Save ratio table into output folder: ratiotable.csv.",
                            compileLog)
  write.csv(auxtreecompilation$ratioTable,
            file.path(outputPath, "ratiotable.csv"), row.names = FALSE)
  compileLog <- appendedCat("Combine and save compiled trees for both enhanced and non-enhanced measurement to alltreeForVol_afterCP.csv.",
                            compileLog)
  prep_smy <- rbindlist(list(auxtreecompilation$fullenhancedtrees[, NET_FCT_METHOD := "Field_Call"],
                             auxtreecompilation$HnonenhancedTrees),
                        fill = TRUE)
  prep_smy[METHOD == "reg_log10", VOL_SRCE := METHOD]
  prep_smy[MEAS_INTENSE %in% c("FULL", "ENHANCED", "H-ENHANCED", "B-SAMPLE"), VOL_SRCE := "Calc"]
  prep_smy[is.na(VOL_SRCE), VOL_SRCE := "Unk"]
  prep_smy[, METHOD := NULL]
  write.csv(prep_smy[order(CLSTR_ID, PLOT, TREE_NO),],
            file.path(outputPath, "alltreeForVol_afterCP.csv"), row.names = FALSE)
  rm(auxtreecompilation)


  ## 7. sammarize and save compiled tree-level data at cluster and cluster/species level
  compileLog <- appendedCat(paste(rep("*", 60), collapse = ""))
  cat("\n\n",   compileLog,
      file = file.path(outputPath, "compilerLog.txt"), append = TRUE, sep = "")
  vrisummaries <- VRISummaries(allVolumeTrees = data.table::copy(prep_smy),
                               clusterPlotHeader = samples,
                               # utilLevel = 4,
                               # weirdUtil = "no",
                               # equation = "KBEC")
                               utilLevel = utilLevel,
                               weirdUtil = weirdUtil,
                               equation = equation)
  compileLog <- appendedCat("Save cluster/species-level volume summaries to Smries_volume_byCLSP.csv",
                            vrisummaries$compileLog)
  write.csv(vrisummaries$vol_bycs, file.path(outputPath, "Smries_volume_byCLSP.csv"),
            row.names = FALSE)
  compileLog <- appendedCat("Save cluster-level volume summaries to Smries_volume_byCL.csv",
                            compileLog)
  write.csv(vrisummaries$vol_byc, file.path(outputPath, "Smries_volume_byCL.csv"),
            row.names = FALSE)
  compileLog <- appendedCat("Save cluster-level height summaries to Smries_height_byCL.csv",
                            compileLog)
  write.csv(vrisummaries$vol_byc, file.path(outputPath, "Smries_height_byCL.csv"),
            row.names = FALSE)
  compileLog <- appendedCat("Save cluster-level species composition summaries to Smries_speciesComposition_byCL.csv",
                            compileLog)
  write.csv(vrisummaries$compositionsmry_byc, file.path(outputPath, "Smries_speciesComposition_byCL.csv"),
            row.names = FALSE)
  cat("\n\n",   compileLog,
      file = file.path(outputPath, "compilerLog.txt"), append = TRUE, sep = "")


  ## 8. small tree and stump compilation
  ## stump data
  compileLog <- appendedCat(paste(rep("*", 60), collapse = ""))
  compileLog <- appendedCat("Start to compile small trees and stump.",
                            compileLog)
  compileLog <- appendedCat("Load cluster header for stump and small trees (i.e., vi_e).",
                            compileLog)
  vi_e <- haven::read_sas(file.path(dataSourcePath, "vi_e.sas7bdat")) %>% data.table
  names(vi_e) <- toupper(names(vi_e))
  compileLog <- appendedCat("Select cluster based on samples.",
                            compileLog)
  vi_e <- vi_e[CLSTR_ID %in% unique(samples$CLSTR_ID),]
  compileLog <- appendedCat(paste("   After selection ", nrow(vi_e), " clusters left.", sep = ""),
                            compileLog)
  compileLog <- appendedCat(paste("   ", nrow(vi_e[PL_ORIG == "SML_TR"]),
                                  " clusters have small trees.", sep = ""),
                            compileLog)
  compileLog <- appendedCat(paste("   ", nrow(vi_e[PL_ORIG == "STUMP"]),
                                  " clusters have stumps.", sep = ""),
                            compileLog)
  compileLog <- appendedCat("Load small trees data (i.e., vi_f).",
                            compileLog)
  ## small tree data
  vi_f <- read_sas(file.path(dataSourcePath, "vi_f.sas7bdat")) %>% data.table
  names(vi_f) <- toupper(names(vi_f))
  vi_f[, obslength := length(TOTAL1), by = c("CLSTR_ID", "PLOT", "SPECIES")]
  compileLog <- appendedCat(logFileProducer(reason = "Duplicate observation at cluster/plot/species level in small tree data",
                                            action = "removed",
                                            displayTable = vi_f[obslength > 1,],
                                            displayColumn = c("CLSTR_ID", "PLOT", "SPECIES")),
                            compileLog)
  vi_f <- unique(vi_f, by = c("CLSTR_ID", "PLOT", "SPECIES"))
  compileLog <- appendedCat(paste("   After removal, small tree data has ",
                                  nrow(vi_f), " observations.", sep = ""),
                            compileLog)
  compileLog <- appendedCat("Select small tree data that have cluster/plot header.",
                            compileLog)
  vi_f[, clusterplot := paste(CLSTR_ID, "_", PLOT, sep = "")]
  vi_e[, clusterplot := paste(CLSTR_ID, "_", PLOT, sep = "")]
  vi_f <- vi_f[clusterplot %in% unique(vi_e[PL_ORIG == "SML_TR",]$clusterplot),]
  compileLog <- appendedCat(paste("   After selection ", nrow(vi_f), " observations left.", sep = ""),
                            compileLog)
  compileLog <- appendedCat("Compile small tree data.",
                            compileLog)
  smalltreecompile <- smallTreeVolSmry(smallTreeData = vi_f,
                                       smallTreePlotHeader = vi_e[PL_ORIG == "SML_TR",])
  compileLog <- appendedCat("Save cluster-level small tree summaries to Smries_smallTree_byCL.csv.",
                            compileLog)
  write.csv(smalltreecompile$clusterSummaries,
            file.path(outputPath, "Smries_smallTree_byCL.csv"), row.names = FALSE)
  compileLog <- appendedCat("Save cluster/species-level small tree summaries to Smries_smallTree_byCLSP.csv.",
                            compileLog)
  write.csv(smalltreecompile$clusterSpeciesSummaries,
            file.path(outputPath, "Smries_smallTree_byCLSP.csv"), row.names = FALSE)
  rm(smalltreecompile)
  compileLog <- appendedCat("Load stump data (i.e., vi_g).",
                            compileLog)
  vi_g <- haven::read_sas(file.path(dataSourcePath, "vi_g.sas7bdat")) %>% data.table
  names(vi_g) <- toupper(names(vi_g))

  compileLog <- appendedCat("Select stump data that have cluster/plot header.",
                            compileLog)
  vi_g[, clusterplot := paste(CLSTR_ID, "_", PLOT, sep = "")]
  vi_g <- vi_g[clusterplot %in% unique(vi_e[PL_ORIG == "SML_TR",]$clusterplot),]
  compileLog <- appendedCat(paste("   After selection ", nrow(vi_g), " observations left.", sep = ""),
                            compileLog)
  compileLog <- appendedCat("Compile stump data.",
                            compileLog)

  ## plot header for stump and small trees
  stumpCompile <- stumpVolSmry(stumpData = vi_g,
                               stumpPlotHeader = vi_e[PL_ORIG == "STUMP",])
  compileLog <- appendedCat("Save cluster-level stump summaries to Smries_stump_byCL.csv.",
                            compileLog)
  write.csv(stumpCompile$stmp_c,
            file.path(outputPath, "Smries_stump_byCL.csv"), row.names = FALSE)
  compileLog <- appendedCat("Save cluster/species-level stump summaries to Smries_stump_byCLSP.csv.",
                            compileLog)
  write.csv(stumpCompile$stmp_cs,
            file.path(outputPath, "Smries_stump_byCLSP.csv"), row.names = FALSE)
  cat("\n\n", compileLog,
      file = file.path(outputPath, "compilerLog.txt"), append = TRUE, sep = "")
  #############################
  #### END OF VRI COMPILER
}







