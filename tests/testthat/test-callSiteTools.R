test_that("callSiteTools.R: sitetolls does not work correctly", {
  library(data.table)
  testdata <- data.table(species = c(rep("FD", 4), rep("PL", 4)),
                         bec_i_c = c(rep("C", 2), rep("I", 6)),
                         agetype = c(1, 0, 1, 0, 1, 1, 0, 0),
                         age = c(rep(35, 4), rep(c(65, 25), 2)),
                         height = 14.3)
  dllloc <- "C:/SiteToolsdll"
  testdata[, si_sp:= ST_SpecRemap(species = species,
                                  ICRegion = bec_i_c,
                                  siteToolsDLLPath = dllloc)]
  testdata[si_sp >= 0, site_curve := ST_DefCurve(siteIndexRef = si_sp,
                                                 siteToolsDLLPath = dllloc)]
  testdata[si_sp >= 0, grth_curve := ST_DefGICurve(siteIndexRef = si_sp,
                                                   siteToolsDLLPath = dllloc)]
  testdata[si_sp >= 0, ':='(si_si = ST_HTAgeToSI(curveRef = site_curve,
                                                 boredAge = age, ageType = agetype,
                                                 height = height, estimateMethod = 1,
                                                 siteToolsDLLPath = dllloc)$output,
                            siErr = ST_HTAgeToSI(curveRef = site_curve,
                                                 boredAge = age, ageType = agetype,
                                                 height = height, estimateMethod = 1,
                                                 siteToolsDLLPath = dllloc)$error)]
  testdata[si_sp >= 0, ':='(age_corr = ST_YrsToBH(SICurveRef = site_curve,
                                                  siteIndex = si_si,
                                                  siteToolsDLLPath = dllloc)$output,
                            ybherr = ST_YrsToBH(SICurveRef = site_curve,
                                                siteIndex = si_si,
                                                siteToolsDLLPath = dllloc)$error)]
  testdata[si_sp >= 0 & siErr < 0, si_si := as.numeric(NA)]
  testdata[si_sp >= 0 & age <= 50, ':='(si_gi = ST_HTAgeToSI(curveRef = grth_curve,
                                                             boredAge = age,
                                                             ageType = agetype,
                                                             height = height,
                                                             estimateMethod = 1,
                                                             siteToolsDLLPath = dllloc)$output,
                                        gierr = ST_HTAgeToSI(curveRef = grth_curve,
                                                             boredAge = age,
                                                             ageType = agetype,
                                                             height = height,
                                                             estimateMethod = 1,
                                                             siteToolsDLLPath = dllloc)$error)]
  testdata[, curve_id := ST_CurveName(SICurveRef = site_curve,
                                      siteToolsDLLPath = dllloc)]
  expect_is(testdata$si_sp, "numeric")
  expect_equal(testdata$si_sp, c(40, 40, 41, 41, 82, 82, 82, 82))
  
  expect_is(testdata$site_curve, "numeric")
  expect_equal(testdata$site_curve, c(100, 100, 96, 96, rep(45, 4)))
  
  expect_is(testdata$grth_curve, "numeric")
  expect_equal(testdata$grth_curve, c(15, 15, 19, 19, rep(42, 4)))
  
  expect_is(testdata$si_si, "numeric")
  expect_equal(as.numeric(round(testdata$si_si, 2)), 
               c(17.89, 22.40, 18.63, 23.01, 11.88, 22.75, 13.02, 26.42))
  expect_is(testdata$siErr, "numeric")
  expect_equal(testdata$siErr, rep(0, 8))  
  
  expect_is(testdata$age_corr, "numeric")
  expect_equal(as.numeric(testdata$age_corr), c(10.5, 9.5, 9.5, 8.5, 8.5, 5.5, 7.5, 5.5))
  
  expect_is(testdata$ybherr, "numeric")
  expect_equal(testdata$ybherr, rep(0, 8))
  
  expect_is(testdata$si_gi, "numeric")
  expect_equal(as.numeric(round(testdata$si_gi, 2)),
               c(19.05, -9.00, 18.69, -9.00,  NA, 22.52, NA, -9.00))
  
  expect_is(testdata$gierr, "numeric")
  expect_equal(testdata$gierr, c(0, -9,  0, -9, NA,  0, NA, -9))
  
  expect_is(testdata$curve_id, "character")
  expect_equal(as.character(testdata$curve_id),
               c(rep("Bruce (1981ac)", 2), rep("Thrower and Goudie (1992ac)", 2),
                 rep("Thrower (1994)", 4)))
})