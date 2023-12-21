# General integration tests for Classic Process analysis

test_that("Error handling works - observations", {
  options <- jaspTools::analysisOptions("ClassicProcess")
  options$dependent <- "contNormal"
  options$covariates <- list("debNaN")
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", "debug", options)
  expect_identical(results[["status"]], "validationError", label = "Observations check")

  options <- jaspTools::analysisOptions("ClassicProcess")
  options$dependent <- "contNormal"
  options$factors <- list("debNaN")
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", "debug", options)
  expect_identical(results[["status"]], "validationError", label = "Observations check")
})

test_that("Error handling works - variance", {
  options <- jaspTools::analysisOptions("ClassicProcess")
  options$dependent <- "contNormal"
  options$covariates <- list("debSame")
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", "debug", options)
  expect_identical(results[["status"]], "validationError", label = "Variance check")
})

test_that("Error handling works - infinity", {
  options <- jaspTools::analysisOptions("ClassicProcess")
  options$dependent <- "contNormal"
  options$covariates <- list("debInf")
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", "debug", options)
  expect_identical(results[["status"]], "validationError", label = "Infinity check")
})

test_that("Error handling works - covariance", {
  options <- jaspTools::analysisOptions("ClassicProcess")
  options$dependent <- "contNormal"
  options$covariates <- list("debCollin1", "debCollin2")
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", "debug", options)
  expect_identical(results[["status"]], "validationError", label = "Covariance check")
})

get_fac_df <- function() {
  df <- data.frame(
    contNormal = rnorm(100),
    contcor1 = rnorm(100),
    facThree = cut(rnorm(100), breaks = 3, labels = c("A", "B", "C")),
    facTwo = sample(c("D", "E"), 100, replace = TRUE)
  )
  return(df)
}

test_that("Factors with more than two levels work", {
  set.seed(1)
  df <- get_fac_df()

  options <- jaspTools::analysisOptions("ClassicProcess")
  options$dependent <- "contNormal"
  options$covariates <- list("contcor1")
  options$factors <- list("facThree")
  options$statisticalPathPlotsCovariances <- TRUE
  options$statisticalPathPlotsResidualVariances <- TRUE
  options$errorCalculationMethod <- "standard"
  options$naAction <- "fiml"
  options$emulation <- "lavaan"
  options$estimator <- "default"
  options$moderationProbes <- list(list(probePercentile = 16, value = "16"), list(probePercentile = 50,
                                                                                  value = "50"), list(probePercentile = 84, value = "84"))
  options$pathPlotsLegend <- TRUE
  options$pathPlotsColorPalette <- "colorblind"
  options$processModels <- list(list(conceptualPathPlot = TRUE, independentCovariances = TRUE,
                                     inputType = "inputVariables", mediationEffects = TRUE, mediatorCovariances = TRUE,
                                     modelNumber = 1, modelNumberCovariates = list(), modelNumberIndependent = "",
                                     modelNumberMediators = list(), modelNumberModeratorW = "",
                                     modelNumberModeratorZ = "", name = "Model 1", pathCoefficients = TRUE, intercepts = FALSE,
                                     processRelationships = list(list(processDependent = "contNormal",
                                                                      processIndependent = "facThree", processType = "moderators",
                                                                      processVariable = "contcor1")), residualCovariances = TRUE,
                                     statisticalPathPlot = TRUE, totalEffects = TRUE, localTests = FALSE,
                                     localTestType = "cis", localTestBootstrap = FALSE, localTestBootstrapSamples = 1000))
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", df, options)

  table <- results[["results"]][["modelSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(738.631697493269, 780.314420469079, 6, "Model 1", 100, -353.315848746635,
                                      2))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_covariancesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.0429566774419572, 0.137322520212336, 0.0471829213851896, "contcor1",
                                      "<unicode>", 0.30492506182821, "facThreeB", 0.0459904363234001,
                                      1.02592897909044, -0.188552871405244, -0.0930470912094735, -0.140799981307359,
                                      "facThreeC", "<unicode>", 7.51558348888182e-09, "facThreeB",
                                      0.0243641671349851, -5.77897781308439, 0.16653755434125, 0.294262457180443,
                                      0.230400005760847, "facThreeB", "<unicode>", 1.53743684450092e-12,
                                      "facThreeB", 0.032583482106475, 7.07106763506597, -0.111386692251649,
                                      0.0439392202030574, -0.0337237360242957, "contcor1", "<unicode>",
                                      0.394725492097836, "facThreeC", 0.0396246853717459, -0.851078960196418,
                                      0.124035773561638, 0.219164194184822, 0.17159998387323, "facThreeC",
                                      "<unicode>", 1.53743684450092e-12, "facThreeC", 0.0242679001689687,
                                      7.07106847640052, 0.562065668856824, 0.993138292338785, 0.777601980597805,
                                      "contNormal", "<unicode>", 1.53743684450092e-12, "contNormal",
                                      0.109969526706156, 7.07106781204577, 0.656578232571228, 1.16013722015981,
                                      0.90835772636552, "contcor1", "<unicode>", 1.53743684450092e-12,
                                      "contcor1", 0.128461285911525, 7.0710620707248))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_mediationEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.830256392443183, 0.671610737357546, 16, -0.0793228275428181,
                                      "B", "facThree", "contNormal", "<unicode>", 0.835982155868673,
                                      0.383136410068569, -0.207035472114545, -0.525905203599432, 1.15334878307229,
                                      16, 0.313721789736431, "C", "facThree", "contNormal", "<unicode>",
                                      0.463967628372663, 0.428388990797144, 0.732329253262693, -0.419840075280424,
                                      0.605570327476428, 50, 0.0928651260980024, "B", "facThree",
                                      "contNormal", "<unicode>", 0.722586733028552, 0.261589093178538,
                                      0.355003815218782, -0.343080242602794, 0.839989902182687, 50,
                                      0.248454829789947, "C", "facThree", "contNormal", "<unicode>",
                                      0.410383843371771, 0.301809154177676, 0.823218336325481, -0.539700678828145,
                                      1.248288868581, 84, 0.354294094876428, "B", "facThree", "contNormal",
                                      "<unicode>", 0.437310752972172, 0.456128163964384, 0.776742422123473,
                                      -0.891019363658371, 1.18974240595951, 84, 0.149361521150572,
                                      "C", "facThree", "contNormal", "<unicode>", 0.778418345460771,
                                      0.530816327756701, 0.281380796596429))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_pathCoefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.38427711542656, 0.649582479357356, 0.132652681965398, "facThreeB",
                                      "<unicode>", 0.614993118624875, "contNormal", 0.263744538914712,
                                      0.502958971250186, -0.368684421054037, 0.835431543008531, 0.233373560977247,
                                      "facThreeC", "<unicode>", 0.447413744521379, "contNormal", 0.307178084281263,
                                      0.759733760054192, -0.772565530832362, 0.48708327509329, -0.142741127869536,
                                      "contcor1", "<unicode>", 0.656898600668854, "contNormal", 0.32134488589117,
                                      -0.444199158401663, -0.440651907089957, 0.890228809546408, 0.224788451228225,
                                      "facThreeB:contcor1", "<unicode>", 0.507917529548882, "contNormal",
                                      0.3395166255947, 0.662083781124072, -0.838061138719313, 0.667651321626665,
                                      -0.0852049085463242, "facThreeC:contcor1", "<unicode>", 0.824454027031606,
                                      "contNormal", 0.384117379763824, -0.221819977525393))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_totalEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.830256392443183, 0.671610737357546, 16, -0.0793228275428181,
                                      "B", "Total", "facThree", "contNormal", "<unicode>", 0.835982155868673,
                                      0.383136410068569, -0.207035472114545, -0.525905203599432, 1.15334878307229,
                                      16, 0.313721789736431, "C", "Total", "facThree", "contNormal",
                                      "<unicode>", 0.463967628372663, 0.428388990797144, 0.732329253262693,
                                      -0.419840075280424, 0.605570327476428, 50, 0.0928651260980024,
                                      "B", "Total", "facThree", "contNormal", "<unicode>", 0.722586733028552,
                                      0.261589093178538, 0.355003815218782, -0.343080242602794, 0.839989902182687,
                                      50, 0.248454829789947, "C", "Total", "facThree", "contNormal",
                                      "<unicode>", 0.410383843371771, 0.301809154177676, 0.823218336325481,
                                      -0.539700678828145, 1.248288868581, 84, 0.354294094876428, "B",
                                      "Total", "facThree", "contNormal", "<unicode>", 0.437310752972172,
                                      0.456128163964384, 0.776742422123473, -0.891019363658371, 1.18974240595951,
                                      84, 0.149361521150572, "C", "Total", "facThree", "contNormal",
                                      "<unicode>", 0.778418345460771, 0.530816327756701, 0.281380796596429
                                 ))

  plotName <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 1"]][["collection"]][["pathPlotContainer_Model 1_conceptPathPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "conceptual-path-plot-facThree")

  plotName <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 1"]][["collection"]][["pathPlotContainer_Model 1_statPathPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "statistical-path-plot-facThree")
})

test_that("Interactions between three-level and two-level factors work", {
  set.seed(1)
  df <- get_fac_df()

  options <- jaspTools::analysisOptions("ClassicProcess")
  options$dependent <- "contNormal"
  options$covariates <- list("contcor1")
  options$factors <- list("facTwo", "facThree")
  options$statisticalPathPlotsCovariances <- TRUE
  options$statisticalPathPlotsResidualVariances <- TRUE
  options$errorCalculationMethod <- "standard"
  options$naAction <- "fiml"
  options$emulation <- "lavaan"
  options$estimator <- "default"
  options$moderationProbes <- list(list(probePercentile = 16, value = "16"), list(probePercentile = 50,
                                                                                  value = "50"), list(probePercentile = 84, value = "84"))
  options$pathPlotsLegend <- TRUE
  options$pathPlotsColorPalette <- "colorblind"
  options$processModels <- list(list(conceptualPathPlot = TRUE, independentCovariances = TRUE,
                                     inputType = "inputVariables", mediationEffects = TRUE, mediatorCovariances = TRUE,
                                     modelNumber = 1, modelNumberCovariates = list(), modelNumberIndependent = "",
                                     modelNumberMediators = list(), modelNumberModeratorW = "",
                                     modelNumberModeratorZ = "", name = "Model 1", pathCoefficients = TRUE, intercepts = FALSE,
                                     processRelationships = list(list(processDependent = "contNormal",
                                                                      processIndependent = "facThree", processType = "moderators",
                                                                      processVariable = "facTwo")), residualCovariances = TRUE,
                                     statisticalPathPlot = TRUE, totalEffects = TRUE, localTests = FALSE,
                                     localTestType = "cis", localTestBootstrap = FALSE, localTestBootstrapSamples = 1000))
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", df, options)

  table <- results[["results"]][["modelSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(607.798723015161, 649.48144599097, 6, "Model 1", 100, -287.89936150758,
                                      2))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_covariancesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.0625879470174504, 0.0313879697933983, -0.015599988612026, "facTwoE",
                                      "<unicode>", 0.515235336015082, "facThreeB", 0.0239738886918634,
                                      -0.65070747647713, -0.188552899318745, -0.0930470981391739,
                                      -0.140799998728959, "facThreeC", "<unicode>", 7.51560835787757e-09,
                                      "facThreeB", 0.0243641724880938, -5.77897725842177, 0.166537561194689,
                                      0.294262481084972, 0.230400021139831, "facThreeB", "<unicode>",
                                      1.53743684450092e-12, "facThreeB", 0.0325834864563229, 7.07106716307582,
                                      -0.00972474511266434, 0.0721247068051297, 0.0311999808462327,
                                      "facTwoE", "<unicode>", 0.135116244945199, "facThreeC", 0.020880345905183,
                                      1.49422720236106, 0.124035784194556, 0.219164231272113, 0.171600007733335,
                                      "facThreeC", "<unicode>", 1.53743684450092e-12, "facThreeC",
                                      0.0242679069176572, 7.07106749319528, 0.564294089241596, 0.997075785302741,
                                      0.780684937272168, "contNormal", "<unicode>", 1.53743684450092e-12,
                                      "contNormal", 0.110405522620536, 7.07106781202772, 0.179548301980658,
                                      0.317251715854103, 0.24840000891738, "facTwoE", "<unicode>",
                                      1.53743684450092e-12, "facTwoE", 0.0351290674113485, 7.07106755806259
                                 ))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_mediationEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.791795096924544, 0.581685251275046, -0.105054922824749, "B",
                                      0, "facThree", "contNormal", "<unicode>", 0.764308431708751,
                                      0.350384078236495, -0.299827901294765, -0.835536962519563, 0.956997866934718,
                                      0.0607304522075771, "C", 0, "facThree", "contNormal", "<unicode>",
                                      0.894346977790344, 0.457287695996857, 0.132805786683564, -0.346979683149472,
                                      1.19016835490491, 0.42159433587772, "B", 1, "facThree", "contNormal",
                                      "<unicode>", 0.282320764531175, 0.392136807150339, 1.07512054005195,
                                      -0.273316897247542, 1.3997156098619, 0.563199356307179, "C",
                                      1, "facThree", "contNormal", "<unicode>", 0.1869751495513, 0.426801849499815,
                                      1.31958040239801))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_pathCoefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.791795096924544, 0.581685251275046, -0.105054922824749, "facThreeB",
                                      "<unicode>", 0.764308431708751, "contNormal", 0.350384078236495,
                                      -0.299827901294765, -0.835536962519563, 0.956997866934718, 0.0607304522075771,
                                      "facThreeC", "<unicode>", 0.894346977790344, "contNormal", 0.457287695996857,
                                      0.132805786683564, -1.50982658881481, 0.360680618617237, -0.574572985098788,
                                      "facTwoE", "<unicode>", 0.228549486558002, "contNormal", 0.477178974253193,
                                      -1.20410373486808, -0.504038904409244, 1.55733742181418, 0.526649258702469,
                                      "facThreeB:facTwoE", "<unicode>", 0.316594805492668, "contNormal",
                                      0.525870970712549, 1.00147999800952, -0.723522417834018, 1.72846022603322,
                                      0.502468904099602, "facThreeC:facTwoE", "<unicode>", 0.421809782418291,
                                      "contNormal", 0.625517270523379, 0.803285421166996))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_totalEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.791795096924544, 0.581685251275046, -0.105054922824749, "B",
                                      0, "Total", "facThree", "contNormal", "<unicode>", 0.764308431708751,
                                      0.350384078236495, -0.299827901294765, -0.835536962519563, 0.956997866934718,
                                      0.0607304522075771, "C", 0, "Total", "facThree", "contNormal",
                                      "<unicode>", 0.894346977790344, 0.457287695996857, 0.132805786683564,
                                      -0.346979683149472, 1.19016835490491, 0.42159433587772, "B",
                                      1, "Total", "facThree", "contNormal", "<unicode>", 0.282320764531175,
                                      0.392136807150339, 1.07512054005195, -0.273316897247542, 1.3997156098619,
                                      0.563199356307179, "C", 1, "Total", "facThree", "contNormal",
                                      "<unicode>", 0.1869751495513, 0.426801849499815, 1.31958040239801
                                 ))

  plotName <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 1"]][["collection"]][["pathPlotContainer_Model 1_conceptPathPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "conceptual-path-plot-facThree-int-facTwo")

  plotName <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 1"]][["collection"]][["pathPlotContainer_Model 1_statPathPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "statistical-path-plot-facThree-int-facTwo")
})

test_that("Interactions between two-level and three-level factors work", {
  set.seed(1)
  df <- get_fac_df()
  options <- jaspTools::analysisOptions("ClassicProcess")
  options$dependent <- "contNormal"
  options$covariates <- list("contcor1")
  options$factors <- list("facTwo", "facThree")
  options$statisticalPathPlotsCovariances <- TRUE
  options$statisticalPathPlotsResidualVariances <- TRUE
  options$errorCalculationMethod <- "standard"
  options$naAction <- "fiml"
  options$emulation <- "lavaan"
  options$estimator <- "default"
  options$moderationProbes <- list(list(probePercentile = 16, value = "16"), list(probePercentile = 50,
                                                                                  value = "50"), list(probePercentile = 84, value = "84"))
  options$pathPlotsLegend <- TRUE
  options$pathPlotsColorPalette <- "colorblind"
  options$processModels <- list(list(conceptualPathPlot = TRUE, independentCovariances = TRUE,
                                     inputType = "inputVariables", mediationEffects = TRUE, mediatorCovariances = TRUE,
                                     modelNumber = 1, modelNumberCovariates = list(), modelNumberIndependent = "",
                                     modelNumberMediators = list(), modelNumberModeratorW = "",
                                     modelNumberModeratorZ = "", name = "Model 1", pathCoefficients = TRUE, intercepts = FALSE,
                                     processRelationships = list(list(processDependent = "contNormal",
                                                                      processIndependent = "facTwo", processType = "moderators",
                                                                      processVariable = "facThree")), residualCovariances = TRUE,
                                     statisticalPathPlot = TRUE, totalEffects = TRUE, localTests = FALSE,
                                     localTestType = "cis", localTestBootstrap = FALSE, localTestBootstrapSamples = 1000))
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", df, options)

  table <- results[["results"]][["modelSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(607.798723015161, 649.48144599097, 6, "Model 1", 100, -287.89936150758,
                                      2))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_covariancesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.00972474511259642, 0.0721247068050616, 0.0311999808462326,
                                      "facThreeC", "<unicode>", 0.135116244944552, "facTwoE", 0.0208803459051483,
                                      1.49422720236353, -0.0625879470174193, 0.0313879697933672, -0.0155999886120261,
                                      "facThreeB", "<unicode>", 0.515235336014803, "facTwoE", 0.0239738886918475,
                                      -0.650707476477561, 0.179548301980438, 0.317251715854323, 0.24840000891738,
                                      "facTwoE", "<unicode>", 1.53743684450092e-12, "facTwoE", 0.0351290674114607,
                                      7.07106755804, 0.5642940892409, 0.997075785303437, 0.780684937272168,
                                      "contNormal", "<unicode>", 1.53743684450092e-12, "contNormal",
                                      0.110405522620891, 7.07106781200496, -0.188552899318674, -0.0930470981392449,
                                      -0.140799998728959, "facThreeC", "<unicode>", 7.51560835787757e-09,
                                      "facThreeB", 0.0243641724880576, -5.77897725843036, 0.166537561194737,
                                      0.294262481084924, 0.230400021139831, "facThreeB", "<unicode>",
                                      1.53743684450092e-12, "facThreeB", 0.0325834864562984, 7.07106716308114,
                                      0.124035784194623, 0.219164231272046, 0.171600007733335, "facThreeC",
                                      "<unicode>", 1.53743684450092e-12, "facThreeC", 0.0242679069176231,
                                      7.07106749320521))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_mediationEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1.50982658998053, 0.360680619782956, -0.574572985098788, 0, 0,
                                      "E", "facTwo", "contNormal", "<unicode>", 0.22854948713801,
                                      0.477178974847959, -1.20410373336726, -0.481073569295704, 0.385226116503068,
                                      -0.0479237263963181, 1, 0, "E", "facTwo", "contNormal", "<unicode>",
                                      0.828324822990655, 0.220998878712067, -0.216850540942141, -0.864792807836215,
                                      0.720584645837844, -0.0721040809991859, 0, 1, "E", "facTwo",
                                      "contNormal", "<unicode>", 0.858502237177862, 0.404440455584724,
                                      -0.178281079460611))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_pathCoefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1.50982658998053, 0.360680619782956, -0.574572985098788, "facTwoE",
                                      "<unicode>", 0.22854948713801, "contNormal", 0.477178974847959,
                                      -1.20410373336726, -0.791795097147449, 0.581685251497951, -0.105054922824749,
                                      "facThreeB", "<unicode>", 0.764308431782987, "contNormal", 0.350384078350224,
                                      -0.299827901197445, -0.835536962531635, 0.956997866946789, 0.0607304522075774,
                                      "facThreeC", "<unicode>", 0.894346977791758, "contNormal", 0.457287696003016,
                                      0.132805786681776, -0.504038905718019, 1.55733742312296, 0.526649258702469,
                                      "facTwoE:facThreeB", "<unicode>", 0.316594806107179, "contNormal",
                                      0.525870971380304, 1.00147999673784, -0.72352241865697, 1.72846022685617,
                                      0.502468904099602, "facTwoE:facThreeC", "<unicode>", 0.421809782729878,
                                      "contNormal", 0.62551727094326, 0.803285420627787))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_totalEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1.50982658998053, 0.360680619782956, -0.574572985098788, 0, 0,
                                      "E", "Total", "facTwo", "contNormal", "<unicode>", 0.22854948713801,
                                      0.477178974847959, -1.20410373336726, -0.481073569295704, 0.385226116503068,
                                      -0.0479237263963181, 1, 0, "E", "Total", "facTwo", "contNormal",
                                      "<unicode>", 0.828324822990655, 0.220998878712067, -0.216850540942141,
                                      -0.864792807836215, 0.720584645837844, -0.0721040809991859,
                                      0, 1, "E", "Total", "facTwo", "contNormal", "<unicode>", 0.858502237177862,
                                      0.404440455584724, -0.178281079460611))

  plotName <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 1"]][["collection"]][["pathPlotContainer_Model 1_conceptPathPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "conceptual-path-plot-facTwo-int-facThree")

  plotName <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 1"]][["collection"]][["pathPlotContainer_Model 1_statPathPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "statistical-path-plot-facTwo-int-facThree")
})

checkTables <- function(results1, results2) {
  table1 <- results1[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_covariancesTable"]][["data"]]
  table2 <- results2[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_covariancesTable"]][["data"]]
  expect_equal(table1, table2)

  table1 <- results1[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_mediationEffectsTable"]][["data"]]
  table2 <- results2[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_mediationEffectsTable"]][["data"]]
  expect_equal(table1, table2)

  table1 <- results1[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_pathCoefficientsTable"]][["data"]]
  table2 <- results2[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_pathCoefficientsTable"]][["data"]]
  expect_equal(table1, table2)

  table1 <- results1[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_totalEffectsTable"]][["data"]]
  table2 <- results2[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_totalEffectsTable"]][["data"]]
  expect_equal(table1, table2)
}

test_that("Standardized estimates match", {
  N <- 100

  set.seed(1)
  dfUnstd <- data.frame(x = rnorm(N, 1, 2), y = rnorm(N), m = rnorm(N, 3, 4))

  options <- jaspTools::analysisOptions("ClassicProcess")
  options$dependent <- "y"
  options$covariates <- list("x", "m")
  options$statisticalPathPlotsCovariances <- TRUE
  options$statisticalPathPlotsResidualVariances <- TRUE
  options$errorCalculationMethod <- "standard"
  options$naAction <- "listwise"
  options$emulation <- "lavaan"
  options$estimator <- "default"
  options$standardizedEstimates <- "standardized"
  options$moderationProbes <- list(list(probePercentile = 16, value = "16"), list(probePercentile = 50,
                                                                                  value = "50"), list(probePercentile = 84, value = "84"))
  options$pathPlotsLegend <- TRUE
  options$pathPlotsColorPalette <- "colorblind"
  options$processModels <- list(list(conceptualPathPlot = TRUE, independentCovariances = TRUE,
                                     inputType = "inputVariables", mediationEffects = TRUE, mediatorCovariances = TRUE,
                                     modelNumber = 1, modelNumberCovariates = list(), modelNumberIndependent = "",
                                     modelNumberMediators = list(), modelNumberModeratorW = "",
                                     modelNumberModeratorZ = "", name = "Model 1", pathCoefficients = TRUE, intercepts = FALSE,
                                     processRelationships = list(list(processDependent = "y",
                                                                      processIndependent = "x", processType = "mediators",
                                                                      processVariable = "m")), residualCovariances = TRUE,
                                     statisticalPathPlot = TRUE, totalEffects = TRUE, localTests = FALSE,
                                     localTestType = "cis", localTestBootstrap = FALSE, localTestBootstrapSamples = 1000))
  set.seed(1)
  resultsUnstd <- jaspTools::runAnalysis("ClassicProcess", dfUnstd, options)

  dfStd <- as.data.frame(scale(dfUnstd))

  options$standardizedEstimates <- "unstandardized"

  set.seed(1)
  resultsStd <- jaspTools::runAnalysis("ClassicProcess", dfStd, options)

  checkTables(resultsUnstd, resultsStd)
})

test_that("Standardized estimates match - missing values/listwise", {
  N <- 100

  set.seed(1)
  dfUnstd <- data.frame(x = rnorm(N, 1, 2), y = rnorm(N), m = rnorm(N, 3, 4))
  dfUnstd$x[1:10] <- NA
  dfUnstd$y[11:20] <- NA
  dfUnstd$m[21:30] <- NA

  options <- jaspTools::analysisOptions("ClassicProcess")
  options$dependent <- "y"
  options$covariates <- list("x", "m")
  options$statisticalPathPlotsCovariances <- TRUE
  options$statisticalPathPlotsResidualVariances <- TRUE
  options$errorCalculationMethod <- "standard"
  options$naAction <- "listwise"
  options$emulation <- "lavaan"
  options$estimator <- "default"
  options$standardizedEstimates <- "standardized"
  options$moderationProbes <- list(list(probePercentile = 16, value = "16"), list(probePercentile = 50,
                                                                                  value = "50"), list(probePercentile = 84, value = "84"))
  options$pathPlotsLegend <- TRUE
  options$pathPlotsColorPalette <- "colorblind"
  options$processModels <- list(list(conceptualPathPlot = TRUE, independentCovariances = TRUE,
                                     inputType = "inputVariables", mediationEffects = TRUE, mediatorCovariances = TRUE,
                                     modelNumber = 1, modelNumberCovariates = list(), modelNumberIndependent = "",
                                     modelNumberMediators = list(), modelNumberModeratorW = "",
                                     modelNumberModeratorZ = "", name = "Model 1", pathCoefficients = TRUE, intercepts = FALSE,
                                     processRelationships = list(list(processDependent = "y",
                                                                      processIndependent = "x", processType = "mediators",
                                                                      processVariable = "m")), residualCovariances = TRUE,
                                     statisticalPathPlot = TRUE, totalEffects = TRUE, localTests = FALSE,
                                     localTestType = "cis", localTestBootstrap = FALSE, localTestBootstrapSamples = 1000))
  set.seed(1)
  resultsUnstd <- jaspTools::runAnalysis("ClassicProcess", dfUnstd, options)

  dfStd <- dfUnstd
  dfStd[complete.cases(dfStd),] <- as.data.frame(scale(dfStd[complete.cases(dfStd),]))

  options$standardizedEstimates <- "unstandardized"

  set.seed(1)
  resultsStd <- jaspTools::runAnalysis("ClassicProcess", dfStd, options)

  checkTables(resultsUnstd, resultsStd)
})

test_that("Standardized estimates match - missing values/fiml", {
  N <- 100

  set.seed(1)
  dfUnstd <- data.frame(x = rnorm(N, 1, 2), y = rnorm(N), m = rnorm(N, 3, 4))
  dfUnstd$x[1:10] <- NA
  dfUnstd$y[11:20] <- NA
  dfUnstd$m[21:30] <- NA

  options <- jaspTools::analysisOptions("ClassicProcess")
  options$dependent <- "y"
  options$covariates <- list("x", "m")
  options$statisticalPathPlotsCovariances <- TRUE
  options$statisticalPathPlotsResidualVariances <- TRUE
  options$errorCalculationMethod <- "standard"
  options$naAction <- "fiml"
  options$emulation <- "lavaan"
  options$estimator <- "default"
  options$standardizedEstimates <- "standardized"
  options$moderationProbes <- list(list(probePercentile = 16, value = "16"), list(probePercentile = 50,
                                                                                  value = "50"), list(probePercentile = 84, value = "84"))
  options$pathPlotsLegend <- TRUE
  options$pathPlotsColorPalette <- "colorblind"
  options$processModels <- list(list(conceptualPathPlot = TRUE, independentCovariances = TRUE,
                                     inputType = "inputVariables", mediationEffects = TRUE, mediatorCovariances = TRUE,
                                     modelNumber = 1, modelNumberCovariates = list(), modelNumberIndependent = "",
                                     modelNumberMediators = list(), modelNumberModeratorW = "",
                                     modelNumberModeratorZ = "", name = "Model 1", pathCoefficients = TRUE, intercepts = FALSE,
                                     processRelationships = list(list(processDependent = "y",
                                                                      processIndependent = "x", processType = "mediators",
                                                                      processVariable = "m")), residualCovariances = TRUE,
                                     statisticalPathPlot = TRUE, totalEffects = TRUE, localTests = FALSE,
                                     localTestType = "cis", localTestBootstrap = FALSE, localTestBootstrapSamples = 1000))
  set.seed(1)
  resultsUnstd <- jaspTools::runAnalysis("ClassicProcess", dfUnstd, options)

  dfStd <- dfUnstd
  dfStd <- as.data.frame(scale(dfStd))

  options$standardizedEstimates <- "unstandardized"

  set.seed(1)
  resultsStd <- jaspTools::runAnalysis("ClassicProcess", dfStd, options)

  checkTables(resultsUnstd, resultsStd)
})

test_that("Standardized estimates match - moderated moderation", {
  N <- 100

  set.seed(1)
  dfUnstd <- data.frame(x = rnorm(N, 1, 2), y = rnorm(N), w = rnorm(N, 3, 4), z = rnorm(N, 3, 4))

  options <- jaspTools::analysisOptions("ClassicProcess")
  options$dependent <- "y"
  options$covariates <- list("x", "w", "z")
  options$statisticalPathPlotsCovariances <- TRUE
  options$statisticalPathPlotsResidualVariances <- TRUE
  options$errorCalculationMethod <- "standard"
  options$naAction <- "listwise"
  options$emulation <- "lavaan"
  options$estimator <- "default"
  options$standardizedEstimates <- "standardized"
  options$moderationProbes <- list(list(probePercentile = 16, value = "16"), list(probePercentile = 50,
                                                                                  value = "50"), list(probePercentile = 84, value = "84"))
  options$pathPlotsLegend <- TRUE
  options$pathPlotsColorPalette <- "colorblind"
  options$processModels <- list(list(conceptualPathPlot = TRUE, independentCovariances = TRUE,
                                     inputType = "inputVariables", mediationEffects = TRUE, mediatorCovariances = TRUE,
                                     modelNumber = 1, modelNumberCovariates = list(), modelNumberIndependent = "",
                                     modelNumberMediators = list(), modelNumberModeratorW = "",
                                     modelNumberModeratorZ = "", name = "Model 1", pathCoefficients = TRUE, intercepts = FALSE,
                                     processRelationships = list(list(processDependent = "y",
                                                                      processIndependent = "x", processType = "moderators",
                                                                      processVariable = "w"), list(processDependent = "y",
                                                                                                   processIndependent = "w", processType = "moderators",
                                                                                                   processVariable = "z")), residualCovariances = TRUE,
                                     statisticalPathPlot = TRUE, totalEffects = TRUE, localTests = FALSE,
                                     localTestType = "cis", localTestBootstrap = FALSE, localTestBootstrapSamples = 1000))
  set.seed(1)
  resultsUnstd <- jaspTools::runAnalysis("ClassicProcess", dfUnstd, options)

  dfStd <- dfUnstd
  dfStd[complete.cases(dfStd),] <- as.data.frame(scale(dfStd[complete.cases(dfStd),]))

  options$standardizedEstimates <- "unstandardized"

  set.seed(1)
  resultsStd <- jaspTools::runAnalysis("ClassicProcess", dfStd, options)

  checkTables(resultsUnstd, resultsStd)
})

test_that("Bootstrapping works", {
  options <- jaspTools::analysisOptions("ClassicProcess")
  options$dependent <- "contNormal"
  options$covariates <- list("contGamma", "contcor1", "contcor2", "debCollin1")
  options$factors <- list("facGender", "facExperim")
  options$statisticalPathPlotsCovariances <- TRUE
  options$statisticalPathPlotsResidualVariances <- TRUE
  options$errorCalculationMethod <- "bootstrap"
  options$bootstrapSamples <- 50
  options$bootstrapCiType <- "bca.simple"
  options$ciLevel <- 0.95
  options$naAction <- "fiml"
  options$emulation <- "lavaan"
  options$estimator <- "default"
  options$moderationProbes <- list(list(probePercentile = 16, value = "16"), list(probePercentile = 50,
                                                                                  value = "50"), list(probePercentile = 84, value = "84"))
  options$pathPlotsLegend <- TRUE
  options$pathPlotsColorPalette <- "colorblind"
  options$processModels <- list(list(conceptualPathPlot = TRUE, independentCovariances = TRUE,
                                     inputType = "inputVariables", mediationEffects = TRUE, mediatorCovariances = TRUE,
                                     modelNumber = 1, modelNumberCovariates = list(), modelNumberIndependent = "",
                                     modelNumberMediators = list(), modelNumberModeratorW = "",
                                     modelNumberModeratorZ = "", name = "Model 1", pathCoefficients = TRUE, intercepts = FALSE,
                                     processRelationships = list(list(processDependent = "contNormal",
                                                                      processIndependent = "contGamma", processType = "mediators",
                                                                      processVariable = "debCollin1"), list(processDependent = "contNormal",
                                                                                                            processIndependent = "contGamma", processType = "moderators",
                                                                                                            processVariable = "contcor1")), residualCovariances = TRUE,
                                     statisticalPathPlot = TRUE, totalEffects = TRUE, localTests = FALSE,
                                     localTestType = "cis", localTestBootstrap = FALSE, localTestBootstrapSamples = 1000))
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", "debug", options)

  table <- results[["results"]][["modelSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(749.493320586277, 785.96570319011, 4, "Model 1", 100, -360.746660293138,
                                      5))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_covariancesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.504547088211434, 0.104510217482129, -0.240321179427596, "contcor1",
                                      "<unicode>", 0.121930541800229, "contGamma", 0.155374616701564,
                                      -1.54672098010187, 1.77329997335421, 3.06208155476733, 2.32480222173548,
                                      "contGamma", "<unicode>", 1.53743684450092e-12, "contGamma",
                                      0.328776852936806, 7.07106416090163, 0.777105932360688, 1.37218432851018,
                                      1.07344821779, "contNormal", "<unicode>", 1.53743684450092e-12,
                                      "contNormal", 0.151808502820305, 7.07106781140341, 0.00500047485324554,
                                      0.00859013710132828, 0.00647531022139937, "debCollin1", "<unicode>",
                                      1.53743684450092e-12, "debCollin1", 0.000915746992393108, 7.07106905639682,
                                      0.748768133799764, 1.31065740716131, 1.01357919508438, "contcor1",
                                      "<unicode>", 1.53743684450092e-12, "contcor1", 0.143341734285338,
                                      7.07106831194561))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_mediationEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.190956789788724, 0.179714799601129, 16, 0.0174323733798352,
                                      "contGamma", "contNormal", "", "<unicode>", "", 0.85373817115482,
                                      0.0945608165031762, 0.184350918535582, -0.176161478356171, 0.0978993044200254,
                                      50, -0.0292524385521792, "contGamma", "contNormal", "", "<unicode>",
                                      "", 0.675653564866377, 0.06991474969386, -0.418401534444, -0.281099611020402,
                                      0.129693424568323, 84, -0.080202977289446, "contGamma", "contNormal",
                                      "", "<unicode>", "", 0.444078455443482, 0.104796067384148, -0.765324303587158,
                                      -0.0178703105626821, 0.0288190536878919, "", 0.00298071207899204,
                                      "contGamma", "debCollin1", "contNormal", "<unicode>", "<unicode>",
                                      0.80239131693294, 0.0119107709679499, 0.250253496353234))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_pathCoefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.176008115605802, 0.097996371247381, -0.0290779947192265, "contGamma",
                                      "<unicode>", 0.677415936551726, "contNormal", 0.0699003882251143,
                                      -0.415991891569769, -3.13474368031614, 1.92627924309315, -0.326538590442452,
                                      "debCollin1", "<unicode>", 0.800334029069255, "contNormal",
                                      1.29110100066379, -0.252914830268562, -0.0987648268858987, 0.594868218345105,
                                      0.261103818832221, "contcor1", "<unicode>", 0.140057795336072,
                                      "contNormal", 0.176950456922243, 1.47557583842215, -0.170982282263468,
                                      0.10218294772372, -0.0479241299320574, "contGamma:contcor1",
                                      "<unicode>", 0.491633860420964, "contNormal", 0.0696862881516905,
                                      -0.68771247835353, -0.018628944631329, 0.00205891127460755,
                                      -0.00912820771031456, "contGamma", "<unicode>", 0.0837000278081541,
                                      "debCollin1", 0.00527761123906351, -1.72960972243464))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_totalEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.185247768110493, 0.184954521048108, 16, 0.0204130854588272,
                                      "Total", "contGamma", "contNormal", "<unicode>", 0.828873774494712,
                                      0.0944410948565151, 0.216146217807417, -0.168690463730101, 0.101377032919165,
                                      50, -0.0262717264731872, "Total", "contGamma", "contNormal",
                                      "<unicode>", 0.70296268972986, 0.0688960355342047, -0.38132421219134,
                                      -0.273074796118108, 0.132617352791238, 84, -0.0772222652104539,
                                      "Total", "contGamma", "contNormal", "<unicode>", 0.455579011178567,
                                      0.103494796871115, -0.746146352715889, -0.0178703105626821,
                                      0.0288190536878919, "", 0.00298071207899204, "Total indirect",
                                      "contGamma", "contNormal", "<unicode>", 0.80239131693294, 0.0119107709679499,
                                      0.250253496353234))
})

test_that("Missing values work", {
  options <- jaspTools::analysisOptions("ClassicProcess")
  options$dependent <- "contNormal"
  options$covariates <- list("contGamma", "debMiss1", "debMiss30", "debMiss80", "contNormal")
  options$factors <- list("facGender", "facExperim")
  options$statisticalPathPlotsCovariances <- TRUE
  options$statisticalPathPlotsResidualVariances <- TRUE
  options$errorCalculationMethod <- "standard"
  options$ciLevel <- 0.95
  options$naAction <- "fiml"
  options$emulation <- "lavaan"
  options$estimator <- "default"
  options$moderationProbes <- list(list(probePercentile = 16, value = "16"), list(probePercentile = 50,
                                                                                  value = "50"), list(probePercentile = 84, value = "84"))
  options$pathPlotsLegend <- TRUE
  options$pathPlotsColorPalette <- "colorblind"
  options$processModels <- list(list(conceptualPathPlot = TRUE, independentCovariances = TRUE,
                                     inputType = "inputVariables", mediationEffects = TRUE, mediatorCovariances = TRUE,
                                     modelNumber = 1, modelNumberCovariates = list(), modelNumberIndependent = "",
                                     modelNumberMediators = list(), modelNumberModeratorW = "",
                                     modelNumberModeratorZ = "", name = "Model 1", pathCoefficients = TRUE, intercepts = FALSE,
                                     processRelationships = list(list(processDependent = "contNormal",
                                                                      processIndependent = "debMiss1", processType = "mediators",
                                                                      processVariable = "debMiss80"), list(processDependent = "contNormal",
                                                                                                            processIndependent = "debMiss1", processType = "moderators",
                                                                                                            processVariable = "debMiss30")), residualCovariances = TRUE,
                                     statisticalPathPlot = TRUE, totalEffects = TRUE, localTests = FALSE,
                                     localTestType = "cis", localTestBootstrap = FALSE, localTestBootstrapSamples = 1000))
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", "debug", options)

  table <- results[["results"]][["modelSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1633.95668168149, 1665.23417274585, 4, "Model 1", 69, -802.978340840746,
                                      5))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_covariancesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-110.494461910264, 193.953812615562, 41.7296753526491, "debMiss30",
                                      "<unicode>", 0.591066612852734, "debMiss1", 77.6668033002837,
                                      0.537291012111176, 479.941573546073, 960.646127774613, 720.293850660343,
                                      "debMiss1", "<unicode>", 4.26250990059884e-09, "debMiss1", 122.630966186184,
                                      5.87367019164442, 0.575040606573285, 1.50092717023376, 1.03798388840352,
                                      "contNormal", "<unicode>", 1.11020512898463e-05, "contNormal",
                                      0.236199892182649, 4.39451465795279, 115.541023544714, 819.391923391939,
                                      467.466473468326, "debMiss80", "<unicode>", 0.00922929168317954,
                                      "debMiss80", 179.557100385291, 2.60344187150074, 383.414147929555,
                                      767.437820778683, 575.425984354119, "debMiss30", "<unicode>",
                                      4.26250923446503e-09, "debMiss30", 97.9670228326279, 5.87367021795903
                                 ))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_mediationEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.00240725924760687, 0.0318799753326526, 16, 0.0147363580425228,
                                      "debMiss1", "contNormal", "", "<unicode>", "", 0.0920365699106518,
                                      0.00874690424179035, 1.6847512714403, -0.00531454395846721,
                                      0.0204294107364655, 50, 0.00755743338899916, "debMiss1", "contNormal",
                                      "", "<unicode>", "", 0.249839277770421, 0.00656745605990665,
                                      1.15073984813331, -0.0143974276249338, 0.016192998151605, 84,
                                      0.000897785263335586, "debMiss1", "contNormal", "", "<unicode>",
                                      "", 0.908410023215281, 0.00780382344212247, 0.115044281818273,
                                      -0.00561727350073337, 0.0137357253091405, "", 0.00405922590420357,
                                      "debMiss1", "debMiss80", "contNormal", "<unicode>", "<unicode>",
                                      0.410967855868118, 0.00493708021232223, 0.822191605085194))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_pathCoefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.00353329547993103, 0.023923072975966, 0.0101948887480175, "debMiss1",
                                      "<unicode>", 0.145526000981534, "contNormal", 0.00700430433224012,
                                      1.45551767376689, -0.0392651628589268, 0.00506600291672997,
                                      -0.0170995799710984, "debMiss80", "<unicode>", 0.130531572103709,
                                      "contNormal", 0.0113091786699489, -1.51200900349519, -0.018755941108979,
                                      0.00250596857890688, -0.00812498626503604, "debMiss30", "<unicode>",
                                      0.134145223722653, "contNormal", 0.00542405622133801, -1.49795391741566,
                                      -0.000711499389714979, 0.000124746250889994, -0.000293376569412492,
                                      "debMiss1:debMiss30", "<unicode>", 0.169065736491731, "contNormal",
                                      0.000213331889565618, -1.37521197608975, -0.669874147126378,
                                      0.195099221535874, -0.237387462795252, "debMiss1", "<unicode>",
                                      0.282014951413891, "debMiss80", 0.220660526286465, -1.07580393643706
                                 ))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_totalEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.00390635118951979, 0.0336848167039331, 16, 0.0187955839467264,
                                      "Total", "debMiss1", "contNormal", "<unicode>", 0.0133541749541057,
                                      0.00759668691601019, 2.47418172613041, 0.00199406108792504,
                                      0.0212392574984804, 50, 0.0116166592932027, "Total", "debMiss1",
                                      "contNormal", "<unicode>", 0.0179755575686842, 0.0049095790949117,
                                      2.36612122314971, -0.00769347000856958, 0.0176074923436479,
                                      84, 0.00495701116753915, "Total", "debMiss1", "contNormal",
                                      "<unicode>", 0.442487475308531, 0.0064544457326227, 0.767999511171801,
                                      -0.00561727350073337, 0.0137357253091405, "", 0.00405922590420357,
                                      "Total indirect", "debMiss1", "contNormal", "<unicode>", 0.410967855868118,
                                      0.00493708021232223, 0.822191605085194))

  plotName <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 1"]][["collection"]][["pathPlotContainer_Model 1_conceptPathPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "conceptual-path-plot-missing")

  plotName <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 1"]][["collection"]][["pathPlotContainer_Model 1_statPathPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "statistical-path-plot-missing")
})

test_that("Not implemented Hayes models error message work", {
  modelNumber <- 20
  options <- jaspTools::analysisOptions("ClassicProcess")
  options$dependent <- "contNormal"
  options$covariates <- list("contGamma", "debCollin1", "contcor1", "contNormal")
  options$factors <- list()
  options$statisticalPathPlotsCovariances <- TRUE
  options$statisticalPathPlotsResidualVariances <- TRUE
  options$errorCalculationMethod <- "standard"
  options$ciLevel <- 0.95
  options$naAction <- "fiml"
  options$emulation <- "lavaan"
  options$estimator <- "default"
  options$moderationProbes <- list(list(probePercentile = 16, value = "16"), list(probePercentile = 50,
                                                                                  value = "50"), list(probePercentile = 84, value = "84"))
  options$pathPlotsLegend <- TRUE
  options$pathPlotsColorPalette <- "colorblind"
  options$processModels <- list(list(conceptualPathPlot = TRUE, independentCovariances = TRUE,
                                     inputType = "inputModelNumber", mediationEffects = TRUE, mediatorCovariances = TRUE,
                                     modelNumber = modelNumber, modelNumberCovariates = list(), modelNumberIndependent = "",
                                     modelNumberMediators = list(), modelNumberModeratorW = "",
                                     modelNumberModeratorZ = "", name = "Model 1", pathCoefficients = TRUE, intercepts = FALSE,
                                     residualCovariances = TRUE, processRelationships = list(),
                                     statisticalPathPlot = TRUE, totalEffects = TRUE, localTests = TRUE,
                                     localTestType = "cis", localTestBootstrap = FALSE, localTestBootstrapSamples = 1000),
                                list(conceptualPathPlot = TRUE, independentCovariances = TRUE,
                                     inputType = "inputVariables", mediationEffects = TRUE, mediatorCovariances = TRUE,
                                     modelNumber = 1, modelNumberCovariates = list(), modelNumberIndependent = "",
                                     modelNumberMediators = list(), modelNumberModeratorW = "",
                                     modelNumberModeratorZ = "", name = "Model 2", pathCoefficients = TRUE, intercepts = FALSE,
                                     processRelationships = list(list(processDependent = "contNormal",
                                                                      processIndependent = "contGamma", processType = "mediators",
                                                                      processVariable = "debCollin1"), list(processDependent = "contNormal",
                                                                                                           processIndependent = "contGamma", processType = "moderators",
                                                                                                           processVariable = "contcor1")), residualCovariances = TRUE,
                                     statisticalPathPlot = TRUE, totalEffects = TRUE, localTests = TRUE,
                                     localTestType = "cis", localTestBootstrap = FALSE, localTestBootstrapSamples = 1000))
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", "debug", options)

  refMsg <- gettextf("Model 1: Hayes model %s not implemented", modelNumber)

  msg <- results[["results"]][["localTestContainer"]][["collection"]][["localTestContainer_Model 1"]][["collection"]][["localTestContainer_Model 1_localTestTable"]][["error"]][["errorMessage"]]
  expect_equal(msg, refMsg)

  msg <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_pathCoefficientsTable"]][["error"]][["errorMessage"]]
  expect_equal(msg, refMsg)

  table <- results[["results"]][["localTestContainer"]][["collection"]][["localTestContainer_Model 2"]][["collection"]][["localTestContainer_Model 2_localTestTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.19692523949692, 0.197938827903528, "contGamma", 0.000527073663605505,
                                      "contcor1", "<unicode>", "<unicode>", 0.995879545677552, "debCollin1"
                                 ))

  table <- results[["results"]][["modelSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(749.493320586277, 785.96570319011, 4, "Model 2", 100, -360.746660293138,
                                      5))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 2"]][["collection"]][["parEstContainer_Model 2_covariancesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.544849832274378, 0.064207473419186, -0.240321179427596, "contcor1",
                                      "<unicode>", 0.121930541800229, "contGamma", 0.155374616701564,
                                      -1.54672098010187, 1.68041143102892, 2.96919301244204, 2.32480222173548,
                                      "contGamma", "<unicode>", 1.53743684450092e-12, "contGamma",
                                      0.328776852936806, 7.07106416090163, 0.775909019715258, 1.37098741586475,
                                      1.07344821779, "contNormal", "<unicode>", 1.53743684450092e-12,
                                      "contNormal", 0.151808502820305, 7.07106781140341, 0.004680479097358,
                                      0.00827014134544073, 0.00647531022139937, "debCollin1", "<unicode>",
                                      1.53743684450092e-12, "debCollin1", 0.000915746992393108, 7.07106905639682,
                                      0.73263455840361, 1.29452383176516, 1.01357919508438, "contcor1",
                                      "<unicode>", 1.53743684450092e-12, "contcor1", 0.143341734285338,
                                      7.07106831194561))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 2"]][["collection"]][["parEstContainer_Model 2_mediationEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.167903421315091, 0.202768168074761, 16, 0.0174323733798352,
                                      "contGamma", "contNormal", "", "<unicode>", "", 0.85373817115482,
                                      0.0945608165031762, 0.184350918535582, -0.166282829940277, 0.107777952835919,
                                      50, -0.0292524385521792, "contGamma", "contNormal", "", "<unicode>",
                                      "", 0.675653564866377, 0.06991474969386, -0.418401534444, -0.285599495083809,
                                      0.125193540504917, 84, -0.080202977289446, "contGamma", "contNormal",
                                      "", "<unicode>", "", 0.444078455443482, 0.104796067384148, -0.765324303587158,
                                      -0.0203639700462949, 0.026325394204279, "", 0.00298071207899204,
                                      "contGamma", "debCollin1", "contNormal", "<unicode>", "<unicode>",
                                      0.80239131693294, 0.0119107709679499, 0.250253496353234))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 2"]][["collection"]][["parEstContainer_Model 2_pathCoefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.166080238145818, 0.107924248707365, -0.0290779947192265, "contGamma",
                                      "<unicode>", 0.677415936551726, "contNormal", 0.0699003882251143,
                                      -0.415991891569769, -2.8570500521471, 2.20397287126219, -0.326538590442452,
                                      "debCollin1", "<unicode>", 0.800334029069255, "contNormal",
                                      1.29110100066379, -0.252914830268562, -0.085712703783281, 0.607920341447722,
                                      0.261103818832221, "contcor1", "<unicode>", 0.140057795336072,
                                      "contNormal", 0.176950456922243, 1.47557583842215, -0.184506744925651,
                                      0.0886584850615364, -0.0479241299320574, "contGamma:contcor1",
                                      "<unicode>", 0.491633860420964, "contNormal", 0.0696862881516905,
                                      -0.68771247835353, -0.0194721356632828, 0.00121572024265372,
                                      -0.00912820771031456, "contGamma", "<unicode>", 0.0837000278081541,
                                      "debCollin1", 0.00527761123906351, -1.72960972243464))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 2"]][["collection"]][["parEstContainer_Model 2_totalEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.164688059120473, 0.205514230038128, 16, 0.0204130854588272,
                                      "Total", "contGamma", "contNormal", "<unicode>", 0.828873774494712,
                                      0.0944410948565151, 0.216146217807417, -0.16130547479782, 0.108762021851446,
                                      50, -0.0262717264731872, "Total", "contGamma", "contNormal",
                                      "<unicode>", 0.70296268972986, 0.0688960355342047, -0.38132421219134,
                                      -0.280068339665127, 0.125623809244219, 84, -0.0772222652104539,
                                      "Total", "contGamma", "contNormal", "<unicode>", 0.455579011178567,
                                      0.103494796871115, -0.746146352715889, -0.0203639700462949,
                                      0.026325394204279, "", 0.00298071207899204, "Total indirect",
                                      "contGamma", "contNormal", "<unicode>", 0.80239131693294, 0.0119107709679499,
                                      0.250253496353234))

  plotName <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 2"]][["collection"]][["pathPlotContainer_Model 2_conceptPathPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "conceptual-path-plot-error-hayes")

  plotName <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 2"]][["collection"]][["pathPlotContainer_Model 2_statPathPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "statistical-path-plot-error-hayes")
})

test_that("No implied conditional independencies error message works", {
  options <- jaspTools::analysisOptions("ClassicProcess")
  options$dependent <- "contNormal"
  options$covariates <- list("contGamma", "debCollin1", "contcor1", "contNormal")
  options$factors <- list("facGender")
  options$statisticalPathPlotsCovariances <- TRUE
  options$statisticalPathPlotsResidualVariances <- TRUE
  options$errorCalculationMethod <- "standard"
  options$ciLevel <- 0.95
  options$naAction <- "fiml"
  options$emulation <- "lavaan"
  options$estimator <- "default"
  options$moderationProbes <- list(list(probePercentile = 16, value = "16"), list(probePercentile = 50,
                                                                                  value = "50"), list(probePercentile = 84, value = "84"))
  options$pathPlotsLegend <- TRUE
  options$pathPlotsColorPalette <- "colorblind"
  options$processModels <- list(list(conceptualPathPlot = TRUE, independentCovariances = TRUE,
                                     inputType = "inputVariables", mediationEffects = TRUE, mediatorCovariances = TRUE,
                                     modelNumber = 1, modelNumberCovariates = list(), modelNumberIndependent = "",
                                     modelNumberMediators = list(), modelNumberModeratorW = "",
                                     modelNumberModeratorZ = "", name = "Model 1", pathCoefficients = TRUE, intercepts = FALSE,
                                     processRelationships = list(list(processDependent = "contNormal",
                                                                      processIndependent = "contGamma", processType = "mediators",
                                                                      processVariable = "debCollin1")), residualCovariances = TRUE,
                                     statisticalPathPlot = TRUE, totalEffects = TRUE, localTests = TRUE,
                                     localTestType = "cis", localTestBootstrap = FALSE, localTestBootstrapSamples = 1000))
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", "debug", options)

  refMsg <- "The specified model does not imply any (conditional) independencies that can be tested."

  msg <- results[["results"]][["localTestContainer"]][["collection"]][["localTestContainer_Model 1"]][["collection"]][["localTestContainer_Model 1_localTestTable"]][["error"]][["errorMessage"]]
  expect_equal(msg, refMsg)


  table <- results[["results"]][["modelSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(459.709342975141, 483.155874649034, 0, "Model 1", 100, -220.854671487571,
                                      4))
})

test_that("Invalid test type error message works", {
  options <- jaspTools::analysisOptions("ClassicProcess")
  options$dependent <- "contNormal"
  options$covariates <- list("contGamma", "debCollin1", "contcor1", "contNormal")
  options$factors <- list("facGender")
  options$statisticalPathPlotsCovariances <- TRUE
  options$statisticalPathPlotsResidualVariances <- TRUE
  options$errorCalculationMethod <- "standard"
  options$ciLevel <- 0.95
  options$naAction <- "fiml"
  options$emulation <- "lavaan"
  options$estimator <- "default"
  options$moderationProbes <- list(list(probePercentile = 16, value = "16"), list(probePercentile = 50,
                                                                                  value = "50"), list(probePercentile = 84, value = "84"))
  options$pathPlotsLegend <- TRUE
  options$pathPlotsColorPalette <- "colorblind"
  options$processModels <- list(list(conceptualPathPlot = TRUE, independentCovariances = TRUE,
                                     inputType = "inputVariables", mediationEffects = TRUE, mediatorCovariances = TRUE,
                                     modelNumber = 1, modelNumberCovariates = list(), modelNumberIndependent = "",
                                     modelNumberMediators = list(), modelNumberModeratorW = "",
                                     modelNumberModeratorZ = "", name = "Model 1", pathCoefficients = TRUE, intercepts = FALSE,
                                     processRelationships = list(list(processDependent = "contNormal",
                                                                      processIndependent = "contGamma", processType = "mediators",
                                                                      processVariable = "debCollin1"), list(processDependent = "contNormal",
                                                                                                            processIndependent = "contGamma", processType = "moderators",
                                                                                                            processVariable = "facGender")), residualCovariances = TRUE,
                                     statisticalPathPlot = TRUE, totalEffects = TRUE, localTests = TRUE,
                                     localTestType = "cis", localTestBootstrap = FALSE, localTestBootstrapSamples = 1000))
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", "debug", options)

  refMsg <- gettext("Linear test type cannot be applied to factor variables. Choose a different test type or remove all factor variables from the model.")

  msg <- results[["results"]][["localTestContainer"]][["collection"]][["localTestContainer_Model 1"]][["collection"]][["localTestContainer_Model 1_localTestTable"]][["error"]][["errorMessage"]]
  expect_equal(msg, refMsg)


  table <- results[["results"]][["modelSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(605.478429431628, 641.950812035461, 4, "Model 1", 100, -288.739214715814,
                                      5))
})

test_that("Local tests work for factors with loess test type", {
  options <- jaspTools::analysisOptions("ClassicProcess")
  options$dependent <- "contNormal"
  options$covariates <- list("contGamma", "debCollin1", "contcor1", "contNormal")
  options$factors <- list("facGender")
  options$statisticalPathPlotsCovariances <- TRUE
  options$statisticalPathPlotsResidualVariances <- TRUE
  options$errorCalculationMethod <- "standard"
  options$ciLevel <- 0.95
  options$naAction <- "fiml"
  options$emulation <- "lavaan"
  options$estimator <- "default"
  options$moderationProbes <- list(list(probePercentile = 16, value = "16"), list(probePercentile = 50,
                                                                                  value = "50"), list(probePercentile = 84, value = "84"))
  options$pathPlotsLegend <- TRUE
  options$pathPlotsColorPalette <- "colorblind"
  options$processModels <- list(list(conceptualPathPlot = TRUE, independentCovariances = TRUE,
                                     inputType = "inputVariables", mediationEffects = TRUE, mediatorCovariances = TRUE,
                                     modelNumber = 1, modelNumberCovariates = list(), modelNumberIndependent = "",
                                     modelNumberMediators = list(), modelNumberModeratorW = "",
                                     modelNumberModeratorZ = "", name = "Model 1", pathCoefficients = TRUE, intercepts = FALSE,
                                     processRelationships = list(list(processDependent = "contNormal",
                                                                      processIndependent = "contGamma", processType = "mediators",
                                                                      processVariable = "debCollin1"), list(processDependent = "contNormal",
                                                                                                            processIndependent = "contGamma", processType = "moderators",
                                                                                                            processVariable = "facGender")), residualCovariances = TRUE,
                                     statisticalPathPlot = TRUE, totalEffects = TRUE, localTests = TRUE,
                                     localTestType = "cis.loess", localTestBootstrap = FALSE, localTestBootstrapSamples = 1000))
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", "debug", options)
  table <- results[["results"]][["localTestContainer"]][["collection"]][["localTestContainer_Model 1"]][["collection"]][["localTestContainer_Model 1_localTestTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.206103108541346, 0.17035274701811, "contGamma", -0.0139864082891252,
                                      "debCollin1", "<unicode>", "<unicode>", "facGenderm", 0.0989527098094575
                                 ))
})

test_that("Path plots for empty moderator model works", {
  options <- jaspTools::analysisOptions("ClassicProcess")
  options$dependent <- "contNormal"
  options$covariates <- list("contGamma", "debCollin1", "contcor1", "contNormal")
  options$factors <- list("facGender")
  options$statisticalPathPlotsCovariances <- TRUE
  options$statisticalPathPlotsResidualVariances <- TRUE
  options$errorCalculationMethod <- "standard"
  options$ciLevel <- 0.95
  options$naAction <- "fiml"
  options$emulation <- "lavaan"
  options$estimator <- "default"
  options$moderationProbes <- list(list(probePercentile = 16, value = "16"), list(probePercentile = 50,
                                                                                  value = "50"), list(probePercentile = 84, value = "84"))
  options$pathPlotsLegend <- TRUE
  options$pathPlotsColorPalette <- "colorblind"
  options$processModels <- list(list(conceptualPathPlot = TRUE, independentCovariances = TRUE,
                                     inputType = "inputModelNumber", mediationEffects = TRUE, mediatorCovariances = TRUE,
                                     modelNumber = 91, modelNumberCovariates = list(), modelNumberIndependent = "contGamma",
                                     modelNumberMediators = list("debCollin1"), modelNumberModeratorW = "",
                                     modelNumberModeratorZ = "", name = "Model 1", pathCoefficients = TRUE, intercepts = FALSE,
                                     processRelationships = list(), residualCovariances = TRUE,
                                     statisticalPathPlot = TRUE, totalEffects = TRUE, localTests = TRUE,
                                     localTestType = "cis.loess", localTestBootstrap = FALSE, localTestBootstrapSamples = 1000))
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", "debug", options)

  plotName <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 1"]][["collection"]][["pathPlotContainer_Model 1_conceptPathPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "conceptual-path-plot-empty-mod")

  plotName <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 1"]][["collection"]][["pathPlotContainer_Model 1_statPathPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "statistical-path-plot-empty-mod")
})

test_that("Path plot for multiple dependent variables work", {
  options <- jaspTools::analysisOptions("ClassicProcess")
  options$dependent <- "contNormal"
  options$covariates <- list("contGamma", "debCollin1", "contcor1", "contNormal", "debMiss1")
  options$factors <- list("facGender")
  options$statisticalPathPlotsCovariances <- TRUE
  options$statisticalPathPlotsResidualVariances <- TRUE
  options$errorCalculationMethod <- "standard"
  options$ciLevel <- 0.95
  options$naAction <- "fiml"
  options$emulation <- "lavaan"
  options$estimator <- "default"
  options$moderationProbes <- list(list(probePercentile = 16, value = "16"), list(probePercentile = 50,
                                                                                  value = "50"), list(probePercentile = 84, value = "84"))
  options$pathPlotsLegend <- TRUE
  options$pathPlotsColorPalette <- "colorblind"
  options$processModels <- list(list(conceptualPathPlot = TRUE, independentCovariances = TRUE,
                                     inputType = "inputVariables", mediationEffects = TRUE, mediatorCovariances = TRUE,
                                     modelNumber = 1, modelNumberCovariates = list(), modelNumberIndependent = "",
                                     modelNumberMediators = list(), modelNumberModeratorW = "",
                                     modelNumberModeratorZ = "", name = "Model 1", pathCoefficients = TRUE, intercepts = FALSE,
                                     processRelationships = list(list(processDependent = "contNormal",
                                                                      processIndependent = "contGamma", processType = "mediators",
                                                                      processVariable = "debCollin1"), list(processDependent = "contcor1",
                                                                                                            processIndependent = "facGender", processType = "mediators",
                                                                                                            processVariable = "debCollin1"), list(processDependent = "debMiss1",
                                                                                                                                                  processIndependent = "contGamma", processType = "mediators",
                                                                                                                                                  processVariable = "debCollin1")), residualCovariances = TRUE,
                                     statisticalPathPlot = TRUE, totalEffects = TRUE, localTests = FALSE,
                                     localTestType = "cis.loess", localTestBootstrap = FALSE, localTestBootstrapSamples = 1000))
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", "debug", options)
  
  plotName <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 1"]][["collection"]][["pathPlotContainer_Model 1_conceptPathPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "conceptual-path-plot-multi-dep")
  
  plotName <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 1"]][["collection"]][["pathPlotContainer_Model 1_statPathPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "statistical-path-plot-multi-dep")
})

test_that("R-squared table matches", {
  options <- jaspTools::analysisOptions("ClassicProcess")
  options$rSquared <- TRUE
  options$dependent <- "contNormal"
  options$covariates <- list("contGamma", "debCollin1", "contcor1", "contNormal", "debMiss1")
  options$factors <- list("facGender")
  options$statisticalPathPlotsCovariances <- TRUE
  options$statisticalPathPlotsResidualVariances <- TRUE
  options$errorCalculationMethod <- "standard"
  options$ciLevel <- 0.95
  options$naAction <- "listwise"
  options$emulation <- "lavaan"
  options$estimator <- "default"
  options$moderationProbes <- list(list(probePercentile = 16, value = "16"), list(probePercentile = 50,
                                                                                  value = "50"), list(probePercentile = 84, value = "84"))
  options$pathPlotsLegend <- TRUE
  options$pathPlotsColorPalette <- "colorblind"
  options$processModels <- list(list(conceptualPathPlot = TRUE, independentCovariances = TRUE,
                                     inputType = "inputVariables", mediationEffects = TRUE, mediatorCovariances = TRUE,
                                     modelNumber = 1, modelNumberCovariates = list(), modelNumberIndependent = "",
                                     modelNumberMediators = list(), modelNumberModeratorW = "",
                                     modelNumberModeratorZ = "", name = "Model 1", pathCoefficients = TRUE, intercepts = FALSE,
                                     processRelationships = list(list(processDependent = "contNormal",
                                                                      processIndependent = "contGamma", processType = "mediators",
                                                                      processVariable = "debCollin1"), list(processDependent = "contcor1",
                                                                                                            processIndependent = "facGender", processType = "mediators",
                                                                                                            processVariable = "debCollin1"), list(processDependent = "debMiss1",
                                                                                                                                                  processIndependent = "contGamma", processType = "mediators",
                                                                                                                                                  processVariable = "debCollin1")), residualCovariances = TRUE,
                                     statisticalPathPlot = TRUE, totalEffects = TRUE, localTests = FALSE,
                                     localTestType = "cis.loess", localTestBootstrap = FALSE, localTestBootstrapSamples = 1000))
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", "debug", options)
  
  table <- results[["results"]][["rSquaredTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("contNormal", 0.00181189494871836, "debCollin1", 0.0265249455063975,
                                      "debMiss1", 0.0250288495182258, "contcor1", 0.0112314309960128
                                 ))
})

test_that("Path coefficients table with intercepts matches", {
  options <- jaspTools::analysisOptions("ClassicProcess")
  options$dependent <- "contNormal"
  options$covariates <- list("contGamma", "debCollin1", "contcor1", "contNormal", "debMiss1")
  options$factors <- list("facGender")
  options$statisticalPathPlotsCovariances <- TRUE
  options$statisticalPathPlotsResidualVariances <- TRUE
  options$errorCalculationMethod <- "standard"
  options$ciLevel <- 0.95
  options$naAction <- "listwise"
  options$emulation <- "lavaan"
  options$estimator <- "default"
  options$moderationProbes <- list(list(probePercentile = 16, value = "16"), list(probePercentile = 50,
                                                                                  value = "50"), list(probePercentile = 84, value = "84"))
  options$pathPlotsLegend <- TRUE
  options$pathPlotsColorPalette <- "colorblind"
  options$processModels <- list(list(conceptualPathPlot = TRUE, independentCovariances = TRUE,
                                     inputType = "inputVariables", mediationEffects = TRUE, mediatorCovariances = TRUE,
                                     modelNumber = 1, modelNumberCovariates = list(), modelNumberIndependent = "",
                                     modelNumberMediators = list(), modelNumberModeratorW = "",
                                     modelNumberModeratorZ = "", name = "Model 1", pathCoefficients = TRUE, intercepts = TRUE,
                                     processRelationships = list(list(processDependent = "contNormal",
                                                                      processIndependent = "contGamma", processType = "mediators",
                                                                      processVariable = "debCollin1"), list(processDependent = "contcor1",
                                                                                                            processIndependent = "facGender", processType = "mediators",
                                                                                                            processVariable = "debCollin1"), list(processDependent = "debMiss1",
                                                                                                                                                  processIndependent = "contGamma", processType = "mediators",
                                                                                                                                                  processVariable = "debCollin1")), residualCovariances = TRUE,
                                     statisticalPathPlot = TRUE, totalEffects = TRUE, localTests = FALSE,
                                     localTestType = "cis.loess", localTestBootstrap = FALSE, localTestBootstrapSamples = 1000))
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", "debug", options)
  
  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_pathCoefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1.81842415998067, 1.75850994333305, -0.0299571083238086, "(Intercept)",
                                      "<unicode>", 0.973810386382783, "contNormal", 0.91249995702169,
                                      -0.0328297092983825, 0.653479549943803, 0.712058021425658, 0.68276878568473,
                                      "(Intercept)", "<unicode>", 0, "debCollin1", 0.0149437622180597,
                                      45.6892163915453, -4.9479870283803, 83.8830280464201, 39.4675205090199,
                                      "(Intercept)", "<unicode>", 0.0815750282662664, "debMiss1",
                                      22.66138964172, 1.74161960643224, -1.53702416025488, 1.76773422033608,
                                      0.115355030040601, "(Intercept)", "<unicode>", 0.891166767987515,
                                      "contcor1", 0.843066098831018, 0.136827978494866, 1.7137813220542,
                                      2.31240655389555, 2.01309393797488, "(Intercept)", "<unicode>",
                                      0, "contGamma", 0.15271332447005, 13.1821761130587, 0.406563633033025,
                                      0.603537375908213, 0.505050504470619, "(Intercept)", "<unicode>",
                                      0, "facGenderm", 0.0502493271378688, 10.0508908922286, -0.165995350051199,
                                      0.106498401702712, -0.0297484741742436, "contGamma", "<unicode>",
                                      0.668692399817187, "contNormal", 0.0695149895363656, -0.427943302195006,
                                      -2.73898714357846, 2.42947526958326, -0.154755936997602, "debCollin1",
                                      "<unicode>", 0.906565370265694, "contNormal", 1.31850953740219,
                                      -0.117371875293759, -0.0185979214400448, 0.00248650372610035,
                                      -0.00805570885697224, "contGamma", "<unicode>", 0.134215466465715,
                                      "debCollin1", 0.00537877872564405, -1.49768363189316, -0.0391876523573127,
                                      0.0248902735585444, -0.00714868939938416, "facGenderm", "<unicode>",
                                      0.661881681066528, "debCollin1", 0.0163467100470457, -0.437316706469394,
                                      -3.99105068626878, 2.81795395174919, -0.586548367259797, "contGamma",
                                      "<unicode>", 0.735608480658925, "debMiss1", 1.73702289728957,
                                      -0.337674516654351, -116.26393123917, 12.0332112282069, -52.1153600054816,
                                      "debCollin1", "<unicode>", 0.111315707132311, "debMiss1", 32.7294642859176,
                                      -1.59230715022443, -2.35410384171132, 2.52278252053418, 0.0843393394114301,
                                      "debCollin1", "<unicode>", 0.945952802832767, "contcor1", 1.24412652495499,
                                      0.067790001836414, -0.596452849320671, 0.17019409854064, -0.213129375390016,
                                      "facGenderm", "<unicode>", 0.275824268295476, "contcor1", 0.195576794754527,
                                      -1.08974776714957))
})
