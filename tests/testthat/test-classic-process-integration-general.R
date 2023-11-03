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
                                     modelNumberModeratorZ = "", name = "Model 1", pathCoefficients = TRUE,
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
                                 list(-0.18855287140525, -0.093047091209467, -0.140799981307359, "facThreeC",
                                      "<unicode>", 7.51558348888182e-09, "facThreeB", 0.0243641671349884,
                                      -5.7789778130836, -0.0429566774419793, 0.137322520212358, 0.0471829213851896,
                                      "contcor1", "<unicode>", 0.304925061828328, "facThreeB", 0.0459904363234113,
                                      1.02592897909019, -0.111386692251663, 0.0439392202030715, -0.0337237360242957,
                                      "contcor1", "<unicode>", 0.394725492097922, "facThreeC", 0.0396246853717531,
                                      -0.851078960196263, 0.166537554341246, 0.294262457180447, 0.230400005760847,
                                      "facThreeB", "<unicode>", 1.53743684450092e-12, "facThreeB",
                                      0.0325834821064772, 7.0710676350655, 0.124035773561631, 0.219164194184828,
                                      0.17159998387323, "facThreeC", "<unicode>", 1.53743684450092e-12,
                                      "facThreeC", 0.0242679001689719, 7.0710684763996, 0.656578232571939,
                                      1.1601372201591, 0.90835772636552, "contcor1", "<unicode>",
                                      1.53743684450092e-12, "contcor1", 0.128461285911162, 7.07106207074478,
                                      0.562065668856282, 0.993138292339328, 0.777601980597805, "contNormal",
                                      "<unicode>", 1.53743684450092e-12, "contNormal", 0.109969526706432,
                                      7.07106781202798))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_mediationEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.830256392676731, 0.671610737591095, 16, -0.0793228275428182,
                                      "B", "facThree", "contNormal", "<unicode>", 0.835982155918959,
                                      0.383136410187729, -0.207035472050155, -0.525905203875692, 1.15334878334855,
                                      16, 0.313721789736431, "C", "facThree", "contNormal", "<unicode>",
                                      0.463967628519699, 0.428388990938096, 0.732329253021736, -0.419840075271681,
                                      0.605570327467686, 50, 0.0928651260980023, "B", "facThree",
                                      "contNormal", "<unicode>", 0.722586733024018, 0.261589093174077,
                                      0.355003815224835, -0.343080242598669, 0.839989902178562, 50,
                                      0.248454829789947, "C", "facThree", "contNormal", "<unicode>",
                                      0.410383843368507, 0.301809154175571, 0.823218336331221, -0.539700679220641,
                                      1.2482888689735, 84, 0.354294094876428, "B", "facThree", "contNormal",
                                      "<unicode>", 0.437310753173408, 0.456128164164641, 0.776742421782455,
                                      -0.891019364021505, 1.18974240632265, 84, 0.149361521150572,
                                      "C", "facThree", "contNormal", "<unicode>", 0.778418345536092,
                                      0.530816327941977, 0.281380796498216))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_pathCoefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.384277115427941, 0.649582479358737, 0.132652681965398, "facThreeB",
                                      "<unicode>", 0.61499311862582, "contNormal", 0.263744538915416,
                                      0.502958971248843, -0.36868442105099, 0.835431543005484, 0.233373560977247,
                                      "facThreeC", "<unicode>", 0.44741374451908, "contNormal", 0.307178084279708,
                                      0.759733760058037, -0.772565531217873, 0.487083275478801, -0.142741127869536,
                                      "contcor1", "<unicode>", 0.656898600865411, "contNormal", 0.321344886087863,
                                      -0.444199158129772, -0.440651907510927, 0.890228809967378, 0.224788451228225,
                                      "facThreeB:contcor1", "<unicode>", 0.507917529817297, "contNormal",
                                      0.339516625809485, 0.662083780705226, -0.838061139155147, 0.667651322062499,
                                      -0.0852049085463242, "facThreeC:contcor1", "<unicode>", 0.824454027131575,
                                      "contNormal", 0.384117379986192, -0.22181997739698))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_totalEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.830256392676731, 0.671610737591095, 16, -0.0793228275428182,
                                      "B", "Total", 0.835982155918959, 0.383136410187729, -0.207035472050155,
                                      -0.525905203875692, 1.15334878334855, 16, 0.313721789736431,
                                      "C", "Total", 0.463967628519699, 0.428388990938096, 0.732329253021736,
                                      -0.419840075271681, 0.605570327467686, 50, 0.0928651260980023,
                                      "B", "Total", 0.722586733024018, 0.261589093174077, 0.355003815224835,
                                      -0.343080242598669, 0.839989902178562, 50, 0.248454829789947,
                                      "C", "Total", 0.410383843368507, 0.301809154175571, 0.823218336331221,
                                      -0.539700679220641, 1.2482888689735, 84, 0.354294094876428,
                                      "B", "Total", 0.437310753173408, 0.456128164164641, 0.776742421782455,
                                      -0.891019364021505, 1.18974240632265, 84, 0.149361521150572,
                                      "C", "Total", 0.778418345536092, 0.530816327941977, 0.281380796498216
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
                                     modelNumberModeratorZ = "", name = "Model 1", pathCoefficients = TRUE,
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
                                 list(-0.188552899318699, -0.0930470981392201, -0.140799998728959, "facThreeC",
                                      "<unicode>", 7.51560835787757e-09, "facThreeB", 0.0243641724880702,
                                      -5.77897725842736, -0.0625879470174146, 0.0313879697933625,
                                      -0.015599988612026, "facTwoE", "<unicode>", 0.515235336014761,
                                      "facThreeB", 0.0239738886918451, -0.650707476477626, -0.00972474511250287,
                                      0.0721247068049682, 0.0311999808462327, "facTwoE", "<unicode>",
                                      0.135116244943659, "facThreeC", 0.0208803459051006, 1.49422720236695,
                                      0.166537561194698, 0.294262481084964, 0.230400021139831, "facThreeB",
                                      "<unicode>", 1.53743684450092e-12, "facThreeB", 0.0325834864563185,
                                      7.07106716307678, 0.124035784194595, 0.219164231272074, 0.171600007733335,
                                      "facThreeC", "<unicode>", 1.53743684450092e-12, "facThreeC",
                                      0.0242679069176374, 7.07106749320103, 0.179548301980595, 0.317251715854165,
                                      0.24840000891738, "facTwoE", "<unicode>", 1.53743684450092e-12,
                                      "facTwoE", 0.0351290674113803, 7.0710675580562, 0.564294089241247,
                                      0.99707578530309, 0.780684937272169, "contNormal", "<unicode>",
                                      1.53743684450092e-12, "contNormal", 0.110405522620714, 7.0710678120163
                                 ))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_mediationEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.79179509702597, 0.581685251376472, -0.105054922824749, "B",
                                      0, "facThree", "contNormal", "<unicode>", 0.76430843174253,
                                      0.350384078288244, -0.299827901250483, -0.835536962301828, 0.956997866716982,
                                      0.0607304522075771, "C", 0, "facThree", "contNormal", "<unicode>",
                                      0.894346977764828, 0.457287695885765, 0.132805786715827, -0.346979683280772,
                                      1.19016835503621, 0.421594335877721, "B", 1, "facThree", "contNormal",
                                      "<unicode>", 0.282320764613395, 0.39213680721733, 1.07512053986828,
                                      -0.273316897248801, 1.39971560986316, 0.563199356307179, "C",
                                      1, "facThree", "contNormal", "<unicode>", 0.186975149551964,
                                      0.426801849500457, 1.31958040239602))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_pathCoefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.79179509702597, 0.581685251376472, -0.105054922824749, "facThreeB",
                                      "<unicode>", 0.76430843174253, "contNormal", 0.350384078288244,
                                      -0.299827901250483, -0.835536962301828, 0.956997866716982, 0.0607304522075771,
                                      "facThreeC", "<unicode>", 0.894346977764828, "contNormal", 0.457287695885765,
                                      0.132805786715827, -1.5098265891821, 0.360680618984523, -0.574572985098788,
                                      "facTwoE", "<unicode>", 0.228549486740747, "contNormal", 0.477178974440588,
                                      -1.20410373439521, -0.504038904791374, 1.55733742219631, 0.52664925870247,
                                      "facThreeB:facTwoE", "<unicode>", 0.31659480567209, "contNormal",
                                      0.525870970907517, 1.00147999763822, -0.723522417690523, 1.72846022588973,
                                      0.502468904099602, "facThreeC:facTwoE", "<unicode>", 0.421809782363961,
                                      "contNormal", 0.625517270450166, 0.803285421261016))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_totalEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.79179509702597, 0.581685251376472, -0.105054922824749, "B",
                                      0, "Total", 0.76430843174253, 0.350384078288244, -0.299827901250483,
                                      -0.835536962301828, 0.956997866716982, 0.0607304522075771, "C",
                                      0, "Total", 0.894346977764828, 0.457287695885765, 0.132805786715827,
                                      -0.346979683280772, 1.19016835503621, 0.421594335877721, "B",
                                      1, "Total", 0.282320764613395, 0.39213680721733, 1.07512053986828,
                                      -0.273316897248801, 1.39971560986316, 0.563199356307179, "C",
                                      1, "Total", 0.186975149551964, 0.426801849500457, 1.31958040239602
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
                                     modelNumberModeratorZ = "", name = "Model 1", pathCoefficients = TRUE,
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
                                 list(-0.0625879470174092, 0.0313879697933571, -0.015599988612026, "facThreeB",
                                      "<unicode>", 0.515235336014713, "facTwoE", 0.0239738886918424,
                                      -0.650707476477701, -0.00972474511269001, 0.0721247068051553,
                                      0.0311999808462326, "facThreeC", "<unicode>", 0.135116244945445,
                                      "facTwoE", 0.020880345905196, 1.49422720236012, -0.188552899318718,
                                      -0.0930470981392011, -0.140799998728959, "facThreeC", "<unicode>",
                                      7.51560835787757e-09, "facThreeB", 0.0243641724880799, -5.77897725842506,
                                      0.17954830198084, 0.317251715853921, 0.24840000891738, "facTwoE",
                                      "<unicode>", 1.53743684450092e-12, "facTwoE", 0.0351290674112554,
                                      7.07106755808134, 0.16653756119465, 0.294262481085012, 0.230400021139831,
                                      "facThreeB", "<unicode>", 1.53743684450092e-12, "facThreeB",
                                      0.0325834864563429, 7.07106716307149, 0.124035784194594, 0.219164231272075,
                                      0.171600007733335, "facThreeC", "<unicode>", 1.53743684450092e-12,
                                      "facThreeC", 0.024267906917638, 7.07106749320088, 0.564294089243091,
                                      0.997075785301246, 0.780684937272169, "contNormal", "<unicode>",
                                      1.53743684450092e-12, "contNormal", 0.110405522619773, 7.07106781207657
                                 ))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_mediationEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1.50982658901334, 0.360680618815769, -0.574572985098788, 0, 0,
                                      "E", "facTwo", "contNormal", "<unicode>", 0.228549486656783,
                                      0.477178974354487, -1.20410373461247, -0.481073569262345, 0.385226116469709,
                                      -0.0479237263963183, 1, 0, "E", "facTwo", "contNormal", "<unicode>",
                                      0.828324822977639, 0.220998878695046, -0.216850540958842, -1.50982658901334,
                                      0.360680618815769, -0.574572985098788, 0, 0, "E", "facTwo",
                                      "contNormal", "<unicode>", 0.228549486656783, 0.477178974354487,
                                      -1.20410373461247, -1.50982658901334, 0.360680618815769, -0.574572985098788,
                                      0, 0, "E", "facTwo", "contNormal", "<unicode>", 0.228549486656783,
                                      0.477178974354487, -1.20410373461247, -0.481073569262345, 0.385226116469709,
                                      -0.0479237263963183, 1, 0, "E", "facTwo", "contNormal", "<unicode>",
                                      0.828324822977639, 0.220998878695046, -0.216850540958842, -1.50982658901334,
                                      0.360680618815769, -0.574572985098788, 0, 0, "E", "facTwo",
                                      "contNormal", "<unicode>", 0.228549486656783, 0.477178974354487,
                                      -1.20410373461247, -0.864792807812527, 0.720584645814155, -0.0721040809991859,
                                      0, 1, "E", "facTwo", "contNormal", "<unicode>", 0.858502237173678,
                                      0.404440455572637, -0.178281079465939, -0.845713837967434, 1.754804193374,
                                      0.454545177703284, 1, 1, "E", "facTwo", "contNormal", "<unicode>",
                                      0.493239705610856, 0.663409647282805, 0.685165160870077, -0.864792807812527,
                                      0.720584645814155, -0.0721040809991859, 0, 1, "E", "facTwo",
                                      "contNormal", "<unicode>", 0.858502237173678, 0.404440455572637,
                                      -0.178281079465939))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_pathCoefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1.50982658901334, 0.360680618815769, -0.574572985098788, "facTwoE",
                                      "<unicode>", 0.228549486656783, "contNormal", 0.477178974354487,
                                      -1.20410373461247, -0.791795097032169, 0.581685251382671, -0.105054922824749,
                                      "facThreeB", "<unicode>", 0.764308431744595, "contNormal", 0.350384078291407,
                                      -0.299827901247776, -0.835536962619522, 0.956997867034676, 0.0607304522075774,
                                      "facThreeC", "<unicode>", 0.894346977802058, "contNormal", 0.457287696047857,
                                      0.132805786668753, -0.504038904576221, 1.55733742198116, 0.526649258702469,
                                      "facTwoE:facThreeB", "<unicode>", 0.316594805571069, "contNormal",
                                      0.525870970797743, 1.00147999784728, -0.723522417887957, 1.72846022608716,
                                      0.502468904099602, "facTwoE:facThreeC", "<unicode>", 0.421809782438714,
                                      "contNormal", 0.625517270550899, 0.803285421131654))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_totalEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1.50982658901334, 0.360680618815769, -0.574572985098788, 0, 0,
                                      "E", "Total", 0.228549486656783, 0.477178974354487, -1.20410373461247,
                                      -0.481073569262345, 0.385226116469709, -0.0479237263963183,
                                      1, 0, "E", "Total", 0.828324822977639, 0.220998878695046, -0.216850540958842,
                                      -1.50982658901334, 0.360680618815769, -0.574572985098788, 0,
                                      0, "E", "Total", 0.228549486656783, 0.477178974354487, -1.20410373461247,
                                      -1.50982658901334, 0.360680618815769, -0.574572985098788, 0,
                                      0, "E", "Total", 0.228549486656783, 0.477178974354487, -1.20410373461247,
                                      -0.481073569262345, 0.385226116469709, -0.0479237263963183,
                                      1, 0, "E", "Total", 0.828324822977639, 0.220998878695046, -0.216850540958842,
                                      -1.50982658901334, 0.360680618815769, -0.574572985098788, 0,
                                      0, "E", "Total", 0.228549486656783, 0.477178974354487, -1.20410373461247,
                                      -0.864792807812527, 0.720584645814155, -0.0721040809991859,
                                      0, 1, "E", "Total", 0.858502237173678, 0.404440455572637, -0.178281079465939,
                                      -0.845713837967434, 1.754804193374, 0.454545177703284, 1, 1,
                                      "E", "Total", 0.493239705610856, 0.663409647282805, 0.685165160870077,
                                      -0.864792807812527, 0.720584645814155, -0.0721040809991859,
                                      0, 1, "E", "Total", 0.858502237173678, 0.404440455572637, -0.178281079465939
                                 ))

  plotName <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 1"]][["collection"]][["pathPlotContainer_Model 1_conceptPathPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "conceptual-path-plot-facTwo-int-facThree")

  plotName <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 1"]][["collection"]][["pathPlotContainer_Model 1_statPathPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "statistical-path-plot-facTwo-int-facThree")
})
