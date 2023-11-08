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
                                      0.828324822977639, 0.220998878695046, -0.216850540958842, -0.864792807812527,
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
                                     modelNumberModeratorZ = "", name = "Model 1", pathCoefficients = TRUE,
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
                                     modelNumberModeratorZ = "", name = "Model 1", pathCoefficients = TRUE,
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
                                     modelNumberModeratorZ = "", name = "Model 1", pathCoefficients = TRUE,
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
                                     modelNumberModeratorZ = "", name = "Model 1", pathCoefficients = TRUE,
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
                                     modelNumberModeratorZ = "", name = "Model 1", pathCoefficients = TRUE,
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
                                 list(-0.50454708821106, 0.104510217479924, -0.240321179428066, "contcor1",
                                      "<unicode>", 0.121930541797918, "contGamma", 0.155374616700906,
                                      -1.54672098011145, 1.77329997335402, 3.06208155476696, 2.32480222173537,
                                      "contGamma", "<unicode>", 1.53743684450092e-12, "contGamma",
                                      0.328776852936759, 7.07106416090231, 0.748768133802888, 1.31065740716119,
                                      1.01357919508512, "contcor1", "<unicode>", 1.53743684450092e-12,
                                      "contcor1", 0.143341734284509, 7.07106831199164, 0.00500047485323911,
                                      0.00859013710131692, 0.00647531022139492, "debCollin1", "<unicode>",
                                      1.53743684450092e-12, "debCollin1", 0.000915746992391851, 7.07106905640168,
                                      0.777105932363479, 1.37218432850775, 1.07344821779, "contNormal",
                                      "<unicode>", 1.53743684450092e-12, "contNormal", 0.151808502818974,
                                      7.07106781146542))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_mediationEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.190956789517269, 0.179714799846874, 16, 0.017432373509147,
                                      "contGamma", "contNormal", "", "<unicode>", "", 0.853738170072067,
                                      0.0945608164966175, 0.184350919915867, -0.176161478237461, 0.0978993045402136,
                                      50, -0.0292524384924568, "contGamma", "contNormal", "", "<unicode>",
                                      "", 0.675653565492472, 0.069914749694237, -0.418401533587526,
                                      -0.281099611060772, 0.12969342454386, 84, -0.0802029773056715,
                                      "contGamma", "contNormal", "", "<unicode>", "", 0.444078455368951,
                                      0.104796067388206, -0.765324303712353, -0.0178703105636702,
                                      0.0288190536884062, "", 0.00298071207887824, "contGamma", "debCollin1",
                                      "contNormal", "<unicode>", "<unicode>", 0.802391316946556, 0.0119107709683332,
                                      0.250253496335626))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_pathCoefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.176008115486518, 0.0979963713680356, -0.0290779946592441, "contGamma",
                                      "<unicode>", 0.677415937181174, "contNormal", 0.069900388225464,
                                      -0.415991890709575, -3.13474368037813, 1.92627924320551, -0.326538590430326,
                                      "debCollin1", "<unicode>", 0.800334029083245, "contNormal",
                                      1.29110100070826, -0.252914830250458, -0.0987648269136903, 0.594868218283032,
                                      0.261103818809805, "contcor1", "<unicode>", 0.140057795350511,
                                      "contNormal", 0.176950456913497, 1.4755758383684, -0.170982282402329,
                                      0.102182947576892, -0.0479241300034941, "contGamma:contcor1",
                                      "<unicode>", 0.491633859762656, "contNormal", 0.0696862881496583,
                                      -0.687712479398705, -0.0186289446313068, 0.00205891127462217,
                                      -0.00912820771030503, "contGamma", "<unicode>", 0.0837000278083639,
                                      "debCollin1", 0.00527761123906158, -1.72960972243347))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_totalEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.185247767847583, 0.184954521301923, 16, 0.0204130855880252,
                                      "Total", 0.828873773424247, 0.0944410948541949, 0.216146219180754,
                                      -0.16869046361089, 0.101377033038379, 50, -0.0262717264135785,
                                      "Total", 0.702962690371781, 0.0688960355342054, -0.381324211326139,
                                      -0.273074796149483, 0.132617352757307, 84, -0.0772222652267933,
                                      "Total", 0.455579011080368, 0.103494796870462, -0.746146352878466,
                                      -0.0178703105636702, 0.0288190536884062, 0.00298071207887824,
                                      "Total indirect", 0.802391316946556, 0.0119107709683332, 0.250253496335626
                                 ))
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
                                     modelNumberModeratorZ = "", name = "Model 1", pathCoefficients = TRUE,
                                     processRelationships = list(list(processDependent = "contNormal",
                                                                      processIndependent = "debMiss1", processType = "mediators",
                                                                      processVariable = "debMiss80"), list(processDependent = "contNormal",
                                                                                                            processIndependent = "debMiss1", processType = "moderators",
                                                                                                            processVariable = "debMiss30")), residualCovariances = TRUE,
                                     statisticalPathPlot = TRUE, totalEffects = TRUE, localTests = FALSE,
                                     localTestType = "cis", localTestBootstrap = FALSE, localTestBootstrapSamples = 1000))
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", "debug", options, makeTests = T)

  table <- results[["results"]][["modelSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1633.95668168149, 1665.23417274585, 4, "Model 1", 69, -802.978340840746,
                                      5))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_covariancesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-110.494463632206, 193.953814337481, 41.7296753526376, "debMiss30",
                                      "<unicode>", 0.591066617050381, "debMiss1", 77.6668041788361,
                                      0.537291006033293, 479.941575046104, 960.646126274836, 720.29385066047,
                                      "debMiss1", "<unicode>", 4.26250901242042e-09, "debMiss1", 122.630965420913,
                                      5.87367022829973, 383.414148911908, 767.437819796141, 575.425984354025,
                                      "debMiss30", "<unicode>", 4.26250834628661e-09, "debMiss30",
                                      97.9670223313699, 5.87367024801129, 115.541023077736, 819.391923859025,
                                      467.466473468381, "debMiss80", "<unicode>", 0.00922929177619203,
                                      "debMiss80", 179.557100623577, 2.60344186804607, 0.575040607190625,
                                      1.50092716961641, 1.03798388840352, "contNormal", "<unicode>",
                                      1.11020509905302e-05, "contNormal", 0.236199891867671, 4.39451466381296
                                 ))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_mediationEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.00240725924854762, 0.0318799753335926, 16, 0.0147363580425225,
                                      "debMiss1", "contNormal", "", "<unicode>", "", 0.0920365699284971,
                                      0.00874690424227016, 1.68475127134784, -0.00531454395872133,
                                      0.0204294107367177, 50, 0.0075574333889982, "debMiss1", "contNormal",
                                      "", "<unicode>", "", 0.249839277779795, 0.00656745606003582,
                                      1.15073984811053, -0.0143974276252919, 0.01619299815196, 84,
                                      0.000897785263334051, "debMiss1", "contNormal", "", "<unicode>",
                                      "", 0.908410023217562, 0.00780382344230435, 0.115044281815395,
                                      -0.00561727350119405, 0.013735725309603, "", 0.0040592259042045,
                                      "debMiss1", "debMiss80", "contNormal", "<unicode>", "<unicode>",
                                      0.41096785589033, 0.00493708021255775, 0.82219160504616))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_pathCoefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.00353329548037963, 0.0239230729764131, 0.0101948887480167,
                                      "debMiss1", "<unicode>", 0.145526000994699, "contNormal", 0.00700430433246862,
                                      1.4555176737193, -0.0392651628833341, 0.0050660029411319, -0.0170995799711011,
                                      "debMiss80", "<unicode>", 0.130531572527147, "contNormal", 0.0113091786824004,
                                      -1.51200900183068, -0.0187559411088868, 0.00250596857881399,
                                      -0.00812498626503641, "debMiss30", "<unicode>", 0.134145223719248,
                                      "contNormal", 0.00542405622129081, -1.49795391742876, -0.000711499389746557,
                                      0.000124746250921522, -0.000293376569412517, "debMiss1:debMiss30",
                                      "<unicode>", 0.169065736523859, "contNormal", 0.000213331889581716,
                                      -1.37521197598609, -0.669874147279466, 0.195099221688927, -0.23738746279527,
                                      "debMiss1", "<unicode>", 0.282014951584179, "debMiss80", 0.220660526364564,
                                      -1.07580393605638))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_totalEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.00390635118894364, 0.0336848167045104, 16, 0.018795583946727,
                                      "Total", 0.0133541749576855, 0.00759668691630445, 2.47418172603465,
                                      0.00199406108787663, 0.0212392574985288, 50, 0.0116166592932027,
                                      "Total", 0.0179755575692619, 0.00490957909493638, 2.36612122313781,
                                      -0.0076934700091281, 0.0176074923442052, 84, 0.00495701116753855,
                                      "Total", 0.44248747532871, 0.00645444573290735, 0.767999511137837,
                                      -0.00561727350119405, 0.013735725309603, 0.0040592259042045,
                                      "Total indirect", 0.41096785589033, 0.00493708021255775, 0.82219160504616
                                 ))

  plotName <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 1"]][["collection"]][["pathPlotContainer_Model 1_conceptPathPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "conceptual-path-plot-missing")

  plotName <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 1"]][["collection"]][["pathPlotContainer_Model 1_statPathPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "statistical-path-plot-missing")
})
