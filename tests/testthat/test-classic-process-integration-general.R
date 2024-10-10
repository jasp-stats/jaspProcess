# General integration tests for Classic Process analysis

test_that("Error handling works - observations", {
  options <- getOptionsClassical()
  options$covariates <- list("contGamma", "debNaN")
  options$factors <- list()
  options$processModels <- list(getProcessModel(list(
          list(processDependent = "contNormal", processIndependent = "contGamma",
              processType = "moderators", processVariable = "debNaN"))))
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", "debug.csv", options)
  expect_identical(results[["status"]], "validationError", label = "Observations check")

  options$covariates <- list("contGamma")
  options$factors <- list("debNaN")
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", "debug.csv", options)
  expect_identical(results[["status"]], "validationError", label = "Observations check")
})

test_that("Error handling works - variance", {
  options <- getOptionsClassical()
  options$dependent <- "contNormal"
  options$covariates <- list("contGamma", "debSame")
  options$factors <- list()
  options$processModels <- list(getProcessModel(list(
          list(processDependent = "contNormal", processIndependent = "contGamma",
              processType = "moderators", processVariable = "debSame"))))
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", "debug.csv", options)
  expect_identical(results[["status"]], "validationError", label = "Variance check")
})

test_that("Error handling works - infinity", {
  options <- getOptionsClassical()
  options$dependent <- "contNormal"
  options$covariates <- list("contGamma", "debInf")
  options$factors <- list()
  options$processModels <- list(getProcessModel(list(
          list(processDependent = "contNormal", processIndependent = "contGamma",
              processType = "moderators", processVariable = "debInf"))))
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", "debug.csv", options)
  expect_identical(results[["status"]], "validationError", label = "Infinity check")
})

test_that("Error handling works - covariance", {
  options <- getOptionsClassical()
  options$dependent <- "contNormal"
  options$covariates <- list("debCollin1", "debCollin2")
  options$processModels <- list(getProcessModel(list(
          list(processDependent = "contNormal", processIndependent = "debCollin1",
              processType = "moderators", processVariable = "debCollin2"))))
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", "debug.csv", options)
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

  options <- getOptionsClassical()
  options$covariates <- list("contcor1")
  options$factors <- list("facThree")
  options$standardizedModelEstimates <- FALSE
  options$processModels <- list(getProcessModel(list(list(processDependent = "contNormal",
                                                                      processIndependent = "facThree", processType = "moderators",
                                                                      processVariable = "contcor1"))))
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", df, options)

  table <- results[["results"]][["modelSummaryTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(862.878179310881, 933.21777433256, 0, "Model 1", 100, -404.439089655441,
			 2))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_covariancesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
		list(-0.0135584854454402, 0.0673717151548735, 0.0269066148547166, "facThreeC:contcor1",
			 "<unicode>", 0.192490655666003, "facThreeB", 0.0206458387089459,
			 1.3032463942992, -0.0674630976001248, 0.0840128844415106, 0.00827489342069291,
			 "facThreeB:contcor1", "<unicode>", 0.830438298811261, "facThreeB",
			 0.0386425422192598, 0.214139467681519, -0.0429565430541084,
			 0.137322515846702, 0.0471829863962967, "contcor1", "<unicode>",
			 0.304924023251369, "facThreeB", 0.0459904009264529, 1.0259311822863,
			 -0.188552901159607, -0.0930471043944246, -0.140800002777016,
			 "facThreeC", "<unicode>", 7.51558903999694e-09, "facThreeB",
			 0.0243641713619537, -5.7789776916807, 0.166537554557795, 0.294262457935726,
			 0.230400006246761, "facThreeB", "<unicode>", 1.53743684450092e-12,
			 "facThreeB", 0.0325834822439107, 7.07106762015342, -0.0680079941001269,
			 0.00242312317601424, -0.0327924354620563, "facThreeC:contcor1",
			 "<unicode>", 0.0679856128722165, "facThreeC", 0.0179674519102629,
			 -1.82510216951384, -0.070412285311941, 0.0602985298946496, -0.00505687770864574,
			 "facThreeB:contcor1", "<unicode>", 0.879461177334092, "facThreeC",
			 0.0333452084419971, -0.151652304631474, -0.111386711403246,
			 0.0439390972636155, -0.0337238070698152, "contcor1", "<unicode>",
			 0.394724180298341, "facThreeC", 0.0396246588947684, -0.851081321844962,
			 0.124035781626099, 0.219164222313221, 0.17160000196966, "facThreeC",
			 "<unicode>", 1.53743684450092e-12, "facThreeC", 0.0242679052874142,
			 7.07106773070583, 0.562065668856422, 0.993138292339188, 0.777601980597805,
			 "contNormal", "<unicode>", 1.53743684450092e-12, "contNormal",
			 0.109969526706361, 7.07106781203257, 0.0947538131752763, 0.269326953225664,
			 0.18204038320047, "facThreeC:contcor1", "<unicode>", 4.35858577967352e-05,
			 "contcor1", 0.0445347826356501, 4.0876001279671, 0.452244315818467,
			 0.846176539044167, 0.649210427431317, "facThreeB:contcor1",
			 "<unicode>", 1.04604769290972e-10, "contcor1", 0.100494760703,
			 6.46014203018983, 0.656577904982369, 1.1601360775407, 0.908356991261534,
			 "contcor1", "<unicode>", 1.53743684450092e-12, "contcor1", 0.128461077991823,
			 7.07106779315172, -0.0663074619937669, 0.0682401863741361, 0.000966362190184602,
			 "facThreeC:contcor1", "<unicode>", 0.977539232368448, "facThreeB:contcor1",
			 0.0343240103974353, 0.0281541165788952, 0.468251719543354, 0.827374348594509,
			 0.647813034068932, "facThreeB:contcor1", "<unicode>", 1.53743684450092e-12,
			 "facThreeB:contcor1", 0.09161459901403, 7.07106772327547, 0.131453638791395,
			 0.232271157493681, 0.181862398142538, "facThreeC:contcor1",
			 "<unicode>", 1.53743684450092e-12, "facThreeC:contcor1", 0.0257192273678296,
			 7.07106770905635))

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

  options <- getOptionsClassical()
  options$dependent <- "contNormal"
  options$covariates <- list("contcor1")
  options$factors <- list("facTwo", "facThree")
  options$standardizedModelEstimates <- FALSE
  options$processModels <- list(getProcessModel(list(list(processDependent = "contNormal",
                                                                      processIndependent = "facThree", processType = "moderators",
                                                                      processVariable = "facTwo"))))

  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", df, options)

  table <- results[["results"]][["modelSummaryTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(514.431378962803, 584.770973984482, 0, "Model 1", 100, -230.215689481402,
			 2))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_covariancesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-0.134503192055385, -0.0574968074688675, -0.0959999997621261,
			 "facThreeC:facTwoE", "<unicode>", 1.02499707543835e-06, "facThreeB",
			 0.0196448468425782, -4.88677771485883, 0.0688094372817376, 0.168790565708898,
			 0.118800001495318, "facThreeB:facTwoE", "<unicode>", 3.19678694116199e-06,
			 "facThreeB", 0.0255058585810246, 4.6577534772227, -0.0625879520743083,
			 0.0313879539603316, -0.0155999990569884, "facTwoE", "<unicode>",
			 0.51523500654286, "facThreeB", 0.02397388594278, -0.65070798677452,
			 -0.188552896785847, -0.0930471033621245, -0.140800000073986,
			 "facThreeC", "<unicode>", 7.51558482114945e-09, "facThreeB",
			 0.0243641705095246, -5.7789777829269, 0.166537552858732, 0.294262452009486,
			 0.230400002434109, "facThreeB", "<unicode>", 1.53743684450092e-12,
			 "facThreeB", 0.0325834811655297, 7.07106773716526, 0.0800360974646364,
			 0.153963901823823, 0.11699999964423, "facThreeC:facTwoE", "<unicode>",
			 5.51240830759525e-10, "facThreeC", 0.018859480312475, 6.2037764405861,
			 -0.113342517977593, -0.0318574826307162, -0.0726000003041547,
			 "facThreeB:facTwoE", "<unicode>", 0.000478515311735483, "facThreeC",
			 0.0207873807859789, -3.49250350737422, -0.00972472354867213,
			 0.0721247234039249, 0.0311999999276264, "facTwoE", "<unicode>",
			 0.135115982489033, "facThreeC", 0.0208803446385278, 1.49422820684947,
			 0.12403578034673, 0.219164217850856, 0.171599999098793, "facThreeC",
			 "<unicode>", 1.53743684450092e-12, "facThreeC", 0.0242679044754103,
			 7.07106784900478, 0.564294089241745, 0.997075785302593, 0.780684937272169,
			 "contNormal", "<unicode>", 1.53743684450092e-12, "contNormal",
			 0.11040552262046, 7.07106781203256, 0.0315898374008467, 0.106410162963877,
			 0.0690000001823616, "facThreeC:facTwoE", "<unicode>", 0.000300354537617675,
			 "facTwoE", 0.0190871684768708, 3.61499403465599, 0.0970736494435958,
			 0.206526349390427, 0.151799999417011, "facThreeB:facTwoE", "<unicode>",
			 5.43221352344858e-08, "facTwoE", 0.0279221202048047, 5.43654988602513,
			 0.179548297589777, 0.317251700540701, 0.248399999065239, "facTwoE",
			 "<unicode>", 1.53743684450092e-12, "facTwoE", 0.0351290646249397,
			 7.07106783847836, -0.0838080493714504, -0.0151919518096889,
			 -0.0495000005905697, "facThreeC:facTwoE", "<unicode>", 0.00468608937428727,
			 "facThreeB:facTwoE", 0.0175044281688328, -2.82785590669599,
			 0.159815333282494, 0.282384668491166, 0.22110000088683, "facThreeB:facTwoE",
			 "<unicode>", 1.53743684450092e-12, "facThreeB:facTwoE", 0.0312682621148866,
			 7.07106778350708, 0.0921594526894641, 0.162840548254633, 0.127500000472049,
			 "facThreeC:facTwoE", "<unicode>", 1.53743684450092e-12, "facThreeC:facTwoE",
			 0.0180312230537634, 7.07106778568953))

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
  options <- getOptionsClassical()
  options$dependent <- "contNormal"
  options$covariates <- list("contcor1")
  options$factors <- list("facTwo", "facThree")
  options$standardizedModelEstimates <- FALSE
  options$processModels <- list(getProcessModel(list(list(processDependent = "contNormal",
                                                                      processIndependent = "facTwo", processType = "moderators",
                                                                      processVariable = "facThree"))))
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", df, options)

  table <- results[["results"]][["modelSummaryTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(514.431378962803, 584.770973984482, 0, "Model 1", 100, -230.215689481402,
			 2))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_covariancesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
		list(0.0315898374008497, 0.106410162963889, 0.0690000001823695, "facTwoE:facThreeC",
			 "<unicode>", 0.000300354537617675, "facTwoE", 0.0190871684768732,
			 3.61499403465593, 0.0970736494435923, 0.206526349390425, 0.151799999417009,
			 "facTwoE:facThreeB", "<unicode>", 5.43221352344858e-08, "facTwoE",
			 0.0279221202048051, 5.43654988602496, -0.00972472354866262,
			 0.0721247234039384, 0.0311999999276379, "facThreeC", "<unicode>",
			 0.135115982488908, "facTwoE", 0.0208803446385288, 1.49422820684995,
			 -0.0625879520743221, 0.0313879539603191, -0.0155999990570015,
			 "facThreeB", "<unicode>", 0.515235006542513, "facTwoE", 0.0239738859427804,
			 -0.650707986775057, 0.179548297589781, 0.317251700540712, 0.248399999065247,
			 "facTwoE", "<unicode>", 1.53743684450092e-12, "facTwoE", 0.0351290646249417,
			 7.07106783847818, 0.564294089241745, 0.997075785302593, 0.780684937272169,
			 "contNormal", "<unicode>", 1.53743684450092e-12, "contNormal",
			 0.11040552262046, 7.07106781203256, -0.134503192055396, -0.0574968074688714,
			 -0.0959999997621337, "facTwoE:facThreeC", "<unicode>", 1.02499707543835e-06,
			 "facThreeB", 0.0196448468425801, -4.88677771485876, 0.0688094372817358,
			 0.168790565708897, 0.118800001495317, "facTwoE:facThreeB", "<unicode>",
			 3.19678694116199e-06, "facThreeB", 0.025505858581025, 4.65775347722259,
			 -0.188552896785846, -0.0930471033621235, -0.140800000073985,
			 "facThreeC", "<unicode>", 7.51558482114945e-09, "facThreeB",
			 0.0243641705095246, -5.77897778292687, 0.166537552858728, 0.294262452009479,
			 0.230400002434104, "facThreeB", "<unicode>", 1.53743684450092e-12,
			 "facThreeB", 0.0325834811655287, 7.07106773716531, 0.0800360974646385,
			 0.153963901823833, 0.116999999644236, "facTwoE:facThreeC", "<unicode>",
			 5.51240830759525e-10, "facThreeC", 0.0188594803124769, 6.20377644058576,
			 -0.11334251797759, -0.0318574826307112, -0.0726000003041507,
			 "facTwoE:facThreeB", "<unicode>", 0.000478515311735928, "facThreeC",
			 0.0207873807859794, -3.49250350737394, 0.12403578034673, 0.219164217850857,
			 0.171599999098793, "facThreeC", "<unicode>", 1.53743684450092e-12,
			 "facThreeC", 0.0242679044754107, 7.07106784900466, -0.0838080493714582,
			 -0.0151919518096885, -0.0495000005905733, "facTwoE:facThreeC",
			 "<unicode>", 0.00468608937428927, "facTwoE:facThreeB", 0.0175044281688349,
			 -2.82785590669587, 0.159815333282494, 0.282384668491169, 0.221100000886831,
			 "facTwoE:facThreeB", "<unicode>", 1.53743684450092e-12, "facTwoE:facThreeB",
			 0.0312682621148873, 7.07106778350697, 0.092159452689468, 0.162840548254649,
			 0.127500000472059, "facTwoE:facThreeC", "<unicode>", 1.53743684450092e-12,
			 "facTwoE:facThreeC", 0.0180312230537665, 7.07106778568887))

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

test_that("Moderated moderation with three-level factor works", {
  set.seed(1)
  df <- get_fac_df()
  options <- getOptionsClassical()
  options$dependent <- "contNormal"
  options$covariates <- list("contcor1")
  options$factors <- list("facTwo", "facThree")
  options$standardizedModelEstimates <- FALSE
  options$processModels <- list(getProcessModel(list(list(processDependent = "contNormal",
                                                                      processIndependent = "facTwo", processType = "moderators",
                                                                      processVariable = "facThree"), list(processDependent = "contNormal",
                                                                      processIndependent = "facThree", processType = "moderators",
                                                                      processVariable = "contcor1"))))
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", df, options)

  table <- results[["results"]][["modelSummaryTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(896.420315021798, 1130.88563176073, 0, "Model 1", 100, -358.210157510899
			))

	table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_covariancesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-0.0647404405799154, 0.0190547610224984, -0.0228428397787085,
			 "facThreeC:contcor1", "<unicode>", 0.285256758740135, "facTwoE",
			 0.0213767197416329, -1.06858489304232, -0.0321257283802815,
			 0.126197204205364, 0.047035737912541, "facThreeB:contcor1",
			 "<unicode>", 0.24419675880559, "facTwoE", 0.0403892453724855,
			 1.16456094880602, -0.0585144888668562, 0.0166128194922014, -0.0209508346873274,
			 "facTwoE:facThreeC:contcor1", "<unicode>", 0.274325925714457,
			 "facTwoE", 0.0191654818536596, -1.0931546019714, -0.0334466775588943,
			 0.0881389168638253, 0.0273461196524655, "facTwoE:facThreeB:contcor1",
			 "<unicode>", 0.377971090665321, "facTwoE", 0.0310173032213274,
			 0.881640787960651, -0.0659137457874188, 0.0804690788826471,
			 0.00727766654761418, "facTwoE:contcor1", "<unicode>", 0.84548237904653,
			 "facTwoE", 0.0373432435046549, 0.194885764186686, -0.0571336858553401,
			 0.129608415299585, 0.0362373647221224, "contcor1", "<unicode>",
			 0.446858211609971, "facTwoE", 0.0476391664918138, 0.760663281721129,
			 0.0315898368651065, 0.106410162979911, 0.0689999999225089, "facTwoE:facThreeC",
			 "<unicode>", 0.000300354584309215, "facTwoE", 0.0190871686176322,
			 3.61499399438261, 0.0970736497867169, 0.206526349318317, 0.151799999552517,
			 "facTwoE:facThreeB", "<unicode>", 5.43221274629246e-08, "facTwoE",
			 0.0279221200988767, 5.43654991150275, -0.00972472486143495,
			 0.0721247223803535, 0.0311999987594593, "facThreeC", "<unicode>",
			 0.135115998486152, "facTwoE", 0.0208803447123025, 1.49422814562427,
			 -0.0625879510768201, 0.0313879536939549, -0.0155999986914326,
			 "facThreeB", "<unicode>", 0.515235010737521, "facTwoE", 0.0239738856203596,
			 -0.650707980277691, 0.17954829790904, 0.317251701654265, 0.248399999781653,
			 "facTwoE", "<unicode>", 1.53743684450092e-12, "facTwoE", 0.0351290648275712,
			 7.07106781808477, 0.53806683880113, 0.950733715045322, 0.744400276923226,
			 "contNormal", "<unicode>", 1.53743684450092e-12, "contNormal",
			 0.105274096743424, 7.07106781203256, -0.01355848824292, 0.0673717083533853,
			 0.0269066100552326, "facThreeC:contcor1", "<unicode>", 0.192490712999472,
			 "facThreeB", 0.0206458376874964, 1.30324622630972, -0.0674630979713119,
			 0.0840128762646671, 0.00827488914667762, "facThreeB:contcor1",
			 "<unicode>", 0.830438376455171, "facThreeB", 0.0386425402279844,
			 0.214139368112375, -0.00726226861661647, 0.065560239082203,
			 0.0291489852327933, "facTwoE:facThreeC:contcor1", "<unicode>",
			 0.116637035129035, "facThreeB", 0.0185775116974685, 1.56904679741181,
			 -0.0370700920818681, 0.0798727054579015, 0.0214013066880167,
			 "facTwoE:facThreeB:contcor1", "<unicode>", 0.473144105858585,
			 "facThreeB", 0.0298328944976029, 0.717372787603172, -0.0218135799503326,
			 0.120458841351955, 0.049322630700811, "facTwoE:contcor1", "<unicode>",
			 0.174162315942846, "facThreeB", 0.0362946519488404, 1.35895037016292,
			 -0.0429565505347366, 0.1373224996315, 0.0471829745483817, "contcor1",
			 "<unicode>", 0.30492412125935, "facThreeB", 0.0459903986982044,
			 1.02593097437583, -0.134503191141371, -0.0574968073140734, -0.0959999992277223,
			 "facTwoE:facThreeC", "<unicode>", 1.0249969661924e-06, "facThreeB",
			 0.0196448466488962, -4.88677773583519, 0.0688094371461021, 0.168790563098174,
			 0.118800000122138, "facTwoE:facThreeB", "<unicode>", 3.19678598703632e-06,
			 "facThreeB", 0.0255058579496128, 4.65775353869016, -0.188552894687668,
			 -0.0930471027521321, -0.1407999987199, "facThreeC", "<unicode>",
			 7.51558326683721e-09, "facThreeB", 0.0243641701298783, -5.77897781739892,
			 0.166537551196334, 0.294262446211143, 0.230399998703739, "facThreeB",
			 "<unicode>", 1.53743684450092e-12, "facThreeB", 0.032583480110422,
			 7.07106785165172, -0.0680079895725037, 0.00242312546501389,
			 -0.0327924320537449, "facThreeC:contcor1", "<unicode>", 0.0679856327402371,
			 "facThreeC", 0.0179674513391749, -1.8251020378303, -0.0704122854253892,
			 0.0602985255434097, -0.00505687994098978, "facThreeB:contcor1",
			 "<unicode>", 0.879461120651116, "facThreeC", 0.0333452073609079,
			 -0.151652376494686, -0.0673310007718806, -0.00371965140823739,
			 -0.035525326090059, "facTwoE:facThreeC:contcor1", "<unicode>",
			 0.028583730525827, "facThreeC", 0.0162276832292331, -2.18918040167695,
			 -0.063475385434144, 0.0373182280832325, -0.0130785786754557,
			 "facTwoE:facThreeB:contcor1", "<unicode>", 0.611008600866985,
			 "facThreeC", 0.0257131289943141, -0.50863427311191, -0.110602230965303,
			 0.0125504069340649, -0.0490259120156192, "facTwoE:contcor1",
			 "<unicode>", 0.118644929310228, "facThreeC", 0.0314170665560134,
			 -1.56048662048734, -0.111386704563909, 0.0439390999047015, -0.033723802329604,
			 "contcor1", "<unicode>", 0.394724233969102, "facThreeC", 0.0396246578237664,
			 -0.851081225220747, 0.0800360974617812, 0.153963902370214, 0.116999999915998,
			 "facTwoE:facThreeC", "<unicode>", 5.51240830759525e-10, "facThreeC",
			 0.0188594804525914, 6.20377640890531, -0.113342517156058, -0.0318574824210341,
			 -0.0725999997885461, "facTwoE:facThreeB", "<unicode>", 0.000478515309190186,
			 "facThreeC", 0.020787380629891, -3.49250350879473, 0.124035780533313,
			 0.219164218501642, 0.171599999517477, "facThreeC", "<unicode>",
			 1.53743684450092e-12, "facThreeC", 0.024267904593832, 7.07106783175224,
			 -0.0255221693729147, 0.0532696070554647, 0.013873718841275,
			 "facThreeC:contcor1", "<unicode>", 0.490053305500653, "facTwoE:facThreeB",
			 0.020100312314379, 0.690224043501567, -0.0230071536532204, 0.126732658942249,
			 0.0518627526445144, "facThreeB:contcor1", "<unicode>", 0.174566293233757,
			 "facTwoE:facThreeB", 0.0381996337117922, 1.35767670014345, -0.0203200301296559,
			 0.0503799192652839, 0.015029944567814, "facTwoE:facThreeC:contcor1",
			 "<unicode>", 0.404659349069155, "facTwoE:facThreeB", 0.0180360327925952,
			 0.833328744777213, -0.0178322551704392, 0.0974926805142516,
			 0.0398302126719062, "facTwoE:facThreeB:contcor1", "<unicode>",
			 0.175787280408379, "facTwoE:facThreeB", 0.0294201670526497,
			 1.35384046598467, -0.0156254328943641, 0.124079721966893, 0.0542271445362646,
			 "facTwoE:contcor1", "<unicode>", 0.128125244250909, "facTwoE:facThreeB",
			 0.0356397250059781, 1.52153655863418, -0.0170349409205557, 0.160884406541541,
			 0.0719247328104926, "contcor1", "<unicode>", 0.113046034042289,
			 "facTwoE:facThreeB", 0.0453884226612075, 1.58464931349036, -0.0838080490447984,
			 -0.0151919516782063, -0.0495000003615023, "facTwoE:facThreeC",
			 "<unicode>", 0.00468608944810067, "facTwoE:facThreeB", 0.0175044281190438,
			 -2.8278559016532, 0.159815332393884, 0.282384665391753, 0.221099998892819,
			 "facTwoE:facThreeB", "<unicode>", 1.53743684450092e-12, "facTwoE:facThreeB",
			 0.0312682615508959, 7.07106784727799, -0.0700592359582211, -0.00841887101993133,
			 -0.0392390534890762, "facThreeC:contcor1", "<unicode>", 0.0125833067611103,
			 "facTwoE:facThreeC", 0.015724871840631, -2.49534965287842, -0.0597804015624259,
			 0.0528846630997589, -0.0034478692313335, "facThreeB:contcor1",
			 "<unicode>", 0.904514146266322, "facTwoE:facThreeC", 0.0287416160579665,
			 -0.119960868740985, -0.0665195766460744, -0.0109074164719913,
			 -0.0387134965590328, "facTwoE:facThreeC:contcor1", "<unicode>",
			 0.00635664374957434, "facTwoE:facThreeC", 0.0141870362447332,
			 -2.72879380098889, -0.0523371362691562, 0.0345027163187342,
			 -0.00891720997521098, "facTwoE:facThreeB:contcor1", "<unicode>",
			 0.687301005298292, "facTwoE:facThreeC", 0.0221534306938475,
			 -0.402520498898959, -0.101180258857533, 0.00534338123513378,
			 -0.0479184388111996, "facTwoE:contcor1", "<unicode>", 0.0778440599429486,
			 "facTwoE:facThreeC", 0.0271748973279386, -1.76333467732864,
			 -0.107031221814729, 0.027283071280335, -0.0398740752671969,
			 "contcor1", "<unicode>", 0.24453969365972, "facTwoE:facThreeC",
			 0.0342644798972118, -1.16371459268645, 0.0921594530719362, 0.162840549588674,
			 0.127500001330305, "facTwoE:facThreeC", "<unicode>", 1.53743684450092e-12,
			 "facTwoE:facThreeC", 0.0180312232965149, 7.07106773809121, 0.0947538092169325,
			 0.26932694389359, 0.182040376555261, "facThreeC:contcor1", "<unicode>",
			 4.35858621896656e-05, "contcor1", 0.0445347812647754, 4.08760010457816,
			 0.452244312479584, 0.84617652445836, 0.649210418468972, "facThreeB:contcor1",
			 "<unicode>", 1.04604769290972e-10, "contcor1", 0.100494757833833,
			 6.4601421254473, 0.0695020168518365, 0.223413978613155, 0.146457997732496,
			 "facTwoE:facThreeC:contcor1", "<unicode>", 0.000191414681856683,
			 "contcor1", 0.0392639770361487, 3.73008566090129, 0.251316914988796,
			 0.52884147333732, 0.390079194163058, "facTwoE:facThreeB:contcor1",
			 "<unicode>", 3.59406902017412e-08, "contcor1", 0.0707983821482441,
			 5.50971904056048, 0.383943552889945, 0.740127142648494, 0.562035347769219,
			 "facTwoE:contcor1", "<unicode>", 6.19450712946445e-10, "contcor1",
			 0.0908648303152708, 6.18540028984969, 0.656577900575943, 1.16013606217267,
			 0.908356981374304, "contcor1", "<unicode>", 1.53743684450092e-12,
			 "contcor1", 0.128461075195444, 7.07106787011013, 0.0797682255393317,
			 0.217922001585344, 0.148845113562338, "facThreeC:contcor1",
			 "<unicode>", 2.40772045629889e-05, "facTwoE:contcor1", 0.0352439578318151,
			 4.2232803214846, 0.246995023813811, 0.527940817766583, 0.387467920790197,
			 "facThreeB:contcor1", "<unicode>", 6.43795052823748e-08, "facTwoE:contcor1",
			 0.0716711623705427, 5.40618999294267, 0.0856438150732335, 0.212157277420783,
			 0.148900546247008, "facTwoE:facThreeC:contcor1", "<unicode>",
			 3.95800311547845e-06, "facTwoE:contcor1", 0.0322744354859252,
			 4.613575543775, 0.268423037208156, 0.505359059997472, 0.386891048602814,
			 "facTwoE:facThreeB:contcor1", "<unicode>", 1.54543711161637e-10,
			 "facTwoE:contcor1", 0.0604439736286578, 6.40082088215624, 0.405636672175093,
			 0.716737085654376, 0.561186878914734, "facTwoE:contcor1", "<unicode>",
			 1.53743684450092e-12, "facTwoE:contcor1", 0.0793638087059771,
			 7.07106788427695, -0.0493177684373359, 0.0543163433021549, 0.00249928743240947,
			 "facThreeC:contcor1", "<unicode>", 0.924684365240192, "facTwoE:facThreeB:contcor1",
			 0.0264377592029607, 0.0945347679893224, 0.262768527895877, 0.510161703386686,
			 0.386465115641282, "facThreeB:contcor1", "<unicode>", 9.15341802354419e-10,
			 "facTwoE:facThreeB:contcor1", 0.0631116636433667, 6.12351336236564,
			 -0.0437379245494166, 0.0491530788606395, 0.00270757715561143,
			 "facTwoE:facThreeC:contcor1", "<unicode>", 0.909033555843793,
			 "facTwoE:facThreeB:contcor1", 0.0236971199835223, 0.11425764639307,
			 0.277777628619867, 0.490817379750527, 0.384297504185197, "facTwoE:facThreeB:contcor1",
			 "<unicode>", 1.53743684450092e-12, "facTwoE:facThreeB:contcor1",
			 0.0543478739433711, 7.07106785052206, 0.103340351420703, 0.189190012604559,
			 0.146265182012631, "facThreeC:contcor1", "<unicode>", 2.41362485553509e-11,
			 "facTwoE:facThreeC:contcor1", 0.0219008262042126, 6.67852347892233,
			 -0.0592519329511364, 0.0613457249446211, 0.00104689599674232,
			 "facThreeB:contcor1", "<unicode>", 0.972854427471433, "facTwoE:facThreeC:contcor1",
			 0.0307652739660056, 0.0340284958261415, 0.105607940957352, 0.186603267000096,
			 0.146105603978724, "facTwoE:facThreeC:contcor1", "<unicode>",
			 1.53743684450092e-12, "facTwoE:facThreeC:contcor1", 0.0206624526475039,
			 7.07106781906524, -0.066307463779303, 0.0682401805749952, 0.000966358397846081,
			 "facThreeC:contcor1", "<unicode>", 0.977539319819127, "facThreeB:contcor1",
			 0.0343240093735377, 0.0281540069322758, 0.468251714248099, 0.82737433012588,
			 0.647813022186989, "facThreeB:contcor1", "<unicode>", 1.53743684450092e-12,
			 "facThreeB:contcor1", 0.0916145956534135, 7.07106785296228,
			 0.131453637865303, 0.232271154263525, 0.181862396064414, "facThreeC:contcor1",
			 "<unicode>", 1.53743684450092e-12, "facThreeC:contcor1", 0.0257192267800475,
			 7.07106778985671))

	table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_mediationEffectsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-1.97167451459102, 0.988455559187214, 16, -0.491609477701903,
			 0, 0, "E", "facTwo", "contNormal", "<unicode>", 0.515040148944578,
			 0.755149098944512, -0.651009818311425, -1.32086292755252, 1.30662687795768,
			 16, -0.00711802479741981, 1, 0, "E", "facTwo", "contNormal",
			 "<unicode>", 0.991527170977456, 0.670290328351823, -0.010619315983452,
			 -1.56978265523222, 1.37220783244855, 16, -0.098787411391833,
			 0, 1, "E", "facTwo", "contNormal", "<unicode>", 0.895280899949492,
			 0.750521568479527, -0.131625013245076, -1.48866165823005, 0.395273169690407,
			 50, -0.546694244269821, 0, 0, "E", "facTwo", "contNormal", "<unicode>",
			 0.255323516020266, 0.480604450587024, -1.1375139027574, -0.549301757463352,
			 0.424896174732677, 50, -0.0622027913653374, 1, 0, "E", "facTwo",
			 "contNormal", "<unicode>", 0.80236431071549, 0.248524447357293,
			 -0.250288420422121, -0.975557482799032, 0.66781312687953, 50,
			 -0.153872177959751, 0, 1, "E", "facTwo", "contNormal", "<unicode>",
			 0.713595941883554, 0.419234899886238, -0.367030936597847, -2.30404731122581,
			 1.04339098060135, 84, -0.630328165312234, 0, 0, "E", "facTwo",
			 "contNormal", "<unicode>", 0.460436112387406, 0.853954031357548,
			 -0.73812891814585, -1.51326314908243, 1.22158972426693, 84,
			 -0.14583671240775, 1, 0, "E", "facTwo", "contNormal", "<unicode>",
			 0.834423931041378, 0.697679369345951, -0.20903113781975, -1.75663624366286,
			 1.28162404565854, 84, -0.237506099002163, 0, 1, "E", "facTwo",
			 "contNormal", "<unicode>", 0.759279098738087, 0.775080642625786,
			 -0.306427597259493))

	table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_pathCoefficientsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-1.4952508133346, 0.376405396119968, -0.559422708607316, "facTwoE",
			 "<unicode>", 0.241343887504401, "contNormal", 0.477472092400155,
			 -1.17163435834587, -0.808220377635, 0.59436777094962, -0.10692630334269,
			 "facThreeB", "<unicode>", 0.765065412434103, "contNormal", 0.357809673965455,
			 -0.29883569708351, -0.829205888637198, 0.969201818414954, 0.0699979648888782,
			 "facThreeC", "<unicode>", 0.878735667018079, "contNormal", 0.45878590658751,
			 0.152572177749594, -0.54442917372999, 1.51341207953896, 0.484491452904484,
			 "facTwoE:facThreeB", "<unicode>", 0.35606187749693, "contNormal",
			 0.524969149816256, 0.922895094071833, -0.830525387533374, 1.61616952015351,
			 0.39282206631007, "facTwoE:facThreeC", "<unicode>", 0.529118134551158,
			 "contNormal", 0.624168333445437, 0.629352764728827, -0.823871404172618,
			 0.735349738299696, -0.0442608329364609, "contcor1", "<unicode>",
			 0.911399833375425, "contNormal", 0.397767804605404, -0.111273040261187,
			 -1.38918101804154, 1.24535656225063, -0.0719122278954538, "facTwoE:contcor1",
			 "<unicode>", 0.914790415422004, "contNormal", 0.67208826311939,
			 -0.106998190329473, -0.977745206488471, 1.79543315427082, 0.408843973891173,
			 "facTwoE:facThreeB:contcor1", "<unicode>", 0.563326972037531,
			 "contNormal", 0.707456459055821, 0.577906906718783, -1.95071036467509,
			 1.37116042215519, -0.289774971259951, "facTwoE:facThreeC:contcor1",
			 "<unicode>", 0.732392304254536, "contNormal", 0.84743158880285,
			 -0.34194497241873, -0.91705304127427, 0.779294465264945, -0.0688792880046625,
			 "facThreeB:contcor1", "<unicode>", 0.873537625823345, "contNormal",
			 0.432749662728445, -0.159166589686946, -1.10340180178156, 1.27857845239661,
			 0.0875883253075207, "facThreeC:contcor1", "<unicode>", 0.885389490670545,
			 "contNormal", 0.607659189905255, 0.144140542532036))

	table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_totalEffectsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-1.97167451459102, 0.988455559187214, 16, -0.491609477701903,
			 0, 0, "E", "Total", "facTwo", "contNormal", "<unicode>", 0.515040148944578,
			 0.755149098944512, -0.651009818311425, -1.32086292755252, 1.30662687795768,
			 16, -0.00711802479741981, 1, 0, "E", "Total", "facTwo", "contNormal",
			 "<unicode>", 0.991527170977456, 0.670290328351823, -0.010619315983452,
			 -1.56978265523222, 1.37220783244855, 16, -0.098787411391833,
			 0, 1, "E", "Total", "facTwo", "contNormal", "<unicode>", 0.895280899949492,
			 0.750521568479527, -0.131625013245076, -1.48866165823005, 0.395273169690407,
			 50, -0.546694244269821, 0, 0, "E", "Total", "facTwo", "contNormal",
			 "<unicode>", 0.255323516020266, 0.480604450587024, -1.1375139027574,
			 -0.549301757463352, 0.424896174732677, 50, -0.0622027913653374,
			 1, 0, "E", "Total", "facTwo", "contNormal", "<unicode>", 0.80236431071549,
			 0.248524447357293, -0.250288420422121, -0.975557482799032, 0.66781312687953,
			 50, -0.153872177959751, 0, 1, "E", "Total", "facTwo", "contNormal",
			 "<unicode>", 0.713595941883554, 0.419234899886238, -0.367030936597847,
			 -2.30404731122581, 1.04339098060135, 84, -0.630328165312234,
			 0, 0, "E", "Total", "facTwo", "contNormal", "<unicode>", 0.460436112387406,
			 0.853954031357548, -0.73812891814585, -1.51326314908243, 1.22158972426693,
			 84, -0.14583671240775, 1, 0, "E", "Total", "facTwo", "contNormal",
			 "<unicode>", 0.834423931041378, 0.697679369345951, -0.20903113781975,
			 -1.75663624366286, 1.28162404565854, 84, -0.237506099002163,
			 0, 1, "E", "Total", "facTwo", "contNormal", "<unicode>", 0.759279098738087,
			 0.775080642625786, -0.306427597259493))

	plotName <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 1"]][["collection"]][["pathPlotContainer_Model 1_conceptPathPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "conceptual-path-plot-mod-mod-facThree")

	plotName <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 1"]][["collection"]][["pathPlotContainer_Model 1_statPathPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "statistical-path-plot-mod-mod-facThree")
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

test_that("Mean-centering works", {
  N <- 100

  set.seed(1)
  df <- data.frame(x = rnorm(N, 1, 2), y = rnorm(N), w = rnorm(N, 3, 4))

  options <- getOptionsClassical()
  options$dependent <- "y"
  options$covariates <- list("x", "w")
  options$standardizedModelEstimates <- FALSE
  options$meanCenteredModeration <- TRUE

  options$processModels <- list(getProcessModel(list(list(processDependent = "y",
                                                                      processIndependent = "x", processType = "moderators",
                                                                      processVariable = "w"))))
  set.seed(1)
  results1 <- jaspTools::runAnalysis("ClassicProcess", df, options)

  df$x <- scale(df$x, scale = FALSE)
  df$w <- scale(df$w, scale = FALSE)

  options$meanCenteredModeration <- FALSE

  set.seed(1)
  results2 <- jaspTools::runAnalysis("ClassicProcess", df, options)

  checkTables(results1, results2)
})

test_that("Mean-centering works - missing values/listwise", {
  N <- 100

  set.seed(1)
  df <- data.frame(x = rnorm(N, 1, 2), y = rnorm(N), w = rnorm(N, 3, 4))

  df$x[1:10] <- NA
  df$y[11:20] <- NA
  df$w[21:30] <- NA

  options <- getOptionsClassical()
  options$dependent <- "y"
  options$covariates <- list("x", "w")
  options$standardizedModelEstimates <- FALSE
  options$meanCenteredModeration <- TRUE

  options$processModels <- list(getProcessModel(list(list(processDependent = "y",
                                                                      processIndependent = "x", processType = "moderators",
                                                                      processVariable = "w"))))
  set.seed(1)
  results1 <- jaspTools::runAnalysis("ClassicProcess", df, options)

  df$x[complete.cases(df)] <- scale(df$x[complete.cases(df)], scale = FALSE)
  df$w[complete.cases(df)] <- scale(df$w[complete.cases(df)], scale = FALSE)

  options$meanCenteredModeration <- FALSE

  set.seed(1)
  results2 <- jaspTools::runAnalysis("ClassicProcess", df, options)

  checkTables(results1, results2)
})

test_that("Mean-centering works - missing values/fiml", {
  N <- 100

  set.seed(1)
  df <- data.frame(x = rnorm(N, 1, 2), y = rnorm(N), w = rnorm(N, 3, 4))

  df$x[1:10] <- NA
  df$y[11:20] <- NA
  df$w[21:30] <- NA

  options <- getOptionsClassical()
  options$dependent <- "y"
  options$covariates <- list("x", "w")
  options$standardizedModelEstimates <- FALSE
  options$meanCenteredModeration <- TRUE

  options$processModels <- list(getProcessModel(list(list(processDependent = "y",
                                                                      processIndependent = "x", processType = "moderators",
                                                                      processVariable = "w"))))
  set.seed(1)
  results1 <- jaspTools::runAnalysis("ClassicProcess", df, options)

  df$x <- scale(df$x, scale = FALSE)
  df$w <- scale(df$w, scale = FALSE)

  options$meanCenteredModeration <- FALSE

  set.seed(1)
  results2 <- jaspTools::runAnalysis("ClassicProcess", df, options)

  checkTables(results1, results2)
})

test_that("Mean-centering works - moderated moderation", {
  N <- 100

  set.seed(1)
  df <- data.frame(x = rnorm(N, 1, 2), y = rnorm(N), w = rnorm(N, 3, 4), z = rnorm(N, 3, 4))

  options <- getOptionsClassical()
  options$dependent <- "y"
  options$covariates <- list("x", "w")
  options$standardizedModelEstimates <- FALSE
  options$meanCenteredModeration <- TRUE

  options$processModels <- list(getProcessModel(list(list(processDependent = "y",
                                                                      processIndependent = "x", processType = "moderators",
                                                                      processVariable = "w"))))
  set.seed(1)
  results1 <- jaspTools::runAnalysis("ClassicProcess", df, options)

  df$x <- scale(df$x, scale = FALSE)
  df$w <- scale(df$w, scale = FALSE)
  df$z <- scale(df$z, scale = FALSE)

  options$meanCenteredModeration <- FALSE

  set.seed(1)
  results2 <- jaspTools::runAnalysis("ClassicProcess", df, options)

  checkTables(results1, results2)
})

test_that("Bootstrapping works", {
  options <- getOptionsClassical()
  options$errorCalculationMethod <- "bootstrap"
  options$bootstrapSamples <- 50
  options$bootstrapCiType <- "bca.simple"
  options$standardizedModelEstimates <- FALSE
  options$processModels <- list(getProcessModel(list(list(processDependent = "contNormal",
                                                                      processIndependent = "contGamma", processType = "mediators",
                                                                      processVariable = "debCollin1"), list(processDependent = "contNormal",
                                                                                                            processIndependent = "contGamma", processType = "moderators",
                                                                                                            processVariable = "contcor1"))))
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", "debug.csv", options)

  table <- results[["results"]][["modelSummaryTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1121.15455096096, 1168.04761430875, 2, "Model 1", 100, -542.577275480482,
			 5))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_covariancesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-1.21372033215787, 0.332014108125832, -0.591842270846295, "contGamma:contcor1",
			 "<unicode>", 0.133383729438247, "contGamma", 0.394327256132322,
			 -1.5008911041333, -0.504543968063766, 0.104512925436125, -0.240319813025714,
			 "contcor1", "<unicode>", 0.121932410808084, "contGamma", 0.155374511548185,
			 -1.54671323263459, 1.77329815422817, 3.06207843633816, 2.32480104984646,
			 "contGamma", "<unicode>", 1.53743684450092e-12, "contGamma",
			 0.328776521475834, 7.07106772530695, 0.777105840758683, 1.37218424543037,
			 1.07344822547951, "contNormal", "<unicode>", 1.53743684450092e-12,
			 "contNormal", 0.151808504994376, 7.07106776079031, 0.00500047633157313,
			 0.00859013987956438, 0.00647531139383704, "debCollin1", "<unicode>",
			 1.53743684450092e-12, "debCollin1", 0.000915747324008519, 7.07106777608973,
			 1.47839513685174, 2.77826006964489, 2.09034581792268, "contGamma:contcor1",
			 "<unicode>", 2.90555801640835e-10, "contcor1", 0.331604290447764,
			 6.30373574208009, 0.748768361379102, 1.31065773525481, 1.01357928574409,
			 "contcor1", "<unicode>", 1.53743684450092e-12, "contcor1", 0.143341759927177,
			 7.07106767950264, 4.98585417022654, 8.61016772330048, 6.5378157224924,
			 "contGamma:contcor1", "<unicode>", 1.53743684450092e-12, "contGamma:contcor1",
			 0.924586773446364, 7.07106775724567))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_mediationEffectsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-0.190960213727908, 0.179711376963541, 16, 0.0174307109997205,
			 "contGamma", "contNormal", "", "<unicode>", "", 0.85375196216475,
			 0.0945608168352223, 0.184333337878146, -0.176163416672431, 0.0978973670864186,
			 50, -0.0292532667968165, "contGamma", "contNormal", "", "<unicode>",
			 "", 0.675644906019766, 0.0699147499445414, -0.418413379437401,
			 -0.281099928388441, 0.129693108675617, 84, -0.0802028951812256,
			 "contGamma", "contNormal", "", "<unicode>", "", 0.444078923517959,
			 0.104796067760515, -0.76532351733377, -0.0178702076419795, 0.0288189436928199,
			 "", 0.00298069970215039, "contGamma", "debCollin1", "contNormal",
			 "<unicode>", "<unicode>", 0.802391237986497, 0.0119107166517032,
			 0.250253598445243))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_pathCoefficientsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-0.176010059472548, 0.0979944283630078, -0.0290788260807188, "contGamma",
			 "<unicode>", 0.677407234579936, "contNormal", 0.0699003884757242,
			 -0.416003783595818, -3.13474615708171, 1.92627678467644, -0.326538760956974,
			 "debCollin1", "<unicode>", 0.800333927718874, "contNormal",
			 1.2911010053447, -0.252914961420693, -0.0987638589659854, 0.594869188688385,
			 0.261104084252693, "contcor1", "<unicode>", 0.140057393797568,
			 "contNormal", 0.17695045754046, 1.47557733323742, -0.170980757847346,
			 0.102184473102191, -0.047923273653209, "contGamma:contcor1",
			 "<unicode>", 0.491641601401047, "contNormal", 0.0696862883971924,
			 -0.68770018830763, -0.0186288604677786, 0.00205899731104504,
			 -0.00912816504054517, "contGamma", "<unicode>", 0.0837015013069473,
			 "debCollin1", 0.0052776117168496, -1.72960148079899))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_totalEffectsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-0.185251190133823, 0.184951089420295, 16, 0.0204114107018709,
			 "Total", "contGamma", "contNormal", "<unicode>", 0.828887592628212,
			 0.0944410924063469, 0.216128490064979, -0.168692405991869, 0.101375092456697,
			 50, -0.0262725670946661, "Total", "contGamma", "contNormal",
			 "<unicode>", 0.702953639077543, 0.0688960359932182, -0.381336410954793,
			 -0.27307512302247, 0.132617039360486, 84, -0.0772221954790752,
			 "Total", "contGamma", "contNormal", "<unicode>", 0.455579433110907,
			 0.103494800308323, -0.746145654168338, -0.0178702076419795,
			 0.0288189436928199, "", 0.00298069970215039, "Total indirect",
			 "contGamma", "contNormal", "<unicode>", 0.802391237986497, 0.0119107166517032,
			 0.250253598445243))
})

test_that("Bootstrapping works (percentile interval)", {
  options <- getOptionsClassical()
  options$errorCalculationMethod <- "bootstrap"
  options$bootstrapSamples <- 50
  options$bootstrapCiType <- "perc"
  options$standardizedModelEstimates <- FALSE
  options$processModels <- list(getProcessModel(list(list(processDependent = "contNormal",
                                                                      processIndependent = "contGamma", processType = "mediators",
                                                                      processVariable = "debCollin1"), list(processDependent = "contNormal",
                                                                                                            processIndependent = "contGamma", processType = "moderators",
                                                                                                            processVariable = "contcor1"))))

  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", "debug.csv", options)

  table <- results[["results"]][["modelSummaryTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1121.15455096096, 1168.04761430875, 2, "Model 1", 100, -542.577275480482,
			 5))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_covariancesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-1.21372033215787, 0.332014108125832, -0.591842270846295, "contGamma:contcor1",
			 "<unicode>", 0.133383729438247, "contGamma", 0.394327256132322,
			 -1.5008911041333, -0.504543968063766, 0.104512925436125, -0.240319813025714,
			 "contcor1", "<unicode>", 0.121932410808084, "contGamma", 0.155374511548185,
			 -1.54671323263459, 1.77329815422817, 3.06207843633816, 2.32480104984646,
			 "contGamma", "<unicode>", 1.53743684450092e-12, "contGamma",
			 0.328776521475834, 7.07106772530695, 0.777105840758683, 1.37218424543037,
			 1.07344822547951, "contNormal", "<unicode>", 1.53743684450092e-12,
			 "contNormal", 0.151808504994376, 7.07106776079031, 0.00500047633157313,
			 0.00859013987956438, 0.00647531139383704, "debCollin1", "<unicode>",
			 1.53743684450092e-12, "debCollin1", 0.000915747324008519, 7.07106777608973,
			 1.47839513685174, 2.77826006964489, 2.09034581792268, "contGamma:contcor1",
			 "<unicode>", 2.90555801640835e-10, "contcor1", 0.331604290447764,
			 6.30373574208009, 0.748768361379102, 1.31065773525481, 1.01357928574409,
			 "contcor1", "<unicode>", 1.53743684450092e-12, "contcor1", 0.143341759927177,
			 7.07106767950264, 4.98585417022654, 8.61016772330048, 6.5378157224924,
			 "contGamma:contcor1", "<unicode>", 1.53743684450092e-12, "contGamma:contcor1",
			 0.924586773446364, 7.07106775724567))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_mediationEffectsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-0.190960213727908, 0.179711376963541, 16, 0.0174307109997205,
			 "contGamma", "contNormal", "", "<unicode>", "", 0.85375196216475,
			 0.0945608168352223, 0.184333337878146, -0.176163416672431, 0.0978973670864186,
			 50, -0.0292532667968165, "contGamma", "contNormal", "", "<unicode>",
			 "", 0.675644906019766, 0.0699147499445414, -0.418413379437401,
			 -0.281099928388441, 0.129693108675617, 84, -0.0802028951812256,
			 "contGamma", "contNormal", "", "<unicode>", "", 0.444078923517959,
			 0.104796067760515, -0.76532351733377, -0.0178702076419795, 0.0288189436928199,
			 "", 0.00298069970215039, "contGamma", "debCollin1", "contNormal",
			 "<unicode>", "<unicode>", 0.802391237986497, 0.0119107166517032,
			 0.250253598445243))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_pathCoefficientsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-0.176010059472548, 0.0979944283630078, -0.0290788260807188, "contGamma",
			 "<unicode>", 0.677407234579936, "contNormal", 0.0699003884757242,
			 -0.416003783595818, -3.13474615708171, 1.92627678467644, -0.326538760956974,
			 "debCollin1", "<unicode>", 0.800333927718874, "contNormal",
			 1.2911010053447, -0.252914961420693, -0.0987638589659854, 0.594869188688385,
			 0.261104084252693, "contcor1", "<unicode>", 0.140057393797568,
			 "contNormal", 0.17695045754046, 1.47557733323742, -0.170980757847346,
			 0.102184473102191, -0.047923273653209, "contGamma:contcor1",
			 "<unicode>", 0.491641601401047, "contNormal", 0.0696862883971924,
			 -0.68770018830763, -0.0186288604677786, 0.00205899731104504,
			 -0.00912816504054517, "contGamma", "<unicode>", 0.0837015013069473,
			 "debCollin1", 0.0052776117168496, -1.72960148079899))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_totalEffectsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-0.185251190133823, 0.184951089420295, 16, 0.0204114107018709,
			 "Total", "contGamma", "contNormal", "<unicode>", 0.828887592628212,
			 0.0944410924063469, 0.216128490064979, -0.168692405991869, 0.101375092456697,
			 50, -0.0262725670946661, "Total", "contGamma", "contNormal",
			 "<unicode>", 0.702953639077543, 0.0688960359932182, -0.381336410954793,
			 -0.27307512302247, 0.132617039360486, 84, -0.0772221954790752,
			 "Total", "contGamma", "contNormal", "<unicode>", 0.455579433110907,
			 0.103494800308323, -0.746145654168338, -0.0178702076419795,
			 0.0288189436928199, "", 0.00298069970215039, "Total indirect",
			 "contGamma", "contNormal", "<unicode>", 0.802391237986497, 0.0119107166517032,
			 0.250253598445243))
})

test_that("Missing values work", {
  options <- getOptionsClassical()
  options$dependent <- "contNormal"
  options$covariates <- list("contGamma", "debMiss1", "debMiss30", "debMiss80", "contNormal")
  options$standardizedModelEstimates <- FALSE
  options$processModels <- list(getProcessModel(list(list(processDependent = "debMiss80",
                                                                      processIndependent = "debMiss1", processType = "mediators",
                                                                      processVariable = "contGamma"), list(processDependent = "debMiss80",
                                                                                                            processIndependent = "debMiss1", processType = "moderators",
                                                                                                            processVariable = "debMiss30"))))
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", "debug.csv", options)

  table <- results[["results"]][["modelSummaryTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(3242.29927078897, 3289.19233413676, 2, "Model 1", 100, -1603.14963539449,
			 5))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_covariancesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(3969.31735389225, 12862.5052034576, 8415.91127867493, "debMiss1:debMiss30",
			 "<unicode>", 0.000207624169863418, "debMiss1", 2268.71205790353,
			 3.70955461243146, -111.551314121713, 182.611942704458, 35.5303142913726,
			 "debMiss30", "<unicode>", 0.635880819874171, "debMiss1", 75.0430260827479,
			 0.473465905441957, 507.69201852885, 900.308589131127, 704.000303829988,
			 "debMiss1", "<unicode>", 2.08300043880172e-12, "debMiss1", 100.159128866445,
			 7.02881815963796, 199.191523920716, 979.658354527071, 589.424939223893,
			 "debMiss80", "<unicode>", 0.00307227962557888, "debMiss80",
			 199.102339829348, 2.96041191544556, 1.68023795596774, 2.96888865160363,
			 2.32456330378568, "contGamma", "<unicode>", 1.53765888910584e-12,
			 "contGamma", 0.328743463094374, 7.07105559424723, -2845.51395623957,
			 4860.54682766431, 1007.51643571237, "debMiss1:debMiss30", "<unicode>",
			 0.608297878494196, "debMiss30", 1965.86795591355, 0.512504633224041,
			 381.253252632382, 757.961176409993, 569.607214521187, "debMiss30",
			 "<unicode>", 3.08163272677575e-09, "debMiss30", 96.100726020742,
			 5.92718950321192, 317176.192231532, 630081.560683141, 473628.876457336,
			 "debMiss1:debMiss30", "<unicode>", 2.96734192772874e-09, "debMiss1:debMiss30",
			 79824.2648639889, 5.93339477994998))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_mediationEffectsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-1.59033995974272, 1.57290172449842, 16, -0.00871911762215183,
			 "debMiss1", "debMiss80", "", "<unicode>", "", 0.991379154549691,
			 0.80696423740242, -0.0108048377090641, -0.620542180517313, 0.256321193183489,
			 50, -0.182110493666912, "debMiss1", "debMiss80", "", "<unicode>",
			 "", 0.415584043485196, 0.223693746573251, -0.814106323742395,
			 -1.51126288571914, 0.825343145936107, 84, -0.342959869891516,
			 "debMiss1", "debMiss80", "", "<unicode>", "", 0.565051224850131,
			 0.59608392044091, -0.575355009807739, -0.0290879766795729, 0.0321442168780467,
			 "", 0.00152812009923691, "debMiss1", "contGamma", "debMiss80",
			 "<unicode>", "<unicode>", 0.922070200074344, 0.0156207445750563,
			 0.0978263290776201))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_pathCoefficientsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-0.899299622625888, 0.662482678662918, -0.118408471981485, "debMiss1",
			 "<unicode>", 0.766318230371068, "debMiss80", 0.398421173452152,
			 -0.297194225285583, -8.89573019879904, 14.1534818283138, 2.62887581475736,
			 "contGamma", "<unicode>", 0.654812220656142, "debMiss80", 5.88000907387127,
			 0.447087033664486, -0.962530047492488, 0.874349074923008, -0.0440904862847397,
			 "debMiss30", "<unicode>", 0.925037848325553, "debMiss80", 0.468600223500168,
			 -0.0940897679378165, -0.0627267644394493, 0.0485550132302331,
			 -0.0070858756046081, "debMiss1:debMiss30", "<unicode>", 0.802895376260123,
			 "debMiss80", 0.0283887302387847, -0.249601709728016, -0.0107824406310878,
			 0.01194500608257, 0.000581282725741058, "debMiss1", "<unicode>",
			 0.920140266886183, "contGamma", 0.00579792457742311, 0.100257034733524
			))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_totalEffectsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-1.59022876873148, 1.57584677368565, 16, -0.00719099752291492,
			 "Total", "debMiss1", "debMiss80", "<unicode>", 0.992896370911926,
			 0.807687173690623, -0.00890319638240209, -0.620715015178984,
			 0.259550268043634, 50, -0.180582373567675, "Total", "debMiss1",
			 "debMiss80", "<unicode>", 0.421307353602045, 0.224561596581886,
			 -0.80415519089804, -1.50918724285623, 0.826323743271677, 84,
			 -0.341431749792279, "Total", "debMiss1", "debMiss80", "<unicode>",
			 0.566604086913181, 0.595804566958915, -0.573059974237866, -0.0290879766795729,
			 0.0321442168780467, "", 0.00152812009923691, "Total indirect",
			 "debMiss1", "debMiss80", "<unicode>", 0.922070200074344, 0.0156207445750563,
			 0.0978263290776201))

  plotName <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 1"]][["collection"]][["pathPlotContainer_Model 1_conceptPathPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "conceptual-path-plot-missing")

  plotName <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 1"]][["collection"]][["pathPlotContainer_Model 1_statPathPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "statistical-path-plot-missing")
})

test_that("Not implemented Hayes models error message work", {
  modelNumber <- 20
  options <- getOptionsClassical()
  options$standardizedModelEstimates <- FALSE
  options$processModels <- list(getProcessModel(list(), name = "Model 1"),
                                getProcessModel(list(list(processDependent = "contNormal",
                                                                      processIndependent = "contGamma", processType = "mediators",
                                                                      processVariable = "debCollin1"), list(processDependent = "contNormal",
                                                                                                           processIndependent = "contGamma", processType = "moderators",
                                                                                                           processVariable = "contcor1")), name = "Model 2"))
  options$processModels[[1]]$inputType <- "inputModelNumber"
  options$processModels[[1]]$modelNumber <- modelNumber
  options$processModels[[1]]$localTests <- TRUE
  options$processModels[[2]]$localTests <- TRUE
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", "debug.csv", options)

  refMsg <- jaspProcess:::.procHayesModelMsg("Model 1", modelNumber)

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
		list(1121.15455096096, 1168.04761430875, 2, "Model 2", 100, -542.577275480482,
			 5))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 2"]][["collection"]][["parEstContainer_Model 2_covariancesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-1.36470949098815, 0.181024949295557, -0.591842270846295, "contGamma:contcor1",
			 "<unicode>", 0.133383729438247, "contGamma", 0.394327256132322,
			 -1.5008911041333, -0.544848259775659, 0.064208633724232, -0.240319813025714,
			 "contcor1", "<unicode>", 0.121932410808084, "contGamma", 0.155374511548185,
			 -1.54671323263459, 1.68041090879146, 2.96919119090145, 2.32480104984646,
			 "contGamma", "<unicode>", 1.53743684450092e-12, "contGamma",
			 0.328776521475834, 7.07106772530695, 0.77590902314366, 1.37098742781535,
			 1.07344822547951, "contNormal", "<unicode>", 1.53743684450092e-12,
			 "contNormal", 0.151808504994376, 7.07106776079031, 0.00468047961984141,
			 0.00827014316783267, 0.00647531139383704, "debCollin1", "<unicode>",
			 1.53743684450092e-12, "debCollin1", 0.000915747324008519, 7.07106777608973,
			 1.4404133515261, 2.74027828431925, 2.09034581792268, "contGamma:contcor1",
			 "<unicode>", 2.90555801640835e-10, "contcor1", 0.331604290447764,
			 6.30373574208009, 0.732634598806234, 1.29452397268194, 1.01357928574409,
			 "contcor1", "<unicode>", 1.53743684450092e-12, "contcor1", 0.143341759927177,
			 7.07106767950264, 4.72565894595543, 8.34997249902937, 6.5378157224924,
			 "contGamma:contcor1", "<unicode>", 1.53743684450092e-12, "contGamma:contcor1",
			 0.924586773446364, 7.07106775724567))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 2"]][["collection"]][["parEstContainer_Model 2_mediationEffectsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-0.167905084346004, 0.202766506345445, 16, 0.0174307109997205,
			 "contGamma", "contNormal", "", "<unicode>", "", 0.85375196216475,
			 0.0945608168352223, 0.184333337878146, -0.166283658676241, 0.107777125082608,
			 50, -0.0292532667968165, "contGamma", "contNormal", "", "<unicode>",
			 "", 0.675644906019766, 0.0699147499445414, -0.418413379437401,
			 -0.285599413713254, 0.125193623350803, 84, -0.0802028951812256,
			 "contGamma", "contNormal", "", "<unicode>", "", 0.444078923517959,
			 0.104796067760515, -0.76532351733377, -0.0203638759652493, 0.0263252753695501,
			 "", 0.00298069970215039, "contGamma", "debCollin1", "contNormal",
			 "<unicode>", "<unicode>", 0.802391237986497, 0.0119107166517032,
			 0.250253598445243))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 2"]][["collection"]][["parEstContainer_Model 2_pathCoefficientsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-0.166081069998497, 0.107923417837059, -0.0290788260807188, "contGamma",
			 "<unicode>", 0.677407234579936, "contNormal", 0.0699003884757242,
			 -0.416003783595818, -2.85705023183605, 2.2039727099221, -0.326538760956974,
			 "debCollin1", "<unicode>", 0.800333927718874, "contNormal",
			 1.2911010053447, -0.252914961420693, -0.0857124395744922, 0.607920608079878,
			 0.261104084252693, "contcor1", "<unicode>", 0.140057393797568,
			 "contNormal", 0.17695045754046, 1.47557733323742, -0.184505889127977,
			 0.0886593418215595, -0.047923273653209, "contGamma:contcor1",
			 "<unicode>", 0.491641601401047, "contNormal", 0.0696862883971924,
			 -0.68770018830763, -0.019472093929957, 0.00121576384886665,
			 -0.00912816504054517, "contGamma", "<unicode>", 0.0837015013069473,
			 "debCollin1", 0.0052776117168496, -1.72960148079899))

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 2"]][["collection"]][["parEstContainer_Model 2_totalEffectsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-0.164689729075188, 0.20551255047893, 16, 0.0204114107018709,
			 "Total", "contGamma", "contNormal", "<unicode>", 0.828887592628212,
			 0.0944410924063469, 0.216128490064979, -0.161306316318949, 0.108761182129617,
			 50, -0.0262725670946661, "Total", "contGamma", "contNormal",
			 "<unicode>", 0.702953639077543, 0.0688960359932182, -0.381336410954793,
			 -0.280068276670553, 0.125623885712403, 84, -0.0772221954790752,
			 "Total", "contGamma", "contNormal", "<unicode>", 0.455579433110907,
			 0.103494800308323, -0.746145654168338, -0.0203638759652493,
			 0.0263252753695501, "", 0.00298069970215039, "Total indirect",
			 "contGamma", "contNormal", "<unicode>", 0.802391237986497, 0.0119107166517032,
			 0.250253598445243))

  plotName <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 2"]][["collection"]][["pathPlotContainer_Model 2_conceptPathPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "conceptual-path-plot-error-hayes")

  plotName <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 2"]][["collection"]][["pathPlotContainer_Model 2_statPathPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "statistical-path-plot-error-hayes")
})

test_that("No implied conditional independencies error message works", {
  options <- getOptionsClassical()
  options$standardizedModelEstimates <- FALSE
  options$processModels <- list(getProcessModel(list(list(processDependent = "contNormal",
                                                                      processIndependent = "contGamma", processType = "mediators",
                                                                      processVariable = "debCollin1"))))
  options$processModels[[1]]$localTests <- TRUE
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", "debug.csv", options)

  refMsg <- jaspProcess:::.procNoImpliedTestsMsg()

  msg <- results[["results"]][["localTestContainer"]][["collection"]][["localTestContainer_Model 1"]][["collection"]][["localTestContainer_Model 1_localTestTable"]][["error"]][["errorMessage"]]
  expect_equal(msg, refMsg)


  table <- results[["results"]][["modelSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(459.709342975141, 483.155874649034, 0, "Model 1", 100, -220.854671487571,
                                      4))
})

test_that("Invalid test type error message works", {
  options <- getOptionsClassical()
  options$standardizedModelEstimates <- FALSE
  options$processModels <- list(getProcessModel(list(list(processDependent = "contNormal",
                                                                      processIndependent = "contGamma", processType = "mediators",
                                                                      processVariable = "debCollin1"), list(processDependent = "contNormal",
                                                                                                            processIndependent = "contGamma", processType = "moderators",
                                                                                                            processVariable = "facGender"))))
  options$processModels[[1]]$localTests <- TRUE
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", "debug.csv", options)

  refMsg <- jaspProcess:::.procLocalTestLinearMsg()

  msg <- results[["results"]][["localTestContainer"]][["collection"]][["localTestContainer_Model 1"]][["collection"]][["localTestContainer_Model 1_localTestTable"]][["error"]][["errorMessage"]]
  expect_equal(msg, refMsg)


  table <- results[["results"]][["modelSummaryTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(828.642678629518, 875.535741977304, 2, "Model 1", 100, -396.321339314759,
			 5))
})

test_that("Local tests work for factors with loess test type", {
  options <- getOptionsClassical()
  options$standardizedModelEstimates <- FALSE
  options$processModels <- list(getProcessModel(list(list(processDependent = "contNormal",
                                                                      processIndependent = "contGamma", processType = "mediators",
                                                                      processVariable = "debCollin1"), list(processDependent = "contNormal",
                                                                                                            processIndependent = "contGamma", processType = "moderators",
                                                                                                            processVariable = "facGender"))))
  options$processModels[[1]]$localTests <- TRUE
  options$processModels[[1]]$localTestType <- "cis.loess"
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", "debug.csv", options)
  table <- results[["results"]][["localTestContainer"]][["collection"]][["localTestContainer_Model 1"]][["collection"]][["localTestContainer_Model 1_localTestTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.206103108541346, 0.17035274701811, "contGamma", -0.0139864082891252,
                                      "debCollin1", "<unicode>", "<unicode>", "facGenderm", 0.0989527098094575
                                 ))
})

test_that("Path plots for empty moderator model works", {
  options <- getOptionsClassical()
  options$standardizedModelEstimates <- FALSE
  options$processModels <- list(getProcessModel(list()))
  options$processModels[[1]]$inputType <- "inputModelNumber"
  options$processModels[[1]]$modelNumber <- 91
  options$processModels[[1]]$modelNumberIndependent <- list(value = "contGamma")
  options$processModels[[1]]$modelNumberMediators <- list(value = "debCollin1")
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", "debug.csv", options)

  plotName <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 1"]][["collection"]][["pathPlotContainer_Model 1_conceptPathPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "conceptual-path-plot-empty-mod")

  plotName <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 1"]][["collection"]][["pathPlotContainer_Model 1_statPathPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "statistical-path-plot-empty-mod")
})

test_that("Path plot for multiple dependent variables work", {
  options <- getOptionsClassical()
  options$standardizedModelEstimates <- FALSE
  options$covariates <- append(options$covariates, "debMiss1")
  options$processModels <- list(getProcessModel(list(list(processDependent = "contNormal",
                                                                      processIndependent = "contGamma", processType = "mediators",
                                                                      processVariable = "debCollin1"), list(processDependent = "contcor1",
                                                                                                            processIndependent = "facGender", processType = "mediators",
                                                                                                            processVariable = "debCollin1"), list(processDependent = "debMiss1",
                                                                                                                                                  processIndependent = "contGamma", processType = "mediators",
                                                                                                                                                  processVariable = "debCollin1"))))

  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", "debug.csv", options)

  plotName <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 1"]][["collection"]][["pathPlotContainer_Model 1_conceptPathPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "conceptual-path-plot-multi-dep")

  plotName <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 1"]][["collection"]][["pathPlotContainer_Model 1_statPathPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "statistical-path-plot-multi-dep")
})

test_that("R-squared table matches", {
  options <- getOptionsClassical()
  options$standardizedModelEstimates <- FALSE
  options$covariates <- append(options$covariates, "debMiss1")
  options$rSquared <- TRUE

  options$processModels <- list(getProcessModel(list(list(processDependent = "contNormal",
                                                                      processIndependent = "contGamma", processType = "mediators",
                                                                      processVariable = "debCollin1"), list(processDependent = "contcor1",
                                                                                                            processIndependent = "facGender", processType = "mediators",
                                                                                                            processVariable = "debCollin1"), list(processDependent = "debMiss1",
                                                                                                                                                  processIndependent = "contGamma", processType = "mediators",
                                                                                                                                                  processVariable = "debCollin1"))))
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", "debug.csv", options)

  table <- results[["results"]][["rSquaredTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("contNormal", 0.0014373222571975, "debCollin1", 0.0299736218393996,
			 "debMiss1", 0.0261468960115053, "contcor1", 0.0089016593319361
			))
})

test_that("Path coefficients table with intercepts matches", {
  options <- getOptionsClassical()
  options$standardizedModelEstimates <- FALSE
  options$covariates <- append(options$covariates, "debMiss1")
  options$rSquared <- TRUE

  options$processModels <- list(getProcessModel(list(list(processDependent = "contNormal",
                                                                      processIndependent = "contGamma", processType = "mediators",
                                                                      processVariable = "debCollin1"), list(processDependent = "contcor1",
                                                                                                            processIndependent = "facGender", processType = "mediators",
                                                                                                            processVariable = "debCollin1"), list(processDependent = "debMiss1",
                                                                                                                                                  processIndependent = "contGamma", processType = "mediators",
                                                                                                                                                  processVariable = "debCollin1"))))
  options$processModels[[1]]$intercepts <- TRUE
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", "debug.csv", options)

  table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_pathCoefficientsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(-1.77676124881745, 1.77349828611803, -0.00163148134970936, "(Intercept)",
			 "<unicode>", 0.998562724825225, "contNormal", 0.905695095149573,
			 -0.00180135826995941, 0.653227865711247, 0.711848894250612,
			 0.682538379980929, "(Intercept)", "<unicode>", 0, "debCollin1",
			 0.0149546188097741, 45.6406404377779, -4.14266246049926, 84.7827534840697,
			 40.3200455117852, "(Intercept)", "<unicode>", 0.0755105126813613,
			 "debMiss1", 22.6854719387707, 1.77735096808262, -1.67485482180179,
			 1.59891259679606, -0.0379711125028608, "(Intercept)", "<unicode>",
			 0.963736138737157, "contcor1", 0.835160095905055, -0.0454656690244663,
			 1.73411925179484, 2.33180238115026, 2.03296081647255, "(Intercept)",
			 "<unicode>", 0, "contGamma", 0.152472987786986, 13.3332523090104,
			 0.402001792258676, 0.597998191852738, 0.499999992055707, "(Intercept)",
			 "<unicode>", 0, "facGenderm", 0.050000000290836, 9.99999978294694,
			 -0.162107027275688, 0.11068832612516, -0.0257093505752638, "contGamma",
			 "<unicode>", 0.711807199324212, "contNormal", 0.0695919301458146,
			 -0.369430054912912, -2.76413415313077, 2.35668701408407, -0.20372356952335,
			 "debCollin1", "<unicode>", 0.876074025178669, "contNormal",
			 1.30635593500881, -0.155947980227897, -0.019339828988455, 0.00153717108327983,
			 -0.00890132895258758, "contGamma", "<unicode>", 0.094654511257402,
			 "debCollin1", 0.00532586318840804, -1.67134014481665, -0.0368533470535606,
			 0.0268102220301853, -0.00502156251168766, "facGenderm", "<unicode>",
			 0.757176708640744, "debCollin1", 0.0162410048311898, -0.309190383469629,
			 -3.99783791927949, 2.8671336212766, -0.565352149001448, "contGamma",
			 "<unicode>", 0.746832714112275, "debMiss1", 1.75130043069824,
			 -0.32281848339182, -117.496326926132, 10.8196233913679, -53.3383517673822,
			 "debCollin1", "<unicode>", 0.103220970451316, "debMiss1", 32.7342622950319,
			 -1.62943497203776, -2.14583711750397, 2.69605094949409, 0.275106915995057,
			 "debCollin1", "<unicode>", 0.823751186575374, "contcor1", 1.23519822435266,
			 0.222722888173868, -0.576767602305583, 0.210438918834703, -0.18316434173544,
			 "facGenderm", "<unicode>", 0.361729479536131, "contcor1", 0.200821680232308,
			 -0.912074540575291))
})

test_that("Directed acyclic graph error message works", {
  options <- getOptionsClassical()
  options$standardizedModelEstimates <- FALSE
  options$processModels <- list(getProcessModel(list(
          list(processDependent = "contNormal", processIndependent = "contGamma",
              processType = "mediators", processVariable = "debCollin1"),
          list(processDependent = "contGamma", processIndependent = "debCollin1",
              processType = "directs", processVariable = ""))),
          getProcessModel(list(
          list(processDependent = "contNormal", processIndependent = "contGamma",
              processType = "mediators", processVariable = "debCollin1")), name = "Model 2"))
  options$processModels[[1]]$localTests <- TRUE
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", "debug.csv", options)

  refMsg <- jaspProcess:::.procEstimationMsg(jaspProcess:::.procDagMsg())

  msg <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_pathCoefficientsTable"]][["error"]][["errorMessage"]]
  expect_equal(msg, refMsg)

  msg <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 1"]][["collection"]][["pathPlotContainer_Model 1_conceptPathPlot"]][["error"]][["errorMessage"]]
  expect_equal(msg, refMsg)

  msg <- results[["results"]][["localTestContainer"]][["collection"]][["localTestContainer_Model 1"]][["collection"]][["localTestContainer_Model 1_localTestTable"]][["error"]][["errorMessage"]]
  expect_equal(msg, refMsg)
})

test_that("Incomplete Hayes configuration works", {
  modelNumber <- 5
  options <- getOptionsClassical()
  options$standardizedModelEstimates <- FALSE
  options$processModels <- list(getProcessModel(list()))
  options$processModels[[1]]$inputType <- "inputModelNumber"
  options$processModels[[1]]$modelNumber <- 5
  options$processModels[[1]]$localTests <- TRUE
  options$processModels[[1]]$modelNumberIndependent <- list(value = "contGamma")
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", "debug.csv", options)

  plotName <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 1"]][["collection"]][["pathPlotContainer_Model 1_conceptPathPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "conceptual-path-plot-incomplete")

	plotName <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 1"]][["collection"]][["pathPlotContainer_Model 1_statPathPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "statistical-path-plot-incomplete")
})
