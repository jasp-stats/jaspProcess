
test_that("Missing values work without independent covariances", {
  options <- getOptionsBayesian()
  options$covariates <- list("contGamma", "debMiss1", "debMiss30", "debMiss80")
  options$processModels <- list(getProcessModel(list(list(processDependent = "debMiss80",
                                                                      processIndependent = "debMiss1", processType = "mediators",
                                                                      processVariable = "contGamma"), list(processDependent = "debMiss80",
                                                                                                            processIndependent = "debMiss1", processType = "moderators",
                                                                                                            processVariable = "debMiss30"))))
  options$processModels[[1]]$independentCovariances <- FALSE
  options$processModels[[1]]$intercepts <- TRUE
  set.seed(1)
  results <- jaspTools::runAnalysis("BayesianProcess", "debug.csv", options)

  table <- results[["results"]][["modelSummaryTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0, "Model 1", 69, 2831.84654144799, 1))

	table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_covariancesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(3, 247082.682637495, 247082.682639476, "debMiss80", 247082.682638674,
			 247082.682638882, "<unicode>", 1.47997997995841, "debMiss80",
			 5.62231943175488e-07, 13, 200, 4170694472577044, 4170694472577044,
			 "contGamma", 4170694472577044, 4170694472577044, "<unicode>",
			 1, "contGamma", 0, 200))

	table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_mediationEffectsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(5, -0.215014442770641, -0.215014442745862, 16, "debMiss1", "debMiss80",
			 "", -0.215014442756571, -0.21501444275598, "<unicode>", "",
			 1.23357468767469, 6.35337884146757e-12, 12, 3, -0.18480506979993,
			 -0.184805069765363, 50, "debMiss1", "debMiss80", "", -0.18480506978615,
			 -0.184805069787627, "<unicode>", "", 1.22102000729357, 9.2366670408536e-12,
			 12, 3, -0.156508691446879, -0.156508691356733, 84, "debMiss1",
			 "debMiss80", "", -0.156508691410163, -0.15650869141524, "<unicode>",
			 "", 1.21180315926087, 2.37910221006175e-11, 12, 2, 2.83446062008339e-05,
			 2.8344611918101e-05, "", "debMiss1", "contGamma", "debMiss80",
			 2.83446089538845e-05, 2.83446088828221e-05, "<unicode>", "<unicode>",
			 1.47813384110761, 1.25333611660017e-12, 20))

	table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_pathCoefficientsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(2, 10.7311891062555, 10.7311891062592, "(Intercept)", 10.7311891062573,
			 10.7311891062573, "<unicode>", "Normal(0,10)", 1.43368621479253,
			 "debMiss80", 1.24632365453172e-12, 200, 9, 2.02273919008768,
			 2.02273919008995, "(Intercept)", 2.022739190089, 2.02273919008909,
			 "<unicode>", "Normal(0,10)", 1.51094915827383, "contGamma",
			 7.48383462073232e-13, 29, "", "", "", "(Intercept)", 3.04981150169565,
			 3.04981150169565, "<unicode>", "", "", "debMiss1", 0, "", "",
			 "", "", "(Intercept)", 7.77994174547826, 7.77994174547826, "<unicode>",
			 "", "", "debMiss30", 0, "", "", "", "", "(Intercept)", 65.4570525044341,
			 65.4570525044341, "<unicode>", "", "", "debMiss1:debMiss30",
			 0, "", 2, -0.202420561677805, -0.202420561675789, "debMiss1",
			 -0.202420561676554, -0.202420561676334, "<unicode>", "Normal(0,10)",
			 1.59051728543811, "debMiss80", 5.7497099716731e-13, 17, 22,
			 -2.05878988110189, -2.05878988110119, "contGamma", -2.05878988110167,
			 -2.05878988110188, "<unicode>", "Normal(0,10)", 1.0102650617491,
			 "debMiss80", 3.18546447565261e-13, 200, 4, 0.00698607346950194,
			 0.00698607347380862, "debMiss30", 0.00698607347216579, 0.0069860734730944,
			 "<unicode>", "Normal(0,10)", 1.32125795144338, "debMiss80",
			 1.56328226026569e-12, 26, 3, 0.00199270270091905, 0.00199270270483371,
			 "debMiss1:debMiss30", 0.00199270270253432, 0.00199270270228076,
			 "<unicode>", "Normal(0,10)", 1.20912970536564, "debMiss80",
			 1.02567182787816e-12, 12, 2, -1.37676079420648e-05, -1.37676051650611e-05,
			 "debMiss1", -1.37676065022804e-05, -1.37676064677624e-05, "<unicode>",
			 "Normal(0,10)", 1.47813384110761, "contGamma", 6.08773603852703e-13,
			 20))

	table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_totalEffectsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(6, -0.214986098162984, -0.214986098136969, 16, "Total", "debMiss1",
			 "debMiss80", -0.214986098147617, -0.214986098147454, "<unicode>",
			 1.23523817690533, 6.42520580960565e-12, 12, 3, -0.184776725191038,
			 -0.184776725157721, 50, "Total", "debMiss1", "debMiss80", -0.184776725177196,
			 -0.184776725178046, "<unicode>", 1.31831286741936, 9.45306659136354e-12,
			 12, 3, -0.156480346837987, -0.156480346749083, 84, "Total",
			 "debMiss1", "debMiss80", -0.156480346801209, -0.156480346805431,
			 "<unicode>", 1.24436459705305, 2.39371216048077e-11, 12, 2,
			 2.83446062008339e-05, 2.8344611918101e-05, "", "Total indirect",
			 "debMiss1", "debMiss80", 2.83446089538845e-05, 2.83446088828221e-05,
			 "<unicode>", 1.47813384110761, 1.25333611660017e-12, 20))
})

test_that("Incomplete Hayes configuration works", {
  modelNumber <- 5
  options <- getOptionsBayesian()
  options$standardizedModelEstimates <- FALSE
  options$processModels <- list(getProcessModel(list()))
  options$processModels[[1]]$inputType <- "inputModelNumber"
  options$processModels[[1]]$modelNumber <- 5
  options$processModels[[1]]$localTests <- TRUE
  set.seed(1)
  results <- jaspTools::runAnalysis("BayesianProcess", "debug.csv", options)

  plotName <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 1"]][["collection"]][["pathPlotContainer_Model 1_conceptPathPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "conceptual-path-plot-incomplete")

	plotName <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 1"]][["collection"]][["pathPlotContainer_Model 1_statPathPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "statistical-path-plot-incomplete")
})
