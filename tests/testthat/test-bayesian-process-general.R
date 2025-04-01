
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
		list(0, "Model 1", 69, 2834.09512862431, 1))

	table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_covariancesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(2, 246652.460018218, 246652.460019803, "debMiss80", 246652.460019258,
			 246652.460019324, "<unicode>", 1.50891224620628, "debMiss80",
			 4.1566048889572e-07, 36, 200, 4310235706571309, 4310235706571309,
			 "contGamma", 4310235706571309, 4310235706571309, "<unicode>",
			 1, "contGamma", 0, 200))

	table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_mediationEffectsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, -0.198786305912991, -0.198786305843689, 16, "debMiss1", "debMiss80",
			 "", -0.198786305876332, -0.198786305869288, "<unicode>", "",
			 2.07407816250739, 2.4605971060919e-11, 17, 6, -0.200754963157099,
			 -0.200754963154242, 50, "debMiss1", "debMiss80", "", -0.20075496315546,
			 -0.200754963155211, "<unicode>", "", 1.0718128139592, 9.11926859907922e-13,
			 43, 1, -0.202599564530513, -0.202599564463123, 84, "debMiss1",
			 "debMiss80", "", -0.202599564499149, -0.202599564508101, "<unicode>",
			 "", 1.98102351744065, 2.3876615103136e-11, 19, 2, -0.00414763312501074,
			 -0.00414763311967673, "", "debMiss1", "contGamma", "debMiss80",
			 -0.00414763312182887, -0.00414763312165685, "<unicode>", "<unicode>",
			 1.63326612559571, 1.68348113433994e-12, 6))

	table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_pathCoefficientsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, 10.7324892614672, 10.7324892614739, "(Intercept)", 10.7324892614708,
			 10.7324892614712, "<unicode>", "Normal(0,10)", 2.13015542914738,
			 "debMiss80", 2.07251232506839e-12, 200, 6, 2.02004979166869,
			 2.02004979167168, "(Intercept)", 2.02004979167032, 2.02004979167027,
			 "<unicode>", "Normal(0,10)", 1.07210813269059, "contGamma",
			 9.25280836164522e-13, 17, "", "", "", "(Intercept)", 3.04981150169565,
			 3.04981150169565, "<unicode>", "", "", "debMiss1", 0, "", "",
			 "", "", "(Intercept)", 7.77994174547826, 7.77994174547826, "<unicode>",
			 "", "", "debMiss30", 0, "", "", "", "", "(Intercept)", 65.4570525044341,
			 65.4570525044341, "<unicode>", "", "", "debMiss1:debMiss30",
			 0, "", 5, -0.200714303986432, -0.200714303983745, "debMiss1",
			 -0.200714303984997, -0.200714303984768, "<unicode>", "Normal(0,10)",
			 1.13874380771057, "debMiss80", 8.13095537961116e-13, 10, 200,
			 -2.05549490855541, -2.05549490855541, "contGamma", -2.05549490855541,
			 -2.05549490855541, "<unicode>", "Normal(0,10)", 1, "debMiss80",
			 0, 200, 1, 0.00464960074951967, 0.00464960075544299, "debMiss30",
			 0.00464960075244427, 0.00464960075192995, "<unicode>", "Normal(0,10)",
			 2.07346954116239, "debMiss80", 1.89473805218112e-12, 11, 1,
			 -0.000129901505252243, -0.000129901500600667, "debMiss1:debMiss30",
			 -0.000129901503076732, -0.000129901503621629, "<unicode>", "Normal(0,10)",
			 2.02260885398654, "debMiss80", 1.65067815389945e-12, 19, 2,
			 0.00201782699748532, 0.00201782700008033, "debMiss1", 0.00201782699853235,
			 0.00201782699844866, "<unicode>", "Normal(0,10)", 1.63326612559571,
			 "contGamma", 8.19015018875789e-13, 16))

	table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_totalEffectsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, -0.202933939032824, -0.202933938965828, 16, "Total", "debMiss1",
			 "debMiss80", -0.202933938998161, -0.202933938989629, "<unicode>",
			 1.98675345251848, 2.33851545373092e-11, 17, 2, -0.204902596279995,
			 -0.204902596274126, 50, "Total", "debMiss1", "debMiss80", -0.204902596277289,
			 -0.204902596277989, "<unicode>", 1.48732441715926, 1.96473677241202e-12,
			 6, 1, -0.206747197652666, -0.206747197582948, 84, "Total", "debMiss1",
			 "debMiss80", -0.206747197620978, -0.206747197628441, "<unicode>",
			 2.01589455081866, 2.5116836127411e-11, 29, 2, -0.00414763312501074,
			 -0.00414763311967673, "", "Total indirect", "debMiss1", "debMiss80",
			 -0.00414763312182887, -0.00414763312165685, "<unicode>", 1.63326612559571,
			 1.68348113433994e-12, 6))
})

test_that("Incomplete Hayes configuration works", {
  options <- getOptionsBayesian()
  options$standardizedModelEstimates <- FALSE
  options$processModels <- list(getProcessModel(list()))
  options$processModels[[1]]$inputType <- "inputModelNumber"
  options$processModels[[1]]$modelNumber <- 5
  options$processModels[[1]]$localTests <- TRUE
  options$syntax <- TRUE
  set.seed(1)
  results <- jaspTools::runAnalysis("BayesianProcess", "debug.csv", options)

  plotName <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 1"]][["collection"]][["pathPlotContainer_Model 1_conceptPathPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "conceptual-path-plot-incomplete")

	plotName <- results[["results"]][["pathPlotContainer"]][["collection"]][["pathPlotContainer_Model 1"]][["collection"]][["pathPlotContainer_Model 1_statPathPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "statistical-path-plot-incomplete")
})
