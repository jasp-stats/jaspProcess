
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
		list("Model 1", 69, 2831.83300302513, -2026044530.14647, 506715812.265735, 1, -2026044530.14647, 506715812.265735))

	table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_covariancesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(10, 247084.937713529, 247084.937715484, "debMiss80", 247084.937714352,
			 247084.937714391, "<unicode>", 1.04677934523384, "debMiss80",
			 4.96675765583445e-07, 17, 200, 4169861089775178, 4169861089775178,
			 "contGamma", 4169861089775178, 4169861089775178, "<unicode>",
			 1, "contGamma", 0, 200))

	table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_mediationEffectsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(2, -0.230875228378302, -0.230875228274066, 16, "debMiss1", "debMiss80",
			 "", -0.230875228295524, -0.230875228283443, "<unicode>", "",
			 1.34182449149079, 2.81008693769313e-11, 7, 1, -0.201838057966146,
			 -0.201838057959112, 50, "debMiss1", "debMiss80", "", -0.201838057962741,
			 -0.201838057963021, "<unicode>", "", 2.09657364174672, 2.1712116687348e-12,
			 200, 7, -0.174580045975219, -0.1745800458751, 84, "debMiss1",
			 "debMiss80", "", -0.174580045952472, -0.174580045961713, "<unicode>",
			 "", 1.13820758258518, 2.55266775606037e-11, 22, 1, -0.000214626245468182,
			 -0.000214626234802818, "", "debMiss1", "contGamma", "debMiss80",
			 -0.00021462623964298, -0.000214626239938754, "<unicode>", "<unicode>",
			 2.12222617299695, 3.439131691028e-12, 9))

	table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_pathCoefficientsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(5, 10.7313081082104, 10.7313081082131, "(Intercept)", 10.7313081082116,
			 10.7313081082114, "<unicode>", "Normal(0,10)", 1.11780719285469,
			 "debMiss80", 8.15815922246695e-13, 13, 1, 2.02289123962856,
			 2.02289123963512, "(Intercept)", 2.0228912396327, 2.02289123963321,
			 "<unicode>", "Normal(0,10)", 1.96187306548877, "contGamma",
			 1.93694678572619e-12, 8, "", "", "", "(Intercept)", 3.04981150169565,
			 3.04981150169565, "<unicode>", "", "", "debMiss1", 0, "", "",
			 "", "", "(Intercept)", 7.77994174547826, 7.77994174547826, "<unicode>",
			 "", "", "debMiss30", 0, "", "", "", "", "(Intercept)", 65.4570525044341,
			 65.4570525044341, "<unicode>", "", "", "debMiss1:debMiss30",
			 0, "", 1, -0.202439436535781, -0.202439436528104, "debMiss1",
			 -0.202439436531798, -0.202439436532052, "<unicode>", "Normal(0,10)",
			 2.10529308667034, "debMiss80", 2.35587732587076e-12, 27, 200,
			 -2.05867759149308, -2.05867759149308, "contGamma", -2.05867759149308,
			 -2.05867759149308, "<unicode>", "Normal(0,10)", 1, "debMiss80",
			 0, 200, 7, 0.00715125845191726, 0.00715125845825294, "debMiss30",
			 0.00715125845611074, 0.00715125845619553, "<unicode>", "Normal(0,10)",
			 1.2360532503874, "debMiss80", 1.57378137456757e-12, 11, 4,
			 0.00192133727987818, 0.00192133728679867, "debMiss1:debMiss30",
			 0.00192133728133283, 0.00192133728054813, "<unicode>", "Normal(0,10)",
			 1.18645483687942, "debMiss80", 1.82452257650895e-12, 22, 1,
			 0.00010425441831674, 0.000104254423497427, "debMiss1", 0.000104254420667842,
			 0.000104254420811514, "<unicode>", "Normal(0,10)", 2.12222617299695,
			 "contGamma", 1.6705538080887e-12, 13))

	table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_totalEffectsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(5, -0.231089854613832, -0.231089854512986, 16, "Total", "debMiss1",
			 "debMiss80", -0.231089854535167, -0.231089854523508, "<unicode>",
			 1.15318045092776, 2.63352952579614e-11, 7, 2, -0.202052684205402,
			 -0.202052684198444, 50, "Total", "debMiss1", "debMiss80", -0.202052684202384,
			 -0.202052684202485, "<unicode>", 1.44009030270064, 1.80393727223553e-12,
			 13, 3, -0.174794672213477, -0.174794672110631, 84, "Total", "debMiss1",
			 "debMiss80", -0.174794672192115, -0.174794672203612, "<unicode>",
			 1.22194586228406, 2.71846094837226e-11, 22, 1, -0.000214626245468182,
			 -0.000214626234802818, "", "Total indirect", "debMiss1", "debMiss80",
			 -0.00021462623964298, -0.000214626239938754, "<unicode>", 2.12222617299695,
			 3.439131691028e-12, 9))
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
