
# MCMC results are not reproducible across platforms (different BLAS/RNG),
# so tests that compare MCMC estimates exactly are skipped on CI. Tests that
# check structure, or estimates against a tolerance, run everywhere.

test_that("Missing values work without independent covariances", {
  skip_on_ci()
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
                                 list("Model 1", 69, 418.231386455361, 403.199433086257, 37.8910646375844,
                                      0.58, 402.088109604119, 37.5918467370759))

	table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_covariancesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
	                               list(206, 220.157645656434, 678.757872317815, "debMiss80", 381.072429015594,
	                                    351.263437317513, "<unicode>", 0.996305941199153, "debMiss80",
	                                    118.262325399777, 155, 154, 2.08061036637023, 4.00941003884884,
	                                    "contGamma", 2.84250935272092, 2.7966033227123, "<unicode>",
	                                    1.02456006679909, "contGamma", 0.508611790014345, 153))

	table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_mediationEffectsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
	                               list(89, -1.40057970460764, 0.432071037689523, 16, "debMiss1", "debMiss80",
	                                    "", -0.402341415164558, -0.377981363797972, "<unicode>", "",
	                                    1.00173267350835, 0.476550433328771, 79, 166, -0.667868310038247,
	                                    0.213228195402421, 50, "debMiss1", "debMiss80", "", -0.213199722827921,
	                                    -0.213340908591568, "<unicode>", "", 1.00649673248191, 0.243574950882809,
	                                    154, 154, -0.714812506581313, 0.586522928817177, 84, "debMiss1",
	                                    "debMiss80", "", -0.0356470735074771, -0.0320107141523223, "<unicode>",
	                                    "", 0.996627164154386, 0.314559187461707, 100, 208, -0.0827905009037452,
	                                    0.130113824177246, "", "debMiss1", "contGamma", "debMiss80",
	                                    0.00612792612362735, 0.000643277461452164, "<unicode>", "<unicode>",
	                                    1.00717690176148, 0.0484337092268937, 59))

	table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_pathCoefficientsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
	                               list(7, -4.0642457494842, 24.3768886430377, "(Intercept)", 10.7950391702589,
	                                    10.1924742928936, "<unicode>", "Normal(0,10)", 1.12027068207783,
	                                    "debMiss80", 7.13701478808876, 16, 180, 1.60915978791644, 2.36446067135159,
	                                    "(Intercept)", 2.01438091040119, 2.00724766299874, "<unicode>",
	                                    "Normal(0,10)", 0.997712704693681, "contGamma", 0.181628473504285,
	                                    134, "", "", "", "(Intercept)", 2.49216032330435, 2.49216032330435,
	                                    "<unicode>", "", "", "debMiss1", 0, "", "", "", "", "(Intercept)",
	                                    6.173707945, 6.173707945, "<unicode>", "", "", "debMiss30",
	                                    0, "", "", "", "", "(Intercept)", 60.7028560551267, 60.7028560551267,
	                                    "<unicode>", "", "", "debMiss1:debMiss30", 0, "", 158, -0.681183823261523,
	                                    0.210153763842165, "debMiss1", -0.217116969549377, -0.217191688604814,
	                                    "<unicode>", "Normal(0,10)", 1.00922138049945, "debMiss80",
	                                    0.246184441257337, 154, 21, -14.4589720154607, 5.34212604755402,
	                                    "contGamma", -4.82131743230868, -4.81530378988807, "<unicode>",
	                                    "Normal(0,10)", 1.06712390920013, "debMiss80", 4.37817872549893,
	                                    22, 57, -0.389138377333645, 0.89385946866556, "debMiss30", 0.249669994579495,
	                                    0.246492400203833, "<unicode>", "Normal(0,10)", 1.01113070753938,
	                                    "debMiss80", 0.333088812268675, 128, 89, -0.0261166703242867,
	                                    0.0575295865572217, "debMiss1:debMiss30", 0.012515165244269,
	                                    0.0115576254747552, "<unicode>", "Normal(0,10)", 0.998958895372493,
	                                    "debMiss80", 0.0217758905602253, 87, 197, -0.0160507146197528,
	                                    0.0145456658904451, "debMiss1", -0.00142411074208153, -0.00122295221008741,
	                                    "<unicode>", "Normal(0,10)", 0.996414724217025, "contGamma",
	                                    0.00762402033914468, 62))

	table <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_totalEffectsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
	                               list(90, -1.47527079626932, 0.431575457502299, 16, "Total", "debMiss1",
	                                    "debMiss80", -0.396213489040931, -0.381985368742383, "<unicode>",
	                                    1.00119749933372, 0.476850854635045, 106, 146, -0.717893126684412,
	                                    0.273231432176105, 50, "Total", "debMiss1", "debMiss80", -0.207071796704294,
	                                    -0.210577200070775, "<unicode>", 1.00410857803111, 0.247648191272265,
	                                    134, 159, -0.732937315783338, 0.641557996996489, 84, "Total",
	                                    "debMiss1", "debMiss80", -0.0295191473838497, -0.0270516664656825,
	                                    "<unicode>", 0.996341407334896, 0.32024639422657, 150, 208,
	                                    -0.0827905009037452, 0.130113824177246, "", "Total indirect",
	                                    "debMiss1", "debMiss80", 0.00612792612362735, 0.000643277461452164,
	                                    "<unicode>", 1.00717690176148, 0.0484337092268937, 59))
})

test_that("Incomplete Hayes configuration works", {
  # TODO run on CI once the snapshots are regenerated in the current svglite
  # format (720x576), as jasp-stats/jaspProcess#234 does for the classical ones
  skip_on_ci()
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

test_that("Covariates in Hayes configuration work", {
  options <- getOptionsBayesian()
  options$hayesNumber <- TRUE
  options$processModels <- list(getProcessModel(list()))
  options$processModels[[1]]$inputType <- "inputModelNumber"
  options$processModels[[1]]$modelNumber <- 4
  options$processModels[[1]]$modelNumberIndependent <- "contGamma"
  options$processModels[[1]]$modelNumberMediators <- list("debCollin1")
  options$processModels[[1]]$modelNumberCovariates <- list("contcor2", "facGender")
  set.seed(1)
  results <- jaspTools::runAnalysis("BayesianProcess", "debug.csv", options)

  # Covariates enter the mediator and outcome equations
  coefTable <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_pathCoefficientsTable"]][["data"]]
  paths <- sapply(coefTable, function(row) paste(row[["lhs"]], row[["rhs"]], sep = "->"))
  expect_setequal(paths, c(
    "contGamma->contNormal", "debCollin1->contNormal", "contcor2->contNormal", "facGenderm->contNormal",
    "contGamma->debCollin1", "contcor2->debCollin1", "facGenderm->debCollin1"
  ))

  # Covariates are not focal: only the direct and indirect effect of X are reported
  medTable <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_mediationEffectsTable"]][["data"]]
  expect_length(medTable, 2)
  expect_true(all(sapply(medTable, function(row) row[["lhs_1"]]) == "contGamma"))

  summaryTable <- results[["results"]][["modelSummaryTable"]][["data"]]
  expect_equal(summaryTable[[1]][["modelNumber"]], 4)
})

test_that("Mean-centered moderation evaluates conditional effects at the requested moderator values", {
  # The moderator probes are computed on the mean-centered moderator, so the model
  # must be fitted on centered data too. Otherwise the conditional effects are
  # evaluated at moderator values shifted by the moderator mean (here by
  # b_XW * mean(Age), about 0.35). With weak priors and n = 815, the posterior
  # means are close to the OLS conditional effects at the raw percentiles.
  options <- getOptionsBayesian()
  options$dependent <- "GovernmentAction"
  options$covariates <- list("NegativeEmotion", "Age")
  options$factors <- list()
  options$meanCenteredModeration <- TRUE
  options$mcmcBurnin <- 500
  options$mcmcSamples <- 1000
  options$processModels <- list(getProcessModel(list(list(processDependent = "GovernmentAction",
          processIndependent = "NegativeEmotion", processType = "moderators",
          processVariable = "Age"))))
  set.seed(1)
  results <- jaspTools::runAnalysis("BayesianProcess", test_path("global_warming.csv"), options)

  data <- read.csv(test_path("global_warming.csv"))
  refCoefs <- coef(lm(GovernmentAction ~ NegativeEmotion * Age, data = data))
  probes <- quantile(data$Age, c(0.16, 0.5, 0.84))
  refEffects <- unname(refCoefs["NegativeEmotion"] + refCoefs["NegativeEmotion:Age"] * probes)

  medTable <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_mediationEffectsTable"]][["data"]]
  effects <- sapply(medTable, function(row) row[["mean"]])
  expect_length(effects, 3)
  expect_lt(max(abs(effects - refEffects)), 0.03)
})

test_that("Model summary works for models fitted to different numbers of cases", {
  # Without independent covariances, cases with a missing predictor are removed, here
  # only from Model 2, which has a covariate with missing values. loo_compare() stops for
  # models with different numbers of cases, so the LOO difference must be left out, and
  # a footnote says that the information criteria are not comparable.
  options <- getOptionsBayesian()
  options$covariates <- c(options$covariates, list("debMiss30"))
  hayesModel4 <- function(name, covariates = list()) {
    model <- getProcessModel(list(), name = name)
    model$inputType <- "inputModelNumber"
    model$modelNumber <- 4
    model$modelNumberIndependent <- "contGamma"
    model$modelNumberMediators <- list("debCollin1")
    model$modelNumberCovariates <- covariates
    model$independentCovariances <- FALSE
    return(model)
  }
  options$processModels <- list(hayesModel4("Model 1"), hayesModel4("Model 2", list("debMiss30")))
  set.seed(1)
  results <- jaspTools::runAnalysis("BayesianProcess", "debug.csv", options)

  summaryTable <- results[["results"]][["modelSummaryTable"]]
  expect_equal(sapply(summaryTable[["data"]][1:2], function(row) row[["N"]]), c(100, 70))
  expect_false(any(sapply(summaryTable[["data"]], function(row) "elpdDiff" %in% names(row))))
  footnotes <- sapply(summaryTable[["footnotes"]], function(fn) fn[["text"]])
  expect_true(jaspProcess:::.procIncomparableModelsMsg() %in% footnotes)
})

test_that("Residual Q-Q plot with credible intervals works", {
  options <- getOptionsBayesian()
  options$dependent <- "contNormal"
  options$covariates <- list("contGamma", "contcor1")
  options$factors <- list()
  processModel <- getProcessModel(list(
          list(processDependent = "contNormal", processIndependent = "contGamma",
              processType = "mediators", processVariable = "contcor1")))
  processModel$residualQqPlot <- TRUE
  processModel$qqPlotCi <- TRUE
  options$processModels <- list(processModel)
  set.seed(1)
  results <- jaspTools::runAnalysis("BayesianProcess", "debug.csv", options)

  modelPlots <- results[["results"]][["residualPlotContainer"]][["collection"]][["residualPlotContainer_Model 1"]][["collection"]]
  # One container per dependent variable: the outcome and the mediator
  expect_setequal(sapply(modelPlots, `[[`, "title"), c("contNormal", "contcor1"))

  outcomePlots <- modelPlots[[which(sapply(modelPlots, `[[`, "title") == "contNormal")]][["collection"]]
  plotName <- outcomePlots[[grep("residualQqPlot$", names(outcomePlots))]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]

  # One credible interval (error bar) per observation, around its posterior mean
  isErrorbar <- sapply(testPlot$layers, function(layer) inherits(layer$geom, "GeomErrorbar"))
  expect_equal(sum(isErrorbar), 1)
  bars <- ggplot2::layer_data(testPlot, which(isErrorbar))
  expect_equal(nrow(bars), 100)
  expect_true(all(bars$ymin < bars$y & bars$y < bars$ymax))
})
