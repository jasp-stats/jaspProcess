# Verification tests against established results for classic process analysis

test_that("Test that single moderation effect matches validated results", {

  options <- getOptionsClassical()
  options$dependent <- "GovernmentAction"
  options$covariates <- list("NegativeEmotion", "Age")
  options$factors <- list()
  options$meanCenteredModeration <- FALSE

  options$processModels <- list(getProcessModel(list(list(processDependent = "GovernmentAction",
          processIndependent = "NegativeEmotion", processType = "moderators",
          processVariable = "Age"))))
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", test_path("global_warming.csv"), options)

	# Check if coefficient estimates match those from Hayes (2022; Table 9.1, p. 322)
  coefTable <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_pathCoefficientsTable"]][["data"]]

	# Original estimates
	coefEstimates <- sapply(coefTable, function(x) x[["est"]])
	coefSes <- sapply(coefTable, function(x) x[["se"]])
	coefEstimatesStd <- sapply(coefTable, function(x) x[["est.std"]])

	# Hayes only provides estimates with 3 decimals
	expect_equal(round(coefEstimates, 3), c(0.147, -0.031, 0.007))
	expect_equal(round(coefSes, 3), c(0.085, 0.006, 0.002))

	# Standardized moderation effect (we leave out the first two estimates because they are different but not meaningful)
	expect_equal(round(coefEstimatesStd[3], 3), 0.131)

	# Check if conditional effects match those from manymome package
	refDf <- read.csv(test_path("global_warming_cond_indirect_effects.csv"))

	medTable <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_mediationEffectsTable"]][["data"]]

	medEstimates <- sapply(medTable, function(x) x[["est"]])
	medEstimatesStd <- sapply(medTable, function(x) x[["est.std"]])

	expect_equal(medEstimates, rev(refDf$ind), tolerance = 1e-7)
	expect_equal(medEstimatesStd, rev(refDf$std), tolerance = 1e-7)
})

test_that("Test that moderated mediation effect matches validated results", {

  options <- getOptionsClassical()
  options$dependent <- "DonationAttitude"
  options$covariates <- list("Justification", "Skepticism")
  options$factors <- list("Framing")
  options$meanCenteredModeration <- FALSE

  options$processModels <- list(getProcessModel(list(list(processDependent = "DonationAttitude",
          processIndependent = "Framing", processType = "mediators",
          processVariable = "Justification"), list(processDependent = "Justification",
          processIndependent = "Framing", processType = "moderators",
          processVariable = "Skepticism"), list(processDependent = "DonationAttitude",
          processIndependent = "Framing", processType = "moderators",
          processVariable = "Skepticism"))))
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", test_path("climate_change_disasters.csv"), options)

	# Check if coefficient estimates match those from Hayes (2022; Table 12.1, p. 463)
  coefTable <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_pathCoefficientsTable"]][["data"]]

	# Original estimates
	coefEstimates <- sapply(coefTable, function(x) x[["est"]])
	coefSes <- sapply(coefTable, function(x) x[["se"]])

	# Hayes only provides estimates with 3 decimals
	expect_equal(round(coefEstimates, 3), c(0.160, -0.923, -0.043, 0.015, -0.562, 0.105, 0.201))
	# SEs differ slightly due to estimation procedure
	expect_equal(round(coefSes, 3), c(0.268, 0.084, 0.047, 0.069, 0.218, 0.038, 0.055), tolerance = 1e-2)

	# Check if conditional effects match those from manymome package
	refDf <- read.csv(test_path("climate_change_disasters_cond_indirect_effects.csv"))

	medTable <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_mediationEffectsTable"]][["data"]]

	medEstimates <- sapply(medTable, function(x) x[["est"]])
	medEstimatesStd <- sapply(medTable, function(x) x[["est.std"]])

	expect_equal(medEstimates, rev(refDf$ind), tolerance = 1e-7)
	expect_equal(medEstimatesStd, rev(refDf$std), tolerance = 1e-7)
})

# Covariates in the Hayes configuration input enter every mediator and outcome
# equation (the PROCESS default), so the path coefficients must equal the OLS
# estimates of these equations (jasp-stats/jaspProcess#63)

getHayesModelCovariates <- function(modelNumber, independent, mediators = list(), modW = "", covariates = list()) {
  model <- getProcessModel(list())
  model$inputType <- "inputModelNumber"
  model$modelNumber <- modelNumber
  model$modelNumberIndependent <- independent
  model$modelNumberMediators <- mediators
  model$modelNumberModeratorW <- modW
  model$modelNumberModeratorZ <- ""
  model$modelNumberCovariates <- covariates
  return(model)
}

getPathCoefficients <- function(results, dependent) {
  coefTable <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_pathCoefficientsTable"]][["data"]]
  coefTable <- Filter(function(row) row[["rhs"]] == dependent, coefTable)
  return(setNames(sapply(coefTable, function(row) row[["est"]]), sapply(coefTable, function(row) row[["lhs"]])))
}

test_that("Test that moderation with covariates in Hayes configuration matches linear regression", {
  options <- getOptionsClassical()
  options$dependent <- "GovernmentAction"
  options$covariates <- list("NegativeEmotion", "Age", "PositiveEmotion", "Ideology", "Sex")
  options$factors <- list()
  options$naAction <- "listwise"
  options$meanCenteredModeration <- FALSE
  options$standardizedModelEstimates <- FALSE
  options$processModels <- list(getHayesModelCovariates(1, "NegativeEmotion", modW = "Age",
                                                        covariates = list("PositiveEmotion", "Ideology", "Sex")))
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", test_path("global_warming.csv"), options)

  data <- read.csv(test_path("global_warming.csv"))
  fit <- lm(GovernmentAction ~ NegativeEmotion * Age + PositiveEmotion + Ideology + Sex, data = data)
  refCoefs <- coef(fit)[-1]

  coefs <- getPathCoefficients(results, "GovernmentAction")
  expect_setequal(names(coefs), names(refCoefs))
  expect_equal(coefs[names(refCoefs)], refCoefs, tolerance = 1e-6)

  # Covariates are not focal: only conditional effects of X are reported
  medTable <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_mediationEffectsTable"]][["data"]]
  expect_length(medTable, 3)
  expect_true(all(sapply(medTable, function(row) row[["lhs_1"]]) == "NegativeEmotion"))
  probes <- quantile(data$Age, c(0.16, 0.5, 0.84))
  expect_equal(sapply(medTable, function(row) row[["est"]]),
               unname(refCoefs["NegativeEmotion"] + refCoefs["NegativeEmotion:Age"] * probes), tolerance = 1e-6)

  # Covariates do not change the recognized Hayes configuration
  summaryTable <- results[["results"]][["modelSummaryTable"]][["data"]]
  expect_equal(summaryTable[[1]][["modelNumber"]], 1)

  rSquaredTable <- results[["results"]][["rSquaredTable"]][["data"]]
  expect_equal(rSquaredTable[[1]][["rsq_1"]], summary(fit)$r.squared, tolerance = 1e-6)
})

test_that("Test that mediation with continuous and categorical covariates in Hayes configuration matches linear regression", {
  options <- getOptionsClassical()
  options$naAction <- "listwise"
  options$meanCenteredModeration <- FALSE
  options$standardizedModelEstimates <- FALSE
  options$processModels <- list(getHayesModelCovariates(4, "contGamma", mediators = list("debCollin1"),
                                                        covariates = list("contcor2", "facGender")))
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicProcess", "debug.csv", options)

  data <- read.csv(system.file("jaspData", "debug.csv", package = "jaspTools"))
  data <- data[complete.cases(data[, c("contNormal", "contGamma", "contcor1", "contcor2", "debCollin1", "facGender", "facExperim")]), ]
  data$facGender <- factor(data$facGender)
  refCoefsM <- coef(lm(debCollin1 ~ contGamma + contcor2 + facGender, data = data))[-1]
  refCoefsY <- coef(lm(contNormal ~ contGamma + debCollin1 + contcor2 + facGender, data = data))[-1]

  coefsM <- getPathCoefficients(results, "debCollin1")
  coefsY <- getPathCoefficients(results, "contNormal")
  expect_setequal(names(coefsM), names(refCoefsM))
  expect_setequal(names(coefsY), names(refCoefsY))
  expect_equal(coefsM[names(refCoefsM)], refCoefsM, tolerance = 1e-6)
  expect_equal(coefsY[names(refCoefsY)], refCoefsY, tolerance = 1e-6)

  # Covariates are not focal: only the direct and indirect effect of X are reported
  medTable <- results[["results"]][["parEstContainer"]][["collection"]][["parEstContainer_Model 1"]][["collection"]][["parEstContainer_Model 1_mediationEffectsTable"]][["data"]]
  expect_length(medTable, 2)
  expect_true(all(sapply(medTable, function(row) row[["lhs_1"]]) == "contGamma"))
  expect_equal(sapply(medTable, function(row) row[["est"]]),
               unname(c(refCoefsY["contGamma"], refCoefsM["contGamma"] * refCoefsY["debCollin1"])), tolerance = 1e-6)

  summaryTable <- results[["results"]][["modelSummaryTable"]][["data"]]
  expect_equal(summaryTable[[1]][["modelNumber"]], 4)
})
