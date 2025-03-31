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

	expect_equal(medEstimates, rev(refDf$ind))
	expect_equal(medEstimatesStd, rev(refDf$std))
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

	expect_equal(medEstimates, rev(refDf$ind))
	expect_equal(medEstimatesStd, rev(refDf$std))
})
