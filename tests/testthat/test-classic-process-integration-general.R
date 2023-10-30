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
