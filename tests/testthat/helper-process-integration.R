### Helper files for integration tests

getOptionsClassical <- function() {
  options <- jaspTools::analysisOptions("ClassicProcess")
  options[["dependent"]] <- "contNormal"
  options[["covariates"]] <- list("contGamma", "contcor1", "contcor2", "debCollin1")
  options[["factors"]] <- list("facGender", "facExperim")

  options[["moderationProbes"]] <- list(
    list(
      "probePercentile" = 16.0,
      "value" = "16"
    ),
    list(
      "probePercentile" = 50.0,
      "value" = "50"
    ),
    list(
      "probePercentile" = 84.0,
      "value" = "84"
    )
  )

  options[["hayesNumber"]] <- TRUE
  options[["emulation"]] <- "lavaan"
  options[["errorCalculationMethod"]] <- "standard"
  options[["estimator"]] <- "default"
  options[["naAction"]] <- "fiml"
  options[["standardizedModelEstimates"]] <- TRUE

  options[["statisticalPathPlotsParameterEstimates"]] <- FALSE
  options[["statisticalPathPlotsCovariances"]] <- TRUE
  options[["statisticalPathPlotsResidualVariances"]] <- TRUE
  options[["pathPlotsLegend"]] <-  FALSE
  options[["pathPlotsLegendLabels"]] <- FALSE
  options[["pathPlotsLegendColor"]] <- FALSE
  options[["pathPlotsColor"]] <-  TRUE
  options[["colorPalette"]] <- "colorblind"
  options[["pathPlotsColorPalette"]] <-  "colorblind"
  options[["pathPlotsLabelLength"]] <- 3

  return(options)
}

getOptionsBayesian <- function() {
  options <- jaspTools::analysisOptions("BayesianProcess")
  options[["dependent"]] <- "contNormal"
  options[["covariates"]] <- list("contGamma", "contcor1", "contcor2", "debCollin1")
  options[["factors"]] <- list("facGender", "facExperim")

  options[["moderationProbes"]] <- list(
    list(
      "probePercentile" = 16.0,
      "value" = "16"
    ),
    list(
      "probePercentile" = 50.0,
      "value" = "50"
    ),
    list(
      "probePercentile" = 84.0,
      "value" = "84"
    )
  )

  options[["mcmcBurnin"]] <- 50
  options[["mcmcChains"]] <- 1
  options[["mcmcSamples"]] <- 200

  # options[["densityPlot"]] <- TRUE
  # options[["tracePlot"]] <- TRUE
  # options[["autoCorPlot"]] <- TRUE

  options[["priorDistributions"]] <- TRUE
  options[["nuPriorMu"]] <- 0
  options[["nuPriorSigma"]] <- 32
  options[["betaPriorMu"]] <-  0
  options[["betaPriorSigma"]] <- 10
  options[["psiPriorAlpha"]] <-  1
  options[["psiPriorBeta"]] <-  0.5
  options[["rhoPriorAlpha"]] <- 1
  options[["rhoPriorBeta"]] <-  1

  options[["statisticalPathPlotsParameterEstimates"]] <- FALSE
  options[["statisticalPathPlotsCovariances"]] <- TRUE
  options[["statisticalPathPlotsResidualVariances"]] <- TRUE
  options[["pathPlotsLegend"]] <- FALSE
  options[["pathPlotsLegendLabels"]] <- FALSE
  options[["pathPlotsLegendColor"]] <- TRUE
  options[["pathPlotsColor"]] <-  TRUE
  options[["colorPalette"]] <- "colorblind"
  options[["pathPlotsColorPalette"]] <-  "colorblind"
  options[["pathPlotsLabelLength"]] <- 3

  options[["seed"]] <- 1
  options[["setSeed"]] <- TRUE

  return(options)
}

getProcessModel <- function(processRelationships, name = "Model 1") {
  return(list(conceptualPathPlot = TRUE, independentCovariances = TRUE,
              inputType = "inputVariables", mediationEffects = TRUE, mediatorCovariances = TRUE,
              dependentCovariances = TRUE, modelNumber = 1, modelNumberCovariates = list(),
              modelNumberIndependent = list(value = ""), modelNumberMediators = list(value = c()),
              modelNumberModeratorW = list(value = ""), modelNumberModeratorZ = list(value = ""), name = name,
              pathCoefficients = TRUE, intercepts = FALSE, processRelationships = processRelationships,
              residualCovariances = TRUE, statisticalPathPlot = TRUE, totalEffects = TRUE,
              localTests = FALSE, localTestType = "cis", localTestBootstrap = FALSE,
              localTestBootstrapSamples = 1000))
}
