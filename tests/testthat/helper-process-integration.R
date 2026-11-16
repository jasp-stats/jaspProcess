### Helper files for integration tests

# Reduce numeric precision to avoid cross-platform rounding boundary failures.
# First round to 4 decimal places, then to 3 significant digits. This is
# coarse enough to absorb platform drift around tie boundaries while still
# preserving the broad numeric structure of the expected tables.
options("jaspRoundToPrecision" = function(x) signif(round(x, digits = 4), digits = 3))

# jaspTools installs the module (when its sources changed) before each runAnalysis(). Unit tests that call
# jaspProcess::: functions directly need the same, as no runAnalysis() may have run yet (e.g. on CI, where the
# Bayesian integration tests are skipped).
installJaspProcessForUnitTests <- function() {
  jaspTools:::reinstallChangedModules()
}

# Unit tests check the logic on plain variable names, but a preceding runAnalysis() leaves the column encoder of its
# data set behind, with which encodeColNames() would encode them. Lasts until `env` (the caller) exits.
localPlainColumnNames <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(encodeColNames = function(x, ...) x, .package = "jaspProcess", .env = env)
}

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
  options[["moderationProbeType"]] <- "percentile"
  options[["moderationProbesMeanSD"]] <- list(
    list("probeSD" = -1, "value" = "-1"),
    list("probeSD" = 0,  "value" = "0"),
    list("probeSD" = 1,  "value" = "1")
  )

  options[["hayesNumber"]] <- TRUE
  options[["emulation"]] <- "lavaan"
  options[["errorCalculationMethod"]] <- "standard"
  options[["estimator"]] <- "default"
  options[["naAction"]] <- "fiml"
  options[["standardizedModelEstimates"]] <- TRUE
  options[["meanCenteredModeration"]] <- TRUE

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
  options[["moderationProbeType"]] <- "percentile"
  options[["moderationProbesMeanSD"]] <- list(
    list("probeSD" = -1, "value" = "-1"),
    list("probeSD" = 0,  "value" = "0"),
    list("probeSD" = 1,  "value" = "1")
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

  options[["meanCenteredModeration"]] <- TRUE

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
  # JASP's ComponentsList keys its rows by `value` ("#", "#2", ...): the analysis form merges rows without one
  for (i in seq_along(processRelationships))
    if (is.null(processRelationships[[i]][["value"]]))
      processRelationships[[i]][["value"]] <- if (i == 1) "#" else paste0("#", i)

  return(list(conceptualPathPlot = TRUE, independentCovariances = TRUE,
              inputType = "inputVariables", mediationEffects = TRUE, mediatorCovariances = TRUE,
              dependentCovariances = TRUE, modelNumber = 1, modelNumberCovariates = list(),
              modelNumberIndependent = list(value = ""), modelNumberMediators = list(value = c()),
              modelNumberModeratorW = list(value = ""), modelNumberModeratorZ = list(value = ""), name = name,
              pathCoefficients = TRUE, intercepts = FALSE, processRelationships = processRelationships,
              residualCovariances = TRUE, statisticalPathPlot = TRUE, residualVsFittedPlot = FALSE, residualQqPlot = FALSE,
              qqPlotCi = FALSE, qqPlotCiLevel = 0.95, totalEffects = TRUE,
              localTests = FALSE, localTestType = "cis", localTestBootstrap = FALSE,
              localTestBootstrapSamples = 1000))
}
