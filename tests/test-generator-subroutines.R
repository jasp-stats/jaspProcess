### Subroutines for Generating Unit Tests

###--------------------------------------------------------------------------------------------------------------------------###

.getOptionsOneModel <- function(k, parms) {
  options <- jaspTools::analysisOptions("ClassicProcess")
  options[["dependent"]]  <- parms$vars$y
  options[["covariates"]] <- parms$vars$numeric
  options[["factors"]]    <- parms$vars$factor

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

  options[["emulation"]] <- "lavaan"
  options[["errorCalculationMethod"]] <- "standard"
  options[["estimator"]] <- "default"
  options[["naAction"]] <- "fiml"

  options[["statisticalPathPlotsParameterEstimates"]] <- TRUE
  options[["statisticalPathPlotsCovariances"]] <- TRUE
  options[["statisticalPathPlotsResidualVariances"]] <- TRUE
  options[["pathPlotsLegend"]] <-  TRUE
  options[["pathPlotsLegendLabels"]] <- FALSE
  options[["pathPlotsLegendColor"]] <- TRUE
  options[["pathPlotsColor"]] <-  TRUE
  options[["colorPalette"]] <- "colorblind"
  options[["pathPlotsColorPalette"]] <-  "colorblind"
  options[["pathPlotsLabelLength"]] <- 3

  options[["processModels"]] <- list(
    list(
      "conceptualPathPlot" = TRUE,
      "independentCovariances" = TRUE,
      "inputType" = "inputVariables",
      "mediationEffects" = TRUE,
      "mediatorCovariances" = TRUE,
      "dependentCovariances" = TRUE,
      "modelNumber" = 1,
      "modelNumberCovariates" = list(),
      "modelNumberIndependent" = "",
      "modelNumberMediators" = list(),
      "modelNumberModeratorW" = "",
      "modelNumberModeratorZ" = "",
      "name" = "Model 1",
      "pathCoefficients" = TRUE,
	    "intercepts" = FALSE,
      "processRelationships" = list(),
      "residualCovariances" = TRUE,
      "statisticalPathPlot" = TRUE,
      "totalEffects" = TRUE,
      "localTests" = FALSE,
      "localTestType" = "cis",
      "localTestBootstrap" = FALSE,
      "localTestBootstrapSamples" = 1000
    )
  )
  return(options)
}

###--------------------------------------------------------------------------------------------------------------------------###

.replaceVariables <- function(x, map) {
  out <- map[names(map) %in% x] |> as.character()
  ifelse(length(out) > 0, out, x)
}

###--------------------------------------------------------------------------------------------------------------------------###

.getProcessRelationships <- function(k, parms) {
  customModels <- parms$customModels
  if(is.null(customModels))
    x <- jaspProcess:::.procGetHardCodedModel(k, 2)
  else
    x <- customModels[[k]]

  x |> lapply(lapply, .replaceVariables, map = parms$map)
}

###--------------------------------------------------------------------------------------------------------------------------###

.polishCapturedCode <- function(out) {
  out |>
    gsub("Now rendering a plot with name: conceptPathPlot", "", x = _) |>
    gsub("Now rendering a plot with name: statPathPlot", "", x = _) |>
    gsub("could not find an old plot", "", x = _) |>
    gsub("Did not store jaspResults", "", x = _) |>
    gsub("Created Write Seal for jaspResults at: ''", "", x = _) |>
    gsub(" analysisOptions", " jaspTools::analysisOptions", x = _) |>
    gsub(" runAnalysis", " jaspTools::runAnalysis", x = _) |>
    gsub("[\n]{4,}", "", x = _) |>
    gsub("test_that\\(\".+?\", \\{", "", x = _) |>
    gsub("\\}\\)", "", x = _)
}

###--------------------------------------------------------------------------------------------------------------------------###

.captureTestCode <- function(k, opts, parms, testObjective = "works") {
  out <- capture.output(jaspTools::runAnalysis("ClassicProcess", dataset = parms$data, options = opts, makeTests = TRUE)) |>
    paste(collapse = "\n") |>
    try(silent = TRUE)

  if (!inherits(out, "try-error")) {
    type <- parms$type
    out <- out |>
      .polishCapturedCode() |>
      gsub("conceptual-path-plot", paste("conceptual-path-plot", type, k, sep = "-"), x = _) |>
      gsub("statistical-path-plot", paste("statistical-path-plot", type, k, sep = "-"), x = _)

    out <- paste0("test_that(\"Test that model number ", k, " - ", type, " ", testObjective, "\", {\n", out, "\n})")
  }
  out
}

###--------------------------------------------------------------------------------------------------------------------------###

makeTestString <- function(k, parms, ...) {
  opts <- .getOptionsOneModel(k, parms)
  opts$processModels[[1]]$processRelationships <- .getProcessRelationships(k, parms)
  .captureTestCode(k, opts, parms, ...)
}
