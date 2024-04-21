### Helper functions for test generation


# Hayes models (classical) ------------------------------------------------

getOptionsOneModel <- function() {
  options <- getOptionsClassical()

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


replaceVariablesContinuous <- function(v) {
  return(switch(v,
                "JaspProcess_Dependent_Encoded" = "contNormal",
                "JaspProcess_Independent_Encoded" = "contGamma",
                "JaspProcess_Mediator_Encoded" = "debCollin1",
                "JaspProcess_ModeratorW_Encoded" = "contcor1",
                "JaspProcess_ModeratorZ_Encoded" = "contcor2",
                "JaspProcess_Mediator_Encoded1" = "debCollin1",
                "JaspProcess_Mediator_Encoded2" = "contcor2",
                "JaspProcess_Mediator_Encoded3" = "debMiss1",
                "JaspProcess_Mediator_Encoded4" = "contcor1",
                v
  ))
}

replaceVariablesFactors <- function(v) {
  return(switch(v,
                "JaspProcess_Dependent_Encoded" = "contNormal",
                "JaspProcess_Independent_Encoded" = "facGender",
                "JaspProcess_Mediator_Encoded" = "debCollin1",
                "JaspProcess_ModeratorW_Encoded" = "facExperim",
                "JaspProcess_ModeratorZ_Encoded" = "contcor2",
                "JaspProcess_Mediator_Encoded1" = "debCollin1",
                "JaspProcess_Mediator_Encoded2" = "contcor2",
                "JaspProcess_Mediator_Encoded3" = "debMiss1",
                "JaspProcess_Mediator_Encoded4" = "contcor1",
                v
  ))
}


addProcessRelationshipsFromModelNumber <- function(k, options, replaceFun) {

  processRelationships <- jaspProcess:::.procGetHardCodedModel(k, 2)
  processRelationships <- lapply(processRelationships, function(row) {
    row <- lapply(row, replaceFun)
    return(row)
  })

  options$processModels[[1]][["processRelationships"]] <- processRelationships

  if (k >= 82) {
    options[["covariates"]] <- c(options[["covariates"]], "debMiss1")
  }

  return(options)
}

polishCapturedCode <- function(out) {
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

captureCodeContinuous <- function(k, opts, type = "ClassicProcess") {
  out <- try(paste(capture.output(jaspTools::runAnalysis(type, "debug", opts, makeTests = TRUE)), collapse = "\n"))
  if (!inherits(out, "try-error")) {
    out <- polishCapturedCode(out)
    out <- gsub("conceptual-path-plot", paste0("conceptual-path-plot-continuous-", k), out)
    out <- gsub("statistical-path-plot", paste0("statistical-path-plot-continuous-", k), out)
    out <- paste0("test_that(\"Test that model number ", k, " - continuous works\", {\n", out, "\n})")
    return(out)
  }
  cat("Continuous", k)
  return("")
}

captureCodeFactor <- function(k, opts, type = "ClassicProcess") {
  out <- try(paste(capture.output(jaspTools::runAnalysis(type, "debug", opts, makeTests = TRUE)), collapse = "\n"))
  if (!inherits(out, "try-error")) {
    out <- polishCapturedCode(out)
    out <- gsub("conceptual-path-plot", paste0("conceptual-path-plot-factor-", k), out)
    out <- gsub("statistical-path-plot", paste0("statistical-path-plot-factor-", k), out)
    out <- paste0("test_that(\"Test that model number ", k, " - factor works\", {\n", out, "\n})")
    return(out)
  }
  cat("Factor", k)
  return("")
}


# Custom models (classical) -----------------------------------------------

addProcessRelationshipsFromCustomModel <- function(k, options, replaceFun) {
  processRelationships <- customModels[[as.character(k)]]
  processRelationships <- lapply(processRelationships, function(row) {
    row <- lapply(row, replaceFun)
    return(row)
  })

  options$processModels[[1]][["processRelationships"]] <- processRelationships

  return(options)
}

replaceVariablesContinuousCustom <- function(v) {
  return(switch(v,
                "Y" = "contNormal",
                "X" = "contGamma",
                "M" = "debCollin1",
                "W" = "contcor1",
                "Z" = "contcor2",
                v
  ))
}

replaceVariablesFactorsCustom <- function(v) {
  return(switch(v,
                "Y" = "contNormal",
                "X" = "contGamma",
                "M" = "debCollin1",
                "W" = "facExperim",
                "Z" = "facGender",
                v
  ))
}


# Bad variable names (classical) ------------------------------------------

getOptionsOneModelBadVarNames <- function(k, parms) {
  options <- getOptionsClassical()
  options[["dependent"]]  <- parms$vars$y
  options[["covariates"]] <- parms$vars$numeric
  options[["factors"]]    <- parms$vars$factor

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

replaceVariables <- function(x, map) {
  out <- map[names(map) %in% x] |> as.character()
  ifelse(length(out) > 0, out, x)
}

getProcessRelationships <- function(k, parms) {
  customModels <- parms$customModels
  if(is.null(customModels))
    x <- jaspProcess:::.procGetHardCodedModel(k, 2)
  else
    x <- customModels[[k]]

  x |> lapply(lapply, replaceVariables, map = parms$map)
}

captureTestCode <- function(k, opts, parms, testObjective = "works") {
  out <- capture.output(jaspTools::runAnalysis("ClassicProcess", dataset = parms$data, options = opts, makeTests = TRUE)) |>
    paste(collapse = "\n") |>
    try(silent = TRUE)

  if (!inherits(out, "try-error")) {
    type <- parms$type
    out <- out |>
      polishCapturedCode() |>
      gsub("conceptual-path-plot", paste("conceptual-path-plot", type, k, sep = "-"), x = _) |>
      gsub("statistical-path-plot", paste("statistical-path-plot", type, k, sep = "-"), x = _) |>
      gsub(testthat::test_path("/"), "", x = _)

    out <- paste0("test_that(\"Test that model number ", k, " - ", type, " ", testObjective, "\", {\n", out, "\n})")
  }
  out
}

makeTestString <- function(k, parms, ...) {
  opts <- getOptionsOneModelBadVarNames(k, parms)
  opts$processModels[[1]]$processRelationships <- getProcessRelationships(k, parms)
  captureTestCode(k, opts, parms, ...)
}


# Hayes models (Bayesian) -------------------------------------------------

getOptionsOneModelBayes <- function() {
  options <- getOptionsBayesian()

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
      "totalEffects" = TRUE
    )
  )
  return(options)
}
