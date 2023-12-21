# Script for generating integration tests for all hard-coded models

getOptionsOneModel <- function() {
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

  options[["emulation"]] = "lavaan"
  options[["errorCalculationMethod"]] = "standard"
  options[["estimator"]] = "default"
  options[["naAction"]] = "fiml"

  options[["statisticalPathPlotsCovariances"]] = TRUE
  options[["statisticalPathPlotsResidualVariances"]] = TRUE
  options[["pathPlotsLegend"]] = TRUE
  options[["pathPlotsColor"]] = TRUE
  options[["pathPlotsColorPalette"]] = "colorblind"

  options[["processModels"]] <- list(
    list(
      "conceptualPathPlot" = TRUE,
      "independentCovariances" = TRUE,
      "inputType" = "inputVariables",
      "mediationEffects" = TRUE,
      "mediatorCovariances" = TRUE,
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
  out <- gsub("Now rendering a plot with name: conceptPathPlot", "", out)
  out <- gsub("Now rendering a plot with name: statPathPlot", "", out)
  out <- gsub("could not find an old plot", "", out)
  out <- gsub("Did not store jaspResults", "", out)
  out <- gsub("Created Write Seal for jaspResults at: ''", "", out)
  out <- gsub(" analysisOptions", " jaspTools::analysisOptions", out)
  out <- gsub(" runAnalysis", " jaspTools::runAnalysis", out)
  out <- gsub("[\n]{4,}", "", out)
  out <- gsub("test_that\\(\".+?\", \\{", "", out)
  out <- gsub("\\}\\)", "", out)
  return(out)
}

captureCodeContinuous <- function(k, opts) {
  out <- try(paste(capture.output(jaspTools::runAnalysis("ClassicProcess", "debug", opts, makeTests = TRUE)), collapse = "\n"))
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

captureCodeFactor <- function(k, opts) {
  out <- try(paste(capture.output(jaspTools::runAnalysis("ClassicProcess", "debug", opts, makeTests = TRUE)), collapse = "\n"))
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

fileContext <- file("tests/testthat/test-classic-process-integration-hayes-models.R")
header <- "# This code is automatically generated by 'generate-tests.R'"
testCode <- ""

for (k in jaspProcess:::.procHardCodedModelNumbers()) {
  if (k %in% c(6, 80, 81)) next
  opts <- getOptionsOneModel()
  opts <- addProcessRelationshipsFromModelNumber(k, opts, replaceVariablesContinuous)
  testCode <- paste(testCode, captureCodeContinuous(k, opts), sep = "\n")
  opts <- getOptionsOneModel()
  opts <- addProcessRelationshipsFromModelNumber(k, opts, replaceVariablesFactors)
  testCode <- paste(testCode, captureCodeFactor(k, opts), sep = "\n")
}

writeLines(paste(header, testCode, sep = "\n\n"), fileContext)
close(fileContext)


customModels <- list(
  "one_confounder" = list(
    list(
      processDependent = "Y",
      processIndependent = "X",
      processType = "confounders",
      processVariable = "W"
    )
  ),
  "one_direct" = list(
    list(
      processDependent = "Y",
      processIndependent = "X",
      processType = "directs",
      processVariable = ""
    )
  ),
  "two_confounder" = list(
    list(
      processDependent = "Y",
      processIndependent = "X",
      processType = "confounders",
      processVariable = "W"
    ),
    list(
      processDependent = "Y",
      processIndependent = "X",
      processType = "confounders",
      processVariable = "Z"
    )
  ),
  "confounder_X_Y" = list(
    list(
      processDependent = "Y",
      processIndependent = "X",
      processType = "mediators",
      processVariable = "M"
    ),
    list(
      processDependent = "Y",
      processIndependent = "X",
      processType = "confounders",
      processVariable = "Z"
    )
  ),
  "confounder_X_M" = list(
    list(
      processDependent = "Y",
      processIndependent = "X",
      processType = "mediators",
      processVariable = "M"
    ),
    list(
      processDependent = "M",
      processIndependent = "X",
      processType = "confounders",
      processVariable = "Z"
    )
  ),
  "confounder_M_Y" = list(
    list(
      processDependent = "Y",
      processIndependent = "X",
      processType = "mediators",
      processVariable = "M"
    ),
    list(
      processDependent = "Y",
      processIndependent = "M",
      processType = "confounders",
      processVariable = "Z"
    )
  ),
  "confounder_moderator" = list(
    list(
      processDependent = "Y",
      processIndependent = "X",
      processType = "moderators",
      processVariable = "W"
    ),
    list(
      processDependent = "Y",
      processIndependent = "X",
      processType = "confounders",
      processVariable = "Z"
    )
  )
)

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

fileContext <- file("tests/testthat/test-classic-process-integration-custom-models.R")
header <- "# This code is automatically generated by 'generate-tests.R'"
testCode <- ""
for (k in names(customModels)) {
  opts <- getOptionsOneModel()
  opts <- addProcessRelationshipsFromCustomModel(k, opts, replaceVariablesContinuousCustom)
  testCode <- paste(testCode, captureCodeContinuous(k, opts), sep = "\n")
  opts <- getOptionsOneModel()
  opts <- addProcessRelationshipsFromCustomModel(k, opts, replaceVariablesFactorsCustom)
  testCode <- paste(testCode, captureCodeFactor(k, opts), sep = "\n")
}
writeLines(paste(header, testCode, sep = "\n\n"), fileContext)
close(fileContext)
