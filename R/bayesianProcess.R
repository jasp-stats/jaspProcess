#
# Copyright (C) 2023 University of Amsterdam and Netherlands eScience Center
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# Main function ----
BayesianProcess <- function(jaspResults, dataset = NULL, options) {
  # Set title
  jaspResults$title <- gettext("Process Analysis")
  # Check if all models are ready to compute something
  ready <- .procIsReady(options)
  if (!ready) {
    # create empty summary table
    .procBayesModelSummaryTable(jaspResults, options, NULL)
    return()
  }
  options$naAction <- "listwise"
  # Read dataset
  dataset <- .procReadData(options)
  # Check for errors in dataset
  .procErrorHandling(dataset, options)
  # Create a container for each model
  modelsContainer <- .procContainerModels(jaspResults, options)
  
  if (ready) {
    # Transform input for each model into a graph for further processing
    .procModelGraph(jaspResults, options)
    # Add factor dummy variables manually to dataset
    # because lavaan does not do it automatically;
    # also add three-way interaction variables manually to dataset 
    # because lavaan does not allow threeway interaction regression terms
    dataset <- .procAddFactorDummyIntVars(jaspResults, dataset, options)
    # Add parameter names to graph of each model
    .procGraphAddParNames(jaspResults, options)
    # Compute quantiles at which to probe moderators for each model
    .procModProbes(jaspResults, dataset, options)
    # Add undirected graph for variances and covariances
    .procResCovGraph(jaspResults, options)
    # Create lavaan syntax for each model from graph
    .procModelSyntax(jaspResults, options)
    # Fit lavaan models based on syntax and dataset and update models container
    modelsContainer <- .procBayesComputeResults(jaspResults, dataset, options)
    # Create container for path plots for each model
    pathPlotContainer <- .procContainerPathPlots(jaspResults, options)
    # Create path plots for each model and add to container
    .procPathPlots(pathPlotContainer, options, modelsContainer)
    # Create table with model fit indices (AIC, ...)
    .procBayesModelSummaryTable(jaspResults, options, modelsContainer)
    # Create container for parameter estimates for each model
    parEstContainer <- .procContainerParameterEstimates(jaspResults, options, modelsContainer)
    # Create tables for parameter estimates
    .procBayesParameterEstimateTables(parEstContainer, options, modelsContainer)

    .procBayesMcmcPlots(jaspResults, options, modelsContainer)

    # Create html output with lavaan syntax for each model
    .procPlotSyntax(jaspResults, options, modelsContainer)
  }

  return()
}

.procBayesComputeResults <- function(jaspResults, dataset, options) {
  modelsContainer <- jaspResults[["modelsContainer"]]
  nModels <- length(options[["processModels"]])

  for (i in 1:nModels) {
    modelOptions <- options[["processModels"]][[i]]
    modelName <- modelOptions[["name"]]

    if (is.null(modelsContainer[[modelName]][["fittedModel"]])) {
      if (modelOptions[["inputType"]] == "inputModelNumber" && !modelOptions[["modelNumber"]] %in% .procHardCodedModelNumbers()) {
        fittedModel <- .procHayesModelMsg(modelName, modelOptions[["modelNumber"]])
      } else {
        fittedModel <- .procBayesResultsFitModel(
          modelsContainer[[modelName]],
          dataset,
          options,
          modelOptions
        )
      }
      state <- createJaspState(object = fittedModel)
      modelsContainer[[modelName]][["fittedModel"]] <- state
    }
  }

  return(modelsContainer)
}

.procBayesGetPriors <- function(options) {
  nuPrior   <- sprintf("normal(%s,%s)", options$nuPriorMu, options$nuPriorSigma)
  betaPrior <- sprintf("normal(%s,%s)", options$betaPriorMu, options$betaPriorSigma)
  psiPrior  <- sprintf("gamma(%s,%s)[sd]", options$psiPriorAlpha, options$psiPriorBeta)
  rhoPrior  <- sprintf("beta(%s,%s)[sd]", options$rhoPriorAlpha, options$rhoPriorBeta)
  return(blavaan::dpriors(nu = nuPrior, beta = betaPrior, psi = psiPrior, rho = rhoPrior))
}

.procBayesResultsFitModel <- function(container, dataset, options, modelOptions) {
  # Check if graph has error message
  if (!.procCheckGraph(container[["graph"]]$object) && jaspBase::isTryError(container[["graph"]]$object)) {
    return(.procEstimationMsg(container[["graph"]]$object))
  }

  # Should model be fitted?
  doFit <- .procCheckFitModel(container[["graph"]]$object)

  if (!doFit) {
    dataset <- NULL
  }

  # Necessary for JASP to find function blavaan
  blavaan <- blavaan::blavaan

  # Suppress console output
  invisible(capture.output(fittedModel <- try(blavaan::bsem(
    model           = container[["syntax"]]$object,
    data            = dataset,
    n.chains        = options$mcmcChains,
    burnin          = options$mcmcBurnin,
    sample          = options$mcmcSamples,
    do.fit          = doFit,
    target          = "stan",
    dp              = .procBayesGetPriors(options),
    seed            = .getSeedJASP(options),
    fixed.x         = !modelOptions$independentCovariances,
    auto.cov.y      = FALSE,
    meanstructure   = modelOptions$intercepts
  ))))
  
  if (jaspBase::isTryError(fittedModel)) {
    return(.procLavaanMsg(fittedModel))
  }

  if (doFit) {
    container[["graph"]]$object <- .procGraphAddEstimates(container[["graph"]]$object, fittedModel)
    container[["resCovGraph"]]$object <- .procGraphAddEstimates(container[["resCovGraph"]]$object, fittedModel, type = "variances")
  }

  return(fittedModel)
}

# Output functions ----

.procBayesCalcModelCaseLogLik <- function(fittedModel) {
  # Adapted from blavaan::blavCompare function
  # See https://github.com/ecmerkle/blavaan/blob/master/R/blav_compare.R

  lavopt <- blavaan::blavInspect(fittedModel, "Options")

  if(lavopt$target == "stan" && blavaan::blavInspect(fittedModel, "meanstructure")){
    return(loo::extract_log_lik(fittedModel@external$mcmcout))
  } else if(blavaan::blavInspect(fittedModel, "categorical") && lavopt$test != "none"){
    return(fittedModel@external$casells)
  }

  lavopt$estimator <- "ML"

  # Model comparison only works using these internal blavaan functions; however, they might change in the future
  return(blavaan:::case_lls(fittedModel@external$mcmcout, blavaan:::make_mcmc(fittedModel@external$mcmcout), fittedModel))
}

.procBayesCalcRelEff <- function(fittedModel, logLik) {
  # Adapted from blavaan::blavCompare function
  # See https://github.com/ecmerkle/blavaan/blob/master/R/blav_compare.R

  nChains <- blavaan::blavInspect(fittedModel, "n.chains")
  nIter <- nrow(logLik)/nChains
  chainId <- rep(1:nChains, each=nIter)

  return(loo::relative_eff(exp(logLik), chain_id = chainId))
}

.procBayesModelSummaryTable <- function(jaspResults, options, modelsContainer) {
  if (!is.null(jaspResults[["modelSummaryTable"]])) return()

  modelNumbers <- lapply(options[["processModels"]], function(mod) {
    graph <- modelsContainer[[mod[["name"]]]][["graph"]]$object
    if (!igraph::is.igraph(graph)) return(NULL)
    return(.procRecognizeModelNumber(graph))
  })

  modelNumberIsValid <- !sapply(modelNumbers, is.null)

  modelNames <- sapply(options[["processModels"]], function(mod) mod[["name"]])

  procResults <- lapply(options[["processModels"]], function(mod) modelsContainer[[mod[["name"]]]][["fittedModel"]]$object)

  # Remove invalid models
  resultIsValid <- sapply(procResults, function(mod) inherits(mod, "lavaan") && mod@Options[["do.fit"]])
  procResults <- procResults[resultIsValid]

  tableRowIsValid <- modelNumberIsValid & resultIsValid

  summaryTable <- createJaspTable(title = gettext("Model summary"), rowNames = modelNames[tableRowIsValid])
  summaryTable$dependOn(c(.procGetDependencies(), "processModels", "bicWeights", "hayesNumber"))
  summaryTable$position <- 1

  ovtWaic <- gettext("WAIC")
  ovtLoo  <- gettext("LOOIC")

  summaryTable$addColumnInfo(name = "Model",        title = "",                             type = "string")
  .procAddHayesModelColumn(summaryTable, options)
  summaryTable$addColumnInfo(name = "ppp",          title = gettext("PPP"),                 type = "number")
  summaryTable$addColumnInfo(name = "bic",          title = gettext("BIC"),                 type = "number")
  if (options[["bicWeights"]]) {
    summaryTable$addColumnInfo(name = "wBIC",       title = gettext("BIC weight"),      type = "number")
  }
  summaryTable$addColumnInfo(name = "waicEst",      title = gettext("Estimate"),            type = "number", overtitle = ovtWaic)
  summaryTable$addColumnInfo(name = "waicSE",       title = gettext("SE"),                  type = "number", overtitle = ovtWaic)
  summaryTable$addColumnInfo(name = "looEst",       title = gettext("Estimate"),            type = "number", overtitle = ovtLoo)
  summaryTable$addColumnInfo(name = "looSE",        title = gettext("SE"),                  type = "number", overtitle = ovtLoo)

  jaspResults[["modelSummaryTable"]] <- summaryTable

  summaryTable[["Model"]]       <- modelNames[tableRowIsValid]
  summaryTable[["modelNumber"]] <- modelNumbers[tableRowIsValid]

  if (length(procResults) == 0) {
    summaryTable$addFootnote(message = .procModelIncompleteMsg())
    return()
  }

  bic <- sapply(procResults, lavaan::fitMeasures, fit.measures = "bic")

  summaryTable[["ppp"]] <- sapply(procResults, function(mod) mod@test[[2]]$stat)
  summaryTable[["bic"]] <- bic

  if (options[["bicWeights"]]) {
    summaryTable[["wBIC"]] <- .computeWeights(bic)
  }

  logLikResults <- lapply(procResults, .procBayesCalcModelCaseLogLik)

  looResults <- mapply(function(mod, ll) {
    rEff <- .procBayesCalcRelEff(mod, ll)
    return(loo::loo(ll, r_eff = rEff))
  }, mod = procResults, ll = logLikResults, SIMPLIFY = FALSE)

  names(looResults) <- modelNames[tableRowIsValid]

  waicResults <- lapply(logLikResults, loo::waic)

  if (length(looResults) > 1) {
    ovtDiff <- gettext("LOO Difference")

    summaryTable$addColumnInfo(name = "elpdDiff", title = gettext("ELPD"), type = "number", overtitle = ovtDiff)
    summaryTable$addColumnInfo(name = "seDiff", title = gettext("SE"), type = "number", overtitle = ovtDiff)
    summaryTable$addColumnInfo(name = "ratioDiff", title = gettext("Ratio"), type = "number", overtitle = ovtDiff)

    looCompare <- loo::loo_compare(looResults)

    elpdDiff <- looCompare[match(names(looResults), row.names(looCompare)), "elpd_diff"]
    seDiff <- looCompare[match(names(looResults), row.names(looCompare)), "se_diff"]
    ratioDiff <- abs(elpdDiff / seDiff)
    ratioDiff[!is.finite(ratioDiff)] <- NA

    summaryTable[["elpdDiff"]] <- elpdDiff
    summaryTable[["seDiff"]] <- seDiff
    summaryTable[["ratioDiff"]] <- ratioDiff
  }

  isBadWaic <- sapply(waicResults, function(mod) sum(mod$pointwise[, 2] > 0.4))
  isBadLoo <- sapply(looResults, function(mod) length(loo::pareto_k_ids(mod, threshold = 0.7)))

  if (any(isBadWaic)) {
    summaryTable$addFootnote(
      message = gettext("Warning: WAIC estimate unreliable -- at least one effective parameter estimate (p_waic) larger than 0.4. We recommend using LOO instead."),
      colNames = "waicEst",
      rowNames = names(looResults)[isBadWaic]
    )
  }

  if (any(isBadLoo)) {
    summaryTable$addFootnote(
      message = gettext("Warning: LOO estimate unreliable -- at least one observation with shape parameter (k) of the generalized Pareto distribution higher than 0.5."),
      colNames = "looEst",
      rowNames = names(looResults)[isBadLoo]
    )
  }

  summaryTable[["waicEst"]] <- sapply(waicResults, function(mod) mod$estimates["waic", "Estimate"])
  summaryTable[["waicSE"]] <- sapply(waicResults, function(mod) mod$estimates["waic", "SE"])
  summaryTable[["looEst"]] <- sapply(looResults, function(mod) mod$estimates["looic", "Estimate"])
  summaryTable[["looSE"]] <- sapply(looResults, function(mod) mod$estimates["looic", "SE"])

  summaryTable$addColumnInfo(name = "N", title = gettext("n"), type = "integer")

  summaryTable[["N"]] <- sapply(procResults, lavaan::lavInspect, what = "nobs")
}

.procBayesParameterEstimateTables <- function(container, options, modelsContainer) {
  if (is.null(modelsContainer)) return()
  
  procResults <- lapply(options[["processModels"]], function(mod) modelsContainer[[mod[["name"]]]][["fittedModel"]]$object)
  modelNames <- sapply(options[["processModels"]], function(mod) mod[["name"]])

  for (i in 1:length(procResults)) {
    if (is.null(container[[modelNames[i]]])) {
      modelContainer <- createJaspContainer(title = modelNames[i])
      modelContainer$dependOn(
        options = c("parameterLabels", "ciLevel"),
        nestedOptions = .procGetSingleModelsDependencies(as.character(i))
      )
      container[[modelNames[i]]] <- modelContainer
    } else {
      modelContainer <- container[[modelNames[i]]]
    }
    
    .procSetContainerError(modelContainer, procResults[[i]])

    if (options[["processModels"]][[i]][["pathCoefficients"]])
      .procBayesPathCoefficientsTable(modelContainer, options, procResults[[i]], i)

    if (options[["processModels"]][[i]][["mediationEffects"]])
      .procBayesPathMediationEffectsTable(modelContainer, options, procResults[[i]], i)

    if (options[["processModels"]][[i]][["totalEffects"]])
      .procBayesPathTotalEffectsTable(modelContainer, options, procResults[[i]], i)

    if (options[["processModels"]][[i]][["residualCovariances"]])
      .procBayesCovariancesTable(modelContainer, options, procResults[[i]], i)
  }
}

.procBayesCoefficientsTable <- function(tbl, options, parStats) {
  titlePosterior <- gettext("Posterior")
  titleCI <- gettextf("%s%% Credible Interval", options$ciLevel * 100)

  tbl$addColumnInfo(name = "mean",      title = gettext("Mean"),        type = "number", format = "sf:4;dp:3", overtitle = titlePosterior)
  tbl$addColumnInfo(name = "median",    title = gettext("Median"),      type = "number", format = "sf:4;dp:3", overtitle = titlePosterior)
  tbl$addColumnInfo(name = "sd",        title = gettext("SD"),          type = "number", format = "sf:4;dp:3", overtitle = titlePosterior)
  tbl$addColumnInfo(name = "ci.lower",  title = gettext("Lower"),       type = "number", format = "sf:4;dp:3", overtitle = titleCI)
  tbl$addColumnInfo(name = "ci.upper",  title = gettext("Upper"),       type = "number", format = "sf:4;dp:3", overtitle = titleCI)
  tbl$addColumnInfo(name = "rhat",      title = gettext("R-hat"),       type = "number" )
  tbl$addColumnInfo(name = "bulkEss",   title = gettext("ESS (bulk)"),  type = "integer")
  tbl$addColumnInfo(name = "tailEss",   title = gettext("ESS (tail)"),  type = "integer")

  tbl[["mean"]]     <- parStats$mean
  tbl[["median"]]   <- parStats$median
  tbl[["sd"]]       <- parStats$sd
  tbl[["ci.lower"]] <- parStats$ci.lower
  tbl[["ci.upper"]] <- parStats$ci.upper
  tbl[["rhat"]]     <- parStats$Rhat
  tbl[["bulkEss"]]  <- parStats$Bulk_ESS
  tbl[["tailEss"]]  <- parStats$Tail_ESS
}

.procBayesGetParStats <- function(stanFit, parTable, options) {
  ci <- c((1-options$ciLevel)/2, 1-(1-options$ciLevel)/2)
  ciNames <- paste0(100*ci, "%")

  includePars <- parTable$stanpnum
  
  if (all(is.na(includePars))) {
    includePars <- parTable$lhs
  }

  # Suppress console output
  invisible(capture.output(parStats <- as.data.frame(
    rstan::monitor(
      as.array(stanFit)[, , na.omit(includePars), drop = FALSE],
      probs = c(0.5, ci), # also compute median
      # This should be zero because as.array does not include warmup samples
      warmup = 0
    )
  )[, c("mean", "50%", "sd", ciNames, "Rhat", "Bulk_ESS", "Tail_ESS")]))

  names(parStats) <- c("mean", "median", "sd", "ci.lower", "ci.upper", "Rhat", "Bulk_ESS", "Tail_ESS")
  
  # Add prior dist to output
  return(cbind(parStats, prior = parTable$prior))
}

.procBayesModelFitChecks <- function(tbl, stanFit, parStats) {
  divIterations <- rstan::get_num_divergent(stanFit)
  lowBmfi       <- rstan::get_low_bfmi_chains(stanFit)
  maxTreedepth  <- rstan::get_num_max_treedepth(stanFit)
  minBulkESS    <- min(parStats[, "Bulk_ESS"])
  minTailESS    <- min(parStats[, "Tail_ESS"])

  if (any(is.infinite(parStats[, "Rhat"])))
    maxRhat     <- Inf
  else
    maxRhat     <- max(parStats[, "Rhat"])

  symbol <- gettext("Warning:")

  if (divIterations > 0) {
    tbl$addFootnote(message = .procBayesDivergentTransitionsFootnote(divIterations), symbol = symbol)
  }

  if (length(lowBmfi) > 0) {
    tbl$addFootnote(message = .procBayesBfmiFootnote(lowBmfi), symbol = symbol)
  }

  if (maxTreedepth > 0) {
    tbl$addFootnote(message = .procBayesMaxTreedepthFootnote(maxTreedepth), symbol = symbol)
  }

  essThreshold <- 100 * stanFit@sim$chains

  if (minBulkESS < essThreshold || !is.finite(minBulkESS)) {
    tbl$addFootnote(message = .procBayesLowEssFootnote(minBulkESS, essThreshold, "bulk"), symbol = symbol)
  }

  if (minTailESS < essThreshold || !is.finite(minBulkESS)) {
    tbl$addFootnote(message = .procBayesLowEssFootnote(minTailESS, essThreshold, "tail"), symbol = symbol)
  }

  rhatTreshold <- 1.05

  if (maxRhat > rhatTreshold) {
    tbl$addFootnote(message = .procBayesRhatFootnote(maxRhat, rhatTreshold), symbol = symbol)
  }
}

.procBayesPathCoefficientsTable <- function(container, options, procResults, modelIdx) {
  if (!is.null(container[["pathCoefficientsTable"]])) return()

  pathCoefTable <- createJaspTable(title = gettext("Path coefficients"))
  pathCoefTable$dependOn(
    options = c("priorDistributions"),
    nestedOptions = list(c("processModels", as.character(modelIdx), "pathCoefficients"),
                         c("processModels", as.character(modelIdx), "intercepts"))
  )
  container[["pathCoefficientsTable"]] <- pathCoefTable

  if (!.procIsValidModel(container, procResults)) return()

  parTable <- lavaan::parameterTable(procResults)
  operators <- if(options[["processModels"]][[modelIdx]][["intercepts"]]) c("~1", "~") else "~"
  parTable <- parTable[parTable$op %in% operators,]
  parTable[which(parTable$op == "~1"), "rhs"] <- "(Intercept)"
  parTable <- parTable[order(parTable$op, decreasing = TRUE),]

  parStats <- .procBayesGetParStats(as.array(procResults@external$mcmcout), parTable, options)

  pathCoefTable$addColumnInfo(name = "lhs", title = "", type = "string")
  pathCoefTable$addColumnInfo(name = "op",  title = "", type = "string")
  pathCoefTable$addColumnInfo(name = "rhs", title = "", type = "string")

  pathCoefTable[["lhs"]] <- gsub("__", ":", parTable$rhs)
  pathCoefTable[["op"]]  <- rep("\u2192", nrow(parTable))
  pathCoefTable[["rhs"]] <- gsub("__", ":", parTable$lhs)

  if (options$parameterLabels) {
    pathCoefTable$addColumnInfo(name = "label", title = gettext("Label"), type = "string")
    pathCoefTable[["label"]] <- parTable$label
  }

  .procBayesCoefficientsTable(pathCoefTable, options, parStats)
  .procBayesModelFitChecks(pathCoefTable, procResults@external$mcmcout, parStats)

  if (options$priorDistributions) {
    # Prior only for coefficients
    pathCoefTable$addColumnInfo(name = "priorDist", title = gettext("Prior"), type = "string")
    pathCoefTable[["priorDist"]] <- stringr::str_to_sentence(parStats$prior)
  }
}

.procBayesAddMedSamples <- function(stanFit, parTable) {
  rhs <- parTable$rhs[parTable$op == ":="]

  samples <- lapply(parTable$pxnames[parTable$op == "~"], function(nm) {
    return(as.array(stanFit)[, , nm])
  })

  names(samples) <- parTable$label[parTable$op == "~"]

  medEffectSamples <- sapply(rhs, function(trm) {
    return(eval(str2lang(trm), envir = samples))
  })

  return(array(medEffectSamples, dim = c(dim(as.array(stanFit))[1:2], length(rhs)), dimnames = list(NULL, NULL, parameters = parTable$label[parTable$op == ":="])))
}

.procBayesPathMediationEffectsTable <- function(container, options, procResults, modelIdx) {
  if (!is.null(container[["mediationEffectsTable"]])) return()

  medEffectsTable <- createJaspTable(title = gettext("Direct and indirect effects"))
  medEffectsTable$dependOn(
    options = c( "moderationProbes"),
    nestedOptions = list(c("processModels", as.character(modelIdx), "mediationEffects"))
  )

  container[["mediationEffectsTable"]] <- medEffectsTable

  if (!.procIsValidModel(container, procResults)) return()

  parTable <- lavaan::parameterTable(procResults)
  medEffects <- parTable[parTable$op == ":=",]

  medEffects <- .procPathMediationEffectsTableHelper(medEffectsTable, medEffects)

  parStats <- .procBayesGetParStats(.procBayesAddMedSamples(procResults@external$mcmcout, parTable), medEffects, options)

  # Add column with parameter labels
  if (options$parameterLabels) {
    medEffectsTable <- .procEffectsTablesParameterLabels(medEffectsTable, medEffects)
  }

  .procBayesCoefficientsTable(medEffectsTable, options, parStats)
  .procBayesModelFitChecks(medEffectsTable, procResults@external$mcmcout, parStats)
}

.procBayesPathTotalEffectsTable <- function(container, options, procResults, modelIdx) {
  if (!is.null(container[["totalEffectsTable"]])) return()

  totEffectsTable <- createJaspTable(title = gettext("Total effects"))
  totEffectsTable$dependOn(
    options = c( "moderationProbes"),
    nestedOptions = list(c("processModels", as.character(modelIdx), "totalEffects"))
  )

  container[["totalEffectsTable"]] <- totEffectsTable

  if (!.procIsValidModel(container, procResults)) return()

  parTable <- lavaan::parameterTable(procResults)
  medEffects <- parTable[parTable$op == ":=",]

  totEffects <- .procPathTotalEffectsTableHelper(totEffectsTable, medEffects)

  parStats <- .procBayesGetParStats(.procBayesAddMedSamples(procResults@external$mcmcout, parTable), totEffects, options)

  # Add column with parameter labels
  if (options$parameterLabels) {
    totEffectsTable <- .procEffectsTablesParameterLabels(totEffectsTable, totEffects)
  }

  .procBayesCoefficientsTable(totEffectsTable, options, parStats)
  .procBayesModelFitChecks(totEffectsTable, procResults@external$mcmcout, parStats)
}

.procBayesCovariancesTable <- function(container, options, procResults, modelIdx) {
  if (!is.null(container[["covariancesTable"]])) return()

  covTable <- createJaspTable(title = gettext("Residual covariances"))
  covTable$dependOn(
    nestedOptions = list(c("processModels", as.character(modelIdx), "residualCovariances"))
  )
  container[["covariancesTable"]] <- covTable

  if (!.procIsValidModel(container, procResults)) return()

  parTable <- lavaan::parameterTable(procResults)
  # Only used free covariances (start == est means fixed parameter)
  covEffects <- parTable[parTable$op == "~~" & parTable$start != parTable$est,]

  parStats <- .procBayesGetParStats(procResults@external$mcmcout, covEffects, options)

  covTable$addColumnInfo(name = "lhs", title = "", type = "string")
  covTable$addColumnInfo(name = "op",  title = "", type = "string")
  covTable$addColumnInfo(name = "rhs", title = "", type = "string")

  covTable[["lhs"]] <- gsub("__", ":", covEffects$rhs)
  covTable[["op"]]  <- rep("\u2194", nrow(covEffects))
  covTable[["rhs"]] <- gsub("__", ":", covEffects$lhs)

  .procBayesCoefficientsTable(covTable, options, parStats)
  .procBayesModelFitChecks(covTable, procResults@external$mcmcout, parStats)
}

.procBayesMcmcPlots <- function(container, options, modelsContainer) {

  if (is.null(container[["mcmcPlotContainer"]])) {
    plotContainer <- createJaspContainer(
      title = gettext("MCMC Plots"),
      dependencies = c("useColorPalette", "colorPalette")
    )
    container[["mcmcPlotContainer"]] <- plotContainer
  } else {
    plotContainer <- container[["mcmcPlotContainer"]]
  }

  procResults <- lapply(options[["processModels"]], function(mod) modelsContainer[[mod[["name"]]]][["fittedModel"]]$object)
  modelNames <- sapply(options[["processModels"]], function(mod) mod[["name"]])

  for (i in 1:length(procResults)) {
    if (is.null(plotContainer[[modelNames[i]]])) {
      modelContainer <- createJaspContainer(title = modelNames[i], , initCollapsed = TRUE)
      modelContainer$dependOn(
        nestedOptions = .procGetSingleModelsDependencies(as.character(i))
      )
      plotContainer[[modelNames[i]]] <- modelContainer
    } else {
      modelContainer <- plotContainer[[modelNames[i]]]
    }
    
    if (!.procIsValidModel(modelContainer, procResults[[i]])) next

    parTbl <- lavaan::parTable(procResults[[i]])
    # Only plot free parameters but not fixed
    parTbl <- parTbl[parTbl$free > 0, ]
    # Names of parameters in MCMC output
    mcmcParams <- parTbl[, "pxnames"]
    # Names of parameters to display
    dispParams <- paste(parTbl$rhs, ifelse(parTbl$op == "~", "\u2192", "\u2194"), parTbl$lhs)
    # Get MCMC samples
    mcmcArray <- as.array(procResults[[i]]@external$mcmcout)
    # Replace parameter names
    dimnames(mcmcArray)[[3]][dimnames(mcmcArray)[[3]] %in% mcmcParams] <- dispParams
    
    # Create dummy mcmcResult for JAGS functions
    mcmcResult <- list(
      samples = lapply(seq(dim(mcmcArray)[2]), function(i) mcmcArray[, i, ])
    )

    # Plotting functions from jaspJags module
    containerObj <- jaspJags:::.JAGSInitPlotsContainers(modelContainer, options, dispParams)

    if (options[["useColorPalette"]]) {
      colorpalette <- options[["colorPalette"]]
      oldColorpalette <- jaspGraphs::getGraphOption("palette")
      on.exit(jaspGraphs::setGraphOption("palette", oldColorpalette))
      jaspGraphs::setGraphOption("palette", colorpalette)
    }

    jaspJags:::.JAGSFillPlotContainers(containerObj, options, mcmcResult, dispParams)

    jaspJags:::.JAGSPlotBivariateScatter(modelContainer, options, mcmcResult, dispParams)
  }
}

# Footnotes ----

.procBayesDivergentTransitionsFootnote <- function(n) gettextf("Estimates might be biased -- %i divergent transitions after warmup.", n)

.procBayesBfmiFootnote <- function(n) gettextf("Bayesian Fraction of Missing Information (BFMI) was too low in %i chains -- the posterior distribution was not explored efficiently.", n)

.procBayesMaxTreedepthFootnote <- function(n) gettextf("The Hamiltonian Monte Carlo procedure might be inefficient -- %i transition exceeded the maximum tree depth.", n)

.procBayesLowEssFootnote <- function(minESS, essThreshold, type) gettextf("Low estimation accuracy for %s quantities -- the smallest Effective Sample Size (ESS) is %.2f < %1.0f.", type, minESS, essThreshold)

.procBayesRhatFootnote <- function(maxRhat, rhatTreshold) gettextf("Inference possibly unreliable -- MCMC chains might not have converged; the largest R-hat is %.3f > %.2f.", maxRhat, rhatTreshold)
