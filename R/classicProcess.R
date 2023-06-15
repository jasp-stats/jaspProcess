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
ClassicProcess <- function(jaspResults, dataset = NULL, options) {
  # Set title
  jaspResults$title <- gettext("Process Analysis")

  ready <- .procIsReady(options)

  if (!ready) return()

  # Init options: add variables to options to be used in the remainder of the analysis
  .procContainerModels(jaspResults, options)
  .procModelRegList(jaspResults, options)

  # read dataset
  dataset <- .procReadData(options)

  # error checking
  ready <- .procErrorHandling(dataset, options)

  .procModProbes(jaspResults, dataset, options)
  .procModelSyntax(jaspResults, options)

  # Compute (a list of) results from which tables and plots can be created
  modelsContainer <- .procComputeResults(jaspResults, dataset, options)

  pathPlotContainer <- .procContainerPathPlots(jaspResults, options)
  .procPathPlots(pathPlotContainer, options, modelsContainer)

  # Output containers, tables, and plots based on the results. These functions should not return anything!
  .procModelFitTable(jaspResults, options, modelsContainer)

  parEstContainer <- .procContainerParameterEstimates(jaspResults, options)

  .procParameterEstimateTables(parEstContainer, options, modelsContainer)

  .procPlotSyntax(jaspResults, options, modelsContainer)

  return()
}

.procGetDependencies <- function() {
  return(c('dependent', 'covariates', 'factors', "naAction", "emulation", "estimator", "standardizedEstimates"))
}

# Init functions ----
.procModelIsComplete <- function(mod) {
  if (mod[["inputType"]] == "inputVariables") {
    if (length(mod[["processRelationships"]]) == 0)
      return(FALSE)

    rowsComplete <- sapply(mod[["processRelationships"]], function(row) {
      row[["processIndependent"]] != "" && row[["processDependent"]] != "" &&
        row[["processType"]] != "" && (
          (row[["processType"]] == "directs" && row[["processVariable"]] == "") ||
          (row[["processType"]] != "directs" && row[["processVariable"]] != "")
        )
    })

    return(all(rowsComplete))
  }

  return(TRUE)
}

.procIsReady <- function(options) {
  if (options[["dependent"]] == "" || length(options[["covariates"]]) == 0)
    return(FALSE)

  if (length(options[["processModels"]]) == 0)
    return(FALSE)

  modelsComplete <- sapply(options[["processModels"]], .procModelIsComplete)

  return(all(modelsComplete))
}

.procContainerModels <- function(jaspResults, options) {
  if(!is.null(jaspResults[["modelsContainer"]])) return()

  modelsContainer <- createJaspContainer()

  for (i in 1:length(options[["processModels"]])) {
    modelOptions <- options[["processModels"]][[i]]
    modelName <- modelOptions[["name"]]
    if (is.null(modelsContainer[[modelName]])) {
      container <- createJaspContainer(title = modelName)
      modelsContainer[[modelName]] <- container
    }
  }

  jaspResults[["modelsContainer"]] <- modelsContainer
}

.procModelRegList <- function(jaspResults, options) {
  modelsContainer <- jaspResults[["modelsContainer"]]

  for (i in 1:length(options[["processModels"]])) {
    modelOptions <- options[["processModels"]][[i]]
    modelName <- modelOptions[["name"]]
  
    if (is.null(modelsContainer[[modelName]][["regList"]])) {
      regList <- .procModelRegListSingleModel(options[["processModels"]][[i]], globalDependent = options[["dependent"]])
      state <- createJaspState(object = regList)
      state$dependOn(
        optionContainsValue = list(processModels = modelOptions),
        nestedOptions = .procGetSingleModelsDependencies(as.character(i))
      )
      modelsContainer[[modelName]][["regList"]] <- state
    }
  }
}

.procModelRegListSingleModel <- function(modelOptions, globalDependent) {
  # Existing Hayes models
  # Hmodels <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
  #              21,22,28,29,58,59,60,61,62,63,64,65,66,67,68,69,70,
  #              71,72,73,75,76,80,81,82,83,84,85,86,87,88,89,90,91,92)
  processRelationships <- switch(modelOptions[["inputType"]],
    inputVariables = modelOptions[["processRelationships"]],
    # Insert function for plotting conceptual hard-coded Hayes model, in case
    # no estimation takes place yet (because of not having filled in all necessary
    # variables)
    inputModelNumber = .HardCodedModels(modelOptions[["modelNumber"]])
  )
  ## TODO: Models involving moderated moderation 3,11,12,13,18,19,20,68,69,70,71,72,73
  ## TODO: Models involving flexible amount of mediators 6,80,81

  regList <- .procProcessRelationshipsToRegList(processRelationships)

  if (modelOptions[["inputType"]] == "inputModelNumber")
    regList <- .procRegListInputModelNumber(regList, modelOptions, globalDependent)

  return(regList)
}

.procCheckRegListVars <- function(vars) {
  # Check if vector of var names contains encoded X, W, Z, or M (not Y!!)
  encoding <- .procVarEncoding()
  return(!any(c(encoding[["X"]], encoding[["W"]], encoding[["Z"]]) %in% vars) && !any(grepl(encoding[["M"]], vars)))
}

.procRegListInputModelNumber <- function(regList, modelOptions, globalDependent) {
  # reading variables specified in the menu, if any
  independent  <- modelOptions[["modelNumberIndependent"]]
  mediators    <- modelOptions[["modelNumberMediators"]]
  covariates   <- modelOptions[["modelNumberCovariates"]]
  modW         <- modelOptions[["modelNumberModeratorW"]]
  modZ         <- modelOptions[["modelNumberModeratorZ"]]
  number       <- modelOptions[["modelNumber"]]

  .replaceDummyVars <- function(vars) {
    # Get encoding
    encoding <- .procVarEncoding()

    # Replace encoded X, W, Z with user variables
    if (independent != "")
      vars[vars == encoding[["X"]]] <- independent
    if (modW != "")
      vars[vars == encoding[["W"]]] <- modW
    if (modZ != "")
      vars[vars == encoding[["Z"]]] <- modZ

    # Replace encoded M with user variables
    # Is var a mediator?
    isMed <- grepl(encoding[["M"]], vars)
    # Which mediator index?
    medIdx <- stringr::str_extract(vars[isMed], "[0-9]")
    medIdx <- as.integer(medIdx[!is.na(medIdx)])

    if (length(medIdx) > 0) {
      for (i in 1:length(medIdx)) {
        if (length(mediators) >= medIdx[i])
          vars[isMed][i] <- mediators[medIdx[i]]
      }
    }

    # If mediator has no index still replace
    if ((length(medIdx) == 0) && sum(isMed) > 0) {
      for (i in 1:length(vars[isMed])) {
        if (length(mediators) >= i)
          vars[isMed][i] <- mediators[i]
      }
    }

    # Replace encoded Y with user variable
    vars[vars == encoding[["Y"]]] <- globalDependent

    return(vars)
  }

  for (i in 1:length(regList)) {
    pathVars <- regList[[i]][["vars"]]

    # Split path interactions
    pathVarsSplit <- strsplit(pathVars, ":")

    # Replace dummy vars for each term of interactions separately
    pathVarsSplit <- lapply(pathVarsSplit, .replaceDummyVars)

    # Paste interaction terms back together
    regList[[i]][["vars"]] <- sapply(pathVarsSplit, paste, collapse = ":")
  }

  # Replace dummy variables in dependent variables
  names(regList) <- .replaceDummyVars(names(regList))

  return(regList)
}

.procModProbes <- function(jaspResults, dataset, options) {
  modelsContainer <- jaspResults[["modelsContainer"]]

  for (i in 1:length(options[["processModels"]])) {
    modelOptions <- options[["processModels"]][[i]]
    modelName <- modelOptions[["name"]]

    if (is.null(modelsContainer[[modelName]][["modProbes"]])) {
      modProbes <- .procModProbesSingleModel(modelsContainer[[modelName]][["regList"]]$object, dataset, options)
      state <- createJaspState(object = modProbes)
      state$dependOn(
        optionContainsValue = list(processModels = modelOptions),
        nestedOptions = .procGetSingleModelsDependencies(as.character(i))
      )
      modelsContainer[[modelName]][["modProbes"]] <- state
    }
  }
}

.procModVarsFromRegList <- function(regList) {
  modVars <- list()

  for (i in 1:length(regList)) {
    rowSplit <- strsplit(regList[[i]][["vars"]], ":")

    for (v in rowSplit[sapply(rowSplit, length) > 1]) {
      modVars[[v[2]]] <- c(modVars[[v[2]]], v[1])
    }
  }

  return(modVars)
}

.procModProbesSingleModel <- function(regList, dataset, options) {
  probeVals <- sapply(options[["moderationProbes"]], function(row) row[["probePercentile"]])/100

  modVars <- .procModVarsFromRegList(regList)

  modProbes <- lapply(encodeColNames(names(modVars)), function(nms) {
    quantile(dataset[[nms]], probs = probeVals)
  })

  names(modProbes) <- names(modVars)

  return(modProbes)
}

.procModelSyntax <- function(jaspResults, options) {
  modelsContainer <- jaspResults[["modelsContainer"]]

  for (i in 1:length(options[["processModels"]])) {
    modelOptions <- options[["processModels"]][[i]]
    modelName <- modelOptions[["name"]]
  
    if (is.null(modelsContainer[[modelName]][["syntax"]])) {
      syntax <- .procModelSyntaxSingleModel(modelsContainer[[modelName]], modelOptions)
      state <- createJaspState(object = syntax)
      state$dependOn(
        optionContainsValue = list(processModels = modelOptions),
        nestedOptions = .procGetSingleModelsDependencies(as.character(i))
      )
      modelsContainer[[modelName]][["syntax"]] <- state
    }
  }
}

.procModelSyntaxSingleModel <- function(container, modelOptions) {
  # Concatenate and collapse par names and var names to regression formula
  regSyntax <- paste(
    paste0(encodeColNames(names(container[["regList"]]$object))),
    sapply(container[["regList"]]$object, function(row) paste(row$parNames, encodeColNames(row$vars), sep = "*", collapse = " + ")),
    sep = " ~ "
  )

  regSyntax <- paste(
    sapply(container[["regList"]]$object, function(row) row[["comment"]]),
    regSyntax,
    sep = "",
    collapse = "\n"
  )

  modVars <- .procModVarsFromRegList(container[["regList"]]$object)

  # modProbeSyntax <- .procModEffects(container[["modProbes"]]$object)

  medEffectSyntax <- .procMedEffects(container[["regList"]]$object, modVars, container[["modProbes"]]$object)

  resCovSyntax <- .procResCov(
    container[["regList"]]$object,
    modelOptions[["independentCovariances"]],
    modelOptions[["mediatorCovariances"]]
  )

  header <- "
  # -------------------------------------------
  # Conditional process model generated by JASP
  # -------------------------------------------
  "

  return(paste(header, regSyntax, resCovSyntax, medEffectSyntax, sep = "\n"))
}

.procAddLavModVar <- function(regList, dependent, variable) {
  # Add variable to list of dep var if not already there
  if (!variable %in% regList[[dependent]][["vars"]]) {
    regList[[dependent]][["vars"]] <- c(regList[[dependent]][["vars"]], variable)
  }
  return(regList)
}

.procAddLavModParNames <- function(regList) {
  # Get names of dependent vars
  depVars <- names(regList)

  for (i in 1:length(regList)) {
    # Split interaction terms
    vSplit <- lapply(regList[[i]]$vars, function(v) {
      unlist(strsplit(v, ":"))
    })
    # Get non-mediator vars (-> cXX)
    isIndep <- sapply(vSplit, function(v) {
      return(any(!v %in% depVars))
    })
    # Get first var of every term
    vars <- sapply(vSplit, function(v) {
      return(v[1])
    })
    # Init par idx vector
    parIdx <- rep(0, length(isIndep))
    # Non-mediator vars get idx 1, .., X
    parIdx[isIndep] <- 1:sum(isIndep)
    # Mediator vars get reversed idx of mediator var in names of dependent vars
    # +1 to skip first dependent var (not a mediator)
    medIdx <- length(regList) - match(vars[!isIndep], depVars) + 1
    parIdx[!isIndep] <- medIdx
    if (regList[[i]]$dep) { # If dependent but not a mediator
      depIdx <- i
      parSym <- ifelse(isIndep, "c", "b")
      comment <- "\n# Dependent regression\n"
    } else { # If dependent and mediator
      depIdx <- length(regList) - i + 1
      parSym <- ifelse(isIndep, "a", "d")
      comment <- ifelse(depIdx == length(regList) - 1, "\n# Mediator regression\n", "")
    }
    # Concatenate par symbols with idx
    regList[[i]][["parNames"]] <- paste0(parSym, paste0(depIdx, parIdx))
    regList[[i]][["comment"]]  <- comment
  }

  return(regList)
}

.procProcessRelationshipsToRegList <- function(processRelationships) {
  regList <- list()

  for (path in processRelationships) {
    dependent <- path[["processDependent"]]
    independent <- path[["processIndependent"]]
    type <- path[["processType"]]
    processVariable <- path[["processVariable"]]

    # Init list for regression of new dependent var
    # dep = TRUE to signal this is NOT a mediator; this is used later when assigning par names
    if (!dependent %in% names(regList)) {
      regList[[dependent]] = list(vars = c(), dep = TRUE)
    }

    # Add independent var to regression of dependent var
    regList <- .procAddLavModVar(regList, dependent, independent)

    if (type != "directs") {
      # Add process var to regression of dependent var
      regList <- .procAddLavModVar(regList, dependent, processVariable)
    }

    if (type == "mediators") {
      # Init list for regression of new process var var
      if (!processVariable %in% names(regList)) {
        # dep = FALSE to signal this is a mediator; this is used later when assigning par names
        regList[[processVariable]] = list(vars = c(), dep = FALSE)
      }
      # Add independent var to regression of process var
      regList <- .procAddLavModVar(regList, processVariable, independent)
    }

    if (type == "moderators") {
      # Add interaction independent x moderator var to regress on dependent var
      interVar <- paste0(independent, ":", processVariable)
      regList <- .procAddLavModVar(regList, dependent, interVar)
    }

    if (type == "confounders") {
      # Add extra regression equation where confounder -> independent variable
      regList[[independent]] = list(vars = c(), dep = FALSE)
      regList <- .procAddLavModVar(regList, independent, processVariable)
    }
  }

  regList <- .procAddLavModParNames(regList)

  return(regList)
}

.procModEffects <- function(modVarProbes) {
  modEffects <- lapply(names(modVarProbes), function(nms) {
    labels <- paste0(nms, gsub("\\%", "", names(modVarProbes[[nms]])))
    values <- modVarProbes[[nms]]
    return(paste(labels, values, sep = " := ", collapse = "\n"))
  })
  return(paste0("\n# Moderation probes\n", paste(modEffects, collapse = "\n")))
}

.procMedEffectFromPath <- function(path, regList, modVarProbes) {
  return(lapply(2:length(path), function(i) {
    regListRow <- regList[[names(path)[i]]]
    isMedVar <- regListRow$vars == names(path)[i-1]
    medPars <- regListRow$parNames[isMedVar]
    isIntVar <- grepl(":", regListRow$vars)
    intVarsProbeNames <- NULL

    if (any(isIntVar)) {
      regVarsSplit <- strsplit(regListRow$vars[isIntVar], ":")
      intVarIsMed <- sapply(regVarsSplit, function(v) v[1] %in% regListRow$vars[isMedVar])

      if (any(intVarIsMed)) {
        modIntVars <- sapply(regVarsSplit[intVarIsMed], function(v) v[2])
        intPars <- regListRow$parNames[isIntVar][intVarIsMed]
        probeLevels <- gsub("\\%", "", names(modVarProbes[[1]]))
        intVarsProbes <- lapply(1:length(modIntVars), function(i) paste(intPars[i], format(modVarProbes[[modIntVars[i]]], digits = 3), sep = "*"))
        intVarsProbeNames <- lapply(modIntVars, function(v) paste(v, probeLevels, sep = "_"))
        medPars <- paste0("(", paste(medPars, .pasteExpandGrid(intVarsProbes, collapse = " + "), sep = " + "), ")")
        intVarsProbeNames <- .pasteExpandGrid(intVarsProbeNames, collapse = ".")
      }
    }

    return(list(medPars = medPars, intVars = intVarsProbeNames))
  }))
}

.procMedEffects <- function(regList, modVars, modVarProbes) {
  # Get dep var
  depVar <- names(regList)[sapply(regList, function(row) row$dep)]

  # Get list of paths
  pathList <- lapply(1:length(regList), function(i) sapply(regList[[i]]$vars, function(v) c(v, names(regList)[i])))

  # Convert path list to matrix
  paths <- matrix(unlist(pathList), ncol = 2, byrow = TRUE)

  # Get exogenous var
  exoVar <- paths[!paths[, 1] %in% paths[, 2], 1]

  # Create graph from paths
  graph <- igraph::graph_from_edgelist(paths)

  # Get simple paths
  medPaths <- igraph::all_simple_paths(graph, from = exoVar, to = depVar, mode = "out")

  # Get par names of simple paths
  medEffectsList <- lapply(medPaths, .procMedEffectFromPath, regList = regList, modVarProbes = modVarProbes)

  .pasteDuplicates <- function(row) {
    pars <- lapply(row, function(col) col$medPars)
    ints <- lapply(row, function(col) col$intVars)

    isDup <- duplicated(ints) | duplicated(ints, fromLast = TRUE)

    parsUnique <- append(list(as.vector(.doCallPaste(pars[isDup & !is.null(ints)], sep = "*"))), pars[!isDup])

    return(parsUnique[sapply(parsUnique, length) > 0])
  }

  medEffectsListCombined <- lapply(medEffectsList, function(row) .pasteExpandGrid(
    .pasteDuplicates(row), collapse = "*"
  ))
  medEffectNamesListCombined <- lapply(medEffectsList, function(row) .pasteExpandGrid(
    unique(Filter(Negate(is.null), lapply(row, function(col) col$intVars))), collapse = "."
  ))

  # Create coef names of mediation effects
  medEffectPathNames <- sapply(medPaths, function(path) paste(decodeColNames(names(path)), collapse = "_"))

  medEffectsCollapsed <- unlist(medEffectsListCombined)
  medEffectNamesCollapsed <- unlist(medEffectNamesListCombined)

  medEffectsCombinedLengths <- sapply(medEffectsListCombined, length)
  medEffectIsConditional <- rep(medEffectsCombinedLengths > 1, medEffectsCombinedLengths)

  medEffectPathNamesRepeated <- rep(medEffectPathNames, medEffectsCombinedLengths)

  medEffectPathNamesCollapsed <- vector("character", length(medEffectsCollapsed))
  medEffectPathNamesCollapsed[!medEffectIsConditional] <- medEffectPathNamesRepeated[!medEffectIsConditional]
  medEffectPathNamesCollapsed[medEffectIsConditional] <- .pasteDot(medEffectPathNamesRepeated[medEffectIsConditional], medEffectNamesCollapsed)

  # Concatenate to mediation effects by multiplying par names of paths
  medEffectsSyntax <- paste(
    medEffectPathNamesCollapsed,
    medEffectsCollapsed,
    sep = " := ",
    collapse = "\n"
  )

  # Get total effect of X on Y
  if (length(medEffectsListCombined) > 1) 
    totEffect <- paste(medEffectsListCombined[[1]], .pasteExpandGrid(.doCallPaste(medEffectsListCombined[-1], sep = " + "), collapse = " + "), sep = " + ")
  else
    totEffect <- medEffectsListCombined[[1]]

  dirEffectIsConditional <- medEffectsCombinedLengths[1] > 1
  indEffectIsConditional <- any(medEffectsCombinedLengths[-1] > 1)

  indEffectNames <- .pasteExpandGrid(.doCallPaste(medEffectNamesListCombined[-1], sep = "."), collapse = ".")

  totEffectLabels <- list()

  if (dirEffectIsConditional) totEffectLabels <- append(totEffectLabels, list(medEffectNamesListCombined[[1]]))
  if (indEffectIsConditional) totEffectLabels <- append(totEffectLabels, list(indEffectNames))
  
  totEffectNames <- .doCallPaste(unique(totEffectLabels), sep = ".")

  # Get total indirect effect of X on Y
  totIndEffect <- .pasteExpandGrid(.doCallPaste(medEffectsListCombined[-1], sep = " + "), collapse = " + ")

  # Only select total effect if there are no indirect effects
  totNames <- if(length(medEffectPathNames) == 1) rep("tot", length(totEffect)) else rep(c("tot", "totInd"), c(length(totEffect), length(totIndEffect)))
  totLabels <- vector("character", length(totNames))

  if(length(medEffectPathNames) == 1) {
    totEffects <- totEffect
    totNames <- rep("tot", length(totEffect))
    totLabels <- .pasteDot(totNames, totEffectNames)
  } else {
    totEffects <- c(totEffect, totIndEffect)
    if (indEffectIsConditional) {
      totLabels <- .pasteDot(
        rep(c("tot", "totInd"), c(length(totEffect), length(totIndEffect))),
        c(totEffectNames, indEffectNames)
      )
    } else {
      totLabels <- c(.pasteDot("tot", totEffectNames), "totInd")
    }
  }

  totalEffectsSyntax <- paste(
    totLabels,
    totEffects,
    sep = " := ",
    collapse = "\n"
  )

  return(paste(
    "\n# Mediation effects",
    medEffectsSyntax,
    "\n# Total effects",
    totalEffectsSyntax,
    sep = "\n"
  ))
}

.procResCov <- function(regList, includeExo, includeMed) {
  resCovList <- list()

  exoVars <- unique(unlist(lapply(regList, function(row) row$vars[!row$vars %in% names(regList)])))
  intIdx  <- grep(":", exoVars)
  
  if (length(exoVars) > 1 && includeExo) {
    exoIdxMat <- which(upper.tri(diag(length(exoVars))), arr.ind = TRUE)
    exoIdxMat <- exoIdxMat[!exoIdxMat[, 1] %in% intIdx & !exoIdxMat[, 2] %in% intIdx, , drop = FALSE]
    
    for (i in 1:nrow(exoIdxMat)) {
      if (!exoVars[exoIdxMat[i, 2]] %in% regList[[exoVars[exoIdxMat[i, 1]]]][["vars"]]) {
        resCovList[[exoVars[exoIdxMat[i, 1]]]] <- c(resCovList[[exoVars[exoIdxMat[i, 1]]]], exoVars[exoIdxMat[i, 2]])
      }
    }
  }

  medVars <- names(regList)[!sapply(regList, function(row) row$dep)]

  if (length(medVars) > 1 && includeMed) {
    medIdxMat <- which(upper.tri(diag(length(medVars))), arr.ind = TRUE)

    for (i in 1:nrow(medIdxMat)) {
      if (!medVars[medIdxMat[i, 2]] %in% regList[[medVars[medIdxMat[i, 1]]]][["vars"]]) {
        resCovList[[medVars[medIdxMat[i, 1]]]] <- c(resCovList[[medVars[medIdxMat[i, 1]]]], medVars[medIdxMat[i, 2]])
      }
    }
  }

  return(paste0(
    "\n# Residual covariances\n",
    paste(
      encodeColNames(names(resCovList)),
      sapply(resCovList, function(row) paste(encodeColNames(row), collapse = " + ")),
      sep = " ~~ ",
      collapse = "\n"
    )
  ))
}

.procReadData <- function(options) {
  # Read in selected variables from dataset
  vars <- lapply(c('dependent', 'covariates', 'factors'), function(x) options[[x]])
  dataset <- .readDataSetToEnd(columns = unlist(vars))
  return(dataset)
}

.procErrorHandling <- function(dataset, options) {
  # See error handling
  vars <- lapply(.procGetDependencies(), function(x) options[[x]])
  .hasErrors(dataset, "run", type = c('observations', 'variance', 'infinity'),
             all.target = vars,
             observations.amount = '< 2',
             exitAnalysisIfErrors = TRUE)

  return(TRUE)
}

# Results functions ----
.procCheckFitModel <- function(regList) {
  return(all(sapply(regList, function(row) {
    varsSplit <- strsplit(row$vars, ":")
    return(all(sapply(varsSplit, .procCheckRegListVars)))
  })))
}

.procResultsFitModel <- function(container, dataset, options) {
  # Should model be fitted?
  doFit <- .procCheckFitModel(container[["regList"]]$object)
  
  if (!doFit)
    dataset <- NULL

  fittedModel <- try(lavaan::sem(
    model           = container[["syntax"]]$object,
    data            = dataset,
    se              = ifelse(options$errorCalculationMethod == "bootstrap", "standard", options$errorCalculationMethod),
    mimic           = options$emulation,
    estimator       = options$estimator,
    std.ov          = options$standardizedEstimate,
    missing         = options$naAction,
    do.fit          = doFit
  ))
  
  if (inherits(fittedModel, "try-error")) {
    errmsg <- gettextf("Estimation failed\nMessage:\n%s", attr(fittedModel, "condition")$message)
    return(jaspSem:::.decodeVarsInMessage(names(dataset), errmsg))
  }

  if (options$errorCalculationMethod == "bootstrap") {
    medResult <- jaspSem:::lavBootstrap(fittedModel, options$bootstrapSamples) # FIXME
  }

  return(fittedModel)
}

.procGetSingleModelsDependencies <- function(modelIdx) {
  return(list(
    c("processModels", modelIdx, "inputType"),
    c("processModels", modelIdx, "processRelationships"),
    c("processModels", modelIdx, "modelNumber"),
    c("processModels", modelIdx, "modelNumberIndependent"),
    c("processModels", modelIdx, "modelNumberMediators"),
    c("processModels", modelIdx, "modelNumberCovariates"),
    c("processModels", modelIdx, "modelNumberModeratorW"),
    c("processModels", modelIdx, "modelNumberModeratorZ"),
    c("processModels", modelIdx, "independentCovariances"),
    c("processModels", modelIdx, "mediatorCovariances")
  ))
}

.procComputeResults <- function(jaspResults, dataset, options) {
  modelsContainer <- jaspResults[["modelsContainer"]]
  nModels <- length(options[["processModels"]])

  for (i in 1:nModels) {
    modelOptions <- options[["processModels"]][[i]]
    modelName <- modelOptions[["name"]]

    if (is.null(modelsContainer[[modelName]][["fittedModel"]])) {
      fittedModel <- .procResultsFitModel(
        modelsContainer[[modelName]],
        dataset,
        options
      )
      state <- createJaspState(object = fittedModel)
      state$dependOn(
        optionContainsValue = list(processModels = modelOptions),
        nestedOptions = .procGetSingleModelsDependencies(as.character(i))
      )
      modelsContainer[[modelName]][["fittedModel"]] <- state
    }
  }

  return(modelsContainer)
}

# Output functions ----
.procFilterFittedModels <- function(procResults) {
  isFitted <- sapply(procResults, function(mod) {
    if (!is.character(mod))
      return(mod@Options[["do.fit"]])
    return(TRUE)
  })

  return(procResults[isFitted])
}

.procModelFitTable <- function(jaspResults, options, modelsContainer) {
  if (!is.null(jaspResults[["modelFitTable"]])) return()

  procResults <- lapply(options[["processModels"]], function(mod) modelsContainer[[mod[["name"]]]][["fittedModel"]]$object)
  procResults <- .procFilterFittedModels(procResults)

  if (length(procResults) == 0) return()

  fitTable <- createJaspTable(title = gettext("Model fit"))
  fitTable$dependOn(c(.procGetDependencies(), "processModels"))
  fitTable$position <- 0

  modelNames <- sapply(options[["processModels"]], function(mod) mod[["name"]])
  isInvalid <- sapply(procResults, is.character)

  if (any(isInvalid)) {
    errmsg <- gettextf("Model fit could not be assessed because one or more models were not estimated: %s", modelNames[isInvalid])
    fitTable$setError(errmsg)
    return()
  }

  fitTable$addColumnInfo(name = "Model",    title = "",                            type = "string" )
  fitTable$addColumnInfo(name = "AIC",      title = gettext("AIC"),                type = "number" )
  fitTable$addColumnInfo(name = "BIC",      title = gettext("BIC"),                type = "number" )
  fitTable$addColumnInfo(name = "N",        title = gettext("n"),                  type = "integer")
  fitTable$addColumnInfo(name = "Chisq",    title = gettext("&#967;&sup2;"),       type = "number" ,
                       overtitle = gettext("Baseline test"))
  fitTable$addColumnInfo(name = "Df",       title = gettext("df"),                 type = "integer",
                       overtitle = gettext("Baseline test"))
  fitTable$addColumnInfo(name = "PrChisq",  title = gettext("p"),                  type = "pvalue",
                       overtitle = gettext("Baseline test"))
  fitTable$addColumnInfo(name = "dchisq",   title = gettext("&#916;&#967;&sup2;"), type = "number" ,
                       overtitle = gettext("Difference test"))
  fitTable$addColumnInfo(name = "ddf",      title = gettext("&#916;df"),           type = "integer",
                       overtitle = gettext("Difference test"))
  fitTable$addColumnInfo(name = "dPrChisq", title = gettext("p"),                  type = "pvalue" ,
                       overtitle = gettext("Difference test"))

  jaspResults[["modelFitTable"]] <- fitTable

  if (any(isInvalid)) {
    errmsg <- gettextf("Model fit could not be assessed because one or more models were not estimated: %s", names(procResults)[isInvalid])
    fitTable$setError(errmsg)
    return()
  }

  if (length(procResults) == 1) {
    lrt <- jaspSem:::.withWarnings(lavaan::lavTestLRT(procResults[[1]])[-1, ])
    rownames(lrt$value) <- names(procResults)
    Ns <- lavaan::lavInspect(procResults[[1]], "ntotal")
  } else {
    Ns <- vapply(procResults, lavaan::lavInspect, 0, what = "ntotal")
    lrtArgs <- procResults
    names(lrtArgs) <- "object" # (the first result is object, the others ...)
    lrtArgs[["model.names"]] <- names(procResults)
    lrt <- try(jaspSem:::.withWarnings(do.call(lavaan::lavTestLRT, lrtArgs)))

    if (inherits(lrt, "try-error")) {
      errmsg <- gettext("Model comparison failed likely because one or more models did not converge")
      fitTable$setError(errmsg)
      return()
    }

    lrt$value[1,5:7] <- NA
  }

  fitTable[["Model"]]    <- rownames(lrt$value)
  fitTable[["AIC"]]      <- lrt$value[["AIC"]]
  fitTable[["BIC"]]      <- lrt$value[["BIC"]]
  fitTable[["N"]]        <- Ns
  fitTable[["Chisq"]]    <- lrt$value[["Chisq"]]
  fitTable[["Df"]]       <- lrt$value[["Df"]]
  fitTable[["PrChisq"]]  <- pchisq(q = lrt$value[["Chisq"]], df = lrt$value[["Df"]], lower.tail = FALSE)
  fitTable[["dchisq"]]   <- lrt$value[["Chisq diff"]]
  fitTable[["ddf"]]      <- lrt$value[["Df diff"]]
  fitTable[["dPrChisq"]] <- lrt$value[["Pr(>Chisq)"]]

  # add warning footnote
  if (!is.null(lrt$warnings)) {
    fitTable$addFootnote(paste0(stringr::str_to_sentence(gsub("lavaan WARNING: ", "", lrt$warnings[[1]]$message)), "."))
  }

  if(options$naAction == "listwise"){
    nrm <- lavaan::lavInspect(procResults[[1]], "norig") - lavaan::lavInspect(procResults[[1]], "ntotal")
    if (nrm != 0) {
      missingFootnote <- gettextf("A total of %g cases were removed due to missing values. You can avoid this by choosing 'FIML' under 'Missing Data Handling' in the Estimation options.",
                                  nrm)
      fitTable$addFootnote(message = missingFootnote)
    }
  }

  # add test statistic correction footnote
  test <- lavaan::lavInspect(procResults[[1]], "options")[["test"]]
  if(length(test) > 1)
    test <- test[[2]]

  if (test != "standard") {
    LUT <- tibble::tribble(
      ~option,              ~name,
      "Satorra.Bentler",    gettext("Satorra-Bentler scaled test-statistic"),
      "Yuan.Bentler",       gettext("Yuan-Bentler scaled test-statistic"),
      "Yuan.Bentler.Mplus", gettext("Yuan-Bentler (Mplus) scaled test-statistic"),
      "mean.var.adjusted",  gettext("mean and variance adjusted test-statistic"),
      "Satterthwaite",      gettext("mean and variance adjusted test-statistic"),
      "scaled.shifted",     gettext("scaled and shifted test-statistic"),
      "Bollen.Stine",       gettext("bootstrap (Bollen-Stine) probability value"),
      "bootstrap",          gettext("bootstrap (Bollen-Stine) probability value"),
      "boot",               gettext("bootstrap (Bollen-Stine) probability value")
    )
    testname <- LUT[test == tolower(LUT$option), "name"][[1]]
    ftext <- gettextf("Model tests based on %s.", testname)
    fitTable$addFootnote(message = ftext)
  }

  if (options$estimator %in% c("dwls", "gls", "wls", "uls")) {
    fitTable$addFootnote(message = gettext("The AIC, BIC and additional information criteria are only available with ML-type estimators"))
  }
}

.procContainerParameterEstimates <- function(jaspResults, options) {
  if (!is.null(jaspResults[["parEstContainer"]])) {
    parEstContainer <- jaspResults[["parEstContainer"]]
  } else {
    parEstContainer <- createJaspContainer("Parameter estimates")
    parEstContainer$dependOn(.procGetDependencies())

    jaspResults[["parEstContainer"]] <- parEstContainer
  }

  return(parEstContainer)
}

.procContainerPathPlots <- function(jaspResults, options) {
  if (!is.null(jaspResults[["pathPlotContainer"]])) {
    pathPlotContainer <- jaspResults[["pathPlotContainer"]]
  } else {
    pathPlotContainer <- createJaspContainer("Path plots")
    pathPlotContainer$dependOn(.procGetDependencies())

    jaspResults[["pathPlotContainer"]] <- pathPlotContainer
  }

  return(pathPlotContainer)
}

.procIsValidModel <- function(container, procResult) {
  if (is.character(procResult)) {
    container$setError(procResult)
    return(FALSE)
  }
  return(TRUE)
}

.procParameterEstimateTables <- function(container, options, modelsContainer) {
  procResults <- lapply(options[["processModels"]], function(mod) modelsContainer[[mod[["name"]]]][["fittedModel"]]$object)
  modelNames <- sapply(options[["processModels"]], function(mod) mod[["name"]])

  for (i in 1:length(procResults)) {
    if (is.null(container[[modelNames[i]]])) {
      modelContainer <- createJaspContainer(title = modelNames[i], , initCollapsed = TRUE)
      modelContainer$dependOn(
        nestedOptions = .procGetSingleModelsDependencies(as.character(i))
      )
      container[[modelNames[i]]] <- modelContainer
    } else {
      modelContainer <- container[[modelNames[i]]]
    }

    if (is.character(procResults[[i]])) {
      modelContainer$setError(procResults[[i]])
      return()
    }

    if (options[["processModels"]][[i]][["pathCoefficients"]])
      .procPathCoefficientsTable(modelContainer, options, procResults[[i]], i)

    if (options[["processModels"]][[i]][["mediationEffects"]])
      .procPathMediationEffectsTable(modelContainer, options, procResults[[i]], i)

    if (options[["processModels"]][[i]][["totalEffects"]])
      .procPathTotalEffectsTable(modelContainer, options, procResults[[i]], i)

    if (options[["processModels"]][[i]][["residualCovariances"]])
      .procCovariancesTable(modelContainer, options, procResults[[i]], i)
  }
}

.procPathPlots <- function(container, options, modelsContainer) {
  for (i in 1:length(options[["processModels"]])) {
    modelOptions <- options[["processModels"]][[i]]
    modelName <- modelOptions[["name"]]
    if (is.null(container[[modelName]])) {
      pathPlotsContainer <- createJaspContainer(title = modelName)
      pathPlotsContainer$dependOn(
        nestedOptions = .procGetSingleModelsDependencies(as.character(i))
      )
      container[[modelName]] <- pathPlotsContainer
    } else {
      pathPlotsContainer <- container[[modelName]]
    }

    valid <- .procIsValidModel(pathPlotsContainer, modelsContainer[[modelName]][["fittedModel"]]$object)

    if (valid) {
      if (options[["processModels"]][[i]][["conceptualPathPlot"]]) {
        .procConceptPathPlot(pathPlotsContainer, options, modelsContainer[[modelName]][["fittedModel"]]$object, i)
      }

      if (options[["processModels"]][[i]][["statisticalPathPlot"]])
        .procStatPathPlot(pathPlotsContainer, options, modelsContainer[[modelName]][["fittedModel"]]$object, i)
    }
  }
}

.procCoefficientsTable <- function(tbl, options, coefs) {
  tbl$addColumnInfo(name = "est",      title = gettext("Estimate"),   type = "number", format = "sf:4;dp:3")
  tbl$addColumnInfo(name = "se",       title = gettext("Std. Error"), type = "number", format = "sf:4;dp:3")
  tbl$addColumnInfo(name = "z",        title = gettext("z-value"),    type = "number", format = "sf:4;dp:3")
  tbl$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "number", format = "dp:3;p:.001")
  tbl$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number", format = "sf:4;dp:3",
                    overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
  tbl$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number", format = "sf:4;dp:3",
                    overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))

  tbl[["est"]]      <- coefs$est
  tbl[["se"]]       <- coefs$se
  tbl[["z"]]        <- coefs$z
  tbl[["pvalue"]]   <- coefs$pvalue
  tbl[["ci.lower"]] <- coefs$ci.lower
  tbl[["ci.upper"]] <- coefs$ci.upper
}

.procGetBootstrapCiType <- function(options) {
  return(switch(options[["bootstrapCiType"]],
    percentileBiasCorrected = "bca.simple",
    percentile = "perc",
    "norm"
  ))
}

.procPathCoefficientsTable <- function(container, options, procResults, modelIdx) {
  if (!is.null(container[["pathCoefficientsTable"]]) || !procResults@Options[["do.fit"]]) return()

  pathCoefTable <- createJaspTable(title = gettext("Path coefficients"))
  pathCoefTable$dependOn(
    options = "parameterLabels",
    nestedOptions = list(c("processModels", as.character(modelIdx), "pathCoefficients"))
  )
  container[["pathCoefficientsTable"]] <- pathCoefTable

  if (container$getError()) return()

  bootstrapCiType <- .procGetBootstrapCiType(options)

  pathCoefs <- lavaan::parameterEstimates(procResults, boot.ci.type = bootstrapCiType,
                                          level = options$ciLevel)

  if (!procResults@Fit@converged) {
    pathCoefTable$addFootnote(gettext("Model did not converge."))
  }

  pathCoefs <- pathCoefs[pathCoefs$op == "~",]

  pathCoefTable$addColumnInfo(name = "lhs", title = "", type = "string")
  pathCoefTable$addColumnInfo(name = "op",  title = "", type = "string")
  pathCoefTable$addColumnInfo(name = "rhs", title = "", type = "string")

  pathCoefTable[["lhs"]]   <- pathCoefs$rhs
  pathCoefTable[["op"]]    <- rep("\u2192", nrow(pathCoefs))
  pathCoefTable[["rhs"]]   <- pathCoefs$lhs

  if (options$parameterLabels) {
    pathCoefTable$addColumnInfo(name = "label", title = gettext("Label"), type = "string")
    pathCoefTable[["label"]] <- pathCoefs$label
  }

  .procCoefficientsTable(pathCoefTable, options, pathCoefs)
}

.procEffectsTablesGetConditionalLabels <- function(paths, mods) {
  modProbes <- list()

  for (path in paths) {
    pathMods <- sapply(path[-1], function(row) row[1])

    for (condEff in path[-1]) {
      modProbes[[condEff[1]]] <- c(modProbes[[condEff[1]]], paste0(condEff[2], "th"))

      for (condEff in mods[!mods %in% pathMods]) {
        modProbes[[condEff]] <- c(modProbes[[condEff]], "")
      }
    }
  }

  return(modProbes)
}

.procPathMediationEffectsTable <- function(container, options, procResults, modelIdx) {
  if (!is.null(container[["mediationEffectsTable"]]) || !procResults@Options[["do.fit"]]) return()

  medEffectsTable <- createJaspTable(title = gettext("Mediation effects"))
  medEffectsTable$dependOn(
    options = c("parameterLabels", "moderationProbes"),
    nestedOptions = list(c("processModels", as.character(modelIdx), "mediationEffects"))
  )

  container[["mediationEffectsTable"]] <- medEffectsTable

  if (container$getError()) return()

  pathCoefs <- lavaan::parameterEstimates(procResults)

  if (!procResults@Fit@converged) {
    medEffectsTable$addFootnote(gettext("Model did not converge."))
  }

  medEffects <- pathCoefs[pathCoefs$op == ":=",]

  labelSplit <- lapply(strsplit(medEffects$lhs, "\\."), strsplit, split = "_")

  # Only use label splits of length > 1 to omit total effects
  labelSplit <- labelSplit[sapply(labelSplit, function(path) length(path[[1]])) > 1]

  # Get paths from label of mediation effect
  medPaths <- lapply(labelSplit, function(path) path[[1]])
  
  # Get path lengths
  medPathLengths <- sapply(medPaths, length)

  # Sort paths to incresaing length
  medLengthSortIdx <- sort(medPathLengths, index.return = TRUE)$ix
  medEffects <- medEffects[medLengthSortIdx, ]

  # Add a column for each step of longest path
  for (i in 1:max(medPathLengths)) {
    # If path has step add var name otherwise empty
    medEffect <- sapply(medPaths[medLengthSortIdx], function(path) ifelse(length(path) >= i, path[i], ""))

    # Add operator columns
    if (i > 1) {
      # Add operator for non-empty path steps otherwise empty
      medOp <- ifelse(medEffect == "", "", "\u2192")
      medEffectsTable$addColumnInfo(name = paste0("op_", i), title = "", type = "string")
      medEffectsTable[[paste0("op_", i)]] <- medOp
    }

    medEffectsTable$addColumnInfo(name = paste0("lhs_", i), title = "", type = "string")
    medEffectsTable[[paste0("lhs_", i)]] <- medEffect
  }

  medEffectIsConditional <- sapply(labelSplit, function(path) length(path) > 1)

  uniqueMods <- unique(unlist(lapply(labelSplit[medEffectIsConditional], function(path) lapply(path[-1], function(row) row[1]))))

  modProbes <- .procEffectsTablesGetConditionalLabels(labelSplit[medEffectIsConditional], uniqueMods)

  for (mod in uniqueMods) {
    medEffectsTable$addColumnInfo(name = mod, title = encodeColNames(mod), type = "string", combine = FALSE) # combine = F because empty cells indicate no moderation
    modLabels <- vector("character", length(medEffectIsConditional))
    modLabels[medEffectIsConditional] <- modProbes[[mod]]
    medEffectsTable[[mod]] <- modLabels
  }

  # Add column with parameter labels
  if (options$parameterLabels) {
    medEffectsTable <- .procEffectsTablesParameterLabels(medEffectsTable, medEffects)
  }

  .procCoefficientsTable(medEffectsTable, options, medEffects)
}

.procPathTotalEffectsTable <- function(container, options, procResults, modelIdx) {
  if (!is.null(container[["totalEffectsTable"]]) || !procResults@Options[["do.fit"]]) return()

  totEffectsTable <- createJaspTable(title = gettext("Total effects"))
  totEffectsTable$dependOn(
    options = c("parameterLabels", "moderationProbes"),
    nestedOptions = list(c("processModels", as.character(modelIdx), "totalEffects"))
  )

  container[["totalEffectsTable"]] <- totEffectsTable

  if (container$getError()) return()

  pathCoefs <- lavaan::parameterEstimates(procResults)

  if (!procResults@Fit@converged) {
    totEffectsTable$addFootnote(gettext("Model did not converge."))
  }

  medEffects <- pathCoefs[pathCoefs$op == ":=", ]

  labelSplit <- lapply(strsplit(medEffects$lhs, "\\."), strsplit, split = "_")

  # Only use label splits of length > 1 to omit total effects
  isTotEffect <- sapply(labelSplit, function(path) length(path[[1]])) == 1
  labelSplit <- labelSplit[isTotEffect]

  # Get paths from label of mediation effect
  totEffectLabels <- sapply(labelSplit, function(path) path[[1]])
  totEffects <- medEffects[isTotEffect, ]

  totEffectsTable$addColumnInfo(name = "lhs", title = "", type = "string", combine = TRUE)
  totEffectsTable[["lhs"]] <- ifelse(totEffectLabels == "tot", gettext("Total"), gettext("Total indirect"))

  uniqueMods <- unique(unlist(lapply(labelSplit, function(path) lapply(path[-1], function(row) row[1]))))

  modProbes <- .procEffectsTablesGetConditionalLabels(labelSplit, uniqueMods)

  for (mod in uniqueMods) {
    totEffectsTable$addColumnInfo(name = mod, title = encodeColNames(mod), type = "string", combine = FALSE)
    totEffectsTable[[mod]] <- modProbes[[mod]]
  }

  # Add column with parameter labels
  if (options$parameterLabels) {
    totEffectsTable <- .procEffectsTablesParameterLabels(totEffectsTable, totEffects)
  }

  .procCoefficientsTable(totEffectsTable, options, totEffects)
}

.procEffectsTablesParameterLabels <- function(jaspTable, effects) {
  jaspTable$addColumnInfo(name = "label", title = gettext("Label"), type = "string")
  jaspTable[["label"]] <- gsub("-", "\u2212", gsub("\\+", " \uFF0B ", gsub("\\*", " \u273B ", effects$rhs)))
  return(jaspTable)
}

.procCovariancesTable <- function(container, options, procResults, modelIdx) {
  if (!is.null(container[["covariancesTable"]])) return()

  pathCoefTable <- createJaspTable(title = gettext("Residual covariances"))
  pathCoefTable$dependOn(
    nestedOptions = list(c("processModels", as.character(modelIdx), "residualCovariances"))
  )
  container[["covariancesTable"]] <- pathCoefTable

  if (container$getError()) return()

  bootstrapCiType <- .procGetBootstrapCiType(options)

  pathCoefs <- lavaan::parameterEstimates(procResults, boot.ci.type = bootstrapCiType,
                                          level = options$ciLevel)

  if (!procResults@Fit@converged) {
    pathCoefTable$addFootnote(gettext("Model did not converge."))
  }

  pathCoefs <- pathCoefs[pathCoefs$op == "~~" & !is.na(pathCoefs$z),]

  pathCoefTable$addColumnInfo(name = "lhs", title = "", type = "string")
  pathCoefTable$addColumnInfo(name = "op",  title = "", type = "string")
  pathCoefTable$addColumnInfo(name = "rhs", title = "", type = "string")

  pathCoefTable[["lhs"]]   <- pathCoefs$rhs
  pathCoefTable[["op"]]    <- rep("\u2194", nrow(pathCoefs))
  pathCoefTable[["rhs"]]   <- pathCoefs$lhs

  .procCoefficientsTable(pathCoefTable, options, pathCoefs)
}

.procConceptPathPlot <- function(container, options, procResults, modelIdx) {
  if (!is.null(container[["conceptPathPlot"]])) return()

  procPathPlot <- createJaspPlot(title = gettext("Conceptual path plot"), height = 320, width = 480)
  procPathPlot$dependOn(nestedOptions = list(c("processModels", as.character(modelIdx), "conceptualPathPlot")))
  container[["conceptPathPlot"]] <- procPathPlot

  if (container$getError()) return()

  procPathPlot$plotObject <- .procLavToGraph(procResults, type = "conceptual", estimates = FALSE, options)
}

.procStatPathPlot <- function(container, options, procResults, modelIdx) {
  if (!is.null(container[["statPathPlot"]]) || !procResults@Options[["do.fit"]]) return()

  procPathPlot <- createJaspPlot(title = gettext("Statistical path plot"), height = 320, width = 480)
  procPathPlot$dependOn(
    options = "statisticalPathPlotsParameterEstimates",
    nestedOptions = list(c("processModels", as.character(modelIdx), "statisticalPathPlot"))
  )
  container[["statPathPlot"]] <- procPathPlot

  if (container$getError()) return()

  procPathPlot$plotObject <- .procLavToGraph(procResults, type = "statistical", estimates = options[["statisticalPathPlotsParameterEstimates"]], options)
}

.procMainGraphLayoutPosHelper <- function(nNodes) {
  # This function positions nodes alternatingly above and below zero with increasing distance
  # Positions a single node a zero
  if (nNodes == 1) return(0)

  if (nNodes %% 2 == 0) {
    nNodesHalf <- nNodes/2
    pos <- -nNodesHalf:nNodesHalf
  } else {
    nNodesHalf <- floor(nNodes/2)
    pos <- -nNodesHalf:(nNodesHalf+1)
  }
  pos <- pos[pos != 0]
  return(rev(pos))
}

.procMainGraphLayoutMedPosHelper <- function(medPaths) {
  nMedPaths <- length(medPaths)

  medPos <- list()
  # Calc y pos of mediator nodes
  medPosY <- .procMainGraphLayoutPosHelper(nMedPaths)

  if (nMedPaths > 0) {
    # Only set pos for nodes that have no pos yet
    for (i in 1:nMedPaths) {
      meds <- medPaths[[i]]
      meds <- meds[!meds %in% names(medPos)]
      nMeds <- length(meds)
      if (nMeds > 0) {
        for (j in 1:nMeds) {
          medPos[[meds[j]]] <- c(j, medPosY[i])
        }
      }
    }
  }

  return(t(sapply(medPos, function(x) x)))
}

.procMainGraphLayout <- function(mainPaths, depVar) {
  # Create graph from path matrix
  graph <- igraph::graph_from_edgelist(mainPaths)
  nodeNames <- igraph::get.vertex.attribute(graph, "name")

  # Get indices of exogeneous nodes (no incoming paths)
  exoIdx <- which(!nodeNames %in% mainPaths[, 2])
  depIdx <- which(nodeNames == depVar)

  # Get all simple paths (each node only visited once) from exo nodes to dep var node
  medPaths <- igraph::all_simple_paths(graph, from = nodeNames[exoIdx][1], to = nodeNames[depIdx], mode = "out")
  medPathLengths <- sapply(medPaths, length)
  # Exclude X and Y from paths
  medPaths <- lapply(medPaths, function(path) names(path)[names(path) %in% nodeNames[-c(exoIdx, depIdx)]])
  # Sort paths according to length (longest at top)
  medPaths <- medPaths[sort(medPathLengths, decreasing = TRUE, index.return = TRUE)$ix]

  # Calc pos of exo nodes
  exoPos <- matrix(c(rep(0, length(exoIdx)), .procMainGraphLayoutPosHelper(length(exoIdx))), ncol = 2)
  # Set y pos of first exo node to 0
  exoPos[1, 2] <- 0
  depPos <- c(max(medPathLengths)-1, 0)

  # Calc pos of mediator nodes
  medPos <- .procMainGraphLayoutMedPosHelper(medPaths)

  # Combine pos
  if (length(medPos) > 0) {
    layout <- rbind(exoPos, depPos, medPos)
    rownames(layout) <- c(nodeNames[c(exoIdx, depIdx)], rownames(medPos))
  } else {
    layout <- rbind(exoPos, depPos)
    rownames(layout) <- nodeNames[c(exoIdx, depIdx)]
  }
  return(layout)
}

.procLavToGraph <- function(procResults, type, estimates, options) {
  # Get table with SEM pars from lavaan model
  parTbl <- lavaan::parameterTable(procResults)

  # Create path matrix where first col is "from" and second col is "to", third col is estimate
  labelField <- ifelse(estimates, "est", "label")
  paths <- matrix(c(decodeColNames(parTbl$rhs), decodeColNames(parTbl$lhs), parTbl[[labelField]])[parTbl$op == "~"], ncol = 3)

  # Check if "from" contains interaction term
  isIntPath <- grepl(":", paths[, 1])

  # Split interaction terms
  intPathsSplit <- strsplit(paths[isIntPath, 1], ":")

  # Get moderator vars from interaction terms
  mods <- sapply(intPathsSplit, function(path) path[2])

  # Get independent vars from interaction terms
  indeps <- sapply(intPathsSplit, function(path) path[1])

  # Create matrix with moderator paths
  if (type == "conceptual") {
    # Adds paths from moderators to helper nodes "iX" which will be invisible
    modPaths <- matrix(c(mods, paste0("i", 1:length(mods)), ""), ncol = 3)

    # Add hidden path for single moderator (fixes qgraph bug)
    if (nrow(modPaths) == 1 && nrow(paths) == 3) {
      modPaths <- rbind(modPaths, paths[paths[, 1] %in% mods, , drop = FALSE])
    }
  } else {
    # Paths from moderator and interaction term to dep var node
    modPaths <- paths[isIntPath | paths[, 1] %in% mods, , drop = FALSE]
  }
  # Filter out non-moderation paths -> main paths
  mainPaths <- paths[!isIntPath & !paths[, 1] %in% mods[!mods %in% paths[, 2]], , drop = FALSE]

  # Get layout of main paths: matrix with x,y coordinates for each node
  layout <- .procMainGraphLayout(mainPaths[, 1:2, drop = FALSE], decodeColNames(options[["dependent"]]))

  # Node names are in rownames
  nodeNames <- rownames(layout)

  # Combine main paths and moderator paths
  if (sum(isIntPath) > 0) mainPaths <- rbind(mainPaths, modPaths)

  # Remove duplicate paths
  mainPaths <- mainPaths[!duplicated(mainPaths), ]

  # Add layout of moderator nodes
  if (length(mods) > 0) {
    for (i in 1:length(mods)) {
      # Get index of independent and dependent node in layout
      idxIndep <- which(nodeNames == indeps[i])
      idxDep <- which(nodeNames == paths[isIntPath, 2][i])
      # Calculate pos of hidden helper node as average between indep and dep node pos
      nodePosI <- apply(layout[c(idxIndep, idxDep), ], 2, mean)
      # Moderator pos has same x pos as hidden helper node
      # y pos is chosen so that graph is balanced out
      modPosY <- ifelse(abs(max(layout[, 2])) > abs(min(layout[, 2])), min(layout[, 2]) - 1, max(layout[, 2]) + 1)
      modPos <- c(nodePosI[1], modPosY)
      # Append to node names and layout
      if (type == "conceptual") {
        nodeNames <- c(nodeNames, mods[i], paste0("i", i))
        layout <- rbind(layout, modPos, nodePosI)
      } else {
        # Place interaction term above/below moderator node
        intPos <- c(nodePosI[1], modPosY + ifelse(modPosY > 0, 1, -1))
        nodeNames <- c(nodeNames, mods[i], paths[isIntPath, 1][i])
        layout <- rbind(layout, modPos, intPos)
      }
    }
  }

  # Order of node labels as in qgraph
  graphNodeNames <- unique(as.vector(mainPaths[, 1:2, drop = FALSE]))
  # Get idx of hidden helper node (to make it invisible)
  graphIntIdx <- grepl("i[[:digit:]]", graphNodeNames)

  nNodes <- length(graphNodeNames)

  # Calc node size depending on number of nodes
  nodeSize <- rep(round(8*exp(-nrow(layout)/80)+1), nNodes)
  # Make hidden helper node invisible step 1
  nodeSize[graphIntIdx] <- 0

  # Invisible node must be circle, otherwise incoming edges are omitted (qgraph bug)
  nodeShape <- rep("square", nNodes)
  nodeShape[graphIntIdx] <- "circle"

  # Make hidden helper node invisible step 2
  nodeLabels <- graphNodeNames
  nodeLabels[graphIntIdx] <- ""

  edge_color <- rep("black", nrow(mainPaths))
  
  # Hide helper edge for single moderator
  if (length(mods) == 1 && nrow(mainPaths) == 3 && type == "conceptual") {
    edge_color[3] <- "white"
  }

  if (type == "conceptual") {
    edge_labels <- FALSE
  } else {
    edge_labels <- mainPaths[, 3]

    if (estimates) edge_labels <- round(as.numeric(edge_labels), 3)
  }
  
  g <- jaspBase:::.suppressGrDevice(qgraph::qgraph(
    mainPaths[, 1:2, drop = FALSE],
    layout = layout[match(graphNodeNames, nodeNames), ], # match order of layout
    vsize = nodeSize,
    shape = nodeShape,
    labels = TRUE,
    border.width = 1.5,
    edge.label.cex = 1.2,
    edge.color = edge_color,
    edge.labels = edge_labels
  ))

  # There seems to be a bug in qgraph where specifying labels
  # in the initial function call does not work
  g$graphAttributes$Nodes$labels <- abbreviate(.procDecodeVarNames(nodeLabels), minlength = 3)

  return(g)
}

.procPlotSyntax <- function(container, options, modelsContainer) {
  if (!options[["syntax"]]) return()

  if (is.null(container[["syntaxContainer"]])) {
    syntaxContainer <- createJaspContainer(title = gettext("Model syntax"))
    syntaxContainer$dependOn(c(.procGetDependencies(), "syntax"))
    container[["syntaxContainer"]] <- syntaxContainer
  } else {
    syntaxContainer <- container[["syntaxContainer"]]
  }

  for (i in 1:length(options[["processModels"]])) {
    modelName <- options[["processModels"]][[i]][["name"]]
    if (is.null(syntaxContainer[[modelName]])) {
      modelSyntax <- createJaspHtml(modelsContainer[[modelName]][["syntax"]]$object, class = "jasp-code", title = modelName)
      modelSyntax$dependOn(
        nestedOptions = .procGetSingleModelsDependencies(as.character(i))
      )
      syntaxContainer[[modelName]] <- modelSyntax
    }
  }
}

# Helper functions ----

.pasteExpandGrid <- function(obj, collapse) {
  return(apply(expand.grid(obj), 1, paste, collapse = collapse))
}

.doCallPaste <- function(obj, sep) {
  return(do.call(paste, append(obj, list(sep = sep))))
}

.pasteDot <- function(...) {
  return(paste(..., sep = "."))
}
