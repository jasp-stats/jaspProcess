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

  dataset <- .procAddIntVars(jaspResults, dataset, options)

  dataset <- .procAddFactorDummyVars(jaspResults, dataset, options)

  .procAddLavModParNames(jaspResults, options)

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
    pathVarsSplit <- strsplit(pathVars, ":|__") # split according to `:` or `__`
    isThreeWayInt <- grepl("__", pathVars)
    
    # Replace dummy vars for each term of interactions separately
    pathVarsSplit <- lapply(pathVarsSplit, .replaceDummyVars)

    # Paste interaction terms back together
    pathVars[!isThreeWayInt] <- unlist(sapply(pathVarsSplit[!isThreeWayInt], paste, collapse = ":"))
    pathVars[isThreeWayInt] <- unlist(sapply(pathVarsSplit[isThreeWayInt], paste, collapse = "__"))
    regList[[i]][["vars"]] <- encodeColNames(pathVars)
  }

  # Replace dummy variables in dependent variables
  names(regList) <- encodeColNames(.replaceDummyVars(names(regList)))
  
  return(regList)
}

.procModProbes <- function(jaspResults, dataset, options) {
  modelsContainer <- jaspResults[["modelsContainer"]]

  for (i in 1:length(options[["processModels"]])) {
    modelOptions <- options[["processModels"]][[i]]
    modelName <- modelOptions[["name"]]

    if (is.null(modelsContainer[[modelName]][["modProbes"]])) {
      modProbes <- .procModProbesSingleModel(modelsContainer[[modelName]], dataset, options)
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
    rowSplit <- .strsplitColon(regList[[i]][["vars"]])

    for (v in rowSplit[sapply(rowSplit, length) > 1]) {
      modVars[[v[2]]] <- c(modVars[[v[2]]], v[1])
    }
  }

  return(modVars)
}

.procModProbesSingleModel <- function(container, dataset, options) {
  probeVals <- sapply(options[["moderationProbes"]], function(row) row[["probePercentile"]])/100

  regList <- container[["regList"]]$object

  modVars <- .procModVarsFromRegList(regList)

  contrasts <- container[["contrasts"]]$object

  modProbes <- lapply(names(modVars), function(nms) {
    matchFac <- sapply(options[["factors"]], grepl, x = nms)
    
    if (length(matchFac) > 0 && any(matchFac)) {
      whichFac <- options[["factors"]][matchFac]
      conMat <- contrasts[[whichFac]]
      colIdx <- which(paste0(whichFac, colnames(conMat)) == nms)
      row.names(conMat) <- as.character(conMat[, colIdx])
      return(conMat[, colIdx])
    }
    return(quantile(dataset[[nms]], probs = probeVals))
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
    paste0(names(container[["regList"]]$object)),
    sapply(container[["regList"]]$object, function(row) paste(row$parNames, row$vars, sep = "*", collapse = " + ")),
    sep = " ~ "
  )

  regSyntax <- paste(
    sapply(container[["regList"]]$object, function(row) row[["comment"]]),
    regSyntax,
    sep = "",
    collapse = "\n"
  )

  modVars <- .procModVarsFromRegList(container[["regList"]]$object)

  medEffectSyntax <- .procMedEffects(container[["regList"]]$object, modVars, container[["modProbes"]]$object, container[["contrasts"]]$object)

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

.procAddLavModParNames <- function(jaspResults, options) {
  modelsContainer <- jaspResults[["modelsContainer"]]

  for (i in 1:length(options[["processModels"]])) {
    modelOptions <- options[["processModels"]][[i]]
    modelName <- modelOptions[["name"]]
  
    if (is.null(modelsContainer[[modelName]][["syntax"]])) {
      regList <- .procAddLavModParNamesSingleModel(modelsContainer[[modelName]][["regList"]]$object)
      modelsContainer[[modelName]][["regList"]]$object <- regList
    }
  }
}

.procAddLavModParNamesSingleModel <- function(regList) {
  # Get names of dependent vars
  depVars <- names(regList)

  for (i in 1:length(regList)) {
    # Split interaction terms
    vSplit <- lapply(regList[[i]]$vars, function(v) {
      unlist(.strsplitColon(v))
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
    # Variable names need to be encoded because JASP does not automatically encode for dropdowns
    dependent <- encodeColNames(path[["processDependent"]])
    independent <- encodeColNames(path[["processIndependent"]])
    type <- encodeColNames(path[["processType"]])
    processVariable <- encodeColNames(path[["processVariable"]])

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
      # This routine adds three-way interactions (moderated moderation) 
      # which are not available in standard lavaan syntax. These interactions
      # are represented as separate variables which are products of the three interacting variables.
      # The names of these variables are the interacting variable names separated by 
      # double underscores, e.g.: var1__var2__var3

      # Get all existing interaction terms
      isInt <- grepl(":", regList[[dependent]][["vars"]])
      if (any(isInt)) {
        # Split interaction terms
        varsSplit <- .strsplitColon(regList[[dependent]][["vars"]])
        for (v in varsSplit[isInt]) {
          # If the second term of the interaction is the independent of current path
          # this indicates a three-way interaction with the independent variable
          if (v[length(v)] == independent) {
            # Create three-way interaction variable name
            interVar <- paste0(paste(v, collapse = "__"), "__", processVariable)
            # Add interaction independent x moderator1 x moderator2
            regList <- .procAddLavModVar(regList, dependent, interVar)

            # Also add a two-way interaction moderator1 x moderator2,
            # otherwise model might be underspecified
            interVar <- paste0(v[1], ":", processVariable)
            regList <- .procAddLavModVar(regList, dependent, interVar)
          }
        }
      }
      # Add regular interaction independent x moderator var to regress on dependent var
      interVar <- paste0(independent, ":", processVariable)
      regList <- .procAddLavModVar(regList, dependent, interVar)
    }

    if (type == "confounders") {
      # Add extra regression equation where confounder -> independent variable
      regList[[independent]] = list(vars = c(), dep = FALSE)
      regList <- .procAddLavModVar(regList, independent, processVariable)
    }
  }

  return(regList)
}

.procModEffects <- function(modProbes) {
  modEffects <- lapply(names(modProbes), function(nms) {
    labels <- paste0(nms, gsub("\\%", "", names(modProbes[[nms]])))
    values <- modProbes[[nms]]
    return(paste(labels, values, sep = " := ", collapse = "\n"))
  })
  return(paste0("\n# Moderation probes\n", paste(modEffects, collapse = "\n")))
}

.procMedEffectFromPath <- function(path, regList, modProbes, contrasts) {
  contrastsConc <- lapply(names(contrasts), function(nm) paste0(nm, colnames(contrasts[[nm]])))

  return(lapply(2:length(path), function(i) {
    regListRow <- regList[[names(path)[i]]]
    isMedVar <- regListRow$vars == names(path)[i-1]
    medPars <- regListRow$parNames[isMedVar]
    isIntVar <- grepl(":", regListRow$vars)
    intVarsProbeNames <- NULL

    if (any(isIntVar)) {
      regVarsSplit <- .strsplitColon(regListRow$vars[isIntVar])
      intVarIsMed <- sapply(regVarsSplit, function(v) v[1] %in% regListRow$vars[isMedVar])

      if (any(intVarIsMed)) {
        modIntVars <- sapply(regVarsSplit[intVarIsMed], function(v) v[2])
        intPars <- regListRow$parNames[isIntVar][intVarIsMed]

        intVarsProbes <- lapply(1:length(modIntVars), function(i) paste(intPars[i], format(modProbes[[modIntVars[i]]], digits = 3), sep = "*"))
        intVarsProbeNames <- lapply(modIntVars, function(v) paste(v, gsub("\\%", "", names(modProbes[[v]])), sep = "__"))
        intVarsProbesOut <- intVarsProbes
        intVarsProbeNamesOut <- intVarsProbeNames

        if (length(contrastsConc) > 0) {
          modIntFacsIdx <- lapply(modIntVars, function(v) which(sapply(contrastsConc, function(w) v %in% w)))

          for (i in 1:length(modIntFacsIdx)) {
            if (length(modIntFacsIdx[[i]]) > 0) {
              isSameFac <- sapply(modIntFacsIdx, function(j) isTRUE(j == modIntFacsIdx[[i]]))
              intVarsProbesOut[[i]] <- .doCallPaste(intVarsProbes[isSameFac], sep = " + ")
              intVarsProbeNamesOut[[i]] <- .doCallPaste(intVarsProbeNames[isSameFac], sep = ".")
            }
          }
        }

        medPars <- paste0("(", paste(medPars, .pasteExpandGrid(intVarsProbesOut[!duplicated(intVarsProbesOut)], collapse = " + "), sep = " + "), ")")
        intVarsProbeNames <- .pasteExpandGrid(intVarsProbeNamesOut[!duplicated(intVarsProbeNamesOut)], collapse = ".")
      }
    }

    return(list(medPars = medPars, intVars = intVarsProbeNames))
  }))
}

.procMedEffects <- function(regList, modVars, modProbes, contrasts) {
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
  medEffectsList <- lapply(medPaths, .procMedEffectFromPath, regList = regList, modProbes = modProbes, contrasts = contrasts)

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
  medEffectPathNames <- sapply(medPaths, function(path) paste(decodeColNames(names(path)), collapse = "__"))

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
  totEffect <- .pasteExpandGrid(medEffectsListCombined, collapse = " + ")

  totEffectNames <- .pasteExpandGrid(Filter(function(x) length(x) > 0, medEffectNamesListCombined), collapse = ".")
  
  # Get total indirect effect of X on Y
  totIndEffect <- .pasteExpandGrid(.doCallPaste(medEffectsListCombined[-1], sep = " + "), collapse = " + ")
  
  indEffectNames <- .pasteExpandGrid(Filter(function(x) length(x) > 0, medEffectNamesListCombined[-1]), collapse = ".")

  # Only select total effect if there are no indirect effects
  if (length(totEffectNames) == 0) {
    totLabels <- "tot"
  } else {
    totLabels <- .pasteDot(rep("tot", length(totEffect)), totEffectNames)
  } 
  if (length(indEffectNames) == 0) {
    indLabels <- "totInd"
  } else {
    indLabels <- .pasteDot(rep("totInd", length(totIndEffect)), indEffectNames)
  } 

  totalEffectsSyntax <- paste(
    c(totLabels, indLabels),
    c(totEffect, totIndEffect),
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
  
  if (length(exoVars[-intIdx]) > 1 && includeExo) {
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
      names(resCovList),
      sapply(resCovList, function(row) paste(row, collapse = " + ")),
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

.procAddIntVars <- function(jaspResults, dataset, options) {
  modelsContainer <- jaspResults[["modelsContainer"]]

  for (i in 1:length(options[["processModels"]])) {
    modelOptions <- options[["processModels"]][[i]]
    modelName <- modelOptions[["name"]]
    regList <- modelsContainer[[modelName]][["regList"]]$object

    if (!.procCheckFitModel(regList)) next

    for (row in regList) {
      # Split three-way interaction variables in syntax
      varsSplit <- strsplit(row[["vars"]], "__")

      # If there is a three-way interaction in a path, create a new variable in the dataset
      # as a product of the three interacting variables
      for (var in varsSplit[sapply(varsSplit, length) > 1]) {
        # Create variable name like var1__var2__var3
        varName <- paste(var, collapse = "__")
        # Compute product of interacting variables by first creating a string "var1 * var2 * var3"
        # Then we evaluate the string as R code using the dataset as the environment to evaluate in
        intVar <- eval(str2lang(paste(encodeColNames(var), collapse = "*")), envir = dataset)
        dataset[[encodeColNames(varName)]] = intVar
      }
    }
  }
  return(dataset)
}

.procAddFactorDummyVars <- function(jaspResults, dataset, options) {
  modelsContainer <- jaspResults[["modelsContainer"]]

  for (i in 1:length(options[["processModels"]])) {
    modelOptions <- options[["processModels"]][[i]]
    modelName <- modelOptions[["name"]]
    regList <- modelsContainer[[modelName]][["regList"]]$object

    if (!.procCheckFitModel(regList)) next

    contrastList <- list()

    for (i in 1:length(regList)) {
      pathVars <- encodeColNames(regList[[i]][["vars"]])
      # Convert regression variables to formula
      pathFormula <- formula(paste("~", paste(pathVars, collapse = "+")))

      # Create dummy variables for factors
      pathDummyMat <- model.matrix(pathFormula, data = dataset)

      # Add dummy variables to dataset
      dataset <- merge(dataset, pathDummyMat)

      # Get dummy coding for contrasts
      contrasts <- attr(pathDummyMat, "contrasts")

      for (f in names(contrasts)) {
        contrastList[[f]] <- do.call(contrasts[[f]], list(levels(as.factor(dataset[[f]]))))
      }

      # Replace dummy-coded variables in regList
      regList[[i]][["vars"]] <- colnames(pathDummyMat)[-1]
    }
    
    modelsContainer[[modelName]][["contrasts"]] <- createJaspState(contrastList)
    modelsContainer[[modelName]][["regList"]]$object <- regList
  }

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
    varsSplit <- .strsplitColon(row$vars)
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

  fitTable <- createJaspTable(title = gettext("Model fit"))
  fitTable$dependOn(c(.procGetDependencies(), "processModels", "aicWeights", "bicWeights"))
  fitTable$position <- 0

  modelNames <- sapply(options[["processModels"]], function(mod) mod[["name"]])
  isInvalid <- sapply(procResults, is.character)

  fitTable$addColumnInfo(name = "Model",    title = "",                            type = "string" )
  fitTable$addColumnInfo(name = "AIC",      title = gettext("AIC"),                type = "number" )
  if (options[["aicWeights"]]) {
    fitTable$addColumnInfo(name = "wAIC", title = gettext("AIC weight"), type = "number")
  }
  fitTable$addColumnInfo(name = "BIC",      title = gettext("BIC"),                type = "number" )
  if (options[["bicWeights"]]) {
    fitTable$addColumnInfo(name = "wBIC", title = gettext("BIC weight"), type = "number")
  }
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

  if (length(procResults) == 0) return()

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
    lrtArgs[["model.names"]] <- modelNames
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

  if (options[["aicWeights"]]) {
    fitTable[["wAIC"]] <- .computeWeights(lrt$value[["AIC"]])
  }
  if (options[["bicWeights"]]) {
    fitTable[["wBIC"]] <- .computeWeights(lrt$value[["BIC"]])
  }

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

  pathCoefTable[["lhs"]] <- gsub("__", ":", pathCoefs$rhs)
  pathCoefTable[["op"]]  <- rep("\u2192", nrow(pathCoefs))
  pathCoefTable[["rhs"]] <- gsub("__", ":", pathCoefs$lhs)

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
      modProbes[[condEff[1]]] <- c(modProbes[[condEff[1]]], condEff[2])

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

  labelSplit <- lapply(strsplit(medEffects$lhs, "\\."), strsplit, split = "__")
  
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
    medEffectsTable$addColumnInfo(name = mod, title = mod, type = "string", combine = FALSE) # combine = F because empty cells indicate no moderation
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

  labelSplit <- lapply(strsplit(medEffects$lhs, "\\."), strsplit, split = "__")

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
    totEffectsTable$addColumnInfo(name = mod, title = mod, type = "string", combine = FALSE)
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
  procPathPlot$dependOn(
    options = "pathPlotsLabelLength",
    nestedOptions = list(c("processModels", as.character(modelIdx), "conceptualPathPlot"))
  )
  container[["conceptPathPlot"]] <- procPathPlot

  if (container$getError()) return()

  procPathPlot$plotObject <- .procLavToGraph(procResults, type = "conceptual", estimates = FALSE, options)
}

.procStatPathPlot <- function(container, options, procResults, modelIdx) {
  if (!is.null(container[["statPathPlot"]]) || !procResults@Options[["do.fit"]]) return()

  procPathPlot <- createJaspPlot(title = gettext("Statistical path plot"), height = 320, width = 480)
  procPathPlot$dependOn(
    options = c("statisticalPathPlotsParameterEstimates", "pathPlotsLabelLength"),
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

.procModGraphLayoutConceptual <- function(intPathsSplitPruned, layout) {
  # Node names are in rownames
  nodeNames <- rownames(layout)

  # Keep track of different moderators
  j <- 1

  for (path in intPathsSplitPruned) {
    # Calc y pos for first moderator in balance to existing layout
    modPosY <-.minMaxSubAddOne(layout[, 2])

    # Iterate over moderators leaving out the first and last element (independent and dependent variables)
    for (i in 1:(length(path)-2)) {
      # Only add moderators (at index i+1 in path) that are not in the layout yet 
      if (!path[i+1] %in% nodeNames) {
        # Get index of independent and dependent node in layout
        # Independent node is at index i and dependent is the last element in path
        idxIndep <- which(nodeNames == path[i])
        idxDep <- which(nodeNames == path[length(path)])
        # Calculate pos of hidden helper node as average between indep and dep node pos
        nodePosI <- apply(layout[c(idxIndep, idxDep), ], 2, mean)
        modPosY <- modPosY + (i-1) * sign(modPosY)

        if (i == 1) {
          # First moderator has same x pos as hidden helper node
          modPos <- c(nodePosI[1], modPosY)
        } else {
          # Moderating moderator gets pos as average between independent (at previous index in path) and first moderator
          idxPrev <- which(nodeNames == path[i-1])
          modPos <- apply(layout[c(idxPrev, idxIndep), ], 2, mean)
        }
        # Append to node names and layout
        # Hidden helper node gets index j
        nodeNameI <- paste0("i",  j)
        # Name of moderator is at index i+1 in path
        nodeNames <- c(nodeNames, path[i+1], nodeNameI)
        layout <- rbind(layout, modPos, nodePosI)
        # Set hidden helper node as last element in path so it becomes the dependent node in next interation
        path[length(path)] <- nodeNameI
        # Increase number of moderators
        j <- j + 1
      }
    }
  }

  # Assign updated node names back to layout
  rownames(layout) <- nodeNames

  return(layout)
}

.procModGraphLayoutStatistical <- function(intPathsSplitPruned, layout) {
  # Node names are in rownames
  nodeNames <- rownames(layout)

  for (path in intPathsSplitPruned) {
    # Get index of independent and dependent node in layout
    idxIndep <- which(nodeNames == path[1])
    idxDep <- which(nodeNames == path[length(path)])

    # Calculate pos of hidden helper node as average between indep and dep node pos
    nodePosI <- apply(layout[c(idxIndep, idxDep), ], 2, mean)

    # Calc y pos for first moderator in balance to existing layout
    modPosY <- .minMaxSubAddOne(layout[, 2])

    # Iterate over moderators leaving out the first and last element (independent and dependent variables)
    for (i in 1:(length(path)-2)) {
      # Update y coordinate depending on number of moderators on path
      # Add +/- 1 for each additional moderator
      modPosY <- modPosY + (i-1) * sign(modPosY)

      # Moderator pos has same x pos as hidden helper node
      modPos <- c(nodePosI[1], modPosY)

      # Calculate all combinations of moderators that interact with each other
      combs <- combn(path[-length(path)], length(path)-i)

      # Create interaction term node labels from combinations
      ints <- apply(combs, 2, paste, collapse = ":")

      # Spread out interaction terms on the y axis (all on same x axis)
      intsPos <- cbind(nodePosI[1], sign(modPosY) * (1:length(ints)) + modPosY)

      # Append to node names and layout
      # Name of moderator is at index i+1 in path, also add interaction terms
      nodeNames <- c(nodeNames, path[i+1], ints)
      layout <- rbind(layout, modPos, intsPos)

      # Update starting pos of next moderator path by maximum of interaction y coordinate
      modPosY <- intsPos[length(intsPos)]
    }
  }

  # Assign updated node names back to layout
  rownames(layout) <- nodeNames

  return(layout)
}

.procPruneIntTerms <- function(paths, prunedPaths = list()) {
  # This function prunes moderator paths
  # It finds the longest path and removes all paths that are a subset of this path
  # leaving longest unique paths
  
  # Find longest moderator path
  longestPathIdx <- which.max(sapply(paths, length))

  # Add it to result
  prunedPaths <- append(prunedPaths, paths[longestPathIdx])

  # Check which other paths are a subset of longest path
  allVarsInLongestPath <- sapply(paths[-longestPathIdx], function(path) all(path %in% paths[[longestPathIdx]]))
  
  # If all are a subset, return only longest paths
  if (all(allVarsInLongestPath)) return(prunedPaths)

  # Apply function recursively removing the longest path and all subset paths
  return(.procPruneIntTerms(paths[-longestPathIdx][!allVarsInLongestPath], prunedPaths))
}

.procLavToGraph <- function(procResults, type, estimates, options) {
  # Get table with SEM pars from lavaan model
  parTbl <- lavaan::parameterTable(procResults)
  parTbl$lhs <- gsub("__", ":", parTbl$lhs)
  parTbl$rhs <- gsub("__", ":", parTbl$rhs)

  # Create path matrix where first col is "from" and second col is "to", third col is estimate
  labelField <- ifelse(estimates, "est", "label")
  paths <- matrix(c(parTbl$rhs, parTbl$lhs, parTbl[[labelField]])[parTbl$op == "~"], ncol = 3)
  
  if (type == "conceptual") {
    # Remove nodes for factor levels and replace them by factor name, removing levels (e.g., factorLevel1 -> factor)
    paths[,1:2] <- apply(paths[, 1:2], 2, function(col) {
      # Init output vector
      out <- numeric(length(col))
      # For each row in column
      for (i in 1:length(col)) {
        # Split variable name according to interactions
        v_split <- .strsplitColon(col[i])[[1]]
        # Init output vector
        v_out <- numeric(length(v_split))
        # For each term of interaction
        for (j in 1:length(v_split)) {
          # Check if variable name is a factor
          matchFac <- sapply(options[["factors"]], grepl, x = v_split[j])
          
          # If it is replace by factor name otherwise keep original variable name
          if (length(matchFac) > 0 && any(matchFac)) {
            v_out[j] <- options[["factors"]][matchFac]
          } else {
            v_out[j] <- v_split[j]
          }
        }

        # Paste interactions back together
        whichFac <- paste(v_out, collapse = ":")

        # If any term was factor replace otherwise keep original variable name
        if (whichFac != "") {
          out[i] <- whichFac
        } else {
          out[i] <- col[i]
        }
      }

      return(out)
    })

    # Remove duplicated paths
    paths <- paths[!duplicated(paths[, 1:2]), ]
  }

  # Check if "from" contains interaction term
  isIntPath <- grepl(":", paths[, 1])

  # Split interaction terms
  intPathsSplit <- .strsplitColon(paths[isIntPath, 1])

  # Get moderator vars from interaction terms
  mods <- unique(unlist(sapply(intPathsSplit, function(path) path[-1])))

  # Create matrix with moderator paths
  if (type == "conceptual") {
    # Adds paths from moderators to helper nodes "iX" which will be invisible
    modPaths <- matrix(c(mods, paste0("i", 1:length(mods)), rep("", length(mods))), ncol = 3)

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
  layout <- .procMainGraphLayout(mainPaths[, 1:2, drop = FALSE], options[["dependent"]])

  # Node names are in rownames
  nodeNames <- rownames(layout)

  # Combine main paths and moderator paths
  if (sum(isIntPath) > 0) mainPaths <- rbind(mainPaths, modPaths)

  # Remove duplicate paths
  mainPaths <- mainPaths[!duplicated(mainPaths), ]
  
  # Add layout of moderator nodes
  if (length(mods) > 0) {
    # Add dependent variable to end of each moderator path
    intPathsSplitDep <- lapply(1:length(intPathsSplit), function(i) c(intPathsSplit[[i]], paths[isIntPath, 2][i]))
    # Prune moderator paths by only leaving the longest unique paths
    intPathsSplitPruned <- .procPruneIntTerms(intPathsSplitDep)

    # Calculate layout
    if (type == "conceptual") {
      layout <- .procModGraphLayoutConceptual(intPathsSplitPruned, layout)
    } else {
      layout <- .procModGraphLayoutStatistical(intPathsSplitPruned, layout)
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
  nodeLabels <- decodeColNames(graphNodeNames) # TODO: Remove decodeColNames in future
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
    layout = layout[match(graphNodeNames, rownames(layout)), ], # match order of layout
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
  g$graphAttributes$Nodes$labels <- abbreviate(
    .procDecodeVarNames(nodeLabels),
    minlength = options[["pathPlotsLabelLength"]]
  )

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

.computeWeights <- function(x) {
  diffExp <- exp(-0.5*(x - min(x, na.rm = TRUE)))
  return(diffExp/sum(diffExp, na.rm = TRUE))
}

.strsplitColon <- function(x) {
  return(strsplit(x, ":"))
}

.minMaxSubAddOne <- function(x) {
  # If max(x) is higher than min(x), return min(x) - 1, otherwise max(x) + 1
  minMax <- range(x)

  if (abs(minMax[2]) > abs(minMax[1])) return(minMax[1] - 1)

  return(minMax[2] + 1)
}
