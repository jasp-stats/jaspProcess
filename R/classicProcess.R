#
# Copyright (C) 2018 University of Amsterdam
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
  jaspResults$title <- "Process Analysis"
  # Init options: add variables to options to be used in the remainder of the analysis
  options <- .procInitOptions(jaspResults, options)
  # read dataset
  dataset <- .procReadData(options)
  # error checking
  ready <- .procErrorHandling(dataset, options)

  # Compute (a list of) results from which tables and plots can be created
  procResults <- .procComputeResults(jaspResults, dataset, options)

  # Output containers, tables, and plots based on the results. These functions should not return anything!
  parEstContainer <- .procContainerParameterEstimates(jaspResults, options)
  pathPlotContainer <- .procContainerPathPlots(jaspResults, options)

  .procParameterEstimateTables(parEstContainer, options, procResults)
  .procPathPlots(pathPlotContainer, options, procResults)

  # .procConceptPathPlot(pathPlotContainer, options, procResults)
  # .procStatPathPlot(pathPlotContainer, options, procResults)

  # .procPathCoefficientsTable(parEstContainer, options, procResults)
  # .procPathMediationEffectsTable(parEstContainer, options, procResults)

  # .procTableSomething(jaspResults, options, procResults)
  # .procTableSthElse(  jaspResults, options, procResults)
  # .procPlotSomething( jaspResults, options, procResults)

  return()
}

.procGetDependencies <- function() {
  #if (modelOptions[["inputType"]] == "inputVariables") {
  #return(c('dependent', 'covariates', 'factors', 'processModels'))

  #}

  #if (modelOptions[["inputType"]] == "inputModelNumber") {
  return(c('dependent', 'covariates', 'factors'))
  #}
}

# Init functions ----
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
    } else { # If dependent and mediator
      depIdx <- length(regList) - i + 1
      parSym <- ifelse(isIndep, "a", "d")
    }
    # Concatenate par symbols with idx
    regList[[i]][["parNames"]] <- paste0(parSym, paste0(depIdx, parIdx))
  }

  return(regList)
}

.procToLavModSingleModel <- function(modelOptions, dependent) {

  regList = list()

  if (modelOptions[["inputType"]] == "inputVariables") {
    for (path in modelOptions[["processRelationships"]]) {
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
        regList[[independent]] = list(vars = c(), dep = TRUE)
        regList <- .procAddLavModVar(regList, independent, processVariable)
      }
    }
  }

  if (modelOptions[["inputType"]] == "inputModelNumber") {
    independent  <- modelOptions[["modelNumberIndependent"]]
    mediators    <- modelOptions[["modelNumberMediators"]]
    covariates   <- modelOptions[["modelNumberCovariates"]]
    modW         <- modelOptions[["modelNumberModeratorW"]]
    modZ         <- modelOptions[["modelNumberModeratorZ"]]
    number       <- modelOptions[["modelNumber"]]

    # # Check Hayes model nr. 1
    # if ((dependent  == "" | independent == "" | modW == "" |  modZ != "" |
    #      mediators != "" | covariates != "" ) & modelOptions[["modelNumber"]] == 1) {
    #   print(stringr::str_glue("Error: The specified Hayes model number {number} does
    #                  not match with the (amount of) selected variables."))
    # }
    #
    #  # Check Hayes model nr. 2
    #  if ((dependent  == "" | independent == "" | modW == "" |  modZ  == "" |
    #       mediators != "" | covariates != "" ) & modelOptions[["modelNumber"]] == 2) {
    #    print(stringr::str_glue("Error: The specified Hayes model number {number} does
    #                  not match with the (amount of) selected variables."))
    # }
    #
    # # Check Hayes model nr. 3
    #  if ((dependent  == "" | independent == "" | modW == "" |  modZ  == "" |
    #       mediators != "" | covariates != "" ) & modelOptions[["modelNumber"]] == 3) {
    #    print(stringr::str_glue("Error: The specified Hayes model number {number} does
    #                  not match with the (amount of) selected variables."))
    #  }
    #
    # # Check Hayes model nr. 4
    # if ((dependent  == "" | independent == "" | modW != "" |  modZ  != "" |
    #      mediators  == "" | length(mediators > 1) | covariates != "" ) & modelOptions[["modelNumber"]] == 4) {
    #   print(stringr::str_glue("Error: The specified Hayes model number {number} does
    #                  not match with the (amount of) selected variables."))
    # }
    #
    #
    # # Check Hayes model nr. 5
    # if ((dependent  == "" | independent == "" | modW != "" |  modZ  != "" |
    #      mediators  == "" | length(mediators > 1) | covariates != "" ) & modelOptions[["modelNumber"]] == 5) {
    #   print(stringr::str_glue("Error: The specified Hayes model number {number} does
    #                  not match with the (amount of) selected variables."))
    # }
    #
    # # Check Hayes model nr. 6
    # if ((dependent  == "" | independent == "" | modW != "" |  modZ  != "" |
    #      mediators  == "" | (length(mediators) >= 2 & length(mediators) <= 4) | covariates != "" ) & modelOptions[["modelNumber"]] == 6) {
    #   print(stringr::str_glue("Error: The specified Hayes model number {number} does
    #                  not match with the (amount of) selected variables."))
    # }
    #
    # # Check Hayes model nr. 7
    # if ((dependent  == "" | independent == "" | modW == "" |  modZ  != "" |
    #      mediators  == "" | length(mediators) > 1 | covariates != "" ) & modelOptions[["modelNumber"]] == 7) {
    #   print(stringr::str_glue("Error: The specified Hayes model number {number} does
    #                  not match with the (amount of) selected variables."))
    # }
    #
    # # Check Hayes model nr. 8
    # if ((dependent  == "" | independent == "" | modW == "" |  modZ != "" |
    #     mediators  == "" | length(mediators) > 1 | covariates != "" ) & modelOptions[["modelNumber"]] == 8) {
    #   print(stringr::str_glue("Error: The specified Hayes model number {number} does
    #                  not match with the (amount of) selected variables."))
    # }

    # Init list for regression of new dependent var
    # dep = TRUE to signal this is NOT a mediator; this is used later when assigning par names
    if (!dependent %in% names(regList)) {
      regList[[dependent]] = list(vars = c(), dep = TRUE)
    }

    # Add independent var to regression of dependent var
    regList <- .procAddLavModVar(regList, dependent, independent)

    # account for multiple mediators
    if(length(mediators) != 0) {
      #if (mediators != "") {
      #if (!is.null(mediators) && mediators != "") {

      for (i in 1:length(mediators)) {
        # Init list for regression of new mediator i
        if (!mediators[i] %in% names(regList)) {
          regList[[mediators[i]]] = list(vars = c(), dep = FALSE)
        }
        # Add independent var to regression of mediator i
        regList <- .procAddLavModVar(regList, mediators[i], independent)
        regList <- .procAddLavModVar(regList, dependent, mediators[i])
      }
    }


    if (!is.null(modW) && modW != "") {
      # Add interaction independent x moderatorW var to regress on dependent var
      interVar <- paste0(independent, ":", modW)
      regList <- .procAddLavModVar(regList, dependent, interVar)
      regList <- .procAddLavModVar(regList, dependent, modW)
    }

    if (!is.null(modZ) && modZ != "") {
      # Add interaction independent x moderatorZ var to regress on dependent var
      interVar <- paste0(independent, ":", modZ)
      regList <- .procAddLavModVar(regList, dependent, interVar)
      regList <- .procAddLavModVar(regList, dependent, modZ)
    }

    #if (covariates != "") {
    if(length(covariates) != 0) {
      #if (!is.null(covariates) && covariates != "") {
      # Add extra regression equation where covariate -> independent variable
      for (i in 1:length(covariates)) {

        #regList[[independent]] = list(vars = c(), dep = TRUE)
        regList <- .procAddLavModVar(regList, dependent, covariates[i])
      }
    }
  }

  regList <- .procAddLavModParNames(regList)

  # Concatenate and collapse par names and var names to regression formula
  regSyntax <- paste(
    paste0(encodeColNames(names(regList))),
    sapply(regList, function(row) paste(row$parNames, encodeColNames(row$vars), sep = "*", collapse = " + ")),
    sep = " ~ ",
    collapse = "\n"
  )

  medEffectSyntax <- .procMedEffects(regList)

  return(paste(regSyntax, medEffectSyntax, sep = "\n"))
}

.procMedEffects <- function(regList) {
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
  medEffectsList <- lapply(medPaths, function(path) sapply(2:length(path), function(i) {
    regListRow <- regList[[names(path)[i]]]
    isIntVar <- grepl(":", regListRow$vars)
    regVarsSplit <- strsplit(regListRow$vars, ":")
    indepIntVars <- sapply(regVarsSplit, function(v) v[1])
    medVars <- regListRow$parNames[regListRow$vars == names(path)[i-1]]
    intVars <- regListRow$parNames[indepIntVars == names(path)[i-1] & isIntVar]
    # return(list(medVars = medVars, intVars = intVars))
    return(medVars)
  }))

  # Concatenate to mediation effects by multiplying par names of paths
  syntax <- paste(
    sapply(medPaths, function(path) paste(decodeColNames(names(path)), collapse = "_")),
    sapply(medEffectsList, function(row) paste(row, collapse = " * ")),
    # paste(
    #   sapply(medEffectsList, function(row) paste(row$medVars, collapse = " * ")),
    #   sapply(medEffectsList, function(row) paste(row$intVars, collapse = " + ")),
    #   sep = " + "
    # ),
    sep = " := ",
    collapse = "\n"
  )

  return(syntax)
}

.procInitOptions <- function(jaspResults, options) {
  # Determine if analysis can be run with user input
  # Calculate any options common to multiple parts of the analysis

  options[["modelSyntax"]] <- lapply(options[["processModels"]], .procToLavModSingleModel, dependent = options[["dependent"]])

  return(options)
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
.procResultsFitModel <- function(syntax, dataset, options) {
  # Helper function to compute actual results
  fittedModel <- lavaan::sem(
    model           = syntax,
    data            = dataset,
    se              = "standard"
  )

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
    c("processModels", modelIdx, "modelNumberModeratorZ")
  ))
}

.procComputeResults <- function(jaspResults, dataset, options) {
  # Function to compute and store results in container
  nModels <- length(options[["processModels"]])
  procResults <- list()

  for (i in 1:nModels) {
    modelStateName <- options[["processModels"]][[i]][["name"]]
    
    if (is.null(jaspResults[[modelStateName]])) {
      jaspResults[[modelStateName]] <- createJaspState()
      jaspResults[[modelStateName]]$dependOn(
        # optionContainsValue = list(processModels = options[["processModels"]][[i]]),
        nestedOptions = .procGetSingleModelsDependencies(as.character(i))
      )
      fittedModel <- .procResultsFitModel(
        options[["modelSyntax"]][[i]],
        dataset,
        options
      )
      jaspResults[[modelStateName]]$object <- fittedModel
      procResults[[modelStateName]] <- fittedModel
    } else {
      procResults[[modelStateName]] <- jaspResults[[modelStateName]]$object
    }
  }

  return(procResults)
}

# Output functions ----
# These are not in use for now, but left here as orientation for later
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

.procParameterEstimateTables <- function(container, options, procResults) {
  modelNames <- names(procResults)

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
    if (options[["processModels"]][[i]][["pathCoefficients"]])
      .procPathCoefficientsTable(modelContainer, options, procResults[[i]], i)

    if (options[["processModels"]][[i]][["mediationEffects"]])
      .procPathMediationEffectsTable(modelContainer, options, procResults[[i]], i)
  }
}

.procPathPlots <- function(container, options, procResults) {
  modelNames <- names(procResults)

  for (i in 1:length(procResults)) {
    if (is.null(container[[modelNames[i]]])) {
      modelContainer <- createJaspContainer(title = modelNames[i])
      modelContainer$dependOn(
        nestedOptions = .procGetSingleModelsDependencies(as.character(i))
      )
      container[[modelNames[i]]] <- modelContainer
    } else {
      modelContainer <- container[[modelNames[i]]]
    }
    if (options[["processModels"]][[i]][["conceptualPathPlot"]])
      .procConceptPathPlot(modelContainer, options, procResults[[i]], i)

    if (options[["processModels"]][[i]][["statisticalPathPlot"]])
      .procStatPathPlot(modelContainer, options, procResults[[i]], i)
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

.procPathCoefficientsTable <- function(container, options, procResults, modelIdx) {
  if (!is.null(container[["pathCoefficientsTable"]])) return()

  # Below is one way of creating a table
  pathCoefTable <- createJaspTable(title = gettext("Path coefficients"))
  pathCoefTable$dependOn(
    options = "parameterLabels",
    nestedOptions = list(c("processModels", as.character(modelIdx), "pathCoefficients"))
  )
  container[["pathCoefficientsTable"]] <- pathCoefTable

  pathCoefs <- lavaan::parameterEstimates(procResults)
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

.procPathMediationEffectsTable <- function(container, options, procResults, modelIdx) {
  if (!is.null(container[["mediationEffectsTable"]])) return()

  # Below is one way of creating a table
  medEffectsTable <- createJaspTable(title = gettext("Mediation effects"))
  medEffectsTable$dependOn(
    options = "parameterLabels",
    nestedOptions = list(c("processModels", as.character(modelIdx), "mediationEffects"))
  )

  container[["mediationEffectsTable"]] <- medEffectsTable

  pathCoefs <- lavaan::parameterEstimates(procResults)
  medEffects <- pathCoefs[pathCoefs$op == ":=",]

  # Get paths from label of mediation effect
  medPaths <- lapply(medEffects$lhs, function(path) strsplit(path, "_")[[1]])
  # Get path lengths
  medPathLengths <- sapply(medPaths, length)
  # Sort paths to incresaing length
  medLengthSortIdx <- sort(medPathLengths, index.return = TRUE)$ix
  medEffects <- medEffects[medLengthSortIdx, ]
  print(medEffects)
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

  # Add column with parameter labels
  if (options$parameterLabels) {
    medEffectsTable$addColumnInfo(name = "label", title = gettext("Label"), type = "string")
    medEffectsTable[["label"]] <- gsub("\\*", " \u273B ", medEffects$rhs)
  }

  .procCoefficientsTable(medEffectsTable, options, medEffects)
}

.procConceptPathPlot <- function(container, options, procResults, modelIdx) {
  if (!is.null(container[["conceptPathPlot"]])) return()

  procPathPlot <- createJaspPlot(title = gettext("Conceptual path plot"), height = 320, width = 480)
  procPathPlot$dependOn(nestedOptions = list(c("processModels", as.character(modelIdx), "conceptualPathPlot")))
  container[["conceptPathPlot"]] <- procPathPlot
  procPathPlot$plotObject <- .procLavToGraph(procResults, type = "conceptual", estimates = FALSE, options)
}

.procStatPathPlot <- function(container, options, procResults, modelIdx) {
  if (!is.null(container[["statPathPlot"]])) return()

  procPathPlot <- createJaspPlot(title = gettext("Statistical path plot"), height = 320, width = 480)
  procPathPlot$dependOn(
    options = "statisticalPathPlotsParameterEstimates",
    nestedOptions = list(c("processModels", as.character(modelIdx), "statisticalPathPlot"))
  )
  container[["statPathPlot"]] <- procPathPlot
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

# .procModGraphLayoutPosHelper <- function(layout)

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
  if (nrow(modPaths) == 2 && nrow(mainPaths) == 3 && type == "conceptual") {
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
  g$graphAttributes$Nodes$labels <- abbreviate(nodeLabels, minlength = 3)

  return(g)
}
