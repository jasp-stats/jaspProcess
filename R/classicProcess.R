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

  # Check if all models are ready to compute something
  ready <- .procIsReady(options)

  if (!ready) return()
  # Read dataset
  dataset <- .procReadData(options)
  # Check for errors in dataset
  ready <- .procErrorHandling(dataset, options)
  # Create a container for each model
  .procContainerModels(jaspResults, options)
  # Transform input for each model into a graph for further processing
  .procModelGraph(jaspResults, options)
  # Add three-way interaction variables manually to dataset 
  # because lavaan does not allow threeway interaction regression terms
  dataset <- .procAddIntVars(jaspResults, dataset, options)
  # Add factor dummy variables manually to dataset
  # because lavaan does not do it automatically
  dataset <- .procAddFactorDummyVars(jaspResults, dataset, options)
  # Add parameter names to graph of each model
  .procAddLavModParNames(jaspResults, options)
  # Compute quantiles at which to probe moderators for each model
  .procModProbes(jaspResults, dataset, options)
  # Create lavaan syntax for each model from graph
  .procModelSyntax(jaspResults, options)
  # Fit lavaan models based on syntax and dataset
  modelsContainer <- .procComputeResults(jaspResults, dataset, options)
  # Create container for path plots for each model
  pathPlotContainer <- .procContainerPathPlots(jaspResults, options)
  # Create path plots for each model and add to container
  .procPathPlots(pathPlotContainer, options, modelsContainer)
  # Create table with model fit indices (AIC, ...)
  .procModelSummaryTable(jaspResults, options, modelsContainer)
  # Create container for parameter estimates for each model
  parEstContainer <- .procContainerParameterEstimates(jaspResults, options)
  # Create tables for parameter estimates
  .procParameterEstimateTables(parEstContainer, options, modelsContainer)
  # Create container for local test results for each model
  localTestContainer <- .procContainerLocalTests(jaspResults, options)
  # Create tables with local test results
  .procLocalTestTables(localTestContainer, dataset, options, modelsContainer)
  # Create html output with lavaan syntax for each model
  .procPlotSyntax(jaspResults, options, modelsContainer)

  return()
}

# Helper function for generic dependencies of all models
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
  if (options[["dependent"]] == "" || (length(options[["covariates"]]) == 0 && length(options[["factors"]]) == 0))
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

.procModelGraph <- function(jaspResults, options) {
  modelsContainer <- jaspResults[["modelsContainer"]]

  for (i in 1:length(options[["processModels"]])) {
    modelOptions <- options[["processModels"]][[i]]
    modelName <- modelOptions[["name"]]

    if (is.null(modelsContainer[[modelName]][["graph"]])) {
      graph <- try(procModelGraphSingleModel(options[["processModels"]][[i]], globalDependent = options[["dependent"]]))
      state <- createJaspState(object = graph)
      state$dependOn(
        optionContainsValue = list(processModels = modelOptions),
        nestedOptions = .procGetSingleModelsDependencies(as.character(i))
      )
      
      modelsContainer[[modelName]][["graph"]] <- state
    }
  }
}

procModelGraphSingleModel <- function(modelOptions, globalDependent, options) {
  processRelationships <- switch(modelOptions[["inputType"]],
    inputVariables = modelOptions[["processRelationships"]],
    # Insert function for plotting conceptual hard-coded Hayes model, in case
    # no estimation takes place yet (because of not having filled in all necessary
    # variables)
    inputModelNumber = .procGetHardCodedModel(modelOptions[["modelNumber"]], length(modelOptions[["modelNumberMediators"]]))
  )
  ## TODO: Models involving moderated moderation 19,20,69,71,73

  graph <- .procProcessRelationshipsToGraph(processRelationships)

  if (modelOptions[["inputType"]] == "inputModelNumber")
    graph <- .procModelGraphInputModelNumber(graph, modelOptions, globalDependent)
  
  return(graph)
}

.procProcessRelationshipsToGraph <- function(processRelationships) {
  graph <- igraph::make_empty_graph()
  
  for (path in processRelationships) {
    dependent <- path[["processDependent"]]
    independent <- path[["processIndependent"]]
    type <- path[["processType"]]
    processVariable <- path[["processVariable"]]

    # Add vertices for independent and dependent var
    verticesToAdd <- c(independent, dependent)
    # Add edge from independent to dependent
    edgesToAdd <- verticesToAdd

    if (type != "directs") {
      # Add vertice for processVariable
      verticesToAdd <- c(verticesToAdd, processVariable)
      # Add edge from processVariable to dependent
      edgesToAdd <- c(edgesToAdd, processVariable, dependent)
    }
    # Only add vertices that are not in graph yet
    verticesToAdd <- verticesToAdd[!verticesToAdd %in% igraph::V(graph)$name]

    # Add vertices with name attribute
    graph <- igraph::add_vertices(graph, length(verticesToAdd), name = verticesToAdd)
    # Add edges with source and target attribute
    graph <- igraph::add_edges(graph,
      edges = edgesToAdd,
      # Add source and target for each edge for simple querying
      source = edgesToAdd[seq(1, length(edgesToAdd), 2)], # Odd indices
      target = edgesToAdd[seq(2, length(edgesToAdd), 2)] # Even indices
    )

    if (type == "mediators") {
      # Add edges from independent to processVariable
      graph <- igraph::add_edges(graph,
        edges = c(independent, processVariable),
        source = independent,
        target = processVariable
      )
    }
    
    if (type == "confounders") {
      # Add extra edge from processVariable to independent
      graph <- igraph::add_edges(graph, edges = c(processVariable, independent), source = processVariable, target = independent)
    }

    if (type == "moderators") {
      # This routine adds three-way interactions (moderated moderation)
      # which are not available in standard lavaan syntax. These interactions
      # are represented as separate variables which are products of the three interacting variables.
      # The names of these variables are the interacting variable names separated by
      # double underscores, e.g.: var1__var2__var3

      # Get all existing interaction terms
      sourceVars <- igraph::E(graph)[.to(dependent)]$source
      isInt <- grepl(":", sourceVars)
      if (any(isInt)) {
        # Split interaction terms
        varsSplit <- .strsplitColon(sourceVars)
        for (v in varsSplit[isInt]) {
          # If the second term of the interaction is the independent of current path
          # this indicates a three-way interaction with the independent variable
          if (v[length(v)] == independent) {
            # Create three-way interaction variable name: independent x moderator1 x moderator2
            interVarThree <- paste0(paste(v, collapse = "__"), "__", processVariable)
            # Also add a two-way interaction moderator1 x moderator2,
            # otherwise model might be underspecified
            interVarTwo <- paste0(v[1], ":", processVariable)
            # Add vertices and edges for interaction terms if not in graph yet
            intVerticesToAdd <- c(interVarTwo, interVarThree)[!c(interVarTwo, interVarThree) %in% igraph::V(graph)$name]

            graph <- igraph::add_vertices(graph, length(intVerticesToAdd), name = intVerticesToAdd)
            graph <- igraph::add_edges(graph,
                edges = c(interVarTwo, dependent, interVarThree, dependent),
                source = c(interVarTwo, interVarThree),
                target = c(dependent, dependent)
              )
          }
        }
      }
      # Add regular interaction independent x moderator var to regress on dependent var
      interVar <- paste0(independent, ":", processVariable)

      # Add vertices and edges for two-way interaction
      if (!interVar %in% igraph::V(graph)$name) {
        graph <- igraph::add_vertices(graph, 1, name = interVar)
      }
      graph <- igraph::add_edges(graph,
        edges = c(interVar, dependent),
        source = interVar, target = dependent
      )
    }
  }

  return(.procGraphAddAttributes(igraph::simplify(graph, edge.attr.comb = "first")))
}

.procGraphAddAttributes <- function(graph) {
  # Is node an interaction term
  igraph::V(graph)$isInt <- grepl(":|__", igraph::V(graph)$name)
  # Which are the node interaction vars
  igraph::V(graph)$intVars <- strsplit(igraph::V(graph)$name, ":|__")
  # How many interaction vars are there
  igraph::V(graph)$intLength <- sapply(igraph::V(graph)$intVars, length)
  # Is global dependent
  igraph::V(graph)$isDep <- igraph::degree(graph, mode = "out") == 0
  # Is exogenous
  igraph::V(graph)$isExo <- igraph::degree(graph, mode = "in") == 0
  # Is mediator
  igraph::V(graph)$isMed <- !igraph::V(graph)$isDep & !igraph::V(graph)$isExo
  # Is part of interaction term
  igraph::V(graph)$isPartOfInt <- sapply(igraph::V(graph)$name, function(v) {
    # Is node name part of any interaction term
    any(sapply(igraph::V(graph)$intVars, function(vars) v %in% vars[-1]))
  })
  # Is treatment variable (i.e., X): When exo, not int, and not part of int
  igraph::V(graph)$isTreat <- igraph::V(graph)$isExo & !igraph::V(graph)$isInt & !igraph::V(graph)$isPartOfInt
  # Is edge moderated
  igraph::E(graph)$isMod <- FALSE

  # Which are moderation variables for each edge; NULL if none
  for (i in 1:length(igraph::E(graph))) {
    sourceNode <- igraph::V(graph)[igraph::E(graph)$source[i]]

    if (sourceNode$isInt) {
      sourceNodeIntVars <- unlist(sourceNode$intVars)
      
      for (v in sourceNodeIntVars) {
        # Set all edges from var interacting with sourceNode to target as isMod
        igraph::E(graph)[source == v & target == igraph::E(graph)$target[i]]$isMod <- TRUE
        # Store unique moderating variables
        igraph::E(graph)[source == v & target == igraph::E(graph)$target[i]]$modVars <- list(
          unique(c(
            igraph::E(graph)[source == v & target == igraph::E(graph)$target[i]]$modVars[[1]],
            sourceNodeIntVars[sourceNodeIntVars != v]
          ))
        )
      }
    }
  }

  return(graph)
}

.procVarEncoding <- function() {
  # Encoding for dummy variables
  return(list(
    Y = "JaspProcess_Dependent_Encoded",
    X = "JaspProcess_Independent_Encoded",
    W = "JaspProcess_ModeratorW_Encoded",
    Z = "JaspProcess_ModeratorZ_Encoded",
    M = "JaspProcess_Mediator_Encoded"
  ))
}

.procEncodePath <- function(path) {
  # Encode all variables in a path
  return(lapply(path, function(v) {
    if (v %in% c("mediators", "moderators", "confounders", "directs", ""))
      return(v)
    if (grepl("M", v))
      return(gsub("M", .procVarEncoding()[["M"]], v))
    return(.procVarEncoding()[[v]])
  }))
}

.procEncodeProcessRelationships <- function(processRelationships) {
  # Encode all paths
  return(lapply(processRelationships, .procEncodePath))
}

.procDecodeVarNames <- function(varNames) {
  # Decode a vector of var names
  encoding <- .procVarEncoding()
  for (nm in names(encoding)) {
    varNames <- gsub(encoding[[nm]], nm, varNames)
  }
  return(varNames)
}

.procCheckRegListVars <- function(vars) {
  # Check if vector of var names contains encoded X, W, Z, or M (not Y!!)
  encoding <- .procVarEncoding()
  return(!any(c(encoding[["X"]], encoding[["W"]], encoding[["Z"]]) %in% vars) && !any(grepl(encoding[["M"]], vars)))
}

.procReplaceDummyVars <- function(vars, modelOptions, globalDependent) {
  # Get encoding
  encoding <- .procVarEncoding()

  independent  <- modelOptions[["modelNumberIndependent"]]
  mediators    <- modelOptions[["modelNumberMediators"]]
  modW         <- modelOptions[["modelNumberModeratorW"]]
  modZ         <- modelOptions[["modelNumberModeratorZ"]]

  # Replace encoded X, W, Z with user variables
  if (independent != "")
    vars <- gsub(encoding[["X"]], independent, vars)
  if (modW != "")
    vars <- gsub(encoding[["W"]], modW, vars)
  if (modZ != "")
    vars <- gsub(encoding[["Z"]], modZ, vars)

  # Is var a mediator?
  isMed <- grepl(encoding[["M"]], vars)
  # Which mediator index?
  medIdx <- stringr::str_extract(vars[isMed], "[0-9]")
  medIdx <- as.integer(medIdx[!is.na(medIdx)])
  
  if (length(medIdx) > 0 && length(mediators) > 0) {
    for (i in 1:length(medIdx)) {
      if (length(mediators) >= medIdx[i]) {
        vars <- gsub(vars[isMed][i], mediators[medIdx[i]], vars)
      }
    }
  }

  # If mediator has no index still replace
  if ((length(medIdx) == 0) && sum(isMed) > 0) {
    for (i in 1:length(vars[isMed])) {
      if (length(mediators) >= i) {
        vars <- gsub(vars[isMed][i], mediators[i], vars)
      }
    }
  }
  
  # Replace encoded Y with user variable
  vars <- gsub(encoding[["Y"]], globalDependent, vars)

  return(vars)
}

.procModelGraphInputModelNumber <- function(graph, modelOptions, globalDependent) {
  # Replace dummy vars in graph attributes
  igraph::V(graph)$name <- .procReplaceDummyVars(igraph::V(graph)$name, modelOptions = modelOptions, globalDependent = globalDependent)
  igraph::E(graph)$source <- .procReplaceDummyVars(igraph::E(graph)$source, modelOptions = modelOptions, globalDependent = globalDependent)
  igraph::E(graph)$target <- .procReplaceDummyVars(igraph::E(graph)$target, modelOptions = modelOptions, globalDependent = globalDependent)
  
  if (!is.null(igraph::E(graph)$modVars))
    igraph::E(graph)$modVars <- sapply(igraph::E(graph)$modVars, function(x) if (!is.null(x)) .procReplaceDummyVars(x, modelOptions = modelOptions, globalDependent = globalDependent)) # Returns a list!
  
  igraph::V(graph)$intVars <- sapply(igraph::V(graph)$intVars, .procReplaceDummyVars, modelOptions = modelOptions, globalDependent = globalDependent)

  if (any(igraph::V(graph)$intLength == 2)) {
    igraph::V(graph)[intLength == 2]$name <- unlist(sapply(igraph::V(graph)[intLength == 2]$intVars, paste, collapse = ":"))
  }
  if (any(igraph::V(graph)$intLength == 3)) {
    igraph::V(graph)[intLength == 3]$name <- unlist(sapply(igraph::V(graph)[intLength == 3]$intVars, paste, collapse = "__"))
  }
  
  return(graph)
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
    graph <- modelsContainer[[modelName]][["graph"]]$object

    if (!.procCheckFitModel(graph)) next
    
    for (v in igraph::V(graph)[intLength == 3]$intVars) {
      # Create variable name like var1__var2__var3
      varName <- paste(encodeColNames(v), collapse = "__")
      # Compute product of interacting variables by first creating a string "var1 * var2 * var3"
      # Then we evaluate the string as R code using the dataset as the environment to evaluate in
      intVar <- eval(str2lang(paste(encodeColNames(v), collapse = "*")), envir = dataset)
      dataset[[varName]] = intVar
      names(dataset)
    }
  }
  return(dataset)
}

.procAddFactorDummyVars <- function(jaspResults, dataset, options) {
  modelsContainer <- jaspResults[["modelsContainer"]]

  for (i in 1:length(options[["processModels"]])) {
    modelOptions <- options[["processModels"]][[i]]
    modelName <- modelOptions[["name"]]
    graph <- modelsContainer[[modelName]][["graph"]]$object

    if (!.procCheckFitModel(graph)) next

    contrastList <- list()

    sourceVars <- unique(igraph::E(graph)$source)
    
    # Convert regression variables to formula
    sourceFormula <- formula(paste("~", paste(encodeColNames(sourceVars), collapse = "+")))

    # Create dummy variables for factors
    sourceDummyMat <- model.matrix(sourceFormula, data = dataset)

    # Add dummy variables to dataset
    dataset <- merge(dataset, sourceDummyMat)

    # Get dummy coding for contrasts
    contrasts <- attr(sourceDummyMat, "contrasts")

    for (f in names(contrasts)) {
      contrastList[[f]] <- do.call(contrasts[[f]], list(levels(as.factor(dataset[[f]]))))
    }

    # Replace dummy-coded variables in graph
    for (v in names(contrastList)) {
      newNodeNames <- paste0(v, colnames(contrastList[[v]]))
      
      # If dummy coding needs additional variables add them as nodes with same edges to target variable
      if (length(newNodeNames) > 1) {
        graph <- igraph::add_vertices(graph, length(newNodeNames[-1]), name = newNodeNames[-1])
        graph <- igraph::add_edges(graph,
          edges = as.vector(rbind(newNodeNames[-1], igraph::E(graph)[.from(v)]$source)),
          source = newNodeNames[-1],
          target = igraph::E(graph)[.from(v)]$source
        )
      }
      
      # Update graph attributes with dummy variables
      igraph::V(graph)$name <- gsub(v, newNodeNames[1], igraph::V(graph)$name)
      igraph::V(graph)$intVars <- sapply(igraph::V(graph)$intVars, function(x) if (!is.null(x)) gsub(v, newNodeNames[1], x)) # Returns a list!
      igraph::E(graph)$source <- gsub(v, newNodeNames[1], igraph::E(graph)$source)
      igraph::E(graph)$target <- gsub(v, newNodeNames[1], igraph::E(graph)$target)
      if (!is.null(igraph::E(graph)$modVars)) {
        igraph::E(graph)$modVars <- sapply(igraph::E(graph)$modVars, function(x) if (!is.null(x)) gsub(v, newNodeNames[1], x)) # Returns a list!
      }
    }
    
    modelsContainer[[modelName]][["contrasts"]] <- createJaspState(contrastList)
    modelsContainer[[modelName]][["graph"]]$object <- graph
  }

  return(dataset)
}

.procAddLavModParNames <- function(jaspResults, options) {
  modelsContainer <- jaspResults[["modelsContainer"]]

  for (i in 1:length(options[["processModels"]])) {
    modelOptions <- options[["processModels"]][[i]]
    modelName <- modelOptions[["name"]]

    graph <- modelsContainer[[modelName]][["graph"]]$object
    if (inherits(graph, "try-error")) next

    if (is.null(modelsContainer[[modelName]][["syntax"]])) {
      graph <- .procAddLavModParNamesSingleModel(graph)
      modelsContainer[[modelName]][["graph"]]$object <- graph
    }
  }
}

.procAddLavModParNamesSingleModel <- function(graph) {
  igraph::E(graph)$parSymbol <- ifelse(
    # If source is mediator and target is global dependent -> b
    igraph::V(graph)[igraph::E(graph)$source]$isMed & igraph::V(graph)[igraph::E(graph)$target]$isDep, "b",
    # If source is mediator and target is not global dependent (i.e., other mediator) -> d
    ifelse(igraph::V(graph)[igraph::E(graph)$source]$isMed & !igraph::V(graph)[igraph::E(graph)$target]$isDep, "d",
      # If source is exogenous and target is mediator -> a; else -> c
      ifelse(igraph::V(graph)[igraph::E(graph)$source]$isExo & igraph::V(graph)[igraph::E(graph)$target]$isMed, "a", "c"))
  )
  # Get occurence of each symbol
  parSymbolTable <- table(igraph::E(graph)$parSymbol)
  # Enumerate each symbol as parIndex
  for (nm in names(parSymbolTable)) {
    igraph::E(graph)[parSymbol == nm]$parIndex <- 1:parSymbolTable[nm]
  }
  # Combine parSymbol and parIndex as parName
  igraph::E(graph)$parName <- paste0(igraph::E(graph)$parSymbol, igraph::E(graph)$parIndex)

  return(graph)
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

.procModProbes <- function(jaspResults, dataset, options) {
  modelsContainer <- jaspResults[["modelsContainer"]]

  for (i in 1:length(options[["processModels"]])) {
    modelOptions <- options[["processModels"]][[i]]
    modelName <- modelOptions[["name"]]

    if (inherits(modelsContainer[[modelName]][["graph"]]$object, "try-error")) next

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

.procModVarsFromGraph <- function(graph) {
  modVars <- list()

  for (vars in igraph::V(graph)$intVars) {
    for (v in vars[-1]) {
      modVars[[v]] <- c(modVars[[v]], v[1])
    }
  }
  
  return(modVars)
}

.procModProbesSingleModel <- function(container, dataset, options) {
  probeVals <- sapply(options[["moderationProbes"]], function(row) row[["probePercentile"]])/100

  graph <- container[["graph"]]$object
  # Get list of which moderators moderate which independent variables
  modVars <- .procModVarsFromGraph(graph)

  contrasts <- container[["contrasts"]]$object

  modProbes <- lapply(encodeColNames(names(modVars)), function(nms) {
    # Is moderator factor
    matchFac <- sapply(options[["factors"]], grepl, x = nms)

    if (length(matchFac) > 0 && any(matchFac)) { # If is factor
      whichFac <- options[["factors"]][matchFac]
      conMat <- contrasts[[whichFac]]
      colIdx <- which(paste0(whichFac, colnames(conMat)) == nms)
      row.names(conMat) <- as.character(conMat[, colIdx])
      # Return matrix with dummy coding for each factor
      return(conMat[, colIdx])
    }
    # If not factor return quantiles of continuous moderator
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

    if (
      inherits(modelsContainer[[modelName]][["graph"]]$object, "try-error")
    ) next

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
  graph <- container[["graph"]]$object
  
  regSyntax <- .procRegSyntax(graph)
  
  medEffectSyntax <- .procMedEffectsSyntax(graph,
    container[["modProbes"]]$object,
    container[["contrasts"]]$object
  )

  resCovSyntax <- .procResCovSyntax(
    container[["graph"]]$object,
    modelOptions[["independentCovariances"]],
    modelOptions[["mediatorCovariances"]]
  )

  headerJasp <- "
  # -------------------------------------------
  # Conditional process model generated by JASP
  # -------------------------------------------
  "

  headerResCov <- "
  # Residual covariances"

  headerMedEffects <- "
  # Mediation, indirect, and total effects"

  return(encodeColNames(paste(
    headerJasp,
    regSyntax,
    headerResCov,
    resCovSyntax,
    headerMedEffects,
    medEffectSyntax,
    sep = "\n")
  ))
}

.procRegSyntax <- function(graph) {
  # Concatenate graph to regression equations
  # e.g., target ~ source1 + source2 + ...
  uniqueTargets <- unique(igraph::E(graph)$target)
  regLines <- sapply(uniqueTargets, function(e) {
    rhs <- paste(
      igraph::E(graph)[.to(e)]$parName,
      igraph::E(graph)[.to(e)]$source,
      sep = "*", collapse = " + "
    )
    return(paste(e, rhs, sep = "~"))
  })
  return(paste(regLines, collapse = "\n"))
}

.procMedEffectsSyntax <- function(graph, modProbs, contrasts) {
  # Get all simple paths from X to Y
  medPaths <- igraph::all_simple_paths(graph,
    from = igraph::V(graph)[isTreat]$name,
    to = igraph::V(graph)[isDep]$name,
    mode = "out"
  )
  
  medEffects <- lapply(medPaths, function(path) {
    # Left hand side of lavaan syntax
    lhs <- paste(names(path), collapse = "__")
    
    # Get moderators on path
    modName <- unlist(Filter(Negate(is.null), sapply(2:length(path), function(i) {
      return(igraph::E(graph)[.from(names(path)[i-1]) & .to(names(path)[i])]$modVars)
    })))
    
    # Right hand side of lavaan syntax
    rhs <- lapply(2:length(path), function(i) {
      # Get edge from path[-1] to path[i]
      pathEdge <- igraph::E(graph)[.from(names(path)[i-1]) & .to(names(path)[i])]

      # If no moderators on edge, return only parName
      if(is.null(pathEdge$modVars[[1]])) return(pathEdge$parName)
      
      modPars <- lapply(pathEdge$modVars[[1]], function(v) { # If moderators 
        # Get edge for two way interaction between X and M
        twoWayEdge <- igraph::E(graph)[
          .from(paste(names(path)[i-1], v, sep = ":")) & 
          .to(names(path)[i])
        ]$parName

        # Concatenate two way edge parName with moderator probes
        return(paste(
          twoWayEdge,
          format(modProbs[[v]], digits = 3),
          sep = "*"
        ))
      })
      # Get all possible combinations of probes from different moderators
      modPars <- apply(expand.grid(modPars), 1, paste, collapse = "+")

      # Get name of potential three-way interaction
      threeWayInt <- paste(c(pathEdge$source, pathEdge$modVars[[1]]), collapse = "__")

      if (threeWayInt %in% igraph::E(graph)$source) { # If three-way int
        # Get edge of three way interaction X x M1 x M2
        threeWayEdge <- igraph::E(graph)[.from(threeWayInt) & .to(names(path)[i])]
        # Combine three way int parName with moderator probes
        threeWayModPars <- paste(
          threeWayEdge$parName,
          apply(
            expand.grid(lapply(pathEdge$modVars[[1]], function(v) format(modProbs[[v]], digits = 3))),
            1, paste, collapse = "*"
          ),
          sep = "*"
        )
        # Add to previous moderator probes
        modPars <- paste(modPars, threeWayModPars, sep = "+")
      } 
      
      # If indirect path add parentheses
      if (i > 1) {
        return(paste0("(", pathEdge$parName, " + ", modPars, ")"))
      }
      # Concanenate path edge parName with moderator probes
      return(paste(pathEdge$parName, modPars, sep = " + "))
    })
    # If indirect paths, multiply their steps
    rhs <- .doCallPaste(rhs, sep = "*")
    
    if (!is.null(modName) && length(modName) > 0) { # If path is moderated
      # Get combinations of moderators
      modName <- lapply(modName, function(v) {
        return(apply(
          expand.grid(v, gsub("\\%", "", names(modProbs[[v]]))), # Remove `%` from quantile name strings
          1, paste, collapse = "__"
        ))
      })
      # Add moderator combinations to left hand side
      lhs <- apply(expand.grid(lhs, apply(expand.grid(modName), 1, paste, collapse = ".")), 1, paste, collapse = ".")
    }

    return(list(lhs = lhs, rhs = rhs, modName = modName))
  })
  # All rhs effects
  totRhs <- lapply(medEffects, function(path) path$rhs)
  # All moderators of total effects
  totLhsMods <- lapply(medEffects, function(path) apply(expand.grid(path$modName), 1, paste, collapse = "."))
  # Add rhs effects
  totEffects <- .doCallPaste(totRhs, sep = " + ")
  # Concatenate rhs and lhs effects
  medEffectsLabeled <- unlist(lapply(medEffects, function(path) paste(path$lhs, path$rhs, sep = " := ")))
  # Concatenate total effects
  totEffectsLabeled <- paste(.pasteDot("tot", unlist(totLhsMods)), totEffects, sep = " := ")
  # Only return total effects when no indirect path
  if (length(medEffects) < 2) {
    return(paste(c(medEffectsLabeled, totEffectsLabeled), collapse = "\n"))
  }
  # Add rhs indirect effects
  totIndEffects <- .doCallPaste(totRhs[-1], sep = " + ")
  # Concatenate indirect effects
  totIndEffectsLabeled <- paste(.pasteDot("totInd", unlist(totLhsMods[-1])), totIndEffects, sep = " := ")

  return(paste(c(medEffectsLabeled, totEffectsLabeled, totIndEffectsLabeled), collapse = "\n"))
}

.procCombVars <- function(graph, vars) {
  graph <- igraph::add_vertices(graph, length(vars), name = vars)
  edgesToAdd <- as.vector(combn(vars, 2))
  graph <- igraph::add_edges(graph,
    edges = edgesToAdd,
    source = edgesToAdd[seq(1, length(edgesToAdd), 2)],
    target = edgesToAdd[seq(2, length(edgesToAdd), 2)]
  )
  return(graph)
}

.procResCovSyntax <- function(graph, includeExo, includeMed) {
  # Create new graph for covariances because we don't care about direction
  resCovGraph <- igraph::make_empty_graph()

  # Get all exogenous vars that are not interaction terms
  exoVars <- igraph::V(graph)[isExo & !isInt]$name

  if (length(exoVars) > 1 && includeExo) {
    # Add vertices and edges for all combinations
    resCovGraph <- .procCombVars(resCovGraph, exoVars)
  }

  # Get all mediator vars that are not interaction terms
  medVars <- igraph::V(graph)[isMed & !isInt]$name

  if (length(medVars) > 1 && includeMed) {
    # Add vertices and edges for all combinations
    resCovGraph <- .procCombVars(resCovGraph, medVars)
  }

  # Concatenate covariances: source ~~ target
  return(paste(igraph::E(resCovGraph)$source, igraph::E(resCovGraph)$target, sep = " ~~ ", collapse = "\n"))
}

# Results functions ----
.procCheckFitModel <- function(graph) {
  if (inherits(graph, "try-error")) return(FALSE)
  return(all(sapply(igraph::V(graph)$intVars, function(v) {
    return(all(sapply(v, .procCheckRegListVars)))
  })))
}

.procResultsFitModel <- function(container, dataset, options) {
  # Should model be fitted?
  doFit <- .procCheckFitModel(container[["graph"]]$object)

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
    if (!is.null(mod) && !is.character(mod) && mod@Fit@converged)
      return(mod@Options[["do.fit"]])
    return(FALSE)
  })

  return(procResults[isFitted])
}

.procIsModelNumberGraph <- function(modelNumber, graph, modelOptions, globalDependent) {
  # Create regList from hard-coded model
  modelNumberGraph <- try(.procProcessRelationshipsToGraph(.procGetHardCodedModel(modelNumber, length(modelOptions[["modelNumberMediators"]]))))

  if (inherits(modelNumberGraph, "try-error")) return(FALSE)

  # Replace dummy variables in regList
  modelNumberGraph <- .procModelGraphInputModelNumber(modelNumberGraph, modelOptions, globalDependent)
  
  # Check if user graph and hard-coded graph are isomorphic
  # (same number of edges and vertices and edge connectivity)
  isIdentical <- igraph::is_isomorphic_to(modelNumberGraph, graph)

  return(isIdentical)
}

.procRecognizeModelNumber <- function(graph) {
  # Get global dependent variable
  globalDependent <- igraph::V(graph)[isDep]$name
  # Get mediator terms
  mediators   <- igraph::V(graph)[isMed]$name
  # Get moderator terms by splitting interaction terms
  moderators  <- unique(unlist(sapply(igraph::V(graph)[isInt]$intVars, function(v) v[length(v)])))
  # First moderator is W
  modW        <- if (length(moderators) > 0)  moderators[1] else ""
  # Second moderator is Z
  modZ        <- if (length(moderators) > 1)  moderators[2] else ""
  # First treatment term is independent
  independent <- igraph::V(graph)[isTreat]$name[1]
  
  # Create model options dummy object
  modelOptions <- list(
    modelNumberIndependent = independent,
    modelNumberMediators = mediators,
    modelNumberModeratorW = modW,
    modelNumberModeratorZ = modZ
  )

  # Check which hard-coded model matches user model
  modelMatch <- sapply(.procHardCodedModelNumbers(), .procIsModelNumberGraph, graph = graph, modelOptions = modelOptions, globalDependent = globalDependent)
  
  # If no match swap W and Z and check again
  if (!any(modelMatch)) {
    modelOptions[["modelNumberModeratorW"]] <- modZ
    modelOptions[["modelNumberModeratorZ"]] <- modW

    modelMatch <- sapply(.procHardCodedModelNumbers(), .procIsModelNumberGraph, graph = graph, modelOptions = modelOptions, globalDependent = globalDependent)
  }

  return(.procHardCodedModelNumbers()[modelMatch])
}

.procModelSummaryTable <- function(jaspResults, options, modelsContainer) {
  if (!is.null(jaspResults[["modelSummaryTable"]])) return()

  summaryTable <- createJaspTable(title = gettext("Model summary"))
  summaryTable$dependOn(c(.procGetDependencies(), "processModels", "aicWeights", "bicWeights", "naAction"))
  summaryTable$position <- 1

  procResults <- lapply(options[["processModels"]], function(mod) modelsContainer[[mod[["name"]]]][["fittedModel"]]$object)
  
  converged <- sapply(procResults, function(mod) mod@Fit@converged)

  if (!all(converged)) {
    summaryTable$addFootnote(message = gettext("At least one model did not converge."))
  }

  procResults <- .procFilterFittedModels(procResults)

  if (options[["naAction"]] == "fiml" && !options[["estimator"]] %in% c("default", "ml")) {
    summaryTable$setError("Full Information Maximum Likelihood estimation only available with 'ML' or 'Auto' estimators. Please choose a different estimator or option for missing value handling.")
  }

  summaryTable$addColumnInfo(name = "Model",        title = "",                         type = "string" )
  summaryTable$addColumnInfo(name = "modelNumber",  title = "Hayes model number",       type = "integer" )
  summaryTable$addColumnInfo(name = "AIC",          title = gettext("AIC"),             type = "number" )
  if (options[["aicWeights"]]) {
    summaryTable$addColumnInfo(name = "wAIC",       title = gettext("AIC weight"),      type = "number")
  }
  summaryTable$addColumnInfo(name = "BIC",          title = gettext("BIC"),             type = "number" )
  if (options[["bicWeights"]]) {
    summaryTable$addColumnInfo(name = "wBIC",       title = gettext("BIC weight"),      type = "number")
  }
  summaryTable$addColumnInfo(name = "logLik",       title = gettext("Log-likelihood"),  type = "number" )
  summaryTable$addColumnInfo(name = "N",            title = gettext("n"),               type = "integer")
  summaryTable$addColumnInfo(name = "Df",           title = gettext("df"),              type = "integer")

  jaspResults[["modelSummaryTable"]] <- summaryTable

  isInvalid <- sapply(procResults, is.character)

  if (any(isInvalid)) {
    errmsg <- gettextf("Model fit could not be assessed because one or more models were not estimated: %s", names(procResults)[isInvalid])
    summaryTable$setError(errmsg)
  }

  modelNames <- sapply(options[["processModels"]], function(mod) mod[["name"]])

  modelNumbers <- lapply(options[["processModels"]], function(mod) {
    if (mod[["inputType"]] == "inputModelNumber") {
      if (!mod[["modelNumber"]] %in% .procHardCodedModelNumbers()) {
        modelNumberTable$setError(gettextf("Hayes model number %s for %s not implemented", mod[["modelNumber"]], mod[["name"]]))
      }
      return(mod[["modelNumber"]])
    }

    return(.procRecognizeModelNumber(modelsContainer[[mod[["name"]]]][["graph"]]$object))
  })

  summaryTable[["Model"]]       <- modelNames
  summaryTable[["modelNumber"]] <- modelNumbers

  if (length(procResults) == 0) return()

  aic <- sapply(procResults, AIC)
  bic <- sapply(procResults, BIC)
  df  <- sapply(procResults, lavaan::fitMeasures, fit.measures = "df")

  summaryTable[["AIC"]]      <- aic
  summaryTable[["BIC"]]      <- bic
  summaryTable[["logLik"]]   <- sapply(procResults, lavaan::logLik)
  summaryTable[["N"]]        <- sapply(procResults, lavaan::lavInspect, what = "nobs")
  summaryTable[["Df"]]       <- df

  if (options[["aicWeights"]]) {
    summaryTable[["wAIC"]] <- .computeWeights(aic)
  }
  if (options[["bicWeights"]]) {
    summaryTable[["wBIC"]] <- .computeWeights(bic)
  }

  if (any(df == 0)) {
    summaryTable$addFootnote(message = gettext("At least one model is saturated (df = 0)."))
  }

  if (options$estimator %in% c("dwls", "gls", "wls", "uls")) {
    summaryTable$addFootnote(message = gettext("The AIC, BIC and additional information criteria are only available with ML-type estimators."))
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
      next
    }
  
    if (!procResults[[i]]@Options[["do.fit"]]) {
      next
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

.procContainerLocalTests <- function(jaspResults, options) {
  if (!is.null(jaspResults[["localTestContainer"]])) {
    localTestContainer <- jaspResults[["localTestContainer"]]
  } else {
    localTestContainer <- createJaspContainer(gettext("Local tests"))
    localTestContainer$dependOn(.procGetDependencies())

    jaspResults[["localTestContainer"]] <- localTestContainer
  }

  return(localTestContainer)
}

.procLocalTestTables <- function(container, dataset, options, modelsContainer) {
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
    if (options[["processModels"]][[i]][["localTests"]])
      .procLocalTestTable(modelContainer, dataset, options, procResults[[i]], i)
  }
}

.procLocalTestTable <- function(container, dataset, options, procResults, modelIdx) {
  if (!is.null(container[["localTestTable"]])) return()

  localTestTable <- createJaspTable(title = gettext("Conditional independence tests"))
  localTestTable$dependOn(
    c("ciLevel", "localTestCorrectionForAllModels"),
    nestedOptions = list(
      c("processModels", as.character(modelIdx), "localTests"),
      c("processModels", as.character(modelIdx), "localTestType"),
      c("processModels", as.character(modelIdx), "localTestBootstrap"),
      c("processModels", as.character(modelIdx), "localTestBootstrapSamples")
    )
  )
  container[["localTestTable"]] <- localTestTable

  if (container$getError()) return()

  if (!procResults@Fit@converged) {
    localTestTable$addFootnote(gettext("Model did not converge."))
  }

  testType <- options[["processModels"]][[modelIdx]][["localTestType"]]

  localTestTable$addColumnInfo(name = "lhs",  title = "", type = "string")
  localTestTable$addColumnInfo(name = "op1",  title = "", type = "string")
  localTestTable$addColumnInfo(name = "rhs",  title = "", type = "string")
  localTestTable$addColumnInfo(name = "op2",  title = "", type = "string")
  localTestTable$addColumnInfo(name = "cond", title = "", type = "string")

  keyEst <- "estimate"

  if (testType == "cis.chisq") {
    keyEst <- "rmsea"
    localTestTable$addColumnInfo(name = keyEst, title = gettext("RMSEA"),         type = "number", format = "sf:4;dp:3" )
    localTestTable$addColumnInfo(name = "x2",   title = gettext("&#967;&sup2;"),  type = "number", format = "sf:4;dp:3" )
    localTestTable$addColumnInfo(name = "df",   title = gettext("df"),            type = "integer"                      )
  } else {
    localTestTable$addColumnInfo(name = keyEst, title = gettext("Estimate"),      type = "number", format = "sf:4;dp:3" )
  }

  adjustMethod <- options[["localTestCorrectionForAllModels"]]

  if (adjustMethod != "none") {
    adjustMethodRef <- list(
      "holm" = "Holm",
      "hochberg" = "Hochberg",
      "hommel" = "Hommel",
      "bonferroni" = "Bonferroni",
      "BH" = "Benjamini-Hochberg",
      "BY" = "Benjamini-Yekuteli"
    )
    localTestTable$addFootnote(gettextf("<em>p</em>-values adjusted for multiple comparisons with the %s method.", adjustMethodRef[[adjustMethod]]))
  }

  nReps  <- NULL
  keyErr <- "p.value"

  if (options[["processModels"]][[modelIdx]][["localTestBootstrap"]] || testType == "cis.loess") {
    keyErr <- "std.error"
    adjustMethod <- "none"
    nReps <- options[["processModels"]][[modelIdx]][["localTestBootstrapSamples"]]
    localTestTable$addColumnInfo(name = keyErr,   title = gettext("Std. error"),   type = "number", format = "sf:4;dp:3")
  } else {
    localTestTable$addColumnInfo(name = keyErr,   title = gettext("p"),          type = "number", format = "dp:3;p:.001")
  }

  localTestTable$addColumnInfo(name = "ci.lower", title = gettext("Lower"),      type = "number", format = "sf:4;dp:3",
                    overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
  localTestTable$addColumnInfo(name = "ci.upper", title = gettext("Upper"),      type = "number", format = "sf:4;dp:3",
                    overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))

  if (testType == "cis" && any(sapply(dataset, is.factor))) {
    localTestTable$setError(gettext("Linear test type cannot be applied to factor variables. Choose a different test type or remove all factor variables from the model."))
    return()
  }

  parTable <- lavaan::parTable(procResults)
  # Only include free parameters in DAG
  parTable <- parTable[parTable$op != ":=" & parTable$free > 0 & !grepl(":|__", parTable$rhs) & !grepl(":|__", parTable$lhs),]

  arrows <- apply(parTable, 1, function(row) {
    op <- switch(row[["op"]],
      "~" = " <- ",
      "~~" = " <-> "
    )
    return(paste(row[["lhs"]], op, row[["rhs"]]))
  })

  parNamesAbbr <- abbreviate(unique(unlist(parTable[c("lhs", "rhs")])))

  graph <- dagitty::dagitty(paste("dag {", paste(arrows, collapse = "\n")," } "))

  localTestResult <- dagitty::localTests(
    graph, dataset,
    type = testType,
    conf.level = options$ciLevel,
    R = nReps
  )

  if (nrow(localTestResult) > 0) {
    implicationSplit <- strsplit(row.names(localTestResult), "\\s+(\\|+|_+\\|+_+)\\s+")
    localTestTable[["lhs"]]  <- sapply(implicationSplit, function(row) names(parNamesAbbr)[parNamesAbbr == row[1]])
    localTestTable[["op1"]]  <- rep("\u2AEB", length(implicationSplit))
    localTestTable[["rhs"]]  <- sapply(implicationSplit, function(row) names(parNamesAbbr)[parNamesAbbr == row[2]])
    localTestTable[["cond"]] <- sapply(implicationSplit, function(row) { if (length(row) == 3) names(parNamesAbbr)[parNamesAbbr == row[3]] else "" })
    localTestTable[["op2"]]  <- sapply(implicationSplit, function(row) { if (length(row) == 3) "\u2223" else "" })
    localTestTable[[keyEst]] <- localTestResult[[keyEst]]
    localTestTable[[keyErr]] <- p.adjust(localTestResult[[keyErr]], method = adjustMethod)
    if (testType == "cis.chisq") {
      localTestTable[["x2"]] <- localTestResult[["x2"]]
      localTestTable[["df"]] <- localTestResult[["df"]]
      localTestTable[["ci.lower"]] <- localTestResult[[paste0("rmsea ", ((1-options$ciLevel)/2)*100, "%")]]
      localTestTable[["ci.upper"]] <- localTestResult[[paste0("rmsea ", (1-(1-options$ciLevel)/2)*100, "%")]]
    } else {
      localTestTable[["ci.lower"]] <- localTestResult[[paste0(((1-options$ciLevel)/2)*100, "%")]]
      localTestTable[["ci.upper"]] <- localTestResult[[paste0((1-(1-options$ciLevel)/2)*100, "%")]]
    }
  } else {
    localTestTable$setError(gettext("The specified model does not imply any (conditional) independencies that can be tested."))
  
  }
}

# Plotting functions ----

.procConceptPathPlot <- function(container, options, procResults, modelIdx) {
  if (!is.null(container[["conceptPathPlot"]])) return()

  procPathPlot <- createJaspPlot(title = gettext("Conceptual path plot"), height = 320, width = 480)
  procPathPlot$dependOn(
    options = c("pathPlotsLegendLabels", "pathPlotsLegendColor", "pathPlotsLabelLength", "pathPlotsColor", "pathPlotsColorPalette"),
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
    options = c("statisticalPathPlotsParameterEstimates", "pathPlotsLegendLabels", "pathPlotsLegendColor", "pathPlotsLabelLength", "pathPlotsColor", "pathPlotsColorPalette"),
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

  # Get terms
  mediators <- paths[paths[, 1] %in% paths[, 2], 1]
  independent <- paths[!paths[, 1] %in% paths[, 2], 1]
  dependent <- paths[!paths[, 2] %in% paths[, 1], 2]

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
  } else {
    # Paths from moderator and interaction term to dep var node
    modPaths <- paths[isIntPath | paths[, 1] %in% mods, , drop = FALSE]
  }

  # Filter out non-moderation paths -> main paths
  mainPaths <- paths[!isIntPath & !paths[, 1] %in% mods[!mods %in% paths[, 2]], , drop = FALSE]

  # Get layout of main paths: matrix with x,y coordinates for each node
  layout <- .procMainGraphLayout(mainPaths[, 1:2, drop = FALSE], options[["dependent"]])

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

  # Create edge list from paths
  graph <- igraph::graph_from_edgelist(mainPaths[, 1:2, drop = FALSE])

  # Order of node labels as in graph
  nodeNames <- igraph::vertex.attributes(graph)[["name"]]

  # Get idx of hidden helper node (to make it invisible)
  graphIntIdx <- grepl("i[[:digit:]]", nodeNames)

  # Make hidden helper node invisible step 2
  nodeLabels <- decodeColNames(nodeNames)
  nodeLabels[graphIntIdx] <- ""

  # Create abbreviated node labels to plot in nodes
  nodeLabelsAbbr <- abbreviate(
    .procDecodeVarNames(nodeLabels),
    minlength = options[["pathPlotsLabelLength"]]
  )

  # Get edge labels for statistical plot
  if (type == "conceptual") {
    edgeLabels <- ""
  } else {
    edgeLabels <- mainPaths[, 3]

    if (estimates) edgeLabels <- round(as.numeric(edgeLabels), 3)
  }

  # Node size (scales with number of nodes automatically)
  nodeSize <- 0.625

  # Create variable for margin around edge ends (no margin for helper nodes)
  endCaps <- rep(ggraph::square(nodeSize, unit = "native"), nrow(mainPaths))
  endCaps[grepl("i[[:digit:]]", mainPaths[, 2])] <- ggraph::square(0, unit = "native")

  # Create visibility variable to make helper nodes transparent
  nodeVis <- rep(0, length(nodeLabels))
  nodeVis[nodeLabels == ""] = 1

  # Create dummy alpha variable for nodes (nessecary for creating the legend)
  nodeAlpha <- if (options[["pathPlotsLegendLabels"]]) nodeLabels else NULL

  # Create node type variable for coloring
  nodeType <- as.factor(ifelse(nodeLabels %in% decodeColNames(mediators), "Mediator",
    ifelse(nodeLabels %in% decodeColNames(mods) | grepl(":", nodeLabels), "Moderator",
      ifelse(nodeLabels %in% decodeColNames(independent), "Independent",
        ifelse(nodeLabels %in% decodeColNames(dependent), "Dependent", NA)
      )
    )
  ))

  # Create function from color palette
  colorFun <- jaspGraphs::JASPcolors(options[["pathPlotsColorPalette"]], asFunction = TRUE)

  if (options[["pathPlotsColor"]]) {
    if (type == "conceptual" && sum(isIntPath) > 0) {
      # Make helper nodes transparent
      colorPalette <- c(colorFun(length(unique(nodeType))-1), "transparent")
    } else {
      colorPalette <- colorFun(length(unique(nodeType)))
    }
  } else {
    nodeType <- NULL
    colorPalette <- rep("transparent", length(unique(nodeType)))
  }

  # Sort layout according to order of nodes in graph
  layout <- layout[match(nodeNames, rownames(layout)), ]

  # Scale x-axis to 4/3 (x/y) ratio of y-axis to make plot wider
  layout[, 1] <- (layout[, 1]) * (max(layout[, 2]) - min(layout[, 2])) / (max(layout[, 1]) - min(layout[, 1]))

  p <- ggraph::ggraph(
      graph,
      layout = layout
    ) +
    # Add square nodes
    ggraph::geom_node_tile(
      ggplot2::aes(color = as.factor(nodeVis), fill = nodeType),
      height = nodeSize,
      width = nodeSize,
      size = 0.9 # Width of border
    ) +
    # Add edges between nodes
    ggraph::geom_edge_link(
      ggplot2::aes(
        label = edgeLabels,
        end_cap = endCaps
      ),
      edge_width = 0.9,
      color = "black",
      arrow = ggplot2::arrow(length = grid::unit(0.05, "native")),
      start_cap = ggraph::square(nodeSize, unit = "native"), # Arrow start has always margin
      angle_calc = "along",
      label_dodge = grid::unit(0.025, "native")
    ) +
    # Add abbreviated node lables with dummy alpha variable to display them in legend
    ggraph::geom_node_text(
      ggplot2::aes(label = nodeLabelsAbbr, alpha = nodeAlpha),
      size = 30/(sum(!graphIntIdx) + options[["pathPlotsLabelLength"]] - 3)
    ) +
    # Make helper nodes transparent and hide color from legend
    ggplot2::scale_color_manual(values = c("black", "transparent"), guide = NULL) +
    ggplot2::scale_fill_manual(
      values = colorPalette,
      # Remove helper node fill color (transparent) from legend
      na.translate = FALSE,
      guide = if (options[["pathPlotsLegendColor"]]) ggplot2::guide_legend(
        title = NULL
      ) else NULL
    )
  
  globalLabelSize <- 16

  if (options[["pathPlotsLegendLabels"]]) {
    nodeLabelUnique <- unique(nodeLabels)
    nodeLabelUnique[nodeLabelUnique == ""] <- NA
    nodeLabelUniqueSorted <- sort(nodeLabelUnique, index.return = TRUE)
    # Add legend for label abbreviations by manually overiding dummy alpha variable
    p <- p + ggplot2::scale_alpha_manual(
      name = "",
      na.translate = FALSE,
      # Make all labels fully visible
      values = rep(1, length(nodeLabelUnique)),
      limits = .procDecodeVarNames(sort(nodeLabelUnique)),
      guide = ggplot2::guide_legend(
        # Sort abbreviated labels according to sort index of full labels
        override.aes = list(
          label = unique(nodeLabelsAbbr)[nodeLabelUniqueSorted[["ix"]]],
          # Make legend labels same size as legend text
          size = globalLabelSize/ggplot2::.pt
        )
      )
    )
  }

  p <- p +
    ggplot2::coord_fixed() +
    jaspGraphs::getEmptyTheme() +
    ggplot2::theme(
      # Remove grey background in legend key
      legend.key = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(
        size = globalLabelSize,
        # Increase spacing dynamically between legend key and labels
        margin = ggplot2::margin(0, 0, 0, 2*options[["pathPlotsLabelLength"]])
      )
    )

  return(p)
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
