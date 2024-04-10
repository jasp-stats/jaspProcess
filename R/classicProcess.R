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
  # Check if analysis has any variables to read in
  if (options[["dependent"]] == "" || (length(options[["covariates"]]) == 0 && length(options[["factors"]]) == 0)) {
    # create empty summary table
    .procModelSummaryTable(jaspResults, options, NULL)
    return()
  }
  # Read dataset
  dataset <- .procReadData(options)
  # Check for errors in dataset
  .procErrorHandling(dataset, options)
  # Create a container for each model
  modelsContainer <- .procContainerModels(jaspResults, options)
  # Check if all models are ready to compute something
  if (.procIsReady(options)) {
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
    modelsContainer <- .procComputeResults(jaspResults, dataset, options)
  }
  # Create container for path plots for each model
  pathPlotContainer <- .procContainerPathPlots(jaspResults, options)
  # Create path plots for each model and add to container
  .procPathPlots(pathPlotContainer, options, modelsContainer)
  # Create table with model fit indices (AIC, ...)
  .procModelSummaryTable(jaspResults, options, modelsContainer)
  # Create R² table if requested
  .procRsquared(jaspResults, options, modelsContainer)
  # Create container for parameter estimates for each model
  parEstContainer <- .procContainerParameterEstimates(jaspResults, options, modelsContainer)
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
  return(c(
    "dependent", "covariates", "factors", "naAction", "emulation", "estimator", "meanCenteredModeration",
    "standardizedModelEstimates", "errorCalculationMethod", "bootstrapCiType"
  ))
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
  if(is.null(jaspResults[["modelsContainer"]])) {
    modelsContainer <- createJaspContainer()
  } else {
    modelsContainer <- jaspResults[["modelsContainer"]]
  }

  for (i in 1:length(options[["processModels"]])) {
    modelOptions <- options[["processModels"]][[i]]
    modelName <- modelOptions[["name"]]
    if (is.null(modelsContainer[[modelName]])) {
      container <- createJaspContainer(title = modelName)
      container$dependOn(
        options = .procGetDependencies(),
        nestedOptions = .procGetSingleModelsDependencies(as.character(i))[-1] # omit model name
      )
      modelsContainer[[modelName]] <- container
    }
  }

  jaspResults[["modelsContainer"]] <- modelsContainer

  return(modelsContainer)
}

.procModelGraph <- function(jaspResults, options) {
  modelsContainer <- jaspResults[["modelsContainer"]]

  for (i in 1:length(options[["processModels"]])) {
    modelOptions <- options[["processModels"]][[i]]
    modelName <- modelOptions[["name"]]

    if (is.null(modelsContainer[[modelName]][["graph"]])) {
      graph <- try(.procModelGraphSingleModel(options[["processModels"]][[i]], globalDependent = options[["dependent"]]))
      state <- createJaspState(object = graph)
      modelsContainer[[modelName]][["graph"]] <- state
    }
  }
}

.procModelGraphSingleModel <- function(modelOptions, globalDependent, options) {
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
    dependent <- encodeColNames(path[["processDependent"]])
    independent <- encodeColNames(path[["processIndependent"]])
    type <- encodeColNames(path[["processType"]])
    processVariable <- encodeColNames(path[["processVariable"]])

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
      # EDIT: Two-way interactions are now also added as product variables, e.g., var1__var2 because this is necessary
      # mean-centering before entering the analysis and specifying the covariance structure

      # Get all existing interaction terms
      sourceVars <- igraph::E(graph)[.to(dependent)]$source
      isInt <- grepl("__", sourceVars)
      if (any(isInt)) {
        # Split interaction terms
        varsSplit <- strsplit(sourceVars, "__")
        for (v in varsSplit[isInt]) {
          # If the second term of the interaction is the independent of current path
          # this indicates a three-way interaction with the independent variable
          if (v[length(v)] == independent) {
            # Create three-way interaction variable name: independent x moderator1 x moderator2
            interVarThree <- paste0(paste(v, collapse = "__"), "__", processVariable)
            # Also add a two-way interaction moderator1 x moderator2,
            # otherwise model might be underspecified
            interVarTwo <- paste0(v[1], "__", processVariable)
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
      interVar <- paste0(independent, "__", processVariable)

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
  igraph::V(graph)$isInt <- grepl("__", igraph::V(graph)$name)
  # Which are the node interaction vars
  igraph::V(graph)$intVars <- strsplit(igraph::V(graph)$name, "__")
  # How many interaction vars are there
  igraph::V(graph)$intLength <- sapply(igraph::V(graph)$intVars, length)
  # Is nested interaction term (fully included in higher-order interaction)
  igraph::V(graph)$isNestedInt <- igraph::V(graph)$isInt & sapply(igraph::V(graph)$intVars, function(vars1) {
    return(any(sapply(igraph::V(graph)$intVars, function(vars2) {
      # If interaction is fully included in higher-order interaction
      return(length(vars1) < length(vars2) && all(vars1 %in% vars2))
    })))
  })
  # Is not not nested interaction term
  igraph::V(graph)$isHigherOrderInt <- igraph::V(graph)$isInt & !igraph::V(graph)$isNestedInt
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
  igraph::E(graph)$modVars <- NA

  for (i in 1:length(igraph::E(graph))) {
    sourceNode <- igraph::V(graph)[igraph::E(graph)$source[i]]

    if (sourceNode$isInt) {
      sourceNodeIntVars <- unlist(sourceNode$intVars)

      for (v in sourceNodeIntVars) {
        # Set all edges from var interacting with sourceNode to target as isMod
        igraph::E(graph)[source == v & target == igraph::E(graph)$target[i]]$isMod <- TRUE
        # Store unique moderating variables
        if (length(sourceNodeIntVars[sourceNodeIntVars != v]) > 0) {
          if (any(is.na(igraph::E(graph)[source == v & target == igraph::E(graph)$target[i]]$modVars))) {
            igraph::E(graph)[source == v & target == igraph::E(graph)$target[i]]$modVars <- sourceNodeIntVars[sourceNodeIntVars != v]
          } else {
            igraph::E(graph)[source == v & target == igraph::E(graph)$target[i]]$modVars <- list(
              unique(c(
                igraph::E(graph)[source == v & target == igraph::E(graph)$target[i]]$modVars[[1]],
                sourceNodeIntVars[sourceNodeIntVars != v]
              ))
            )
          }
        }
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

  return(encodeColNames(vars))
}

.procModelGraphInputModelNumber <- function(graph, modelOptions, globalDependent) {
  # Replace dummy vars in graph attributes
  igraph::V(graph)$name <- .procReplaceDummyVars(igraph::V(graph)$name, modelOptions = modelOptions, globalDependent = globalDependent)
  igraph::E(graph)$source <- .procReplaceDummyVars(igraph::E(graph)$source, modelOptions = modelOptions, globalDependent = globalDependent)
  igraph::E(graph)$target <- .procReplaceDummyVars(igraph::E(graph)$target, modelOptions = modelOptions, globalDependent = globalDependent)
  
  if (!is.null(igraph::E(graph)$modVars))
    igraph::E(graph)$modVars <- sapply(igraph::E(graph)$modVars, function(x) if (!is.null(x)) .procReplaceDummyVars(x, modelOptions = modelOptions, globalDependent = globalDependent)) # Returns a list!
  
  igraph::V(graph)$intVars <- sapply(igraph::V(graph)$intVars, .procReplaceDummyVars, modelOptions = modelOptions, globalDependent = globalDependent)

  if (any(igraph::V(graph)$isInt)) {
    igraph::V(graph)[isInt]$name <- unlist(sapply(igraph::V(graph)[isInt]$intVars, paste, collapse = "__"))
  }
  
  return(graph)
}

.procReadData <- function(options) {
  # Read in selected variables from dataset
  dataset <- .readDataSetToEnd(
    columns = c(options[['dependent']], options[['covariates']]),
    columns.as.factor = options[['factors']]
  )

  return(dataset)
}

.procAddFactorDummyIntVars <- function(jaspResults, dataset, options) {
  modelsContainer <- jaspResults[["modelsContainer"]]

  for (i in 1:length(options[["processModels"]])) {
    modelOptions <- options[["processModels"]][[i]]
    modelName <- modelOptions[["name"]]
    graph <- modelsContainer[[modelName]][["graph"]]$object

    if (!.procCheckFitModel(graph)) next

    # Get all predictor vars
    sourceVars <- unique(igraph::E(graph)$source)

    # Create new (dummy-coded) variables for two and three-way interactions (moderated moderation)
    for (v in igraph::V(graph)[intLength >= 2]$intVars) {
      # Compute factor dummy variables involved in interaction
      varFormula <- formula(paste("~", paste(v, collapse = "*")))
      # Create model matrix but keep missing values in place because it needs to be merged with dataset later
      varDummyMat <- model.matrix(varFormula, data = model.frame(dataset, na.action = NULL))
      # Rename col names to match syntax
      colnames(varDummyMat) <- gsub(":", "__", colnames(varDummyMat))
      # Add interaction variables to dataset
      dataset <- cbind(dataset, varDummyMat[, -1])
      # Remove old interaction term
      sourceVars <- sourceVars[sourceVars != paste(v, collapse = "__")]
    }

    # Convert regression variables to formula
    sourceFormula <- formula(paste("~", paste(sourceVars, collapse = "+")))

    # Create dummy variables for factors, again keep missing values in place
    sourceDummyMat <- model.matrix(sourceFormula, data = model.frame(dataset, na.action = NULL))
    
    # Add dummy variables to dataset
    dataset <- cbind(dataset, sourceDummyMat)

    # Get dummy coding for contrasts
    contrasts <- attr(sourceDummyMat, "contrasts")

    contrastList <- list()

    for (f in names(contrasts)) {
      contrastList[[f]] <- do.call(contrasts[[f]], list(levels(as.factor(dataset[[f]]))))
    }

    # We need to make a new graph, otherwise igraph messes up the order of nodes
    facGraph <- igraph::make_empty_graph()

    # Decode names to match with graph node names (FIXME)
    names(contrastList) <- names(contrastList)
    
    # Split terms of predictor vars
    sourcVarsSplit <- strsplit(unique(igraph::E(graph)$source), "__")

    # Replace dummy-coded variables in graph
    # Goes through all source variables and replaces them with dummy coded variable names
    # if necessary
    for (vars in sourcVarsSplit) {
      # Concatenate variable with factor levels from contrast list to get dummy variable name
      contr <- lapply(vars, function(v) {
        if (v %in% names(contrastList)) {
          return(paste0(v, colnames(contrastList[[v]])))
        } else {
          return(v)
        }
      })
  
      # Concatenate source var and interaction terms in dummy coded variable
      contr <- .doCallPaste(contr, sep = "__")
      sourceName <- paste(vars, collapse = "__")
      

      # Add nodes for source variables not in graph
      contrNotInGraph <- contr[!contr %in% igraph::V(facGraph)$name]

      if (length(contrNotInGraph) > 0) {
        facGraph <- igraph::add_vertices(facGraph, length(contrNotInGraph), name = contrNotInGraph)
      }

      # Get target variable
      target <- igraph::E(graph)[.from(sourceName)]$target

      # Add nodes for targets not in graph
      targetNotInGraph <- target[!target %in% igraph::V(facGraph)$name]

      if (length(targetNotInGraph) > 0) {
        facGraph <- igraph::add_vertices(facGraph, length(targetNotInGraph), name = targetNotInGraph)
      }

      # Add edges between source and target variables
      for (t in target) {
        facGraph <- igraph::add_edges(facGraph,
          edges = as.vector(rbind(contr, t)),
          source = contr,
          target = t
        )
      }
    }

    # Add attributes to graph
    facGraph <- .procGraphAddAttributes(facGraph)

    modelsContainer[[modelName]][["contrasts"]] <- createJaspState(contrastList)
    modelsContainer[[modelName]][["graph"]]$object <- facGraph
  }

  return(dataset)
}

.procGraphAddParNames <- function(jaspResults, options) {
  modelsContainer <- jaspResults[["modelsContainer"]]

  for (i in 1:length(options[["processModels"]])) {
    modelOptions <- options[["processModels"]][[i]]
    modelName <- modelOptions[["name"]]

    graph <- modelsContainer[[modelName]][["graph"]]$object
    
    if (!.procCheckGraph(graph)) next

    if (is.null(modelsContainer[[modelName]][["syntax"]])) {
      graph <- .procGraphAddParNamesSingleModel(graph)
      modelsContainer[[modelName]][["graph"]]$object <- graph
    }
  }
}

.procGraphAddParNamesSingleModel <- function(graph) {
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
  .hasErrors(dataset, "run",
    type = c('observations', 'variance', 'infinity'),
    observations.target = c(
      options[["dependent"]],
      options[["covariates"]],
      options[["factors"]]
    ),
    observations.amount = '< 2',
    variance.target = c(
      options[["dependent"]],
      options[["covariates"]]
    ),
    infinity.target = c(
      options[["dependent"]],
      options[["covariates"]]
    ),
    exitAnalysisIfErrors = TRUE
  )
  if (length(options[["covariates"]]) > 1) {
    .hasErrors(dataset, "run",
      type = "varCovData",
      varCovData.target = options[["covariates"]],
      varCovData.corFun = stats::cov,
      exitAnalysisIfErrors = TRUE
    )
  }
}

.procMeanCenter <- function(graph, dataset, options) {
  # Center interacting variables
  if (options$meanCenteredModeration) {
    # Only use complete cases for standardization with listwise deletion
    # Otherwise data entered into lavaan is not actually standardized
    if (options$naAction == "listwise") {
      isComplete <- complete.cases(dataset)
    } else {
      isComplete <- !logical(nrow(dataset))
    }
    
    for (v in unique(unlist(igraph::V(graph)[intLength > 1]$intVars))) {
      # Only center continuous variables
      if (v %in% options[["covariates"]]) {
        dataset[[v]][isComplete] <- scale(dataset[[v]][isComplete], center = TRUE, scale = FALSE)
      }
    }

    # Recalculate two- and three-way interaction variables with centered terms
    for (v in grep("__", names(dataset), value = TRUE)) {
      vSplit <- strsplit(v, "__")[[1]]
      vCentered <- eval(str2lang(paste(vSplit, collapse = "*")), envir = dataset)
      dataset[[v]] <- vCentered
    }
  }

  return(dataset)
}

.procModProbes <- function(jaspResults, dataset, options) {
  modelsContainer <- jaspResults[["modelsContainer"]]

  for (i in 1:length(options[["processModels"]])) {
    modelOptions <- options[["processModels"]][[i]]
    modelName <- modelOptions[["name"]]

    if (!.procCheckGraph(modelsContainer[[modelName]][["graph"]]$object)) next

    if (is.null(modelsContainer[[modelName]][["modProbes"]])) {
      modProbes <- .procModProbesSingleModel(modelsContainer[[modelName]], dataset, options)
      state <- createJaspState(object = modProbes)
      modelsContainer[[modelName]][["modProbes"]] <- state
    }
  }
}

.procModProbesSingleModel <- function(container, dataset, options) {
  probeVals <- sapply(options[["moderationProbes"]], function(row) row[["probePercentile"]])/100

  graph <- container[["graph"]]$object
  # Get list of moderators
  modVars <- unique(unlist(lapply(igraph::V(graph)$intVars, function(vars) vars[-1])))

  contrasts <- container[["contrasts"]]$object

  # Mean-center data set before quantile computation
  dataset <- .procMeanCenter(container[["graph"]]$object, dataset, options)

  modProbes <- lapply(modVars, function(nms) {
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
    # If listwise missing value deletion, only use complete cases for quantiles
    if (options$naAction == "listwise") {
      isComplete <- complete.cases(dataset)
    } else {
      isComplete <- !logical(nrow(dataset))
    }

    vComplete <- dataset[[nms]][isComplete]

    # If not factor return quantiles of continuous moderator
    return(quantile(vComplete, probs = probeVals, na.rm = TRUE))
  })
  
  names(modProbes) <- modVars
  return(modProbes)
}

.procResCovGraph <- function(jaspResults, options) {
  modelsContainer <- jaspResults[["modelsContainer"]]

  for (i in 1:length(options[["processModels"]])) {
    modelOptions <- options[["processModels"]][[i]]
    modelName <- modelOptions[["name"]]

    if (
      !.procCheckGraph(modelsContainer[[modelName]][["graph"]]$object)
    ) next

    if (is.null(modelsContainer[[modelName]][["resCovGraph"]])) {
      resCovGraph <- .procResCovGraphSingleModel(modelsContainer[[modelName]][["graph"]]$object, modelOptions)
      state <- createJaspState(object = resCovGraph)
      modelsContainer[[modelName]][["resCovGraph"]] <- state
    }
  }
}

.procCombVars <- function(graph, vars) {
  varsNotInGraph <- vars[!vars %in% igraph::V(graph)$name]
  graph <- igraph::add_vertices(graph, length(varsNotInGraph), name = varsNotInGraph)
  if (length(vars) > 1) {
    edgesToAdd <- c(combn(vars, 2), rep(vars, each = 2))
    graph <- igraph::add_edges(graph,
      edges = edgesToAdd,
      source = edgesToAdd[seq(1, length(edgesToAdd), 2)],
      target = edgesToAdd[seq(2, length(edgesToAdd), 2)]
    )
  } else {
    graph <- igraph::add_edges(graph,
      edges = c(vars, vars),
      source = vars,
      target = vars
    )
  }
  return(graph)
}

.procResCovGraphSingleModel <- function(graph, modelOptions) {
  # Create new graph for covariances because we don't care about direction
  resCovGraph <- igraph::make_empty_graph(directed = FALSE)

  # Get all exogenous vars that are not interaction terms
  exoVars <- igraph::V(graph)[isExo & !isInt]$name

  if (modelOptions[["independentCovariances"]]) {
    # Add vertices and edges for all combinations
    resCovGraph <- .procCombVars(resCovGraph, exoVars)
  }

  # Get all mediator vars that are not interaction terms
  medVars <- igraph::V(graph)[isMed & !isInt]$name

  if (modelOptions[["mediatorCovariances"]]) {
    # Add vertices and edges for all combinations
    resCovGraph <- .procCombVars(resCovGraph, medVars)
  }

  # Add vertices and edges for first dependent var
  depVars <- igraph::V(graph)[isDep]$name
  resCovGraph <- .procCombVars(resCovGraph, depVars[1])
  
  # Remove edges that are in graph from resCovGraph
  return(igraph::difference(resCovGraph, igraph::as.undirected(graph)))
}

.procModelSyntax <- function(jaspResults, options) {
  modelsContainer <- jaspResults[["modelsContainer"]]

  for (i in 1:length(options[["processModels"]])) {
    modelOptions <- options[["processModels"]][[i]]
    modelName <- modelOptions[["name"]]

    if (
      !.procCheckGraph(modelsContainer[[modelName]][["graph"]]$object)
    ) next

    if (is.null(modelsContainer[[modelName]][["syntax"]])) {
      syntax <- .procModelSyntaxSingleModel(modelsContainer[[modelName]], options)
      state <- createJaspState(object = syntax)
      modelsContainer[[modelName]][["syntax"]] <- state
    }
  }
}

.procModelSyntaxSingleModel <- function(container, options) {
  graph <- container[["graph"]]$object
  
  doFit <- .procCheckFitModel(graph)

  regSyntax <- .procRegSyntax(graph)
  
  medEffectSyntax <- ""
  resCovSyntax <- ""

  if (doFit) {
    medEffectSyntax <- .procMedEffectsSyntax(graph,
      container[["modProbes"]]$object,
      container[["contrasts"]]$object
    )
    
    resCovSyntax <- .procResCovSyntax(container[["resCovGraph"]]$object)
  }

  headerJasp <- "
  # -------------------------------------------
  # Conditional process model generated by JASP
  # -------------------------------------------
  "

  headerResCov <- "
  # Residual covariances"

  headerMedEffects <- "
  # Mediation, indirect, and total effects"

  return(paste(
    headerJasp,
    regSyntax,
    headerResCov,
    resCovSyntax,
    headerMedEffects,
    medEffectSyntax,
    sep = "\n")
  )
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
    return(paste(e, rhs, sep = " ~ "))
  })
  return(paste(regLines, collapse = "\n"))
}

.procMedEffectsSyntaxMergeFacMods <- function(mods, contrFacVars, modOut, sep) {
  # Concatenates the dummy variables of factor moderators so they are not combined with each other as other moderators
  if (length(contrFacVars) > 0) {
    # Check if moderator is part of which contrast, returns NA if not
    modInWhichContr <- sapply(mods, function(v) {
      whichContr <- which(sapply(contrFacVars, function(contr) v %in% contr))
      if (length(whichContr) == 0) return(NA)
      return(whichContr)
    })
    # Replace dummy variables of factor moderators with concatenated variables
    for (i in unique(na.omit(modInWhichContr))) {
      # Which dummy variables should be replaced
      modOutInContr <- which(!is.na(modInWhichContr) & modInWhichContr == i)
      # Replace dummy variables
      modOut[[modOutInContr[1]]] <- .doCallPaste(Filter(Negate(is.null), modOut[modOutInContr]), sep = sep)
      # If more than two factor levels remove variables for additional levels
      if (length(modOutInContr) > 1) {
        modOut <- modOut[-modOutInContr[-1]]
      }
    }
  }
  return(modOut)
}

.procMedEffectsSyntaxModPars <- function(pathEdge, sourceNode, contrFacVars, graph, modProbes) {
  # Get moderator parameters for two-way interactions
  modPars <- lapply(pathEdge$modVars[[1]], function(v) {
    # Get edge for two way interaction between X and M
    twoWayEdge <- igraph::E(graph)[paste(sourceNode, v, sep = "__") %--% pathEdge$target]$parName

    # Concatenate two way edge parName with moderator probes
    return(apply(expand.grid(
      twoWayEdge,
      format(modProbes[[v]], digits = 3)
    ), 1, paste, collapse = "*"))
  })
  
  # Concatenate dummy variables for factor moderators
  modPars <- .procMedEffectsSyntaxMergeFacMods(pathEdge$modVars[[1]], contrFacVars, modPars, sep = " + ")
  
  # Get all possible combinations of probes from different moderators
  modPars <- apply(expand.grid(modPars), 1, paste, collapse = " + ")
  
  # Get name of potential three-way interaction
  threeWayInt <- paste(c(pathEdge$source, pathEdge$modVars[[1]]), collapse = "__")

  if (length(pathEdge$modVars[[1]]) > 1 && threeWayInt %in% igraph::E(graph)$source) { # If three-way int
    # Get edge of three way interaction X x M1 x M2
    threeWayEdge <- igraph::E(graph)[threeWayInt %--% pathEdge$target]
    # Combine three way int parName with moderator probes
    threeWayModPars <- paste(
      threeWayEdge$parName,
      apply(
        expand.grid(lapply(pathEdge$modVars[[1]], function(v) format(modProbes[[v]], digits = 3))),
        1, paste, collapse = "*"
      ),
      sep = "*"
    )
    # Add to previous moderator probes
    modPars <- paste(modPars, threeWayModPars, sep = "+")
  }
  return(modPars)
}

.procContrFacVars <- function(contrasts) {
  # Concatenate factor names with levels
  return(lapply(names(contrasts), function(contr) paste0(contr, colnames(contrasts[[contr]]))))
}

.procMedEffectsSyntaxGetLhs <- function(path, graph, modProbes, contrasts) {
  # Get factor names with level names
  contrFacVars <- .procContrFacVars(contrasts)
  # Is source node a factor with contrast
  sourceInContrFacVars <- sapply(contrFacVars, function(v) names(path)[1] %in% v)

  if (any(sourceInContrFacVars)) {
    # Replace factor in lhs with factor with level names appended
    facContr <- names(contrasts)[sourceInContrFacVars]
    facContrLevels <- paste(facContr, colnames(contrasts[[facContr]]), sep = "__")
    lhs <- paste(c(facContr, names(path)[-1]), collapse = "__")
    lhs <- apply(expand.grid(lhs, facContrLevels), 1, paste, collapse = ".")
  } else {
    # Just concatenate path vars to lhs
    lhs <- paste(names(path), collapse = "__")
  }
  
  # Get moderators on path
  modsOnPath <- unique(na.omit(unlist(sapply(2:length(path), function(i) {
    return(igraph::E(graph)[names(path)[i-1] %--% names(path)[i]]$modVars)
  }))))

  if (!is.null(modsOnPath) && length(modsOnPath) > 0) { # If path is moderated
    # Get combinations of moderators
    modsOnPathWithProbes <- lapply(modsOnPath, function(v) {
      return(apply(
        expand.grid(v, gsub("\\%", "", names(modProbes[[v]]))), # Remove `%` from quantile name strings
        1, paste, collapse = "__"
      ))
    })
    
    # Concatenate dummy variables for factor moderators
    modsOnPathWithProbes <- .procMedEffectsSyntaxMergeFacMods(modsOnPath, contrFacVars, modsOnPathWithProbes, sep = ".")
    
    # Add moderator combinations to left hand side
    lhs <- apply(expand.grid(lhs, apply(expand.grid(modsOnPathWithProbes), 1, paste, collapse = ".")), 1, paste, collapse = ".")
  }

  return(lhs)
}

.procMedEffectsSyntaxGetRhs <- function(path, graph, modProbes, contrasts) {
  # Get factor names with level names
  contrFacVars <- .procContrFacVars(contrasts)
  # Is source node a factor with contrast
  sourceInContrFacVars <- sapply(contrFacVars, function(v) names(path)[1] %in% v)
  
  if (any(sourceInContrFacVars)) {
    sourceFacVars <- contrFacVars[[which(sourceInContrFacVars)]]
  } else {
    sourceFacVars <- numeric(0)
  }

  # Right hand side of lavaan syntax
  rhs <- lapply(2:length(path), function(i) {
    # Get edge from path[-1] to path[i]
    if (any(sourceInContrFacVars) && names(path)[i-1] %in% sourceFacVars) {
      # Get all vars with factor levels as source nodes
      sourceNode <- sourceFacVars
    } else {
      # Just get previous node as source node
      sourceNode <- names(path)[i-1]
    }

    # Edge from previous node to current node
    pathEdge <- igraph::E(graph)[sourceNode %--% names(path)[i]]

    # If no moderators on edge, return only parName
    if(any(is.na(unlist(pathEdge$modVars)))) return(pathEdge$parName)

    # Get pars from moderators
    modPars <- .procMedEffectsSyntaxModPars(pathEdge, sourceNode, contrFacVars, graph, modProbes)
    
    # If indirect path add parentheses
    if (i > 1) {
      return(paste0("(", pathEdge$parName, " + ", modPars, ")"))
    }
    # Concanenate path edge parName with moderator probes
    return(paste(pathEdge$parName, modPars, sep = " + "))
  })
  
  # If indirect paths, multiply their steps
  rhs <- .doCallPaste(rhs, sep = "*")

  return(rhs)
}

.procMedEffectsSyntaxSinglePath <- function(path, graph, modProbes, contrasts) {
  # Right hand side of lavaan syntax
  rhs <- .procMedEffectsSyntaxGetRhs(path, graph, modProbes, contrasts)

  # Left hand side of lavaan syntax
  lhs <- .procMedEffectsSyntaxGetLhs(path, graph, modProbes, contrasts)

  # Encode column names because they will be split at '.'
  return(list(lhs = lhs, rhs = rhs))
}

.procRemoveDuplicateLabels <- function(condEffects) {
  # Remove duplicate conditional effect labels
  # Get unique valid conditional effect labels
  filteredConditionialEffects <- unique(unlist(Filter(function(x) !is.null(x) & all(x != ""), condEffects)))
  # If no conditional effects return NULL
  if (is.null(filteredConditionialEffects)) return(NULL)
  # Split conditional effect labels
  filteredConditionialEffectsSplit <- strsplit(filteredConditionialEffects, "\\.")
  # Check which labels are fully included in other labels
  isDuplicate <- sapply(1:length(filteredConditionialEffectsSplit), function(i) {
    # Iterate over all other split labels
    return(any(sapply(filteredConditionialEffectsSplit[-i], function(path) {
      # Check if all split chunks of label i are included in the split label
      return(all(filteredConditionialEffectsSplit[[i]] %in% path))
    })))
  })
  return(filteredConditionialEffects[!isDuplicate])
}

.procMedEffectsSyntax <- function(graph, modProbes, contrasts, standardize = FALSE) {
  allMedEffects <- list()
  allTotEffects <- list()
  allTotIndEffects <- list()

  # Iterate over all combinations of treatment and dependent variables
  for (trt in igraph::V(graph)[isTreat]$name) {
    for (dep in igraph::V(graph)[isDep]$name) {
      # Get all simple paths from X to Y
      medPaths <- igraph::all_simple_paths(graph,
        from = trt,
        to = dep,
        mode = "out"
      )
      # Get lhs and rhs plus moderator names for each path
      medEffects <- lapply(
        medPaths,
        .procMedEffectsSyntaxSinglePath,
        graph = graph,
        modProbes = modProbes,
        contrasts = contrasts
      )
      medPathLengthsSorted <- sort.int(sapply(medPaths, length), index.return = TRUE)
      # Sort effects according to path length to preserve order
      medEffects <- medEffects[medPathLengthsSorted$ix]
      # All rhs effects
      totRhs <- lapply(medEffects, function(path) path$rhs)
      # Add rhs effects
      totEffects <- .doCallPaste(totRhs, sep = " + ")
      # Get conditional effect labels from lhs of each path
      totEffectsConditional <- lapply(medEffects, function(path) {
        # Split lhs and remove first middle part of chunk, then paste together again
        return(sapply(strsplit(path$lhs, "\\."), function(path) {
          # Split first chunk
          pathSplit <- strsplit(path[1], "__")[[1]]
          # Only use first and last node of path for total effects
          paste(c(paste(pathSplit[c(1, length(pathSplit))], collapse = "__"), path[-1]), collapse = ".")
        }))
      })
      # Remove duplicate labels
      totEffectsConditionalUnique <- .procRemoveDuplicateLabels(totEffectsConditional)
      # Append __std to effect labels to indicate standardized effects
      if (standardize) {
        medEffects <- lapply(medEffects, function(path) list(lhs = paste0(path$lhs, "__std"), rhs = path$rhs))
        totEffectsConditionalUnique <- paste0(totEffectsConditionalUnique, "__std")
      }
      # Concatenate rhs and lhs effects
      medEffectsLabeled <- unlist(lapply(medEffects, function(path) paste(path$lhs, path$rhs, sep = " := ")))
      # Concatenate total effects
      totEffectsLabeled <- paste(
        paste0("tot.", totEffectsConditionalUnique), totEffects, sep = " := "
      )
      
      allMedEffects <- c(allMedEffects, medEffectsLabeled)
      allTotEffects <- c(allTotEffects, totEffectsLabeled)

      # Indirect paths have more than two nodes
      isIndirect <- medPathLengthsSorted$x > 2

      if (any(isIndirect)) {
        totIndEffectsConditionalUnique <- .procRemoveDuplicateLabels(totEffectsConditional[isIndirect])
        
        if (standardize) {
          totIndEffectsConditionalUnique <- paste0(totIndEffectsConditionalUnique, "__std")
        }

        # Add rhs indirect effects
        totIndEffects <- .doCallPaste(totRhs[isIndirect], sep = " + ")
        # Concatenate indirect effects
        totIndEffectsLabeled <- paste(
          paste0("totInd.", totIndEffectsConditionalUnique), totIndEffects, sep = " := "
        )

        allTotIndEffects <- c(allTotIndEffects, totIndEffectsLabeled)
      }
    }
  }

  allEffects <- c(allMedEffects, allTotEffects)

  # Only return total effects when no indirect path
  if (length(allTotIndEffects) > 0) {
    allEffects <- c(allEffects, allTotIndEffects)
  }
  # Remove duplicated effects (can occur for factors with more than two levels)
  return(paste(allEffects[!duplicated(allMedEffects)], collapse = "\n"))
}

.procResCovSyntax <- function(graph) {
  # Concatenate covariances: source ~~ target
  return(paste(igraph::E(graph)$source, igraph::E(graph)$target, sep = " ~~ ", collapse = "\n"))
}

# Results functions ----
.procCheckFitModelVars <- function(vars) {
  # Check if vector of var names contains encoded X, W, Z, or M (not Y!!)
  encoding <- .procVarEncoding()
  return(!any(unlist(encoding) %in% vars) && !any(grepl(encoding[["M"]], vars)))
}

.procCheckGraph <- function(graph) {
  return(igraph::is.igraph(graph))
}

.procCheckFitModel <- function(graph) {
  if (!.procCheckGraph(graph)) return(FALSE)
  allSourcesValid <- .procCheckFitModelVars(igraph::E(graph)$source)
  allTargetsValid <- .procCheckFitModelVars(igraph::E(graph)$target)
  return(allSourcesValid && allTargetsValid)
}

.procGraphAddEstimates <- function(graph, fittedModel, type = "effects") {
  parTbl <- lavaan::parameterTable(fittedModel)
  if (type == "effects") {
    est <- parTbl[parTbl$op == "~", ]
  } else {
    est <- parTbl[parTbl$op == "~~", ]
  }

  igraph::E(graph)$parEst <- NA
  
  for (i in 1:nrow(est)) {
    if (all(c(est$rhs[i], est$lhs[i])) %in% igraph::V(graph)$name) {
      igraph::E(graph)[est$rhs[i] %--% est$lhs[i]]$parEst <- est$est[i]
    }
  }

  return(graph)
}

.procStandardizedEstimates <- function(fittedModel, options) {
  # Gets standardized estimates with appropriate interaction terms
  # Note that we don't use lavaan::standardizedSolution: 
  # Its interaction terms are not standardized the way users might expect because
  # the standardization is applied to the product of the interacting variables 
  # instead of the variables before the product

  # Get unstandardized coefficients
  parTbl <- lavaan::parameterTable(fittedModel)

  # Get implied covariance matrix to calc standaridized interaction coeffcients
  covImplied <- lavaan::lavInspect(fittedModel, "implied")$cov

  # Get implied standard deviations of variables
  sdImplied <- sqrt(diag(covImplied))

  # Split lhs and rhs labels
  rhsSplit <- strsplit(parTbl$rhs, ":|__")
  lhsSplit <- strsplit(parTbl$lhs, "\\.")

  estStd <- parTbl$est
  
  for (i in 1:length(estStd)) {
    # If estimate is not defined effect
    if (parTbl$op[i] != ":=") {
      # Divide by implied SD of lhs variables
      estStd[i] <- estStd[i] / sdImplied[parTbl$lhs[i]]

      # If is not intercept
      if (parTbl$op[i] != "~1") {
        # Multiply by implied SDs of rhs variables
        for (v in rhsSplit[[i]]) {
          # Don't standardize by SDs of factors as this leads to uninterpretable coefficients
          # Instead the coefficients are partially standardized
          if (!any(sapply(options[["factors"]], grepl, x = v))) {
            estStd[i] <- estStd[i] * sdImplied[v]
          }
        }
      }
    # If effect is defined by other estimates
    } else {
      # Get lhs and rhs labels for defined effects
      if (lhsSplit[[i]][1] %in% c("tot", "totInd")) {
        lhsSplit2 <- strsplit(lhsSplit[[i]][2], "__")[[1]]
      } else {
        lhsSplit2 <- strsplit(lhsSplit[[i]][1], "__")[[1]]
      }
      
      # Divide by implied SD of lhs variables
      estStd[i] <- estStd[i] / sdImplied[lhsSplit2[length(lhsSplit2)]]

      # Don't standardize for factors
      if (!any(sapply(options[["factors"]], grepl, x = lhsSplit2[1]))) {
        # Multiply by implied SDs of rhs variables
        estStd[i] <- estStd[i] * sdImplied[lhsSplit2[1]]
      }
    }
  }

  return(estStd)
}

.procBootstrap <- function(fittedModel, samples) {
  # Run bootstrap, track progress with progress bar
  # Notes: faulty runs are simply ignored
  # recommended: add a warning if not all boot samples are successful
  # fit <- lavBootstrap(fit, samples = 1000)
  # if (nrow(fit@boot$coef) < 1000)
  #  tab$addFootnote(gettextf("Not all bootstrap samples were successful: CI based on %.0f samples.", nrow(fit@boot$coef)),
  #                  "<em>Note.</em>")
  
  
  coef_with_callback <- function(lav_object) {
    # Progress bar is ticked every time coef() is evaluated, which happens once on the main object:
    # https://github.com/yrosseel/lavaan/blob/77a568a574e4113245e2f6aff1d7c3120a26dd90/R/lav_bootstrap.R#L107
    # and then every time on a successful bootstrap:
    # https://github.com/yrosseel/lavaan/blob/77a568a574e4113245e2f6aff1d7c3120a26dd90/R/lav_bootstrap.R#L375
    # i.e., samples + 1 times
    progressbarTick()
    return(lavaan::coef(lav_object, type = "user"))
  }
  startProgressbar(samples + 1)
  
  bootResult <- lavaan::bootstrapLavaan(object = fittedModel, R = samples, FUN = coef_with_callback)
  
  # Add the bootstrap samples to the fit object
  fittedModel@boot       <- list(coef = bootResult)
  fittedModel@Options$se <- "bootstrap"
  
  return(fittedModel)
}

.procResultsFitModel <- function(container, dataset, options, modelOptions) {
  # Should model be fitted?
  doFit <- .procCheckFitModel(container[["graph"]]$object)

  if (!doFit) {
    dataset <- NULL
  } else {
    dataset <- .procMeanCenter(container[["graph"]]$object, dataset, options)
  }

  fittedModel <- try(lavaan::sem(
    model           = container[["syntax"]]$object,
    data            = dataset,
    se              = ifelse(options$errorCalculationMethod == "bootstrap", "standard", options$errorCalculationMethod),
    mimic           = options$emulation,
    estimator       = options$estimator,
    missing         = options$naAction,
    meanstructure   = modelOptions$intercepts,
    do.fit          = doFit
  ))

  if (jaspBase::isTryError(fittedModel)) {
    return(gettextf("Estimation failed: %s", gsub("lavaan ERROR:", "", jaspBase::.extractErrorMessage(fittedModel))))
  }

  if (doFit) {
    if (options$errorCalculationMethod == "bootstrap") {
      fittedModel <- .procBootstrap(fittedModel, options$bootstrapSamples)
    }
    container[["graph"]]$object <- .procGraphAddEstimates(container[["graph"]]$object, fittedModel)
    container[["resCovGraph"]]$object <- .procGraphAddEstimates(container[["resCovGraph"]]$object, fittedModel, type = "variances")
  }

  return(fittedModel)
}

.procGetSingleModelsDependencies <- function(modelIdx) {
  return(list(
    c("processModels", modelIdx, "name"),
    c("processModels", modelIdx, "inputType"),
    c("processModels", modelIdx, "processRelationships"),
    c("processModels", modelIdx, "modelNumber"),
    c("processModels", modelIdx, "modelNumberIndependent"),
    c("processModels", modelIdx, "modelNumberMediators"),
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
      if (modelOptions[["inputType"]] == "inputModelNumber" && !modelOptions[["modelNumber"]] %in% .procHardCodedModelNumbers()) {
        fittedModel <- gettextf("%1$s: Hayes model %2$s not implemented", modelName, modelOptions[["modelNumber"]])
      } else {
        fittedModel <- .procResultsFitModel(
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

# Output functions ----

.procIsModelNumberGraph <- function(modelNumber, graph, modelOptions, globalDependent) {
  # Create regList from hard-coded model
  modelNumberGraph <- try(.procProcessRelationshipsToGraph(.procGetHardCodedModel(modelNumber, length(modelOptions[["modelNumberMediators"]]))))

  if (!igraph::is.igraph(modelNumberGraph)) return(FALSE)

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
  summaryTable$dependOn(c(.procGetDependencies(), "processModels", "aicWeights", "bicWeights", "naAction"))
  summaryTable$position <- 1

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

  summaryTable[["Model"]]       <- modelNames[tableRowIsValid]
  summaryTable[["modelNumber"]] <- modelNumbers[tableRowIsValid]

  converged <- sapply(procResults, function(mod) mod@Fit@converged)

  if (any(!converged)) {
    summaryTable$addFootnote(
      message = gettextf("Model did not converge."),
      symbol = "\u2020",
      colNames = "Model",
      rowNames = modelNames[tableRowIsValid & !converged]
    )
  }

  if (length(procResults) == 0) {
    summaryTable$addFootnote(message = gettext("At least one model is incomplete or no model is specified. Please add at least one model and complete specified models."))
    return()
  }

  summaryTable[["N"]] <- sapply(procResults, lavaan::lavInspect, what = "nobs")

  if (any(converged)) {
    aic <- sapply(procResults[converged], AIC)
    bic <- sapply(procResults[converged], BIC)
    df  <- sapply(procResults[converged], lavaan::fitMeasures, fit.measures = "df")

    summaryTable[["AIC"]][converged]      <- aic
    summaryTable[["BIC"]][converged]      <- bic
    summaryTable[["logLik"]][converged]   <- sapply(procResults[converged], lavaan::logLik)
    summaryTable[["Df"]][converged]       <- df

    if (options[["aicWeights"]]) {
      summaryTable[["wAIC"]][converged] <- .computeWeights(aic)
    }
    if (options[["bicWeights"]]) {
      summaryTable[["wBIC"]][converged] <- .computeWeights(bic)
    }
  }
  if (options$estimator %in% c("dwls", "gls", "wls", "uls")) {
    summaryTable$addFootnote(message = gettext("The AIC, BIC and additional information criteria are only available with ML-type estimators."))
  }
}

.procContainerParameterEstimates <- function(jaspResults, options, modelsContainer) {
  if (!is.null(jaspResults[["parEstContainer"]])) {
    parEstContainer <- jaspResults[["parEstContainer"]]
  } else {
    parEstContainer <- createJaspContainer("Parameter estimates")
    parEstContainer$dependOn(.procGetDependencies())

    jaspResults[["parEstContainer"]] <- parEstContainer
  }

  validModel <- sapply(options[["processModels"]], function(mod) !is.null(modelsContainer[[mod[["name"]]]][["fittedModel"]]$object))

  if (any(validModel) && is.null(parEstContainer[["warning"]])) {
    warningHtml <- createJaspHtml(text = gettext(
      "<b>Important</b>: Parameter estimates can only be interpreted as causal effects if all confounding effects are accounted for and if the causal effect directions are correctly specified."
    ))
    warningHtml$position <- 1
    warningHtml$dependOn("processModels")
    parEstContainer[["warning"]] <- warningHtml
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
  if (inherits(procResult, "lavaan")) return(TRUE)
  if (is.character(procResult)) {
    container$setError(procResult)
  }
  return(FALSE)
}

.procSetContainerError <- function(container, procResult) {
  if (is.character(procResult)) {
    container$setError(procResult)
  }
}

.procParameterEstimateTables <- function(container, options, modelsContainer) {
  if (is.null(modelsContainer)) return()
  
  procResults <- lapply(options[["processModels"]], function(mod) modelsContainer[[mod[["name"]]]][["fittedModel"]]$object)
  modelNames <- sapply(options[["processModels"]], function(mod) mod[["name"]])

  for (i in 1:length(procResults)) {
    if (is.null(container[[modelNames[i]]])) {
      modelContainer <- createJaspContainer(title = modelNames[i], , initCollapsed = TRUE)
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
      .procPathCoefficientsTable(modelContainer, options, procResults[[i]], i)

    if (options[["processModels"]][[i]][["mediationEffects"]])
      .procPathMediationEffectsTable(modelContainer, options, procResults[[i]], i)

    if (options[["processModels"]][[i]][["totalEffects"]])
      .procPathTotalEffectsTable(modelContainer, options, procResults[[i]], i)

    if (options[["processModels"]][[i]][["residualCovariances"]])
      .procCovariancesTable(modelContainer, options, procResults[[i]], i)
  }
}

.procIsValidModel <- function(container, procResults) {
  return(inherits(procResults, "lavaan") && !container$getError() && procResults@Options[["do.fit"]])
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
  
  if (options$standardizedModelEstimates && "est.std" %in% names(coefs)) {
    tbl$addColumnInfo(name = "est.std", title = gettext("Std. Estimate"), type = "number", format = "sf:4;dp:3")
    tbl[["est.std"]] <- coefs$est.std
    
    # Set row names to assign footnotes to cells
    for (i in 1:nrow(coefs)) {
      tbl$setRowName(i, coefs$rowId[i])
    }

    rowHasFac <- sapply(coefs$rowId, function(v) any(sapply(options[["factors"]], grepl, x = v)))

    if (any(rowHasFac)) {
      tbl$addFootnote(
        message = gettext("Partially standardized estimate because effect involves categorical predictor."),
        colNames = "est.std",
        rowNames = coefs$rowId[rowHasFac]
      )
    }
  }

  if (options$meanCenteredModeration) {
    tbl$addFootnote(gettext("Moderation effect estimates are based on mean-centered variables."))
  }

  if (options$errorCalculationMethod == "bootstrap") {
    txt <- switch(options[["bootstrapCiType"]],
      percentileBiasCorrected = gettext("bias-corrected percentile"),
      percentile = gettext("percentile"),
      gettext("normal theory")
    )
    tbl$addFootnote(gettextf("Confidence intervals are %s bootstrapped.", txt))
  }
}

.procGetBootstrapCiType <- function(options) {
  return(switch(options[["bootstrapCiType"]],
    percentileBiasCorrected = "bca.simple",
    percentile = "perc",
    "norm"
  ))
}

.procBootstrapSamplesFootnote <- function(tbl, procResults, options) {
  if (options$errorCalculationMethod == "bootstrap" && nrow(procResults@boot$coef) < options$bootstrapSamples) {
    tab$addFootnote(gettextf("Not all bootstrap samples were successful: CI based on %.0f samples.", nrow(procResults@boot$coef)))
  }
}

.procPathCoefficientsTable <- function(container, options, procResults, modelIdx) {
  if (!is.null(container[["pathCoefficientsTable"]])) return()

  pathCoefTable <- createJaspTable(title = gettext("Path coefficients"))
  pathCoefTable$dependOn(
    nestedOptions = list(c("processModels", as.character(modelIdx), "pathCoefficients"),
                         c("processModels", as.character(modelIdx), "intercepts"))
  )
  container[["pathCoefficientsTable"]] <- pathCoefTable

  if (!.procIsValidModel(container, procResults)) return()

  bootstrapCiType <- .procGetBootstrapCiType(options)

  pathCoefs <- lavaan::parameterEstimates(procResults, boot.ci.type = bootstrapCiType,
                                          level = options$ciLevel)

  if (options$standardizedModelEstimates) {
    pathCoefs["rowId"] <- pathCoefs$rhs
    pathCoefs["est.std"] <- .procStandardizedEstimates(procResults, options)
  }

  if (!procResults@Fit@converged) {
    pathCoefTable$addFootnote(gettext("Model did not converge."))
  }

  .procBootstrapSamplesFootnote(pathCoefTable, procResults, options)
  
  # select paths from parameter estimates
  operators <- if(options[["processModels"]][[modelIdx]][["intercepts"]]) c("~1", "~") else "~"
  pathCoefs <- pathCoefs[pathCoefs$op %in% operators & !is.na(pathCoefs$z),]
  pathCoefs[which(pathCoefs$op == "~1"), "rhs"] <- "(Intercept)"
  pathCoefs <- pathCoefs[order(pathCoefs$op, decreasing = TRUE),]

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
    # If path is not conditional, add empty string to all mods
    if (length(path) == 1) {
      for (mod in mods) {
        modProbes[[mod]] <- c(modProbes[[mod]], "")
      }
    }
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
  if (!is.null(container[["mediationEffectsTable"]])) return()

  medEffectsTable <- createJaspTable(title = gettext("Mediation effects"))
  medEffectsTable$dependOn(
    options = "moderationProbes",
    nestedOptions = list(c("processModels", as.character(modelIdx), "mediationEffects"))
  )

  container[["mediationEffectsTable"]] <- medEffectsTable

  if (!.procIsValidModel(container, procResults)) return()

  bootstrapCiType <- .procGetBootstrapCiType(options)

  pathCoefs <- lavaan::parameterEstimates(procResults, boot.ci.type = bootstrapCiType,
                                          level = options$ciLevel)

  if (options$standardizedModelEstimates) {
    pathCoefs["rowId"] <- pathCoefs$lhs
    pathCoefs["est.std"] <- .procStandardizedEstimates(procResults, options)
  }

  if (!procResults@Fit@converged) {
    medEffectsTable$addFootnote(gettext("Model did not converge."))
  }

  .procBootstrapSamplesFootnote(medEffectsTable, procResults, options)
  
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

  uniqueMods <- unique(unlist(lapply(labelSplit, function(path) lapply(path[-1], function(row) row[1]))))

  modProbes <- .procEffectsTablesGetConditionalLabels(labelSplit[medLengthSortIdx], uniqueMods)

  for (mod in uniqueMods) {
    medEffectsTable$addColumnInfo(name = mod, title = mod, type = "string", combine = FALSE)
    medEffectsTable[[mod]] <- modProbes[[mod]]
  }

  # Add column with parameter labels
  if (options$parameterLabels) {
    medEffectsTable <- .procEffectsTablesParameterLabels(medEffectsTable, medEffects)
  }

  .procCoefficientsTable(medEffectsTable, options, medEffects)
}

.procPathTotalEffectsTable <- function(container, options, procResults, modelIdx) {
  if (!is.null(container[["totalEffectsTable"]])) return()

  totEffectsTable <- createJaspTable(title = gettext("Total effects"))
  totEffectsTable$dependOn(
    options = "moderationProbes",
    nestedOptions = list(c("processModels", as.character(modelIdx), "totalEffects"))
  )

  container[["totalEffectsTable"]] <- totEffectsTable

  if (!.procIsValidModel(container, procResults)) return()

  bootstrapCiType <- .procGetBootstrapCiType(options)

  pathCoefs <- lavaan::parameterEstimates(procResults, boot.ci.type = bootstrapCiType,
                                          level = options$ciLevel)

  if (options$standardizedModelEstimates) {
    pathCoefs["rowId"] <- pathCoefs$lhs
    pathCoefs["est.std"] <- .procStandardizedEstimates(procResults, options)
  }

  if (!procResults@Fit@converged) {
    totEffectsTable$addFootnote(gettext("Model did not converge."))
  }

  .procBootstrapSamplesFootnote(totEffectsTable, procResults, options)

  medEffects <- pathCoefs[pathCoefs$op == ":=",]

  labelSplit <- lapply(strsplit(medEffects$lhs, "\\."), strsplit, split = "__")

  # Only use label splits of length > 1 to omit total effects
  isTotEffect <- sapply(labelSplit, function(path) length(path[[1]])) == 1
  labelSplit <- labelSplit[isTotEffect]

  # Get paths from label of mediation effect
  totEffectLabels <- sapply(labelSplit, function(path) path[[1]])
  totEffects <- medEffects[isTotEffect, ]

  totEffectsTable$addColumnInfo(name = "lbl", title = "", type = "string", combine = TRUE)
  totEffectsTable[["lbl"]] <- ifelse(totEffectLabels == "tot", gettext("Total"), gettext("Total indirect"))

  totEffectsTable$addColumnInfo(name = "lhs_1", title = "", type = "string")
  totEffectsTable[["lhs_1"]] <- lapply(labelSplit, function(path) path[[2]][1])
  totEffectsTable$addColumnInfo(name = "op", title = "", type = "string")
  totEffectsTable[["op"]] <- rep_len("\u2192", length(totEffectLabels))
  totEffectsTable$addColumnInfo(name = "lhs_2", title = "", type = "string")
  totEffectsTable[["lhs_2"]] <- lapply(labelSplit, function(path) path[[2]][2])

  uniqueMods <- unique(unlist(lapply(labelSplit, function(path) lapply(path[-c(1:2)], function(row) row[1]))))

  modProbes <- .procEffectsTablesGetConditionalLabels(lapply(labelSplit, function(path) path[-2]), uniqueMods)

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

  if (!.procIsValidModel(container, procResults)) return()

  bootstrapCiType <- .procGetBootstrapCiType(options)

  pathCoefs <- lavaan::parameterEstimates(procResults, boot.ci.type = bootstrapCiType,
                                          level = options$ciLevel)

  if (!procResults@Fit@converged) {
    pathCoefTable$addFootnote(gettext("Model did not converge."))
  }

  .procBootstrapSamplesFootnote(pathCoefTable, procResults, options)

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
  if(is.null(modelsContainer)) return()

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

    # Sets container error if invalid model
    .procSetContainerError(modelContainer, procResults[[i]])

    contrasts <- modelsContainer[[modelNames[i]]][["contrasts"]]$object

    if (options[["processModels"]][[i]][["localTests"]])
      .procLocalTestTable(modelContainer, dataset, options, procResults[[i]], contrasts, i)
  }
}

.procLocalTestTable <- function(container, dataset, options, procResults, contrasts, modelIdx) {
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
  
  if (!.procIsValidModel(container, procResults)) return()

  # Only test variables in dataset that are part of model
  dataset <- dataset[procResults@Data@ov$name]

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

  if (testType == "cis" && !is.null(contrasts) && length(contrasts) > 0) {
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


.procRsquared <- function(jaspResults, options, modelsContainer) {
  # adapted from .semRsquared() from jaspSem/R/sem.R (one table for several models)
  if (!options[["rSquared"]] || !is.null(jaspResults[["rSquaredTable"]])) return()
  
  # retrieve model objects
  procResults <- lapply(options[["processModels"]], function(mod) modelsContainer[[mod[["name"]]]][["fittedModel"]]$object)
  modelNames <- sapply(options[["processModels"]], function(mod) mod[["name"]])
  resultIsValid <- sapply(procResults, function(mod) inherits(mod, "lavaan") && mod@Options[["do.fit"]])
  procResults <- procResults[resultIsValid]
  modelNames <- modelNames[resultIsValid]
  # init table
  tabr2 <- createJaspTable(gettext("R-squared"))
  tabr2$addColumnInfo(name = "__var__", title = "", type = "string")
  for (i in seq_along(options[["processModels"]])) {
    tabr2$addColumnInfo(name = paste0("rsq_", i), title = modelNames[i],
                        overtitle = "R\u00B2", type = "number")
  }
  
  tabr2$dependOn(c(.procGetDependencies(), "rSquared", "processModels"))
  tabr2$position <- 1
  
  jaspResults[["rSquaredTable"]] <- tabr2
  
  
  # compute data and fill table
  
  # retrieve r²
  r2li <- lapply(procResults, lavaan::inspect, what = "r2")
  
  # generate df with variable names
  r2df <- data.frame("varname__" = unique(unlist(lapply(r2li, names))))
  tabr2[["__var__"]] <- unique(unlist(lapply(r2li, names)))
  
  for (i in 1:length(r2li)) {
    # fill matching vars from model with df
    r2df[match(names(r2li[[i]]), r2df[["varname__"]]), i + 1] <- r2li[[i]]
    # add column to table
    tabr2[[paste0("rsq_", i)]] <- r2df[[i + 1]]
  }
}

# Plotting functions ----

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

    valid <- inherits(modelsContainer[[modelName]][["fittedModel"]]$object, "lavaan")

    if (valid) {
      if (options[["processModels"]][[i]][["conceptualPathPlot"]]) {
        .procConceptPathPlot(pathPlotsContainer, options, modelsContainer[[modelName]], i)
      }

      if (options[["processModels"]][[i]][["statisticalPathPlot"]])
        .procStatPathPlot(pathPlotsContainer, options, modelsContainer[[modelName]], i)
    }
  }
}

.procConceptPathPlot <- function(container, options, modelContainer, modelIdx) {
  if (!is.null(container[["conceptPathPlot"]])) return()

  procPathPlot <- createJaspPlot(title = gettext("Conceptual path plot"), height = 480, width = 480)
  procPathPlot$dependOn(
    options = c("pathPlotsLegendLabels", "pathPlotsLegendColor", "pathPlotsLabelLength", "pathPlotsColor", "pathPlotsColorPalette"),
    nestedOptions = list(c("processModels", as.character(modelIdx), "conceptualPathPlot"))
  )
  container[["conceptPathPlot"]] <- procPathPlot

  if (container$getError()) return()

  procPathPlot$plotObject <- .procLavToGraph(modelContainer, type = "conceptual", estimates = FALSE, options)
}

.procStatPathPlot <- function(container, options, modelContainer, modelIdx) {
  if (!is.null(container[["statPathPlot"]])) return()

  procPathPlot <- createJaspPlot(title = gettext("Statistical path plot"), height = 480, width = 480)
  procPathPlot$dependOn(
    options = c(
      "statisticalPathPlotsParameterEstimates",
      "statisticalPathPlotsCovariances",
      "statisticalPathPlotsResidualVariances",
      "pathPlotsLegendLabels",
      "pathPlotsLegendColor",
      "pathPlotsLabelLength",
      "pathPlotsColor",
      "pathPlotsColorPalette"
    ),
    nestedOptions = list(c("processModels", as.character(modelIdx), "statisticalPathPlot"))
  )
  container[["statPathPlot"]] <- procPathPlot

  if (container$getError()) return()

  procPathPlot$plotObject <- .procLavToGraph(modelContainer, type = "statistical", estimates = options[["statisticalPathPlotsParameterEstimates"]], options)
}

.procMainGraphLayoutPosHelper <- function(nodes) {
  # This function positions nodes alternatingly above and below zero with increasing distance
  nUniqueNodes <- length(unique(nodes))

  if (nUniqueNodes %% 2 == 0) {
    nUniqueNodesHalf <- nUniqueNodes/2
    pos <- -nUniqueNodesHalf:nUniqueNodesHalf
  } else {
    nUniqueNodesHalf <- floor(nUniqueNodes/2)
    pos <- -nUniqueNodesHalf:(nUniqueNodesHalf+1)
  }
  pos <- pos[pos != 0]

  # If node is outside of pos range, replace with max of node
  adjustedNodes <- pmatch(nodes, 1:nUniqueNodes, nomatch = nUniqueNodes, duplicates.ok = TRUE)

  # Reverse to start with positive pos
  return(rev(pos)[adjustedNodes])
}

.procMainGraphLayout <- function(graph) {
  igraph::V(graph)$posX <- NA
  igraph::V(graph)$posY <- NA

  # Calc pos of treat nodes
  igraph::V(graph)[isTreat]$posX <- 0
  igraph::V(graph)[isTreat]$posY <- .procMainGraphLayoutPosHelper(1:sum(igraph::V(graph)$isTreat))
  
  # Set y pos of first treat node to 0
  igraph::V(graph)[isTreat]$posY[1] <- 0

  igraph::V(graph)[isDep]$posX <- 1
  igraph::V(graph)[isDep]$posY <- .procMainGraphLayoutPosHelper(1:sum(igraph::V(graph)$isDep))
  igraph::V(graph)[isDep]$posY[1] <- 0
  
  return(graph)
}

.procMedGraphLayout <- function(graph) {
  # Add basic layout to graph
  graph <- .procMainGraphLayout(graph)

  # Get all simple paths (each node only visited once) from exo nodes to dep var node
  medPaths <- igraph::all_simple_paths(
    graph,
    from = igraph::V(graph)[isTreat]$name[1],
    to = igraph::V(graph)[isDep]$name,
    mode = "out"
  )
  medPathLengths <- sapply(medPaths, length)

  # Sort paths according to length (longest at top)
  medPaths <- medPaths[sort(medPathLengths, decreasing = TRUE, index.return = TRUE)$ix]
  
  # Calc pos of mediator nodes
  if (any(igraph::V(graph)$isMed)) {
    igraph::V(graph)[isMed]$posX <- sapply(igraph::V(graph)[isMed]$name, function(v) {
      # Which position has mediator in first occuring path
      return(na.omit(sapply(medPaths, function(path) match(v, names(path))))[1])
    }) - 1
    igraph::V(graph)[isMed]$posY <- .procMainGraphLayoutPosHelper(
      sapply(igraph::V(graph)[isMed]$name, 
        function(v) {
          # Which index has first occuring path
          return(max(which(sapply(medPaths, function(path) v %in% names(path))), na.rm = TRUE))
        }
      )
    )
  }
  # Adjust pos of dependent according to longest med path
  igraph::V(graph)[isDep]$posX <- max(medPathLengths) - 1
  
  return(graph)
}

.procGraphLayoutConceptual <- function(graph) {
  # Add mediator layout to graph
  graph <- .procMedGraphLayout(graph)

  # Calc pos of moderator nodes
  if (any(igraph::E(graph)$isMod)) {
    # Calc pos based on higher order interactions
    for (i in 1:length(igraph::V(graph)[isHigherOrderInt])) {
      # Get vars of interaction
      modIntVars <- igraph::V(graph)[isHigherOrderInt][i]$intVars[[1]]
      
      nMods <- length(modIntVars)-1
      # Get target vars of interaction
      target <- igraph::E(graph)[.from(igraph::V(graph)[isHigherOrderInt][i]$name)]$target
      
      for (l in 1:length(target)) {
        # Delete edges from moderators to target variable
        modIntVarHasEdgeToTarget <- sapply(modIntVars[-1], function(v) igraph::are_adjacent(graph, v, target[l]))
        
        if (any(modIntVarHasEdgeToTarget)) {
          graph <- igraph::delete_edges(graph,
            paste(modIntVars[-1][modIntVarHasEdgeToTarget], target[l], sep = "|")
          )
        }

        # Add helper nodes ("i"lj)
        helperNodeNames <- paste0("i", i, l, 1:nMods)
        graph <- igraph::add_vertices(graph, 
          nMods,
          name = helperNodeNames,
          posX = NA, posY = NA, isHigherOrderInt = FALSE
        )

        # Add edges from moderators to helper nodes
        graph <- igraph::add_edges(graph,
          edges = c(rbind(modIntVars[-1], helperNodeNames)),
          source = modIntVars[-1],
          target = helperNodeNames
        )
        
        # Create new target variable to modify later
        helperTarget <- target[l]

        for (j in 2:length(modIntVars)) {
          helperNodeName <- paste0("i", i, l, j-1)
          # Pos of helper nodes is mean of source and target nodes
          helperNodePosX <- mean(c(igraph::V(graph)[modIntVars[j-1]]$posX, igraph::V(graph)[helperTarget]$posX))
          helperNodePosY <- mean(c(igraph::V(graph)[modIntVars[j-1]]$posY, igraph::V(graph)[helperTarget]$posY))
          
          igraph::V(graph)[helperNodeName]$posX <- helperNodePosX
          igraph::V(graph)[helperNodeName]$posY <- helperNodePosY

          # Only assign pos of nodes that do not have pos yet
          if (is.na(igraph::V(graph)[modIntVars[j]]$posX) && is.na(igraph::V(graph)[modIntVars[j]]$posY)) {
            # Pos of moderator depends on index
            if (j %% 2 == 0) {
              # Place first moderator above source and target
              igraph::V(graph)[modIntVars[j]]$posX <- helperNodePosX
              igraph::V(graph)[modIntVars[j]]$posY <- .minMaxSubAddOne(igraph::V(graph)$posY)
            } else {
              # Place second moderator to the right of source and target
              igraph::V(graph)[modIntVars[j]]$posX <- .minMaxSubAddOne(helperNodePosX)
              igraph::V(graph)[modIntVars[j]]$posY <- helperNodePosY
            }
          }
          # Set helper node as next target
          helperTarget <- helperNodeName
        }
      }
    }
  }

  return(graph)
}

.procGraphLayoutStatistical <- function(graph) {
  # Add mediator layout to graph
  graph <- .procMedGraphLayout(graph)

  if (any(igraph::E(graph)$isMod)) {
    # Calc pos based on higher order interactions
    for (i in 1:sum(igraph::V(graph)$isHigherOrderInt)) {
      # Get vars of interaction
      modIntVars <- igraph::V(graph)[isHigherOrderInt][i]$intVars[[1]]
      # Get target var of interaction
      target <- igraph::E(graph)[.from(igraph::V(graph)[isHigherOrderInt][i]$name)]$target

      for (j in 1:length(igraph::V(graph)[isInt | isPartOfInt])) {
        # Update all interaction terms nested in current higher order interaction
        if (all(igraph::V(graph)[isInt | isPartOfInt]$intVars[j][[1]] %in% modIntVars)) {
          # x pos is mean of source and target
          igraph::V(graph)[isInt | isPartOfInt]$posX[j] <- mean(c(igraph::V(graph)[modIntVars[1]]$posX, igraph::V(graph)[target]$posX))
          # y pos depends on index 
          if (i %% 2 == 0) {
            # Place second higher-order terms below min pos
            igraph::V(graph)[isInt | isPartOfInt]$posY[j] <- min(igraph::V(graph)$posY, na.rm = TRUE) - 1
          } else {
            # Place first higher-order terms above max pos
            igraph::V(graph)[isInt | isPartOfInt]$posY[j] <- max(igraph::V(graph)$posY, na.rm = TRUE) + 1
          }
        }
      }
    }
  }
  return(graph)
}

.procLavToGraph <- function(container, type, estimates, options) {
  graph <- container[["graph"]]$object

  if (!.procCheckGraph(graph)) return()
  
  if (type == "conceptual") {
    graph <- .procGraphLayoutConceptual(graph)
  } else {
    graph <- .procGraphLayoutStatistical(graph)
  }

  layout <- cbind(igraph::V(graph)$posX, igraph::V(graph)$posY)
  rownames(layout) <- igraph::V(graph)$name

  # Order of node labels as in graph
  nodeNames <- igraph::V(graph)$name

  # Get idx of hidden helper node (to make it invisible)
  nodeIsHelper <- grepl("i[[:digit:]]", nodeNames)

  # Make hidden helper node invisible step 2
  nodeLabels <- gsub("__", ":", decodeColNames(nodeNames))
  nodeLabels[nodeIsHelper] <- ""

  # Create abbreviated node labels to plot in nodes
  nodeLabelsAbbr <- abbreviate(
    .procDecodeVarNames(nodeLabels),
    minlength = options[["pathPlotsLabelLength"]]
  )

  # Get edge labels for statistical plot
  if (type == "conceptual") {
    edgeLabels <- ""
  } else {
    edgeLabels <- if (estimates && !is.null(igraph::E(graph)$parEst)) round(as.numeric(igraph::E(graph)$parEst), 3) else igraph::E(graph)$parName
    
    resCovGraph <- igraph::as.directed(container[["resCovGraph"]]$object, mode = "arbitrary")
  }

  # Node size (scales with number of nodes automatically)
  nodeSize <- 0.625

  # Create variable for margin around edge ends (no margin for helper nodes)
  endCaps <- rep(ggraph::square(nodeSize, unit = "native"), length(igraph::E(graph)))
  endCaps[grepl("i[[:digit:]]", igraph::E(graph)$target)] <- ggraph::square(0, unit = "native")

  # Create visibility variable to make helper nodes transparent
  nodeVis <- rep(0, length(nodeLabels))
  nodeVis[nodeLabels == ""] = 1

  # Create dummy alpha variable for nodes (nessecary for creating the legend)
  nodeAlpha <- if (options[["pathPlotsLegendLabels"]]) nodeLabels else NULL

  # Create node type variable for coloring
  nodeType <- as.factor(ifelse(igraph::V(graph)$isMed, "Mediator",
    ifelse(igraph::V(graph)$isInt | igraph::V(graph)$isPartOfInt, "Moderator",
      ifelse(igraph::V(graph)$isExo, "Independent",
        ifelse(igraph::V(graph)$isDep, "Dependent", NA)
      )
    )
  ))

  # Create function from color palette
  colorFun <- jaspGraphs::JASPcolors(options[["pathPlotsColorPalette"]], asFunction = TRUE)

  if (options[["pathPlotsColor"]]) {
    if (type == "conceptual" && any(igraph::V(graph)$isInt)) {
      # Make helper nodes transparent
      colorPalette <- c(colorFun(length(unique(nodeType))-1), "transparent")
    } else {
      colorPalette <- colorFun(length(unique(nodeType)))
    }
  } else {
    nodeType <- NULL
    colorPalette <- rep("transparent", length(unique(nodeType)))
  }

  # Scale layout so that there is always one full step between each pos
  decimalPos <- layout[!nodeIsHelper,] %% 1
  
  # Path plots look better without scaling x-axis
  # if (any(na.omit(decimalPos[,1]) > 0)) {
  #   layout[,1] <- layout[,1] * (1/min(decimalPos[,1][decimalPos[,1] > 0], na.rm = TRUE))
  # }
  if (any(na.omit(decimalPos[,2]) > 0)) {
    yScale <- (1/min(decimalPos[,2][decimalPos[,2] > 0], na.rm = TRUE))
    layout[,2] <- layout[,2] * yScale
  }
  
  xRange <- diff(range(layout[,1], na.rm = TRUE))
  yRange <- diff(range(layout[,2], na.rm = TRUE))

  labelScale <- pmax(xRange, yRange)

  plotLayout <- ggraph::create_layout(graph, layout = layout[match(igraph::V(graph)$name, rownames(layout)), , drop = FALSE])

  if (type == "statistical") {
    graph <- graph + resCovGraph
  }

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
      data = ggraph::get_edges()(plotLayout),
      ggplot2::aes(
        label = edgeLabels,
        end_cap = endCaps
      ),
      edge_width = 0.9,
      color = "black",
      arrow = ggplot2::arrow(length = grid::unit(0.05, "native")),
      start_cap = ggraph::square(nodeSize, unit = "native"), # Arrow start has always margin
      angle_calc = "along",
      label_size = 7/sqrt(labelScale),
      label_dodge = grid::unit(0.025, "native"),
      label_push = grid::unit(-0.01, "native")
    ) +
    # Add abbreviated node lables with dummy alpha variable to display them in legend
    ggraph::geom_node_text(
      ggplot2::aes(label = nodeLabelsAbbr, alpha = nodeAlpha),
      size = 20/(labelScale + options[["pathPlotsLabelLength"]] - 3)
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
  
  if (type == "statistical" && length(igraph::V(resCovGraph)) > 0) {
    if (estimates && !is.null(igraph::E(resCovGraph)$parEst)) {
    covEdgeLabels <- round(as.numeric(igraph::E(resCovGraph)$parEst), 3)
    } else {
      covEdgeLabels <- ""
    }
    # drop = FALSE is important here to keep matrix for 2 elements!
    covLayout <- layout[rownames(layout) %in% igraph::V(resCovGraph)$name, , drop = FALSE]
    covPlotLayout <- ggraph::create_layout(
      resCovGraph, layout = covLayout[match(igraph::V(resCovGraph)$name, rownames(covLayout)), , drop = FALSE]
    )
    
    if (options[["statisticalPathPlotsCovariances"]]) {
      p <- p + 
        ggraph::geom_edge_arc(
          data = ggraph::get_edges()(covPlotLayout),
          mapping = ggplot2::aes(label = covEdgeLabels),
          fold = TRUE,
          edge_width = 0.9,
          color = "black",
          alpha = 0.5,
          arrow = ggplot2::arrow(ends = "both", length = grid::unit(0.025, "native")),
          start_cap = ggraph::square(nodeSize, unit = "native"),
          end_cap = ggraph::square(nodeSize, unit = "native"),
          angle_calc = "along",
          label_dodge = grid::unit(0.025, "native")
        )
    }

    if (options[["statisticalPathPlotsResidualVariances"]]) {
      p <- p + 
        ggraph::geom_edge_loop(
          data = ggraph::get_edges()(covPlotLayout),
          mapping = ggplot2::aes(label = covEdgeLabels, direction = ifelse(igraph::V(graph)[source]$isDep, 45, 135)),
          edge_width = 0.9,
          color = "black",
          alpha = 0.5,
          arrow = ggplot2::arrow(ends = "both", length = grid::unit(0.025, "native")),
          start_cap = ggraph::square(nodeSize, unit = "native"),
          end_cap = ggraph::square(nodeSize, unit = "native"),
          angle_calc = "along",
          label_dodge = grid::unit(-0.025, "native")
        )
    }
  }

  globalLabelSize <- 16

  if (options[["pathPlotsLegendLabels"]]) {
    # Get labels of nodes that are in visbile in plot
    nodeLabelUnique <- unique(nodeLabels[!is.na(igraph::V(graph)$posX)])
    nodeLabelUnique[nodeLabelUnique == ""] <- NA
    nodeLabelUniqueSorted <- sort(nodeLabelUnique, index.return = TRUE)
    # Add legend for label abbreviations by manually overiding dummy alpha variable
    p <- p + ggplot2::scale_alpha_manual(
      name = "",
      na.translate = FALSE,
      # Make all labels fully visible
      values = rep(1, length(nodeLabelUnique)),
      limits = abbreviate(.procDecodeVarNames(sort(nodeLabelUnique)), minlength = 15),
      guide = ggplot2::guide_legend(
        # Sort abbreviated labels according to sort index of full labels
        override.aes = list(
          label = unique(nodeLabelsAbbr[!is.na(igraph::V(graph)$posX)])[nodeLabelUniqueSorted[["ix"]]],
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
        margin = ggplot2::margin(0, 0, 0, 4*options[["pathPlotsLabelLength"]])
      )
    )

  return(p)
}

.procPlotSyntax <- function(container, options, modelsContainer) {
  if (!options[["syntax"]] || is.null(modelsContainer)) return()

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

.doCallPaste <- function(obj, sep) {
  return(do.call(paste, append(obj, list(sep = sep))))
}

.computeWeights <- function(x) {
  diffExp <- exp(-0.5*(x - min(x, na.rm = TRUE)))
  return(diffExp/sum(diffExp, na.rm = TRUE))
}

.minMaxSubAddOne <- function(x) {
  # If max(x) is higher than min(x), return min(x) - 1, otherwise max(x) + 1
  minMax <- range(x, na.rm = TRUE)

  if (abs(minMax[2]) > abs(minMax[1])) return(minMax[1] - 1)

  return(minMax[2] + 1)
}
