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
  mainContainer <- .procContainerMain(jaspResults, options, procResults)

  .procConceptPathPlot(jaspResults, options, procResults)

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
  return(c('dependent', 'modelNumberIndependent','modelNumberMediators',
           'modelNumberCovariates', 'modelNumberModeratorW',
           'modelNumberModeratorZ','processModels','covariates', 'factors'))
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

.procToLavModSingleModel <- function(modelOptions) {

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

    dependent    <- modelOptions[["dependent"]]
    independent  <- modelOptions[["modelNumberIndependent"]]
    mediators    <- modelOptions[["modelNumberMediators"]]
    covariates   <- modelOptions[["modelNumberCovariates"]]
    modW         <- modelOptions[["modelNumberModeratorW"]]
    modZ         <- modelOptions[["modelNumberModeratorZ"]]
    number       <- modelOptions[["modelNumber"]]

    print(dependent)
    print(independent)
    print(mediators)
    print(covariates)
    print(modW)
    print(modZ)
    print(modelOptions[["modelNumber"]])

    print(length(dependent))
    print(length(independent))
    print(length(mediators))
    print(length(covariates))
    print(length(modW))
    print(length(modZ))

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

  # regSyntax <- paste(
  #    paste0(names(regList)),
  #    sapply(regList, function(row) paste(row$parNames, row$vars, sep = "*", collapse = " + ")),
  #    sep = " ~ ",
  #    collapse = "\n"
  #  )

  print(regSyntax)

  return(regSyntax)

}

.procInitOptions <- function(jaspResults, options) {
  # Determine if analysis can be run with user input
  # Calculate any options common to multiple parts of the analysis
  model = options[["processModels"]][[1]]
  model[["dependent"]] = options[["dependent"]]

  modelSyntax <- .procToLavModSingleModel(model)

  options[["modelSyntax"]] <- list(modelSyntax)

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
.procComputeResults <- function(jaspResults, dataset, options) {
  # Function to compute and store results in container
  if (is.null(jaspResults[["stateProcResults"]])) {
    procResults <- .procResultsHelper(dataset, options)
    jaspResults[["model"]] <- createJaspState(object = procResults, dependencies = .procGetDependencies())
  } else {
    procResults <- jaspResults[["model"]]$object
  }
  procResults
}

.procResultsHelper <- function(dataset, options) {
  # Helper function to compute actual results
  procResult <- lavaan::sem(
    model           = options[["modelSyntax"]][[1]],
    data            = dataset,
    se              = "standard"
  )

  return(procResult)
}

# Output functions ----
# These are not in use for now, but left here as orientation for later
.procContainerMain <- function(jaspResults, options, procResults) {
  if (!is.null(jaspResults[["procMainContainer"]])) {
    mainContainer <- jaspResults[["procMainContainer"]]
  } else {
    mainContainer <- createJaspContainer("Model fit tables")
    mainContainer$dependOn(.procGetDependencies())

    jaspResults[["procMainContainer"]] <- mainContainer
  }

  return(mainContainer)
}

.procTableSomething <- function(jaspResults, options, procResults) {
  if (!is.null(jaspResults[["procMainContainer"]][["procTable"]])) return()

  # Below is one way of creating a table
  procTable <- createJaspTable(title = "proc Table")
  procTable$dependOnOptions(c("variables", "someotheroption")) # not strictly necessary because container

  # Bind table to jaspResults
  jaspResults[["procMainContainer"]][["procTable"]] <- procTable

  # Add column info
  procTable$addColumnInfo(name = "chisq",  title = "\u03a7\u00b2", type = "number", format = "sf:4")
  procTable$addColumnInfo(name = "pvalue", title = "p",            type = "number", format = "dp:3;p:.001")
  procTable$addColumnInfo(name = "BF",     title = "Bayes Factor", type = "number", format = "sf:4")
  procTable$addColumnInfo(name = "sth",    title = "Some Title",   type = "string")

  # Add data per column
  procTable[["chisq"]]  <- procResults$column1
  procTable[["pvalue"]] <- procResults$column2
  procTable[["BF"]]     <- procResults$column3
  procTable[["sth"]]    <- procResults$sometext
}

.procTableSthElse <- function(jaspResults, options, procResults) {
  if (!is.null(jaspResults[["procMainContainer"]][["procTable2"]])) return()

  # Below is one way of creating a table
  procTable2 <- createJaspTable(title = "proc Table Something Else")
  procTable2$dependOnOptions(c("variables", "someotheroption"))

  # Bind table to jaspResults
  jaspResults[["procMainContainer"]][["procTable2"]] <- procTable2

  # Add column info
  procTable2$addColumnInfo(name = "hallo", title = "Hallo", type = "string")
  procTable2$addColumnInfo(name = "doei",  title = "Doei",  type = "string")

  # Calculate some data from results
  procSummary <- summary(procResults$someObject)

  # Add data per column. Calculations are allowed here too!
  procTable2[["hallo"]] <- ifelse(procSummary$hallo > 1, "Hallo!", "Hello!")
  procTable2[["doei"]]  <- procSummary$doei^2
}

.procConceptPathPlot <- function(jaspResults, options, procResults) {
  if (!is.null(jaspResults[["conceptPathPlot"]])) return()

  procPathPlot <- createJaspPlot(title = gettext("Conceptual path plot"), height = 320, width = 480)
  procPathPlot$dependOn(.procGetDependencies())
  jaspResults[["conceptPathPlot"]] <- procPathPlot
  procPathPlot$plotObject <- .procLavToGraph(procResults, options)
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
  medPaths <- igraph::all_simple_paths(graph, from = nodeNames[exoIdx], to = nodeNames[depIdx])
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
  layout <- rbind(exoPos, depPos, medPos)
  rownames(layout) <- c(nodeNames[c(exoIdx, depIdx)], rownames(medPos))
  return(layout)
}

.procLavToGraph <- function(procResults, options) {
  # Get table with SEM pars from lavaan model
  parTbl <- procResults@ParTable

  # Create path matrix where first col is "from" and second col is "to"
  paths <- matrix(c(decodeColNames(parTbl$rhs), decodeColNames(parTbl$lhs))[parTbl$op == "~"], ncol = 2)

  # Check if "from" contains interaction term
  isIntPath <- grepl(":", paths[, 1])

  # Split interaction terms
  intPathsSplit <- strsplit(paths[isIntPath, 1], ":")

  # Get moderator vars from interaction terms
  mods <- sapply(intPathsSplit, function(path) path[2])

  # Get independent vars from interaction terms
  indeps <- sapply(intPathsSplit, function(path) path[1])

  # Create matrix with moderator paths
  # Adds a path from moderator to helper node "iX" which will be invisible
  modPaths <- matrix(c(mods, paste0("i", 1:length(mods))), ncol = 2)

  # Filter out non-moderation paths -> main paths
  mainPaths <- matrix(paths[!isIntPath & !paths[, 1] %in% mods[!mods %in% paths[, 2]], ], ncol = 2)

  # Get layout of main paths: matrix with x,y coordinates for each node
  layout <- .procMainGraphLayout(mainPaths, decodeColNames(options[["dependent"]]))

  # Node names are in rownames
  nodeNames <- rownames(layout)

  # Combine main paths and moderator paths
  if (sum(isIntPath) > 0) mainPaths <- rbind(mainPaths, modPaths)

  # Add layout of moderator nodes
  if (length(mods) > 0) {
    for (i in 1:length(mods)) {
      # Get index of independent and dependent node in layout
      idxIndep <- which(nodeNames == indeps[i])
      idxDep <- which(nodeNames == paths[isIntPath, 2][i])
      # Calculate pos of hidden helper node as average between indep and dep node pos
      nodePosI <- apply(layout[c(idxIndep, idxDep), ], 2, mean)
      # Moderator pos has same x pos as hidden helper node
      modPos <- c(nodePosI[1], max(layout[, 2]) + 1)
      # Append to node names and layout
      nodeNames <- c(nodeNames, mods[i], paste0("i", i))
      layout <- rbind(layout, modPos, nodePosI)
    }
  }

  # Order of node labels as in qgraph
  graphNodeNames <- unique(as.vector(mainPaths))
  # Get idx of hidden helper node (to make it invisible)
  graphIntIdx <- grepl("i[[:digit:]]", graphNodeNames)

  # Calc node size depending on number of nodes
  nodeSize <- rep(round(8*exp(-nrow(layout)/80)+1), length(graphNodeNames))
  # Make hidden helper node invisible step 1
  nodeSize[graphIntIdx] <- 0

  # Invisible node must be circle, otherwise incoming edges are omitted (qgraph bug)
  nodeShape <- rep("square", length(graphNodeNames))
  nodeShape[graphIntIdx] <- "circle"

  # Make hidden helper node invisible step 2
  nodeLabels <- graphNodeNames
  nodeLabels[graphIntIdx] <- ""

  g <- jaspBase:::.suppressGrDevice(qgraph::qgraph(
    mainPaths,
    layout = layout[match(graphNodeNames, nodeNames), ], # match order of layout
    vsize = nodeSize,
    shape = nodeShape,
    labels = TRUE,
    border.width = 1.5,
    edge.label.cex = 1.2,
    edge.color = "black"
  ))

  # There seems to be a bug in qgraph where specifying labels
  # in the initial function call does not work
  g$graphAttributes$Nodes$labels <- nodeLabels

  return(g)
}

.procPlotSomething <- function(jaspResults, options, procResults) {
  if (!is.null(jaspResults[["procPlot"]])) return()

  procPlot <- createJaspPlot(title = "proc Plot", height = 320, width = 480)
  procPlot$dependOnOptions(c("variables", "someotheroption"))

  # Bind plot to jaspResults
  jaspResults[["procPlot"]] <- procPlot

  procPlot$plotObject <- plot(procResults$someObject)
}
