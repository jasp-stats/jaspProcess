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

  .procPathPlot(jaspResults, options, procResults)

  # .procTableSomething(jaspResults, options, procResults)
  # .procTableSthElse(  jaspResults, options, procResults)
  # .procPlotSomething( jaspResults, options, procResults)

  return()
}

.procGetDependencies <- function() {
  return(c('dependent', 'covariates', 'factors', 'processModels'))
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
    
    # Add process var to regression of dependent var
    regList <- .procAddLavModVar(regList, dependent, processVariable)

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
      # Add interaction independent x moderator var to regression of dependent var
      interVar <- paste0(independent, ":", processVariable)
      regList <- .procAddLavModVar(regList, dependent, interVar)
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

  return(regSyntax)
}

.procInitOptions <- function(jaspResults, options) {
  # Determine if analysis can be run with user input
  # Calculate any options common to multiple parts of the analysis
  model = options[["processModels"]][[1]]

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

.procPathPlot <- function(jaspResults, options, procResults) {
  if (!is.null(jaspResults[["pathPlot"]])) return()

  procPathPlot <- createJaspPlot(title = gettext("Path plot"), height = 320, width = 480)
  procPathPlot$dependOn(.procGetDependencies())
  jaspResults[["pathPlot"]] <- procPathPlot
  procPathPlot$plotObject <- .procPathPlotHelper(procResults, options)
}

.procPathPlotHelper <- function(procResults, options) {
  plotObject <- jaspSem:::.medLavToPlotObj(procResults, options)

  parTable <- procResults@ParTable

  nPars <- length(parTable[["label"]][parTable[["label"]] != ""])

  p <- jaspBase:::.suppressGrDevice(semPlot::semPaths(
    object         = plotObject,
    intercepts     = FALSE,
    residuals      = TRUE,
    thresholds     = FALSE,
    whatLabels     = "name",
    border.width   = 1.5,
    edge.label.cex = 1.2,
    lty            = 2,
    sizeMan        = round(8*exp(-nPars/80)+1)
  ))
  return (p)
}

.procPlotSomething <- function(jaspResults, options, procResults) {
  if (!is.null(jaspResults[["procPlot"]])) return()

  procPlot <- createJaspPlot(title = "proc Plot", height = 320, width = 480)
  procPlot$dependOnOptions(c("variables", "someotheroption"))
  
  # Bind plot to jaspResults
  jaspResults[["procPlot"]] <- procPlot

  procPlot$plotObject <- plot(procResults$someObject)
}
