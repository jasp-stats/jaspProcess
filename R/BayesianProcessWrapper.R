#
# Copyright (C) 2013-2025 University of Amsterdam
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

# This is a generated file. Don't change it!

#' Bayesian Process Model
#'
BayesianProcess <- function(
          data = NULL,
          version = "1",
          aggregatedChains = TRUE,
          autoCorPlot = FALSE,
          autoCorPlotLags = 20,
          autoCorPlotType = "lines",
          betaPriorMu = 0,
          betaPriorSigma = 10,
          bicWeights = FALSE,
          bivariateScatterDiagonalType = "density",
          bivariateScatterOffDiagonalType = "hexagon",
          bivariateScatterPlot = FALSE,
          ciLevel = 0.95,
          colorPalette = "colorblind",
          conceptualPathPlotsForAllModels = TRUE,
          covariates = list(types = list(), value = list()),
          densityPlot = FALSE,
          dependent = list(types = list(), value = ""),
          dependentCovariancesForAllModels = FALSE,
          factors = list(types = list(), value = list()),
          hayesNumber = FALSE,
          histogramPlot = FALSE,
          independentCovariancesForAllModels = TRUE,
          interceptsForAllModels = FALSE,
          legend = TRUE,
          mcmcBurnin = 500,
          mcmcChains = 3,
          mcmcSamples = 1000,
          meanCenteredModeration = TRUE,
          mediationEffectsForAllModels = TRUE,
          mediatorCovariancesForAllModels = FALSE,
          moderationProbeType = "percentile",
          moderationProbes = list(list(value = "16"), list(value = "50"), list(value = "84")),
          moderationProbesMeanSD = list(list(value = "-1"), list(value = "0"), list(value = "1")),
          nuPriorMu = 0,
          nuPriorSigma = 32,
          parameterLabels = FALSE,
          pathCoefficientsForAllModels = TRUE,
          pathPlotsLabelLength = 3,
          pathPlotsLegendColor = FALSE,
          pathPlotsLegendLabels = TRUE,
          plotHeight = 320,
          plotWidth = 480,
          priorDistributions = FALSE,
          processModels = list(list(conceptualPathPlot = TRUE, dependentCovariances = FALSE, independentCovariances = TRUE, inputType = "inputVariables", intercepts = FALSE, mediationEffects = TRUE, mediatorCovariances = FALSE, modelNumber = 1, modelNumberIndependent = NULL, modelNumberMediators = NULL, modelNumberModeratorW = NULL, modelNumberModeratorZ = NULL, name = "Model 1", pathCoefficients = TRUE, processRelationships = list(list(processDependent = NULL, processIndependent = NULL, processType = "", processVariable = NULL, value = "#")), qqPlotCi = FALSE, qqPlotCiLevel = 0.95, residualCovariances = FALSE, residualQqPlot = FALSE, residualVsFittedPlot = FALSE, statisticalPathPlot = FALSE, totalEffects = TRUE)),
          psiPriorAlpha = 1,
          psiPriorBeta = 0.5,
          qqPlotCiForAllModels = FALSE,
          qqPlotCiLevelForAllModels = 0.95,
          residualCovariancesForAllModels = FALSE,
          residualQqPlotForAllModels = FALSE,
          residualVsFittedPlotForAllModels = FALSE,
          rhoPriorAlpha = 1,
          rhoPriorBeta = 1,
          seed = 1,
          setSeed = FALSE,
          statisticalPathPlotsCovariances = FALSE,
          statisticalPathPlotsForAllModels = FALSE,
          statisticalPathPlotsParameterEstimates = FALSE,
          statisticalPathPlotsResidualVariances = FALSE,
          syntax = FALSE,
          totalEffectsForAllModels = TRUE,
          tracePlot = FALSE,
          useColorPalette = TRUE) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("colorPalette", "covariates", "dependent", "factors", "moderationProbes", "moderationProbesMeanSD", "processModels")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspProcess", "BayesianProcess", "BayesianProcess.qml", options, version, TRUE))
}