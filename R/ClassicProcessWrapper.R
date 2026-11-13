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

#' Classical Process Model
#'
ClassicProcess <- function(
          data = NULL,
          version = "1",
          aicWeights = FALSE,
          bicWeights = FALSE,
          bootstrapCiType = "percentile",
          bootstrapSamples = 1000,
          ciLevel = 0.95,
          colorPalette = "colorblind",
          conceptualPathPlotsForAllModels = TRUE,
          covariates = list(types = list(), value = list()),
          dependent = list(types = list(), value = ""),
          dependentCovariancesForAllModels = FALSE,
          emulation = "lavaan",
          errorCalculationMethod = "standard",
          estimator = "default",
          factors = list(types = list(), value = list()),
          hayesNumber = FALSE,
          independentCovariancesForAllModels = TRUE,
          interceptsForAllModels = FALSE,
          localTestBootstrapForAllModels = FALSE,
          localTestBootstrapSamplesForAllModels = 1000,
          localTestCorrectionForAllModels = "bonferroni",
          localTestTypeForAllModels = "cis",
          localTestsForAllModels = FALSE,
          meanCenteredModeration = TRUE,
          mediationEffectsForAllModels = TRUE,
          mediatorCovariancesForAllModels = FALSE,
          moderatedMediationIndex = FALSE,
          moderationProbeType = "percentile",
          moderationProbes = list(list(value = "16"), list(value = "50"), list(value = "84")),
          moderationProbesMeanSD = list(list(value = "-1"), list(value = "0"), list(value = "1")),
          naAction = "listwise",
          parameterLabels = FALSE,
          pathCoefficientsForAllModels = TRUE,
          pathPlotsLabelLength = 3,
          pathPlotsLegendColor = FALSE,
          pathPlotsLegendLabels = TRUE,
          plotHeight = 320,
          plotWidth = 480,
          processModels = list(list(conceptualPathPlot = TRUE, dependentCovariances = FALSE, independentCovariances = TRUE, inputType = "inputVariables", intercepts = FALSE, localTestBootstrap = FALSE, localTestBootstrapSamples = 1000, localTestType = "cis", localTests = FALSE, mediationEffects = TRUE, mediatorCovariances = FALSE, modelNumber = 1, modelNumberIndependent = NULL, modelNumberMediators = NULL, modelNumberModeratorW = NULL, modelNumberModeratorZ = NULL, name = "Model 1", pathCoefficients = TRUE, processRelationships = list(list(processDependent = NULL, processIndependent = NULL, processType = "", processVariable = NULL, value = "#")), qqPlotCi = FALSE, qqPlotCiLevel = 0.95, residualCovariances = FALSE, residualQqPlot = FALSE, residualVsFittedPlot = FALSE, statisticalPathPlot = FALSE, totalEffects = TRUE)),
          qqPlotCiForAllModels = FALSE,
          qqPlotCiLevelForAllModels = 0.95,
          rSquared = TRUE,
          residualCovariancesForAllModels = FALSE,
          residualQqPlotForAllModels = FALSE,
          residualVsFittedPlotForAllModels = FALSE,
          seed = 1,
          setSeed = FALSE,
          standardizedModelEstimates = FALSE,
          statisticalPathPlotsCovariances = FALSE,
          statisticalPathPlotsForAllModels = FALSE,
          statisticalPathPlotsParameterEstimates = FALSE,
          statisticalPathPlotsResidualVariances = FALSE,
          syntax = FALSE,
          totalEffectsForAllModels = TRUE,
          useColorPalette = TRUE) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("bootstrapCiType", "colorPalette", "covariates", "dependent", "factors", "localTestCorrectionForAllModels", "localTestTypeForAllModels", "moderationProbes", "moderationProbesMeanSD", "processModels")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspProcess", "ClassicProcess", "ClassicProcess.qml", options, version, TRUE))
}