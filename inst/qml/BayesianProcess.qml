//
// Copyright (C) 2023 University of Amsterdam and Netherlands eScience Center
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls
import "./common" as Common

Form
{
	Common.VariablesForm {}

    Section
    {
        title: qsTr("Models")
        columns: 1

        TabView
        {
            id:				models
            name:			"processModels"
            maximumItems:	10
            newItemName:	qsTr("Model 1")
            optionKey:		"name"

            content: Group
            {
                Group {
					Layout.leftMargin: jaspTheme.contentMargin

					Common.InputType
					{
						id: inputType
						modelName: rowValue
					}

                    Common.Separator {}

                    Common.InputVariables {
						visible: inputType.value == "inputVariables"
						adjustedWidth: models.width - 2 * jaspTheme.contentMargin
						colWidth: (models.width - 3 * 40 * preferencesModel.uiScale) / 4
					}

                    Common.InputModelNumber {
						visible: inputType.value == "inputModelNumber"
						adjustedWidth: models.width - 2 * jaspTheme.contentMargin
					}

					Group
					{
                        id: opts
						columns: 	3
                        
						Common.ResidualCovariances
						{
							independentCovariancesChecked: 	independentCovariancesForAllModels.checked
							mediatorCovariancesChecked: 	mediatorCovariancesForAllModels.checked
							dependentCovariancesChecked:	dependentCovariancesForAllModels.checked
						}

						Common.ParameterEstimates
						{
							pathCoefficientsChecked: 	pathCoefficientsForAllModels.checked
							interceptsChecked: 			interceptsForAllModels.checked
							mediationEffectsChecked: 	mediationEffectsForAllModels.checked
							totalEffectsChecked: 		totalEffectsForAllModels.checked
							residualCovariancesChecked: residualCovariancesForAllModels.checked
						}

						Common.PathPlots
						{
							conceptualPathPlotsChecked:		conceptualPathPlotsForAllModels.checked
							statisticalPathPlotsChecked:	statisticalPathPlotsForAllModels.checked
						}
					}
                }
            }
        }
    }

    Section
    {
        title: qsTr("Options")
        columns: 3

		Group
		{
			CheckBox 
			{
				label: qsTr("Prior distributions")
				name: "priorDistributions"
			}
			CheckBox 
			{
				label: qsTr("Parameter labels")
				name: "parameterLabels"
			}
			CheckBox 
			{
				label: qsTr("Lavaan syntax")
				name: "syntax"
			}
			CheckBox 
			{
				label: qsTr("BIC weights")
				name: "bicWeights"
			}
			CheckBox 
			{
				label: qsTr("Hayes configuration number")
				name: "hayesNumber"
			}
		}

		Group
		{
			IntegerField
			{
				name:			"mcmcBurnin"
				id:				warmup
				label:			qsTr("Burnin")
				defaultValue:	500
				min:			100
			}

			IntegerField
			{
				name:			"mcmcSamples"
				label:			qsTr("Samples")
				defaultValue:	1000
				min:			parseInt(warmup.value) + 100
			}

			IntegerField
			{
				name:			"mcmcChains"
				label:			qsTr("Chains")
				defaultValue:	3
				min:			1
			}
		}

		Group
		{
			SetSeed {}

			CIField 
			{
				text: qsTr("Credible intervals")
				name: "ciLevel"
			}
			
			CheckBox
			{
				label: qsTr("Mean-centered moderation")
				name: "meanCenteredModeration"
			}
		}
	}

	Section
	{
		title: qsTr("Plots")
		columns: 1

		CheckBox
		{
			name: 		"useColorPalette"
			label: 		qsTr("Color palette")
			checked: 	true
			childrenOnSameRow: true

			ColorPalette
			{
				name: "colorPalette"
				label: ""
			}
		}

		Common.PathPlotOptions {}

		Group
		{
			title: qsTr("MCMC Plots")
			columns: 2
			Group
			{
				CheckBox { name: "aggregatedChains";	label: qsTr("Aggregate chains for densities and histograms");	checked:true	}
				CheckBox { name: "legend";				label: qsTr("Show legends");									checked:true	}
				CheckBox { name: "densityPlot";			label: qsTr("Density")															}
				CheckBox { name: "histogramPlot";		label: qsTr("Histogram")														}
				CheckBox { name: "tracePlot";			label: qsTr("Trace");															}
			}
			Group
			{
				columns: 2
				CheckBox { label: qsTr("Autocorrelation");	name: "autoCorPlot"; id: autoCorrelation
					IntegerField
					{
						name: "autoCorPlotLags"
						label: qsTr("No. lags")
						defaultValue: 20
						min: 1
						max: 100
					}
					RadioButtonGroup
					{
						name: "autoCorPlotType"
						title: qsTr("Type")
						RadioButton { value: "lines";	label: qsTr("line"); checked:true	}
						RadioButton { value: "bars";	label: qsTr("bar")					}
					}
				}
				CheckBox { label: qsTr("Bivariate scatter");  name: "bivariateScatterPlot"; id: bivariateScatter
					RadioButtonGroup
					{
						name: "bivariateScatterDiagonalType"
						title: qsTr("Diagonal plot type")
						RadioButton { value: "density";		label: qsTr("Density"); checked:true	}
						RadioButton { value: "histogram";	label: qsTr("Histogram")				}
					}
					RadioButtonGroup
					{
						name: "bivariateScatterOffDiagonalType"
						title: qsTr("Off-diagonal plot type")
						RadioButton { value: "hexagon";		label: qsTr("Hexagonal"); checked:true	}
						RadioButton { value: "contour";		label: qsTr("Contour")					}
					}
				}
			}
		}
	}

	Section 
    {
        id: advanced
        title: qsTr("Advanced")
        columns: 1

        Group
        {
            title: qsTr("Set for All Models")
            columns: 4

			Group
			{
                title: qsTr("Residual Covariances")
				CheckBox
                {
                    id:			independentCovariancesForAllModels
                    name: 		"independentCovariancesForAllModels"
                    label: 		qsTr("Independent variables")
					checked:	true
                }
				CheckBox
                {
                    id:			mediatorCovariancesForAllModels
                    name: 		"mediatorCovariancesForAllModels"
                    label: 		qsTr("Mediators")
                }
				CheckBox
                {
                    id:			dependentCovariancesForAllModels
                    name: 		"dependentCovariancesForAllModels"
                    label: 		qsTr("Dependent variables")
                }
			}

            Group
            {
                title: qsTr("Parameter Estimates")
				columns: 1

                CheckBox
                {
                    id:			pathCoefficientsForAllModels
                    name: 		"pathCoefficientsForAllModels"
                    label: 		qsTr("Paths")
                    checked: 	true

					CheckBox
                    {
                        id:			interceptsForAllModels
                        name: 		"interceptsForAllModels"
                        label: 		qsTr("Intercepts")
                    }
                }
                CheckBox
                {
                    id:			mediationEffectsForAllModels
                    name: 		"mediationEffectsForAllModels"
                    label: 		qsTr("Mediation")
                    checked: 	true
                }
				CheckBox
                {
                    id:			totalEffectsForAllModels
                    name: 		"totalEffectsForAllModels"
                    label: 		qsTr("Total")
                    checked: 	true
                }
				CheckBox
                {
                    id:			residualCovariancesForAllModels
                    name: 		"residualCovariancesForAllModels"
                    label: 		qsTr("Residual covariances")
                }
            }

            Group
            {
                title: qsTr("Path Plots")
                CheckBox
                {
                    id:			conceptualPathPlotsForAllModels
                    name: 		"conceptualPathPlotsForAllModels"
                    label: 		qsTr("Conceptual")
                    checked: 	true
                }
                CheckBox
                {
                    id:			statisticalPathPlotsForAllModels
                    name: 		"statisticalPathPlotsForAllModels"
                    label: 		qsTr("Statistical")
                }
            }
		}

		Group
		{
			title: qsTr("Prior distributions")

			columns: 1

			Group
			{
				columns: 4

				Label
				{
					text: qsTr("Intercepts:")
				}
				Label
				{
					text: qsTr("v ~ Normal(μ,σ)")
				}
				FormulaField
				{
					name: "nuPriorMu"
					label: "μ"
					value: "0"
					fieldWidth: 40 * preferencesModel.uiScale
				}
				FormulaField
				{
					name: "nuPriorSigma"
					label: "σ"
					value: "32"
					min: 0
					inclusive: JASP.None
					fieldWidth: 40 * preferencesModel.uiScale
				}
				
				Label
				{
					text: qsTr("Path coefficients:")
				}
				Label
				{
					text: qsTr("β ~ Normal(μ,σ)")
				}
				FormulaField
				{
					name: "betaPriorMu"
					label: "μ"
					value: "0"
					fieldWidth: 40 * preferencesModel.uiScale
				}
				FormulaField
				{
					name: "betaPriorSigma"
					label: "σ"
					value: "10"
					min: 0
					inclusive: JASP.None
					fieldWidth: 40 * preferencesModel.uiScale
				}

				Label
				{
					text: qsTr("Standard deviations:")
				}
				Label
				{
					text: qsTr("ψ ~ Gamma(α,β)")
				}
				FormulaField
				{
					name: "psiPriorAlpha"
					label: "α"
					value: "1.0"
					min: 0
					inclusive: JASP.None
					fieldWidth: 40 * preferencesModel.uiScale
				}
				FormulaField
				{
					name: "psiPriorBeta"
					label: "β"
					value: "0.5"
					min: 0
					inclusive: JASP.None
					fieldWidth: 40 * preferencesModel.uiScale
				}

				Label
				{
					text: qsTr("Correlations:")
				}
				Label
				{
					text: qsTr("ρ ~ Beta(α,β)")
				}
				FormulaField
				{
					name: "rhoPriorAlpha"
					label: "α"
					value: "1"
					min: 0
					inclusive: JASP.None
					fieldWidth: 40 * preferencesModel.uiScale
				}
				FormulaField
				{
					name: "rhoPriorBeta"
					label: "β"
					value: "1"
					min: 0
					inclusive: JASP.None
					fieldWidth: 40 * preferencesModel.uiScale
				}
			}
		}

		Common.ModerationProbes {}
    }
}
