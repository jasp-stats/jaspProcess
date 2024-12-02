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
				childControlsArea.anchors.leftMargin: jaspTheme.contentMargin

				Common.InputType
				{
					id: inputType
					modelName: rowValue
				}

				Common.Separator {}

				Common.InputVariables
				{
					visible: inputType.value == "inputVariables"
					adjustedWidth: models.width - 2 * jaspTheme.contentMargin
				}

				Common.InputModelNumber
				{
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

					Group
					{
						title: qsTr("Tests")
						columns: 	1
						CheckBox
						{
							name: "localTests"
							label: qsTr("Local tests")
							checked: localTestsForAllModels.checked

							Label
							{
								text: qsTr("Test type")
							}
							DropDown
							{
								id: localTestType
								name: "localTestType"
								label: ""
								values:
								[
									{ label: qsTr("Linear"), 			value: "cis" 				},
									{ label: qsTr("Loess"), 			value: "cis.loess" 			},
									{ label: qsTr("Chi-square"), 		value: "cis.chisq" 			},
									{ label: qsTr("Tetrad"), 			value: "tetrads" 			},
									{ label: qsTr("Tetrad within"), 	value: "tetrads.within" 	},
									{ label: qsTr("Tetrad between"), 	value: "tetrads.between" 	},
									{ label: qsTr("Tetrad epistemic"), 	value: "tetrads.epistemic" 	}
								]
								currentValue: localTestTypeForAllModels.currentValue
							}
							CheckBox
							{
								label: qsTr("Bootstrap")
								name: "localTestBootstrap"
								checked: localTestType.currentValue == "cis.loess" || localTestBootstrapForAllModels.checked
								Label
								{
									text: qsTr("Replications")
								}
								IntegerField
								{
									name: "localTestBootstrapSamples"
									defaultValue: localTestBootstrapSamplesForAllModels.defaultValue
									min: 500
									max: 100000
								}
							}
						}
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

	Section
	{
		title: qsTr("Options")
		columns: 3

		Group
		{
			CheckBox { label: qsTr("Parameter labels");				name: "parameterLabels" }
			CheckBox { label: qsTr("Lavaan syntax");       			name: "syntax" 			}
			CheckBox { label: qsTr("R-squared");       				name: "rSquared" 		}
			CheckBox { label: qsTr("AIC weights");     				name: "aicWeights" 		}
			CheckBox { label: qsTr("BIC weights");     				name: "bicWeights" 		}
			CheckBox { label: qsTr("Hayes configuration number"); 	name: "hayesNumber";  	}
		}
		Group
		{
			CheckBox
			{
				label: qsTr("Mean-centered moderation")
				name: "meanCenteredModeration"
			}
			CheckBox
			{
				label: qsTr("Standardized estimates")
				name: "standardizedModelEstimates"
			}
		}
		Group
		{
			CIField {
				text: qsTr("Confidence intervals")
				name: "ciLevel"
			}
			RadioButtonGroup {
				title: qsTr("Method")
				name: "errorCalculationMethod"
				RadioButton { text: qsTr("Standard")  ; name: "standard" ; checked: true }
				RadioButton { text: qsTr("Robust")    ; name: "robust" }
				RadioButton {
					text: qsTr("Bootstrap")
					name: "bootstrap"
					IntegerField {
						text: qsTr("Replications")
						name: "bootstrapSamples"
						defaultValue: 1000
						min: 500
						max: 100000
					}
					DropDown {
						label: qsTr("Type")
						name: "bootstrapCiType"
						values: [
							{ label: qsTr("Percentile"),                value: "percentile"         },
							{ label: qsTr("Bias-corrected percentile"), value: "percentileBiasCorrected"   },
							{ label: qsTr("Normal theory"),             value: "normalTheory"         }
						]
					}
				}
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
			preferredWidth: parent.width

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
					label: 		qsTr("Indirect")
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

			Group
			{
				title: qsTr("Tests")
				columns: 	1
				CheckBox
				{
					id: localTestsForAllModels
					name: "localTestsForAllModels"
					label: qsTr("Local tests")

					Label
					{
						text: qsTr("Test type")
					}
					DropDown
					{
						id: localTestTypeForAllModels
						name: "localTestTypeForAllModels"
						label: ""
						values:
						[
							{ label: qsTr("Linear"), 			value: "cis" 				},
							{ label: qsTr("Loess"), 			value: "cis.loess" 			},
							{ label: qsTr("Chi-square"), 		value: "cis.chisq" 			},
							{ label: qsTr("Tetrad"), 			value: "tetrads" 			},
							{ label: qsTr("Tetrad within"), 	value: "tetrads.within" 	},
							{ label: qsTr("Tetrad between"), 	value: "tetrads.between" 	},
							{ label: qsTr("Tetrad epistemic"), 	value: "tetrads.epistemic" 	}
						]
					}
				}
				CheckBox
				{
					id: localTestBootstrapForAllModels
					label: qsTr("Bootstrap")
					name: "localTestBootstrapForAllModels"
					checked: localTestTypeForAllModels.currentValue == "cis.loess"
					Label
					{
						text: qsTr("Replications")
					}
					IntegerField
					{
						id: localTestBootstrapSamplesForAllModels
						name: "localTestBootstrapSamplesForAllModels"
						defaultValue: 1000
						min: 500
						max: 100000
					}
				}
				Label
				{
					text: qsTr("Adjustment method")
				}
				DropDown
				{
					name: "localTestCorrectionForAllModels"
					label: ""
					values:
					[
						{ label: qsTr("None"), 					value: "none" 		},
						{ label: qsTr("Holm"), 					value: "holm" 		},
						{ label: qsTr("Hochberg"), 				value: "hochberg" 	},
						{ label: qsTr("Hommel"), 				value: "hommel" 	},
						{ label: qsTr("Bonferroni"), 			value: "bonferroni" },
						{ label: qsTr("Benjamini-Hochberg"), 	value: "BH" 		},
						{ label: qsTr("Benjamini-Yekuteli"), 	value: "BY" 		}
					]
					startValue: "bonferroni"
				}
			}
		}

		Group
		{
			columns: 3

			RadioButtonGroup
			{
				title: qsTr("Missing Value Handling")
				name: "naAction"
				RadioButton { text: qsTr("Exclude cases listwise")              ; name: "listwise" ; checked: true 	}
				RadioButton { text: qsTr("Full Information Maximum Likelihood") ; name: "fiml"  					}
			}
			RadioButtonGroup
			{
				title: qsTr("Emulation")
				name: "emulation"
				RadioButton { text: qsTr("None")  ; name: "lavaan"  ; checked: true }
				RadioButton { text: qsTr("Mplus") ; name: "mplus" }
				RadioButton { text: qsTr("EQS")   ; name: "eqs"   }
			}
			RadioButtonGroup
			{
				title: qsTr("Estimator")
				name: "estimator"
				RadioButton { text: qsTr("Auto") ; name: "default"; checked: true }
				RadioButton { text: qsTr("ML")   ; name: "ml"       }
				RadioButton { text: qsTr("GLS")  ; name: "gls"      }
				RadioButton { text: qsTr("WLS")  ; name: "wls"      }
				RadioButton { text: qsTr("ULS")  ; name: "uls"      }
				RadioButton { text: qsTr("DWLS") ; name: "dwls"     }
			}
		}

		Common.ModerationProbes {}
	}
}
