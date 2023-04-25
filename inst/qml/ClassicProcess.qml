
import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls

Form
{

	// Formula
	// {
	// 	lhs: "dependent"
	// 	rhs: [{ name: "modelTerms", extraOptions: "isNuisance" }]
	// 	userMustSpecify: "covariates"
	// }

	VariablesForm
	{

		AvailableVariablesList { name: "allVariablesList" }
		AssignedVariablesList 
		{
			name:				"dependent"
			title:				qsTr("Dependent Variable")
			suggestedColumns:	["scale"]
			singleVariable:		true
		}
		AssignedVariablesList
		{
			name:			"covariates"
			title:			qsTr("Covariates")
			allowedColumns:	["ordinal", "scale"]
		}
		AssignedVariablesList
		{
			name:			"factors"
			title:			qsTr("Factors")
			allowedColumns:	["ordinal", "nominal", "nominalText"]
		}
	}

	Section
	{
		title: qsTr("Models")
		columns: 1

		Group
		{
			title: qsTr("Set for all models")
			columns: 2

			Group
			{
				CheckBox
				{
					id:			pathcoefficientsForAllModels
					name: 		"pathCoefficientsForAllModels"
					label: 		qsTr("Path coefficients")
					checked: 	true
				}

				CheckBox
				{
					id:			mediationEffectsForAllModels
					name: 		"mediationEffectsForAllModels"
					label: 		qsTr("Mediation effects")
					checked: 	true
				}
			}

			Group
			{
				CheckBox
				{
					id:			conceptualPathPlotsForAllModels
					name: 		"conceptualPathPlotsForAllModels"
					label: 		qsTr("Conceptual path plots")
					checked: 	true
				}
				CheckBox
				{
					id:			statisticalPathPlotsForAllModels
					name: 		"statisticalPathPlotsForAllModels"
					label: 		qsTr("Statistical path plots")
				}
			}
		}

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
					anchors.left: 		parent.left
					anchors.margins: 	jaspTheme.contentMargin
					RadioButtonGroup
					{
						name: 					"inputType"
						title: 					qsTr("Input type for %1").arg(rowValue)
						radioButtonsOnSameRow: 	true
						columns: 				2

						RadioButton{
							id: 		variables
							value: 		"inputVariables"
							label: 		qsTr("Variables")
							checked: 	true
						}
						RadioButton{
							id: 	modelNumber
							value: 	"inputModelNumber"
							label: 	qsTr("Model number")
						}
					}
					Group
					{
						title: ""
						Rectangle
						{
							id: 			separator
							border.width: 	1
							height: 		2
							width: 			models.width - 2 * jaspTheme.contentMargin
							border.color: 	jaspTheme.gray
						}
					}

					Group
					{
						id: 		modelsGroup
						visible: 	variables.checked

						property int colWidth: (models.width - 3 * 40 * preferencesModel.uiScale) / 4 

						RowLayout
						{
							anchors.left: 		parent.left
							anchors.margins: 	jaspTheme.contentMargin
							Label 
							{
								text: qsTr("From")
								Layout.preferredWidth: modelsGroup.colWidth
							}
							Label
							{
								text: qsTr("To")
								Layout.preferredWidth: modelsGroup.colWidth
							}
							Label
							{
								text: qsTr("Process Type")
								Layout.preferredWidth: modelsGroup.colWidth
							}
							Label
							{
								text: qsTr("Process Variable")
								Layout.preferredWidth: modelsGroup.colWidth
							}
						}

						ComponentsList
						{
							id: 					relations
							name: 					"processRelationships"
							preferredWidth: 		models.width - 2 * jaspTheme.contentMargin
							itemRectangle.color: 	jaspTheme.controlBackgroundColor
							rowComponent: 			RowLayout
							{
								id: 		rowComp
								enabled: 	rowIndex === relations.count - 1
								
								Layout.columnSpan: 4
								DropDown
								{
									id: 				procIndep
									name: 				'processIndependent'
									source: 			['covariates', 'factors']
									controlMinWidth: 	modelsGroup.colWidth
									addEmptyValue: 		true
								}
								DropDown
								{
									id: 				procDep
									name: 				'processDependent'
									source: 			['dependent', "processVariable"]
									controlMinWidth: 	modelsGroup.colWidth
									addEmptyValue: 		true
								}							
								DropDown
								{
									id: 	procType
									name: 	'processType'
									values: 
									[
										{ label: qsTr("Mediator"), 		value: 'mediators'		},
										{ label: qsTr("Moderator"), 	value: 'moderators'		},
										{ label: qsTr("Confounder"), 	value: 'confounders'	},
										{ label: qsTr("Direct"), 		value: 'directs'		}
									]
									controlMinWidth: 	modelsGroup.colWidth
									addEmptyValue: 		true
									onCurrentValueChanged: 
									{
										if (currentValue == "directs")
										{
											procVar.currentValue = ""
										}
									}
								}
								DropDown
								{
									id: 				procVar
									name: 				'processVariable'
									enabled: 			procType.currentValue != "directs"
									source: 			procType.currentValue == 'mediators' ? ['covariates'] : ['covariates', 'factors']
									controlMinWidth: 	modelsGroup.colWidth
									addEmptyValue: 		true
								}
								function enableAddItemManually()
								{
									var nextItem = relations.count == 0 || (procDep.currentValue != "" && 
										procIndep.currentValue != "" && procType.currentValue != "" && 
										((procType.currentValue != "directs" && procVar.currentValue != "") || 
										(procType.currentValue == "directs" && procVar.currentValue == ""))) ;

									return nextItem
								}
								onParentChanged: 
								{
									if (parent)
									{
										parent.isDeletable = 		Qt.binding(function() { return rowComp.enabled })
										relations.addItemManually = Qt.binding(enableAddItemManually)
									}
								}
							}
						}
					}
					Group
					{
						visible: modelNumber.checked

						IntegerField
						{
							name: 			"modelNumber"
							label: 			qsTr("Model number")
							defaultValue: 	1
							min: 			1
							max: 			92
						}
						VariablesForm
						{
							preferredWidth: models.width - 2 * jaspTheme.contentMargin
							AvailableVariablesList
							{
								name: 		"allAssignedVars"
								source: 	["covariates", "factors"]
								
							}
							AssignedVariablesList
							{
								name: 				"modelNumberIndependent"
								title: 				qsTr("Independent")
								singleVariable: 	true
							}
							AssignedVariablesList
							{
								name: 				"modelNumberMediators"
								title: 				qsTr("Mediators")
								allowedColumns: 	["scale", "ordinal"]
							}
							AssignedVariablesList
							{
								name: 				"modelNumberCovariates"
								title: 				qsTr("Covariates")
								allowedColumns: 	["scale", "ordinal"]
							}
							AssignedVariablesList
							{
								name: 				"modelNumberModeratorW"
								title: 				qsTr("Moderator Z")
								singleVariable: 	true
							}
							AssignedVariablesList
							{
								name: 				"modelNumberModeratorZ"
								title: 				qsTr("Moderator W")
								singleVariable: 	true
							}
						}
					}

					Group
					{
						title: qsTr("Path plots for %1").arg(rowValue)
						columns: 2
						CheckBox
						{
							name: "conceptualPathPlot"
							label: qsTr("Conceptual")
							checked: conceptualPathPlotsForAllModels.checked
						}
						CheckBox
						{
							name: "statisticalPathPlot"
							label: qsTr("Statistical")
							checked: statisticalPathPlotsForAllModels.checked
						}
					}
				}
			}
		}
	}

	Section
	{
		title: qsTr("Options")
		columns: 2

		CheckBox
		{
			name: "parameterLabels"
			label: qsTr("Parameter labels")
		}

		CIField
		{
			name: "ciLevel"
			label: qsTr("Confidence intervals")
		}
	}

	Section
	{
		title: qsTr("Plots")
		columns: 2

		CheckBox
		{
			name: "statisticalPathPlotsParameterEstimates"
			label: qsTr("Parameter estimates")
		}
	}
}
