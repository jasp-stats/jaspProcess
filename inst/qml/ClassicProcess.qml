
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
		columns: 2

		TabView
		{
			id:					models
			name:				"processModels"
			maximumItems:		10
			newItemName:		qsTr("Model 1")
			optionKey:			"name"

			content: Group
			{
				RadioButtonGroup
				{
					name: "inputType"
					title: qsTr("Input type")
					radioButtonsOnSameRow: true
					columns: 3

					RadioButton{
						id: variables
						value: "inputVariables"
						label: qsTr("Variables")
						checked: true
					}
					RadioButton{
						id: modelNumber
						value: "inputModelNumber"
						label: qsTr("Model number")
					}
					RadioButton{
						id: syntax
						value: "inputSyntax"
						label: qsTr("Syntax")
					}
				}

				Group
				{
					visible: variables.checked

					RowLayout
					{
						Label { text: qsTr("To"); }
						Label { text: qsTr("From")}
						Label { text: qsTr("Process Type")}
						Label { text: qsTr("Process Variable")}
					}

					ComponentsList
					{
						name: "processRelationships"
						
						preferredWidth: models.width
						rowComponent: RowLayout
						{
							Layout.columnSpan: 4
							DropDown
							{
								name: 'processDependent'
								source: ['dependent', "processVariable"]
							}
							DropDown
							{
								name: 'processIndependent'
								source: ['covariates', 'factors']
							}							
							DropDown
							{
								name: 'processType'
								values: 
								[
									{ label: qsTr("Mediator"), value: 'mediators'},
									{ label: qsTr("Moderator"), value: 'moderators'},
									{ label: qsTr("Confounder"), value: 'confounders'},
									{ label: qsTr("Direct"), value: 'directs'}
								]
							}
							DropDown
							{
								name: 'processVariable'
								source: ['covariates', 'factors']
							}
						}
					}
				}

				DropDown
				{
					name: "modelNumber"
					label: qsTr("Model number")
					values: [1, 2, 3, 4, 5]
					visible: modelNumber.checked
				}

				TextArea 
				{
					name: "syntax"
					width: models.width
					textType: JASP.TextTypeLavaan
					visible: syntax.checked
				}
			}
		}
	}
}
