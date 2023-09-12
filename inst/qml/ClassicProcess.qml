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
            title:			qsTr("Continuous Predictors")
            allowedColumns:	["ordinal", "scale"]
        }
        AssignedVariablesList
        {
            name:			"factors"
            title:			qsTr("Categorical Predictors")
            allowedColumns:	["ordinal", "nominal", "nominalText"]
        }
    }

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
                            minimumItems:           1
                            rowComponent: 			RowLayout
                            {
                                id: 		rowComp
                                enabled: 	rowIndex === relations.count - 1

                                Layout.columnSpan: 4
                                DropDown
                                {
                                    id: 				    procIndep
                                    name: 				    'processIndependent'
                                    source: 			    ['covariates', 'factors']
                                    controlMinWidth: 	    modelsGroup.colWidth
                                    addEmptyValue: 		    true
                                    onCurrentValueChanged:
                                    {
                                        if (currentIndex > 0 && (procVar.currentValue == currentValue || procDep.currentValue == currentValue))
                                            addControlError("Same value!")
                                        else
                                        {
                                            clearControlError()
                                            procVar.clearControlError()
											procDep.clearControlError()
                                        }
                                    }
                                }
                                DropDown
                                {
                                    id: 				procDep
                                    name: 				'processDependent'
                                    source: 			["dependent", "processVariable"] //, {name: "processRelationships.processVariable", use: "discardIndex=" + (relations.count - 1)}]
                                    controlMinWidth: 	modelsGroup.colWidth
                                    addEmptyValue: 		true
									onCurrentValueChanged:
                                    {
                                        if (currentIndex > 0 && (procVar.currentValue == currentValue || procIndep.currentValue == currentValue))
                                            addControlError("Same value!")
                                        else
                                        {
                                            clearControlError()
                                            procVar.clearControlError()
											procIndep.clearControlError()
                                        }
                                    }
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
                                    id: 				    procVar
                                    name: 				    'processVariable'
                                    enabled: 			    procType.currentValue != "directs"
                                    source: 			    procType.currentValue == 'mediators' ? ['covariates'] : ['covariates', 'factors']
                                    controlMinWidth: 	    modelsGroup.colWidth
                                    addEmptyValue: 		    true
                                    onCurrentValueChanged:
                                    {
                                        if (currentIndex > 0 && (procIndep.currentValue == currentValue || procDep.currentValue == currentValue))
                                            addControlError("Same value!")
                                        else
                                        {
                                            clearControlError()
                                            procIndep.clearControlError()
											procDep.clearControlError()
                                        }
                                    }
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
                            label: 			qsTr("Hayes model number")
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
                                title: 				qsTr("Independent X")
                                singleVariable: 	true
                            }
                            AssignedVariablesList
                            {
                                name: 				"modelNumberMediators"
                                title: 				qsTr("Mediators M")
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
								id: modelNumberModeratorW
                                name: 				"modelNumberModeratorW"
                                title: 				qsTr("Moderator W")
                                singleVariable: 	true
                            }
                            AssignedVariablesList
                            {
								id: modelNumberModeratorZ
                                name: 				"modelNumberModeratorZ"
                                title: 				qsTr("Moderator Z")
                                singleVariable: 	true
                            }
                        }
                    }

					Group
					{
                        id: opts
						title: 		qsTr("Options for %1").arg(rowValue)
						columns: 	4
                        
						Group
						{
                            title: qsTr("Residual Covariances")

							CheckBox
							{
								name: "independentCovariances"
								label: qsTr("Independent variables")
								checked: independentCovariancesForAllModels.checked
							}
							CheckBox
							{
								name: "mediatorCovariances"
								label: qsTr("Mediators")
								checked: mediatorCovariancesForAllModels.checked
							}
						}

						Group
						{
                            title: qsTr("Effects")
							columns: 	1
							CheckBox
							{
								name: "pathCoefficients"
								label: qsTr("Paths")
								checked: pathCoefficientsForAllModels.checked
							}
							CheckBox
							{
								name: "mediationEffects"
								label: qsTr("Mediation")
								checked: mediationEffectsForAllModels.checked
							}
							CheckBox
							{
								name: "totalEffects"
								label: qsTr("Total")
								checked: totalEffectsForAllModels.checked
							}
							CheckBox
							{
								name: "residualCovariances"
								label: qsTr("Residual covariances")
								checked: residualCovariancesForAllModels.checked
							}
						}

						Group
						{
                            title: qsTr("Path Plots")
							columns: 	1
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
					}
                }
            }
        }
    }

    Section
    {
        title: qsTr("Options")
        columns: 2

		GroupBox
		{
			CheckBox { label: qsTr("Parameter labels") 	;		name: "parameterLabels" }
			CheckBox { label: qsTr("Standardized estimates") ;  name: "standardizedEstimates" }
			CheckBox { label: qsTr("Lavaan syntax")     ;       name: "syntax" }
			CheckBox { label: qsTr("R-squared")         ;       name: "rSquared" }
			CheckBox { label: qsTr("AIC weights")         ;     name: "aicWeights" }
			CheckBox { label: qsTr("BIC weights")         ;     name: "bicWeights" }
		}
        GroupBox
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
                            { label: qsTr("Bias-corrected percentile"), value: "percentileBiasCorrected"   },
                            { label: qsTr("Percentile"),                value: "percentile"         },
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

		Group
		{
			title: qsTr("Path Plots")
			columns: 3

			CheckBox
			{
				name: "statisticalPathPlotsParameterEstimates"
				label: qsTr("Parameter estimates")
			}

			CheckBox
			{
				name: "pathPlotsLegend"
				label: qsTr("Legend")
			}

			IntegerField
			{
				name: "pathPlotsLabelLength"
				label: qsTr("Label length")
				defaultValue: 3
				min: 3
				max: 10
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
                    checked: 	true
                }
				CheckBox
                {
                    id:			mediatorCovariancesForAllModels
                    name: 		"mediatorCovariancesForAllModels"
                    label: 		qsTr("Mediators")
                }
			}

            Group
            {
                title: qsTr("Effects")
				columns: 1

                CheckBox
                {
                    id:			pathCoefficientsForAllModels
                    name: 		"pathCoefficientsForAllModels"
                    label: 		qsTr("Paths")
                    checked: 	true
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
                RadioButton { text: qsTr("Full Information Maximum Likelihood") ; name: "fiml" ; checked: true }
                RadioButton { text: qsTr("Exclude cases listwise")              ; name: "listwise"             }
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

		ComponentsList
		{
			name: "moderationProbes"
			title: qsTr("Probe Conditional Continuous Effects")
			preferredWidth: parent.width/2
			values: [16, 50, 84]
			minimumItems: 1
			maximumItems: 10
			rowComponent: DoubleField
			{
				name: "probePercentile"
				afterLabel: qsTr("th percentile")
				defaultValue: rowValue
				min: 0
				max: 100
			}
		}
    }
}
