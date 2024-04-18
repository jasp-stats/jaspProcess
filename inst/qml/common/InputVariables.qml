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

Group
{
	property int adjustedWidth: parent.width

	id: 		modelsGroup

	property int colWidth: parent.width / 4 - jaspTheme.contentMargin
	property int labelWidth: modelsGroup.colWidth + jaspTheme.contentMargin

	RowLayout
	{
		anchors.left: 		parent.left
		anchors.margins: 	jaspTheme.contentMargin
		Label
		{
			text: qsTr("From")
			Layout.preferredWidth: modelsGroup.labelWidth
		}
		Label
		{
			
			text: qsTr("To")
			Layout.preferredWidth: modelsGroup.labelWidth
		}
		Label
		{
			text: qsTr("Process Type")
			Layout.preferredWidth: modelsGroup.labelWidth
		}
		Label
		{
			text: qsTr("Process Variable")
			Layout.preferredWidth: modelsGroup.labelWidth
		}
	}

	ComponentsList
	{
		id: 					relations
		name: 					"processRelationships"
		preferredWidth: 		adjustedWidth
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
