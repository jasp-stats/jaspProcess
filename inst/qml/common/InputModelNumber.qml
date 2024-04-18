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
import JASP
import JASP.Controls

Group
{
	property int adjustedWidth: parent.width

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
		preferredWidth: adjustedWidth

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
		// TODO
		// AssignedVariablesList
		// {
		//     name: 				"modelNumberCovariates"
		//     title: 				qsTr("Covariates")
		//     allowedColumns: 	["scale", "ordinal"]
		// }
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
