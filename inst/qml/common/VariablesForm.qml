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
