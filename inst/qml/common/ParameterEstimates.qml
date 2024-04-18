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
	property bool pathCoefficientsChecked: 		true
	property bool interceptsChecked: 			false
	property bool mediationEffectsChecked: 		true
	property bool totalEffectsChecked: 			true
	property bool residualCovariancesChecked: 	false

	title: qsTr("Parameter Estimates")
	// columns: 	1
	CheckBox
	{
		name: "pathCoefficients"
		label: qsTr("Paths")
		checked: pathCoefficientsForAllModels.checked
		
		CheckBox
		{
			name: "intercepts"
			label: qsTr("Intercepts")
			checked: interceptsForAllModels.checked
		}
	}
	CheckBox
	{
		name: "mediationEffects"
		label: qsTr("Indirect")
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
