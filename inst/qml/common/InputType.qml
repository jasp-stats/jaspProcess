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

RadioButtonGroup
{
	property string modelName: ""

	name: 					"inputType"
	title: 					qsTr("Input type for %1").arg(modelName)
	radioButtonsOnSameRow: 	true
	columns: 				2

	RadioButton
	{
		value: 		"inputVariables"
		label: 		qsTr("Variables")
		checked: 	true
	}
	RadioButton
	{
		value: 	"inputModelNumber"
		label: 	qsTr("Model number")
	}
}
