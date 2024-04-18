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
	title: qsTr("Path Plots")
	columns: 1

	Group
	{
		columns: 3
		CheckBox
		{
			name: "statisticalPathPlotsCovariances"
			label: qsTr("Covariances")
		}

		CheckBox
		{
			name: "statisticalPathPlotsResidualVariances"
			label: qsTr("Residual variances")
		}

		CheckBox
		{
			name: "statisticalPathPlotsParameterEstimates"
			label: qsTr("Parameter estimates")
		}
	}

	Group {
		Group
		{
			title: qsTr("Legend")
			columns: 2

			CheckBox
			{
				name: "pathPlotsLegendLabels"
				label: qsTr("Labels")
			}
			CheckBox
			{
				name: "pathPlotsLegendColor"
				label: qsTr("Color")
			}
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
