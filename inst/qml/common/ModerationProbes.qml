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
	title: qsTr("Probe Conditional Continuous Effects")

	RadioButtonGroup
	{
		name: "moderationProbeType"
		title: qsTr("Probe method")

		RadioButton
		{
			value: "percentile"
			label: qsTr("Percentiles")
			checked: true

			ComponentsList
			{
				name: "moderationProbes"
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

		RadioButton
		{
			value: "meanSD"
			label: qsTr("Mean ± SD")

			ComponentsList
			{
				name: "moderationProbesMeanSD"
				preferredWidth: parent.width/2
				values: [-1, 0, 1]
				minimumItems: 1
				maximumItems: 10
				rowComponent: DoubleField
				{
					name: "probeSD"
					afterLabel: qsTr(" SD from the mean")
					defaultValue: rowValue
					min: -10
					max: 10
					negativeValues: true
				}
			}
		}
	}
}
