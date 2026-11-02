//
// Copyright (C) 2026 University of Amsterdam
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
	property bool residualVsFittedPlotChecked: 	false
	property bool residualQqPlotChecked: 		false

	title: qsTr("Residual Plots")
	columns: 	1
	CheckBox
	{
		name: "residualVsFittedPlot"
		label: qsTr("Residuals vs. predicted")
		checked: residualVsFittedPlotChecked
		info: qsTr("For each dependent variable (the outcome and each mediator), a scatterplot of the residuals against the predicted values. Under constant residual variance (homoscedasticity), the residuals scatter evenly around zero.")
	}
	CheckBox
	{
		name: "residualQqPlot"
		label: qsTr("Q-Q plot standardized residuals")
		checked: residualQqPlotChecked
		info: qsTr("For each dependent variable (the outcome and each mediator), a Q-Q plot of the standardized residuals. If the residuals are normally distributed, the points lie close to the diagonal.")
	}
}
