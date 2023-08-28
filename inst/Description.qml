import QtQuick		2.12
import JASP.Module	1.0

Description
{
	name		: "jaspProcess"
	title		: qsTr("Process")
	description	: qsTr("Test and compare causal and conditional process models.")
	icon		: "process.svg"
	version		: "0.17.1"
	author		: "JASP Team and Netherlands eScience Center"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "https://jasp-stats.org"
	license		: "GPL (>= 2)"

	Analysis
	{
		title	:	qsTr("Classical Process Model")
		func	:	"ClassicProcess"
		qml		:	"ClassicProcess.qml"
	}
	// Analysis
	// {
	// 	title	:	qsTr("Baysian Process Model")
	// 	func	:	"BayesProcess"
	// 	qml		:	"BayesProcess.qml"
	// }
}
