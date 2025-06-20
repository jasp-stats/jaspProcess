import QtQuick
import JASP.Module

Description
{
	name		: "jaspProcess"
	title		: qsTr("Process")
	description	: qsTr("Test and compare causal and conditional process models.")
	icon		: "classical-process.svg"
	version			: "0.95.0"
	author		: "JASP Team and Netherlands eScience Center"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "https://jasp-stats.org"
	license		: "GPL (>= 2)"
	preloadData : true

	GroupTitle
	{
		title	: 	qsTr("Classical")
		icon	: 	"classical-process.svg"
	}

	Analysis
	{
		title	:	qsTr("Classical Process Model")
		func	:	"ClassicProcess"
		qml		:	"ClassicProcess.qml"
	}

	Separator {}

	GroupTitle
	{
		title	: 	qsTr("Bayesian")
		icon	: 	"bayesian-process.svg"
	}

	Analysis
	{
		title	:	qsTr("Bayesian Process Model")
		func	:	"BayesianProcess"
		qml		:	"BayesianProcess.qml"
	}
}
