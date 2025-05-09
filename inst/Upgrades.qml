import QtQuick
import JASP.Module

Upgrades
{
	Upgrade
	{
		functionName: "ClassicProcess"
		fromVersion: "0.18.3"
		toVersion: "0.19.0"
		msg: "From version 0.19.0, Process computes standardized estimates via the model-implied covariance matrix instead of standardizing variables before entering the analysis. Standardized estimates are added to the table of unstandardized estimates."

		ChangeCopy
		{
			from: "standardizedEstimates"
			to: "standardizedModelEstimates"
		}

		ChangeCopy
		{
			from: "standardizedEstimates"
			to: "meanCenteredModeration"
		}

		ChangeJS
		{
			name: "standardizedModelEstimates"
			jsFunction: function(options)
			{
				return options["standardizedEstimates"] === "standardized";
			}
		}

		ChangeJS
		{
			name: "meanCenteredModeration"
			jsFunction: function(options)
			{
				return options["standardizedEstimates"] === "centered";
			}
		}
	}
}
