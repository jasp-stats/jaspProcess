Conditional Process Analysis
===

Conditional process analysis allows users to test moderation and mediation effects as well as combinations of both effects. In a moderation effect, the relationship between variables *X* and *Y* differs dependening on the value of a third variable *W* (the moderator). A mediation effect is a causal chain, where the effect of *X* on *Y* goes either fully or partially through a third variable *M* (the mediator). Conditional process models combine (multiple) moderation and mediation effects.

### Assumptions

#### Causal Assumptions

For details, see Kline (2012).

Conditional process models involving medation effects are *causal* models with causal effects. **Interpreting model estimates as causal effects requires the causal model to be correct**. This implies that:

- Causes must occur before their effects, e.g., if *X* -> *M* -> *Y*, then *X* must occur before *M* and *Y*.
- There are no other plausible explanations (e.g., confounding variables) that can account for statistical associations between two variables.
- The direction of causal effects is correctly specified, e.g., *X* -> *M* -> *Y* instead of *X* <- *M* <- *Y*

Furthermore, conditional process models in JASP are conceptualized as directed acyclic graphs (DAGs). Therefore the model should not contain feedback loops.

#### Structural Equation Model Assumptions

Conditional process models in JASP are conceptualized and estimated as structural equation models (SEMs) which make the following assumptions:

- Exogeneity: The parameters of the conditional distribution of dependent (endogeneous) variables given the independent (exogeneous) variables should be unrelated to the parameters of the unconditional distribution of the independent variables. This implies that all omitted causes of the dependent variables are unrelated to the independent variables (pseudo-isolation).
- Local independence: The absence of covariance between (the residual variances of) two dependent variables implies that, given the other variables in the model, the two variables should be independent. This implies that they have no omitted common causes.

#### Data Assumptions

If the estimation method is maximum likelihood, conditional process models in JASP require all dependent (endogeneous) variables in the model to follow a multivariate normal distribution. This implies that:

- All univariate distributions of dependent variables should be normal.
- All bivariate associations between dependent variables are linear.
- The distribution of residuals does not depend on the independent variables, i.e., they are homoscedastic.

### Input
---
#### Assignment Box

- Dependent: The final dependent variable of interest. It is possible to have additional dependent variables from `Continuous Predictors` but only when setting `InputType` to `Paths` and adding them in the `To` column.
- Continuous Predictors: Continuous or ordinal predictor variables.
- Categorical Predictors: Categorical independent predictor variables.

#### Models

This section allows users to specify multiple process models through one of two interfaces. New models can be added by clicking the green `+` button. For each model, the following options are available:

- Input type:
	- Paths: Models can be specified by adding and specifying paths. Paths indicate causality. For each path, there are four options
		- From: The variable from which the path starts.
		- To: The variable at which the path ends.
		- Process Type: The type process influencing the path.
			- Mediator: The path is mediated by a third variable. Adds an indirect path through the `Process Variable`.
			- Moderator: The path is moderated by a third variable. Adds a direct path from the `Process Variable` and the interaction between the `Process Variable` and `From` to the `To` variable.
			- Confounder: The path is confounded by a third variable. Adds a direct path from the `Process Variable` both to `From` and `To`.
			- Direct: Adds a direct path from `From` to `To`.
	- Hayes configuration: Models can be specified by choosing a `Hayes configuration number` and adding the required variables to assignment boxes.
		- Independent X: The independent (exogenous) variable.
		- Mediators M: Mediator variables.
		- Moderator W: First moderator variable.
		- Moderator Z: Second moderator variable.

- Residual Covariances: Adds covariances between the residuals of variables indicating that they have a common cause not specified in the model. Also adds variances for independent (exogenous) variables. This adds degrees of freedom corresponding to the number of covariances and variances added.
	- Independent variables: Adds residual (co-) variances for independent (exogenous) variables. If the model includes an interaction term between a moderator and a mediator (e.g., second stage moderated mediation), residual covariances between the (error term of the) mediator, the moderator and the interaction term are added (see Kwan & Chan, 2018).
	- Mediators: Adds residual (co-) variances for mediator variables.
	- Dependent variables: Adds residual (co-) variances for dependent variables.
- Parameter Estimates: Determines which parameter estimates are shown in the output. Requesting intercepts adds the mean structure to the model, which adds parameters and will therefore affect some fit indices (such as the information criteria AIC and BIC).
- Path plots: Displays path plots in the output.
	- Conceptual: Shows a conceptual path plot. These plots contain no parameter labels or estimates and simplify moderation effects in the model structure by omitting direct effects and interactions. Instead moderators point to the path they are moderating. These plots can be shown even when the model is not complete yet (when `Input type` is `Model number`) and are easier to understand than the statistical path plots.
	- Statistical: Shows a statistical path plot including parameter labels or estimates for all paths.
- Tests:
	- Local tests: Test the local implications of the specified causal model. Conducts (conditional) independence tests for each implication of the model. Significant independence tests indicate that the model does not correctly account for the local implication (*local misfit*) and is misspecified.
	- Test type: Type of the local test. For tetrads test types, see Kenny (1979).
		- Linear: Test local implications with (partial) correlation tests. Only allowed if all variables are continuous. Assumes that relationships are linear.
		- Loess: Test local implications with loess regression. Can also fit nonlinear relationships and categorical data.
		- Chi-square: Test local implications with chi-square tests. Suitable for categorical data.
		- Tetrads within: Test all implied vanishing tetrads assuming homogeneity within constructs.
		- Tetrads between: Test all implied vanishing tetrads assuming homogeneity between constructs.
		- Tetrads epistemic: Test all implied vanishing tetrads assuming epistemic correlations.
	- Bootstrap: Whether to compute confidence intervals using bootstrapping. Otherwise, they are estimated using normal approximation. For tetrads tests, the approximation is only valid for large samples.
	- Replications: Number of bootstrap replications.

#### Options

- Parameter labels: Display the labels of parameter in the output tables. For indirect and direct (total) effects, display the equations for the effects.
- Lavaan syntax: Show the lavaan syntax used to fit the model in the output.
- R-squared: Show *R*², the proportion of variation explained in each endogenous (outcome) variable from its predictors, in the output.
- AIC weights: Show the Akaike weights in the summary table in the output.
- BIC weigths: Show the Schwarz weights in the summary table in the output.
- Hayes configuration number: Display the configuration number according to Hayes (2022).
- Moderated mediation index: Display the index of moderated mediation (Hayes, 2015) for each moderated indirect path in a separate table for each model. For indirect paths that have dual moderation (i.e., the same moderator moderates multiple relationships) or moderated moderation, the index cannot be calculated and is therefore omitted.
- Mean-centered moderation: Continuous variables involved in moderation effects are mean-centered before entering the analysis. When `Missing Value Handling` is `Exclude cases listwise`, centering is applied only based on complete cases.
- Standardized estimates: Adds standardized parameter estimates to the output tables. The standardization is done by multiplying the estimate with (*SD*<sub>X</sub>/*SD*<sub>Y</sub>) where *SD*<sub>X</sub> and *SD*<sub>Y</sub> are the model-implied standard deviations for the independent and dependent variable, respectively. Note that the standardization of estimates of interaction effects is based on the product of the standard deviations of the individual terms and not on the standard deviation of the product term, i.e., (*SD*<sub>X</sub>*SD*<sub>W</sub>/*SD*<sub>Y</sub>) instead of (*SD*<sub>XW</sub>/*SD*<sub>Y</sub>), where *SD*<sub>W</sub> is the model-implied standard deviation of the moderator. The estimates of (conditional) indirect and total effects are also only standardized by (*SD*<sub>X</sub>/*SD*<sub>Y</sub>) but not by standard deviations of mediators or moderators. For effects involving categorical independent variables, estimates are partially standardized, i.e., only multiplied by *SD*<sub>Y</sub>. Conditional effects are probed on the unstandardized scale of moderators. See Cheung and Cheung (2023) for details.
- Confidence intervals: The level of confidence intervals for parameter estimates in the output tables.
- Method:
	- Standard: Calculates standard errors and confidence intervals based on the inverted expected information matrix.
	- Robust: Calculates robust standard errors and confidence intervals.
	- Bootstrap: Calculates bootstrap standard errors and confidence intervals.
		- Replications: Number of bootstrap replications.
		- Type: Type of boostrap.

#### Plots

- Path plots: Options for path plots.
	- Covariances: Whether to display covariances between variable nodes.
	- Residual variances: Whether to display residual covariances.
	- Parameter estimates: Whether to show parameter estimates instead of parameter labels.
	- Color palette: Color palette for node colors.
	- Legend:
		- Whether to display the full node labels in the legend.
		- Whether to display the color labels in the legend.
	- Label length: Length of abbreviated node labels.

#### Advanced

- Set for All Models: Sets options for all models at the same time. See `Models`.
- Adjustment method: Method for adjusting local tests for multiple comparisons.
- Missing Value Handling:
	- Exclude cases listwise: Only use complete cases in the model.
	- Full Information Maximum Likelihood: Computes the likelihood case-by-case using all available data. Only allowed when `Estimator` is `Auto` or `ML`. This option forces the model to add a mean structure increasing the number of parameters in the model and affecting fit indices (AIC and BIC).
	- Emulation: Sets the default values corresponding to different SEM software.
	- Estimator: The estimator for the SEMs. See lavaan manual.
- Probe Conditional Continuous Effects: The percentiles at which the continuous moderators are be probed.

### References

Ankan, A., Wortel, I. M. N., & Textor, J. (2021). Testing graphical causal models using the R package “dagitty.” *Current Protocols, 1*(2), e45. https://doi.org/10.1002/cpz1.45

Cheung, S. F., Cheung, S. H. (2023). manymome: An R package for computing the indirect effects, conditional effects, and conditional indirect effects, standardized or unstandardized, and their bootstrap confidence intervals, in many (though not all) models. *Behavior Research Methods*. https://doi.org/10.3758/s13428-023-02224-z

Hayes, A. F. (2015). An index and test of linear moderated mediation. *Multivariate Behavioral Research, 50*(1), 1–22. https://doi.org/10.1080/00273171.2014.962683

Hayes, A. F. (2022). *Introduction to mediation, moderation, and conditional process analysis* (3rd Ed.). Guilford Press.

Hoyle, R. H. (2012). *Handbook of structural equation modeling*. Guilford Press.

Kenny, D. A. (1979). *Correlation and Causality*. Wiley.

Kline, R. B. (2012). Assumptions in structural equation modeling. In *Handbook of structural equation modeling* (pp. 111–125). Guilford Press.

Kwan, J. L. Y., & Chan, W. (2018). Variable system: An alternative approach for the analysis of mediated moderation. *Psychological Methods, 23*(2), 262–277. https://doi.org/10.1037/met0000160

Rosseel, Y. (2012). lavaan: An R package for structural equation modeling. *Journal of Statistical Software, 48*(2). https://doi.org/10.18637/jss.v048.i02

### R Packages
---

- dagitty
- ggraph
- igraph
- lavaan

