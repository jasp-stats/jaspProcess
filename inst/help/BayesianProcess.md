Bayesian Conditional Process Analysis
===

Bayesian conditional process analysis allows users to test moderation and mediation effects as well as combinations of both effects. In a moderation effect, the relationship between variables *X* and *Y* differs dependening on the value of a third variable *W* (the moderator). A mediation effect is a causal chain, where the effect of *X* on *Y* goes either fully or partially through a third variable *M* (the mediator). Conditional process models combine (multiple) moderation and mediation effects.

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

Bayesian conditional process models in JASP require all dependent (endogeneous) variables in the model to follow a multivariate normal distribution. This implies that:

- All univariate distributions of dependent variables should be normal.
- All bivariate associations between dependent variables are linear.
- The distribution of residuals does not depend on the independent variables, i.e., they are homoscedastic.

### Input
---
#### Assignment Box

- Dependent: The final dependent variable of interest.
- Continuous Predictors: Continuous or ordinal predictor variables.
- Categorical Predictors: Categorical independent predictor variables.

#### Models

This section allows users to specify multiple process models through one of two interfaces. New models can be added by clicking the green `+` button. For each model, the following options are available:

- Input type:
	- Variables: Models can be specified by adding and specifying paths. Paths indicate causality. For each path, there are four options
		- From: The variable from which the path starts.
		- To: The variable at which the path ends.
		- Process Type: The type process influencing the path.
			- Mediator: The path is mediated by a third variable. Adds an indirect path through the `Process Variable`.
			- Moderator: The path is moderated by a third variable. Adds a direct path from the `Process Variable` and the interaction between the `Process Variable` and `From` to the `To` variable.
			- Confounder: The path is confounded by a third variable. Adds a direct path from the `Process Variable` both to `From` and `To`.
			- Direct: Adds a direct path from `From` to `To`.
	- Model number: Models can be specified by choosing a `Hayes model number` and adding the required variables to assignment boxes.
		- Independent X: The independent (exogenous) variable.
		- Mediators M: Mediator variables.
		- Moderator W: First moderator variable.
		- Moderator Z: Second moderator variable.

- Residual Covariances: Adds covariances between the residuals of variables indicating that they have a common cause not specified in the model. Also adds variances for independent (exogenous) variables. This adds degrees of freedom corresponding to the number of covariances and variances added.
- Parameter Estimates: Determines which parameter estimates are shown in the output. Requesting intercepts adds the mean structure to the model, which adds parameters and will therefore affect some fit indices (such as the information criteria AIC and BIC).
- Path plots: Displays path plots in the output.
	- Conceptual: Shows a conceptual path plot. These plots contain no parameter labels or estimates and simplify moderation effects in the model structure by omitting direct effects and interactions. Instead moderators point to the path they are moderating. These plots can be shown even when the model is not complete yet (when `Input type` is `Model number`) and are easier to understand than the statistical path plots.
	- Statistical: Shows a statistical path plot including parameter labels or estimates for all paths.

#### Options

- Burnin: Number of burnin samples drawn for each MCMC chain.
- Samples: Number of samples drawn for each MCMC chain.
- Chains: Number of chains in the MCMC algorithm.
- Repeatability: Seed to exactly reproduce the output.
- Prior distributions: Show the prior distributions for path coefficients in the output. The distributions can be modified in the `Advanced` section.
- Parameter labels: Display the labels of parameter in the output tables. For indirect and direct (total) effects, display the equations for the effects.
- Lavaan syntax: Show the lavaan syntax used to fit the model in the output.
- Credible intervals: The level of credible intervals in the output tables. Credible intervals are based on quantiles of the posterior samples.

#### Plots

- Color palette: Color palette for plots.
- MCMC plots: Options for displaying posterior distributions and MCMC diagnostics.
	- Aggregate chains for densities and histograms: If checked, the samples of different chains are aggregated in density plots and histograms. If unchecked, there are separate colors per chain.
	- Density: Show a density plot of the posterior samples.
	- Histogram: Show a histogram of the posterior samples.
	- Trace: Show a trace plot of the posterior samples.
	- Autocorrelation: Plot the autocorrelation of the posterior samples.
		- No. lags: the maximum number of lags to show in the autocorrelation plot.
		- Type: whether to display the autocorrelation as a bar at each lag, or as a line that connects subsequent lags.
	- Bivariate scatter: Show a bivariate scatter plot of all pairs of variables. Only shows output when more than 1 parameter is sampled.
		- Diagonal plot type: Show a density plot or a histogram on the diagonal entries of the scatter plot.
		- Off-diagonal plot type: Show a hexagonal bivariate density plot, or a contour plot on the off-diagonal entries of the scatter plot.
- Path plots: Options for path plots.
	- Covariances: Whether to display covariances between variable nodes.
	- Residual variances: Whether to display residual covariances.
	- Parameter estimates: Whether to show parameter estimates instead of parameter labels.
	- Legend:
		- Whether to display the full node labels in the legend.
		- Whether to display the color labels in the legend.
	- Label length: Length of abbreviated node labels.

#### Advanced

- Set for All Models: Sets options for all models at the same time. See `Models`.
- Prior distributions: Set prior distributions for different types of parameters.
	- Intercepts: Location and scale parameters of normal priors on intercepts (v).
	- Path coefficients: Location and scale parameters of normal priors on path coefficients (β).
	- Standard deviatons: Shape and rate parameters of gamma priors on standard deviations of the decomposed covariance matrix.
	- Standard deviatons: Shape and rate parameters of gamma priors on correlations of the decomposed covariance matrix.
- Probe Conditional Continuous Effects: The percentiles at which the continuous moderators are be probed.

### References

Hayes, A. F. (2022). *Introduction to mediation, moderation, and conditional process analysis* (3rd Ed.). Guilford Press.

Hoyle, R. H. (2012). *Handbook of structural equation modeling*. Guilford Press.

Kenny, D. A. (1979). *Correlation and Causality*. Wiley.

Kline, R. B. (2012). Assumptions in structural equation modeling. In *Handbook of structural equation modeling* (pp. 111–125). Guilford Press.

Merkle, E. C., Fitzsimmons, E., Uanhoro, J., & Goodrich, B. (2021). Efficient Bayesian structural equation modeling in Stan. *Journal of Statistical Software, 100*(6). https://doi.org/10.18637/jss.v100.i06

Merkle, E. C., & Rosseel, Y. (2018). blavaan: Bayesian structural equation models via parameter expansion. *Journal of Statistical Software, 85*(4). https://doi.org/10.18637/jss.v085.i04

Rosseel, Y. (2012). lavaan: An R package for structural equation modeling. *Journal of Statistical Software, 48*(2). https://doi.org/10.18637/jss.v048.i02

### R Packages
---

- ggraph
- igraph
- lavaan
- blavaan
- rstan
- loo
