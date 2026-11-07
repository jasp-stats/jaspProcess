# jaspProcess Changelog

> **HOW TO READ AND UPDATE THIS CHANGELOG:**
> 
> This document follows a modified [Keep a Changelog](https://keepachangelog.com/) format adapted for the R/JASP ecosystem. Releases are listed in reverse chronological order (newest first).
> As an example see [jaspModuleTemplate](https://github.com/jasp-stats/jaspModuleTemplate/blob/master/NEWS.md)
> * **Adding New Changes (For Contributors):** All new commits should be logged at the very top of the file under the `# jaspModuleTemplate (development version)` header. Place your bullet point under the appropriate category (`## Added`, `## Fixed`, etc.). 
> * **Issue References:** Please reference the relevant GitHub Issue (if any) at the end of your line (e.g., `([Issue #19](https://github.com/jasp-stats/jaspModuleTemplate/issues/19)`). 
> * **Format Categories:** >   * **Added:** New template features, QML examples, or build tools.
>   * **Changed:** Updates to default configurations, boilerplate code, or dependencies. 
>   * **Fixed:** Bug fixes in the build pipeline, R wrappers, or QML layouts.
>   * **Deprecated / Removed:** Outdated template components or legacy code.


---

# jaspProcess (development version)

## Added
* Covariates box for the "Hayes configuration" input type. As in PROCESS, covariates are added to every mediator and outcome equation, so the path coefficients, effects, and R² are adjusted for them. Covariates can be continuous or categorical, and they are left out of the mediation and total effects tables and the conceptual path plot ([Issue #63](https://github.com/jasp-stats/jaspProcess/issues/63), [Issue #4388](https://github.com/jasp-stats/jasp-issues/issues/4388), [Issue #3481](https://github.com/jasp-stats/jasp-issues/issues/3481)).
* The model summary table adds a footnote when the compared models differ in the variables they describe or in the cases used, because their information criteria are then not comparable. With `Independent variables` selected under `Residual Covariances` (the default), this includes models with different independent variables, moderators, or covariates, because the likelihood then also describes these variables.

## Fixed
* Process models with a categorical predictor no longer error when a factor level label contains a space or other special character (e.g. "emotion regulation"). Such labels are now coerced to syntactically valid names before being used in mean-centering and lavaan model syntax ([Issue #3597](https://github.com/jasp-stats/jasp-issues/issues/3597)).
* With listwise deletion, mean-centering of moderators and the moderator probe values now use each model's own complete cases. Previously, missing values in any other variable in the variable boxes (for example a covariate used only in another model) restricted the centering sample and changed the estimates of models with complete data.
* The Bayesian analysis now fits the mean-centered data when "Mean-centered moderation" is selected (the default). Previously only the moderator probe values were centered, so conditional effects were evaluated at moderator values shifted by the moderator mean.
* Factor levels that contain two consecutive underscores or end with an underscore now stop the analysis with a message asking to rename them, instead of causing an unrelated error later.
* The Bayesian analysis no longer stops with an error when the models have different numbers of cases, for example without independent covariances and with a covariate with missing values in only one model. The LOO difference is then left out of the model summary table.

---

# jaspModuleTemplate 0.2.0
## Added
* Added NEWS.md
* Added workflow to remind users to update their `NEWS.md`.
* Added workflow to auto-bump version when user does not do so.

---

# jaspModuleTemplate 0.1.0

## Added
* Initial examples to showcase JASP module development

## Changed
* Use best practices for checking input ([Issue #19](https://github.com/jasp-stats/jaspModuleTemplate/issues/19)).
* The main results table now defaults to displaying 95% Confidence Intervals for effect sizes.

## Fixed
* Remove deprecated dependencies from qml files ([Issue #14](https://github.com/jasp-stats/jaspModuleTemplate/issues/14)).
