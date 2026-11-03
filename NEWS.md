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
* Residual plots for each model (`Residuals vs. predicted` and `Q-Q plot standardized residuals`), shown for every dependent variable (the outcome and each mediator), to check the homoscedasticity and normality assumptions of the regression equations. Available in the classical and Bayesian analysis, per model or for all models at once. As in the other JASP analyses, the Q-Q plot can show a confidence band (classical) or the credible interval of each residual (Bayesian).

## Fixed
* Requesting output for a single model (e.g., a plot or a parameter estimates table) no longer recomputes the model summary and R² tables. In the Bayesian analysis, this avoids recomputing WAIC and LOO for every model.
* Process models with a categorical predictor no longer error when a factor level label contains a space or other special character (e.g. "emotion regulation"). Such labels are now coerced to syntactically valid names before being used in mean-centering and lavaan model syntax ([Issue #3597](https://github.com/jasp-stats/jasp-issues/issues/3597)).

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
