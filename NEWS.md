# ggDoE 0.7.2

CHANGES

* Added new plot to visualize collinearity through variance inflation factor (VIF) in `diagnostic_plots()` function. This is the sixth diagnostic plot.

---

# ggDoE 0.7.1

BREAKING

* in `gg_rsm()` restricted model class to only be an `rsm` object. Future updates will add back
`lm` class object option.

BUG FIXES

* added error message to `diagnostic_plots()` to check if there is insufficient degrees of freedom to obtain standard errors, t.value. It won't plot any of the diagnostic plots for that model.

MINOR CHANGES

* Consistent use of left-arrow operator (<-) for all functions in this package

---

# ggDoE 0.7

* Added a `NEWS.md` file to track changes to the package.

CURRENT FUNCTIONS

* `alias_matrix()`  Color Map on Correlations 

* `boxcox_transform()` Box-Cox transformation plot with 95\% confidence interval of lambda values to consider 

* `gg_boxplots()` Boxplots created with ggplot2

* `diagnostic_plots()` Regression Diagnostic Plots with ggplot2

* `gg_rsm()` Contour plot(s) of a fitted linear model in ggplot2 from an RSM object

* `half_normal()` A  halfnormal plot for factorial effects

* `lambda_plot()` Lambda plot for tracing t-staitics across different values of lambda

* `main_effects()` Main effects plots for each factor in a design given a specified response

* `pareto_plot()` A bar plot with ordered effects, margin of error (ME) and simultaneous margin of error (SME) cutoffs.

* `viridisPalette()` A simple wrapper for the `viridisLite` package used for changing the color palette in most plots above
