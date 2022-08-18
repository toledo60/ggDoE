# ggDoE 0.7.8

BREAKING

* `gg_boxplots()` arguments `response`,`factor`,`group_var` are now string characters 

CHANGES

* removed `dplyr` dependency in favor of `data.table`. `data.table` has significantly less dependencies compared to `dplyr`.

* added `insight`to Imports for checking if required packages are installed for each respective function. `insight` will later be used for cleaning up source code for extracting information from model objects such as variables, responses, etc...

* switched `gridExtra` and `unrepx` from Imports to Suggests

MINOR CHANGES

* updated pkgdown website with new theme and reference structure

---

# ggDoE 0.7.7

BREAKING

* renamed argument `ncols` to `n_columns` in all functions which previously used this argument. These functions are: `diagnostic_plots()`,`gg_rsm()`, `interaction_effects()`,`main_effects()`,`twoD_projections()`

NEW FEATURES

* `half_normal()` has new argument `point_color` to change the color of the points
* `diagnostic_plots()` has a new argument `cooksD_type` to change the threshold computed for Cook's distance plot (plot #6)

CHANGES

* `half_normal()` now uses `ggrepel::geom_text_repel` instead of hard-coded `geom_text` to try and avoid label overlap

---

# ggDoE 0.7.6

* minor fixes to documentation/vignettes
* moved `unrepex` from Suggests to Imports 
* added cran-comments.md

---

# ggDoE 0.7.5

NEW FEATURES

* changed appearance of `gg_boxplots()` from using fill to color. Added new argument 
`jitter_points` to `gg_boxplots()` to overlay points to boxplots.

* Added initial vignette for a quick overview of `ggDoE`. Still needs further details for certain plots

---

# ggDoE 0.7.4

BUX FIXES

* Fixed bug in `twoD_projections()` that would output the same plot for every plot in the grid

---

# ggDoE 0.7.3

NEW FUNCTIONS

* Added new function `twoD_projections()`. This function will output all two dimensional projections from a Latin hypercube design. Will add further details to documentation in future updates.

CHANGES

* Switched `gridExtra` to Imports instead of Suggests under Description file

---

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
