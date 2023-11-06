
# ggDoE <img src="man/figures/logo.png" align="right" height="139" alt="" />

[![CRAN
status](https://www.r-pkg.org/badges/version/ggDoE)](https://cran.r-project.org/package=ggDoE)
[![R-CMD-check](https://github.com/toledo60/ggDoE/workflows/R-CMD-check/badge.svg)](https://github.com/toledo60/ggDoE/actions)
[![Documentation](https://img.shields.io/badge/Documentation-ggDoE-blue)](https://ggdoe.netlify.app/)

## Installation

You can install:

- the latest release from CRAN with

``` r
install.packages('ggDoE')
```

- the development version from GitHub with

``` r
if (!require("remotes")) install.packages("remotes")
remotes::install_github("toledo60/ggDoE")
```

## Overview

With ggDoE you’ll be able to generate common plots used in Design of
Experiments with ggplot2.

``` r
library(ggDoE)
```

The following plots are currently available:

1.  [Alias Matrix](#alias-matrix)
2.  [Box-Cox Transformation](#box-cox-transformation)
3.  [Lambda Plot](#lambda-plot)
4.  [Boxplots](#boxplots)
5.  [Regression Diagnostic Plots](#regression-diagnostic-plots)
6.  [Half-Normal Plot](#half-normal-plot)
7.  [Interaction Effects Plot for a Factorial
    Design](#interaction-effects-plot)
8.  [Main Effects Plot for a Factorial Design](#main-effects-plot)
9.  [Contour Plots for Response Surface Methodology](#contour-plots)
10. [Pareto Plot](#pareto-plot)
11. [Two Dimensional Projections of a Latin Hypercube
    Design](#two-dimensional-projections)

The following datasets/designs are included in ggDoE as tibbles:

- **adapted_epitaxial**: Adapted epitaxial layer experiment obtain from
  the book <br> *“Experiments: Planning, Analysis, and Optimization, 2nd
  Edition”*

- **original_epitaxial**: Original epitaxial layer experiment obtain
  from the book <br> *“Experiments: Planning, Analysis, and
  Optimization, 2nd Edition”*

- **pulp_experiment**: Reflectance Data, Pulp Experiment obtain from the
  book <br> *“Experiments: Planning, Analysis, and Optimization, 2nd
  Edition”*

- **girder_experiment**: Girder experiment obtain from the book <br>
  *“Experiments: Planning, Analysis, and Optimization, 2nd Edition”*

- **aliased_design**: D-efficient minimal aliasing design obtained from
  the article <br> *“Efficient Designs With Minimal Aliasing by Bradley
  Jones and Christopher J. Nachtsheim”*

## Citation

If you want to cite this package in a scientific journal or in any other
context, run the following code in your `R` console

``` r
citation('ggDoE')
```

    To cite package 'ggDoE' in publications use:

      Toledo Luna J (2023). _ggDoE: Modern Graphs for Design of Experiments
      with 'ggplot2'_. R package version 0.7.9,
      <https://CRAN.R-project.org/package=ggDoE>.

    A BibTeX entry for LaTeX users is

      @Manual{,
        title = {ggDoE: Modern Graphs for Design of Experiments with 'ggplot2'},
        author = {Jose {Toledo Luna}},
        year = {2023},
        note = {R package version 0.7.9},
        url = {https://CRAN.R-project.org/package=ggDoE},
      }

## Contributing to the package

I welcome feedback, suggestions, issues, and contributions! Check out
the
[CONTRIBUTING](https://github.com/toledo60/ggDoE/blob/main/.github/CONTRIBUTING.md)
file for more details.

## Examples of Plots

### Alias Matrix

Correlation matrix plot to visualize the Alias matrix

``` r
alias_matrix(design=aliased_design)
```

![](man/figures/alias_matrix.png)

### Box-Cox Transformation

``` r
model <- lm(s2 ~ (A+B+C+D),data = adapted_epitaxial)
boxcox_transform(model,lambda = seq(-5,5,0.2))
```

![](man/figures/boxcox_transformation.png)

### Lambda Plot

Obtain the trace plot of the *t*-statistics after applying Boxcox
transformation across a specified sequence of lambda values

``` r
model <-  lm(s2 ~ (A+B+C)^2,data=original_epitaxial)
lambda_plot(model)
```

![](man/figures/lambda_plot1.png)

``` r
lambda_plot(model, lambda = seq(0,2,by=0.1))
```

![](man/figures/lambda_plot2.png)

### Boxplots

``` r
data <- ToothGrowth
data$dose <- factor(data$dose,levels = c(0.5, 1, 2),
                    labels = c("D0.5", "D1", "D2"))

gg_boxplots(data,y = 'len',x = 'dose')
```

![](man/figures/boxplot1.png)

``` r
gg_boxplots(data,y = 'len',x = 'dose',
            group_var = 'supp',
            color_palette = 'viridis',
            jitter_points = TRUE)
```

![](man/figures/boxplot2.png)

### Regression Diagnostic Plots

1.  Residual vs. Fitted Values
2.  Normal-QQ plot
3.  Scale-Location plot
4.  Residual vs. Leverage
5.  Cook’s Distance
6.  Collinearity

The default plots are 1-4

``` r
model <- lm(mpg ~ wt + am + gear + vs * cyl, data = mtcars)
gg_lm(model,which_plots=1:6)
```

![](man/figures/diagnostic_plots.png)

### Half-Normal Plot

``` r
model <- lm(ybar ~ (A+B+C+D)^4,data=adapted_epitaxial)
half_normal(model)
```

![](man/figures/half_normal1.png)

``` r
half_normal(model,method='Zahn',alpha=0.1,
            ref_line=TRUE,label_active=TRUE,
            margin_errors=TRUE)
```

![](man/figures/half_normal2.png)

### Interaction Effects Plot

Interaction effects plot between two factors in a *factorial design*

``` r
interaction_effects(adapted_epitaxial,response = 'ybar',
                    exclude_vars = c('s2','lns2'))
```

![](man/figures/interactions1.png)

``` r
interaction_effects(adapted_epitaxial,response = 'ybar',
                    exclude_vars = c('A','s2','lns2'),
                    n_columns=3)
```

![](man/figures/interactions2.png)

### Main Effects Plots

Main effect plots for each factor in a *factorial design*

``` r
main_effects(original_epitaxial,
             response='s2',
             exclude_vars = c('ybar','lns2'))
```

![](man/figures/main_effects1.png)

``` r
main_effects(original_epitaxial,
             response='s2',
             exclude_vars = c('A','ybar','lns2'),
             color_palette = 'viridis',
             n_columns=3)
```

![](man/figures/main_effects2.png)

### Contour Plots

contour plot(s) that display the fitted surface for an *rsm* object
involving two or more numerical predictors

``` r
heli.rsm <- rsm::rsm(ave ~ SO(x1, x2, x3, x4), 
                     data = rsm::heli)
```

``` r
gg_rsm(heli.rsm,formula = ~x1+x2+x3+x4,
       at = rsm::xs(heli.rsm))
```

![](man/figures/contour1.png)

### Pareto Plot

Pareto plot of effects with cutoff values for the margin of error (ME)
and simultaneous margin of error (SME)

``` r
model <- lm(lns2 ~ (A+B+C+D)^4,data=original_epitaxial)
pareto_plot(model)
```

![](man/figures/pareto_plot1.png)

``` r
pareto_plot(model,method='Zahn',alpha=0.1)
```

![](man/figures/pareto_plot2.png)

### Two Dimensional Projections

This function will output all two dimensional projections from a Latin
hypercube design

``` r
set.seed(10)
X <- lhs::randomLHS(n=10, k=4)
pair_plots(X,n_columns=3,grid = TRUE)
```

![](man/figures/pair_plots.png)
