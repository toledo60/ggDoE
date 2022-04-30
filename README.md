
# ggDoE

[![R-CMD-check](https://github.com/toledo60/ggDoE/workflows/R-CMD-check/badge.svg)](https://github.com/toledo60/ggDoE/actions)
[![Documentation](https://img.shields.io/badge/Documentation-ggDoE-blue)](https://ggdoe.netlify.app/)
[![Netlify
Status](https://api.netlify.com/api/v1/badges/20d30180-f503-4b63-ba9c-c95bfca3826e/deploy-status)](https://app.netlify.com/sites/ggdoe/deploys)

## Installation

You can get the development version from GitHub:

``` r
#install.packages("remotes") 
remotes::install_github("toledo60/ggDoE")
```

## Overview

With ggDoE you’ll be able to generate common plots used in Design of
Experiments with ggplot2.

The following plots are currently available:

-   [alias_matrix()](https://ggdoe.netlify.app/reference/alias_matrix.html):
    Correlation matrix plot to visualize the Alias matrix

-   [boxcox_transform()](https://ggdoe.netlify.app/reference/boxcox_transform.html):
    Boxcox transformation plot

-   [diagnostic_plots()](https://ggdoe.netlify.app/reference/diagnostic_plots.html):
    Regression diagnostics plots

-   [half_normal()](https://ggdoe.netlify.app/reference/half_normal.html):
    Half-Normal effects plot

-   [interaction_effect()](https://ggdoe.netlify.app/reference/interaction_effect.html):
    Interaction effects plot between two factors in a factorial design

-   [main_effect()](https://ggdoe.netlify.app/reference/main_effect.html):
    Main effect plot for one factor in a factorial design

-   [pareto_plot()](https://ggdoe.netlify.app/reference/pareto_plot.html):
    Pareto plot of effects with cutoff values for the margin of error
    (ME) and simultaneous margin of error (SME)

``` r
library(ggDoE)
```

**Alias Matrix**

``` r
alias_matrix(design=aliased_design)
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
alias_matrix(design=aliased_design, symmetric=TRUE,
             intercept=FALSE)
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

**Box-Cox Transformation**

``` r
model <- lm(s2 ~ (A+B+C+D),data = adapted_epitaxial)
boxcox_transform(model,lambda = seq(-2,3,0.2))
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

**Regression Diagnostic Plots**

``` r
data(mtcars)
mtcars_lm <- lm(mpg ~.,data=mtcars)
diagnostic_plots(mtcars_lm)
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

**Half-Normal Plot**

``` r
m1 <- lm(lns2 ~ (A+B+C+D)^4,data=original_epitaxial)
half_normal(m1)
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
half_normal(m1,method='Zahn',alpha=0.1,
            ref_line=TRUE,label_active=TRUE,
            showME=TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

**Interaction Effects Plot (Factorial Design)**

``` r
interaction_effect(data = adapted_epitaxial, factor_1 = A,
                   factor_2 = B, response_var = ybar)
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

**Main Effects Plots (Factorial Design)**

``` r
main_effect(data = adapted_epitaxial, factor_1 = B, 
            response_var = ybar)
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

**Pareto Plot**

``` r
m1 <- lm(lns2 ~ (A+B+C+D)^4,data=original_epitaxial)
pareto_plot(m1)
```

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
pareto_plot(m1,method='Zahn',alpha=0.1)
```

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

### Contributing to the package

I welcome feedback, suggestions, issues, and contributions! Check out
the
[CONTRIBUTING](https://github.com/toledo60/ggDoE/blob/main/.github/CONTRIBUTING.md)
file for more details.
