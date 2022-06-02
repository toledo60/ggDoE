
# ggDoE

[![R-CMD-check](https://github.com/toledo60/ggDoE/workflows/R-CMD-check/badge.svg)](https://github.com/toledo60/ggDoE/actions)
[![Documentation](https://img.shields.io/badge/Documentation-ggDoE-blue)](https://ggdoe.netlify.app/)
[![Netlify
Status](https://api.netlify.com/api/v1/badges/20d30180-f503-4b63-ba9c-c95bfca3826e/deploy-status)](https://app.netlify.com/sites/ggdoe/deploys)

## Installation

You can get the development version from GitHub:

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

**Alias Matrix**

Correlation matrix plot to visualize the Alias matrix

``` r
alias_matrix(design=aliased_design)
```

![](man/figures/alias_matrix.png)

**Box-Cox Transformation**

``` r
model <- lm(s2 ~ (A+B+C+D),data = adapted_epitaxial)
boxcox_transform(model,lambda = seq(-5,5,0.2))
```

![](man/figures/boxcox_transformation.png)

**Lambda Plot**

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

**Boxplots**

``` r
data <- ToothGrowth
data$dose <- factor(data$dose,levels = c(0.5, 1, 2),
                    labels = c("D0.5", "D1", "D2"))

gg_boxplots(data,response = len,
            factor = dose)
```

![](man/figures/boxplot1.png)

``` r
gg_boxplots(data,response = len,
            factor = dose,
            group_var = supp,
            color_palette = 'viridis')
```

![](man/figures/boxplot2.png)

**Regression Diagnostic Plots**

1.  Residual vs. Fitted Values
2.  Normal-QQ plot
3.  Scale-Location plot
4.  Residual vs. Leverage
5.  Cook’s Distance

``` r
data(mtcars)
mtcars_lm <- lm(mpg ~.,data=mtcars)
diagnostic_plots(mtcars_lm)
```

![](man/figures/diagnostic1.png)

``` r
diagnostic_plots(mtcars_lm,
                 which_plots=c(1,3,4,5))
```

![](man/figures/diagnostic2.png)

**Half-Normal Plot**

``` r
m1 <- lm(lns2 ~ (A+B+C+D)^4,data=original_epitaxial)
half_normal(m1)
```

![](man/figures/half_normal1.png)

``` r
half_normal(m1,method='Zahn',alpha=0.1,
            ref_line=TRUE,label_active=TRUE,
            margin_errors=TRUE)
```

![](man/figures/half_normal2.png)

**Interaction Effects Plot (Factorial Design)**

Interaction effects plot between two factors in a factorial design

``` r
interaction_effects(adapted_epitaxial,response = 'ybar',
                    exclude_vars = c('s2','lns2'))
```

![](man/figures/interactions1.png)

``` r
interaction_effects(adapted_epitaxial,response = 'ybar',
                    exclude_vars = c('A','s2','lns2'),
                    ncols=3)
```

![](man/figures/interactions2.png)

**Main Effects Plots (Factorial Design)**

Main effect plots for each factor in a factorial design

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
             ncols=3)
```

![](man/figures/main_effects2.png)

**Pareto Plot**

Pareto plot of effects with cutoff values for the margin of error (ME)
and simultaneous margin of error (SME)

``` r
m1 <- lm(lns2 ~ (A+B+C+D)^4,data=original_epitaxial)
pareto_plot(m1)
```

![](man/figures/pareto_plot1.png)

``` r
pareto_plot(m1,method='Zahn',alpha=0.1)
```

![](man/figures/pareto_plot2.png)

Lastly, the following datasets/designs are included in ggDoE as tibbles:

-   **adapted_epitaxial**: Adapted epitaxial layer experiment obtain
    from the book <br> *“Experiments: Planning, Analysis, and
    Optimization, 2nd Edition”*
    [source](https://www2.isye.gatech.edu/~jeffwu/wuhamadabook/data/originallayer.dat)
-   **original_epitaxial**: Original epitaxial layer experiment obtain
    from the book <br> *“Experiments: Planning, Analysis, and
    Optimization, 2nd Edition”*
    [source](https://www2.isye.gatech.edu/~jeffwu/wuhamadabook/data/originallayer.dat)
-   **aliased_design**: D-efficient minimal aliasing design obtained
    from the article <br> *“Efficient Designs With Minimal Aliasing”*
    [source](https://www.tandfonline.com/doi/abs/10.1198/TECH.2010.09113)

### Contributing to the package

I welcome feedback, suggestions, issues, and contributions! Check out
the
[CONTRIBUTING](https://github.com/toledo60/ggDoE/blob/main/.github/CONTRIBUTING.md)
file for more details.
