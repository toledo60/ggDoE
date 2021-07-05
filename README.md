# ggDoE

[![R-CMD-check](https://github.com/toledo60/ggDoE/workflows/R-CMD-check/badge.svg)](https://github.com/toledo60/ggDoE/actions)
[![Documentation](https://img.shields.io/badge/Documentation-ggDoE-blue)](https://ggdoe.netlify.app/)
[![Netlify Status](https://api.netlify.com/api/v1/badges/20d30180-f503-4b63-ba9c-c95bfca3826e/deploy-status)](https://app.netlify.com/sites/ggdoe/deploys)

## Overview

With ggDoE you'll be able to generate common plots used in Design of Experiments with ggplot2.

The following plots are currently available

- [alias_matrix()](https://toledo60.github.io/ggDoE/reference/alias_matrix.html): Correlation matrix plot to visualize the Alias matrix

- [boxcox_transform()](https://toledo60.github.io/ggDoE/reference/boxcox_transform.html): Boxcox transformation plot

- [diagnostic_plots()](https://toledo60.github.io/ggDoE/reference/diagnostic_plots.html): Regression diagnostics plots

- [halfnormal()](https://toledo60.github.io/ggDoE/reference/halfnormal.html): Half-Normal effects plot

- [interaction_effect()](https://toledo60.github.io/ggDoE/reference/interaction_effect.html): Interaction effects plot between two factors in a factorial design

- [Lenth_method()](https://toledo60.github.io/ggDoE/reference/Lenth_method.html): Visualize effect significance for experiments without variance
estimates using Lenth's method

- [main_effect()](https://toledo60.github.io/ggDoE/reference/main_effect.html): Main effect plot for one factor in a factorial design


## Installation

You can get the development version from GitHub:

```{r}
install.packages("remotes") # if not installed yet
remotes::install_github("toledo60/ggDoE")
```

### Contributing to the package
I welcome feedback, suggestions, issues, and contributions!
Check out the `CONTRIBUTING` file for more details.
