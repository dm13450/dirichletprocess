
<!-- README.md is generated from README.Rmd. Please edit that file -->
dirichletprocess
================

The dirichletprocess package provides tools for you to build custom Dirichlet Process mixture models. You can use the prebuilt Normal/Weibull/Beta distribtuions or create your own following the instructions in the Custom Distribtuions vignettes.

Installation
------------

You can install dirichletprocess from github with:

``` r
# install.packages("devtools")
devtools::install_github("dm13450/dirichletprocess")
```

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
library(dirichletprocess)
y <- rnorm(100)
dp <- DirichletProcessGaussian(y)
dp <- Fit(dp, 100, progressBar = FALSE)
plot(dp)
```
