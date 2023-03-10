
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dirichletprocess

[![R build
status](https://github.com/dm13450/dirichletprocess/workflows/R-CMD-check/badge.svg)](https://github.com/dm13450/dirichletprocess/actions)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/dm13450/dirichletprocess?branch=master&svg=true)](https://ci.appveyor.com/project/dm13450/dirichletprocess)
[![Coverage
Status](https://codecov.io/gh/dm13450/dirichletprocess/branch/master/graph/badge.svg)](https://app.codecov.io/gh/dm13450/dirichletprocess)

The dirichletprocess package provides tools for you to build custom
Dirichlet process mixture models. You can use the pre-built
Normal/Weibull/Beta distributions or create your own following the
instructions in the vignette. In as little as four lines of code you can
be modelling your data nonparametrically.

## Installation

You can install the stable release of dirichletprocess from CRAN:

``` r
install.packages("dirichletprocess")
```

You can also install the development build of dirichletprocess from
github with:

``` r
# install.packages("devtools")
devtools::install_github("dm13450/dirichletprocess")
```

For a full guide to the package and its capabilities please consult the
vignette:

``` r
browseVignettes(package = "dirichletprocess")
```

## Examples

### Density Estimation

Dirichlet processes can be used for nonparametric density estimation.

``` r
faithfulTransformed <- faithful$waiting - mean(faithful$waiting)
faithfulTransformed <- faithfulTransformed/sd(faithful$waiting)
dp <- DirichletProcessGaussian(faithfulTransformed)
dp <- Fit(dp, 100, progressBar = FALSE)
plot(dp)
```

<img src=https://github.com/dm13450/dirichletprocess/raw/master/vignettes/img/density-1.png width=50% />

### Clustering

Dirichlet processes can also be used to cluster data based on their
common distribution parameters.

``` r
faithfulTrans <- scale(faithful)
dpCluster <-  DirichletProcessMvnormal(faithfulTrans)
dpCluster <- Fit(dpCluster, 2000, progressBar = FALSE)
plot(dpCluster)
```

<img src=https://github.com/dm13450/dirichletprocess/raw/master/vignettes/img/clustering-1.png width=50% />

For more detailed explanations and examples see the vignette.

### Tutorials

I’ve written a number of tutorials:

-   [Non parametric
    priors](https://dm13450.github.io/2019/02/22/Nonparametric-Prior.html)
-   [Calculating cluster
    probabilities](https://dm13450.github.io/2018/11/21/Cluster-Probabilities.html)
-   [Clustering](https://dm13450.github.io/2018/05/30/Clustering.html)
-   [Point
    processes](https://dm13450.github.io/2018/03/08/dirichletprocess-pointprocess.html)
-   [Custom
    mixtures](https://dm13450.github.io/2018/02/21/Custom-Distributions-Conjugate.html)
-   [Density
    estimation](https://dm13450.github.io/2018/02/01/Dirichlet-Density.html)
-   [Checking
    convergence](https://dm13450.github.io/2020/01/11/Dirichlet-Convergence.html)

and some case studies:

-   [State of the Market - Infinite State Hidden Markov
    Models](https://dm13450.github.io/2020/06/03/State-of-the-Market.html)
-   [Palmer Penguins and an Introduction to Dirichlet
    Processes](https://dm13450.github.io/2020/09/28/PriorToPosterior.html)
