
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dirichletprocess

[![Travis-CI Build
Status](https://travis-ci.org/dm13450/dirichletprocess.svg?branch=master)](https://travis-ci.org/dm13450/dirichletprocess)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/dm13450/dirichletprocess?branch=master&svg=true)](https://ci.appveyor.com/project/dm13450/dirichletprocess)
[![Coverage
Status](https://codecov.io/gh/dm13450/dirichletprocess/branch/master/graph/badge.svg)](https://codecov.io/gh/dm13450/dirichletprocess)

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

![](vignettes/img/density-1.png)<!-- -->

``` r
data.frame(Weight=dp$weights, Mean=c(dp$clusterParameters[[1]]), SD=c(dp$clusterParameters[[1]]))
#>        Weight       Mean         SD
#> 1 0.371323529 -1.1756510 -1.1756510
#> 2 0.625000000  0.6597522  0.6597522
#> 3 0.003676471  0.1061095  0.1061095
```

### Clustering

Dirichlet processes can also be used to cluster data based on their
common distribution parameters.

``` r
faithfulTrans <- scale(faithful)
dpCluster <-  DirichletProcessMvnormal(faithfulTrans)
dpCluster <- Fit(dpCluster, 2000, progressBar = FALSE)
plot(dpCluster)
```

![](vignettes/img/clustering-1.png)<!-- -->

For more detailed explanations and examples see the vignette.

### Tutorials

I’ve written a number of tutorials:

  - [Non parametric
    priors](http://dm13450.github.io/2019/02/22/Nonparametric-Prior.html)
  - [Calculating cluster
    probabilities](http://dm13450.github.io/2018/11/21/Cluster-Probabilities.html)
  - [Clustering](http://dm13450.github.io/2018/05/30/Clustering.html)
  - [Point
    processes](http://dm13450.github.io/2018/03/08/dirichletprocess-pointprocess.html)
  - [Custom
    mixtures](http://dm13450.github.io/2018/02/21/Custom-Distributions-Conjugate.html)
  - [Density
    estimation](http://dm13450.github.io/2018/02/01/Dirichlet-Density.html)
