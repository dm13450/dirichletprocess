
<!-- README.md is maintained directly in this repository. -->

# dirichletprocess

[![R build
status](https://github.com/dm13450/dirichletprocess/workflows/R-CMD-check/badge.svg)](https://github.com/dm13450/dirichletprocess/actions)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/dm13450/dirichletprocess?branch=master&svg=true)](https://ci.appveyor.com/project/dm13450/dirichletprocess)
[![Coverage
Status](https://codecov.io/gh/dm13450/dirichletprocess/branch/master/graph/badge.svg)](https://app.codecov.io/gh/dm13450/dirichletprocess)

The dirichletprocess package provides tools for building custom
Dirichlet process mixture models for nonparametric Bayesian analysis.
The current interface supports ordinary, hierarchical, and hidden
Markov model workflows. The main supported ordinary models use validated
compiled C++ core sampler paths when available, with R fallbacks kept for
unsupported paths.

**Key Features:**
- Pre-built distributions: Normal, Beta, Exponential, Weibull, Multivariate Normal
- Hierarchical Dirichlet Process models
- Compiled C++ core sampler paths for the main supported ordinary models
- Multivariate normal support with `covModel = "FULL"`
- Retained-sample posterior summaries through `PosteriorSummary()`
- Ordinary univariate `plot()` intervals based on retained MCMC samples

Model your data nonparametrically in as little as four lines of code.

## Installation

### System requirements

`dirichletprocess` ships compiled C++ samplers (via Rcpp + RcppArmadillo)
with optional OpenMP parallelism, so a working C++17 toolchain is
required to install from source:

- **R** ≥ 4.0
- **C++17 compiler** — `g++` on Linux, Apple `clang++` on macOS, or
  Rtools on Windows
- **OpenMP runtime** — built into `g++`/Rtools; on macOS install
  `libomp` (`brew install libomp`) and configure `~/.R/Makevars` per the
  CRAN macOS instructions at <https://mac.r-project.org/openmp/>. The
  package will still install without OpenMP, but compiled paths run
  single-threaded.
- **LAPACK/BLAS** — provided by your R install; no extra setup needed
- **(Optional) LaTeX** — only needed if you want to build the PDF
  vignette locally (`R CMD build` with vignettes, or
  `devtools::install_github(..., build_vignettes = TRUE)`). TinyTeX
  (`tinytex::install_tinytex()`) is the easiest option.

### R package dependencies

Imports (auto-installed): `Rcpp` (≥ 1.0.11), `RcppArmadillo`, `gtools`,
`ggplot2`, `mvtnorm`, `abind`. Suggested (only required for tests and
vignettes): `testthat`, `knitr`, `rmarkdown`, `tidyr`, `dplyr`, `coda`,
`pkgload`.

If installing fails on a fresh R session because a build dependency is
missing, you can pre-install everything in one shot:

``` r
install.packages(c(
  "Rcpp", "RcppArmadillo", "gtools", "ggplot2", "mvtnorm", "abind"
))
# Optional, only for tests/vignettes:
install.packages(c(
  "testthat", "knitr", "rmarkdown", "tidyr", "dplyr", "coda", "pkgload"
))
```

### Installing the package

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
library(dirichletprocess)

faithfulTransformed <- faithful$waiting - mean(faithful$waiting)
faithfulTransformed <- faithfulTransformed/sd(faithful$waiting)

# Ordinary fits retain samples by default for PosteriorSummary() and plot()
dp <- DirichletProcessGaussian(faithfulTransformed)
dp <- Fit(dp, 100, progressBar = FALSE)
plot(dp)
PosteriorSummary(dp, seq(-2, 2, length.out = 5))
```

<img src=https://github.com/dm13450/dirichletprocess/raw/master/vignettes/img/density-1.png width=50% />

### Clustering

Dirichlet processes can also be used to cluster data based on their
common distribution parameters.

``` r
faithfulTrans <- scale(faithful)

dpCluster <- DirichletProcessMvnormal(faithfulTrans)
dpCluster <- Fit(dpCluster, 500, progressBar = FALSE)
plot(dpCluster)
```

<img src=https://github.com/dm13450/dirichletprocess/raw/master/vignettes/img/clustering-1.png width=50% />

For more detailed explanations and examples see the vignette.

## Posterior Summaries and Plotting

For ordinary non-hierarchical Dirichlet process objects, retained MCMC
history is now the standard route for posterior summaries and interval
plots.

```r
y <- rt(200, 3) + 2
dp <- DirichletProcessGaussian(y)
dp <- Fit(dp, 200, progressBar = FALSE, thinning = 2)

summary_df <- PosteriorSummary(dp, seq(-4, 8, length.out = 25))
head(summary_df)
```

Repeated `Fit()` calls append newly retained samples. Setting
`storeSamples = FALSE` still updates the fitted state but appends no new
retained sample history.

Ordinary univariate `plot()` uses retained-sample `PosteriorSummary()`
intervals when usable stored samples exist. If not, it plots only the
current fitted curve and explains why intervals are unavailable.

`PosteriorFrame()`, `PosteriorFunction()`, and `PosteriorClusters()`
remain available as lower-level conditional-on-current-state helpers.

## Implementation Control

Ordinary supported `Fit()` paths select validated compiled C++ samplers
automatically when available. You can override that routing globally:

```r
set_use_cpp(TRUE)    # Force supported ordinary Fit() paths to use C++
set_use_cpp(FALSE)   # Force ordinary Fit() paths to stay in R
set_use_cpp(NULL)    # Restore automatic ordinary routing
```

Constructor `cpp` arguments are retained for backward compatibility, but
they are not the primary live routing interface. In particular,
hierarchical and HMM constructor `cpp` arguments should not be read as
enabling compiled fitting for those paths.

## Model Notes

- `DirichletProcessMvnormal()` currently supports only `covModel = "FULL"`.
- Beta mixtures use a bounded mean/precision parameterisation, not the
  ordinary Beta shape-parameter parameterisation.
- Top-level hierarchical `PosteriorSummary()`, `PosteriorFrame()`,
  `PosteriorFunction()`, `PosteriorClusters()`, and `plot()` calls are
  intentionally unsupported. For a local restaurant object, use
  `dplist$indDP[[j]]`.

## Supported Distributions

- Normal (Gaussian)
- Normal with fixed variance
- Exponential
- Beta
- Beta with boundary-avoiding precision prior
- Weibull
- Multivariate Normal with Normal-Wishart prior (`covModel = "FULL"` only)
- Multivariate Normal with semi-conjugate priors
- Hierarchical Beta
- Hierarchical Multivariate Normal variants
- Hidden Markov model constructions

### Tutorials

I've written a number of tutorials:

-   [Non parametric
    priors](https://dm13450.github.io/2019/02/22/Nonparametric-Prior.html)
-   [Calculating cluster
    probabilities](https://dm13450.github.io/2018/11/21/Cluster-Probabilities.html)
-   [Clustering](https://dm13450.github.io/2018/05/30/Clustering.html)
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
