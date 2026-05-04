// inst/include/dirichletprocess.h
// Main header file for dirichletprocess C++ functionality
#ifndef DIRICHLETPROCESS_H
#define DIRICHLETPROCESS_H

// Core Rcpp headers
#include <RcppArmadillo.h>

// Base classes and utilities
#include "DirichletProcessBase.h"
#include "RcppConversions.h"
#include "utilities.h"
#include "mixing_distribution_base.h"

// Core Dirichlet Process implementation (defined in DirichletProcessBase.h)

// MCMC runners
#include "mcmc_runner.h"
#include "mcmc_runner_manual.h"
#include "hierarchical_mcmc_runner.h"
#include "markov_mcmc_runner.h"

// Distribution implementations
#include "NormalDistribution.h"
#include "ExponentialDistribution.h"
#include "BetaDistribution.h"
#include "WeibullDistribution.h"
#include "MVNormalDistribution.h"
#include "MVNormal2Distribution.h"

// Mixing distributions
#include "gaussian_mixing.h"
#include "exponential_mixing.h"
#include "beta_mixing.h"
#include "beta2_mixing.h"
#include "weibull_mixing.h"
#include "mvnormal_mixing.h"
#include "mvnormal2_mixing.h"
#include "normal_fixed_variance_mixing.h"

// Hierarchical implementations
#include "HierarchicalDP.h"
#include "hierarchical_beta_mixing.h"
#include "hierarchical_mvnormal_mixing.h"

// Specialized implementations
#include "MarkovDP.h"
#include "ConjugateDP.h"
#include "NonConjugateDP.h"

// Likelihood functions
#include "likelihood_functions.h"

// Benchmarking and profiling
#include "Benchmarking.h"
#include "MemoryProfiling.h"

#endif // DIRICHLETPROCESS_H
