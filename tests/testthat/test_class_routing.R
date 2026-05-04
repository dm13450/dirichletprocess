context("Class Propagation and C++ Routing")

make_weibullcens_md <- function(list_first = FALSE) {
  md <- MixingDistribution("weibullcens",
                           c(1, 2, 0.5),
                           "nonconjugate",
                           mhStepSize = c(0.11, 0.11),
                           hyperPriorParameters = c(2.222, 2, 1, 0.05))

  class(md) <- if (list_first) {
    c("list", "weibullcens", "weibull", "nonconjugate")
  } else {
    c("weibullcens", "weibull", "nonconjugate")
  }

  md
}

make_expcustom_md <- function(list_first = FALSE) {
  md <- ExponentialMixtureCreate(c(0.5, 0.5))

  class(md) <- if (list_first) {
    c("list", "expcustom", "exponential", "conjugate")
  } else {
    c("expcustom", "exponential", "conjugate")
  }

  md
}

Likelihood.weibullcens <- function(mdobj, x, theta) {
  a <- theta[[1]][,,, drop = TRUE]
  b <- theta[[2]][,,, drop = TRUE]

  uncens <- as.numeric(b^(-1) * a * x[, 1]^(a - 1) * exp(-b^(-1) * x[, 1]^a))
  cens <- as.numeric(1 - exp(-x[, 1]^a / b))

  if (nrow(x) == 1) {
    if (x[, 2] == 1) {
      return(cens)
    }
    return(uncens)
  }

  out <- uncens
  out[x[, 2] == 1] <- cens[x[, 2] == 1]
  out
}

test_that("DirichletProcessCreate preserves custom subclasses instead of stripping by position", {
  dp_list_first <- DirichletProcessCreate(cbind(c(1.1, 1.2, 1.3), c(0, 1, 0)),
                                          make_weibullcens_md(list_first = TRUE),
                                          c(2, 0.9))
  dp_no_list <- DirichletProcessCreate(cbind(c(1.1, 1.2, 1.3), c(0, 1, 0)),
                                       make_weibullcens_md(list_first = FALSE),
                                       c(2, 0.9))
  dp_exp <- DirichletProcessCreate(rexp(4),
                                   make_expcustom_md(list_first = FALSE),
                                   c(2, 4))

  expect_identical(class(dp_list_first),
                   c("list", "dirichletprocess", "weibullcens", "weibull", "nonconjugate"))
  expect_identical(class(dp_no_list),
                   c("list", "dirichletprocess", "weibullcens", "weibull", "nonconjugate"))
  expect_identical(class(dp_exp),
                   c("list", "dirichletprocess", "expcustom", "exponential", "conjugate"))
})

test_that("built-in supported classes still route to the validated C++ paths", {
  should_use_cpp_fit_fn <- getFromNamespace("should_use_cpp_fit", "dirichletprocess")
  old_cpp_setting <- using_cpp()
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)

  set_use_cpp(NULL)

  gaussian_dp <- DirichletProcessGaussian(rnorm(6), cpp = TRUE)
  beta_dp <- DirichletProcessBeta(runif(6, 0.1, 0.9),
                                  maxY = 1,
                                  verbose = FALSE,
                                  cpp = TRUE)
  weibull_dp <- DirichletProcessWeibull(rweibull(6, 1.2, 1),
                                        matrix(c(1, 1, 1), ncol = 3),
                                        verbose = FALSE,
                                        cpp = TRUE)
  mvnormal_dp <- DirichletProcessMvnormal(matrix(rnorm(12), ncol = 2),
                                          g0Priors = list(mu0 = c(0, 0),
                                                          Lambda = diag(2),
                                                          kappa0 = 2,
                                                          nu = 2,
                                                          covModel = "FULL"),
                                          cpp = TRUE)

  for (dp in list(gaussian_dp, beta_dp, weibull_dp, mvnormal_dp)) {
    skip_if_not(can_use_cpp(dp), "C++ sampler not available")
    expect_true(should_use_cpp_fit_fn(dp))

    fit <- Fit(dp, 1, FALSE, FALSE)
    expect_length(fit$alphaChain, 1)
    expect_length(fit$labelsChain, 1)
  }
})

test_that("custom subclasses default to the safe R path even when they inherit from supported built-ins", {
  should_use_cpp_fit_fn <- getFromNamespace("should_use_cpp_fit", "dirichletprocess")
  old_cpp_setting <- using_cpp()
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)

  custom_exp_dp <- DirichletProcessCreate(rexp(6),
                                          make_expcustom_md(list_first = FALSE),
                                          c(2, 4))
  custom_exp_dp <- Initialise(custom_exp_dp)

  custom_weibull_dp <- DirichletProcessCreate(cbind(c(1.1, 1.2, 1.3, 1.4),
                                                    c(0, 1, 0, 1)),
                                              make_weibullcens_md(list_first = FALSE),
                                              c(2, 0.9),
                                              mhDraws = 5)
  custom_weibull_dp <- Initialise(custom_weibull_dp,
                                  posterior = FALSE,
                                  verbose = FALSE)

  set_use_cpp(NULL)
  expect_false(can_use_cpp(custom_exp_dp))
  expect_false(can_use_cpp(custom_weibull_dp))
  expect_false(should_use_cpp_fit_fn(custom_exp_dp))
  expect_false(should_use_cpp_fit_fn(custom_weibull_dp))

  set_use_cpp(TRUE)
  expect_false(should_use_cpp_fit_fn(custom_exp_dp))
  expect_false(should_use_cpp_fit_fn(custom_weibull_dp))

  fit_exp <- Fit(custom_exp_dp, 2, FALSE, FALSE)
  expect_length(fit_exp$alphaChain, 2)
  expect_length(fit_exp$labelsChain, 2)
})

test_that("MetropolisHastings.list uses class-aware dispatch for custom subclass hierarchies", {
  test_data <- rweibull(20, 1.1, 0.9)
  md <- make_weibullcens_md(list_first = TRUE)

  mh <- MetropolisHastings(md, test_data, PriorDraw(md), 6)

  expect_equal(length(mh$parameter_samples), 2)
  expect_equal(dim(mh$parameter_samples[[1]])[3], 6)
  expect_equal(dim(mh$parameter_samples[[2]])[3], 6)
})
