context("C++ Fit Phase 1 Structure")

test_that("constructors do not mutate the C++ override and Fit auto-selects supported paths", {
  should_use_cpp_fit_fn <- getFromNamespace("should_use_cpp_fit", "dirichletprocess")
  old_cpp_setting <- using_cpp()
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)

  set_use_cpp(NULL)
  expect_null(getOption("dirichletprocesscpp.use_cpp", NULL))

  dp_auto <- DirichletProcessGaussian(rnorm(6), cpp = TRUE)
  expect_null(getOption("dirichletprocesscpp.use_cpp", NULL))

  skip_if_not(can_use_cpp(dp_auto), "C++ sampler not available")
  expect_true(should_use_cpp_fit_fn(dp_auto))

  set_use_cpp(FALSE)
  expect_false(should_use_cpp_fit_fn(dp_auto))
  expect_identical(getOption("dirichletprocesscpp.use_cpp", NULL), FALSE)

  dp_forced_r <- DirichletProcessExponential(rexp(6), cpp = FALSE)
  expect_identical(getOption("dirichletprocesscpp.use_cpp", NULL), FALSE)
  expect_false(should_use_cpp_fit_fn(dp_forced_r))

  set_use_cpp(TRUE)
  expect_true(should_use_cpp_fit_fn(dp_auto))
})

test_that("prepare_mcmc_params preserves current state and m", {
  prepare_mcmc_params_fn <- getFromNamespace("prepare_mcmc_params", "dirichletprocess")
  dp <- DirichletProcessGaussian(rnorm(6))
  dp$m <- 7

  params <- prepare_mcmc_params_fn(dp, its = 1, updatePrior = FALSE)

  expect_identical(params$update_concentration, TRUE)
  expect_identical(params$m_auxiliary, 7L)
  expect_equal(params$initial_cluster_labels, dp$clusterLabels - 1L)
  expect_length(params$initial_cluster_params, dp$numberClusters)
})

test_that("run_mcmc_cpp supports no-history mode for the live one-step path", {
  prepare_mixing_dist_params_fn <- getFromNamespace("prepare_mixing_dist_params",
                                                    "dirichletprocess")
  prepare_mcmc_params_fn <- getFromNamespace("prepare_mcmc_params",
                                             "dirichletprocess")
  run_mcmc_cpp_fn <- getFromNamespace("run_mcmc_cpp", "dirichletprocess")

  dp <- DirichletProcessGaussian(rnorm(8))
  skip_if_not(can_use_cpp(dp), "C++ sampler not available")

  mixing_params <- prepare_mixing_dist_params_fn(dp)
  mcmc_params <- prepare_mcmc_params_fn(dp, its = 1, updatePrior = FALSE,
                                        store_history = FALSE)
  expected_pre_update_loglik <- sum(log(LikelihoodDP(dp)))
  result <- run_mcmc_cpp_fn(as.matrix(dp$data), mixing_params, mcmc_params)

  expect_false("labelsChain" %in% names(result))
  expect_true("cluster_labels" %in% names(result))
  expect_true("theta" %in% names(result))
  expect_true("likelihoodChain" %in% names(result))
  expect_identical(length(result$cluster_labels), 1L)
  expect_identical(length(result$theta), 1L)
  expect_equal(length(result$alphaChain), 1L)
  expect_equal(result$likelihoodChain[1], expected_pre_update_loglik)
})

test_that("live C++ Fit stores the repaired-style pre-update likelihood chain", {
  old_cpp_setting <- using_cpp()
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)
  set_use_cpp(TRUE)

  dp <- DirichletProcessGaussianFixedVariance(c(-4.2, -4.0, -3.8, 0.0, 0.1, 0.3),
                                              sigma = 1.5,
                                              cpp = TRUE)
  skip_if_not(can_use_cpp(dp), "C++ sampler not available")

  expected_pre_update_loglik <- sum(log(LikelihoodDP(dp)))
  fit <- Fit(dp, 1, FALSE, FALSE)

  expect_length(fit$likelihoodChain, 1)
  expect_equal(fit$likelihoodChain[1], expected_pre_update_loglik)
})

test_that("prior update support matches the repaired R semantics", {
  supports_prior_update <- getFromNamespace("SupportsPriorUpdate", "dirichletprocess")
  assert_prior_update_supported <- getFromNamespace("AssertPriorUpdateSupported", "dirichletprocess")

  expect_false(supports_prior_update(GaussianMixtureCreate()))
  expect_false(supports_prior_update(ExponentialMixtureCreate()))
  expect_true(supports_prior_update(BetaMixtureCreate()))
  expect_true(supports_prior_update(WeibullMixtureCreate(matrix(c(1, 1, 1), ncol = 3), 1)))

  expect_error(
    assert_prior_update_supported(GaussianMixtureCreate()),
    "not supported for model family 'normal'"
  )
})

test_that("C++ Fit preserves incoming state and stores repaired-style chains", {
  old_cpp_setting <- using_cpp()
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)

  dp <- DirichletProcessGaussian(rnorm(8))
  set_use_cpp(TRUE)
  skip_if_not(can_use_cpp(dp), "C++ sampler not available")

  dp <- Fit(dp, 2, FALSE, FALSE)

  expect_length(dp$alphaChain, 2)
  expect_length(dp$weightsChain, 2)
  expect_length(dp$clusterParametersChain, 2)
  expect_length(dp$priorParametersChain, 2)
  expect_length(dp$labelsChain, 2)
  expect_equal(length(dp$clusterLabels), dp$n)
  expect_true(all(unlist(dp$labelsChain) >= 1L))

  previous_labels <- dp$clusterLabels
  previous_alpha <- dp$alpha
  previous_params <- dp$clusterParameters

  dp_next <- Fit(dp, 1, FALSE, FALSE)

  expect_length(dp_next$alphaChain, 3)
  expect_equal(dp_next$storedIterations, 1:3)
  expect_equal(dp_next$labelsChain[[3]], previous_labels)
  expect_equal(dp_next$alphaChain[3], previous_alpha)
  expect_equal(dp_next$clusterParametersChain[[3]], previous_params)
})

test_that("Gaussian batch fast path preserves one-step semantics and repaired-style storage", {
  prepare_mixing_dist_params_fn <- getFromNamespace("prepare_mixing_dist_params",
                                                    "dirichletprocess")
  prepare_mcmc_params_fn <- getFromNamespace("prepare_mcmc_params",
                                             "dirichletprocess")
  run_mcmc_cpp_fn <- getFromNamespace("run_mcmc_cpp", "dirichletprocess")
  run_gaussian_fit_cpp_batch_fn <- getFromNamespace("run_gaussian_fit_cpp_batch",
                                                    "dirichletprocess")
  update_dpobj_from_cpp_result_fn <- getFromNamespace("update_dpobj_from_cpp_result",
                                                      "dirichletprocess")
  update_gaussian_dpobj_from_cpp_batch_result_fn <- getFromNamespace(
    "update_gaussian_dpobj_from_cpp_batch_result",
    "dirichletprocess"
  )

  dp <- DirichletProcessGaussian(c(-4.3, -4.1, -3.9, -0.2, 0.0, 0.2, 4.1, 4.3, 4.5, 8.8),
                                 g0Priors = c(0, 1, 1, 1),
                                 alphaPriors = c(2, 4),
                                 cpp = TRUE)
  skip_if_not(can_use_cpp(dp), "C++ sampler not available")

  dp$clusterLabels <- c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 3L)
  dp$numberClusters <- 3L
  dp$pointsPerCluster <- c(3, 3, 4)
  dp$weights <- dp$pointsPerCluster / dp$n
  dp$alpha <- 1.75
  dp$clusterParameters <- list(
    array(c(-4.1, 0.0, 5.0), dim = c(1, 1, 3)),
    array(c(0.45, 0.35, 1.10), dim = c(1, 1, 3))
  )

  mixing_params <- prepare_mixing_dist_params_fn(dp)

  set.seed(123)
  batch_result <- run_gaussian_fit_cpp_batch_fn(
    as.matrix(dp$data),
    mixing_params,
    prepare_mcmc_params_fn(dp, its = 1, updatePrior = FALSE,
                           n_burn = 0, thin = 1, store_history = FALSE)
  )

  dp_batch <- update_gaussian_dpobj_from_cpp_batch_result_fn(
    unserialize(serialize(dp, NULL)),
    batch_result
  )

  dp_step <- unserialize(serialize(dp, NULL))
  expected_alpha <- dp_step$alpha
  expected_likelihood <- sum(log(LikelihoodDP(dp_step)))
  expected_labels <- dp_step$clusterLabels
  expected_params <- dp_step$clusterParameters

  set.seed(123)
  step_result <- run_mcmc_cpp_fn(
    as.matrix(dp_step$data),
    mixing_params,
    prepare_mcmc_params_fn(dp_step, its = 1, updatePrior = FALSE,
                           n_burn = 0, thin = 1, store_history = FALSE)
  )
  dp_step <- update_dpobj_from_cpp_result_fn(dp_step, step_result)

  expect_equal(dp_batch$alphaChain, expected_alpha)
  expect_equal(dp_batch$likelihoodChain, expected_likelihood)
  expect_equal(dp_batch$labelsChain[[1]], expected_labels)
  expect_equal(unname(dp_batch$clusterParametersChain[[1]]),
               unname(expected_params))
  expect_equal(dp_batch$clusterLabels, dp_step$clusterLabels)
  expect_equal(dp_batch$alpha, dp_step$alpha)
  expect_equal(dp_batch$clusterParameters, dp_step$clusterParameters)
})

test_that("normalFixedVariance batch fast path preserves one-step semantics and repaired-style storage", {
  prepare_mixing_dist_params_fn <- getFromNamespace("prepare_mixing_dist_params",
                                                    "dirichletprocess")
  prepare_mcmc_params_fn <- getFromNamespace("prepare_mcmc_params",
                                             "dirichletprocess")
  run_mcmc_cpp_fn <- getFromNamespace("run_mcmc_cpp", "dirichletprocess")
  run_nfv_fit_cpp_batch_fn <- getFromNamespace("run_normal_fixed_variance_fit_cpp_batch",
                                               "dirichletprocess")
  update_dpobj_from_cpp_result_fn <- getFromNamespace("update_dpobj_from_cpp_result",
                                                      "dirichletprocess")
  update_nfv_dpobj_from_cpp_batch_result_fn <- getFromNamespace(
    "update_normal_fixed_variance_dpobj_from_cpp_batch_result",
    "dirichletprocess"
  )

  dp <- DirichletProcessGaussianFixedVariance(c(-4.2, -4.0, -3.8, 0.0, 0.1, 0.3, 3.9, 4.1, 4.2),
                                              sigma = 1.5,
                                              g0Priors = c(0, 1),
                                              alphaPriors = c(2, 4),
                                              cpp = TRUE)
  skip_if_not(can_use_cpp(dp), "C++ sampler not available")

  dp$clusterLabels <- c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L)
  dp$numberClusters <- 3L
  dp$pointsPerCluster <- c(3, 3, 3)
  dp$weights <- dp$pointsPerCluster / dp$n
  dp$alpha <- 1.75
  dp$clusterParameters <- list(
    array(c(-4.0, 0.1, 4.05), dim = c(1, 1, 3))
  )

  mixing_params <- prepare_mixing_dist_params_fn(dp)

  set.seed(321)
  batch_result <- run_nfv_fit_cpp_batch_fn(
    as.matrix(dp$data),
    mixing_params,
    prepare_mcmc_params_fn(dp, its = 1, updatePrior = FALSE,
                           n_burn = 0, thin = 1, store_history = FALSE)
  )

  dp_batch <- update_nfv_dpobj_from_cpp_batch_result_fn(
    unserialize(serialize(dp, NULL)),
    batch_result
  )

  dp_step <- unserialize(serialize(dp, NULL))
  expected_alpha <- dp_step$alpha
  expected_likelihood <- sum(log(LikelihoodDP(dp_step)))
  expected_labels <- dp_step$clusterLabels
  expected_params <- dp_step$clusterParameters

  set.seed(321)
  step_result <- run_mcmc_cpp_fn(
    as.matrix(dp_step$data),
    mixing_params,
    prepare_mcmc_params_fn(dp_step, its = 1, updatePrior = FALSE,
                           n_burn = 0, thin = 1, store_history = FALSE)
  )
  dp_step <- update_dpobj_from_cpp_result_fn(dp_step, step_result)

  expect_equal(dp_batch$alphaChain, expected_alpha)
  expect_equal(dp_batch$likelihoodChain, expected_likelihood)
  expect_equal(dp_batch$labelsChain[[1]], expected_labels)
  expect_equal(unname(dp_batch$clusterParametersChain[[1]]),
               unname(expected_params))
  expect_equal(dp_batch$clusterLabels, dp_step$clusterLabels)
  expect_equal(dp_batch$alpha, dp_step$alpha)
  expect_equal(dp_batch$clusterParameters, dp_step$clusterParameters)
})

test_that("exponential batch fast path preserves one-step semantics and repaired-style storage", {
  prepare_mixing_dist_params_fn <- getFromNamespace("prepare_mixing_dist_params",
                                                    "dirichletprocess")
  prepare_mcmc_params_fn <- getFromNamespace("prepare_mcmc_params",
                                             "dirichletprocess")
  run_mcmc_cpp_fn <- getFromNamespace("run_mcmc_cpp", "dirichletprocess")
  run_exp_fit_cpp_batch_fn <- getFromNamespace("run_exponential_fit_cpp_batch",
                                               "dirichletprocess")
  update_dpobj_from_cpp_result_fn <- getFromNamespace("update_dpobj_from_cpp_result",
                                                      "dirichletprocess")
  update_exp_dpobj_from_cpp_batch_result_fn <- getFromNamespace(
    "update_exponential_dpobj_from_cpp_batch_result",
    "dirichletprocess"
  )

  dp <- DirichletProcessExponential(c(0.05, 0.08, 0.11, 0.35, 0.40, 0.52, 1.25, 1.40, 1.55),
                                    g0Priors = c(0.5, 0.5),
                                    alphaPriors = c(2, 4),
                                    cpp = TRUE)
  skip_if_not(can_use_cpp(dp), "C++ sampler not available")

  dp$clusterLabels <- c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L)
  dp$numberClusters <- 3L
  dp$pointsPerCluster <- c(3, 3, 3)
  dp$weights <- dp$pointsPerCluster / dp$n
  dp$alpha <- 1.75
  dp$clusterParameters <- list(
    array(c(8.5, 2.5, 0.7), dim = c(1, 1, 3))
  )

  mixing_params <- prepare_mixing_dist_params_fn(dp)

  set.seed(456)
  batch_result <- run_exp_fit_cpp_batch_fn(
    as.matrix(dp$data),
    mixing_params,
    prepare_mcmc_params_fn(dp, its = 1, updatePrior = FALSE,
                           n_burn = 0, thin = 1, store_history = FALSE)
  )

  dp_batch <- update_exp_dpobj_from_cpp_batch_result_fn(
    unserialize(serialize(dp, NULL)),
    batch_result
  )

  dp_step <- unserialize(serialize(dp, NULL))
  expected_alpha <- dp_step$alpha
  expected_likelihood <- sum(log(LikelihoodDP(dp_step)))
  expected_labels <- dp_step$clusterLabels
  expected_params <- dp_step$clusterParameters

  set.seed(456)
  step_result <- run_mcmc_cpp_fn(
    as.matrix(dp_step$data),
    mixing_params,
    prepare_mcmc_params_fn(dp_step, its = 1, updatePrior = FALSE,
                           n_burn = 0, thin = 1, store_history = FALSE)
  )
  dp_step <- update_dpobj_from_cpp_result_fn(dp_step, step_result)

  expect_equal(dp_batch$alphaChain, expected_alpha)
  expect_equal(dp_batch$likelihoodChain, expected_likelihood)
  expect_equal(dp_batch$labelsChain[[1]], expected_labels)
  expect_equal(unname(dp_batch$clusterParametersChain[[1]]),
               unname(expected_params))
  expect_equal(dp_batch$clusterLabels, dp_step$clusterLabels)
  expect_equal(dp_batch$alpha, dp_step$alpha)
  expect_equal(dp_batch$clusterParameters, dp_step$clusterParameters)
})

test_that("mvnormal FULL batch fast path preserves one-step semantics and repaired-style storage", {
  prepare_mixing_dist_params_fn <- getFromNamespace("prepare_mixing_dist_params",
                                                    "dirichletprocess")
  prepare_mcmc_params_fn <- getFromNamespace("prepare_mcmc_params",
                                             "dirichletprocess")
  run_mcmc_cpp_fn <- getFromNamespace("run_mcmc_cpp", "dirichletprocess")
  run_mvnormal_fit_cpp_batch_fn <- getFromNamespace("run_mvnormal_fit_cpp_batch",
                                                    "dirichletprocess")
  update_dpobj_from_cpp_result_fn <- getFromNamespace("update_dpobj_from_cpp_result",
                                                      "dirichletprocess")
  update_mvnormal_dpobj_from_cpp_batch_result_fn <- getFromNamespace(
    "update_mvnormal_dpobj_from_cpp_batch_result",
    "dirichletprocess"
  )

  dp <- DirichletProcessMvnormal(
    matrix(c(0.0, 0.1,
             0.2, -0.1,
             1.1, 1.0,
             1.2, 0.9,
             -1.0, -0.9,
             -1.1, -1.2), byrow = TRUE, ncol = 2),
    cpp = TRUE
  )
  skip_if_not(can_use_cpp(dp), "C++ sampler not available")

  mu <- array(0, dim = c(1, 2, 3))
  mu[, , 1] <- matrix(c(0, 0), nrow = 1)
  mu[, , 2] <- matrix(c(1, 1), nrow = 1)
  mu[, , 3] <- matrix(c(-1, -1), nrow = 1)

  sig <- array(0, dim = c(2, 2, 3))
  sig[, , 1] <- diag(2)
  sig[, , 2] <- matrix(c(1.0, 0.2,
                         0.2, 1.3), nrow = 2)
  sig[, , 3] <- matrix(c(0.7, 0.1,
                         0.1, 0.8), nrow = 2)

  dp$clusterLabels <- c(1L, 1L, 2L, 2L, 3L, 3L)
  dp$numberClusters <- 3L
  dp$pointsPerCluster <- c(2, 2, 2)
  dp$weights <- dp$pointsPerCluster / dp$n
  dp$alpha <- 1.75
  dp$clusterParameters <- list(mu = mu, sig = sig)

  mixing_params <- prepare_mixing_dist_params_fn(dp)

  set.seed(654)
  batch_result <- run_mvnormal_fit_cpp_batch_fn(
    as.matrix(dp$data),
    mixing_params,
    prepare_mcmc_params_fn(dp, its = 1, updatePrior = FALSE,
                           n_burn = 0, thin = 1, store_history = FALSE)
  )

  dp_batch <- update_mvnormal_dpobj_from_cpp_batch_result_fn(
    unserialize(serialize(dp, NULL)),
    batch_result
  )

  dp_step <- unserialize(serialize(dp, NULL))
  expected_alpha <- dp_step$alpha
  expected_likelihood <- sum(log(LikelihoodDP(dp_step)))
  expected_labels <- dp_step$clusterLabels
  expected_params <- dp_step$clusterParameters

  set.seed(654)
  step_result <- run_mcmc_cpp_fn(
    as.matrix(dp_step$data),
    mixing_params,
    prepare_mcmc_params_fn(dp_step, its = 1, updatePrior = FALSE,
                           n_burn = 0, thin = 1, store_history = FALSE)
  )
  dp_step <- update_dpobj_from_cpp_result_fn(dp_step, step_result)

  expect_equal(dp_batch$alphaChain, expected_alpha)
  expect_equal(dp_batch$likelihoodChain, expected_likelihood)
  expect_equal(dp_batch$labelsChain[[1]], expected_labels)
  expect_equal(unname(dp_batch$clusterParametersChain[[1]]),
               unname(expected_params))
  expect_equal(dp_batch$clusterLabels, dp_step$clusterLabels)
  expect_equal(dp_batch$alpha, dp_step$alpha)
  expect_equal(dp_batch$clusterParameters, dp_step$clusterParameters)
})

test_that("C++ Fit rejects unsupported updatePrior families and stores supported prior chains", {
  old_cpp_setting <- using_cpp()
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)

  gaussian_dp <- DirichletProcessGaussian(rnorm(6))
  set_use_cpp(TRUE)
  skip_if_not(can_use_cpp(gaussian_dp), "C++ sampler not available")

  expect_error(
    Fit(gaussian_dp, 1, TRUE, FALSE),
    "not supported for model family 'normal'"
  )

  weibull_dp <- DirichletProcessWeibull(rweibull(6, 1, 1),
                                        matrix(c(1, 1, 1), ncol = 3),
                                        verbose = FALSE,
                                        mhDraws = 10)
  set_use_cpp(TRUE)
  weibull_dp <- Fit(weibull_dp, 2, TRUE, FALSE)

  expect_length(weibull_dp$priorParametersChain, 2)
})
