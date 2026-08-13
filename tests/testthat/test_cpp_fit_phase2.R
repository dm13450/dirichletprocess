context("C++ Fit Phase 2 Model Kernels")

test_that("prepare_mixing_dist_params carries mhDraws into beta and mvnormal2 kernels", {
  prepare_mixing_dist_params_fn <- getFromNamespace("prepare_mixing_dist_params",
                                                    "dirichletprocess")

  beta_dp <- DirichletProcessBeta(rbeta(8, 2, 5), verbose = FALSE)
  beta_dp$mhDraws <- 37
  beta_params <- prepare_mixing_dist_params_fn(beta_dp)
  expect_identical(beta_params$mh_draws, 37)
  expect_equal(beta_params$mhStepSize, beta_dp$mixingDistribution$mhStepSize)

  mvnormal2_dp <- DirichletProcessMvnormal2(matrix(rnorm(16), ncol = 2))
  mvnormal2_dp$mhDraws <- 19
  mvnormal2_params <- prepare_mixing_dist_params_fn(mvnormal2_dp)
  expect_identical(mvnormal2_params$mh_draws, 19)
})

test_that("fallback Fit.default uses repaired updatePrior semantics", {
  old_cpp_setting <- using_cpp()
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)
  set_use_cpp(FALSE)

  gaussian_dp <- DirichletProcessGaussian(rnorm(10))
  expect_error(
    Fit(gaussian_dp, 1, TRUE, FALSE),
    "not supported for model family 'normal'"
  )
})

test_that("weibull prior density includes the lambda contribution", {
  md <- WeibullMixtureCreate(matrix(c(6, 2, 3), ncol = 3), mhStepSize = 0.2)
  theta <- list(array(1.5, dim = c(1, 1, 1)),
                array(2.0, dim = c(1, 1, 1)))

  expected <- dunif(theta[[1]], 0, 6) *
    dgamma(1 / theta[[2]], 2, 3) / theta[[2]]^2

  expect_equal(PriorDensity(md, theta), expected)
})

test_that("weibull LikelihoodDP matches repaired R bookkeeping", {
  dp <- DirichletProcessWeibull(c(0.2, 0.4, 0.7, 1.3, 2.1, 3.4),
                                g0Priors = c(6, 2, 3),
                                mhStepSize = 0.2,
                                verbose = FALSE)

  dp$clusterLabels <- c(1, 1, 2, 2, 2, 1)
  dp$numberClusters <- 2
  dp$pointsPerCluster <- c(3, 3)
  dp$clusterParameters <- list(array(c(1.2, 0.9), dim = c(1, 1, 2)),
                               array(c(2.0, 1.5), dim = c(1, 1, 2)))

  expected <- vapply(seq_len(nrow(dp$data)),
                     function(i) Likelihood(dp$mixingDistribution,
                                            dp$data[i, , drop = FALSE],
                                            dp$clusterParameters),
                     numeric(dp$numberClusters))
  dim(expected) <- c(nrow(dp$data), dp$numberClusters)
  expected <- as.matrix(expected) %*% (dp$pointsPerCluster / dp$n)

  expect_equal(LikelihoodDP(dp), expected)
})

test_that("normalFixedVariance prior draw uses sigma as in the repaired R package", {
  prior_draw_helper <- getFromNamespace("cpp_normal_fixed_variance_prior_draw",
                                        "dirichletprocess")

  set.seed(1)
  draws <- prior_draw_helper(mu0 = 0, sigma0 = 0.01, sigma = 2, n = 4000)

  expect_gt(stats::sd(draws), 1.0)
})

test_that("normalFixedVariance posterior draw matches the repaired R scale convention", {
  posterior_draw_helper <- getFromNamespace("cpp_normal_fixed_variance_posterior_draw",
                                            "dirichletprocess")

  md <- GaussianFixedVarianceMixtureCreate(c(0, 1), sigma = 1.5)
  x <- matrix(c(-4.2, -4.0, -3.8), ncol = 1)

  set.seed(999)
  draw_r <- PosteriorDraw(md, x, 5)[[1]][1, 1, ]
  set.seed(999)
  draw_cpp <- posterior_draw_helper(x, md$priorParameters[1], md$priorParameters[2], md$sigma, 5)

  expect_equal(draw_cpp, draw_r)
})

test_that("normalFixedVariance LikelihoodDP matches repaired R bookkeeping", {
  dp <- DirichletProcessGaussianFixedVariance(c(-4.2, -4.0, -3.8, 0.0, 0.1, 0.3),
                                              sigma = 1.5)

  dp$clusterLabels <- c(1, 1, 2, 2, 3, 3)
  dp$numberClusters <- 3
  dp$pointsPerCluster <- c(2, 2, 2)
  dp$clusterParameters <- list(array(c(-4.1, 0.05, 0.25), dim = c(1, 1, 3)))

  expected <- vapply(seq_len(nrow(dp$data)),
                     function(i) Likelihood(dp$mixingDistribution,
                                            dp$data[i, , drop = FALSE],
                                            dp$clusterParameters),
                     numeric(dp$numberClusters))
  dim(expected) <- c(nrow(dp$data), dp$numberClusters)
  expected <- as.matrix(expected) %*% (dp$pointsPerCluster / dp$n)

  expect_equal(LikelihoodDP(dp), expected)
})

test_that("exponential LikelihoodDP matches repaired R bookkeeping", {
  old_cpp_setting <- using_cpp()
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)
  set_use_cpp(TRUE)

  dp <- DirichletProcessExponential(c(0.15, 0.4, 1.1, 2.2, 3.5), cpp = TRUE)

  dp$clusterLabels <- c(1, 1, 2, 3, 3)
  dp$numberClusters <- 3
  dp$pointsPerCluster <- c(2, 1, 2)
  dp$clusterParameters <- list(array(c(0.5, 2.0, 5.0), dim = c(1, 1, 3)))

  expected <- vapply(
    seq_len(nrow(dp$data)),
    function(i) {
      Likelihood(dp$mixingDistribution,
                 dp$data[i, , drop = FALSE],
                 dp$clusterParameters)
    },
    numeric(dp$numberClusters)
  )

  dim(expected) <- c(nrow(dp$data), dp$numberClusters)
  expected <- as.matrix(expected) %*% (dp$pointsPerCluster / dp$n)

  expect_equal(LikelihoodDP(dp), expected)
})

test_that("exponential live C++ Fit stores repaired-R pre-update likelihoodChain", {
  old_cpp_setting <- using_cpp()
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)
  set_use_cpp(TRUE)

  dp <- DirichletProcessExponential(c(0.15, 0.4, 1.1, 2.2, 3.5), cpp = TRUE)

  dp$clusterLabels <- c(1, 1, 2, 3, 3)
  dp$numberClusters <- 3
  dp$pointsPerCluster <- c(2, 1, 2)
  dp$clusterParameters <- list(array(c(0.5, 2.0, 5.0), dim = c(1, 1, 3)))
  dp$weights <- dp$pointsPerCluster / dp$n

  expected <- sum(log(LikelihoodDP(dp)))

  set.seed(123)
  fit_dp <- Fit(dp, 1, FALSE, FALSE)

  expect_equal(fit_dp$likelihoodChain[1], expected)
})

test_that("beta2 cpp=TRUE constructor initializes through repaired-R nonconjugate semantics", {
  old_cpp_setting <- using_cpp()
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)

  y <- c(0.4, 0.8, 1.3, 2.0, 2.7, 3.3)

  set.seed(42)
  dp_r <- DirichletProcessBeta2(y, maxY = 4, mhDraws = 40, verbose = FALSE, cpp = FALSE)

  set.seed(42)
  dp_cpp <- DirichletProcessBeta2(y, maxY = 4, mhDraws = 40, verbose = FALSE, cpp = TRUE)

  expect_equal(dp_cpp$clusterLabels, dp_r$clusterLabels)
  expect_equal(dp_cpp$pointsPerCluster, dp_r$pointsPerCluster)
  expect_equal(dp_cpp$alpha, dp_r$alpha)
  expect_equal(dp_cpp$clusterParameters, dp_r$clusterParameters)
})

test_that("beta2 LikelihoodDP matches repaired R bookkeeping", {
  old_cpp_setting <- using_cpp()
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)
  set_use_cpp(TRUE)

  dp <- DirichletProcessBeta2(c(0.4, 0.8, 1.3, 2.0, 2.7, 3.3),
                              maxY = 4,
                              mhDraws = 40,
                              verbose = FALSE,
                              cpp = TRUE)

  dp$clusterLabels <- c(1, 1, 2, 2, 3, 3)
  dp$numberClusters <- 3
  dp$pointsPerCluster <- c(2, 2, 2)
  dp$clusterParameters <- list(
    mu = array(c(0.6, 2.0, 3.0), dim = c(1, 1, 3)),
    nu = array(c(5.0, 8.0, 10.0), dim = c(1, 1, 3))
  )

  expected <- vapply(
    seq_len(nrow(dp$data)),
    function(i) Likelihood(dp$mixingDistribution,
                           dp$data[i, , drop = FALSE],
                           dp$clusterParameters),
    numeric(dp$numberClusters)
  )
  dim(expected) <- c(nrow(dp$data), dp$numberClusters)
  expected <- as.matrix(expected) %*% (dp$pointsPerCluster / dp$n)

  expect_equal(LikelihoodDP(dp), expected)
})

test_that("beta2 live C++ Fit stores repaired-R pre-update likelihoodChain", {
  old_cpp_setting <- using_cpp()
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)
  set_use_cpp(TRUE)

  dp <- DirichletProcessBeta2(c(0.4, 0.8, 1.3, 2.0, 2.7, 3.3),
                              maxY = 4,
                              mhDraws = 40,
                              verbose = FALSE,
                              cpp = TRUE)

  dp$clusterLabels <- c(1, 1, 2, 2, 3, 3)
  dp$numberClusters <- 3
  dp$pointsPerCluster <- c(2, 2, 2)
  dp$clusterParameters <- list(
    mu = array(c(0.6, 2.0, 3.0), dim = c(1, 1, 3)),
    nu = array(c(5.0, 8.0, 10.0), dim = c(1, 1, 3))
  )
  dp$weights <- dp$pointsPerCluster / dp$n
  dp$alpha <- 1.7

  expected <- sum(log(LikelihoodDP(dp)))

  set.seed(123)
  fit_dp <- Fit(dp, 1, FALSE, FALSE)

  expect_equal(fit_dp$likelihoodChain[1], expected)
  expect_equal(fit_dp$alphaChain[1], dp$alpha)
  expect_equal(fit_dp$labelsChain[[1]], dp$clusterLabels)
})

test_that("beta2 live C++ kernel honors the incoming warm start when mhDraws = 1", {
  old_cpp_setting <- using_cpp()
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)
  set_use_cpp(TRUE)

  dp <- DirichletProcessBeta2(c(0.4, 0.8, 1.3, 2.0, 2.7, 3.3),
                              maxY = 4,
                              mhDraws = 1,
                              verbose = FALSE,
                              cpp = TRUE)

  dp$clusterLabels <- rep(1, 6)
  dp$numberClusters <- 1
  dp$pointsPerCluster <- 6
  dp$clusterParameters <- list(
    mu = array(2.1, dim = c(1, 1, 1)),
    nu = array(9.5, dim = c(1, 1, 1))
  )
  dp$weights <- 1
  dp$alpha <- 1e-12
  dp$m <- 1

  set.seed(456)
  fit_dp <- Fit(dp, 1, FALSE, FALSE)

  expect_equal(fit_dp$numberClusters, 1)
  expect_equal(fit_dp$clusterParameters, dp$clusterParameters)
})

test_that("normalFixedVariance live C++ Fit matches repaired R singleton cleanup path", {
  old_cpp_setting <- using_cpp()
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)

  copy_state <- function(src, dst) {
    for (nm in c("alpha", "clusterLabels", "pointsPerCluster",
                 "numberClusters", "clusterParameters", "weights")) {
      dst[[nm]] <- unserialize(serialize(src[[nm]], NULL))
    }
    dst
  }

  edge_y <- c(-12, -11.7, -0.2, 0, 0.05, 0.2, 9.8, 10.1)

  set_use_cpp(FALSE)
  base_r <- NULL
  for (s in 1:100) {
    set.seed(s)
    cand <- DirichletProcessGaussianFixedVariance(edge_y, sigma = 1.5)
    cand <- Fit(cand, 6, FALSE, FALSE)
    if (cand$numberClusters >= 3 && any(cand$pointsPerCluster == 1)) {
      base_r <- cand
      break
    }
  }

  expect_false(is.null(base_r))

  r_obj <- unserialize(serialize(base_r, NULL))

  set_use_cpp(TRUE)
  c_obj <- DirichletProcessGaussianFixedVariance(edge_y, sigma = 1.5, cpp = TRUE)
  c_obj <- copy_state(base_r, c_obj)

  set.seed(2024)
  r_fit <- Fit(r_obj, 1, FALSE, FALSE)

  set.seed(2024)
  c_fit <- Fit(c_obj, 1, FALSE, FALSE)

  expect_equal(c_fit$clusterLabels, r_fit$clusterLabels)
  expect_equal(c_fit$pointsPerCluster, r_fit$pointsPerCluster)
  expect_equal(c_fit$alpha, r_fit$alpha)
  expect_equal(c_fit$clusterParameters[[1]], r_fit$clusterParameters[[1]])
})

test_that("mvnormal2 posterior draw uses the repaired warm start when provided", {
  md <- Mvnormal2Create(list(mu0 = matrix(c(0, 0), nrow = 1),
                             sigma0 = diag(2),
                             phi0 = diag(2),
                             nu0 = 4))
  x <- matrix(c(3, 3,
                3.5, 2.5,
                2.8, 3.2), byrow = TRUE, ncol = 2)

  start_a <- list(array(matrix(c(0, 0), nrow = 1), dim = c(1, 2, 1)),
                  array(diag(2), dim = c(2, 2, 1)))
  start_b <- list(array(matrix(c(4, 4), nrow = 1), dim = c(1, 2, 1)),
                  array(diag(2), dim = c(2, 2, 1)))

  set.seed(123)
  draw_a <- PosteriorDraw(md, x, n = 1, start_pos = start_a)
  set.seed(123)
  draw_b <- PosteriorDraw(md, x, n = 1, start_pos = start_b)

  expect_false(isTRUE(all.equal(draw_a$mu, draw_b$mu)))
})

test_that("mvnormal2 LikelihoodDP matches repaired R bookkeeping", {
  old_cpp_setting <- using_cpp()
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)
  set_use_cpp(TRUE)

  dp <- DirichletProcessMvnormal2(
    matrix(c(0.0, 0.1,
             0.2, -0.1,
             1.1, 1.0,
             1.2, 0.9,
             -1.0, -0.9,
             -1.1, -1.2), byrow = TRUE, ncol = 2),
    cpp = TRUE
  )

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

  dp$clusterLabels <- c(1, 1, 2, 2, 3, 3)
  dp$numberClusters <- 3
  dp$pointsPerCluster <- c(2, 2, 2)
  dp$clusterParameters <- list(mu, sig)

  expected <- vapply(
    seq_len(nrow(dp$data)),
    function(i) {
      c(
        mvtnorm::dmvnorm(dp$data[i, , drop = FALSE], mu[, , 1], sig[, , 1]),
        mvtnorm::dmvnorm(dp$data[i, , drop = FALSE], mu[, , 2], sig[, , 2]),
        mvtnorm::dmvnorm(dp$data[i, , drop = FALSE], mu[, , 3], sig[, , 3])
      )
    },
    numeric(dp$numberClusters)
  )

  dim(expected) <- c(nrow(dp$data), dp$numberClusters)
  expected <- as.matrix(expected) %*% (dp$pointsPerCluster / dp$n)

  expect_equal(LikelihoodDP(dp), expected)
})

test_that("mvnormal FULL LikelihoodDP matches repaired R bookkeeping", {
  old_cpp_setting <- using_cpp()
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)
  set_use_cpp(TRUE)

  dp <- DirichletProcessMvnormal(
    matrix(c(0.0, 0.1,
             0.2, -0.1,
             1.1, 1.0,
             1.2, 0.9,
             -1.0, -0.9,
             -1.1, -1.2), byrow = TRUE, ncol = 2),
    cpp = TRUE
  )

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

  dp$clusterLabels <- c(1, 1, 2, 2, 3, 3)
  dp$numberClusters <- 3
  dp$pointsPerCluster <- c(2, 2, 2)
  dp$clusterParameters <- list(mu = mu, sig = sig)

  expected <- vapply(
    seq_len(nrow(dp$data)),
    function(i) {
      Likelihood(dp$mixingDistribution,
                 dp$data[i, , drop = FALSE],
                 dp$clusterParameters)
    },
    numeric(dp$numberClusters)
  )

  dim(expected) <- c(nrow(dp$data), dp$numberClusters)
  expected <- as.matrix(expected) %*% (dp$pointsPerCluster / dp$n)

  expect_equal(LikelihoodDP(dp), expected)
})

test_that("mvnormal FULL live C++ Fit stores repaired-R pre-update likelihoodChain", {
  old_cpp_setting <- using_cpp()
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)
  set_use_cpp(TRUE)

  test_data <- matrix(c(0.0, 0.1,
                        0.2, -0.1,
                        1.1, 1.0,
                        1.2, 0.9,
                        -1.0, -0.9,
                        -1.1, -1.2), byrow = TRUE, ncol = 2)

  dp <- DirichletProcessMvnormal(test_data, cpp = TRUE)

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

  dp$clusterLabels <- c(1, 1, 2, 2, 3, 3)
  dp$numberClusters <- 3
  dp$pointsPerCluster <- c(2, 2, 2)
  dp$clusterParameters <- list(mu = mu, sig = sig)
  dp$weights <- dp$pointsPerCluster / dp$n

  expected <- sum(log(LikelihoodDP(dp)))

  set.seed(123)
  fit_dp <- Fit(dp, 1, FALSE, FALSE)

  expect_equal(fit_dp$likelihoodChain[1], expected)
})
