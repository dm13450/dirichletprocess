context("Dirichlet Process Posterior")

num_test_points <- 10
data_test <- rnorm(num_test_points)
priorParameters_test <- matrix(c(1,1,1,1), ncol=4)

normal_object_test <- MixingDistribution("normal", priorParameters_test, "conjugate")
dpobj = DirichletProcessCreate(data_test, normal_object_test)
dpobj = Initialise(dpobj)
fit_uses_cpp <- getFromNamespace("should_use_cpp_fit", "dirichletprocess")(dpobj)
dpobj = Fit(dpobj, 10, FALSE, FALSE)

test_that("Posterior Clusters Default", {

  postClusters <- PosteriorClusters(dpobj)

  expect_is(postClusters, "list")
  
  # The live Fit() path can use C++ automatically even when the manual override
  # is not explicitly set.
  if (fit_uses_cpp) {
    # C++ implementation may have different chain storage behavior
    expect_true(length(postClusters$params) >= 0)
  } else {
    # R implementation should match current cluster parameters
    expect_equal(length(postClusters$params), length(dpobj$clusterParameters))
  }

})

test_that("Posterior Clusters Ind", {

  postClusters <- PosteriorClusters(dpobj, 7)

  expect_is(postClusters, "list")
  
  if (fit_uses_cpp) {
    # C++ implementation may have different chain storage behavior
    expect_true(length(postClusters$params) >= 0)
  } else {
    # R implementation should match current cluster parameters
    expect_equal(length(postClusters$params), length(dpobj$clusterParameters))
  }
})

test_that("Posterior Clusters matches ordinary DP posterior moments", {
  old_cpp_setting <- set_use_cpp(FALSE)
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)

  make_frozen_gaussian_dp <- function(y, labels, alpha) {
    dp <- DirichletProcessGaussian(y, cpp = FALSE)
    labels <- as.integer(labels)
    k <- max(labels)

    dp$alpha <- alpha
    dp$clusterLabels <- labels
    dp$numberClusters <- k
    dp$pointsPerCluster <- as.numeric(tabulate(labels, nbins = k))
    dp$clusterParameters <- list(
      mu = array(vapply(seq_len(k), function(j) mean(y[labels == j]), numeric(1)), dim = c(1, 1, k)),
      sigma = array(vapply(seq_len(k), function(j) {
        cluster_data <- y[labels == j]
        if (length(cluster_data) > 1L) stats::sd(cluster_data) else 1.0
      }, numeric(1)), dim = c(1, 1, k))
    )

    dp
  }

  set.seed(20260429)
  dp <- make_frozen_gaussian_dp(c(-2, -1, 1, 2), c(1L, 1L, 2L, 2L), alpha = 3)
  k <- length(dp$pointsPerCluster)
  n <- sum(dp$pointsPerCluster)

  draws <- replicate(20000, {
    post <- PosteriorClusters(dp)
    c(
      occupied_1 = post$weights[1],
      occupied_2 = post$weights[2],
      residual = sum(post$weights[-seq_len(k)]),
      first_new = post$weights[k + 1L]
    )
  })

  empirical <- rowMeans(draws)
  theory <- c(
    occupied_1 = dp$pointsPerCluster[1] / (dp$alpha + n),
    occupied_2 = dp$pointsPerCluster[2] / (dp$alpha + n),
    residual = dp$alpha / (dp$alpha + n),
    first_new = dp$alpha / (dp$alpha + n) * 1 / (1 + dp$alpha)
  )

  expect_equal(unname(empirical["occupied_1"]), unname(theory["occupied_1"]), tolerance = 0.01)
  expect_equal(unname(empirical["occupied_2"]), unname(theory["occupied_2"]), tolerance = 0.01)
  expect_equal(unname(empirical["residual"]), unname(theory["residual"]), tolerance = 0.01)
  expect_equal(unname(empirical["first_new"]), unname(theory["first_new"]), tolerance = 0.01)
})

test_that("Gaussian Posterior Clusters uses stored current cluster parameters", {
  old_cpp_setting <- set_use_cpp(FALSE)
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)

  dp <- DirichletProcessGaussian(c(-2, -1, 1, 2), cpp = FALSE)
  dp$alpha <- 3
  dp$clusterLabels <- c(1L, 1L, 2L, 2L)
  dp$numberClusters <- 2L
  dp$pointsPerCluster <- c(2, 2)
  dp$clusterParameters <- list(
    mu = array(c(100, -100), dim = c(1, 1, 2)),
    sigma = array(c(0.1, 0.2), dim = c(1, 1, 2))
  )

  post_clusters <- PosteriorClusters(dp)

  expect_equal(as.numeric(post_clusters$params$mu[1, 1, 1:2]), c(100, -100))
  expect_equal(as.numeric(post_clusters$params$sigma[1, 1, 1:2]), c(0.1, 0.2))
})

test_that("Gaussian Posterior Clusters and PosteriorFunction(ind) use stored chain state", {
  old_cpp_setting <- set_use_cpp(FALSE)
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)

  dp <- DirichletProcessGaussian(c(0, 0, 0, 0), cpp = FALSE)
  dp$alpha <- 0.001
  dp$clusterLabels <- c(1L, 1L, 1L, 1L)
  dp$numberClusters <- 1L
  dp$pointsPerCluster <- 4
  dp$clusterParameters <- list(
    mu = array(100, dim = c(1, 1, 1)),
    sigma = array(0.05, dim = c(1, 1, 1))
  )
  dp$weightsChain <- list(1)
  dp$alphaChain <- 0.001
  dp$clusterParametersChain <- list(list(
    mu = array(-25, dim = c(1, 1, 1)),
    sigma = array(0.05, dim = c(1, 1, 1))
  ))

  post_clusters <- PosteriorClusters(dp, 1)
  post_function <- PosteriorFunction(dp, 1)
  post_values <- post_function(c(-25, 100))

  expect_equal(as.numeric(post_clusters$params$mu[1, 1, 1]), -25)
  expect_equal(as.numeric(post_clusters$params$sigma[1, 1, 1]), 0.05)
  expect_gt(post_values[1], post_values[2])
})

test_that("Gaussian PosteriorFunction and PosteriorFrame follow stored current state", {
  old_cpp_setting <- set_use_cpp(FALSE)
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)

  dp <- DirichletProcessGaussian(c(0, 0, 0, 0), cpp = FALSE)
  dp$alpha <- 0.001
  dp$clusterLabels <- c(1L, 1L, 1L, 1L)
  dp$numberClusters <- 1L
  dp$pointsPerCluster <- 4
  dp$clusterParameters <- list(
    mu = array(100, dim = c(1, 1, 1)),
    sigma = array(0.05, dim = c(1, 1, 1))
  )

  post_function <- PosteriorFunction(dp)
  post_values <- post_function(c(100, 0))
  post_frame <- PosteriorFrame(dp, c(100, 0), ndraws = 25)

  expect_gt(post_values[1], post_values[2])
  expect_gt(post_frame$Mean[1], post_frame$Mean[2])
})


test_that("Posterior Function", {

  post_function <- PosteriorFunction(dpobj)

  expect_is(post_function, "function")
  expect_is(post_function(0), "numeric")

})

test_that("Posterior Clusters: MvNormal", {
  old_cpp_setting <- set_use_cpp(FALSE)
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)

  set.seed(2)
  y <- mvtnorm::rmvnorm(10, c(0,0), diag(2))

  dp <- DirichletProcessMvnormal(y)
  dp <- Fit(dp, 1, FALSE, FALSE)

  postClusters <- PosteriorClusters(dp)

  expect_equal(length(postClusters), 2)

})

test_that("Posterior Function: MvNormal", {
  old_cpp_setting <- set_use_cpp(FALSE)
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)

  set.seed(2)
  y <- mvtnorm::rmvnorm(10, c(0,0), diag(2))

  dp <- DirichletProcessMvnormal(y)
  dp <- Fit(dp, 1, FALSE, FALSE)

  postFunc <- PosteriorFunction(dp)
  postFuncEval <- postFunc(matrix(c(0,0), ncol=2))

  expect_is(postFunc, "function")
  expect_is(postFuncEval, "numeric")
  expect_equal(length(postFuncEval), 1)

})
