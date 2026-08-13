context("Beta Initialization")

posterior_peak_location <- function(dp_obj, grid = seq(0.05, 0.95, by = 0.10)) {
  posterior_values <- PosteriorFunction(dp_obj)(grid)
  grid[which.max(posterior_values)]
}

test_that("Beta initialization preserves the sampled alpha draw", {
  pts <- c(0.12, 0.19, 0.23, 0.28, 0.31, 0.37, 0.41, 0.46, 0.52, 0.61)
  md_obj <- BetaMixtureCreate(c(2, 8), c(1, 1), 1, hyperPriorParameters = c(1, 0.125))

  set.seed(20260430)
  raw_dp <- DirichletProcessCreate(pts, md_obj, c(2, 4))
  alpha_before_initialise <- raw_dp$alpha

  set.seed(20260430)
  beta_dp <- DirichletProcessBeta(pts, 1, verbose = FALSE)

  expect_equal(beta_dp$alpha, alpha_before_initialise)
  expect_false(isTRUE(all.equal(beta_dp$alpha, 0.5)))
})

test_that("Beta initialization remains a one-cluster constructor state", {
  set.seed(101)
  pts <- rbeta(24, 4, 6)

  set.seed(20260430)
  beta_dp <- DirichletProcessBeta(pts, 1, verbose = FALSE)

  expect_equal(beta_dp$numberClusters, 1)
  expect_equal(beta_dp$clusterLabels, rep(1, length(pts)))
  expect_equal(beta_dp$pointsPerCluster, length(pts))
  expect_equal(dim(beta_dp$clusterParameters$mu), c(1, 1, 1))
  expect_equal(dim(beta_dp$clusterParameters$nu), c(1, 1, 1))
})

test_that("Beta constructor posterior peak is data-informed for canonical datasets", {
  set.seed(101)
  interior_pts <- rbeta(24, 4, 6)
  set.seed(303)
  left_skew_pts <- rbeta(24, 0.8, 6)

  set.seed(20260430)
  interior_dp <- DirichletProcessBeta(interior_pts, 1, verbose = FALSE)
  set.seed(20260430)
  left_skew_dp <- DirichletProcessBeta(left_skew_pts, 1, verbose = FALSE)

  interior_peak <- posterior_peak_location(interior_dp)
  left_skew_peak <- posterior_peak_location(left_skew_dp)

  expect_true(interior_peak >= 0.15 && interior_peak <= 0.55)
  expect_true(left_skew_peak <= 0.15)
})
