context("Posterior Summary")

manual_posterior_summary <- function(dp, x, inds, level = 0.95) {
  draws <- vapply(
    inds,
    function(ind) PosteriorFunction(dp, ind = ind)(x),
    numeric(length(x))
  )

  if (!is.matrix(draws)) {
    draws <- matrix(draws, nrow = length(x))
  }

  tail_prob <- (1 - level) / 2

  data.frame(
    x = x,
    Mean = rowMeans(draws),
    Median = apply(draws, 1, stats::median),
    Lower = apply(draws, 1, stats::quantile, probs = tail_prob, names = FALSE),
    Upper = apply(draws, 1, stats::quantile, probs = 1 - tail_prob, names = FALSE)
  )
}

make_expcustom_md_for_summary_validation <- function() {
  md <- ExponentialMixtureCreate(c(0.5, 0.5))
  class(md) <- c("expcustom", "exponential", "conjugate")
  md
}

test_that("PosteriorSummary returns a tidy ordinary-DP summary frame", {
  old_cpp_setting <- set_use_cpp(FALSE)
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)

  set.seed(301)
  dp <- DirichletProcessGaussian(rnorm(20), cpp = FALSE)
  dp <- Fit(dp, 6, FALSE, FALSE)
  grid <- seq(-2, 2, length.out = 7)

  set.seed(302)
  summary_df <- PosteriorSummary(dp, grid)

  expect_s3_class(summary_df, "data.frame")
  expect_equal(names(summary_df), c("x", "Mean", "Median", "Lower", "Upper"))
  expect_equal(nrow(summary_df), length(grid))
  expect_equal(summary_df$x, grid)
})

test_that("PosteriorSummary returns finite summaries across representative ordinary models", {
  old_cpp_setting <- set_use_cpp(TRUE)
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)

  set.seed(311)
  model_cases <- list(
    gaussian = list(
      dp = Fit(DirichletProcessGaussian(rnorm(24), cpp = FALSE), 4, FALSE, FALSE),
      grid = seq(-2, 2, length.out = 7)
    ),
    exponential = list(
      dp = Fit(DirichletProcessExponential(rexp(24), cpp = FALSE), 4, FALSE, FALSE),
      grid = seq(0.05, 3, length.out = 7)
    ),
    beta = list(
      dp = Fit(DirichletProcessBeta(rbeta(24, 2, 5), maxY = 1, cpp = FALSE), 4, FALSE, FALSE),
      grid = seq(0.05, 0.95, length.out = 7)
    ),
    weibull = list(
      dp = Fit(DirichletProcessWeibull(rweibull(24, 1.8, 1.2),
                                       c(10, 2, 4),
                                       cpp = FALSE),
               4, FALSE, FALSE),
      grid = seq(0.05, 3, length.out = 7)
    ),
    expcustom = local({
      custom_dp <- DirichletProcessCreate(rexp(24),
                                          make_expcustom_md_for_summary_validation(),
                                          c(2, 4))
      custom_dp <- Initialise(custom_dp)
      custom_dp <- Fit(custom_dp, 4, FALSE, FALSE)
      list(dp = custom_dp, grid = seq(0.05, 3, length.out = 7))
    })
  )

  for (case_name in names(model_cases)) {
    case <- model_cases[[case_name]]
    summary_df <- PosteriorSummary(case$dp, case$grid)

    expect_true(is.data.frame(summary_df), info = case_name)
    expect_equal(names(summary_df), c("x", "Mean", "Median", "Lower", "Upper"),
                 info = case_name)
    expect_equal(nrow(summary_df), length(case$grid), info = case_name)
    expect_true(
      all(is.finite(as.matrix(summary_df[, c("Mean", "Median", "Lower", "Upper")]))),
      info = case_name
    )
  }
})

test_that("PosteriorSummary burnin uses stored iteration numbers", {
  old_cpp_setting <- set_use_cpp(FALSE)
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)

  set.seed(303)
  dp <- DirichletProcessGaussian(rnorm(20), cpp = FALSE)
  dp <- Fit(dp, 6, FALSE, FALSE, thinning = 2)
  grid <- seq(-1.5, 1.5, length.out = 5)

  expect_equal(dp$storedIterations, c(1L, 3L, 5L))

  set.seed(304)
  summary_df <- PosteriorSummary(dp, grid, burnin = 2)
  set.seed(304)
  manual_df <- manual_posterior_summary(dp, grid, inds = c(2L, 3L))

  expect_equal(summary_df, manual_df)
})

test_that("PosteriorSummary burnin respects appended stored iterations across Fit calls", {
  old_cpp_setting <- set_use_cpp(FALSE)
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)

  set.seed(312)
  dp <- DirichletProcessGaussian(rnorm(24), cpp = FALSE)
  dp <- Fit(dp, 4, FALSE, FALSE)
  dp <- Fit(dp, 4, FALSE, FALSE, thinning = 2)
  grid <- seq(-1.5, 1.5, length.out = 5)

  expect_equal(dp$storedIterations, c(1L, 2L, 3L, 4L, 5L, 7L))

  set.seed(313)
  summary_df <- PosteriorSummary(dp, grid, burnin = 4)
  set.seed(313)
  manual_df <- manual_posterior_summary(dp, grid, inds = c(5L, 6L))

  expect_equal(summary_df, manual_df)
})

test_that("PosteriorSummary summary-time thinning uses retained stored positions", {
  old_cpp_setting <- set_use_cpp(FALSE)
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)

  set.seed(305)
  dp <- DirichletProcessGaussian(rnorm(20), cpp = FALSE)
  dp <- Fit(dp, 6, FALSE, FALSE, thinning = 2)
  grid <- seq(-1, 1, length.out = 5)

  set.seed(306)
  summary_df <- PosteriorSummary(dp, grid, thinning = 2)
  set.seed(306)
  manual_df <- manual_posterior_summary(dp, grid, inds = c(1L, 3L))

  expect_equal(summary_df, manual_df)
})

test_that("PosteriorSummary level changes interval width as expected", {
  old_cpp_setting <- set_use_cpp(FALSE)
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)

  set.seed(307)
  dp <- DirichletProcessGaussian(rnorm(20), cpp = FALSE)
  dp <- Fit(dp, 8, FALSE, FALSE)
  grid <- seq(-1.5, 1.5, length.out = 6)

  set.seed(308)
  summary_95 <- PosteriorSummary(dp, grid, level = 0.95)
  set.seed(308)
  summary_50 <- PosteriorSummary(dp, grid, level = 0.50)

  width_95 <- summary_95$Upper - summary_95$Lower
  width_50 <- summary_50$Upper - summary_50$Lower

  expect_true(all(width_95 >= width_50 - 1e-12))
})

test_that("PosteriorSummary fails clearly when no stored samples exist", {
  old_cpp_setting <- set_use_cpp(FALSE)
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)

  set.seed(309)
  dp <- DirichletProcessGaussian(rnorm(20), cpp = FALSE)
  dp <- Fit(dp, 4, FALSE, FALSE, storeSamples = FALSE)

  expect_error(
    PosteriorSummary(dp, seq(-1, 1, length.out = 5)),
    "requires stored ordinary DP samples"
  )
})

test_that("PosteriorSummary fails clearly for invalid level and over-filtered samples", {
  old_cpp_setting <- set_use_cpp(FALSE)
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)

  set.seed(310)
  dp <- DirichletProcessGaussian(rnorm(20), cpp = FALSE)
  dp <- Fit(dp, 6, FALSE, FALSE, thinning = 2)

  expect_error(
    PosteriorSummary(dp, seq(-1, 1, length.out = 5), level = 1),
    "'level' must be"
  )

  expect_error(
    PosteriorSummary(dp, seq(-1, 1, length.out = 5), burnin = 4),
    "at least two retained stored samples"
  )
})

test_that("PosteriorSummary fails clearly for top-level hierarchical objects", {
  set.seed(314)
  beta_dp <- DirichletProcessHierarchicalBeta(list(rbeta(8, 2, 5), rbeta(8, 3, 4)), 1)
  beta_dp <- Fit(beta_dp, 1, progressBar = FALSE)

  expect_error(
    PosteriorSummary(beta_dp, seq(0.1, 0.9, length.out = 5)),
    "not defined for top-level hierarchical HDP objects"
  )

  set.seed(315)
  mvnormal_dp <- DirichletProcessHierarchicalMvnormal2(list(
    mvtnorm::rmvnorm(8, c(0, 0), diag(2)),
    mvtnorm::rmvnorm(8, c(1, -1), diag(2))
  ))
  mvnormal_dp <- Fit(mvnormal_dp, 1, progressBar = FALSE)

  expect_error(
    PosteriorSummary(mvnormal_dp, matrix(c(0, 0), ncol = 2)),
    "not defined for top-level hierarchical HDP objects"
  )
})

test_that("PosteriorSummary works directly on local hierarchical restaurant objects", {
  set.seed(316)
  dp <- DirichletProcessHierarchicalBeta(list(rbeta(10, 2, 5), rbeta(10, 3, 4)), 1)
  dp <- Fit(dp, 3, progressBar = FALSE)

  local_dp <- dp$indDP[[1]]
  original_class <- class(local_dp)
  original_md_class <- class(local_dp$mixingDistribution)
  grid <- seq(0.1, 0.9, length.out = 5)

  expect_true(inherits(local_dp, "hierarchical"))
  expect_true(inherits(local_dp, "dirichletprocess"))

  summary_df <- PosteriorSummary(local_dp, grid)

  expect_s3_class(summary_df, "data.frame")
  expect_equal(names(summary_df), c("x", "Mean", "Median", "Lower", "Upper"))
  expect_equal(summary_df$x, grid)
  expect_equal(nrow(summary_df), length(grid))
  expect_equal(class(local_dp), original_class)
  expect_equal(class(local_dp$mixingDistribution), original_md_class)
})
