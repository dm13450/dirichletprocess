context("Posterior Object Classes")

with_pdf_device <- function(expr) {
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit({
    grDevices::dev.off()
    unlink(path)
  }, add = TRUE)

  force(expr)
}

make_small_fitted_gaussian_dp <- function() {
  old_cpp_setting <- set_use_cpp(FALSE)
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)

  set.seed(3301)
  dp <- DirichletProcessGaussian(rnorm(12), cpp = FALSE)
  Fit(dp, 4, FALSE, FALSE)
}

test_that("PosteriorClusters has an S3 class while preserving list access", {
  dp <- make_small_fitted_gaussian_dp()
  clusters <- PosteriorClusters(dp)

  expect_s3_class(clusters, "dirichletprocess_posterior_clusters")
  expect_true(inherits(clusters, "list"))
  expect_true(is.list(clusters))
  expect_equal(names(clusters), c("weights", "params"))
  expect_true(is.numeric(clusters$weights))
  expect_true(is.list(clusters$params))
  expect_equal(attr(clusters, "source_function"), "PosteriorClusters")
  expect_equal(attr(clusters, "kernel"), "normal")
  expect_equal(attr(clusters, "conjugacy"), "conjugate")

  expect_output(print(clusters), "dirichletprocess_posterior_clusters")

  clusters_summary <- summary(clusters)
  expect_s3_class(clusters_summary, "data.frame")
  expect_equal(clusters_summary$object, "posterior_clusters")
  expect_equal(clusters_summary$atoms, length(clusters$weights))

  expect_error(with_pdf_device(plot(clusters)), NA)
})

test_that("PosteriorSummary has an S3 class while preserving data-frame access", {
  dp <- make_small_fitted_gaussian_dp()
  grid <- seq(-2, 2, length.out = 6)

  set.seed(3302)
  summary_df <- PosteriorSummary(dp, grid, level = 0.9)

  expect_s3_class(summary_df, "dirichletprocess_posterior_summary")
  expect_s3_class(summary_df, "data.frame")
  expect_true(is.data.frame(summary_df))
  expect_equal(names(summary_df), c("x", "Mean", "Median", "Lower", "Upper"))
  expect_equal(summary_df$x, grid)
  expect_true(is.numeric(summary_df$Mean))
  expect_true(is.numeric(summary_df$Lower))
  expect_true(is.numeric(summary_df$Upper))
  expect_equal(attr(summary_df, "source_function"), "PosteriorSummary")
  expect_equal(attr(summary_df, "kernel"), "normal")
  expect_equal(attr(summary_df, "level"), 0.9)
  expect_equal(attr(summary_df, "retained_samples"), 4L)

  expect_output(print(summary_df), "dirichletprocess_posterior_summary")
  expect_error(summary(summary_df), NA)
  expect_error(with_pdf_device(plot(summary_df)), NA)
})

test_that("PosteriorFrame inherits posterior summary methods without changing columns", {
  dp <- make_small_fitted_gaussian_dp()
  grid <- seq(-2, 2, length.out = 5)

  frame <- PosteriorFrame(dp, grid, ndraws = 5, ci_size = 0.2)

  expect_s3_class(frame, "dirichletprocess_posterior_frame")
  expect_s3_class(frame, "dirichletprocess_posterior_summary")
  expect_s3_class(frame, "data.frame")
  expect_equal(names(frame), c("Mean", "X10.", "X90.", "x"))
  expect_equal(frame$x, grid)
  expect_true(is.numeric(frame$Mean))
  expect_equal(attr(frame, "source_function"), "PosteriorFrame")
  expect_equal(attr(frame, "ndraws"), 5)
  expect_equal(attr(frame, "ci_size"), 0.2)

  expect_output(print(frame), "dirichletprocess_posterior_summary")
  expect_error(summary(frame), NA)
  expect_error(with_pdf_device(plot(frame)), NA)
})

test_that("PriorClusters has an S3 class while preserving list access", {
  dp <- DirichletProcessGaussian(rnorm(8), cpp = FALSE)
  clusters <- PriorClusters(dp)

  expect_s3_class(clusters, "dirichletprocess_prior_clusters")
  expect_true(inherits(clusters, "list"))
  expect_true(is.list(clusters))
  expect_equal(names(clusters), c("weights", "params"))
  expect_true(is.numeric(clusters$weights))
  expect_true(is.list(clusters$params))
  expect_equal(attr(clusters, "source_function"), "PriorClusters")
  expect_equal(attr(clusters, "kernel"), "normal")

  expect_output(print(clusters), "dirichletprocess_prior_clusters")
  expect_s3_class(summary(clusters), "data.frame")
  expect_error(with_pdf_device(plot(clusters)), NA)
})

test_that("posterior, prior, and likelihood function helpers remain callable functions", {
  dp <- make_small_fitted_gaussian_dp()
  grid <- seq(-1, 1, length.out = 3)

  posterior_fun <- PosteriorFunction(dp)
  prior_fun <- getFromNamespace("PriorFunction", "dirichletprocess")(dp)
  likelihood_fun <- LikelihoodFunction(dp)

  expect_type(posterior_fun, "closure")
  expect_type(prior_fun, "closure")
  expect_type(likelihood_fun, "closure")
  expect_true(is.numeric(posterior_fun(grid)))
  expect_true(is.numeric(prior_fun(grid)))
  expect_true(is.numeric(likelihood_fun(grid)))
})
