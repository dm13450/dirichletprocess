context("Weibull Uniform Gamma Tests")

# Access function from namespace if not available in global environment
if (!exists("MhParameterProposal")) {
  MhParameterProposal <- get("MhParameterProposal", getNamespace("dirichletprocess"))
}

test_mdobj <- WeibullMixtureCreate(c(1,1,1), c(1,1))

test_that("Weibull Creation", {

  expect_is(test_mdobj, c("list", "weibull", "nonconjugate"))

})

test_that("Weibull Likelihood", {

  weibull_test_params_1 <- list(array(1, dim=c(1,1,1)), array(1, dim=c(1,1,1)))
  expect_equal(Likelihood(test_mdobj, 0, weibull_test_params_1), dweibull(0, 1, 1))

  weibull_test_params_2 <- list(array(c(1,2), dim=c(1,1,2)), array(c(1,2), dim=c(1,1,2)))
  expect_equal(Likelihood(test_mdobj, 0, weibull_test_params_2), dweibull(0, c(1,2), c(1,2)))

  expect_equal(Likelihood(test_mdobj, c(0, 0), weibull_test_params_2),
               dweibull(c(0,0), c(1,2), c(1,2)))
})

test_that("Weibull Likelihood: Negative x", {

  weibull_test_params_1 <- list(array(1, dim=c(1,1,1)), array(1, dim=c(1,1,1)))
  weibull_test_params_2 <- list(array(.1, dim=c(1,1,1)), array(.1, dim=c(1,1,1)))
  expect_warning(
    testEval <- sapply(seq(-10, -0.1, by=0.1), function(x) Likelihood(test_mdobj, x, weibull_test_params_1)),
    NA
  )
  expect_warning(
    testEval2 <- sapply(seq(-10, -0.1, by=0.1), function(x) Likelihood(test_mdobj, x, weibull_test_params_2)),
    NA
  )

  expect_true(all(testEval==0))
  expect_true(all(testEval2==0))
})

test_that("Weibull Likelihood rejects invalid parameters without warnings", {
  invalid_alpha <- list(array(-0.5, dim = c(1, 1, 1)), array(1, dim = c(1, 1, 1)))
  invalid_lambda <- list(array(1, dim = c(1, 1, 1)), array(-1, dim = c(1, 1, 1)))
  x <- c(0.5, 1, 2)

  expect_warning(lik_alpha <- Likelihood(test_mdobj, x, invalid_alpha), NA)
  expect_warning(lik_lambda <- Likelihood(test_mdobj, x, invalid_lambda), NA)
  expect_warning(prior_alpha <- PriorDensity(test_mdobj, invalid_alpha), NA)
  expect_warning(prior_lambda <- PriorDensity(test_mdobj, invalid_lambda), NA)

  expect_equal(lik_alpha, rep(0, length(x)))
  expect_equal(lik_lambda, rep(0, length(x)))
  expect_equal(as.numeric(prior_alpha), 0)
  expect_equal(as.numeric(prior_lambda), 0)
  expect_equal(dim(prior_alpha), c(1, 1, 1))
  expect_equal(dim(prior_lambda), c(1, 1, 1))
})


test_that("Weibull Prior Density", {
  
  # Create proper parameter format (list with 3D arrays)
  theta <- list(
    array(1, dim = c(1, 1, 1)),
    array(1, dim = c(1, 1, 1))
  )

  expect_equal(as.numeric(PriorDensity(test_mdobj, theta)),
               as.numeric(dunif(1, 0, 1) * dgamma(1, 1, 1)))
  expect_equal(dim(PriorDensity(test_mdobj, theta)), c(1, 1, 1))

})

test_that("Weibull Prior Draw", {

  expect_is(PriorDraw(test_mdobj, 1), "list")
  expect_equal(dim(PriorDraw(test_mdobj, 10)[[1]]), c(1,1,10))
  expect_equal(dim(PriorDraw(test_mdobj, 10)[[2]]), c(1,1,10))

})

test_that("Weibull Posterior Draw", {

  expect_is(PosteriorDraw(test_mdobj, rweibull(10, 1,1), 1), "list")

  expect_equal(dim(PosteriorDraw(test_mdobj, rweibull(10, 1,1), 1)[[1]]), c(1, 1,1))
  expect_equal(dim(PosteriorDraw(test_mdobj, rweibull(10, 1,1), 1)[[2]]), c(1, 1,1))
  expect_equal(dim(PosteriorDraw(test_mdobj, rweibull(10, 1,1), 10)[[1]]), c(1, 1,10))
  expect_equal(dim(PosteriorDraw(test_mdobj, rweibull(10, 1,1), 10)[[2]]), c(1, 1,10))

})

test_that("Prior Parameter Update", {

  test_clusterParameters <- list(array(c(runif(10)), dim=c(1,1,10)), array(c(rgamma(10, 1,1)), dim=c(1,1,10)))

  test_mdobj <- PriorParametersUpdate(test_mdobj, test_clusterParameters)

  expect_equal(dim(test_mdobj$priorParameters), c(1,3))

})

test_that("Cluster Parameter Update", {

  test_data <- c(rlnorm(100, 0.4,0.25), rlnorm(100, 1.4, 0.6))

  dpobj <- DirichletProcessWeibull(test_data, c(10, 2, 0.01))
  dpobj <- ClusterComponentUpdate(dpobj)
  dpobj <- ClusterParameterUpdate(dpobj)

  expect_is(dpobj, c("list", "dirichletprocess", "weibull", "nonconjugate"))

})

test_that("Weibull Posterior helpers handle fitted theta shapes", {

  set.seed(1)
  dpobj <- DirichletProcessWeibull(rweibull(20, 1.5, 2), c(10, 2, 0.01), verbose = FALSE)
  dpobj <- Fit(dpobj, 5, FALSE, FALSE)

  x_grid <- seq(0.2, 3, length.out = 5)
  post_func <- PosteriorFunction(dpobj)
  post_eval <- post_func(x_grid)
  post_frame <- PosteriorFrame(dpobj, x_grid, ndraws = 3)

  expect_is(post_func, "function")
  expect_type(post_eval, "double")
  expect_length(post_eval, length(x_grid))
  expect_true(all(is.finite(post_eval)))
  expect_s3_class(post_frame, "data.frame")
  expect_equal(nrow(post_frame), length(x_grid))
})

test_that("Weibull constructor fit summary and plot run without NaNs warnings", {
  old_cpp_setting <- set_use_cpp(FALSE)
  on.exit(set_use_cpp(old_cpp_setting), add = TRUE)

  set.seed(2)
  expect_warning({
    dpobj <- DirichletProcessWeibull(rweibull(20, 1.5, 2), c(10, 2, 0.01), verbose = FALSE)
    dpobj <- Fit(dpobj, 5, FALSE, FALSE)
    post_summary <- PosteriorSummary(dpobj, seq(0.2, 3, length.out = 5))
    graph <- plot(dpobj, data_method = "none", xlim = c(0, 3), xgrid_pts = 11)
  }, NA)

  expect_s3_class(post_summary, "data.frame")
  expect_equal(nrow(post_summary), 5)
  expect_is(graph, c("gg", "ggplot"))
})

test_that("Parameter Proposal", {

  old_param <- PriorDraw(test_mdobj, 1)
  new_param <- MhParameterProposal(test_mdobj, old_param)
})
