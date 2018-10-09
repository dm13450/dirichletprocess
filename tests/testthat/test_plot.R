context("Plot")


graphTest <- function(dpobj){
  graph <- plot(dpobj)
  graph2 <- plot(dpobj, likelihood=TRUE)
  graph3 <- plot(dpobj, likelihood=TRUE, single=TRUE)
  return(list(graph, graph2, graph3))
}

test_that("Normal Plotting", {

  dp <- DirichletProcessGaussian(rnorm(10))

  graphs <- graphTest(dp)

  for(i in seq_along(graphs)){
    expect_is(graphs[[i]], c("gg", "ggplot"))
  }


})

test_that("Exp Plotting", {

  dp <- DirichletProcessExponential(rexp(10))

  graphs <- graphTest(dp)

  for(i in seq_along(graphs)){
    expect_is(graphs[[i]], c("gg", "ggplot"))
  }

})

test_that("Beta Plotting", {

  dp <- DirichletProcessBeta(rbeta(10, 2, 3), 1)

  graphs <- graphTest(dp)

  for(i in seq_along(graphs)){
    expect_is(graphs[[i]], c("gg", "ggplot"))
  }

})

test_that("Weibull Plotting", {

  dp <- DirichletProcessWeibull(rweibull(10, 2, 3), c(10, 2, 4))

  graphs <- graphTest(dp)

  for(i in seq_along(graphs)){
    expect_is(graphs[[i]], c("gg", "ggplot"))
  }

})

test_that("MvNormal Plotting", {
  testData <- matrix(c(rnorm(10), rnorm(10)), ncol=2)
  dp <- DirichletProcessMvnormal(testData)

  graphs <- graphTest(dp)

  for(i in seq_along(graphs)){
    expect_is(graphs[[i]], c("gg", "ggplot"))
  }

})






test_that("Plotting options", {

  dp <- DirichletProcessGaussian(c(rnorm(50, 2, .2), rnorm(60)))
  dp <- Fit(dp, 10)

  # Check options
  graphs <- list(
    plot(dp),
    plot(dp, data_fill = "grey"),
    plot(dp, data_fill = "grey", data_bw = .1),
    plot(dp, data_fill = "grey", data_method = "hist", data_bw = .2),
    plot(dp, data_fill = "grey", data_method = "hist", likelihood = FALSE),
    plot(dp, data_fill = "grey", data_method = "hist", likelihood = TRUE),
    plot(dp, data_fill = "grey", data_method = "hist", single = FALSE),
    plot(dp, data_fill = "grey", data_method = "hist", single = FALSE, likelihood = TRUE),
    plot(dp, data_fill = "grey", data_method = "hist", single = TRUE),
    plot(dp, xgrid_pts = 4, data_fill = "grey80"),
    plot(dp, xgrid_pts = 1000, data_bw = .2, data_fill = "grey80"),
    plot(dp, quant_pts = 4, data_fill = "grey80"),
    plot(dp, quant_pts = 1000, data_bw = .2, data_fill = "grey80")
  )

  for(i in seq_along(graphs)){
    expect_is(graphs[[i]], c("gg", "ggplot"))
  }


})

