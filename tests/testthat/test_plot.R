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






