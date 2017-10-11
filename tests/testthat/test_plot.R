context("Plot")

num_test_points <- 10
data_test <- rnorm(num_test_points)
priorParameters_test <- matrix(c(1,1,1,1), ncol=4)

normal_object_test <- MixingDistribution("normal", priorParameters_test, "conjugate")
dpobj <- DirichletProcessCreate(data_test, normal_object_test)
dpobj <- Initialise(dpobj)

test_that("Basic Plot", {

  graph <- plot(dpobj)

  expect_is(graph, c("gg", "ggplot"))
})

test_that("Basic Plot with Posterior", {

  graph <- plot(dpobj, posterior=TRUE)
  expect_is(graph, c("gg", "ggplot"))
})
