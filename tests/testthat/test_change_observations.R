context("Change Observations")

num_test_points = 10

test_that("Conjugate Change Observations", {

  data_test = rnorm(num_test_points)
  dpobj <- DirichletProcessGaussian(data_test)
  dpobj <- Fit(dpobj, 10, FALSE, FALSE)

  new_data_test <- rnorm(20)
  dpobj <- ChangeObservations(dpobj, new_data_test)

  expect_equal(dpobj$data, matrix(new_data_test, ncol=1))
  expect_equal(dpobj$n, length(new_data_test))

  expect_equal(sum(dpobj$pointsPerCluster), length(new_data_test))

})

test_that("Non Conjugate Change Observations", {

  data_test <- rweibull(num_test_points, 1, 1)
  dpobj <- DirichletProcessWeibull(data_test, c(10, 2, 4))
  dpobj <- Fit(dpobj, 10, FALSE, FALSE)

  new_data_test <- rweibull(20, 1, 1)
  dpobj <- ChangeObservations(dpobj, new_data_test)

  expect_equal(dpobj$data, matrix(new_data_test, ncol=1))
  expect_equal(dpobj$n, length(new_data_test))
  expect_equal(sum(dpobj$pointsPerCluster), length(new_data_test))

})

test_that("Hierarchcial Change Observations", {

  testData <- list(rbeta(10, 2,3), rbeta(10, 5, 4))
  dpList <- DirichletProcessHierarchicalBeta(testData, 1)
  dpList <- Fit(dpList, 5, FALSE, FALSE)
  newData <- list(rbeta(10, 2,3), rbeta(20, 5, 4))
  dpListNew <- ChangeObservations(dpList, newData)

  expect_equal(newData[[1]], c(dpListNew$indDP[[1]]$data))
  expect_equal(newData[[2]], c(dpListNew$indDP[[2]]$data))
  expect_equal(sum(dpListNew$indDP[[1]]$pointsPerCluster), 10)
  expect_equal(sum(dpListNew$indDP[[2]]$pointsPerCluster), 20)

  dpListNew <- Fit(dpListNew, 2, FALSE, FALSE)

  expect_is(dpListNew, class(dpList))

})

test_that("2D Change Observations", {

  testData <- scale(faithful)

  dp <- DirichletProcessMvnormal(testData)
  dp <- Fit(dp, 3, progressBar = F)
  dpNew <- ChangeObservations(dp, abs(testData))
  dpNew <- Fit(dpNew, 4, progressBar = F)

  expect_is(dpNew, class(dp))
  expect_equal(abs(testData), dpNew$data)

})



