context("Change Observations")

num_test_points = 10

expect_hierarchical_change_observations_state <- function(dpList, newData) {
  expect_equal(length(dpList$indDP), length(newData))
  expect_equal(length(dpList$tableDishLabels), length(dpList$indDP))

  totalTables <- 0

  for (i in seq_along(dpList$indDP)) {
    localDp <- dpList$indDP[[i]]
    expectedN <- if (is.matrix(newData[[i]])) nrow(newData[[i]]) else length(newData[[i]])
    localDishes <- dpList$tableDishLabels[[i]]

    if (is.matrix(newData[[i]])) {
      expect_equal(localDp$data, newData[[i]])
    } else {
      expect_equal(c(localDp$data), c(newData[[i]]))
    }
    expect_equal(localDp$n, expectedN)
    expect_equal(length(localDp$clusterLabels), expectedN)
    expect_equal(sum(localDp$pointsPerCluster), expectedN)
    expect_equal(length(localDp$pointsPerCluster), localDp$numberClusters)
    expect_equal(length(localDishes), localDp$numberClusters)

    if (length(localDishes) > 0) {
      expect_true(all(localDishes %in% seq_len(length(dpList$dishTableCounts))))
    }

    for (componentIndex in seq_along(localDp$clusterParameters)) {
      expect_equal(localDp$clusterParameters[[componentIndex]],
                   dpList$globalParameters[[componentIndex]][, , localDishes, drop = FALSE])
    }

    totalTables <- totalTables + localDp$numberClusters
  }

  expect_equal(sum(dpList$dishTableCounts), totalTables)

  for (componentIndex in seq_along(dpList$globalParameters)) {
    expect_equal(dim(dpList$globalParameters[[componentIndex]])[3],
                 length(dpList$dishTableCounts))
  }
}

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
  set.seed(101)

  testData <- list(rbeta(10, 2,3), rbeta(10, 5, 4))
  dpList <- DirichletProcessHierarchicalBeta(testData, 1)
  dpList <- Fit(dpList, 5, FALSE, FALSE)
  newData <- list(rbeta(10, 2,3), rbeta(20, 5, 4))
  dpListNew <- ChangeObservations(dpList, newData)

  expect_equal(newData[[1]], c(dpListNew$indDP[[1]]$data))
  expect_equal(newData[[2]], c(dpListNew$indDP[[2]]$data))
  expect_equal(sum(dpListNew$indDP[[1]]$pointsPerCluster), 10)
  expect_equal(sum(dpListNew$indDP[[2]]$pointsPerCluster), 20)
  expect_hierarchical_change_observations_state(dpListNew, newData)

  dpListNew <- Fit(dpListNew, 2, FALSE, FALSE)

  expect_is(dpListNew, class(dpList))

})

test_that("Hierarchical local DP ChangeObservations uses local replacement logic", {
  set.seed(102)

  testData <- list(rbeta(8, 2, 3), rbeta(9, 5, 4))
  dpList <- DirichletProcessHierarchicalBeta(testData, 1)
  replacement <- rbeta(12, 2, 3)

  localDp <- ChangeObservations(dpList$indDP[[1]], replacement)

  expect_equal(c(localDp$data), replacement)
  expect_equal(localDp$n, length(replacement))
  expect_equal(length(localDp$clusterLabels), length(replacement))
  expect_equal(sum(localDp$pointsPerCluster), length(replacement))
})

test_that("Hierarchical Beta ChangeObservations keeps uneven groups coherent", {
  set.seed(103)

  testData <- list(rbeta(6, 2, 3), rbeta(8, 5, 4), rbeta(5, 4, 8))
  dpList <- DirichletProcessHierarchicalBeta(testData, 1)
  dpList <- Fit(dpList, 3, FALSE, FALSE)
  newData <- list(rbeta(7, 2, 3), rbeta(13, 5, 4), rbeta(4, 4, 8))

  dpListNew <- ChangeObservations(dpList, newData)

  expect_equal(vapply(dpListNew$indDP,
                      function(localDp) sum(localDp$pointsPerCluster),
                      numeric(1)),
               c(7, 13, 4))
  expect_hierarchical_change_observations_state(dpListNew, newData)
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


