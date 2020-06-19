context("Duplicate Cluster Remove")


test_that("Cluster Remove", {

  testData <- rbeta(10, 2, 4)

  testDP <- DirichletProcessBeta(testData, 1, verbose=FALSE)

  preCP <- list()
  preCP[[1]] <- array(c(1,2,1), dim=c(1,1,3))
  preCP[[2]] <- array(c(10,12,10), dim=c(1,1,3))

  preNumCluster <- 3
  preLabels <- c(1,1,1,1,3,3, 2,2,2,2)
  prePointsPerCluster <- c(4, 4, 2)

  testDP$numberClusters <- preNumCluster
  testDP$clusterLabels <- preLabels
  testDP$pointsPerCluster <- prePointsPerCluster
  testDP$clusterParameters <- preCP

  testDP <- DuplicateClusterRemove(testDP)

  expect_length(testDP$clusterParameters[[1]], 2)
  expect_equal(testDP$numberClusters, 2)
  expect_equal(testDP$clusterLabels, c(1,1,1,1,1,1,2,2,2,2))
  expect_equal(testDP$pointsPerCluster, c(6, 4))
})

test_that("Cluster Remove 2D", {

  #require(mvtnorm)
  testData <- mvtnorm::rmvnorm(10, c(0,0), diag(2))

  testDP <- DirichletProcessMvnormal2(testData)

  preCP <- list()
  preCP[[1]] <- array(c(c(0,0),c(1,1),c(0,0)), dim=c(1,2,3))
  preCP[[2]] <- array(c(diag(2),diag(2),diag(2)), dim=c(2,2,3))

  preNumCluster <- 3
  preLabels <- c(1,1,1,1,3,3, 2,2,2,2)
  prePointsPerCluster <- c(4, 4, 2)

  testDP$numberClusters <- preNumCluster
  testDP$clusterLabels <- preLabels
  testDP$pointsPerCluster <- prePointsPerCluster
  testDP$clusterParameters <- preCP

  testDP <- DuplicateClusterRemove(testDP)

  expect_equal(dim(testDP$clusterParameters[[1]]), c(1,2,2))
  expect_equal(testDP$numberClusters, 2)
  expect_equal(testDP$clusterLabels, c(1,1,1,1,1,1,2,2,2,2))
  expect_equal(testDP$pointsPerCluster, c(6, 4))
})

test_that("No Cluster Change", {

  testData <- rbeta(10, 2, 4)

  testDP <- DirichletProcessBeta(testData, 1, verbose=FALSE)

  preCP <- list()
  preCP[[1]] <- array(c(1,2), dim=c(1,1,2))
  preCP[[2]] <- array(c(10,12), dim=c(1,1,2))

  preNumCluster <- 2
  preLabels <- c(1,1,1,1,2,2, 2,2,2,2)
  prePointsPerCluster <- c(4, 6)

  testDP$numberClusters <- preNumCluster
  testDP$clusterLabels <- preLabels
  testDP$pointsPerCluster <- prePointsPerCluster
  testDP$clusterParameters <- preCP

  testDP <- DuplicateClusterRemove(testDP)

  expect_length(testDP$clusterParameters[[1]], 2)
  expect_equal(testDP$numberClusters, 2)
  expect_equal(testDP$clusterLabels, c(1,1,1,1,2,2, 2,2,2,2))
  expect_equal(testDP$pointsPerCluster, c(4, 6))
})

test_that("3 Cluster Change", {

  testData <- rbeta(10, 2, 4)

  testDP <- DirichletProcessBeta(testData, 1, verbose = FALSE)

  preCP <- list()
  preCP[[1]] <- array(c(1, 4, 4, 4), dim=c(1,1,4))
  preCP[[2]] <- array(c(10, 13, 13, 13 ), dim=c(1,1,4))

  preNumCluster <- 4
  preLabels <- c(1,1,4,4,3,3, 4, 4,2,2)
  prePointsPerCluster <- c(2, 2, 2, 4)

  testDP$numberClusters <- preNumCluster
  testDP$clusterLabels <- preLabels
  testDP$pointsPerCluster <- prePointsPerCluster
  testDP$clusterParameters <- preCP

  testDP <- DuplicateClusterRemove(testDP)

  expect_length(testDP$clusterParameters[[1]], 2)
  expect_equal(testDP$numberClusters, 2)
  expect_equal(testDP$clusterLabels, c(1,1, 2,2, 2,2, 2,2,2,2))
  expect_equal(testDP$pointsPerCluster, c(2, 8))
})

test_that("3 Cluster Change 2D", {

  #require(mvtnorm)
  testData <- mvtnorm::rmvnorm(10, c(0,0), diag(2))

  testDP <- DirichletProcessMvnormal2(testData)

  preCP <- list()
  preCP[[1]] <- array(c(c(0,0),c(1,1),c(1,1), c(1,1)), dim=c(1,2,4))
  preCP[[2]] <- array(c(matrix(c(1.5,0,0,1), nrow=2, ncol=2),diag(2),diag(2),diag(2)), dim=c(2,2,4))

  preNumCluster <- 4
  preLabels <- c(1,1,4,4,3,3, 4, 4,2,2)
  prePointsPerCluster <- c(2, 2, 2, 4)

  testDP$numberClusters <- preNumCluster
  testDP$clusterLabels <- preLabels
  testDP$pointsPerCluster <- prePointsPerCluster
  testDP$clusterParameters <- preCP

  testDP <- DuplicateClusterRemove(testDP)

  expect_equal(dim(testDP$clusterParameters[[1]]), c(1,2,2))
  expect_equal(testDP$numberClusters, 2)
  expect_equal(testDP$clusterLabels, c(1,1, 2,2, 2,2, 2,2,2,2))
  expect_equal(testDP$pointsPerCluster, c(2, 8))
})

test_that("Rebase 1", {

  testData <- rbeta(4, 2, 4)

  testDP <- DirichletProcessBeta(testData, 1, verbose=FALSE)

  preCP <- list()
  preCP[[1]] <- array(c(1, 1, 4), dim=c(1,1,4))
  preCP[[2]] <- array(c(13, 13, 16 ), dim=c(1,1,4))

  preNumCluster <- 3
  preLabels <- c(1,1,2,3)
  prePointsPerCluster <- c(2, 1, 1)

  testDP$numberClusters <- preNumCluster
  testDP$clusterLabels <- preLabels
  testDP$pointsPerCluster <- prePointsPerCluster
  testDP$clusterParameters <- preCP

  testDP <- DuplicateClusterRemove(testDP)

  expect_length(testDP$clusterParameters[[1]], 2)
  expect_equal(testDP$numberClusters, 2)
  expect_equal(testDP$clusterLabels, c(1,1,1,2))
  expect_equal(testDP$pointsPerCluster, c(3, 1))

})

test_that("Duplicate Cluster 2", {

  testData <- rbeta(6, 2, 4)

  testDP <- DirichletProcessBeta(testData, 1, verbose=FALSE)

  preCP <- list()
  preCP[[1]] <- array(c(1.5, 1.5, 2.5, 3.5, 3.5, 2.5), dim=c(1,1,6))
  preCP[[2]] <- array(c(11, 11, 12, 13, 13, 12), dim=c(1,1,6))

  preNumCluster <- 6
  preLabels <- c(1, 2, 3, 4, 5, 6)
  prePointsPerCluster <- c(1, 1, 1, 1, 1, 1)


  testDP$numberClusters <- preNumCluster
  testDP$clusterLabels <- preLabels
  testDP$pointsPerCluster <- prePointsPerCluster
  testDP$clusterParameters <- preCP

  testDP <- DuplicateClusterRemove(testDP)

  expect_length(testDP$clusterParameters[[1]], 3)
  expect_equal(testDP$numberClusters, 3)
  expect_equal(testDP$clusterLabels, c(1,1,2,3,3,2))
  expect_equal(testDP$pointsPerCluster, c(2, 2, 2))


})

