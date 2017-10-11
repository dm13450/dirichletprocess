context("Table Update Tests")


dataTest <- list(rbeta(10, 1, 3))
dpobjlistTest <- DirichletProcessHierarchicalBeta(dataTest, 1)


test_that("Table Update Single Cluster", {
  skip("Skip Table Update")
  dataTest <- list(rbeta(100, 1, 3))
  dpobjlistTest <- DirichletProcessHierarchicalBeta(dataTest, 1)

  preCP <- list()
  preCP[[1]] <- array(c(1), dim=c(1,1,1))
  preCP[[2]] <- array(c(10), dim=c(1,1,1))

  dpobjlistTest[[1]]$mixingDistribution$theta_k <- preCP

  preNumCluster <- 1
  preLabels <- rep_len(1, 100)
  prePointsPerCluster <- 100

  dpobjlistTest[[1]]$numberClusters <- preNumCluster
  dpobjlistTest[[1]]$clusterLabels <- preLabels
  dpobjlistTest[[1]]$pointsPerCluster <- prePointsPerCluster
  dpobjlistTest[[1]]$clusterParameters <- preCP


  dpobjlistTest <- TableUpdate(dpobjlistTest)

  expect_length(dpobjlistTest[[1]]$clusterParameters, 2)
  expect_length(dpobjlistTest[[1]]$clusterParameters[[1]], 1)
  expect_length(dpobjlistTest[[1]]$clusterParameters[[2]], 1)
  expect_equal(dpobjlistTest[[1]]$numberClusters, 1)

})


test_that("Table Update 2 Clusters", {
  skip("Table Update")
  dataTest <- list(rbeta(100, 1, 3))
  dpobjlistTest <- DirichletProcessHierarchicalBeta(dataTest, 1)

  preCP <- list()
  preCP[[1]] <- array(c(0.1, 0.1), dim=c(1,1,2))
  preCP[[2]] <- array(c(11, 12), dim=c(1,1,2))

  dpobjlistTest[[1]]$mixingDistribution$theta_k <- preCP

  preNumCluster <- 2
  preLabels <- rep(c(1,2), 50)
  prePointsPerCluster <- c(50, 50)

  dpobjlistTest[[1]]$numberClusters <- preNumCluster
  dpobjlistTest[[1]]$clusterLabels <- preLabels
  dpobjlistTest[[1]]$pointsPerCluster <- prePointsPerCluster
  dpobjlistTest[[1]]$clusterParameters <- preCP


  dpobjlistTest <- TableUpdate(dpobjlistTest)

  expect_length(dpobjlistTest[[1]]$clusterParameters, 2)
  expect_length(dpobjlistTest[[1]]$clusterParameters[[1]], 2)
  expect_length(dpobjlistTest[[1]]$clusterParameters[[2]], 2)
  expect_equal(dpobjlistTest[[1]]$numberClusters, 2)
})


