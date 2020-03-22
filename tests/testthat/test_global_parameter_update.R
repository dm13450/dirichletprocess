context("Global Parameter Update Tests")

test_that("1 Data; 1 Cluster", {
  dataTest <- list(rbeta(100, 1, 3))
  dpobjlistTest <- DirichletProcessHierarchicalBeta(dataTest, 1)

  preCP <- list()
  preCP[[1]] <- array(c(1), dim=c(1,1,1))
  preCP[[2]] <- array(c(10), dim=c(1,1,1))

  dpobjlistTest$globalParameters <- preCP

  preNumCluster <- 1
  preLabels <- rep_len(1, 100)
  prePointsPerCluster <- 100

  dpobjlistTest$indDP[[1]]$numberClusters <- preNumCluster
  dpobjlistTest$indDP[[1]]$clusterLabels <- preLabels
  dpobjlistTest$indDP[[1]]$pointsPerCluster <- prePointsPerCluster
  dpobjlistTest$indDP[[1]]$clusterParameters <- preCP

  dpobjlistTest <- GlobalParameterUpdate(dpobjlistTest)

  expect_length(dpobjlistTest$indDP[[1]]$clusterParameters[[1]], 1)
  expect_length(dpobjlistTest$indDP[[1]]$clusterParameters[[2]], 1)

  expect_equal(dpobjlistTest$indDP[[1]]$clusterParameters[[1]][,,1], dpobjlistTest$globalParameters[[1]][,,1])
  expect_equal(dpobjlistTest$indDP[[1]]$clusterParameters[[2]][,,1], dpobjlistTest$globalParameters[[2]][,,1])

})


test_that("2 Data; 1 Cluster", {
  dataTest <- list(rbeta(100, 1, 3), rbeta(100, 1, 3))
  dpobjlistTest <- DirichletProcessHierarchicalBeta(dataTest, 1)

  preCP <- list()
  preCP[[1]] <- array(c(1), dim=c(1,1,1))
  preCP[[2]] <- array(c(10), dim=c(1,1,1))

  preNumCluster <- 1
  preLabels <- rep_len(1, 100)
  prePointsPerCluster <- 100

  dpobjlistTest$globalParameters <- preCP

  for(i in seq_along(dpobjlistTest$indDP)){
    dpobjlistTest$indDP[[i]]$numberClusters <- preNumCluster
    dpobjlistTest$indDP[[i]]$clusterLabels <- preLabels
    dpobjlistTest$indDP[[i]]$pointsPerCluster <- prePointsPerCluster
    dpobjlistTest$indDP[[i]]$clusterParameters <- preCP
  }

  dpobjlistTest <- GlobalParameterUpdate(dpobjlistTest)

  expect_length(dpobjlistTest$indDP[[1]]$clusterParameters[[1]], 1)
  expect_length(dpobjlistTest$indDP[[1]]$clusterParameters[[2]], 1)

  expect_equal(dpobjlistTest$indDP[[1]]$clusterParameters[[1]][,,1], dpobjlistTest$globalParameters[[1]][,,1])
  expect_equal(dpobjlistTest$indDP[[1]]$clusterParameters[[2]][,,1], dpobjlistTest$globalParameters[[2]][,,1])
  expect_equal(dpobjlistTest$indDP[[2]]$clusterParameters[[1]][,,1], dpobjlistTest$globalParameters[[1]][,,1])
  expect_equal(dpobjlistTest$indDP[[2]]$clusterParameters[[2]][,,1], dpobjlistTest$globalParameters[[2]][,,1])
  expect_equal(dpobjlistTest$indDP[[1]]$clusterParameters[[1]][,,1], dpobjlistTest$indDP[[2]]$clusterParameters[[1]][,,1])
})

test_that("2 Data; 1 Cluster, 2D", {
  require(mvtnorm)
  dataTest <- list(rmvnorm(100, c(0,0), diag(2)), rmvnorm(100, c(0,0), diag(2)))
  dpobjlistTest <- DirichletProcessHierarchicalMvnormal2(dataTest)
  
  preCP <- list()
  preCP[[1]] <- array(c(c(0,0)), dim=c(1,2,1))
  preCP[[2]] <- array(c(diag(2)), dim=c(2,2,1))
  
  preNumCluster <- 1
  preLabels <- rep_len(1, 100)
  prePointsPerCluster <- 100
  
  dpobjlistTest$globalParameters <- preCP
  
  for(i in seq_along(dpobjlistTest$indDP)){
    dpobjlistTest$indDP[[i]]$numberClusters <- preNumCluster
    dpobjlistTest$indDP[[i]]$clusterLabels <- preLabels
    dpobjlistTest$indDP[[i]]$pointsPerCluster <- prePointsPerCluster
    dpobjlistTest$indDP[[i]]$clusterParameters <- preCP
  }
  
  dpobjlistTest <- GlobalParameterUpdate(dpobjlistTest)
  
  expect_equal(dim(dpobjlistTest$indDP[[1]]$clusterParameters[[1]]), c(1,2,1))
  expect_equal(dim(dpobjlistTest$indDP[[1]]$clusterParameters[[2]]), c(2,2,1))
  
  expect_equal(dpobjlistTest$indDP[[1]]$clusterParameters[[1]][,,1], dpobjlistTest$globalParameters[[1]][,,1])
  expect_equal(dpobjlistTest$indDP[[1]]$clusterParameters[[2]][,,1], dpobjlistTest$globalParameters[[2]][,,1])
  expect_equal(dpobjlistTest$indDP[[2]]$clusterParameters[[1]][,,1], dpobjlistTest$globalParameters[[1]][,,1])
  expect_equal(dpobjlistTest$indDP[[2]]$clusterParameters[[2]][,,1], dpobjlistTest$globalParameters[[2]][,,1])
  expect_equal(dpobjlistTest$indDP[[1]]$clusterParameters[[1]][,,1], dpobjlistTest$indDP[[2]]$clusterParameters[[1]][,,1])
})

test_that("5 Data",{
  dataTest <- list(rbeta(10, 1, 3), rbeta(10, 1, 3), rbeta(10, 3, 5), rbeta(10, 4, 5), rbeta(10, 6, 3))
  dpobjlistTest <- DirichletProcessHierarchicalBeta(dataTest, 1)

  dpobjlistTest <- GlobalParameterUpdate(dpobjlistTest)

})

