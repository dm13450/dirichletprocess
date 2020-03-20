context("Update G0")


test_that("2 Data, 1 Cluster", {

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

  dpobjlistTest <- UpdateG0(dpobjlistTest)

  for(i in seq_along(dpobjlistTest$indDP)){
    expect_true(c(dpobjlistTest$indDP[[i]]$clusterParameters[[1]]) %in% c(dpobjlistTest$globalParameters[[1]]))
  }

})

test_that("2 Data, 1 Cluster, 2D", {
  require(mvtnorm)
  dataTest <- list(rmvnorm(100, c(0,0), diag(2)), rmvnorm(100, c(1,1), diag(2)))
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
  
  dpobjlistTest <- UpdateG0(dpobjlistTest)
  
  for(i in seq_along(dpobjlistTest$indDP)){
    expect_equal(dpobjlistTest$indDP[[i]]$clusterParameters[[1]][,,1], dpobjlistTest$globalParameters[[1]][,,1])
  }
  
})

test_that("5 Data Cluster Component then G0", {

  dataTest <- list(rbeta(10, 2, 3), rbeta(10, 1, 3), rbeta(10, 5, 3), rbeta(10, 6, 2), rbeta(10, 9, 4))
  dpobjlistTest <- DirichletProcessHierarchicalBeta(dataTest, 1)

  dpobjlistTest <- ClusterComponentUpdate(dpobjlistTest)
  dpobjlistTest <- UpdateG0(dpobjlistTest)

  for(i in seq_along(dpobjlistTest$indDP)){
    expect_true(all(c(dpobjlistTest$indDP[[i]]$clusterParameters[[1]]) %in% c(dpobjlistTest$globalParameters[[1]])))
  }
})

test_that("5 Data Cluster Component then G0, 2D", {
  
  dataTest <- list(rmvnorm(100, c(0,0), diag(2)), rmvnorm(100, c(1,1), diag(2)), rmvnorm(100, c(-1,-1), diag(2)), rmvnorm(100, c(2,2), diag(2)), rmvnorm(100, c(-2,-2), diag(2)))
  dpobjlistTest <- DirichletProcessHierarchicalMvnormal2(dataTest)
  
  dpobjlistTest <- ClusterComponentUpdate(dpobjlistTest)
  dpobjlistTest <- UpdateG0(dpobjlistTest)
  
  for(i in seq_along(dpobjlistTest$indDP)){
    expect_true(all(c(dpobjlistTest$indDP[[i]]$clusterParameters[[1]]) %in% c(dpobjlistTest$globalParameters[[1]])))
  }
})

test_that("5 Data Cluster Component, Global Param then G0", {

  dataTest <- list(rbeta(10, 2, 3), rbeta(10, 1, 3), rbeta(10, 5, 3), rbeta(10, 6, 2), rbeta(10, 9, 4))
  dpobjlistTest <- DirichletProcessHierarchicalBeta(dataTest, 1)

  dpobjlistTest <- ClusterComponentUpdate(dpobjlistTest)
  dpobjlistTest <- GlobalParameterUpdate(dpobjlistTest)
  dpobjlistTest <- UpdateG0(dpobjlistTest)

  for(i in seq_along(dpobjlistTest$indDP)){
    expect_true(all(c(dpobjlistTest$indDP[[i]]$clusterParameters[[1]]) %in% c(dpobjlistTest$globalParameters[[1]])))
  }
})

test_that("5 Data Cluster Component, Global Param then G0, 2D", {
  require(mvtnorm)
  dataTest <- list(rmvnorm(100, c(0,0), diag(2)), rmvnorm(100, c(1,1), diag(2)), rmvnorm(100, c(-1,-1), diag(2)), rmvnorm(100, c(2,2), diag(2)), rmvnorm(100, c(-2,-2), diag(2)))
  dpobjlistTest <- DirichletProcessHierarchicalMvnormal2(dataTest)
  
  dpobjlistTest <- ClusterComponentUpdate(dpobjlistTest)
  dpobjlistTest <- GlobalParameterUpdate(dpobjlistTest)
  dpobjlistTest <- UpdateG0(dpobjlistTest)
  
  for(i in seq_along(dpobjlistTest$indDP)){
    expect_true(all(c(dpobjlistTest$indDP[[i]]$clusterParameters[[1]]) %in% c(dpobjlistTest$globalParameters[[1]])))
  }
})

