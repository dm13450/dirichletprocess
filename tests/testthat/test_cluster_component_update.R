context("Cluster Component Update")


test_that("Hierarchical Beta 5 Data", {

  dataTest <- list(rbeta(10, 1, 3), rbeta(10, 1, 3), rbeta(10, 3, 5), rbeta(10, 4, 5), rbeta(10, 6, 3))
  dpobjlistTest <- DirichletProcessHierarchicalBeta(dataTest, 1)

  dpobjlistTest <- ClusterComponentUpdate(dpobjlistTest)

  for(i in seq_along(dpobjlistTest$indDP)){
    expect_true(all(c(dpobjlistTest$indDP[[i]]$clusterParameters[[1]]) %in% c(dpobjlistTest$globalParameters[[1]])))
    expect_true(all(c(dpobjlistTest$indDP[[i]]$clusterParameters[[2]]) %in% c(dpobjlistTest$globalParameters[[2]])))
  }
})

test_that("Hierarchical Mv Normal 5 Data", {
  require(mvtnorm)
  dataTest <- list(rmvnorm(100, c(0,0), diag(2)), rmvnorm(100, c(1,1), diag(2)), rmvnorm(100, c(-1,-1), diag(2)), rmvnorm(100, c(2,2), diag(2)), rmvnorm(100, c(-2,-2), diag(2)))
  dpobjlistTest <- DirichletProcessHierarchicalMvnormal2(dataTest)
  
  dpobjlistTest <- ClusterComponentUpdate(dpobjlistTest)
  
  for(i in seq_along(dpobjlistTest$indDP)){
    expect_true(all(c(dpobjlistTest$indDP[[i]]$clusterParameters[[1]]) %in% c(dpobjlistTest$globalParameters[[1]])))
    expect_true(all(c(dpobjlistTest$indDP[[i]]$clusterParameters[[2]]) %in% c(dpobjlistTest$globalParameters[[2]])))
  }
})