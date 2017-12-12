context("Cluster Label Change")

test_that("Cluster Label Change Conjugate", {

  num_test_points = 10
  data_test = rnorm(num_test_points)
  priorParameters_test = matrix(c(1,1,1,1), ncol=4)

  normal_object_test = MixingDistribution("normal", priorParameters_test, "conjugate")
  dpobj = DirichletProcessCreate(data_test, normal_object_test)
  dpobj = Initialise(dpobj, verbose=FALSE)

  clusterLabels <- dpobj$clusterLabels
  pointsPerCluster <- dpobj$pointsPerCluster

  currentLabel <- clusterLabels[1]
  pointsPerCluster[currentLabel] <- pointsPerCluster[currentLabel] - 1

  dpobj$pointsPerCluster <- pointsPerCluster

  dpobj = ClusterLabelChange(dpobj, 1, 2, currentLabel)

  expect_equal(dpobj$numberClusters, 2)

})

test_that("Cluster Label Change Nonconjugate", {

  num_test_points = 10
  data_test = rweibull(num_test_points, 1,1)
  priorParameters_test = matrix(c(1,1,1), ncol=3)

  weibull_object_test <- WeibullMixtureCreate(priorParameters_test, 1)
  dpobj = DirichletProcessCreate(data_test, weibull_object_test)
  dpobj = Initialise(dpobj, verbose=FALSE)

  clusterLabels <- dpobj$clusterLabels
  pointsPerCluster <- dpobj$pointsPerCluster

  currentLabel <- clusterLabels[1]
  pointsPerCluster[currentLabel] <- pointsPerCluster[currentLabel] - 1

  dpobj$pointsPerCluster <- pointsPerCluster

  aux <- PriorDraw(weibull_object_test, 3)

  dpobj = ClusterLabelChange(dpobj, 1, 2, currentLabel, aux)

  expect_equal(dpobj$numberClusters, 2)
  expect_equal(sum(dpobj$pointsPerCluster), num_test_points)
})

test_that("Cluster Label Change Nonconjugate 2", {

  num_test_points = 10
  data_test = rweibull(num_test_points, 1,1)
  priorParameters_test = matrix(c(1,1,1), ncol=3)

  weibull_object_test <- WeibullMixtureCreate(priorParameters_test, 1)
  dpobj = DirichletProcessCreate(data_test, weibull_object_test)

  dpobj$clusterLabels <- 1
  dpobj$numberClusters <- 1
  dpobj$pointsPerCluster <- 10
  dpobj$clusterParameters <- PriorDraw(dpobj$mixingDistribution, 1)
  dpobj$m <- 3

  clusterLabels <- dpobj$clusterLabels
  pointsPerCluster <- dpobj$pointsPerCluster

  currentLabel <- clusterLabels[1]
  pointsPerCluster[currentLabel] <- pointsPerCluster[currentLabel] - 1

  dpobj$pointsPerCluster <- pointsPerCluster

  aux <- PriorDraw(weibull_object_test, 3)

  dpobj = ClusterLabelChange(dpobj, 1, 2, currentLabel, aux)

  expect_equal(dpobj$numberClusters, 2)
  expect_equal(dpobj$pointsPerCluster, c(9,1))
  expect_equal(sum(dpobj$pointsPerCluster), num_test_points)
})

test_that("Cluster Label Change Nonconjugate 3", {

  num_test_points = 10
  data_test = rweibull(num_test_points, 1,1)
  priorParameters_test = matrix(c(1,1,1), ncol=3)

  weibull_object_test <- WeibullMixtureCreate(priorParameters_test, 1)
  dpobj = DirichletProcessCreate(data_test, weibull_object_test)

  dpobj$clusterLabels <- rep(c(1,2), 5)
  dpobj$numberClusters <- 2
  dpobj$pointsPerCluster <- c(5,5)
  dpobj$clusterParameters <- PriorDraw(dpobj$mixingDistribution, 2)
  dpobj$m <- 3

  clusterLabels <- dpobj$clusterLabels
  pointsPerCluster <- dpobj$pointsPerCluster

  currentLabel <- clusterLabels[1]
  pointsPerCluster[currentLabel] <- pointsPerCluster[currentLabel] - 1

  dpobj$pointsPerCluster <- pointsPerCluster

  aux <- PriorDraw(weibull_object_test, 3)

  dpobj = ClusterLabelChange(dpobj, 1, 2, currentLabel, aux)

  expect_equal(dpobj$numberClusters, 2)
  expect_equal(dpobj$pointsPerCluster, c(4,6))
  expect_equal(sum(dpobj$pointsPerCluster), num_test_points)
})

test_that("Cluster Label Change Nonconjugate 4", {

  num_test_points = 10
  data_test = rweibull(num_test_points, 1,1)
  priorParameters_test = matrix(c(1,1,1), ncol=3)

  weibull_object_test <- WeibullMixtureCreate(priorParameters_test, 1)
  dpobj = DirichletProcessCreate(data_test, weibull_object_test)

  dpobj$clusterLabels <- c(1, rep.int(2, 9))
  dpobj$numberClusters <- 2
  dpobj$pointsPerCluster <- c(1,9)
  dpobj$clusterParameters <- PriorDraw(dpobj$mixingDistribution, 2)
  dpobj$m <- 3

  clusterLabels <- dpobj$clusterLabels
  pointsPerCluster <- dpobj$pointsPerCluster

  currentLabel <- clusterLabels[1]
  pointsPerCluster[currentLabel] <- pointsPerCluster[currentLabel] - 1

  dpobj$pointsPerCluster <- pointsPerCluster

  aux <- PriorDraw(weibull_object_test, 3)

  dpobj = ClusterLabelChange(dpobj, 1, 2, currentLabel, aux)

  expect_equal(dpobj$numberClusters, 1)
  expect_equal(dpobj$pointsPerCluster, 10)
  expect_equal(sum(dpobj$pointsPerCluster), num_test_points)

})

test_that("Cluster Label Change Nonconjugate 5", {

  num_test_points = 10
  data_test = rweibull(num_test_points, 1,1)
  priorParameters_test = matrix(c(1,1,1), ncol=3)

  weibull_object_test <- WeibullMixtureCreate(priorParameters_test, 1)
  dpobj = DirichletProcessCreate(data_test, weibull_object_test)

  dpobj$clusterLabels <- c(1, rep.int(2, 9))
  dpobj$numberClusters <- 2
  dpobj$pointsPerCluster <- c(1,9)
  dpobj$clusterParameters <- PriorDraw(dpobj$mixingDistribution, 2)
  dpobj$m <- 3

  clusterLabels <- dpobj$clusterLabels
  pointsPerCluster <- dpobj$pointsPerCluster

  currentLabel <- clusterLabels[1]
  pointsPerCluster[currentLabel] <- pointsPerCluster[currentLabel] - 1

  dpobj$pointsPerCluster <- pointsPerCluster

  aux <- PriorDraw(weibull_object_test, 3)

  dpobj = ClusterLabelChange(dpobj, 1, 3, currentLabel, aux)

  expect_equal(dpobj$numberClusters, 2)
  expect_equal(dpobj$pointsPerCluster, c(1, 9))
  expect_equal(sum(dpobj$pointsPerCluster), num_test_points)

})

test_that("Cluster Label Change Nonconjugate 6", {

  num_test_points = 10
  data_test = rweibull(num_test_points, 1,1)
  priorParameters_test = matrix(c(1,1,1), ncol=3)

  weibull_object_test <- WeibullMixtureCreate(priorParameters_test, 1)
  dpobj = DirichletProcessCreate(data_test, weibull_object_test)

  dpobj$clusterLabels <- c(1, 1, rep.int(2, 8))
  dpobj$numberClusters <- 2
  dpobj$pointsPerCluster <- c(2,8)
  dpobj$clusterParameters <- PriorDraw(dpobj$mixingDistribution, 2)
  dpobj$m <- 3

  clusterLabels <- dpobj$clusterLabels
  pointsPerCluster <- dpobj$pointsPerCluster

  currentLabel <- clusterLabels[1]
  pointsPerCluster[currentLabel] <- pointsPerCluster[currentLabel] - 1

  dpobj$pointsPerCluster <- pointsPerCluster

  aux <- PriorDraw(weibull_object_test, 3)

  dpobj = ClusterLabelChange(dpobj, 1, 3, currentLabel, aux)

  expect_equal(dpobj$numberClusters, 3)
  expect_equal(dpobj$pointsPerCluster, c(1, 8, 1))
  expect_equal(sum(dpobj$pointsPerCluster), num_test_points)

})
