context("Dirichlet Non Conjugate Functions")


num_test_points = 10
data_test = rweibull(num_test_points, 1,1)
priorParameters_test = matrix(c(1,1,1), ncol=3)
weibull_object_test <- WeibullMixtureCreate(priorParameters_test, 1)


test_that("Dirichlet Nonconjugate Class", {

  dpobj <- DirichletProcessCreate(data_test, weibull_object_test)

  expect_is(dpobj, c("list", "dirichletprocess", "weibull", "nonconjugate"))

})

test_that("Nonconjugate Initialise", {

  dpobj <- DirichletProcessCreate(data_test, weibull_object_test)

  dpobj <- Initialise(dpobj, verbose=FALSE)

  expect_is(dpobj, c("list", "dirichletprocess", "weibull", "nonconjugate"))
  expect_equal(dpobj$numberClusters, 1)
  expect_equal(dpobj$clusterLabels, rep_len(1, (num_test_points)))
  expect_equal(dpobj$pointsPerCluster, (num_test_points))

  expect_length((dpobj$clusterParameters), 2)

})

test_that("Dirichlet Nonconjugate Cluster Label Update", {

  dpobj = DirichletProcessCreate(data_test, weibull_object_test)
  dpobj = Initialise(dpobj, verbose=FALSE)
  dpobj = ClusterComponentUpdate(dpobj)

  expect_equal(length(dpobj$clusterLabels), num_test_points)
  expect_equal(sum(dpobj$pointsPerCluster), num_test_points)

  expect_length((dpobj$clusterParameters), 2)
})

test_that("Dirichlet Nonconjugate Cluster Parameter Update", {

  dpobj = DirichletProcessCreate(data_test, weibull_object_test)
  dpobj = Initialise(dpobj, verbose=FALSE)
  dpobj = ClusterParameterUpdate(dpobj)

  expect_equal(length(dpobj$clusterLabels), (num_test_points))
  expect_equal(sum(dpobj$pointsPerCluster), num_test_points)
  expect_length((dpobj$clusterParameters), 2)

})

test_that("Dirichlet Nonconjugate Cluster Predict", {

  dpobj <- DirichletProcessCreate(data_test, weibull_object_test)
  dpobj <- Initialise(dpobj, verbose=FALSE)
  dpobj <- Fit(dpobj, 10, FALSE, FALSE)
  predicted_labels <- ClusterLabelPredict(dpobj, rweibull(10, 1,1))

  expect_equal(length(predicted_labels$componentIndexes), 10)

})


