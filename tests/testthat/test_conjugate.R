context("Dirichlet Conjugate Functions")

num_test_points = 10
data_test = rnorm(num_test_points)
priorParameters_test = matrix(c(1,1,1,1), ncol=4)
normal_object_test = MixingDistribution("normal", priorParameters_test, "conjugate")


test_that("Dirichlet Conjugate Class", {

  dpobj = DirichletProcessCreate(data_test, normal_object_test)

  expect_is((dpobj), c("list", "dirichletprocess", "normal", "conjugate"))
})

test_that("Dirichlet Conjugate Initialise", {

  dpobj = DirichletProcessCreate(data_test, normal_object_test)
  dpobj = Initialise(dpobj)

  expect_equal(dpobj$numberClusters, 1)
  expect_equal(dpobj$clusterLabels, rep(1, dpobj$n))
  expect_equal(dpobj$pointsPerCluster, dpobj$n)

  expect_length((dpobj$clusterParameters), 2)
  expect_length((dpobj$predictiveArray), num_test_points)

})

test_that("Dirichlet Conjugate Cluster Label Update", {

  dpobj = DirichletProcessCreate(data_test, normal_object_test)
  dpobj = Initialise(dpobj)
  dpobj = ClusterComponentUpdate(dpobj)

  expect_equal(length(dpobj$clusterLabels), num_test_points)
  expect_equal(sum(dpobj$pointsPerCluster), num_test_points)

  expect_length(dpobj$clusterParameters, 2)
})

test_that("Dirichlet Conjugate Cluster Parameter Update", {

  dpobj = DirichletProcessCreate(data_test, normal_object_test)
  dpobj = Initialise(dpobj)
  dpobj = ClusterParameterUpdate(dpobj)

  expect_length(dpobj$clusterLabels, num_test_points)
  expect_equal(sum(dpobj$pointsPerCluster), num_test_points)
  expect_length(dpobj$clusterParameters, 2)
  expect_length(dpobj$clusterParameters[[1]], 1)
  expect_length(dpobj$clusterParameters[[2]], 1)

})

test_that("Dirichlet Conjugate Cluster Predict", {

  dpobj <- DirichletProcessCreate(data_test, normal_object_test)
  dpobj <- Initialise(dpobj)
  dpobj <- Fit(dpobj, 10, FALSE, FALSE)
  predicted_labels <- ClusterLabelPredict(dpobj, rnorm(10))

  expect_equal(length(predicted_labels$componentIndexes), 10)

})


