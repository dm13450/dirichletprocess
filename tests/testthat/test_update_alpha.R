context("Update Alpha")

test_that("Update Alpha",{

  num_test_points = 10
  data_test = rnorm(num_test_points)
  priorParameters_test = matrix(c(1,1,1,1), ncol=4)

  normal_object_test = MixingDistribution("normal", priorParameters_test, "conjugate")
  dpobj = DirichletProcessCreate(data_test, normal_object_test)
  dpobj = Initialise(dpobj)

  dpobj = UpdateAlpha(dpobj)

  expect_is(dpobj$alpha, "numeric")
})

test_that("Update Alpha Hierarchical", {

  dataTest <- list(rbeta(10, 1, 3), rbeta(10, 1, 3), rbeta(10, 3, 5), rbeta(10, 4, 5), rbeta(10, 6, 3))
  dpobjlistTest <- DirichletProcessHierarchicalBeta(dataTest, 1)
  dpobjlistTest <- ClusterComponentUpdate(dpobjlistTest)
  dpobjlistTest <- UpdateAlpha(dpobjlistTest)

  for(i in seq_along(dpobjlistTest$indDP)){
    expect_is(dpobjlistTest$indDP[[i]]$alpha, "numeric")
  }

})
