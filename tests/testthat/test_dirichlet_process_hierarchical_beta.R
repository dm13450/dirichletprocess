context("Hierarchical Beta")


test_that("Hierarchical Beta Update",{

  testData <- replicate(3, rbeta(100, 3, 6), simplify = FALSE)

  dpobj <- DirichletProcessHierarchicalBeta(testData, 1, gammaPriors = c(2, 0.01))

  expect_is(dpobj, c("list"))
  expect_length(dpobj$indDP, 3)

})
