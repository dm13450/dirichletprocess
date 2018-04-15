context("Hierarchical Beta")


test_that("",{

  testData <- replicate(3, rbeta(100, 3, 6), simplify = FALSE)

  dp <- DirichletProcessHierarchicalBeta(testData, 1, gammaPriors = c(2, 0.01))


})
