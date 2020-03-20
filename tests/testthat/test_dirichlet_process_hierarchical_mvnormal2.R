context("Hierarchical Mv Normal")

test_that("",{
  require(mvtnorm)
  testData <- replicate(3, rmvnorm(500, c(3,3), diag(2)), simplify = FALSE)
  g0Priors <- list(nu0 = 2,
                   phi0 = diag(2),
                   mu0 = matrix(c(0, 0),ncol=2),
                   sigma0 = diag(2))
  
  dp <- DirichletProcessHierarchicalMvnormal2(testData, g0Priors, gammaPriors = c(2, 0.01))
  expect_s3_class(dp, c("list", "dirichletprocess", "hierarchical"))
  
})