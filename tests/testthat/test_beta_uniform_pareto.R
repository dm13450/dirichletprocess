context("Beta Uniform Pareto Distribution")


test_that("Beta2 Create",{

 mdobj <- BetaMixture2Create()

 expect_is(mdobj, c("list", "beta2", "nonconjugate"))

})

test_that("Beta2 Likelihood", {

  beta2Obj <- BetaMixture2Create()
  betaObj <- BetaMixtureCreate()

  testTheta <- list()
  testTheta[[1]] <- array(0.5, dim=c(1,1,1))
  testTheta[[2]] <- array(0.5, dim=c(1,1,1))

  oldLik <- Likelihood(betaObj, c(0.1, 0.2), testTheta)
  newLik <- Likelihood(beta2Obj, c(0.1, 0.2), testTheta)

  expect_equal(newLik, oldLik)

})

test_that("Beta2 Prior Draw", {

  beta2Obj <- BetaMixture2Create()

  pd <- PriorDraw(beta2Obj, 1)

  expect_is(pd, "list")
  expect_length(pd, 2)
  lapply(pd, function(x) expect_length(x, 1))

  pd <- PriorDraw(beta2Obj, 10)

  expect_is(pd, "list")
  expect_length(pd, 2)
  lapply(pd, function(x) expect_length(x, 10))

})

test_that("Beta2 Prior Density",{

  beta2Obj <- BetaMixture2Create()

  pd <- PriorDraw(beta2Obj, 1)

  testDensity <- PriorDensity(beta2Obj, pd)

  expect_is(testDensity, "numeric")

})


test_that("Beta2 Parameter Proposal",{

  beta2Obj <- BetaMixture2Create()

  pd <- PriorDraw(beta2Obj, 1)

  newParams <- MhParameterProposal(beta2Obj, pd)

  expect_is(pd, "list")
  expect_length(pd, 2)
  lapply(pd, function(x) expect_length(x, 1))

})


