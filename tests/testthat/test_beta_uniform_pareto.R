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
  names(testTheta) <- c("mu", "nu")

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

test_that("Beta2 Posterior helpers handle fitted theta shapes", {

  set.seed(1)
  dpobj <- DirichletProcessBeta2(rbeta(10, 2, 2), maxY = 1, cpp = FALSE)
  dpobj <- Fit(dpobj, 5, FALSE, FALSE)

  x_grid <- ppoints(5)
  post_func <- PosteriorFunction(dpobj)
  post_eval <- post_func(x_grid)
  post_frame <- PosteriorFrame(dpobj, x_grid, ndraws = 3)

  expect_is(post_func, "function")
  expect_type(post_eval, "double")
  expect_length(post_eval, length(x_grid))
  expect_true(all(is.finite(post_eval)))
  expect_s3_class(post_frame, "data.frame")
  expect_equal(nrow(post_frame), length(x_grid))
})


test_that("Beta2 Parameter Proposal",{

  beta2Obj <- BetaMixture2Create()

  pd <- PriorDraw(beta2Obj, 1)

  # Access function from namespace if not available in global environment
  if (!exists("MhParameterProposal")) {
    MhParameterProposal <- get("MhParameterProposal", getNamespace("dirichletprocess"))
  }
  newParams <- MhParameterProposal(beta2Obj, pd)

  expect_is(pd, "list")
  expect_length(pd, 2)
  lapply(pd, function(x) expect_length(x, 1))

})

