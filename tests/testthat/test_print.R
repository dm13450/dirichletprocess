context("Print")



test_that("Normal Printing", {

  dp <- DirichletProcessGaussian(rnorm(10))

  expect_error(capture.output(print(dp)), NA)
  expect_error(capture.output(print(dp, param_summary = TRUE)), NA)

  dpfit <- Fit(dp, 2, progressBar = FALSE)
  expect_error(capture.output(print(dpfit)), NA)
  expect_error(capture.output(print(dpfit, param_summary = TRUE)), NA)

})

test_that("Exp Printing", {

  dp <- DirichletProcessExponential(rexp(10))

  expect_error(capture.output(print(dp)), NA)
  expect_error(capture.output(print(dp, param_summary = TRUE)), NA)

  dpfit <- Fit(dp, 2, progressBar = FALSE)
  expect_error(capture.output(print(dpfit)), NA)
  expect_error(capture.output(print(dpfit, param_summary = TRUE)), NA)
})

test_that("Beta Printing", {

  capture.output(dp <- DirichletProcessBeta(rbeta(10, 2, 3), 1))

  expect_error(capture.output(print(dp)), NA)
  expect_error(capture.output(print(dp, param_summary = TRUE)), NA)

  dpfit <- Fit(dp, 2, progressBar = FALSE)
  expect_error(capture.output(print(dpfit)), NA)
  expect_error(capture.output(print(dpfit, param_summary = TRUE)), NA)
})

test_that("Weibull Printing", {

  dp <- DirichletProcessWeibull(rweibull(10, 2, 3), c(10, 2, 4))

  expect_error(capture.output(print(dp)), NA)
  expect_error(capture.output(print(dp, param_summary = TRUE)), NA)

  dpfit <- Fit(dp, 2, progressBar = FALSE)
  expect_error(capture.output(print(dpfit)), NA)
  expect_error(capture.output(print(dpfit, param_summary = TRUE)), NA)
})

test_that("MvNormal Printing", {
  testData <- matrix(c(rnorm(10), rnorm(10)), ncol = 2)
  dp <- DirichletProcessMvnormal(testData)

  expect_error(capture.output(print(dp)), NA)
  expect_error(capture.output(print(dp, param_summary = TRUE)), NA)

  dpfit <- Fit(dp, 2, progressBar = FALSE)
  expect_error(capture.output(print(dpfit)), NA)
  expect_error(capture.output(print(dpfit, param_summary = TRUE)), NA)
})

test_that("Hierarchical Printing", {

  N <- 300

  #Sample N random uniform U
  U <- runif(N)
  group1 <- matrix(nrow=N, ncol=2)
  group2 <- matrix(nrow=N, ncol=2)
  #Sampling from the mixture

  m1 <- c(-2,-2)
  m2 <- c(2,2)

  for(i in 1:N){
    if(U[i]<.3){
      group1[i,] <- mvtnorm::rmvnorm(1,m1)
      group2[i,] <- mvtnorm::rmvnorm(1,m1)
    }else if(U[i]<0.7){
      group1[i,] <- mvtnorm::rmvnorm(1,m2)
      group2[i,] <- mvtnorm::rmvnorm(1,m1)
    }else {
      group1[i,] <- mvtnorm::rmvnorm(1,m2)
      group2[i,] <- mvtnorm::rmvnorm(1,m2)
    }
  }

  data_hdp <- list(group1, group2)

  hdp_mvnorm <- DirichletProcessHierarchicalMvnormal2(dataList = data_hdp)

  expect_error(capture.output(print(hdp_mvnorm)), NA)

})





