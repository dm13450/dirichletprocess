context("Update Alpha Beta HMM")

test_that("Log Posterior", {

  expect_is(alphabeta_log_posterior(2, 2, rep(1, 10)),
            "numeric")


  ### This gives warning, need to account for negatives
  expect_is(alphabeta_log_posterior(2, -2, rep(1, 10)),
            "numeric")
  expect_equal(alphabeta_log_posterior(2, -2, rep(1, 10)), -Inf)
  expect_equal(alphabeta_log_posterior(-2, 2, rep(1, 10)), -Inf)
  expect_equal(alphabeta_log_posterior(-2, -2, rep(1, 10)), -Inf)

})

test_that("MH Sampling", {

  newPars <- update_alpha_beta(rep(1, 10))

  expect_length(newPars, 2)
  expect_is(newPars, "numeric")

})

