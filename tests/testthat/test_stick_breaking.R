context("Stick Breaking")

test_that("pi Dirichlet Function", {

  expect_is(piDirichlet(rbeta(10, 1,2)), "numeric")
  expect_length(piDirichlet(rbeta(10, 1,2)), 10)
})

test_that("Numeric and Length", {

  expect_is(StickBreaking(1, 10), "numeric")
  expect_length(StickBreaking(1, 10), 10)
})


test_that("Draw Gj", {

  beta_k <- StickBreaking(2, 10)
  pi_k <- draw_gj(2, beta_k)

  expect_length(pi_k, 10)
  expect_is(pi_k, "numeric")
})


