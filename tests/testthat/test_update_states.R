context("HMM Update States")

mdobj <- GaussianMixtureCreate()
data <- c(rnorm(50, 1, sqrt(3)), rnorm(50, 3, sqrt(3)), rnorm(50, 5, sqrt(3)))
states <- seq_along(data)
oldparams <- PriorDraw(mdobj, length(data))
params <- lapply(states,
                    function(i) lapply(oldparams, function(x) x[,,i,drop=F]))

alpha <- 2
beta <- 3

test_that("Update States", {

  new_states <- update_states(mdobj, data, states, params, alpha, beta)

  expect_is(new_states, "list")
  expect_length(new_states, 2)
  expect_length(new_states[[1]], length(data))
  expect_length(new_states[[2]], length(data))

})
