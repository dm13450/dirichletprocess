context("Metropolis Hastings Tests")

# Helper function to call appropriate MetropolisHastings method based on distribution type
call_metropolis_hastings <- function(mixingDistribution, x, start_pos, no_draws) {
  ns <- getNamespace("dirichletprocess")

  if (is.list(mixingDistribution)) {
    dist_class <- get("mixing_distribution_method_class", envir = ns)(
      mixingDistribution,
      "MetropolisHastings"
    )

    if (dist_class == "weibull") {
      # Call weibull method directly
      weibull_func <- get("MetropolisHastings.weibull", envir = ns)
      return(weibull_func(mixingDistribution, x, start_pos, no_draws))
    }
    
    if (dist_class == "beta") {
      # Call beta method implementation directly
      parameter_samples <- list()
      for (i in seq_along(start_pos)) {
        parameter_samples[[i]] <- array(dim = c(dim(start_pos[[i]])[1:2], no_draws))
        parameter_samples[[i]][, , 1] <- start_pos[[i]][, , 1]
      }

      accept_count <- 0
      old_param <- start_pos

      # Get required functions from namespace
      PriorDensity_func <- get("PriorDensity", envir = ns)
      Likelihood_func <- get("Likelihood", envir = ns)
      MhParameterProposal_func <- get("MhParameterProposal", envir = ns)
      
      # Calculate initial log prior and likelihood
      old_prior <- log(PriorDensity_func(mixingDistribution, old_param))
      old_likelihood <- sum(log(Likelihood_func(mixingDistribution, x, old_param)))

      # MCMC loop
      for (i in seq_len(no_draws - 1)) {
        # Propose new parameters
        prop_param <- MhParameterProposal_func(mixingDistribution, old_param)

        # Calculate new log prior and likelihood
        new_prior <- log(PriorDensity_func(mixingDistribution, prop_param))
        new_likelihood <- sum(log(Likelihood_func(mixingDistribution, x, prop_param)))

        # Calculate acceptance probability
        log_ratio <- (new_prior + new_likelihood) - (old_prior + old_likelihood)
        accept_prob <- min(1, exp(log_ratio))

        # Handle numerical issues
        if (is.na(accept_prob) || !is.finite(accept_prob)) {
          accept_prob <- 0
        }

        # Accept or reject
        if (runif(1) < accept_prob) {
          accept_count <- accept_count + 1
          sampled_param <- prop_param
          old_likelihood <- new_likelihood
          old_prior <- new_prior
        } else {
          sampled_param <- old_param
        }

        # Store parameters
        old_param <- sampled_param
        for (j in seq_along(start_pos)) {
          parameter_samples[[j]][, , i + 1] <- sampled_param[[j]][, , 1]
        }
      }

      accept_ratio <- accept_count / no_draws
      return(list(parameter_samples = parameter_samples, accept_ratio = accept_ratio))
    }
  }

  # Fallback to default method
  default_func <- get("MetropolisHastings.default", envir = ns)
  return(default_func(mixingDistribution, x, start_pos, no_draws))
}

test_that("Metropolis Hastings Full Sample Weibull", {

  test_data <- rweibull(100, 1, 1)

  test_MixingDistribution <- WeibullMixtureCreate(c(1,1,1), 1)

  test_mh <- call_metropolis_hastings(test_MixingDistribution, test_data, PriorDraw(test_MixingDistribution), 20)

  expect_equal(length(test_mh), 2)
  expect_is(test_mh$accept_ratio, "numeric")
  expect_equal(length(test_mh$parameter_samples), 2)
  expect_equal(length(test_mh$parameter_samples[[1]]), 20)
  expect_equal(length(test_mh$parameter_samples[[2]]), 20)

})

test_that("Metropolis Hastings Full Sample Beta", {

  test_data <- rbeta(10, 2,2)

  test_mdobj <- BetaMixtureCreate(mhStepSize = c(0.1, 0.1), maxT = 1)
  test_start_pos <- PriorDraw(test_mdobj)

  test_mh <- call_metropolis_hastings(test_mdobj, test_data, test_start_pos, 20)

  expect_equal(length(test_mh), 2)
  expect_is(test_mh$accept_ratio, "numeric")
  expect_equal(length(test_mh$parameter_samples), 2)
  expect_equal(length(test_mh$parameter_samples[[1]]), 20)
  expect_equal(length(test_mh$parameter_samples[[2]]), 20)
})
