#' Update States for Dirichlet Process
#'
#' Updates the states of a Dirichlet process object using either C++ or R implementation.
#'
#' @param dp A Dirichlet process object
#' @return Updated Dirichlet process object
#' @export
UpdateStates <- function(dp){
  # Use C++ implementation if enabled
  if (using_cpp_markov_samplers()) {
    cpp_result <- UpdateStates.cpp(dp)
    dp$states <- cpp_result[[1]]
    dp$params <- cpp_result[[2]]
    return(dp)
  }

  # Original R implementation
  new_states <- update_states(dp$mixingDistribution,
                              dp$data,
                              dp$states,
                              dp$params,
                              dp$alpha,
                              dp$beta)
  dp$states <- new_states[[1]]
  dp$params <- new_states[[2]]
  return(dp)
}

update_states <- function(mdobj, data, states, params, alpha, beta){

  n <- length(data)

  for(i in seq_len(n)){

    if(i == 1){

      if (states[1] != states[2]){

        states_eq <- states[2:n] == states[2]
        n_s2 <- sum(states_eq) - 1

        wts <- c(
          alpha/(beta + alpha),
          (n_s2 + alpha)/(n_s2 + beta + alpha)
        )

        likelihoodValue <- numeric(2)
        for (k in 1:2) {
          # Extract parameters for state k
          state_params <- params[[states[k]]]
          if (inherits(mdobj, "normal")) {
            if (!is.list(state_params) || !all(c("mean", "sd") %in% names(state_params))) {
              if (is.numeric(state_params)) {
                state_params <- list(mean = state_params, sd = 1)
              } else if (is.list(state_params)) {
                if (!("mean" %in% names(state_params))) state_params$mean <- 0
                if (!("sd" %in% names(state_params))) state_params$sd <- 1
              } else {
                stop("Invalid state parameters for normal distribution")
              }
            }
          }
          # Call Likelihood with properly formatted parameters
          likelihoodValue[k] <- Likelihood(mdobj, data[i], state_params)
        }

        newState <- sample(states[1:2], 1, prob=wts*likelihoodValue)
        states[i] <- newState
        params[[i]] <- params[[newState]]
      }

    } else if ( (i == n)  ) {
      if (states[i] - states[i-1] != 1){

        states_eq1 <- states[1:(n-1)] == states[i-1]

        n_sn1 <- sum(states_eq1) - 1


        likelihoodValue <- numeric(2)
        candidate_indices <- c(i-1, i)
        for (k in 1:2) {
          state_params <- params[[states[candidate_indices[k]]]]
          if (inherits(mdobj, "normal")) {
            if (!is.list(state_params) || !all(c("mean", "sd") %in% names(state_params))) {
              if (is.numeric(state_params)) {
                state_params <- list(mean = state_params, sd = 1)
              } else if (is.list(state_params)) {
                if (!("mean" %in% names(state_params))) state_params$mean <- 0
                if (!("sd" %in% names(state_params))) state_params$sd <- 1
              } else {
                stop("Invalid state parameters for normal distribution")
              }
            }
          }
          likelihoodValue[k] <- Likelihood(mdobj, data[i], state_params)
        }

        wts <- c(n_sn1 + alpha,
                 beta)

        candiateStates <- c(i-1, i)

        newState <- sample(candiateStates, 1, prob=wts*likelihoodValue)

        states[i] <- states[newState]
        params[[i]] <- params[[newState]]
      }
    } else {

      if(states[i-1] != states[i+1]){

        niiVec <- states[1:(i-1)] == states[i-1]
        nipipVec <- states[(i+1):n] == states[i+1]

        nii <- sum(niiVec) - 1
        nipip <- sum(nipipVec) - 1


        candiateStates <- c(i-1, i+1)

        likelihoodValue <- numeric(2)
        for (k in 1:2) {
          state_params <- params[[states[candiateStates[k]]]]
          if (inherits(mdobj, "normal")) {
            if (!is.list(state_params) || !all(c("mean", "sd") %in% names(state_params))) {
              if (is.numeric(state_params)) {
                state_params <- list(mean = state_params, sd = 1)
              } else if (is.list(state_params)) {
                if (!("mean" %in% names(state_params))) state_params$mean <- 0
                if (!("sd" %in% names(state_params))) state_params$sd <- 1
              } else {
                stop("Invalid state parameters for normal distribution")
              }
            }
          }
          likelihoodValue[k] <- Likelihood(mdobj, data[i], state_params)
        }

        wts <- c(
          (nii + alpha)/(nii + 1 + beta + alpha),
          (nipip + alpha) / (nipip + beta + alpha)
        )

        newState <- sample(candiateStates, 1, prob = wts*likelihoodValue)

        states[i] <- states[newState]
        params[[i]] <- params[[newState]]

      }

    }


  }

  states <- relabel_states(states)

  return(list(states, params))
}

relabel_states <- function(dp_states){
  newUniqueStates <- length(unique(dp_states))
  rep(seq_len(newUniqueStates), table(dp_states))
}
