
UpdateStates <- function(dp){

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

        likelihoodValue <- vapply(params[1:2], function(x) Likelihood(mdobj, data[i], x), numeric(1))

        newState <- sample(states[1:2], 1, prob=wts*likelihoodValue)
        states[i] <- newState
        params[i] <- params[newState]
      }

    } else if ( (i == n)  ) {
      if (states[i] - states[i-1] != 1){

        states_eq1 <- states[1:(n-1)] == states[i-1]

        n_sn1 <- sum(states_eq1) - 1


        likelihoodValue <- vapply(params[(i-1):i], function(x) Likelihood(mdobj, data[i], x), numeric(1))

        wts <- c(n_sn1 + alpha,
                 beta)

        candiateStates <- c(i-1, i)

        newState <- sample(candiateStates, 1, prob=wts*likelihoodValue)

        states[i] <- states[newState]
        params[i] <- params[newState]
      }
    } else {

      if(states[i-1] != states[i+1]){

        niiVec <- states[1:(i-1)] == states[i-1]
        nipipVec <- states[(i+1):n] == states[i+1]

        nii <- sum(niiVec) - 1
        nipip <- sum(nipipVec) - 1


        candiateStates <- c(i-1, i+1)

        likelihoodValue <- vapply(params[candiateStates], function(x) Likelihood(mdobj, data[i], x), numeric(1))

        wts <- c(
          (nii + alpha)/(nii + 1 + beta + alpha),
          (nipip + alpha) / (nipip + beta + alpha)
        )

        newState <- sample(candiateStates, 1, prob = wts*likelihoodValue)

        states[i] <- states[newState]
        params[i] <- params[newState]

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


