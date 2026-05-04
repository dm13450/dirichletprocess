#' Fit a Hidden Markov Dirichlet Process Model
#'
#' @param dpObj Initialised Dirichlet Process object
#' @param its Number of iterations to use
#' @param updatePrior Logical flag, defaults to \code{FALSE}. Set whether the parameters of the base measure are updated.
#' @param progressBar Logical flag indicating whether to display a progress bar.
#' @param ... Additional arguments
#' @return A Dirichlet Process object with the fitted cluster parameters and states.
#' @export
Fit.markov <- function(dpObj, its, updatePrior=FALSE, progressBar = FALSE, ...){
  # Use C++ implementation if enabled
  if (using_cpp_markov_samplers()) {
    return(Fit.markov.cpp(dpObj, its, updatePrior, progressBar))
  }

  # Original R implementation follows...
  dpObj <- fit_hmm(dpObj, its, progressBar)
  return(dpObj)
}

# Helper function to ensure proper initialization of HMM parameters
initialize_hmm_params <- function(dpObj) {
  mdobj <- dpObj$mixingDistribution

  # Ensure all theta_k entries have proper structure
  if (inherits(mdobj, "normal")) {
    for (k in seq_along(mdobj$theta_k)) {
      if (!is.list(mdobj$theta_k[[k]]) ||
          !all(c("mean", "sd") %in% names(mdobj$theta_k[[k]]))) {
        # Convert to proper structure
        if (is.numeric(mdobj$theta_k[[k]])) {
          mdobj$theta_k[[k]] <- list(
            mean = mdobj$theta_k[[k]],
            sd = ifelse(is.null(mdobj$tau), 1, sqrt(1/mdobj$tau))
          )
        }
      }
    }
    dpObj$mixingDistribution <- mdobj
  }

  return(dpObj)
}

#' Fit Hidden Markov Model
#'
#' Internal function for fitting Hidden Markov Dirichlet Process models.
#'
#' @param dpObj Dirichlet Process object
#' @param its Number of iterations
#' @param progressBar Display progress bar
#' @return Fitted Dirichlet Process object
#' @export
fit_hmm <- function(dpObj, its, progressBar=F){

  if (progressBar){
    pb <- txtProgressBar(min=0, max=its, width=50, char="-", style=3)
  }

  alphaChain <- numeric(its)
  betaChain <- numeric(its)
  statesChain <- vector("list", its)
  paramChain <- vector("list", its)

  # Ensure initial parameters are correctly structured
  dpObj <- initialize_hmm_params(dpObj)

  for(i in seq_len(its)){

    alphaChain[i] <- dpObj$alpha
    betaChain[i] <- dpObj$beta
    statesChain[[i]] <- dpObj$states


    paramChain[[i]] <- dpObj$uniqueParams

    dpObj <- UpdateStates(dpObj)
    dpObj <- UpdateAlphaBeta(dpObj)
    dpObj <- param_update(dpObj)


    if (progressBar) {
      setTxtProgressBar(pb, i)
    }

  }

  dpObj$alphaChain <- alphaChain
  dpObj$betaChain <- betaChain
  dpObj$statesChain <- statesChain
  dpObj$paramChain <- paramChain
  if (progressBar) {
    close(pb)
  }

  return(dpObj)
}


param_update <- function(dp){
  # Use C++ implementation if enabled
  if (using_cpp_markov_samplers()) {
    return(param_update.cpp(dp))
  }

  # Create a temporary DP object for parameter updates
  temp_dp <- dp
  unique_states <- unique(dp$states)

  # Get parameters for each unique state
  new_unique_params <- list()

  for (i in seq_along(unique_states)) {
    state_indices <- which(dp$states == unique_states[i])
    if (length(state_indices) > 0) {
      state_data <- dp$data[state_indices, , drop = FALSE]

      # Create a temporary object for this state's data
      temp_dp$data <- state_data
      temp_dp$n <- nrow(state_data)
      temp_dp$clusterLabels <- rep(1, nrow(state_data))
      temp_dp$numberClusters <- 1
      temp_dp$pointsPerCluster <- nrow(state_data)

      # Get posterior parameters for this state
      if (inherits(dp$mixingDistribution, "conjugate")) {
        post_params <- PosteriorDraw(dp$mixingDistribution, state_data)
      } else {
        # For non-conjugate, need to use current params as start
        current_params <- dp$uniqueParams
        if (length(current_params) > 0 && i <= length(current_params[[1]])) {
          start_params <- lapply(current_params, function(x) x[,,i,drop=FALSE])
        } else {
          start_params <- PriorDraw(dp$mixingDistribution, 1)
        }
        post_params <- PosteriorDraw(dp$mixingDistribution, state_data, 1, start_pos = start_params)
      }

      # Store parameters
      if (i == 1) {
        for (j in seq_along(post_params)) {
          new_unique_params[[j]] <- post_params[[j]]
        }
      } else {
        for (j in seq_along(post_params)) {
          new_unique_params[[j]] <- abind::abind(new_unique_params[[j]], post_params[[j]], along = 3)
        }
      }
    }
  }

  dp$uniqueParams <- new_unique_params

  # Update params to reference the unique parameters
  dp$params <- lapply(dp$states, function(state) {
    state_idx <- which(unique_states == state)
    lapply(new_unique_params, function(param) {
      if (length(dim(param)) == 3) {
        param[,,state_idx,drop=FALSE]
      } else {
        param[state_idx]
      }
    })
  })

  return(dp)
}
