#' Generate a weighted function.
#'
#' @param func Function that is used of the form func(x, params).
#' @param weights Weighting of each cluster.
#' @param params Cluster parameter list
#' @return weighted function
#'
#' @export
weighted_function_generator <- function(func, weights, params) {

  weights <- weights/sum(weights)

  weightedFunc <- function(y) {

    if (class(y) == "matrix"){
      out <- numeric(nrow(y))
    }
    else{
      out <- numeric(length(y))
    }
    cumWeight <- 0
    for (i in seq_along(weights)) {
      if (cumWeight > (1 - 1e-6)){
        break
      }
      cl_params <- vector("list", length = length(params))
      for (j in seq_along(params)) {
        cl_params[[j]] <- params[[j]][, , i, drop = FALSE]
      }
      out <- out + weights[i] * func(y, cl_params)
      cumWeight <- cumWeight + weights[i]
    }

    return(out)
  }
  return(weightedFunc)
}

dpareto <- function(x, xm, alpha) ifelse(x > xm, alpha * xm^alpha/(x^(alpha + 1)),
  0)
ppareto <- function(q, xm, alpha) ifelse(q > xm, 1 - (xm/q)^alpha, 0)
qpareto <- function(p, xm, alpha) ifelse(p < 0 | p > 1, NaN, xm * (1 - p)^(-1/alpha))
rpareto <- function(n, xm, alpha) qpareto(runif(n), xm, alpha)

VectorToArray <- function(paramVector){

  paramList <- vector("list", length(paramVector))

  paramList <- lapply(seq_along(paramList), function(i) array(paramVector[i], dim = c(1,1,1)))

  return(paramList)
}




