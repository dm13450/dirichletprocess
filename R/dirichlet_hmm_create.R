#' Create a generic Dirichlet process hidden Markov Model
#'
#' Create a hidden Markov model where the data is believed to be generated from the mixing object distribution.
#'
#' @param x Data to be modelled
#' @param mdobj Mixing disitribution object
#' @param alpha Alpha parameter
#' @param beta Beta parameter
#' @export
DirichletHMMCreate <- function(x, mdobj, alpha, beta){

  if(is.vector(x)){
    x <- matrix(x, ncol=1)
  }

  states <- seq_len(nrow(x))
  params <- PriorDraw(mdobj, nrow(x))

  newParams <- lapply(seq_along(states),
                      function(i) lapply(params, function(x) x[,,i, drop=F]))

  dp <- list()

  dp$data <- x
  dp$n <- length(x)
  dp$mixingDistribution <- mdobj
  dp$states <- states
  dp$uniqueParams <- params
  dp$params <- newParams
  dp$alpha <- alpha
  dp$beta <- beta

  class(dp) <- append(class(dp), c("markov", "dirichletprocess", class(mdobj)[-1]))

  return(dp)
}
