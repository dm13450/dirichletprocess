DirichletHMMCreate <- function(x, mdobj, alpha, beta){

  states <- seq_len(length(x))
  params <- PriorDraw(mdobj, length(x))

  newParams <- lapply(seq_along(states),
                      function(i) lapply(params, function(x) x[,,i, drop=F]))

  dp <- list()

  dp$data <- x
  dp$mdobj <- mdobj
  dp$states <- states
  dp$params <- newParams
  dp$alpha <- alpha
  dp$beta <- beta

  return(dp)
}
