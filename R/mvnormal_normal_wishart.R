#' Create a multivariate normal mixing distribution
#'
#' @param priorParameters The prior parameters for the Multivariate Normal.
#' @export
MvnormalCreate <- function(priorParameters) {

  mdobj <- MixingDistribution("mvnormal", priorParameters, "conjugate")

  return(mdobj)
}

Likelihood.mvnormal <- function(mdobj, x, theta) {

  y <- sapply(seq_len(dim(theta[[1]])[3]),
            function(i) mvtnorm::dmvnorm(x, theta[[1]][,, i], theta[[2]][, , i]))

  return(y)
}

PriorDraw.mvnormal <- function(mdobj, n = 1) {

  priorParameters <- mdobj$priorParameters

  sig <- rWishart(n, priorParameters$nu, priorParameters$Lambda)

  mu <- simplify2array(lapply(seq_len(n), function(x) mvtnorm::rmvnorm(1, priorParameters$mu0,
    solve(sig[, , x] * priorParameters$kappa0))))

  theta <- list(mu = mu, sig = sig)
  return(theta)
}

PosteriorDraw.mvnormal <- function(mdobj, x, n = 1) {

  post_parameters <- PosteriorParameters(mdobj, x)

  sig <- rWishart(n, post_parameters$nu_n, post_parameters$t_n)
  mu <- simplify2array(lapply(seq_len(n), function(x) mvtnorm::rmvnorm(1, post_parameters$mu_n,
    solve(post_parameters$kappa_n * sig[, , x]))))

  return(list(mu = mu, sig = sig/post_parameters$kappa_n^2))
}

PosteriorParameters.mvnormal <- function(mdobj, x) {

  if (!is.matrix(x)) {
    x <- matrix(x, ncol = length(x))
  }

  kappa0 <- mdobj$priorParameters$kappa0
  mu0 <- mdobj$priorParameters$mu0

  kappa_n <- kappa0 + nrow(x)
  nu_n <- mdobj$priorParameters$nu + nrow(x)

  mu_n <- (kappa0 * mu0 + nrow(x) * colMeans(x))/(nrow(x) + kappa0)

  sum_squares <- (nrow(x) - 1) * var(x)

  sum_squares[is.na(sum_squares)] <- 0

  t_n <- mdobj$priorParameters$Lambda + sum_squares + ((kappa0 * nrow(x))/(kappa0 +
    nrow(x))) * ((mu0 - colMeans(x)) %*% t(mu0 - colMeans(x)))

  return(list(mu_n = mu_n, t_n = t_n, kappa_n = kappa_n, nu_n = nu_n))
}

Predictive.mvnormal <- function(mdobj, x) {

  priorParameters <- mdobj$priorParameters
  pred <- numeric(nrow(x))

  d <- ncol(x)

  for (i in seq_along(pred)) {

    post_params <- PosteriorParameters(mdobj, x[i, ,drop=FALSE])

    pred[i] <- (pi^(-nrow(x[i,,drop=FALSE]) * d/2))
    pred[i] <- pred[i] * (priorParameters$kappa0/post_params$kappa_n)^(d/2)
    pred[i] <- pred[i] * (det(priorParameters$Lambda)^(priorParameters$nu/2))/(det(post_params$t_n)^(post_params$nu_n/2))

    if (pred[i] > 0) {
      gamma_contrib <- prod(sapply(seq_along(d),
                            function(j) gamma(priorParameters$nu/2 + nrow(x[i,,drop=FALSE])/2 + (1 - j)/2)))/prod(sapply(seq_along(d),
                            function(j) gamma(priorParameters$nu/2 + (1 - j)/2)))
      pred[i] <- pred[i] * gamma_contrib
    }
  }
  return(pred)
}
