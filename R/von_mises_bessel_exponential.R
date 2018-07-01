#'Create a von Mises mixing distribution
#'
#'
#'@param priorParameters Prior parameters for the base measure which are, in
#'  order, (mu_0, R_0, c).
#'
#'@return Mixing distribution object
#'@export
vonMisesMixtureCreate <- function(priorParameters){
  mdobj <- MixingDistribution("normal", priorParameters, "conjugate")
  return(mdobj)
}

logBesselI <- function(x, nu) log(besselI(x, nu, expon.scaled = TRUE)) + x

# von Mises
dvm <- function(x, mu, kp) {
  logpdf <- kp * cos(x - mu) - log(2*pi) - logBesselI(kp, 0)
  return(exp(logpdf))
}

library(flexcircmix)
library(circglmbayes)

Likelihood.vonmises <- function(mdobj, x, theta) dvm(x, theta[[1]], theta[[2]])

PriorDraw.vonmises <- function(mdobj, n = 1) {

  priorParameters <- mdobj$priorParameters
  mu_0 <- priorParameters[1]
  R_0  <- priorParameters[2]
  c    <- priorParameters[3]

  if (c < 0 | R_0 < -1) stop("Prior parameters out of bounds.")

  kp <- rbesselexp(n, c, R_0)
  mu <- vapply(kp, function(kpi) {
    rvmc(1, mu_0, -R_0 * kpi / c)
    }, FUN.VALUE = 0)

  theta <- list(array(mu, dim = c(1, 1, n)),
                array(kp, dim = c(1, 1, n)))
  return(theta)
}


PriorDraw.vonmises <- function(mdobj, n = 1, nsamp = 5) {

  priorParameters <- mdobj$priorParameters
  mu_0 <- priorParameters[1]
  R_0  <- priorParameters[2]
  c    <- priorParameters[3]

  if (c < 0 | R_0 < -1) stop("Prior parameters out of bounds.")

  kp <- rbesselexp(n, c, R_0)
  mu <- vapply(kp, function(kpi) {
    rvmc(1, mu_0, -R_0 * kpi / c)
  }, FUN.VALUE = 0)

  theta <- list(array(mu, dim = c(1, 1, n)),
                array(kp, dim = c(1, 1, n)))
  return(theta)
}



dbesselexp2 <- function(kp, mu, mu_n, R_n, n_n) {
  eta <- n_n
  g <- -R_n * cos(mu - mu_n) / n_n
  dbesselexp(kp, eta, g)
}
rbesselexp2 <- function(kp, mu, mu_n, R_n, n_n) {
  eta <- n_n
  g <- -R_n * cos(mu - mu_n) / n_n
  rbesselexp(kp, eta, g)
}


# // Sample a new value for kappa.
etag    = - R_psi * cos(b0_cur - psi_bar);
sk_res  = sampleKappa(etag, n_post);
kp_cur  = sk_res(0);

dbesselexp(3, 15, 9)
draws <- PriorDraw.vonmises(list(priorParameters = c(mu_0 = 0, g = -19/20, c = 20)), n = 10000)
plot(cbind(mu = draws[[1]], kp = draws[[2]]))
# PosteriorParameters.normal <- function(mdobj, x) {
#
#   priorParameters <- mdobj$priorParameters
#
#   n.x <- length(x)
#   ybar <- mean(x)
#
#   mu0 <- priorParameters[1]
#   kappa0 <- priorParameters[2]
#   alpha0 <- priorParameters[3]
#   beta0 <- priorParameters[4]
#
#   mu.n <- (kappa0 * mu0 + n.x * ybar)/(kappa0 + n.x)
#   kappa.n <- kappa0 + n.x
#   alpha.n <- alpha0 + n.x/2
#   beta.n <- beta0 + 0.5 * sum((x - ybar)^2) + kappa0 * n.x * (ybar - mu0)^2/(2 *
#                                                                                (kappa0 + n.x))
#
#   PosteriorParameters <- matrix(c(mu.n, kappa.n, alpha.n, beta.n), ncol = 4)
#   return(PosteriorParameters)
# }
#
# PosteriorDraw.normal <- function(mdobj, x, n = 1) {
#
#   PosteriorParameters_calc <- PosteriorParameters(mdobj, x)
#
#   lambda <- rgamma(n, PosteriorParameters_calc[3], PosteriorParameters_calc[4])
#   mu <- rnorm(n, PosteriorParameters_calc[1], 1/sqrt(PosteriorParameters_calc[2] *
#                                                        lambda))
#   theta <- list(array(mu, dim = c(1, 1, n)), array(sqrt(1/lambda), dim = c(1, 1,
#                                                                            n)))
#   return(theta)
# }
#
# Predictive.normal <- function(mdobj, x) {
#
#   priorParameters <- mdobj$priorParameters
#   predictiveArray <- numeric(length(x))
#
#   for (i in seq_along(x)) {
#
#     PosteriorParameters_calc <- PosteriorParameters(mdobj, x[i])
#
#     predictiveArray[i] <- (gamma(PosteriorParameters_calc[3])/gamma(priorParameters[3])) *
#       ((priorParameters[4]^(priorParameters[3]))/PosteriorParameters_calc[4]^PosteriorParameters_calc[3]) *
#       sqrt(priorParameters[2]/PosteriorParameters_calc[2])
#   }
#   return(predictiveArray)
# }
