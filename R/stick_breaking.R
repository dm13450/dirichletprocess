#' The Stick Breaking representation of the Dirichlet process.
#'
#' A Dirichlet process can be represented using a stick breaking construction
#' \deqn{G = \sum _{i=1} ^n pi _i \delta _{\theta _i}},
#' where \eqn{\pi _k = \beta _k  \prod _{k=1} ^{n-1} (1- \beta _k )} are the stick breaking weights.
#' The atoms \eqn{\delta _{\theta _i}} are drawn from \eqn{G_0} the base measure of the Dirichlet Process.
#' The \eqn{\beta _k \sim \mathrm{Beta} (1, \alpha)}. In theory \eqn{n} should be infinite, but we chose some value of \eqn{N} to truncate
#' the series. For more details see reference.
#'
#' @param alpha Concentration parameter of the Dirichlet Process.
#' @param N Truncation value.
#' @return Vector of stick breaking probabilities.
#'
#' @references Ishwaran, H., & James, L. F. (2001). Gibbs sampling methods for stick-breaking priors. Journal of the American Statistical Association, 96(453), 161-173.
#'@export
StickBreaking <- function(alpha, N) {

  betas <- rbeta(N, 1, alpha)
  pis <- piDirichlet(betas)

  return(pis)
}

#'@describeIn StickBreaking Function for calculating stick lengths.
#'@param betas Draws from the Beta distribution.
#'@export
piDirichlet <- function(betas) {
  logCompsFull <- numeric(length(betas))

  betaLogsComp <- log(1 - betas)

  logCompsFull[1] <- 0
  logCompsFull[-1] <- betaLogsComp[-length(betas)]

  logCompsFullSum <- cumsum(logCompsFull)

  logPis <- log(betas) + logCompsFullSum

  exp(logPis)
}

draw_gj <- function(alpha0, beta_k) {

  betaSum <- cumsum(beta_k)

  pi_prime <- vapply(seq_along(beta_k), function(i){
    shape2 <- 1 - sum(betaSum[i])
    if(shape2 < 0) {
      shape2 <- 0
    }
    rbeta(1, alpha0 * beta_k[i], alpha0 * shape2)
  }, numeric(1)
    )
  pi_k <- piDirichlet(pi_prime)
  if(anyNA(pi_k)){
    print("alpha")
    print(alpha0)
    print("beta")
    print(beta_k)
    print("Shape")
    print(alpha0*beta_k)
    print("scale")
    print(sapply(seq_along(beta_k), function(i) alpha0*(1-sum(beta_k[1:i]))))
    stop("Error")
    }
  return(pi_k)
}

