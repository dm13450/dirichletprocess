#' Summarise retained posterior draws from an ordinary Dirichlet process fit.
#'
#' Uses the retained MCMC samples stored on an ordinary fitted Dirichlet
#' process object to evaluate \code{PosteriorFunction(dpobj, ind = i)} across
#' saved iterations and return pointwise posterior summaries.
#'
#' @param dpobj A fitted ordinary Dirichlet process object with stored samples.
#' @param x Evaluation points. For univariate objects this is typically a
#'   numeric grid. For multivariate objects, rows of a matrix or data frame are
#'   treated as evaluation points.
#' @param burnin Non-negative integer burn-in threshold applied to
#'   \code{storedIterations}. Stored iterations less than or equal to
#'   \code{burnin} are dropped.
#' @param thinning Positive integer summary-time thinning applied to the
#'   retained stored iterations after burn-in.
#' @param level Pointwise credible interval level. Must lie strictly between
#'   0 and 1.
#' @return A data frame with columns \code{x}, \code{Mean}, \code{Median},
#'   \code{Lower}, and \code{Upper}.
#' @details This is the recommended retained-sample posterior summary helper
#'   for ordinary non-hierarchical Dirichlet process objects. It errors when no
#'   stored samples are available, when burn-in removes all stored samples, or
#'   when fewer than two retained samples remain after summary-time thinning.
#'
#'   Top-level hierarchical HDP objects are intentionally unsupported in the
#'   rewritten live path. For a local restaurant object, use
#'   \code{PosteriorSummary(dplist$indDP[[j]], x, ...)} instead.
#' @examples
#' dp <- DirichletProcessGaussian(rnorm(30))
#' dp <- Fit(dp, 12, progressBar = FALSE, thinning = 2)
#' PosteriorSummary(dp, seq(-2, 2, length.out = 5))
#' @export
PosteriorSummary <- function(dpobj, x, burnin = 0, thinning = 1, level = 0.95) {
  if (inherits(dpobj, "hierarchical") && "indDP" %in% names(dpobj)) {
    stop(
      "PosteriorSummary() is not defined for top-level hierarchical HDP objects in the rewritten R path. Use PosteriorSummary(dpobj$indDP[[j]], x, ...) for a restaurant-specific posterior summary.",
      call. = FALSE
    )
  }

  if (!inherits(dpobj, "dirichletprocess")) {
    stop("dpobj must be a dirichletprocess object.", call. = FALSE)
  }

  if (length(burnin) != 1L || is.na(burnin) || !is.numeric(burnin) ||
      burnin < 0 || burnin != as.integer(burnin)) {
    stop("'burnin' must be a non-negative integer.", call. = FALSE)
  }

  if (length(thinning) != 1L || is.na(thinning) || !is.numeric(thinning) ||
      thinning < 1 || thinning != as.integer(thinning)) {
    stop("'thinning' must be a positive integer.", call. = FALSE)
  }

  if (length(level) != 1L || is.na(level) || !is.numeric(level) ||
      level <= 0 || level >= 1) {
    stop("'level' must be a single numeric value strictly between 0 and 1.", call. = FALSE)
  }

  storage <- dp_existing_sample_storage(dpobj)

  if (length(storage$storedIterations) == 0L) {
    stop(
      "PosteriorSummary() requires stored ordinary DP samples. Fit with storeSamples = TRUE before calling PosteriorSummary().",
      call. = FALSE
    )
  }

  retained_positions <- which(storage$storedIterations > as.integer(burnin))

  if (length(retained_positions) == 0L) {
    stop(
      "PosteriorSummary() has no stored samples remaining after burnin.",
      call. = FALSE
    )
  }

  retained_positions <- retained_positions[seq.int(1L, length(retained_positions),
                                                   by = as.integer(thinning))]

  if (length(retained_positions) < 2L) {
    stop(
      "PosteriorSummary() requires at least two retained stored samples after burnin/thinning.",
      call. = FALSE
    )
  }

  n_eval <- if (is.matrix(x) || is.data.frame(x)) nrow(as.matrix(x)) else length(x)
  posterior_draws <- vapply(
    retained_positions,
    function(ind) PosteriorFunction(dpobj, ind = ind)(x),
    numeric(n_eval)
  )

  if (!is.matrix(posterior_draws)) {
    posterior_draws <- matrix(posterior_draws, nrow = n_eval)
  }

  tail_prob <- (1 - level) / 2
  posterior_mean <- rowMeans(posterior_draws)
  posterior_median <- apply(posterior_draws, 1, stats::median)
  posterior_lower <- apply(posterior_draws, 1, stats::quantile,
                           probs = tail_prob, names = FALSE)
  posterior_upper <- apply(posterior_draws, 1, stats::quantile,
                           probs = 1 - tail_prob, names = FALSE)

  if (is.matrix(x) || is.data.frame(x)) {
    x_column <- I(asplit(as.matrix(x), 1))
  } else {
    x_column <- x
  }

  data.frame(
    x = x_column,
    Mean = posterior_mean,
    Median = posterior_median,
    Lower = posterior_lower,
    Upper = posterior_upper
  )
}
