#' Plot the Dirichlet process object
#'
#' For a univariate Dirichlet process plot the density of the data with the
#' posterior distribution and, when stored MCMC samples are available,
#' \code{PosteriorSummary()}-based pointwise credible intervals overlayed. If
#' no usable stored samples are available, the current fitted curve is shown
#' without intervals and a message explains why. Top-level hierarchical HDP
#' objects are intentionally unsupported in the rewritten live path; plot a
#' local restaurant object such as \code{dplist$indDP[[j]]} instead.
#'
#' For multivariate data the first two columns of the data are plotted with the
#' data points coloured by their cluster labels. The additional arguments are
#' not used for multivariate data.
#'
#' @param x Dirichlet Process Object to plot
#' @param likelihood Logical, indicating whether to plot the likelihood from the
#'   dpobj.
#' @param single Retained for backward compatibility.
#' @param data_method A string containing either "density" (default),
#'   "hist"/"histogram", or "none". Data is plotted according to this method.
#' @param data_fill Passed to `fill` in the data geom, for example a color.
#'   Defaults to "black".
#' @param data_bw Bandwith to be passed either as the binwidth of
#'   \code{geom_histogram}, or as the bw of \code{geom_density}.
#' @param ci_size Numeric tail probability for the pointwise interval.
#'   Defaults to `.05`, giving a `0.95` interval when stored samples are
#'   available.
#' @param xgrid_pts Integer, the number of points on the x-axis to evaluate.
#' @param quant_pts Retained for backward compatibility.
#' @param xlim Default NA. If a vector of length two, the limits on the x-axis
#'   of the plot. If \code{NA} (default), the limits will be automatically
#'   chosen.
#' @param ... Further arguments, currently ignored.
#' @return A ggplot object.
#' @export
#'
#' @examples
#' dp <- DirichletProcessGaussian(c(rnorm(50, 2, .2), rnorm(60)))
#' dp <- Fit(dp, 100, progressBar = FALSE)
#' plot(dp)
#'
#' plot(dp, likelihood = TRUE, data_method = "hist",
#'      data_fill = rgb(.5, .5, .8, .6), data_bw = .3)
#'
plot.dirichletprocess <- function(x, ...) {

  plot_dirichletprocess(x, ...)

}
