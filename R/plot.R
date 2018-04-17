#' Plot the Dirichlet process object
#'
#' For a univariate Dirichlet process plot the density of the data with the
#' posterior distribution and credible intervals overlayed. For multivariate data
#' the first two columns of the data are plotted with the data points coloured by their
#' cluster labels.
#'
#' @param x Dirichlet Process Object to plot
#' @param likelihood Logical, indicating whether to plot the likelihood from the dpobj.
#' @param single Logical, indicating whether to draw the posterior from the last iteration or use the full cluster sequence.
#' @param y Consistency for generic.
#' @param ... Graphical arguments
#' @return A ggplot object
#' @export
plot.dirichletprocess <- function(x, likelihood = FALSE, single = TRUE, y=NULL,...) {

  plot_dirichletprocess(x, likelihood, single)

}



