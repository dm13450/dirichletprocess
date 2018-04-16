#' Plot the Dirichlet Process
#'
#' @param x Dirichlet Process Object to plot
#' @param likelihood Logical, indicating whether to plot the likelihood from the dpobj.
#' @param single Logical, indicating whether to draw the posterior from the last iteration or use the full cluster sequence.
#' @param y Consistency for generic.
#' @param ... Graphical arguments
#' @export
plot.dirichletprocess <- function(x, likelihood = FALSE, single = TRUE, y=NULL,...) {

  plot_dirichletprocess(x, likelihood, single)

}



