#' Identifies the correct clusters labels, in any dimension,
#' when cluster parameters and global parameters are matched.
#'
#' @param array The array containing matching indexes.
#' @param dpObj A hierarchical dirichletprocess object.
#'
#' @return The array containing the correct matching indexes
#' @export


true_cluster_labels <- function(array, dpObj) {
  data <- dpObj$indDP[[1]]$data

  if (ncol(data) == 1) {
    return(array)
  } else {

    delta <- ncol(data)
    k <- length(array)/delta
    array <- array[delta*seq_len(k)]
    array <- array/delta
    return(array)
  }
}
