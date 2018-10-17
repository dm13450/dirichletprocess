#' Print the Dirichlet process object
#'
#' Print a Dirichlet process object. This will print some basic information
#' about the \code{dirichletprocess} object.
#'
#'
#' @param x Dirichlet Process Object to print.
#' @param param_summary If \code{TRUE}, print the overall averages of each
#'   parameter of the model. Note that this averages over all clusters and over
#'   all iterations, so it will only give a loose sense of the resulting DPM
#'   model.
#' @param digits Integer; Number of digits to display.
#' @param ... Further arguments passed to or from other methods.
#' @export
#'
#' @examples
#' dp <- Fit(DirichletProcessGaussian(rnorm(10)), 100)
#' dp
#'
print.dirichletprocess <- function(x, param_summary = FALSE, digits = 2, ...) {


  # Formatting function.
  mysprint <- function(num) sprintf(paste0("%.", digits, "f"), num)

  # Main title.
  burntxt <- ifelse(is.null(x$n_burned), "", paste0(" (plus ", x$n_burned, " burned)"))
  cat("Dirichlet process object run for ", length(x$labelsChain),
      " iterations", burntxt, ".\n", sep = "")


  # Model info that always exists.
  model_print <- data.frame(. = c(x$mixingDistribution$distribution,
                                  paste(x$mixingDistribution$priorParameters, collapse = ", "),
                                  x$mixingDistribution$conjugate,
                                  x$n), stringsAsFactors = FALSE)
  rownames(model_print) <- c("Mixing distribution",
                             "Base measure parameters ",
                             "Conjugacy",
                             "Sample size")

  # Check if there is an alpha prior to add.
  if ("alphaPriorParameters" %in% names(x)) {
    apr_df <- data.frame(. = paste(x$alphaPriorParameters, collapse = ", "), stringsAsFactors = FALSE)
    rownames(apr_df) <- "Alpha Prior parameters"
    model_print <- rbind(model_print[1:2, , drop = FALSE],
                         apr_df,
                         model_print[3:4, , drop = FALSE])
  }


  # Spacing only.
  post_print <- data.frame(. = "", stringsAsFactors = FALSE)
  rownames(post_print) <- " "

  if ("labelsChain" %in% names(x)) {

    n_clust <- sapply(x$labelsChain, function(x) length(unique(x)))
    nc_df <- data.frame(. = mysprint(mean(n_clust)), stringsAsFactors = FALSE)
    rownames(nc_df) <- "Mean number of clusters"
    post_print <- rbind(post_print, nc_df)
  }


  if ("alphaChain" %in% names(x)) {
    ac_df <- data.frame(. = mysprint(stats::median(x$alphaChain)), stringsAsFactors = FALSE)
    rownames(ac_df) <- "Median alpha"
    post_print <- rbind(post_print, ac_df)
  }


  if (param_summary) {

    # Check whether there is a chain; skips this for dp obj that have not yet
    # been fit.
    if ("clusterParametersChain" %in% names(x)) {

      # Get averages over all clusters and iterations for the parameters.
      n_params <- length(x$clusterParametersChain[[1]])
      meani <- numeric(n_params)
      sdi   <- numeric(n_params)

      for (i in seq_len(n_params)) {
        param_i <- unlist(sapply(x$clusterParametersChain, function(x) x[[i]]))
        meani[i] <- mean(param_i)
        sdi[i]   <- stats::sd(param_i)
      }

      param_dat <- data.frame(. = paste0(mysprint(meani), " (",
                                         mysprint(sdi), ")"),
                              stringsAsFactors = FALSE)
      rownames(param_dat) <- paste("Overall mean (sd) parameter", 1:n_params, " ")

      post_print <- rbind(post_print, param_dat)
    }
  }

  bars <- "--------"

  total_print <- rbind(model_print,

                       post_print)

 colnames(total_print) <- NULL
 rownames(total_print) <- paste0("  ", rownames(total_print))

 print(total_print)

 cat("\n")

 invisible(total_print)
}




