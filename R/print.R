

#' Print the Dirichlet process object
#'
#' For a univariate Dirichlet process plot the density of the data with the
#' posterior distribution and credible intervals overlayed. For multivariate data
#' the first two columns of the data are plotted with the data points coloured by their
#' cluster labels.
#'
#' @param x Dirichlet Process Object to print.
#' @export
print.dirichletprocess <- function(x, verbose = FALSE, digits = 2) {

  cat("Dirichlet process object run for", length(x$alphaChain), "iterations.\n")


  n_clust <- sapply(x$labelsChain, function(x) length(unique(x)))

  mysprint <- function(num) sprintf(paste0("%.", digits, "f"), num)

  # Empty output
  model_print <- data.frame(. = c(x$mixingDistribution$distribution,
                                  paste(x$mixingDistribution$priorParameters, collapse = ", "),
                                  x$mixingDistribution$conjugate,
                                  x$n), stringsAsFactors = FALSE)
  rownames(model_print) <- c("Mixing distribution",
                             "Prior parameters",
                             "Conjugacy",
                             "Sample size")

  post_print <- data.frame(. = mean(n_clust), stringsAsFactors = FALSE)

  rownames(post_print) <- c("Mean number of clusters")


  if ("alphaChain" %in% names(x)) {
    ac_df <- data.frame(. = mysprint(median(x$alphaChain)), stringsAsFactors = FALSE)
    rownames(ac_df) <- "Median alpha"
    post_print <- rbind(post_print, ac_df)
  }


  if (verbose) {

    n_params <- length(x$clusterParametersChain[[1]])
    meani <- numeric(n_params)
    sdi   <- numeric(n_params)

    for (i in seq_len(n_params)) {
      param_i <- unlist(sapply(x$clusterParametersChain, function(x) x[[i]]))
      meani[i] <- mean(param_i)
      sdi[i]   <- sd(param_i)
    }

    param_dat <- data.frame(. = paste0(mysprint(meani), " (",
                                       mysprint(sdi), ")"),
                            stringsAsFactors = FALSE)
    rownames(param_dat) <- paste("Overall mean (sd) parameter", 1:n_params, " ")

    post_print <- rbind(post_print, param_dat)
  }

  bars <- "--------"

  total_print <- rbind(model_print,
                       " " = "",
                       post_print)

 colnames(total_print) <- NULL
 rownames(total_print) <- paste0("  ", rownames(total_print))

 print(total_print)

 cat("\n")

 invisible(total_print)
}




