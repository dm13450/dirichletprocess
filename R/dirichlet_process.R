#' Create a Dirichlet Process Mixture
#'
#' Convenience front-end for the package's built-in ordinary Dirichlet process
#' mixture models. This function normalises \code{family} and dispatches to the
#' corresponding model-specific constructor.
#'
#' \code{DirichletProcess()} is deliberately a thin wrapper, not a unified
#' hyperparameter interface. The \code{...} arguments are passed unchanged to
#' the selected constructor, so the model-specific help pages remain the
#' detailed reference for prior, tuning, and family-specific arguments.
#'
#' Supported ordinary families:
#'
#' \tabular{llll}{
#' \strong{family} \tab \strong{model} \tab \strong{constructor} \tab \strong{required extras}\cr
#' \code{gaussian} \tab univariate Gaussian mixture \tab \code{\link{DirichletProcessGaussian}()} \tab none\cr
#' \code{beta} \tab bounded Beta mixture \tab \code{\link{DirichletProcessBeta}()} \tab none; \code{maxY = 1} by default\cr
#' \code{beta2} \tab boundary-mass Beta variant \tab \code{\link{DirichletProcessBeta2}()} \tab \code{maxY}\cr
#' \code{weibull} \tab Weibull mixture \tab \code{\link{DirichletProcessWeibull}()} \tab \code{g0Priors}\cr
#' \code{mvnormal} \tab multivariate normal mixture \tab \code{\link{DirichletProcessMvnormal}()} \tab matrix data, observations in rows\cr
#' \code{mvnormal2} \tab semi-conjugate multivariate normal mixture \tab \code{\link{DirichletProcessMvnormal2}()} \tab matrix data, observations in rows\cr
#' \code{exponential} \tab exponential mixture \tab \code{\link{DirichletProcessExponential}()} \tab none\cr
#' \code{gaussian_fixed_variance} \tab Gaussian mixture with fixed variance \tab \code{\link{DirichletProcessGaussianFixedVariance}()} \tab \code{sigma}\cr
#' }
#'
#' Common aliases such as \code{"normal"}, \code{"mvn"}, and
#' \code{"normal_fixed_variance"} are accepted. Partial matching is not used.
#'
#' Hierarchical models use \code{\link{DirichletProcessHierarchicalBeta}()} or
#' \code{\link{DirichletProcessHierarchicalMvnormal2}()}. Hidden Markov or
#' Markov models use \code{\link{DirichletHMMCreate}()}. Custom kernels use
#' \code{\link{MixingDistribution}()} with
#' \code{\link{DirichletProcessCreate}()}.
#'
#' @param y Data for the ordinary Dirichlet process mixture.
#' @param family Character string selecting the ordinary mixture family.
#' @param ... Arguments passed unchanged to the selected model-specific
#'   constructor.
#' @return A \code{dirichletprocess} object from the selected ordinary
#'   constructor.
#' @seealso \code{\link{DirichletProcessGaussian}},
#'   \code{\link{DirichletProcessBeta}},
#'   \code{\link{DirichletProcessBeta2}},
#'   \code{\link{DirichletProcessWeibull}},
#'   \code{\link{DirichletProcessMvnormal}},
#'   \code{\link{DirichletProcessMvnormal2}},
#'   \code{\link{DirichletProcessExponential}},
#'   \code{\link{DirichletProcessGaussianFixedVariance}},
#'   \code{\link{MixingDistribution}},
#'   \code{\link{DirichletProcessCreate}}
#' @examples
#' y <- rnorm(10)
#' dp <- DirichletProcess(y, family = "gaussian")
#'
#' y_beta <- c(0.15, 0.33, 0.68, 0.91)
#' dp_beta <- DirichletProcess(y_beta, family = "beta", maxY = 1)
#'
#' x_mvn <- cbind(rnorm(8), rnorm(8))
#' dp_mvn <- DirichletProcess(x_mvn, family = "mvnormal")
#'
#' dp_fixed <- DirichletProcess(y, family = "gaussian_fixed_variance", sigma = 1)
#' @name DirichletProcess
#' @export
DirichletProcess <- function(y, family = "gaussian", ...) {
  canonical_family <- normalise_dirichletprocess_family(family)
  dots <- list(...)

  if (identical(canonical_family, "beta2")) {
    require_dirichletprocess_family_argument(dots, "maxY", canonical_family)
  }
  if (identical(canonical_family, "weibull")) {
    require_dirichletprocess_family_argument(dots, "g0Priors", canonical_family)
  }
  if (identical(canonical_family, "gaussian_fixed_variance")) {
    require_dirichletprocess_family_argument(dots, "sigma", canonical_family)
  }

  switch(canonical_family,
         gaussian = DirichletProcessGaussian(y, ...),
         beta = DirichletProcessBeta(y, ...),
         beta2 = DirichletProcessBeta2(y, ...),
         weibull = DirichletProcessWeibull(y, ...),
         mvnormal = DirichletProcessMvnormal(y, ...),
         mvnormal2 = DirichletProcessMvnormal2(y, ...),
         exponential = DirichletProcessExponential(y, ...),
         gaussian_fixed_variance = DirichletProcessGaussianFixedVariance(y, ...))
}

normalise_dirichletprocess_family <- function(family) {
  if (length(family) != 1L || is.na(family)) {
    stop("`family` must be a single non-missing character value.",
         call. = FALSE)
  }

  original_family <- as.character(family)
  family_key <- trimws(original_family)

  if (!nzchar(family_key)) {
    stop("`family` must be a single non-empty character value.",
         call. = FALSE)
  }

  family_key <- gsub("([[:lower:]])([[:upper:]])", "\\1_\\2", family_key)
  family_key <- tolower(family_key)
  family_key <- gsub("[[:space:]-]+", "_", family_key)
  family_key <- gsub("_+", "_", family_key)
  family_key <- gsub("^_|_$", "", family_key)

  aliases <- c(
    gaussian = "gaussian",
    normal = "gaussian",
    univariate_gaussian = "gaussian",
    normal_inverse_gamma = "gaussian",
    beta = "beta",
    bounded_beta = "beta",
    beta_mean_precision = "beta",
    beta2 = "beta2",
    boundary_beta = "beta2",
    beta_boundary = "beta2",
    beta_pareto = "beta2",
    weibull = "weibull",
    survival_weibull = "weibull",
    mvnormal = "mvnormal",
    mvn = "mvnormal",
    multivariate_normal = "mvnormal",
    normal_wishart = "mvnormal",
    mvnormal2 = "mvnormal2",
    mvn2 = "mvnormal2",
    semi_conjugate_mvnormal = "mvnormal2",
    semi_conjugate_mvn = "mvnormal2",
    exponential = "exponential",
    gamma_exponential = "exponential",
    gaussian_fixed_variance = "gaussian_fixed_variance",
    normal_fixed_variance = "gaussian_fixed_variance",
    fixed_variance_gaussian = "gaussian_fixed_variance"
  )

  canonical_family <- unname(aliases[family_key])

  if (is.na(canonical_family)) {
    stop(
      paste0(
        "Unsupported Dirichlet process family '", original_family, "'. ",
        "Supported ordinary families are: gaussian, beta, beta2, weibull, ",
        "mvnormal, mvnormal2, exponential, gaussian_fixed_variance. ",
        "For hierarchical models use DirichletProcessHierarchicalBeta() or ",
        "DirichletProcessHierarchicalMvnormal2(); for HMM models use ",
        "DirichletHMMCreate(); for custom kernels use MixingDistribution() ",
        "with DirichletProcessCreate()."
      ),
      call. = FALSE
    )
  }

  canonical_family
}

require_dirichletprocess_family_argument <- function(dots, argument, family) {
  dot_names <- names(dots)
  has_named_argument <- !is.null(dot_names) && argument %in% dot_names
  has_positional_argument <- if (is.null(dot_names)) {
    length(dots) > 0L
  } else {
    any(!nzchar(dot_names))
  }

  if (!has_named_argument && !has_positional_argument) {
    stop("family '", family, "' requires `", argument, "`.",
         call. = FALSE)
  }

  invisible(TRUE)
}
