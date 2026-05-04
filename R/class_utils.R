class_without_list <- function(x) {
  cls <- if (is.character(x)) x else class(x)
  cls[cls != "list"]
}

mixing_distribution_classes <- function(md_obj) {
  class_without_list(md_obj)
}

mixing_distribution_primary_class <- function(md_obj) {
  cls <- mixing_distribution_classes(md_obj)
  cls <- cls[!cls %in% c("MixingDistribution", "conjugate", "nonconjugate", "hierarchical")]

  if (length(cls) == 0L) {
    return(NA_character_)
  }

  cls[[1]]
}

mixing_distribution_display_class <- function(md_obj) {
  primary <- mixing_distribution_primary_class(md_obj)

  if (!is.na(primary)) {
    return(primary)
  }

  cls <- mixing_distribution_classes(md_obj)
  if (length(cls) == 0L) {
    return(NA_character_)
  }

  cls[[1]]
}

mixing_distribution_supported_cpp_class <- function(md_obj) {
  primary <- mixing_distribution_primary_class(md_obj)
  supported <- c("normal_inverse_gamma", "normal", "normalFixedVariance", "beta",
                 "beta2", "weibull", "exponential", "mvnormal", "mvnormal2")

  if (is.na(primary) || !primary %in% supported) {
    return(NULL)
  }

  if (identical(primary, "mvnormal")) {
    cov_model <- if (is.null(md_obj$priorParameters$covModel)) {
      "FULL"
    } else {
      as.character(md_obj$priorParameters$covModel)
    }

    if (!identical(cov_model, "FULL")) {
      return(NULL)
    }
  }

  primary
}

mixing_distribution_method_class <- function(md_obj, generic) {
  cls <- mixing_distribution_classes(md_obj)
  cls <- cls[!cls %in% c("MixingDistribution", "conjugate", "nonconjugate", "hierarchical")]

  for (cl in cls) {
    if (!is.null(utils::getS3method(generic, cl, optional = TRUE))) {
      return(cl)
    }
  }

  NULL
}
