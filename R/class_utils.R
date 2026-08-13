class_without_list <- function(x) {
  cls <- if (is.character(x)) x else class(x)
  cls[cls != "list"]
}

metadata_property_classes <- function() {
  c("conjugate", "nonconjugate")
}

mixing_distribution_base_classes <- function() {
  c("list", "MixingDistribution", "mixing_distribution")
}

dp_base_classes <- function() {
  c("list", "dirichletprocess")
}

class_without_metadata <- function(x, extra = character()) {
  cls <- if (is.character(x)) x else class(x)
  cls[!cls %in% c(mixing_distribution_base_classes(),
                  dp_base_classes(),
                  metadata_property_classes(),
                  extra)]
}

object_field <- function(x, name) {
  if (!is.list(x) || is.null(x[[name]])) {
    return(NULL)
  }

  x[[name]]
}

scalar_character_or_na <- function(x) {
  if (is.null(x) || length(x) == 0L || !is.character(x)) {
    return(NA_character_)
  }

  value <- x[[1]]
  if (is.na(value) || !nzchar(value)) {
    return(NA_character_)
  }

  value
}

normalise_conjugacy <- function(x) {
  if (is.logical(x) && length(x) == 1L && !is.na(x)) {
    return(if (x) "conjugate" else "nonconjugate")
  }

  if (is.character(x)) {
    matches <- x[x %in% metadata_property_classes()]
    if (length(matches) > 0L) {
      return(matches[[1]])
    }
  }

  value <- scalar_character_or_na(x)
  if (value %in% metadata_property_classes()) {
    return(value)
  }

  NA_character_
}

mixing_distribution_classes <- function(md_obj) {
  class_without_list(md_obj)
}

mixing_distribution_kernel <- function(x) {
  distribution <- scalar_character_or_na(object_field(x, "distribution"))
  if (!is.na(distribution)) {
    return(distribution)
  }

  cls <- class_without_metadata(x, extra = c("hierarchical", "markov",
                                            "hdp", "hdp_mvnormal"))
  if (length(cls) == 0L) {
    return(NA_character_)
  }

  cls[[1]]
}

mixing_distribution_conjugacy <- function(x) {
  conjugacy <- normalise_conjugacy(object_field(x, "conjugacy"))
  if (!is.na(conjugacy)) {
    return(conjugacy)
  }

  conjugacy <- normalise_conjugacy(object_field(x, "conjugate"))
  if (!is.na(conjugacy)) {
    return(conjugacy)
  }

  cls <- class(x)
  if ("conjugate" %in% cls) {
    return("conjugate")
  }
  if ("nonconjugate" %in% cls) {
    return("nonconjugate")
  }

  NA_character_
}

dp_type <- function(x) {
  cls <- class(x)

  if ("markov" %in% cls) {
    return("markov")
  }

  if ("hdp" %in% cls || "hdp_mvnormal" %in% cls ||
      identical(object_field(x, "type"), "hierarchical_mvnormal")) {
    return("hierarchical")
  }

  if ("hierarchical" %in% cls && !is.null(object_field(x, "indDP"))) {
    return("hierarchical")
  }

  if ("dirichletprocess" %in% cls) {
    return("dirichletprocess")
  }

  NA_character_
}

dp_kernel <- function(x) {
  mixing_distribution <- object_field(x, "mixingDistribution")
  if (!is.null(mixing_distribution)) {
    return(mixing_distribution_kernel(mixing_distribution))
  }

  ind_dp <- object_field(x, "indDP")
  if (is.list(ind_dp) && length(ind_dp) > 0L) {
    kernels <- unique(vapply(ind_dp, dp_kernel, character(1)))
    kernels <- kernels[!is.na(kernels)]
    if (length(kernels) == 1L) {
      return(kernels[[1]])
    }
    return(NA_character_)
  }

  if ("hdp_mvnormal" %in% class(x) ||
      identical(object_field(x, "type"), "hierarchical_mvnormal")) {
    return("mvnormal")
  }

  cls <- class_without_metadata(x, extra = c("hierarchical", "markov",
                                            "hdp", "hdp_mvnormal"))
  if (length(cls) == 0L) {
    return(NA_character_)
  }

  cls[[1]]
}

dp_conjugacy <- function(x) {
  mixing_distribution <- object_field(x, "mixingDistribution")
  if (!is.null(mixing_distribution)) {
    return(mixing_distribution_conjugacy(mixing_distribution))
  }

  ind_dp <- object_field(x, "indDP")
  if (is.list(ind_dp) && length(ind_dp) > 0L) {
    conjugacies <- unique(vapply(ind_dp, dp_conjugacy, character(1)))
    conjugacies <- conjugacies[!is.na(conjugacies)]
    if (length(conjugacies) == 1L) {
      return(conjugacies[[1]])
    }
    return(NA_character_)
  }

  normalise_conjugacy(class(x))
}

is_conjugate_dp <- function(x) {
  identical(dp_conjugacy(x), "conjugate")
}

mixing_distribution_primary_class <- function(md_obj) {
  cls <- mixing_distribution_classes(md_obj)
  cls <- cls[!cls %in% c("MixingDistribution", "mixing_distribution",
                         metadata_property_classes(), "hierarchical")]

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
  cls <- cls[!cls %in% c("MixingDistribution", "mixing_distribution",
                         metadata_property_classes(), "hierarchical")]

  for (cl in cls) {
    if (!is.null(utils::getS3method(generic, cl, optional = TRUE))) {
      return(cl)
    }
  }

  NULL
}
