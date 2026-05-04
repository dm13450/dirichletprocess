# R/zzz.R (package startup)

.onLoad <- function(libname, pkgname) {
  # Set default options
  options(
    dirichletprocesscpp.use_cpp = NULL,  # Automatic selection for supported non-hierarchical Fit() paths
    dirichletprocesscpp.cpp_debug = FALSE  # For debugging
  )
}
