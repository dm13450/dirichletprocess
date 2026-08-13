# Set implementation preference helpers.
get_implementation <- function(func_name) {
  if (using_cpp()) {
    cpp_env <- get("cpp_implementations", envir = parent.env(environment()))
    if (exists(func_name, envir = cpp_env)) {
      return(get(func_name, envir = cpp_env))
    }
    warning("No C++ implementation available for ", func_name,
            ", falling back to R implementation")
  }
  # Return the R implementation (which is the default)
  get(func_name, envir = parent.env(environment()))
}
