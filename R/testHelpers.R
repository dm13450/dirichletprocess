#' Compare R and C++ function outputs
#'
#' @param r_func R function to test
#' @param cpp_func C++ function to test
#' @param ... Arguments to pass to both functions
#' @param tolerance Numeric tolerance for differences
#' @return Logical indicating whether outputs match
#' @keywords internal
compare_r_cpp <- function(r_func, cpp_func, ..., tolerance = 1e-10) {
  r_result <- r_func(...)
  cpp_result <- cpp_func(...)

  # Check if results have the same structure
  if (!identical(dim(r_result), dim(cpp_result))) {
    return(list(equal = FALSE, message = "Dimensions don't match"))
  }

  # Check if values are approximately equal
  max_diff <- max(abs(as.numeric(r_result) - as.numeric(cpp_result)))
  if (max_diff > tolerance) {
    return(list(equal = FALSE,
                message = sprintf("Max difference %g exceeds tolerance %g",
                                  max_diff, tolerance)))
  }

  list(equal = TRUE, message = "Results match")
}

#' Benchmark R vs C++ implementation
#'
#' @param r_func R function to benchmark
#' @param cpp_func C++ function to benchmark
#' @param ... Arguments to pass to both functions
#' @param times Number of repetitions
#' @return Benchmark results
#' @keywords internal
benchmark_r_cpp <- function(r_func, cpp_func, ..., times = 100) {
  r_time <- system.time(
    for (i in 1:times) r_result <- r_func(...)
  )

  cpp_time <- system.time(
    for (i in 1:times) cpp_result <- cpp_func(...)
  )

  list(
    r_time = r_time,
    cpp_time = cpp_time,
    speedup = r_time["elapsed"] / cpp_time["elapsed"]
  )
}
