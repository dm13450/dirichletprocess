#ifndef BENCHMARKING_H
#define BENCHMARKING_H

#include <RcppArmadillo.h>
#include <chrono>
#include <vector>
#include <string>
#include "DirichletProcessBase.h"

namespace dp {

// Timer class for C++ benchmarking
class Timer {
private:
  std::chrono::high_resolution_clock::time_point start_time;

public:
  // Start timer
  void start() {
    start_time = std::chrono::high_resolution_clock::now();
  }

  // Return elapsed time in milliseconds
  double elapsed_ms() {
    auto end_time = std::chrono::high_resolution_clock::now();
    return std::chrono::duration<double, std::milli>(
        end_time - start_time).count();
  }
};

// Function to benchmark a component
template<typename F>
double benchmark_function(F func, int times = 10) {
  Timer timer;
  std::vector<double> timings(times);

  for (int i = 0; i < times; i++) {
    timer.start();
    func();
    timings[i] = timer.elapsed_ms();
  }

  // Calculate median
  std::sort(timings.begin(), timings.end());
  if (times % 2 == 0) {
    return (timings[times/2 - 1] + timings[times/2]) / 2.0;
  } else {
    return timings[times/2];
  }
}

// FUNCTION DECLARATIONS ONLY (not definitions)
// Forward declare functions that will be defined in the .cpp file
size_t current_memory_usage();
Rcpp::List benchmark_cpp_components(const Rcpp::List& dpObj,
                                    const Rcpp::StringVector& components,
                                    int times = 10);

} // namespace dp

#endif
