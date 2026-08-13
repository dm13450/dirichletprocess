// src/Benchmarking.cpp
#include "Benchmarking.h"
#include "RcppConversions.h"
#include "MemoryProfiling.h"
#include "NormalDistribution.h"

// Platform-specific includes for memory tracking
#ifdef _WIN32
#include <windows.h>
#include <psapi.h>
#endif

#ifdef __APPLE__
#include <mach/mach.h>
#include <sys/resource.h>
#endif

#ifdef __linux__
#include <sys/resource.h>
#include <unistd.h>
#endif

namespace dp {

// Internal implementation functions (NO [[Rcpp::export]] here)
size_t current_memory_usage_impl() {
#ifdef _WIN32
  // Windows implementation
  PROCESS_MEMORY_COUNTERS_EX pmc;
  if (GetProcessMemoryInfo(GetCurrentProcess(), (PROCESS_MEMORY_COUNTERS*)&pmc, sizeof(pmc))) {
    return pmc.WorkingSetSize;
  }
  return 0;
#elif defined(__APPLE__) || defined(__linux__)
  // Mac/Linux implementation
  struct rusage usage;
  if (getrusage(RUSAGE_SELF, &usage) == 0) {
#ifdef __APPLE__
    return usage.ru_maxrss; // macOS gives bytes
#else
    return usage.ru_maxrss * 1024; // Linux gives KB
#endif
  }
  return 0;
#else
  // Fallback for unsupported platforms
  return 0;
#endif
}

// Internal implementation (NO [[Rcpp::export]] here)
Rcpp::List benchmark_cpp_components_impl(const Rcpp::List& dpObj,
                                         const Rcpp::StringVector& components,
                                         int times) {
  Rcpp::List results;

  try {
    // For now, implement a simple benchmark framework
    // This will be expanded once the core classes are implemented

    for (int i = 0; i < components.size(); i++) {
      std::string component = Rcpp::as<std::string>(components[i]);
      double time_ms = 0.0;

      if (component == "clusterComponentUpdate") {
        // Placeholder - implement when DP classes are ready
        time_ms = 1.0; // Mock timing
      } else if (component == "clusterParameterUpdate") {
        // Placeholder - implement when DP classes are ready
        time_ms = 2.0; // Mock timing
      } else if (component == "updateAlpha") {
        // Placeholder - implement when DP classes are ready
        time_ms = 0.5; // Mock timing
      } else if (component == "likelihood") {
        // Implement likelihood benchmarking for Normal distribution
        std::string distType = getDistributionType(dpObj);

        if (distType == "normal") {
          // Simple benchmark for normal likelihood
          Rcpp::NumericVector x = Rcpp::rnorm(100);
          Rcpp::List theta = Rcpp::List::create(
            Rcpp::Named("mu") = 0.0,
            Rcpp::Named("sigma") = 1.0
          );

          time_ms = benchmark_function([&]() {
            // Mock likelihood calculation
            for (int j = 0; j < x.size(); j++) {
              R::dnorm(x[j], 0.0, 1.0, false);
            }
          }, times);
        } else {
          Rcpp::warning("Unsupported distribution type for likelihood benchmarking: " + distType);
        }
      } else {
        Rcpp::warning("Unknown component: " + component);
        continue;
      }

      results[component] = time_ms;
    }

  } catch (std::exception& e) {
    Rcpp::stop("Error in C++ benchmarking: " + std::string(e.what()));
  }

  return results;
}

} // namespace dp

// Exported wrapper functions (ONLY these have [[Rcpp::export]])

// [[Rcpp::export]]
size_t current_memory_usage() {
  return dp::current_memory_usage_impl();
}

// [[Rcpp::export]]
Rcpp::List benchmark_cpp_components_impl(const Rcpp::List& dpObj,
                                         const Rcpp::StringVector& components,
                                         int times) {
  return dp::benchmark_cpp_components_impl(dpObj, components, times);
}

// [[Rcpp::export]]
Rcpp::List benchmark_cpp_components(const Rcpp::List& dpObj,
                                    const Rcpp::StringVector& components,
                                    int times) {
  return dp::benchmark_cpp_components_impl(dpObj, components, times);
}
