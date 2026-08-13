// inst/include/MemoryProfiling.h
#ifndef MEMORY_PROFILING_H
#define MEMORY_PROFILING_H

#include <RcppArmadillo.h>
#include <vector>
#include <string>

namespace dp {

// Simple memory tracker
class MemoryTracker {
private:
  std::vector<size_t> allocations;
  std::vector<std::string> descriptions;

public:
  // Record an allocation
  void record(size_t bytes, const std::string& description) {
    allocations.push_back(bytes);
    descriptions.push_back(description);
  }

  // Get total allocated memory
  size_t total() const {
    size_t total = 0;
    for (size_t alloc : allocations) {
      total += alloc;
    }
    return total;
  }

  // Get summary as R data frame
  Rcpp::DataFrame summary() const {
    if (allocations.empty()) {
      return Rcpp::DataFrame::create();
    }

    Rcpp::NumericVector bytes(allocations.begin(), allocations.end());
    Rcpp::CharacterVector desc(descriptions.begin(), descriptions.end());

    return Rcpp::DataFrame::create(
      Rcpp::Named("description") = desc,
      Rcpp::Named("bytes") = bytes,
      Rcpp::Named("mb") = bytes / (1024.0 * 1024.0)
    );
  }

  // Clear all records
  void clear() {
    allocations.clear();
    descriptions.clear();
  }
};

// Global memory tracker instance (declaration only)
extern MemoryTracker g_memory_tracker;

// Function declarations (not definitions)
Rcpp::DataFrame get_memory_tracking();
void clear_memory_tracking();

} // namespace dp

// Macro to easily track allocations
#define TRACK_ALLOC(bytes, desc) dp::g_memory_tracker.record(bytes, desc)

#endif
