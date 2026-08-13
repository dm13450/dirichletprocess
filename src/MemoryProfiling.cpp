// src/MemoryProfiling.cpp
#include "MemoryProfiling.h"

namespace dp {

// Global memory tracker instance (definition)
MemoryTracker g_memory_tracker;

// Internal implementation functions
Rcpp::DataFrame get_memory_tracking_impl() {
  return g_memory_tracker.summary();
}

void clear_memory_tracking_impl() {
  g_memory_tracker.clear();
}

} // namespace dp

// Exported functions (in global namespace)
// [[Rcpp::export]]
Rcpp::DataFrame get_memory_tracking() {
  return dp::get_memory_tracking_impl();
}

// [[Rcpp::export]]
void clear_memory_tracking() {
  dp::clear_memory_tracking_impl();
}
