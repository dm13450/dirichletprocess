context("HDP Phase 4 Concentration And Parameter Updates")

expect_hdp_iteration_state_consistent <- function(dpobjlist) {
  total_tables <- 0L

  for (group_index in seq_along(dpobjlist$indDP)) {
    local_dp <- dpobjlist$indDP[[group_index]]
    local_dishes <- dpobjlist$tableDishLabels[[group_index]]

    expect_equal(local_dp$numberClusters, length(local_dp$pointsPerCluster))
    expect_equal(local_dp$numberClusters, length(local_dishes))
    expect_equal(sum(local_dp$pointsPerCluster), local_dp$n)
    expect_true(is.finite(local_dp$alpha))
    expect_true(local_dp$alpha > 0)
    expect_equal(local_dp$mixingDistribution$alpha, local_dp$alpha)
    expect_equal(local_dp$mixingDistribution$gamma, dpobjlist$gamma)

    if (local_dp$numberClusters > 0L) {
      for (component_index in seq_along(local_dp$clusterParameters)) {
        expect_equal(local_dp$clusterParameters[[component_index]],
                     dpobjlist$globalParameters[[component_index]][, , local_dishes, drop = FALSE])
      }
    }

    total_tables <- total_tables + local_dp$numberClusters
  }

  expect_true(is.finite(dpobjlist$gamma))
  expect_true(dpobjlist$gamma > 0)
  expect_equal(sum(dpobjlist$dishTableCounts), total_tables)
}

test_that("UpdateAlpha.hierarchical uses true restaurant table counts", {
  update_conc <- getFromNamespace("update_concentration", "dirichletprocess")

  set.seed(51)
  dp <- DirichletProcessHierarchicalBeta(list(rbeta(2, 2, 5), rbeta(2, 3, 4)), 1)
  dp$indDP[[1]]$pointsPerCluster <- c(1, 1)
  dp$indDP[[1]]$numberClusters <- 99
  dp$indDP[[1]]$clusterLabels <- c(1L, 2L)
  dp$tableDishLabels[[1]] <- c(1L, 2L)

  set.seed(5101)
  expected_alpha <- update_conc(dp$indDP[[1]]$alpha,
                                dp$indDP[[1]]$n,
                                length(dp$indDP[[1]]$pointsPerCluster),
                                dp$indDP[[1]]$alphaPriorParameters)

  set.seed(5101)
  dp <- UpdateAlpha(dp)

  expect_equal(dp$indDP[[1]]$alpha, expected_alpha)
  expect_equal(dp$indDP[[1]]$mixingDistribution$alpha, expected_alpha)
  expect_equal(dp$indDP[[1]]$numberClusters, 2)
})

test_that("UpdateGamma uses total occupied tables and active dishes", {
  update_conc <- getFromNamespace("update_concentration", "dirichletprocess")
  update_gamma <- getFromNamespace("UpdateGamma", "dirichletprocess")

  set.seed(52)
  dp <- DirichletProcessHierarchicalBeta(list(rbeta(3, 2, 5), rbeta(3, 3, 4)), 1)
  dp$indDP[[1]]$pointsPerCluster <- c(2, 1)
  dp$indDP[[1]]$numberClusters <- 42
  dp$indDP[[2]]$pointsPerCluster <- c(3)
  dp$indDP[[2]]$numberClusters <- 99
  dp$tableDishLabels[[1]] <- c(1L, 2L)
  dp$tableDishLabels[[2]] <- 2L
  dp$dishTableCounts <- c(1L, 2L)

  set.seed(5201)
  expected_gamma <- update_conc(dp$gamma,
                                sum(vapply(dp$indDP, function(x) length(x$pointsPerCluster), integer(1))),
                                length(dp$dishTableCounts),
                                dp$gammaPriors)

  set.seed(5201)
  dp <- update_gamma(dp)

  expect_equal(dp$gamma, expected_gamma)
  expect_true(all(vapply(dp$indDP,
                         function(x) identical(x$mixingDistribution$gamma, expected_gamma),
                         logical(1))))
})

test_that("ClusterParameterUpdate.hierarchical is now a compatibility refresh, not a local redraw", {
  set.seed(53)
  dp <- DirichletProcessHierarchicalBeta(list(rbeta(6, 2, 5), rbeta(6, 3, 4)), 1)
  dp <- ClusterComponentUpdate(dp)
  dp <- GlobalParameterUpdate(dp)

  original_global <- dp$globalParameters
  dp$indDP[[1]]$clusterParameters$mu[, , 1] <- -999
  dp$indDP[[1]]$clusterParameters$nu[, , 1] <- -999

  dp <- ClusterParameterUpdate(dp)

  expect_identical(dp$globalParameters, original_global)
  expect_equal(dp$indDP[[1]]$clusterParameters$mu[, , 1, drop = FALSE],
               dp$globalParameters$mu[, , dp$tableDishLabels[[1]][1], drop = FALSE])
  expect_equal(dp$indDP[[1]]$clusterParameters$nu[, , 1, drop = FALSE],
               dp$globalParameters$nu[, , dp$tableDishLabels[[1]][1], drop = FALSE])
})

test_that("Hierarchical beta Fit(2) keeps CRF state invariants", {
  set.seed(54)
  dp <- DirichletProcessHierarchicalBeta(list(rbeta(10, 2, 5), rbeta(10, 3, 4)), 1)

  expect_no_warning({
    dp <- Fit(dp, 2, progressBar = FALSE)
  })

  expect_hdp_iteration_state_consistent(dp)
  expect_length(dp$gammaChain, 2)
  expect_equal(length(dp$globalParametersChain), 2)
  expect_equal(length(dp$indDP[[1]]$alphaChain), 2)
  expect_equal(length(dp$indDP[[1]]$clusterParametersChain), 2)
})

test_that("Hierarchical mvnormal2 Fit(2) keeps CRF state invariants", {
  set.seed(55)
  dp <- DirichletProcessHierarchicalMvnormal2(list(
    mvtnorm::rmvnorm(10, c(0, 0), diag(2)),
    mvtnorm::rmvnorm(10, c(1, -1), diag(2))
  ))

  expect_no_warning({
    dp <- Fit(dp, 2, progressBar = FALSE)
  })

  expect_hdp_iteration_state_consistent(dp)
  expect_length(dp$gammaChain, 2)
  expect_equal(length(dp$globalParametersChain), 2)
  expect_equal(length(dp$indDP[[1]]$alphaChain), 2)
  expect_equal(length(dp$indDP[[1]]$clusterParametersChain), 2)
})
