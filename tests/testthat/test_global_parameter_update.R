context("HDP Global Parameter Update")

expect_hdp_global_state_consistent <- function(dpobjlist) {
  total_tables <- 0L

  for (group_index in seq_along(dpobjlist$indDP)) {
    local_dp <- dpobjlist$indDP[[group_index]]
    local_dishes <- dpobjlist$tableDishLabels[[group_index]]

    expect_equal(length(local_dishes), local_dp$numberClusters)
    expect_equal(length(local_dp$pointsPerCluster), local_dp$numberClusters)
    expect_equal(sum(local_dp$pointsPerCluster), local_dp$n)

    if (local_dp$numberClusters > 0L) {
      expect_true(all(local_dishes %in% seq_len(length(dpobjlist$dishTableCounts))))
      for (component_index in seq_along(local_dp$clusterParameters)) {
        expect_equal(local_dp$clusterParameters[[component_index]],
                     dpobjlist$globalParameters[[component_index]][, , local_dishes, drop = FALSE])
      }
    }

    total_tables <- total_tables + local_dp$numberClusters
  }

  expect_equal(sum(dpobjlist$dishTableCounts), total_tables)
}

test_that("GlobalParameterUpdate maintains explicit table/dish invariants for hierarchical beta", {
  set.seed(31)
  dataTest <- list(rbeta(10, 2, 5), rbeta(10, 3, 4))
  dp <- DirichletProcessHierarchicalBeta(dataTest, 1)

  dp <- ClusterComponentUpdate(dp)
  dp <- GlobalParameterUpdate(dp)

  expect_hdp_global_state_consistent(dp)
  expect_true(all(vapply(dp$indDP,
                         function(x) identical(x$mixingDistribution$theta_k, dp$globalParameters),
                         logical(1))))
})

test_that("GlobalParameterUpdate maintains explicit table/dish invariants for hierarchical mvnormal2", {
  set.seed(31)
  dataTest <- list(
    mvtnorm::rmvnorm(12, c(0, 0), diag(2)),
    mvtnorm::rmvnorm(12, c(1, -1), diag(2))
  )
  dp <- DirichletProcessHierarchicalMvnormal2(dataTest)

  dp <- ClusterComponentUpdate(dp)
  dp <- GlobalParameterUpdate(dp)

  expect_hdp_global_state_consistent(dp)
  expect_true(all(vapply(dp$indDP,
                         function(x) identical(x$mixingDistribution$theta_k, dp$globalParameters),
                         logical(1))))
})

test_that("GlobalParameterUpdate retires and recreates a dish cleanly for a single-table hierarchy", {
  set.seed(35)
  dp <- DirichletProcessHierarchicalBeta(list(rbeta(5, 2, 5)), 1)

  dp <- GlobalParameterUpdate(dp)

  expect_equal(dp$tableDishLabels[[1]], 1L)
  expect_equal(dp$dishTableCounts, 1L)
  expect_equal(dim(dp$globalParameters[[1]])[3], 1)
  expect_equal(dp$indDP[[1]]$clusterParameters[[1]],
               dp$globalParameters[[1]][, , 1, drop = FALSE])
  expect_equal(dp$indDP[[1]]$clusterParameters[[2]],
               dp$globalParameters[[2]][, , 1, drop = FALSE])
})

test_that("GlobalParameterUpdate can retire a dish and maintain cross-group sharing consistency", {
  set.seed(32)
  unassign_table <- getFromNamespace("hdp_unassign_table_from_dish", "dirichletprocess")
  assign_table <- getFromNamespace("hdp_assign_table_to_dish", "dirichletprocess")
  refresh_atoms <- getFromNamespace("hdp_refresh_global_atoms", "dirichletprocess")

  dp <- DirichletProcessHierarchicalBeta(list(rbeta(1, 2, 5), rbeta(1, 2, 5)), 1)

  dp <- unassign_table(dp, 2L, 1L)
  dp <- assign_table(dp, 2L, 1L, 1L)
  dp <- refresh_atoms(dp)

  expect_equal(dp$tableDishLabels[[1]], 1L)
  expect_equal(dp$tableDishLabels[[2]], 1L)
  expect_equal(dp$dishTableCounts, 2L)
  expect_equal(dim(dp$globalParameters[[1]])[3], 1)
  expect_equal(dp$indDP[[1]]$clusterParameters[[1]],
               dp$indDP[[2]]$clusterParameters[[1]])
  expect_equal(dp$indDP[[1]]$clusterParameters[[2]],
               dp$indDP[[2]]$clusterParameters[[2]])
})

test_that("GlobalParameterUpdate uses pooled beta dish data with the current dish warm start", {
  set.seed(33)
  unassign_table <- getFromNamespace("hdp_unassign_table_from_dish", "dirichletprocess")
  assign_table <- getFromNamespace("hdp_assign_table_to_dish", "dirichletprocess")
  refresh_atoms <- getFromNamespace("hdp_refresh_global_atoms", "dirichletprocess")

  dp <- DirichletProcessHierarchicalBeta(list(rbeta(3, 2, 5), rbeta(3, 2, 5)), 1)
  dp <- unassign_table(dp, 2L, 1L)
  dp <- assign_table(dp, 2L, 1L, 1L, refresh = TRUE)

  pooled_data <- matrix(c(dp$indDP[[1]]$data, dp$indDP[[2]]$data), ncol = 1)
  current_param <- list(
    mu = dp$globalParameters$mu[, , 1, drop = FALSE],
    nu = dp$globalParameters$nu[, , 1, drop = FALSE]
  )

  set.seed(404)
  expected <- PosteriorDraw(dp$indDP[[1]]$mixingDistribution,
                            pooled_data,
                            n = dp$indDP[[1]]$mhDraws,
                            start_pos = current_param)
  expected_last <- list(
    mu = expected$mu[, , dp$indDP[[1]]$mhDraws, drop = FALSE],
    nu = expected$nu[, , dp$indDP[[1]]$mhDraws, drop = FALSE]
  )

  set.seed(404)
  dp <- refresh_atoms(dp)

  expect_equal(dp$globalParameters$mu[, , 1, drop = FALSE], expected_last$mu)
  expect_equal(dp$globalParameters$nu[, , 1, drop = FALSE], expected_last$nu)
})

test_that("GlobalParameterUpdate uses pooled mvnormal2 dish data", {
  set.seed(34)
  unassign_table <- getFromNamespace("hdp_unassign_table_from_dish", "dirichletprocess")
  assign_table <- getFromNamespace("hdp_assign_table_to_dish", "dirichletprocess")
  refresh_atoms <- getFromNamespace("hdp_refresh_global_atoms", "dirichletprocess")

  dataTest <- list(
    mvtnorm::rmvnorm(2, c(0, 0), diag(2)),
    mvtnorm::rmvnorm(2, c(0, 0), diag(2))
  )
  dp <- DirichletProcessHierarchicalMvnormal2(dataTest)
  dp <- unassign_table(dp, 2L, 1L)
  dp <- assign_table(dp, 2L, 1L, 1L, refresh = TRUE)

  pooled_data <- rbind(dp$indDP[[1]]$data, dp$indDP[[2]]$data)
  current_param <- list(
    mu = dp$globalParameters$mu[, , 1, drop = FALSE],
    sig = dp$globalParameters$sig[, , 1, drop = FALSE]
  )

  set.seed(405)
  expected <- PosteriorDraw(dp$indDP[[1]]$mixingDistribution,
                            pooled_data,
                            n = 1L,
                            start_pos = current_param)

  set.seed(405)
  dp <- refresh_atoms(dp)

  expect_equal(dp$globalParameters$mu[, , 1, drop = FALSE], expected$mu[, , 1, drop = FALSE])
  expect_equal(dp$globalParameters$sig[, , 1, drop = FALSE], expected$sig[, , 1, drop = FALSE])
})
