context("HDP Phase 2 Local Update")

expect_hdp_local_state_consistent <- function(dpobjlist) {
  total_tables <- 0L

  for (group_index in seq_along(dpobjlist$indDP)) {
    local_dp <- dpobjlist$indDP[[group_index]]
    local_dishes <- dpobjlist$tableDishLabels[[group_index]]

    expect_equal(sum(local_dp$pointsPerCluster), local_dp$n)
    expect_equal(length(local_dp$pointsPerCluster), local_dp$numberClusters)
    expect_equal(length(local_dishes), local_dp$numberClusters)

    if (local_dp$numberClusters > 0L) {
      expect_true(all(local_dp$clusterLabels %in% seq_len(local_dp$numberClusters)))
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

test_that("Hierarchical Beta local update maintains explicit table/dish state", {
  set.seed(11)
  dataTest <- list(rbeta(12, 2, 5), rbeta(12, 4, 3))

  dp <- DirichletProcessHierarchicalBeta(dataTest, 1)
  dp <- ClusterComponentUpdate(dp)

  expect_hdp_local_state_consistent(dp)
})

test_that("Hierarchical Mvnormal2 local update maintains explicit table/dish state", {
  set.seed(11)
  dataTest <- list(
    mvtnorm::rmvnorm(15, c(0, 0), diag(2)),
    mvtnorm::rmvnorm(15, c(1, -1), diag(2))
  )

  dp <- DirichletProcessHierarchicalMvnormal2(dataTest)
  dp <- ClusterComponentUpdate(dp)

  expect_hdp_local_state_consistent(dp)
})

test_that("Removing an empty table retires an empty dish and relabels remaining dish references", {
  set.seed(21)
  remove_customer <- getFromNamespace("hdp_remove_customer_from_table", "dirichletprocess")

  dp <- DirichletProcessHierarchicalBeta(list(rbeta(1, 2, 5), rbeta(1, 3, 4)), 1)

  expect_equal(dp$tableDishLabels[[1]], 1L)
  expect_equal(dp$tableDishLabels[[2]], 2L)
  expect_equal(dp$dishTableCounts, c(1L, 1L))

  dp <- remove_customer(dp, 1L, 1L)

  expect_equal(dp$indDP[[1]]$numberClusters, 0)
  expect_equal(dp$tableDishLabels[[1]], integer(0))
  expect_equal(dp$indDP[[1]]$clusterLabels, NA_integer_)
  expect_equal(dp$dishTableCounts, 1L)
  expect_equal(dp$tableDishLabels[[2]], 1L)
  expect_equal(dim(dp$globalParameters[[1]])[3], 1)
  expect_equal(dp$indDP[[2]]$clusterParameters[[1]],
               dp$globalParameters[[1]][, , 1, drop = FALSE])
  expect_equal(dp$indDP[[2]]$clusterParameters[[2]],
               dp$globalParameters[[2]][, , 1, drop = FALSE])
})

test_that("Splitting a table onto a new table with an existing dish updates dish counts cleanly", {
  set.seed(22)
  remove_customer <- getFromNamespace("hdp_remove_customer_from_table", "dirichletprocess")
  seat_new_table <- getFromNamespace("hdp_seat_customer_new_table", "dirichletprocess")

  dp <- DirichletProcessHierarchicalBeta(list(rbeta(2, 2, 5)), 1)

  expect_equal(dp$indDP[[1]]$pointsPerCluster, 2)
  expect_equal(dp$tableDishLabels[[1]], 1L)
  expect_equal(dp$dishTableCounts, 1L)

  dp <- remove_customer(dp, 1L, 1L)
  dp <- seat_new_table(dp, 1L, 1L, 1L)

  expect_equal(dp$indDP[[1]]$clusterLabels, c(2L, 1L))
  expect_equal(dp$indDP[[1]]$pointsPerCluster, c(1, 1))
  expect_equal(dp$indDP[[1]]$numberClusters, 2)
  expect_equal(dp$tableDishLabels[[1]], c(1L, 1L))
  expect_equal(dp$dishTableCounts, 2L)

  expect_equal(dp$indDP[[1]]$clusterParameters[[1]][, , 1, drop = FALSE],
               dp$globalParameters[[1]][, , 1, drop = FALSE])
  expect_equal(dp$indDP[[1]]$clusterParameters[[1]][, , 2, drop = FALSE],
               dp$globalParameters[[1]][, , 1, drop = FALSE])
})
