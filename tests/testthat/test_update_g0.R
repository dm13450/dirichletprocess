context("HDP UpdateG0 Compatibility")

test_that("UpdateG0 is a no-op compatibility shim for hierarchical beta", {
  set.seed(41)
  update_g0 <- getFromNamespace("UpdateG0", "dirichletprocess")
  dp <- DirichletProcessHierarchicalBeta(list(rbeta(8, 2, 5), rbeta(8, 3, 4)), 1)
  dp <- ClusterComponentUpdate(dp)
  dp <- GlobalParameterUpdate(dp)

  before <- dp
  after <- update_g0(dp)

  expect_identical(after$tableDishLabels, before$tableDishLabels)
  expect_identical(after$dishTableCounts, before$dishTableCounts)
  expect_identical(after$globalParameters, before$globalParameters)
  expect_identical(after$indDP[[1]]$clusterParameters, before$indDP[[1]]$clusterParameters)
  expect_identical(after$indDP[[2]]$clusterParameters, before$indDP[[2]]$clusterParameters)
})

test_that("Hierarchical beta Fit no longer depends on legacy UpdateG0 behavior", {
  set.seed(42)
  dp <- DirichletProcessHierarchicalBeta(list(rbeta(10, 2, 5), rbeta(10, 3, 4)), 1)

  expect_no_warning({
    dp <- Fit(dp, 1, progressBar = FALSE)
  })

  expect_true("tableDishLabels" %in% names(dp))
  expect_true("dishTableCounts" %in% names(dp))
  expect_equal(sum(dp$dishTableCounts),
               sum(vapply(dp$indDP, function(x) x$numberClusters, numeric(1))))
})

test_that("Hierarchical mvnormal2 Fit no longer depends on legacy UpdateG0 behavior", {
  set.seed(43)
  dp <- DirichletProcessHierarchicalMvnormal2(list(
    mvtnorm::rmvnorm(10, c(0, 0), diag(2)),
    mvtnorm::rmvnorm(10, c(1, -1), diag(2))
  ))

  expect_no_warning({
    dp <- Fit(dp, 1, progressBar = FALSE)
  })

  expect_true("tableDishLabels" %in% names(dp))
  expect_true("dishTableCounts" %in% names(dp))
  expect_equal(sum(dp$dishTableCounts),
               sum(vapply(dp$indDP, function(x) x$numberClusters, numeric(1))))
})
