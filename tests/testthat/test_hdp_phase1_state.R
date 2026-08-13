context("HDP Phase 1 State")

expect_hdp_phase1_state <- function(dpobjlist) {
  expect_s3_class(dpobjlist, c("hierarchical", "dirichletprocess", "list"))
  expect_true("tableDishLabels" %in% names(dpobjlist))
  expect_true("dishTableCounts" %in% names(dpobjlist))
  expect_true("globalParameters" %in% names(dpobjlist))
  expect_true("globalStick" %in% names(dpobjlist))

  total_tables <- sum(vapply(dpobjlist$indDP,
                             function(dp_obj) as.integer(dp_obj$numberClusters),
                             integer(1)))

  expect_length(dpobjlist$tableDishLabels, length(dpobjlist$indDP))
  expect_equal(length(dpobjlist$dishTableCounts), total_tables)
  expect_true(all(dpobjlist$dishTableCounts == 1L))

  if (total_tables > 0L) {
    expect_equal(unlist(dpobjlist$tableDishLabels, use.names = FALSE),
                 seq_len(total_tables))
  }

  for (i in seq_along(dpobjlist$indDP)) {
    local_dp <- dpobjlist$indDP[[i]]
    local_dishes <- dpobjlist$tableDishLabels[[i]]

    expect_length(local_dishes, local_dp$numberClusters)

    for (j in seq_along(local_dp$clusterParameters)) {
      expect_equal(local_dp$clusterParameters[[j]],
                   dpobjlist$globalParameters[[j]][, , local_dishes, drop = FALSE])
    }
  }
}

test_that("Hierarchical Beta constructor seeds canonical Phase 1 table/dish state", {
  set.seed(1)
  dataTest <- replicate(3, rbeta(50, 2, 5), simplify = FALSE)

  dp <- DirichletProcessHierarchicalBeta(dataTest, 1, gammaPriors = c(2, 0.01))

  expect_hdp_phase1_state(dp)
  expect_true(all(vapply(dp$indDP,
                         function(dp_obj) !is.null(dp_obj$mixingDistribution$theta_k),
                         logical(1))))
  expect_true(all(vapply(dp$indDP,
                         function(dp_obj) !is.null(dp_obj$mixingDistribution$pi_k),
                         logical(1))))
})

test_that("Hierarchical Mvnormal2 constructor seeds canonical Phase 1 table/dish state", {
  set.seed(1)
  dataTest <- replicate(3, mvtnorm::rmvnorm(60, c(1, -1), diag(2)), simplify = FALSE)
  g0Priors <- list(nu0 = 2,
                   phi0 = diag(2),
                   mu0 = matrix(c(0, 0), ncol = 2),
                   sigma0 = diag(2))

  dp <- DirichletProcessHierarchicalMvnormal2(dataTest, g0Priors, gammaPriors = c(2, 0.01))

  expect_hdp_phase1_state(dp)
  expect_true(all(vapply(dp$indDP,
                         function(dp_obj) !is.null(dp_obj$mixingDistribution$theta_k),
                         logical(1))))
  expect_true(all(vapply(dp$indDP,
                         function(dp_obj) !is.null(dp_obj$mixingDistribution$pi_k),
                         logical(1))))
})
