context("Constructor cpp compatibility flags")

test_that("Hierarchical constructors do not alter ordinary C++ routing override", {
  old_cpp_setting <- getOption("dirichletprocesscpp.use_cpp", NULL)
  old_hierarchical_setting <- getOption("dirichletprocesscpp.force_cpp_hierarchical", NULL)
  on.exit(options(dirichletprocesscpp.use_cpp = old_cpp_setting), add = TRUE)
  on.exit(options(dirichletprocesscpp.force_cpp_hierarchical = old_hierarchical_setting), add = TRUE)

  options(dirichletprocesscpp.use_cpp = NULL)
  options(dirichletprocesscpp.force_cpp_hierarchical = TRUE)

  beta_data <- list(rbeta(6, 2, 5), rbeta(7, 3, 4))
  mvn_data <- list(
    mvtnorm::rmvnorm(6, c(0, 0), diag(2)),
    mvtnorm::rmvnorm(7, c(1, -1), diag(2))
  )

  beta_dp_false <- DirichletProcessHierarchicalBeta(beta_data, maxY = 1, cpp = FALSE)
  expect_null(getOption("dirichletprocesscpp.use_cpp", NULL))
  expect_true(getOption("dirichletprocesscpp.force_cpp_hierarchical", FALSE))
  expect_false(can_use_hierarchical_cpp(beta_dp_false))

  beta_dp_true <- DirichletProcessHierarchicalBeta(beta_data, maxY = 1, cpp = TRUE)
  expect_null(getOption("dirichletprocesscpp.use_cpp", NULL))
  expect_true(getOption("dirichletprocesscpp.force_cpp_hierarchical", FALSE))
  expect_false(can_use_hierarchical_cpp(beta_dp_true))

  mvn_dp_false <- DirichletProcessHierarchicalMvnormal2(mvn_data, cpp = FALSE)
  expect_null(getOption("dirichletprocesscpp.use_cpp", NULL))
  expect_true(getOption("dirichletprocesscpp.force_cpp_hierarchical", FALSE))
  expect_false(can_use_hierarchical_cpp(mvn_dp_false))

  mvn_dp_true <- DirichletProcessHierarchicalMvnormal2(mvn_data, cpp = TRUE)
  expect_null(getOption("dirichletprocesscpp.use_cpp", NULL))
  expect_true(getOption("dirichletprocesscpp.force_cpp_hierarchical", FALSE))
  expect_false(can_use_hierarchical_cpp(mvn_dp_true))
})

test_that("HMM constructor cpp compatibility flag does not alter unrelated routing flags", {
  old_cpp_setting <- getOption("dirichletprocesscpp.use_cpp", NULL)
  old_markov_setting <- getOption("dirichletprocesscpp.use_cpp_markov", NULL)
  on.exit(options(dirichletprocesscpp.use_cpp = old_cpp_setting), add = TRUE)
  on.exit(options(dirichletprocesscpp.use_cpp_markov = old_markov_setting), add = TRUE)

  options(dirichletprocesscpp.use_cpp = NULL)
  options(dirichletprocesscpp.use_cpp_markov = TRUE)

  dp_true <- DirichletHMMCreate(rnorm(12), GaussianMixtureCreate(), 2, 3, cpp = TRUE)
  expect_null(getOption("dirichletprocesscpp.use_cpp", NULL))
  expect_true(getOption("dirichletprocesscpp.use_cpp_markov", FALSE))
  expect_true(using_cpp_markov_samplers())
  expect_true(inherits(dp_true, "markov"))

  options(dirichletprocesscpp.use_cpp_markov = FALSE)
  dp_false <- DirichletHMMCreate(rnorm(12), GaussianMixtureCreate(), 2, 3, cpp = FALSE)
  expect_null(getOption("dirichletprocesscpp.use_cpp", NULL))
  expect_false(getOption("dirichletprocesscpp.use_cpp_markov", TRUE))
  expect_false(using_cpp_markov_samplers())
  expect_true(inherits(dp_false, "markov"))
})
