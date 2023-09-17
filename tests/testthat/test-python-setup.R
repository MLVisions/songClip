
testthat::test_that("python setup", {
  skip_if_no_python()
  py_env <- tryCatch({
    setup_py_env(py_pkgs = c("pandas", "numpy", "scipy"))
  }, error = identity)

  testthat::expect_true(is.list(py_env))
})
