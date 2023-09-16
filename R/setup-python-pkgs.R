

SONGCLIP_PYTHON_ENV <- "songClip-python"



install_py_pkgs <- function(
    py_pkgs = c("scipy", "pandas"),
    python_version = NULL,
    env = SONGCLIP_PYTHON_ENV,
    method = "auto",
    ...
){
  purrr::walk(py_pkgs, function(pkg){
    reticulate::py_install(
      pkg,
      envname = env,
      method = method,
      python_version = python_version,
      ...
    )
  })
}



setup_py_env <- function(env = SONGCLIP_PYTHON_ENV,
                         py_pkgs = c("scipy", "pandas")
){
  # create a new environment
  reticulate::virtualenv_create(env)

  # install python packages
  install_py_pkgs(py_pkgs, env = env)

  purrr::walk(py_pkgs, function(pkg_name){
    if(reticulate::py_module_available(pkg_name)){
      pkg <- reticulate::import(pkg_name)
      assign(pkg_name, pkg)
    }else{
      message(glue::glue("Could not install {pkg_name}"))
    }
  })
}





# helper functions to skip tests if we don't have the 'foo' module

skip_if_no_scipy <- function() {
  have_scipy <- py_module_available("scipy")
  if (!have_scipy)
    skip("scipy not available for testing")
}

skip_if_no_pandas <- function() {
  have_pandas <- py_module_available("pandas")
  if (!have_pandas)
    skip("pandas not available for testing")
}

