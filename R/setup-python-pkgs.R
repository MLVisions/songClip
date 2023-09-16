
#' Default environment for python packages
SONGCLIP_PYTHON_ENV <- "songClip-python"


#' Install python packages to an environment
#'
#' @param py_pkgs vector of python packages to install
#' @param python_version The requested Python version. Ignored when attempting
#'        to install with a Python virtual environment.
#' @param env a virtual environment name
#' @param method Installation method. By default, "auto" automatically finds a
#'        method that will work in the local environment. Change the default to
#'        force a specific installation method. Note that the "virtualenv"
#'        method is not available on Windows.
#'
#' @details
#' see `?reticulate::py_install` for more details
#'
#' @keywords internal
install_py_pkgs <- function(
    py_pkgs = c("scipy", "pandas"),
    python_version = NULL,
    env = SONGCLIP_PYTHON_ENV,
    method = "virtualenv",
    ...
){
  purrr::walk(py_pkgs, function(pkg){
    reticulate::virtualenv_install(
      pkg,
      envname = env,
      # method = method,
      python_version = python_version,
      ...
    )
  })
}



#' Import python packages into the environment
#'
#' @inheritParams install_py_pkgs
import_py_pkgs <- function(py_pkgs = c("scipy", "pandas")){

  # import each package
  purrr::walk(py_pkgs, function(pkg_name){
    if(reticulate::py_module_available(pkg_name)){
      pkg <- reticulate::import(pkg_name)
      assign(pkg_name, pkg)
    }else{
      message(glue::glue("Could not import {pkg_name}"))
    }
  })
}


#' Set up virtual python environment with required packages
#'
#' @inheritParams install_py_pkgs
#'
#' @details
#'
#' This should only be called once per R session because of how long it takes.
#'
#' We will want to *install* all packages we need for the app once at the beginning,
#' and then *import* packages one at a time within each function that needs them
#'
#' @keywords internal
setup_py_env <- function(
    py_pkgs = c("numpy", "scipy", "pandas"),
    env = SONGCLIP_PYTHON_ENV
){
  # create a new environment
  env_path <- tryCatch(reticulate::virtualenv_create(env), error = identity)

  # make sure the environment was created
  env_exists <- reticulate::virtualenv_exists(env)
  if(isFALSE(env_exists)){
    cli::cli_abort(glue::glue("env {env} could not be created:\n\n {env_path$message}"))
  }

  # Configure python
  config <- reticulate::py_discover_config(use_environment = env)
  # Not sure if `RETICULATE_PYTHON` is done right - see ?reticulate::use_virtualenv
  Sys.setenv("RETICULATE_PYTHON" = config$python)
  # Sys.setenv("RETICULATE_PYTHON" = virtualenv_starter())

  # install python packages
  reticulate::import_main()
  install_py_pkgs(py_pkgs, env = env)

  # load the virtual environemnt (or conda)
  # this takes a couple seconds
  reticulate::use_virtualenv(virtualenv = env_path)

  return(invisible(env))
}



#' Shut down virtual environment
#'
#' @inheritParams install_py_pkgs
#' @param force Logical (`TRUE`/`FALSE`). If `FALSE`, confirm before removing
#'        packages or virtual environments
#'
#' @details
#' Potentially call this at some point
#'
#' This will shut down the environment...
#' Shutting it down means you'd have to install the packages again, should we
#' need them again. Given that, we should only shut this down -if we are done-
#' using the python packages.
#'
#' i.e. probably dont run this once per function, but after all python related functions
#' are done (probably when the app closes, or maybe not at all (i.e. it runs for the
#' whole session))
#'
#' @keywords internal
shutdown_virtual_env <- function(env = SONGCLIP_PYTHON_ENV, force = TRUE){
  if(reticulate::virtualenv_exists(env)){
    reticulate::virtualenv_remove(env, confirm = !force)
  }else{
    warning(glue::glue("Virtual environment {env} does not exist."))
  }
}









# helper functions to skip tests if we don't have the 'foo' module

skip_if_no_scipy <- function() {
  have_scipy <- reticulate::py_module_available("scipy")
  if (!have_scipy)
    skip("scipy not available for testing")
}

skip_if_no_pandas <- function() {
  have_pandas <- reticulate::py_module_available("pandas")
  if (!have_pandas)
    skip("pandas not available for testing")
}

