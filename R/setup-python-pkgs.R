
#' Default environment for python packages
SONGCLIP_PYTHON_ENV <- "songClip-python"


#' Install python packages to an environment
#'
#' @param py_pkgs vector of python packages to install
#' @param python_version The requested Python version. Ignored when attempting
#'        to install with a Python virtual environment.
#' @param virtual_env Logical (`TRUE`/`FALSE`). If `TRUE`, use a virtual environment.
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
    virtual_env = FALSE,
    env = SONGCLIP_PYTHON_ENV,
    method = "auto",
    ...
){

  py_install_fn <- ifelse(isTRUE(virtual_env),
                          reticulate::virtualenv_install,
                          reticulate::py_install
  )

  purrr::walk(py_pkgs, function(pkg){
    args <- list(pkg, python_version = python_version)

    if(isTRUE(virtual_env)){
      args <- c(args, list(envname = env))
    }else{
      args <- c(args, list(method = method))
    }
    do.call(py_install_fn, args)
  })
}



#' Import python packages into the environment
#'
#' @inheritParams install_py_pkgs
#' @inheritParams import_main_py
#'
#' @details
#'
#' This function loops through `py_pkgs`, and does the equivalent of:
#' `py_pkg <- reticulate::import("py_pkg")`
#' or
#' `scipy <- reticulate::import("scipy")`
#'
#'
#' @keywords internal
import_py_pkgs <- function(
    py_pkgs = c("scipy", "pandas"),
    envir = NULL
){

  if(is.null(envir)){
    envir <- parent.frame()
  }

  # import each package
  purrr::walk(py_pkgs, function(pkg_name){
    if(reticulate::py_module_available(pkg_name)){
      pkg <- reticulate::import(pkg_name)
      assign(pkg_name, pkg, envir = envir)
    }else{
      message(glue::glue("Could not import {pkg_name}"))
    }
  })
}


#' Import main module and default functions
#'
#' @details
#'
#' See `main$` and `builtins$` after running this function
#'
#' Sourcing scripts **after** importing the main modules will add those functions
#' under `main$`
#'
#'
#' @param envir environment to load the main modules into
#'
#' @keywords internal
import_main_py <- function(envir = NULL){

  if(is.null(envir)){
    envir <- parent.frame()
  }

  # import main modules
  main <- reticulate::import_main()
  builtins <- reticulate::import_builtins()

  # assign to environment the function was -called- in
  assign("main", main, envir = envir)
  assign("builtins", builtins, envir = envir)
}

#' Set up a python environment with required packages
#'
#' @inheritParams install_py_pkgs
#' @details
#'
#' This should only be called once per R session because of how long it takes.
#'
#' We will want to *install* all packages we need for the app once at the beginning,
#' and then *import* packages one at a time within each function that needs them
#'
#' If we use a virtual environment, we are only using that for packages that dont
#' come with base python. i.e. the user still needs to have python installed.
#'
#' A virtual environment would also be required for each new session of the app,
#' meaning the user would be installing these packages every time they want to run
#' the app on a clean R session.
#'
#' If we dont use a virtual environment, users would only need to install the required
#' packages once. I think this could be preferable, and would offer significant
#' speed improvements.
#'
#' @keywords internal
setup_py_env <- function(
    py_pkgs = c("scipy", "pandas"),
    virtual_env = FALSE,
    env = SONGCLIP_PYTHON_ENV
){

  # Make sure python is installed
  checkmate::assert_true(reticulate::py_available())

  if(isTRUE(virtual_env)){
    ### This currently doesnt work ### - cant import modules after instaling them
    # create a new environment
    env_path <- tryCatch(reticulate::virtualenv_create(env), error = identity)

    # make sure the environment was created
    env_exists <- reticulate::virtualenv_exists(env)
    if(isFALSE(env_exists)){
      cli::cli_abort(glue::glue("env {env} could not be created:\n\n {env_path$message}"))
    }
    # Not sure if this is done right - see ?reticulate::use_virtualenv section
    # on `RETICULATE_PYTHON`
    config <- reticulate::py_discover_config(use_environment = env)
    python_path <- config$python
    # python_path <- virtualenv_starter()
  }else{
    config <- reticulate::py_discover_config()
    python_path <- config$python
  }

  # Configure python installation
  Sys.setenv("RETICULATE_PYTHON" = config$python)

  # install python packages
  install_py_pkgs(py_pkgs, env = env, virtual_env = virtual_env)

  if(isTRUE(virtual_env)){
    # load the virtual (or conda) environment - this takes a couple seconds
    reticulate::use_virtualenv(virtualenv = env_path)
  }

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

