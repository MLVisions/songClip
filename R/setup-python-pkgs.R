
#' Default environment name for python packages
#'
#' Can be use for virtual of conda environment
SONGCLIP_PYTHON_ENV <- "songClip-python"



#' Set up a python environment with required packages
#'
#' @param py_pkgs vector of python packages to install
#' @param python_version The requested Python version. Ignored when attempting
#'        to install with a Python virtual environment.
#' @param virtual_env Logical (`TRUE`/`FALSE`). If `TRUE`, use a virtual environment.
#' @param env_name a virtual environment name
#' @param conda_name name of the conda environment to use if `virtual_env = FALSE`.
#'        Run `reticulate::conda_list()` to see a list of available conda environments.
#'        The `conda_name` should match the `name` element of that list if you are loading
#'        *an existing* conda environment. If `conda_name` is not found in this list, a new
#'        one will be created matching the name of `env_name`.
#' @param update Logical (`TRUE`/`FALSE`). If `TRUE`, update packages that are
#'        already installed. Otherwise skip their installation.
#'
#' @details
#'
#'
#' We will want to *install* all packages we need for the app once at the beginning,
#' and then *import* packages one at a time within each function that needs them
#'
#' If we use a virtual/conda environment, we are only using that for packages that dont
#' come with base python. i.e. the user still needs to have python installed.
#'
#' An environment will be required for each new session of the app.
#'
#' For a virtual environment, the user would be installing these packages *every time*
#' they want to run the app on a clean R session.
#'
#' For a conda environment, users would only need to install the required
#' packages once. I think this could be preferable, and would offer significant
#' speed improvements once the packages have been installed once.
#'
#' @note
#' You must restart your R session if you want to alternate between both environment types
#'
#'
#' @examples
#' \dontrun{
#'
#' # With a conda environment (the default)
#' # much faster loading time after you've installed the packages once
#' py_env <- setup_py_env(py_pkgs = c("pandas", "numpy", "scipy"), virtual_env = FALSE)
#'
#'
#' # With a virtual environment
#' # note: you must restart your R session if you want to try both environment types
#' py_env <- setup_py_env(py_pkgs = c("pandas", "numpy", "scipy"), virtual_env = TRUE)
#'
#' # shutdown virtual environment
#' shutdown_virtual_env(py_env$env_name)
#'
#' }
#'
#' @keywords internal
setup_py_env <- function(
    py_pkgs = c("scipy", "pandas"),
    python_version = NULL,
    virtual_env = FALSE,
    env_name = SONGCLIP_PYTHON_ENV,
    conda_name = c("base", "r-reticulate", SONGCLIP_PYTHON_ENV),
    update = FALSE
){

  # Make sure python is installed
  checkmate::assert_true(python_is_installed())

  conda_name <- match.arg(conda_name)

  if(isTRUE(virtual_env)){
    ### This currently doesnt work ### - cant import modules after installing them
    # create a new environment
    env_path <- tryCatch(reticulate::virtualenv_create(env_name), error = identity)

    # make sure the environment was created
    env_exists <- reticulate::virtualenv_exists(env_name)
    if(isFALSE(env_exists)){
      cli::cli_abort(glue::glue("env {env_name} could not be created:\n\n {env_path$message}"))
    }

    # Configure vitual environment
    reticulate::use_virtualenv(virtualenv = env_path, required = TRUE)
    message(glue::glue("a virtual environment has been loaded at: {env_path}"))
  }else{
    conda_envir_lst <- reticulate::conda_list()
    conda_envirs <- conda_envir_lst %>% dplyr::pull(name)

    # pull previous conda environment if it exists, otherwise create new one
    if(conda_name %in% conda_envirs){
      # loads a local conda library
      env_path <- conda_envir_lst %>% dplyr::filter(name == conda_name) %>% dplyr::pull(python)
      # overwrite env_name with conda_name
      env_name <- conda_name
    }else{
      # Creates a local conda library
      env_path <- tryCatch(reticulate::conda_create(env_name), error = identity)
      # make sure the environment was created
      env_exists <- !inherits(env_path, "error") && fs::file_exists(env_path)
      if(isFALSE(env_exists)){
        cli::cli_abort(glue::glue("env {env_name} could not be created:\n\n {env_path$message}"))
      }
    }

    # Configure conda environment
    reticulate::use_condaenv(condaenv = env_path, required = TRUE)
    message(glue::glue("a conda environment has been loaded at: {env_path}"))
  }

  # configure python in environment
  config <- reticulate::py_config()

  # install python packages
  installed_pkgs <- install_py_pkgs(
    py_pkgs,
    env_name = env_name,
    virtual_env = virtual_env,
    update = update,
    python_version = python_version
  )

  env_list <- list(
    env_name = env_name,
    env_path = env_path,
    config = config,
    installed_pkgs = installed_pkgs
  )

  return(env_list)
}

#' Install python packages to an environment
#'
#' @inheritParams setup_py_env
#'
#' @details
#' see `?reticulate::py_install` for more details
#'
#' @keywords internal
install_py_pkgs <- function(
    py_pkgs = c("scipy", "pandas"),
    virtual_env = FALSE,
    env_name = SONGCLIP_PYTHON_ENV,
    update = FALSE,
    python_version = NULL
){



  py_install_fn <- ifelse(isTRUE(virtual_env),
                          reticulate::virtualenv_install,
                          reticulate::conda_install
  )

  # Check if already installed
  if(isFALSE(update)){
    installed_pkgs <- reticulate::py_list_packages(env_name) %>%
      tibble::as_tibble()

    py_pkgs_installed <- py_pkgs[py_pkgs %in% installed_pkgs$package] %>%
      paste(collapse = ", ")
    msg <- paste0(
      "The following packages have already been installed",
      " (set `update = TRUE` to override). Skipping...",
      glue::glue("\n\n{py_pkgs_installed}\n")
    )
    message(msg)
    # filter list of packages to install
    py_pkgs <- py_pkgs[!(py_pkgs %in% installed_pkgs$package)]
  }


  if(!rlang::is_empty(py_pkgs)){
    purrr::walk(py_pkgs, function(pkg){
      args <- list(pkg, python_version = python_version,
                   envname = env_name)
      do.call(py_install_fn, args)
    })
  }

  # Get new selection of installed packages
  installed_pkgs <- reticulate::py_list_packages(env_name) %>%
    tibble::as_tibble()

  return(installed_pkgs)
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
      message(glue::glue("Could not import {pkg_name}. Please check that it is installed"))
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
shutdown_virtual_env <- function(env_name = SONGCLIP_PYTHON_ENV, force = TRUE){
  if(reticulate::virtualenv_exists(env_name)){
    reticulate::virtualenv_remove(env_name, confirm = !force)
    # Sys.setenv("RETICULATE_PYTHON_ENV" = "")
  }else{
    warning(glue::glue("Virtual environment {env_name} does not exist."))
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

