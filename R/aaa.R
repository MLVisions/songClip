#' @import shiny
#' @importFrom shinycssloaders withSpinner
NULL


.onLoad <- function(libname, pkgname){

  # TODO: determine if this is necessary
  # we may not need miniconda for a virtual environment
  miniconda_installed <- TRUE #file.exists(is_miniconda_installed())

  if(isFALSE(miniconda_installed)){
    user_permission <- utils::askYesNo("Install miniconda? Downloads ~50MB and takes time")
    if (isTRUE(user_permission)) {
      reticulate::install_miniconda()
    } else {
      packageStartupMessage("Python is required for this Package. You should run `reticulate::install_miniconda()` before proceeding")
    }
  }

}

#' Adds the content of www to src_name
#'
#'
#' @noRd
#'
setup_resource_paths <- function(){
  description <- packageDescription("songClip")
  src_name <- paste0("songClip-", description$Version)
  src_path <- system.file("www", package = "songClip")
  shiny::addResourcePath(src_name, src_path)
  # Add shinyFiles resource path (needed for installation)
  shiny::addResourcePath('sF', system.file('www', package='shinyFiles'))
  return(
    list(
      version = description$Version,
      src_name = src_name,
      src_path = src_path
    )
  )
}

# TODO: refactor this: could conflict with user objects
src_params <- setup_resource_paths()
src_name <- src_params$src_name
src_path <- src_params$src_path
pkg_version <- src_params$version

#' Header Text
VERSION_HEADER <- sprintf("songClip %s", pkg_version)

#' Example audio directory
EXAMPLE_AUDIO_DIR <- system.file(file.path("examples"), package = "songClip", mustWork = TRUE)

# Make sure python is set up ----------------------------------------------

is_miniconda_installed <- function(path = reticulate::miniconda_path()){
  exe <- if (xfun::is_windows()){
    "condabin/conda.bat"
  }else{
    "bin/conda"
  }
  file.path(path, exe)
}


# Js for header -----------------------------------------------------------

cssHeader <-  '.myClass { font-size: 14px;line-height: 50px;text-align: right;
              padding: 0 15px;overflow: hidden;color: white;float: right !important;}'

jqueryHeader <- paste0('$(document).ready(function() {
                 $("header").find("nav").append(\'<span class="myClass">',
                       VERSION_HEADER,'</span>\');})')

noWrap_css <- "
.nowrap {
  white-space: nowrap;
}"

