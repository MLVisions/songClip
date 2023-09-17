#' @import shiny
#' @importFrom shinycssloaders withSpinner
NULL

# needed for running a shiny app in the Rstudio viewer
utils::globalVariables(".rs.invokeShinyPaneViewer")
utils::globalVariables(".rs.invokeShinyWindowExternal")

utils::globalVariables(".data")


.onLoad <- function(libname, pkgname){

  # Check audio player is set
  wav_player <- set_audio_player()
  if (!is.null(wav_player)) {
    packageStartupMessage(glue::glue("Detected Audio Player: '{wav_player}'"))
  }else{
    cli::cli_div(theme = list(span.emph = list(color = "red"), span.code = list(color = "blue")))
    cli::cli_warn(c("!" = "{.emph Could not find audio player}",
                    ">" = "Please set the path to your audio player via {.code tuneR::setWavPlayer('path/to/player')}"))
  }
}

#' Adds the content of www to src_name for use in the shiny app
#'
#'
#' @noRd
#'
setup_resource_paths <- function(){
  description <- utils::packageDescription("songClip")
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

python_is_installed <- function(use_environment = NULL){
  config <- tryCatch({
    reticulate::py_discover_config(use_environment = use_environment)
    }, error = identity)

  if(inherits(config, "error")){
    FALSE
  }else{
    fs::file_exists(config$python)
  }
}

is_miniconda_installed <- function(path = reticulate::miniconda_path()){
  exe <- if (xfun::is_windows()){
    "condabin/conda.bat"
  }else{
    "bin/conda"
  }
  file.path(path, exe)
}


# Make sure audio player is set up ----------------------------------------

#' Set audio player on package load. Checks path at '/usr/bin/afplay' (default on macos)
#'
#' @details
#'
#' Check set path via tuneR::getWavPlayer()
#'
#' TODO: add support for Windows
#'
#' @keywords internal
set_audio_player <- function(){
  player_path <- tuneR::getWavPlayer()
  player_chk <- '/usr/bin/afplay'

  player_is_set <- !is.null(player_path) && nzchar(player_path) && fs::file_exists(player_path)
  os_supported <- .Platform$OS.type == "unix" && fs::file_exists(player_chk)

  if(os_supported && !player_is_set){
    # player not set and os is supported
    tuneR::setWavPlayer(player_chk)
    path <- tuneR::getWavPlayer()
    return(path)
  }else if(os_supported && player_is_set){
    # path was already set
    return(player_path)
  }else{
    # os not supported
    return(NULL)
  }

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

