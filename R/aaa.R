
# Set audio player on package load. Checks path at '/usr/bin/afplay' (default on macos)
# Check set path via tuneR::getWavPlayer()
#
# TODO: add support for Windows
set_audio_player <- function(){
  player <- '/usr/bin/afplay'
  if(.Platform$OS.type == "unix" && fs::file_exists(player)){
    tuneR::setWavPlayer(player)
  }else{
    cli::cli_div(theme = list(span.emph = list(color = "red")))
    cli::cli_warn(c("!" = "{.emph Audio player not set}. Please set the path to your audio player via:",
                    ">" = "{.code tuneR::setWavPlayer('path/to/player')}"))
  }
}

set_audio_player()


#' Adds the content of www to src_name
#'
#' @import shiny
#' @import ggplot2
#'
#' @noRd
#'
.onLoad <- function(...){
  description <- packageDescription("songClip")
  src_name <- paste0("merge-shiny-", description$Version)
  src_path <- system.file("www", package = "songClip")
  addResourcePath(src_name, src_path)
  # Add shinyFiles resource path (needed for installation)
  addResourcePath('sF', system.file('www', package='shinyFiles'))
  return(
    list(
      version = description$Version,
      src_name = src_name,
      src_path = src_path
    )
  )
}

# Redefine src_name
src_params <- .onLoad()
src_name <- src_params$src_name
src_path <- src_params$src_path
merge.shiny_version <- src_params$version

# Header Text
ver_text <- sprintf("songClip %s", merge.shiny_version)

# Js for header -----------------------------------------------------------

jsHeader <-  '.myClass { font-size: 14px;line-height: 50px;text-align: right;
              padding: 0 15px;overflow: hidden;color: white;float: right !important;}'

jqueryHeader <- paste0('$(document).ready(function() {
                 $("header").find("nav").append(\'<span class="myClass">',
                       ver_text,'</span>\');})')

noWrap_css <- "
.nowrap {
  white-space: nowrap;
}"


EXAMPLE_AUDIO_DIR <- system.file(file.path("examples"), package = "songClip", mustWork = TRUE)
