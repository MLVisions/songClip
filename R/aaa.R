#' @import shiny
#' @import ggplot2
#' @importFrom shinycssloaders withSpinner
NULL



#' Adds the content of www to src_name
#'
#'
#' @noRd
#'
.onLoad <- function(...){
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

# Redefine src_name
src_params <- .onLoad()
src_name <- src_params$src_name
src_path <- src_params$src_path
pkg_version <- src_params$version

#' Header Text
VERSION_HEADER <- sprintf("songClip %s", pkg_version)

# Js for header -----------------------------------------------------------

jsHeader <-  '.myClass { font-size: 14px;line-height: 50px;text-align: right;
              padding: 0 15px;overflow: hidden;color: white;float: right !important;}'

jqueryHeader <- paste0('$(document).ready(function() {
                 $("header").find("nav").append(\'<span class="myClass">',
                       VERSION_HEADER,'</span>\');})')

noWrap_css <- "
.nowrap {
  white-space: nowrap;
}"


#' Example audio directory
EXAMPLE_AUDIO_DIR <- system.file(file.path("examples"), package = "songClip", mustWork = TRUE)


