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
  player <- '/usr/bin/afplay'
  if(.Platform$OS.type == "unix" && fs::file_exists(player)){
    path <- tuneR::setWavPlayer(player)
    return(path$WavPlayer)
  }else{
    cli::cli_div(theme = list(span.emph = list(color = "red"), span.code = list(color = "blue")))
    cli::cli_warn(c("!" = "{.emph Could not find audio player}",
                    ">" = "Please set the path to your audio player via {.code tuneR::setWavPlayer('path/to/player')}"))
    return(NULL)
  }
}




.onAttach <- function(libname, pkgname) {
  wav_player <- set_audio_player()
  if (!is.null(wav_player)) {
    packageStartupMessage(glue::glue("Detected Audio Player: '{wav_player}'"))
  }
}
