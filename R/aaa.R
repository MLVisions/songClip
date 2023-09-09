
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
