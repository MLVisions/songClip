
# Set audio player on package load. Checks path at '/usr/bin/afplay' (default on macos)
# Check set path via tuneR::getWavPlayer()
#
# TODO: add support for Windows
set_audio_player <- function(){
  player <- '/usr/bin/afplay'
  if(fs::file_exists(player)){
    tuneR::setWavPlayer(player)
  }
}

set_audio_player()
