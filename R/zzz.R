
# TODO: revisit if this is preferable over .onLoad for some checks
# potentially loading python packages?
.onAttach <- function(libname, pkgname) {
  # wav_player <- set_audio_player()
  # if (!is.null(wav_player)) {
  #   packageStartupMessage(glue::glue("Detected Audio Player: '{wav_player}'"))
  # }
}
