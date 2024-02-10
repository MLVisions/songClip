

.onAttach <- function(libname, pkgname) {

  if (!getOption(".songclip_initialized", default = FALSE) && isFALSE(testthat::is_testing())) {

    # Check audio player is set
    wav_player <- set_audio_player()
    if (!is.null(wav_player)) {
      packageStartupMessage(glue::glue("Detected Audio Player: '{wav_player}'"))
      processx::process$new("say", args = c("-v", "Daniel", "'Welcome to songClip; Audio player detected'"))
    }else{
      cli::cli_div(theme = list(span.emph = list(color = "red"), span.code = list(color = "blue")))
      cli::cli_warn(c("!" = "{.emph Could not find audio player}. Some features may be disabled.",
                      ">" = "You can set the path to your local audio player via {.code tuneR::setWavPlayer('path/to/player')}"))
    }

    # Set the flag to indicate package initialization
    options(.songclip_initialized = TRUE)
  }
}
