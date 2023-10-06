

.onAttach <- function(libname, pkgname) {

  if (!getOption(".songclip_initialized", default = FALSE) && isFALSE(testthat::is_testing())) {
    # Make sure python is installed:
    py_installed <- python_is_installed()
    if(isFALSE(py_installed)){
      user_permission <- utils::askYesNo("Install python? This will take some time")
      if (isTRUE(user_permission)) {
        reticulate::install_python()
      } else {
        packageStartupMessage("Python is required for this Package. Run `reticulate::install_python()` before proceeding")
      }
    }

    # make sure miniconda is installed:
    miniconda_installed <- file.exists(is_miniconda_installed())
    if(isFALSE(miniconda_installed)){
      user_permission <- utils::askYesNo("Install miniconda? Downloads ~50MB and takes time")
      if (isTRUE(user_permission)) {
        reticulate::install_miniconda()
      } else {
        packageStartupMessage("miniconda is required for this Package. You should run `reticulate::install_miniconda()` before proceeding")
      }
    }

    # TODO: potentially load required python packages

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
