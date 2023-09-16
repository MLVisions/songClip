

# All setup (pkg installation/setup) function can go in
# `setup-python-pkgs.R` (such as `setup_py_env()`)

# Python functions -that interface with R- can go here, or in similar files

# Define python scripts in `inst/python/<script_name>.py`
# - then source using `system.file` and `reticulate::source_python`
# - example below



example_py_func <- function(
    audio_path = file.path(EXAMPLE_AUDIO_DIR, "flowers.mp3")
){

  # import py packages (this doesnt work, and may not be necessary?)
  # setup_py_env(py_pkgs = c("scipy", "pandas"))

  # use python script (probably dont want both)
  py_script <- system.file("python", "example_script.py", package = "songClip", mustWork = TRUE)
  reticulate::source_python(py_script)

  # some python stuff - `add` is a function defined in `py_script`
  # actual functions will take in `audio_path`
  x <- add(5, 10)

  return(x)
}





# Shutting down virtual environment ---------------------------------------

# potentially call this at some point:
# `reticulate::virtualenv_remove(SONGCLIP_PYTHON_ENV)`

# This will shut down the environment
# shutting it down means you'd have to install the packages again,
# so we should only shut this down -if we are done- using the python packages
# i.e. probably dont run this per function, but after all python related functions
# are done (probably when the app closes)

