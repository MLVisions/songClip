
#' Package directory containing python scripts
SONGCLIP_PYTHON_DIR <- system.file("python", package = "songClip", mustWork = TRUE)


# All setup (pkg installation/setup) function can go in
# `setup-python-pkgs.R` (such as `setup_py_env()`)

# Python functions -that interface with R- can go here, or in similar files
# - we can come up with better name(s) than `python.R` once the scripts/list of
#   functions serve a particular purpose.

# Define python scripts in `inst/python/<script_name>.py`
# - then source using `system.file` and `reticulate::source_python`
# - example of this below


# Check out `?reticulate::repl_python()`
# - it allows you to -interactively- run python code in the R console
# - could bypass the need for a script for smaller operations



# Example functions -------------------------------------------------------

# Each example adds a bit of complexity
# Once you go through each, it probably makes the most sense to work off the
# last one (or make a new one).



#' Simple python function
#'
#' @details
#' This function does not require imports or virtual environment (much faster)
example_py_func <- function(
    x = 5,
    y = 10
){

  # source specific python script
  py_script <- file.path(SONGCLIP_PYTHON_DIR, "example-script.py")
  reticulate::source_python(py_script)

  # `py_add` is a function defined in `py_script`
  sum <- py_add(x, y)
  cat("sum:\n")

  return(sum)
}



# Since `sum` is a built in python function, the above function could be simplified to:
example_py_func2 <- function(
    x = 5,
    y = 10
){

  # import main python functions (see `main$` and `builtins$` after running)
  import_main_py()

  sum <- builtins$sum(c(x, y))
  builtins$print('sum:\n')

  return(sum)
}





#' Do something with python
#'
#' Example with working with `audio_path` and importing a python package that
#' *comes with python*
#'
#' @param audio_path path to audio file
#'
#' @details
#' The `os` python module comes with python:
#' - We dont need a virtual environment for those packages either (much quicker)
#' - Virtual environment (or conda) is only needed when we have to *install*
#'   packages.
#'
#'
#' @keywords internal
check_audio_py <- function(
    audio_path = file.path(EXAMPLE_AUDIO_DIR, "flowers.mp3")
){

  # import main python functions
  import_main_py()

  # source specific python scripts
  py_script <- file.path(SONGCLIP_PYTHON_DIR, "check-audio-path.py")
  reticulate::source_python(py_script)

  # some python stuff
  # note: make sure the python function names dont overlap with R function names
  audio_exists <- py_check_audio(audio_path)

  # since we called `import_main_py()` before sourcing the script, we can also
  # reference the python function `py_check_audio` via `main$py_check_audio`:
  audio_exists2 <- main$py_check_audio(audio_path)
  audio_exists == audio_exists2

  return(audio_exists)
}






#' Do something with python (this is probably the function to work with)
#'
#' Example with working with `audio_path` and importing python modules
#'
#' @param audio_path path to audio file
#' @param setup_env Logical (`TRUE`/`FALSE`). If `TRUE`, set up a virtual
#'        environment. This should probably be done outside of the function, once
#'        per R session.
#'
#' @details
#' The `os` python module comes with python:
#' - We apparently dont need a virtual environment for those packages either
#' - Virtual environment (or conda) is only needed when we have to *install*
#'   packages.
#'
#' @examples
#' \dontrun{
#'
#'  # With a virtual environment
#' py_env <- setup_py_env(py_pkgs = c("pandas", "numpy", "scipy"), virtual_env = TRUE)
#'
#' process_audio_py()
#'
#' # shutdown virtual environment
#' shutdown_virtual_env(py_env$env_name)
#'
#'
#' # With a conda environment
#' # note: you must restart your R session if you want to try both environment types
#' # much faster loading time after you've installed the packages once
#' py_env <- setup_py_env(py_pkgs = c("pandas", "numpy", "scipy"), virtual_env = FALSE)
#'
#' process_audio_py()
#'
#' }
#'
#' @keywords internal
process_audio_py <- function(
    audio_path = file.path(EXAMPLE_AUDIO_DIR, "flowers.mp3"),
    setup_env = FALSE
){

  # optionally set up a conda or virtual environment
  if(isTRUE(setup_env)){
    virtual_env <- TRUE
    # setup environment for installing non-base python packages
    py_env <- setup_py_env(py_pkgs = c("pandas", "scipy", "numpy"),
                           virtual_env = virtual_env,
                           update = FALSE)
    # close down the virtual environment when exiting -if created in this function-
    if(virtual_env){
      on.exit(shutdown_virtual_env(py_env$env_name))
    }
  }

  # import main python functions
  import_main_py()

  # import required python packages - this can also be done within `script.py`
  # assuming they are already installed, or came with python (e.g. `difflib`)
  difflib <- reticulate::import("difflib")
  filecmp <- reticulate::import("filecmp")
  import_py_pkgs(py_pkgs = c("pandas"))

  # This object (`py_objs`) will keep track of everything in your python environment
  # even after it's called
  py_objs <- reticulate::py_run_string("data={'col1': [1, 2], 'col2': [3, 4]}")

  data_df <- pandas$DataFrame(data=py_objs$data)

  # source specific python scripts
  # - this script also imports scipy.
  # - We had only imported pandas *before* sourcing this script
  py_script <- file.path(SONGCLIP_PYTHON_DIR, "process-audio.py")
  reticulate::source_python(py_script)

  # df2 appeared in `py_objs` after sourcing process-audio.py
  data_return <- py_objs$df2

  return(data_return)
}