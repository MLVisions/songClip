
# TODO: revisit if this is preferable over .onLoad for some checks
# potentially loading python packages?
.onAttach <- function(libname, pkgname) {
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
}
