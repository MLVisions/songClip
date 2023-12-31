% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/python.R
\name{process_audio_py}
\alias{process_audio_py}
\title{Do something with python (this is probably the function to work with)}
\usage{
process_audio_py(audio_path = file.path(EXAMPLE_AUDIO_DIR, "flowers.mp3"))
}
\arguments{
\item{audio_path}{path to audio file}

\item{setup_env}{Logical (\code{TRUE}/\code{FALSE}). If \code{TRUE}, set up a virtual
environment. This should probably be done outside of the function, once
per R session.}
}
\description{
Example with working with \code{audio_path} and importing python modules
}
\details{
The \code{os} python module comes with python:
\itemize{
\item We apparently dont need a virtual environment for those packages either
\item Virtual environment (or conda) is only needed when we have to \emph{install}
packages.
}
}
\examples{
\dontrun{

## With a conda environment (the default) ##
# much faster loading time after you've installed the packages once
py_env <- setup_py_env(py_pkgs = c("pandas", "numpy", "scipy"))
py_env

process_audio_py()


## With a virtual environment ##
# note: you must restart your R session if you've already generated an environment
py_env <- setup_py_env(py_pkgs = c("pandas", "numpy", "scipy"), virtual_env = TRUE)
py_env

process_audio_py()

# shutdown virtual environment
shutdown_virtual_env(py_env$env_name)

# Installing a package after setup (works with both environment types)
setup_py_env(py_pkgs = c("pandas", "numpy"), virtual_env = TRUE)
install_py_pkgs(py_pkgs = c('scipy'), virtual_env = TRUE)
process_audio_py()

}

}
\keyword{internal}
