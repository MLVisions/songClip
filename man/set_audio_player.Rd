% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aaa.R
\name{set_audio_player}
\alias{set_audio_player}
\title{Set audio player on package load. Checks path at '/usr/bin/afplay' (default on macos)}
\usage{
set_audio_player()
}
\description{
Set audio player on package load. Checks path at '/usr/bin/afplay' (default on macos)
}
\details{
Check set path via tuneR::getWavPlayer()

TODO: add support for Windows
}
\keyword{internal}
