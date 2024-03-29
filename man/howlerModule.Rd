% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/howler.R
\name{howlerModule}
\alias{howlerModule}
\alias{howlerModuleUI}
\alias{howlerBasicModuleUI}
\alias{howlerModuleServer}
\title{Howler.js Module}
\usage{
howlerModuleUI(id, files, ..., include_current_track = TRUE, width = "300px")

howlerBasicModuleUI(id, files, ..., width = "300px")

howlerModuleServer(id)
}
\arguments{
\item{id}{ID to give to the namespace of the module. The howler player will have the ID \code{{id}-howler}.}

\item{files}{Files that will be used in the player. This can either be a single vector, or a list where different
formats of the same file are kept in each element of the list.}

\item{...}{Further arguments to send to \code{\link{howler}}}

\item{include_current_track}{Logical, should the current track be included in the UI of the module?}

\item{width}{Width (in pixels) of the player. Defaults to 300px.}
}
\value{
The UI will provide a player with a play/pause button, previous and next buttons, duration information
and a volume slider.

The server-side module will return a list of reactive objects:
\describe{
\item{playing}{Logical value whether or not the player is currently playing}
\item{track}{Name of the track currently loaded}
\item{duration}{Duration (in seconds) of the track currently loaded}
\item{seek}{Current position (in seconds) of the track currently loaded}
}
}
\description{
A simple module containing a howler player and a default set of howler buttons. The module also contains the
current position of the track being played and the duration of the track.
}
\examples{
 \dontrun{
if (interactive()) {
  sound_files <- file.path("audio", list.files(EXAMPLE_AUDIO_DIR)[1:2])
  addResourcePath("audio", EXAMPLE_AUDIO_DIR)

  ui <- fluidPage(
    title = "howler.js Module",
    h3("Full module"),
    howlerModuleUI("howl", sound_files),
    br(),
    h3("Simple module"),
    howlerBasicModuleUI("howl2", sound_files[1])
  )

  server <- function(input, output, session) {
    howlerModuleServer("howl")
    howlerModuleServer("howl2")
  }

  shinyApp(ui, server)
}
}
}
\keyword{internal}
