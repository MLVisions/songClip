
#' Make howler UI for use in \code{\link{audio_playpack_ui}}
#'
#' @param files Files that will be used in the player.
#' @param howler_id ID to correspond to the howler object and its related buttons.
#' @param seek_ping_rate Number of milliseconds between each update of `input${id}_seek` while playing. Default is
#' set to 1000. If set to 0, then `input${id}_seek` will not exist. Decreasing this number too much could increase
#' the cross-talk between the server and client to the point where significant lags occur.
#' @param include_current_track Logical, should the current track be included in the UI of the module?
#' @param width Width (in pixels) of the player. Defaults to 300px.
#'
#' @keywords internal
make_howler_ui <- function(
    files,
    howler_id = "howler",
    seek_ping_rate = 100,
    include_current_track = TRUE,
    width = NULL,
    options = list(),
    ...
){

  # Fill full width if not specified
  width <- if(is.null(width)) "-webkit-fill-available" else width

  div(
    class = "howler-module",
    style = paste0("width:", width, ";"),
    howler(elementId = howler_id, tracks = files, auto_continue = FALSE,
           seek_ping_rate = seek_ping_rate, options = options, ...),
    div(
      class = "howler-module-container",
      if (include_current_track) howlerCurrentTrack(howler_id),
      howlerSeekSlider(howler_id),
      div(
        class = "howler-module-settings",
        div(
          class = "howler-module-buttons",
          if (length(files) > 1) howlerPreviousButton(howler_id),
          howlerPlayPauseButton(howler_id),
          if (length(files) > 1) howlerNextButton(howler_id)
        ),
        span(
          class = "howler-module-duration",
          howlerSeekTime(howler_id),
          "/",
          howlerDurationTime(howler_id)
        ),
        div(
          class = "howler-module-volume",
          howlerVolumeSlider(howler_id)
        )
      )
    )
  )
}




# Howler Module -----------------------------------------------------------

#' Howler.js Module
#'
#' @description
#' A simple module containing a howler player and a default set of howler buttons. The module also contains the
#' current position of the track being played and the duration of the track.
#'
#' @param id ID to give to the namespace of the module. The howler player will have the ID \code{{id}-howler}.
#' @param files Files that will be used in the player. This can either be a single vector, or a list where different
#' formats of the same file are kept in each element of the list.
#' @param ... Further arguments to send to \code{\link{howler}}
#' @param include_current_track Logical, should the current track be included in the UI of the module?
#' @param width Width (in pixels) of the player. Defaults to 300px.
#'
#' @return
#' The UI will provide a player with a play/pause button, previous and next buttons, duration information
#' and a volume slider.
#'
#' The server-side module will return a list of reactive objects:
#' \describe{
#' \item{playing}{Logical value whether or not the player is currently playing}
#' \item{track}{Name of the track currently loaded}
#' \item{duration}{Duration (in seconds) of the track currently loaded}
#' \item{seek}{Current position (in seconds) of the track currently loaded}
#' }
#'
#' @examples
#' if (interactive()) {
#'   sound_files <- file.path("audio", list.files(EXAMPLE_AUDIO_DIR)[1:2])
#'   addResourcePath("audio", EXAMPLE_AUDIO_DIR)
#'
#'   ui <- fluidPage(
#'     title = "howler.js Module",
#'     h3("Full module"),
#'     howlerModuleUI("howl", sound_files),
#'     br(),
#'     h3("Simple module"),
#'     howlerBasicModuleUI("howl2", sound_files[1])
#'   )
#'
#'   server <- function(input, output, session) {
#'     howlerModuleServer("howl")
#'     howlerModuleServer("howl2")
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
#' @name howlerModule
#' @rdname howlerModule
#' @keywords internal
howlerModuleUI <- function(id, files, ..., include_current_track = TRUE, width = "300px") {
  ns <- NS(id)
  howler_id <- ns("howler")

  div(
    class = "howler-module",
    style = paste0("width:", width, ";"),
    howler(elementId = howler_id, tracks = files, ...),
    div(
      class = "howler-module-container",
      if (include_current_track) howlerCurrentTrack(howler_id),
      howlerSeekSlider(howler_id),
      div(
        class = "howler-module-settings",
        div(
          class = "howler-module-buttons",
          if (length(files) > 1) howlerPreviousButton(howler_id),
          howlerPlayPauseButton(howler_id),
          if (length(files) > 1) howlerNextButton(howler_id)
        ),
        span(
          class = "howler-module-duration",
          howlerSeekTime(howler_id),
          "/",
          howlerDurationTime(howler_id)
        ),
        div(
          class = "howler-module-volume",
          howlerVolumeSlider(howler_id)
        )
      )
    )
  )
}

#' @rdname howlerModule
#' @keywords internal
howlerBasicModuleUI <- function(id, files, ..., width = "300px") {
  if (length(files) > 1) stop("Only one file can be included in the basic module")
  # ns <- NS(id)

  id_suffix <- fs::path_ext_remove(basename(files))
  howler_id <- paste0(id,"_howler_", id_suffix)

  div(
    class = "howler-module howler-basic-module",
    style = paste0("width:", width, ";"),
    howler(elementId = howler_id, tracks = files, ...),
    div(
      class = "howler-module-container",
      div(
        class = "howler-module-settings",
        div(
          class = "howler-module-buttons",
          howlerPlayPauseButton(howler_id),
        ),
        span(
          class = "howler-module-duration",
          howlerSeekTime(howler_id),
          "/",
          howlerDurationTime(howler_id)
        ),
        div(
          class = "howler-module-seek",
          howlerSeekSlider(howler_id)
        ),
        div(
          class = "howler-module-volume",
          howlerVolumeSlider(howler_id, button = FALSE),
          howlerVolumeToggleButton(howler_id)
        )
      )
    )
  )
}

#' @rdname howlerModule
#' @keywords internal
howlerModuleServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      return(
        list(
          playing = reactive(shiny::req(input$howler_playing)),
          track = reactive(shiny::req(input$howler_track)),
          duration = reactive(shiny::req(input$howler_duration)),
          seek = reactive(shiny::req(input$howler_seek))
        )
      )
    }
  )
}



# Main howler functions ---------------------------------------------------

#' Create a Howler Audio Player
#'
#' @description
#' \code{howler} is used to initialise the 'howler.js' framework by adding all of the specified tracks to the
#' player, and can be run by either including UI buttons or server-side actions.
#'
#' @param tracks A named vector of file paths to sounds. If multiple file extensions are included, then use a named
#' list instead, with each list item containing each extension of the sound.
#' @param options A named list of options to add to the player. For a full list of options see
#' \url{https://github.com/goldfire/howler.js}
#' @param track_formats An optional list of formats of the sounds. By default 'howler' will guess the format to
#' play in. Must be the same length as tracks
#' @param auto_continue If there are multiple files, would you like to auto play the next file after the current
#' one has finished? Defaults to `FALSE`
#' @param auto_loop Once all files have been played, would you like to restart playing the playlist?
#' Defaults to `FALSE`
#' @param seek_ping_rate Number of milliseconds between each update of `input${id}_seek` while playing. Default is
#' set to 1000. If set to 0, then `input${id}_seek` will not exist. Decreasing this number too much could increase
#' the cross-talk between the server and client to the point where significant lags occur.
#' @param elementId HTML id tag to be given to the howler player element
#'
#' @details
#' All buttons associated with the `howler` should be given the same \code{id} argument. This is to ensure
#' that the buttons are linked to the player.
#'
#' i.e. If `howler(id = "howler")`, then `howlerPlayButton(id = "howler")`
#'
#' @return
#' A shiny.tag containing all of the required options for a `Howl` JS object to be initialized in a shiny application.
#'
#' On the server side there will be up to four additional objects available as inputs:
#' \describe{
#' \item{\code{\{id\}_playing}}{A logical value as to whether or not the `howler` is playing audio}
#' \item{\code{\{id\}_track}}{Basename of the file currently loaded}
#' \item{\code{\{id\}_seek}}{(If `seek_ping_rate > 0`) the current time (in seconds) of the track loaded.
#'  Returns a named list.}
#' \item{\code{\{id\}_duration}}{The duration (in seconds) of the track loaded}
#' }
#'
#' @examples
#'
#' \dontrun{
#' if (interactive()) {
#'   library(shiny)
#'
#'   sound_file <- list.files(EXAMPLE_AUDIO_DIR)[1]
#'   addResourcePath("audio", EXAMPLE_AUDIO_DIR)
#'
#'   ui <- fluidPage(
#'     title = "howler.js Player",
#'     howler(elementId = "howler", c(sound = file.path("audio", sound_file))),
#'     howlerPlayPauseButton("howler")
#'   )
#'
#'   server <- function(input, output) {
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
#'
#' # Multiple file formats
#' howler(
#'   elementId = "howler",
#'   list(
#'     track_1 = c("audio/sound.webm", "audio/sound.mp3"),
#'     track_2 = c("audio/sound2.webm", "audio/sound2.mp3"),
#'   )
#' )
#' }
#'
#' @seealso \code{\link{howlerButton}}, \code{\link{howlerServer}}
#'
#' @import htmlwidgets
#' @import shiny
#'
#' @keywords internal
howler <- function(tracks, options = list(), track_formats = NULL,
                   auto_continue = FALSE, auto_loop = FALSE, seek_ping_rate = 1000, elementId = NULL) {

  if (!(is.null(track_formats) || length(tracks) == length(track_formats))) {
    stop("Track formats must be the same length as tracks")
  }

  if (seek_ping_rate < 0) {
    stop("Seek ping rate cannot be negative")
  }

  if (is.null(names(tracks))) {
    track_names <- vapply(
      tracks,
      function(x) sub("\\.[^\\.]+$", "", basename(x[1])),
      character(1),
      USE.NAMES = FALSE
    )
  } else {
    track_names <- names(tracks)
  }

  settings <- list(
    tracks = as.list(unname(tracks)),
    names = as.list(track_names),
    formats = as.list(track_formats),
    auto_continue = auto_continue,
    auto_loop = auto_loop,
    seek_ping_rate = seek_ping_rate,
    options = options
  )

  settings <- settings[!vapply(settings, is.null, logical(1))]

  htmlwidgets::createWidget(
    name = "howler",
    x = settings,
    width = "0",
    height = "0",
    package = "songClip",
    elementId = elementId
  )
}

#' Shiny bindings for howler
#'
#' Output and render functions for using howler within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param expr An expression that generates a howler
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @return
#' An output or render function that enables the use of the widget within Shiny applications.
#'
#' @name howler-shiny
#' @keywords internal
howlerOutput <- function(outputId) {
  htmlwidgets::shinyWidgetOutput(outputId, 'howler', package = 'songClip')
}

#' @rdname howler-shiny
#' @keywords internal
renderHowler <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, howlerOutput, env, quoted = TRUE)
}

widget_html.howler <- function(id, style, class, ...) {
  tags$audio(
    id = id,
    style = style,
    class = class,
    ...
  )
}



# Audio Buttons -----------------------------------------------------------


#' Audio Buttons
#'
#' @description
#' Buttons that can be used to interact with the \code{\link{howler}}.
#'
#' \code{howlerPlayButton}, \code{howlerPauseButton}, \code{howlerPlayPauseButton} and
#' \code{howlerStopButton} will all be applied to the current track.
#'
#' \code{howlerBackButton} and \code{howlerForwardButton} will
#' change the track position by a specified amount of time.
#'
#' \code{howlerPreviousButton} and \code{howlerNextButton} will
#' play the previous/following track supplied to the player.
#'
#' \code{howlerVolumeDownButton} and \code{howlerVolumeUpButton} will
#' change the volume of the player by a specified percentage.
#'
#' \code{howlerButton} is a customisable version of any of the above individual button.
#'
#' @param howler_id ID given to the \code{\link{howler}} player.
#' @param button_type Type of button to create. Available buttons are in the details, default set to \code{play_pause}.
#' @param ... Attributes/Inner tags added to the button
#'
#' @return
#' An HTML tag containing the audio button.
#'
#' An additional input will be available in the server side in the form \code{\{id\}_\{button_type\}}. For example
#' \code{howlerBackButton("howler")} will create an input element of \code{input$howler_back}. All of these will work in
#' the same way as \code{\link[shiny]{actionButton}}
#'
#' @details
#' The following \code{button_type} are available to create:
#'
#' \describe{
#' \item{\code{play_pause}}{(default) Switch between playing and pausing the track}
#' \item{\code{play}}{Resumes the current track}
#' \item{\code{pause}}{Pauses the current track}
#' \item{\code{stop}}{Stops current track, when played will start from beginning}
#' \item{\code{previous},\code{next}}{Switches to the previous/following track}
#' \item{\code{volumedown},\code{volumeup}}{Decreases/Increases the volume by 10\%
#' (If using \code{howlerButton} include the attribute \code{`data-volume-change`})}
#' \item{\code{back},\code{forward}}{Seek forward/backwards 10s
#' (If using \code{howlerButton} include the attribute \code{`data-seek-change`} with negative values to go backwards)}
#' }
#'
#' When using a \code{play_pause} button, the icon will toggle between the play and pause button
#' depending on whether or not the track is playing.
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   sound_file <- list.files(EXAMPLE_AUDIO_DIR)[1]
#'   addResourcePath("audio", EXAMPLE_AUDIO_DIR)
#'
#'   ui <- fluidPage(
#'     tile = "howler.js Player",
#'     howler(elementId = "howler", file.path("audio", sound_file)),
#'     howlerPreviousButton("howler"),
#'     howlerBackButton("howler"),
#'     howlerPlayPauseButton("howler"),
#'     howlerForwardButton("howler"),
#'     howlerNextButton("howler"),
#'     howlerVolumeDownButton("howler"),
#'     howlerVolumeUpButton("howler")
#'   )
#'
#'   server <- function(input, output) {
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
#' @rdname howlerButton
#' @keywords internal
howlerButton <- function(howler_id, button_type = HOWLER_BUTTON_TYPES, ...) {
  button_type <- match.arg(button_type)

  tags$button(
    `data-howler` = howler_id,
    class = paste0("action-button howler-button howler-", button_type, "-button"),
    `aria-label` = button_type,
    title = button_type,
    type = "button",
    ...
  )
}

#' @rdname howlerButton
#' @keywords internal
howlerPlayButton <- function(howler_id) {
  howlerButton(howler_id, "play", shiny::icon("play"))
}

#' @rdname howlerButton
#' @keywords internal
howlerPauseButton <- function(howler_id) {
  howlerButton(howler_id, "pause", shiny::icon("pause"))
}

#' @rdname howlerButton
#' @keywords internal
howlerPlayPauseButton <- function(howler_id) {
  btn <- howlerButton(howler_id, "play_pause", shiny::icon("play"))
  btn$attribs$`aria-label` <- "play"
  btn$attribs$title <- "play"
  btn
}

#' @rdname howlerButton
#' @keywords internal
howlerStopButton <- function(howler_id) {
  howlerButton(howler_id, "stop", shiny::icon("stop"))
}

#' @param seek_change Time (in seconds) to move forward/backward the track when clicked. Default is 10 seconds
#'
#' @rdname howlerButton
#' @keywords internal
howlerBackButton <- function(howler_id, seek_change = 10) {
  howlerButton(howler_id, "back", shiny::icon("backward"), `data-seek-change` = -abs(seek_change))
}

#' @rdname howlerButton
#' @keywords internal
howlerForwardButton <- function(howler_id, seek_change = 10) {
  howlerButton(howler_id, "forward", shiny::icon("forward"), `data-seek-change` = seek_change)
}

#' @rdname howlerButton
#' @keywords internal
howlerPreviousButton <- function(howler_id) {
  howlerButton(howler_id, "previous", shiny::icon("step-backward"))
}

#' @rdname howlerButton
#' @keywords internal
howlerNextButton <- function(howler_id) {
  howlerButton(howler_id, "next", shiny::icon("step-forward"))
}

#' @param volume_change How much to change the volume by. Default is 10%.
#'
#' @rdname howlerButton
#' @keywords internal
howlerVolumeUpButton <- function(howler_id, volume_change = 0.1) {
  howlerButton(howler_id, "volumeup", shiny::icon("volume-up"), `data-volume-change` = volume_change)
}

#' @rdname howlerButton
#' @keywords internal
howlerVolumeDownButton <- function(howler_id, volume_change = 0.1) {
  howlerButton(howler_id, "volumedown", shiny::icon("volume-down"), `data-volume-change` = volume_change)
}

#' @rdname howlerButton
#' @keywords internal
howlerVolumeToggleButton <- function(howler_id) {
  howlerButton(howler_id, "volumetoggle", shiny::icon("volume-up"))
}

HOWLER_BUTTON_TYPES <- c(
  "play_pause", "play", "pause", "stop", "previous", "next",
  "volumeup", "volumedown", "volumetoggle", "forward", "back"
)



# Track Track UI ----------------------------------------------------------



#' Current Track
#'
#' @description
#' A way to display track information in the UI without having to communicate with the server.
#'
#' @param id ID given to the current track label. For it to work with the \code{\link{howler}}, the ID
#' must match that of the \code{howler}.
#' @param ... Other attributes to give to the HTML tag.
#'
#' @return
#' A \code{div} tag that will be linked to the \code{\link{howler}} to show the current track.
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'
#'   ui <- fluidPage(
#'     title = "howler.js Player",
#'     howler(elementId = "sound", "audio/sound.mp3"),
#'     howlerCurrentTrack("sound"),
#'     p(
#'       howlerSeekTime("sound"),
#'       "/",
#'       howlerDurationTime("sound")
#'     ),
#'     howlerPlayPauseButton("sound")
#'   )
#'
#'   server <- function(input, output, session) {
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
#' @rdname howler_meta
#' @keywords internal
howlerCurrentTrack <- function(id, ...) {
  div(class = "howler-current-track", `data-howler` = id, ...)
}

#' @rdname howler_meta
#' @keywords internal
howlerSeekTime <- function(id, ...) {
  span(class = "howler-seek", `data-howler` = id, ...)
}

#' @rdname howler_meta
#' @keywords internal
howlerDurationTime <- function(id, ...) {
  span(class = "howler-duration", `data-howler` = id, ...)
}



# Control Track Server ----------------------------------------------------


#' Update howler.js Server-Side
#'
#' @description
#' Change the state of the howler player from the server.
#'
#' \code{playHowl}, \code{pauseHowl}, \code{togglePlayHowl} and \code{stopHowl}
#' will all be applied to the current track.
#'
#' \code{changeTrack} will update the track to the file specified.
#'
#' \code{addTrack} will add a new track to the specified player.
#'
#' @param session Shiny session
#' @param id ID of the \code{howler} to update
#' @param track Either the track name of the file to change to, or the index of the file to play.
#' If the file is not included in the player nothing will happen.
#'
#'
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   sound_files <- file.path("audio", list.files(EXAMPLE_AUDIO_DIR))
#'   addResourcePath("audio", EXAMPLE_AUDIO_DIR)
#'
#'
#'   ui <- fluidPage(
#'     title = "howler.js Player",
#'     selectInput("track", "Select Track", basename(sound_files)),
#'     howler(elementId = "howler", sound_files),
#'     howlerPlayPauseButton("howler")
#'   )
#'
#'   server <- function(input, output) {
#'     observeEvent(input$track, changeTrack("howler", input$track))
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
#' @return
#' Updates the the state of the specified \code{howler} in the shiny application.
#'
#' @name howlerServer
#' @rdname howlerServer
#' @keywords internal
changeTrack <- function(id, track, play_track = FALSE, session = getDefaultReactiveDomain()) {
  message_name <- paste0("changeHowlerTrack_", session$ns(id))
  track_info <- list(
    track = track,
    play = play_track
  )
  session$sendCustomMessage(message_name, track_info)
}

#' @param play_track Logical, should the new track be played on addition?
#'
#' @rdname howlerServer
#' @keywords internal
addTrack <- function(id, track, play_track = FALSE, session = getDefaultReactiveDomain()) {
  if (is.null(names(track))) {
    track_name <- vapply(
      track,
      function(x) sub("\\.[^\\.]+$", "", basename(x[1])),
      character(1),
      USE.NAMES = FALSE
    )
  } else {
    track_name <- names(track)
  }

  message_name <- paste0("addHowlerTrack_", session$ns(id))
  track_info <- list(
    track = as.list(unname(track)),
    track_name = as.list(track_name),
    play = play_track
  )
  session$sendCustomMessage(message_name, track_info)
}

#' @rdname howlerServer
#' @keywords internal
playHowl <- function(id, session = getDefaultReactiveDomain()) {
  message_name <- paste0("playHowler_", session$ns(id))
  session$sendCustomMessage(message_name, id)
}

#' @rdname howlerServer
#' @keywords internal
pauseHowl <- function(id, session = getDefaultReactiveDomain()) {
  message_name <- paste0("pauseHowler_", session$ns(id))
  session$sendCustomMessage(message_name, id)
}

#' @rdname howlerServer
#' @keywords internal
togglePlayHowl <- function(id, session = getDefaultReactiveDomain()) {
  message_name <- paste0("togglePlayHowler_", session$ns(id))
  session$sendCustomMessage(message_name, id)
}

#' @rdname howlerServer
#' @keywords internal
stopHowl <- function(id, session = getDefaultReactiveDomain()) {
  message_name <- paste0("stopHowler_", session$ns(id))
  session$sendCustomMessage(message_name, id)
}

#' @param seek Time (in seconds) to set the position of the track
#' @rdname howlerServer
#' @keywords internal
seekHowl <- function(id, seek, session = getDefaultReactiveDomain()) {
  message_name <- paste0("seekHowler_", session$ns(id))
  session$sendCustomMessage(message_name, as.numeric(seek))
}



# Volume Slider -----------------------------------------------------------


#' Volume Slider
#'
#' @description
#' A more user friendly way to adjust the volume of the \code{howler} than by using buttons. There are
#' still volume up/down buttons, but a slider can be moved up/down as required.
#'
#' @param id ID given to the volume slider. For it to work with the \code{\link{howler}},
#'        the IDmust match that of the \code{howler}.
#' @param volume Initial volume to set the player at. Defaults at 100%
#' @param button Logical, should a mute toggle button be included next to the slider? Default is \code{TRUE}
#'
#' @return
#' A volume slider with a \code{\link{howlerVolumeDownButton}} and a \code{\link{howlerVolumeUpButton}} either side.
#'
#' @details
#' If using \code{howlerVolumeSlider}, avoid using the volume buttons, as this will cause duplicate IDs to appear in the
#' shiny application and prevents the slider from working properly.
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   sound_file <- file.path("audio", list.files(EXAMPLE_AUDIO_DIR))[1]
#'   addResourcePath("audio", EXAMPLE_AUDIO_DIR)
#'
#'   ui <- fluidPage(
#'     title = "howler.js Player",
#'     howler(elementId = "sound", sound_file),
#'     howlerPlayPauseButton("sound"),
#'     howlerVolumeSlider("sound")
#'   )
#'
#'   server <- function(input, output, session) {
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
#' @keywords internal
howlerVolumeSlider <- function(id, volume = 1, button = TRUE) {
  if (volume < 0 || volume > 1) {
    stop("Volume must be between 0 and 1")
  }

  tagList(
    if (button) howlerVolumeToggleButton(id),
    tags$input(
      class = "howler-volume-slider",
      `data-howler` = id,
      type = "range",
      min = 0,
      max = 1,
      step = 0.01,
      value = volume
    )
  )
}


#' Seek Slider
#'
#' @description
#' A UI element that can be included with a \code{\link{howler}} to manually change the location of the track.
#'
#' @param id ID given to the volume slider. For it to work with the \code{howler}, the ID
#' must match that of the \code{howler}.
#'
#' @return A slider element of class \code{howler-seek-slider} that will display the position of the current track
#' playing.
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   sound_file <- file.path("audio", list.files(EXAMPLE_AUDIO_DIR))[1]
#'   addResourcePath("audio", EXAMPLE_AUDIO_DIR)
#'
#'   ui <- fluidPage(
#'     title = "howler.js Player",
#'     howler(elementId = "sound", sound_file),
#'     howlerPlayPauseButton("sound"),
#'     howlerSeekSlider("sound")
#'   )
#'
#'   server <- function(input, output, session) {
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
#' @keywords internal
howlerSeekSlider <- function(id) {
  tags$input(
    class = "howler-seek-slider",
    `data-howler` = id,
    type = "range",
    min = 0,
    max = 100,
    step = 1,
    value = 0
  )
}

