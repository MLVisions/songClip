
# Module UI ---------------------------------------------------------------

#' Create shiny module for tuning an audio file
#' @describeIn tune_audio_ui Create module UI for tuning an audio file
#'
#' @keywords internal
tune_audio_ui <- function(id){

  ns <- shiny::NS(id)

  tagList(
    shinydashboardPlus::box(
      title = "Tune Audio",
      solidHeader = TRUE,
      width = 12,
      collapsible = FALSE,
      collapsed = FALSE,
      status = "primary",
      fluidRow(
        column(
          width = 12,
          shinyWidgets::pickerInput(ns("audio_select"), "Select an audio file", choices = c()),
          bslib::navset_card_pill(
            placement = "above",
            ### Cropping, looping, and speed ###
            bslib::nav_panel(
              # TODO: make better title for this
              title = "Cropping, Looping, & Speed",
              audio_playpack_ui(ns("audio_playback"))
            ),
            ### Equalizer ###
            bslib::nav_panel(
              title = "Equalizer",
              # also add labels for adjusting Trebble, Bass, etc.
              make_equalizer_ui(ns("equalizer"))
            ),
            bslib::nav_spacer(),
            bslib::nav_menu(
              title = "Options",
              align = "right",
              bslib::nav_panel(
                title = "Save Audio as MP3", icon = icon("music"),
                h3("Save tuned audio as a new MP3 file")
                # TODO: make module (simple) for saving editted video out as MP3
                # - I dont think this will include equalizer settings (I think those will
                #   happen *over* the audio file, rather than actually modifying the file itself),
                #   however those settings could more easily be cached in the other module.
              ),
              bslib::nav_panel(
                title = "Cache Settings", icon = icon("list-check"),
                # TODO: move text to separate file
                # TODO: make module for saving settings out as a yaml, mapped to file path.
                # We cant save settings across R sessions, but there is a work around:
                # - User will have to choose a directory to store saved yamls.
                # - They will have to set this directory every time, as there is no persistent
                #   storage across R sessions.
                # - Upon loading the directory, we can read in all yamls and perform some checks.
                # - There can be multiple cached scenarios saved per song. This loading should be fast.
                # TODO: make UI for setting caching directory on load
                h3("Save the current tuning settings for this song"),
                p("Upon loading this song in the future (must be in the same file location),
                  you will be able to choose this scenario.")
              )
            )
          )
        )
      )
    )
  )
}


# Module Server -----------------------------------------------------------

#' @describeIn tune_audio_ui Create module server for tuning an audio file
#'
#' @param audio_choices reactive list of audio files and their parameters
#' @param audio_dir reactive audio directory (selected)
#'
#' @keywords internal
tune_audio_server <- function(id, audio_choices, audio_dir) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    .rv <- reactiveValues()

    # TODO: use shinyjs to disable play button if tuneR::getWavPlayer() is not set
    # Add alert and potentially allow user to search for it (low priority)
    observe({
      audio_player_set <-
        !is.null(tuneR::getWavPlayer()) &&
        nzchar(tuneR::getWavPlayer()) &&
        fs::file_exists(tuneR::getWavPlayer())

      if(isFALSE(audio_player_set)){
        # shinyjs::disable()
      }
    })

    # Set audio choices with their info based on audio library
    observeEvent(audio_choices(), {
      audio_choices <- shiny::req(audio_choices())
      # Update Choices
      shinyWidgets::updatePickerInput(
        session = session, "audio_select", choices = audio_choices$choice_name,
        selected = audio_choices$choice_name[1],
        choicesOpt = list(icon = audio_choices$icon, subtext = audio_choices$subtext),
        options = shinyWidgets::pickerOptions(container = "body",iconBase = "fas"),
      )
    })





    # Cropping, Looping, & Speed ----------------------------------------------

    audio_select <- reactive(input$audio_select)

    audio_playpack <- audio_playpack_server("audio_playback",
                                            audio_choices = audio_choices,
                                            audio_dir = audio_dir,
                                            audio_select = audio_select
    )

    # update input$audio_select if track is changed via next/previous buttons.
    observeEvent(audio_playpack$selected_track(), {
      selected_track <- shiny::req(audio_playpack$selected_track())
      if(selected_track != input$audio_select){
        shinyWidgets::updatePickerInput(
          session = session, "audio_select", selected = selected_track
        )
      }
    })


    # Equalizer ---------------------------------------------------------------

    equalizer <- make_equalizer_server("equalizer")
    equalizer_data <- reactive({equalizer$eq_data()})

    observe({
      eq_data <- shiny::req(equalizer_data())
    })


    return(
      list(

      )
    )

  })
}


setup_audio <- function(audio_path){
  # www_dir <- system.file("www", package = "songClip", mustWork = TRUE)
  # path_play <- file.path(www_dir, "play.wav")

  audio_obj <- tuneR::readMP3(here::here(audio_path))
  # tuneR::writeWave(audio_obj, path_play)

  return(audio_obj)
}
