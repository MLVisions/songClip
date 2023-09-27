
# Module UI ---------------------------------------------------------------

#' Create shiny module for tuning an audio file
#' @describeIn tune_audio_ui Create module UI for tuning an audio file
#'
#' @keywords internal
tune_audio_ui <- function(id){

  ns <- shiny::NS(id)

  tagList(
    fluidRow(
      column(
        width = 4,
        shinyWidgets::pickerInput(
          ns("audio_select"), "Current Track", choices = c(),
          options = shinyWidgets::pickerOptions(
            container = "body", style = "btn-primary"))
      ),
      column(
        width = 4, offset = 4, align = "right",
        import_audio_ui(ns("import_audio"))
      )
    ),
    tags$hr(class = "custom-hr"),
    easy_row(
      easy_col = TRUE,
      bslib::navset_card_pill(
        placement = "above",
        ### Playback ###
        bslib::nav_panel(
          # TODO: make better title for this
          title = "Playback",
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
            title = "YouTube to MP3", icon = icon("download"),
            download_audio_ui(ns("download_audio"))
          ),
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
}


# Module Server -----------------------------------------------------------

#' @describeIn tune_audio_ui Create module server for tuning an audio file
#'
#' @param audio_dir_init initial audio directory
#'
#' @keywords internal
tune_audio_server <- function(id, audio_dir_init) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    .rv <- reactiveValues()


    # Set up audio library and choices ----------------------------------------

    imported_audio <- import_audio_server("import_audio", audio_dir = audio_dir_init)

    # TODO: use shinyjs to disable play button if tuneR::getWavPlayer() is not set
    # and local audio player is being used. Add alert and potentially allow user to
    # search for it (low priority)
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
    observeEvent(imported_audio$audio_choices(), {
      audio_choices <- shiny::req(imported_audio$audio_choices())
      # Update Choices
      shinyWidgets::updatePickerInput(
        session = session, "audio_select", choices = audio_choices$choice_name,
        selected = audio_choices$choice_name[1],
        choicesOpt = list(icon = audio_choices$icon, subtext = audio_choices$subtext),
        options = shinyWidgets::pickerOptions(container = "body",iconBase = "fas"),
      )
    })


    # Audio playback control --------------------------------------------------


    audio_select <- reactive(input$audio_select)

    audio_playpack <- audio_playpack_server("audio_playback",
                                            audio_choices = imported_audio$audio_choices,
                                            audio_dir = imported_audio$audio_dir,
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


    # Convert & Download YouTube to MP3 ---------------------------------------

    # TODO: This should be a nested module (inside import_audio_server)
    dwnld_audio <- download_audio_server("download_audio")


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
