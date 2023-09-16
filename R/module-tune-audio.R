
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
              br(),
              fluidRow(
                style = "background-color: #252525; padding-bottom: 1em;
                margin-right: 0px; margin-left: 0px;",
                # Inputs
                fluidRow(
                  style = "margin-right: 0px; margin-left: 0px;",
                  br(),
                  column(
                    width = 2, offset = 10, align = "right",
                    uiOutput(ns("play_pause_btn_ui"))
                  )
                ),
                plotly::plotlyOutput(ns("audio_plot")) %>% withSpinner(color="#086A87")
              )
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
      audio_player_set <- nzchar(tuneR::getWavPlayer()) && fs::file_exists(tuneR::getWavPlayer())

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


    # Load Chosen audio object
    audio_obj <- reactive({
      audio_select <- shiny::req(input$audio_select, audio_dir())
      audio_path <- file.path(audio_dir(), audio_select)
      tuneR::readMP3(here::here(audio_path))
    })



    # Cropping, Looping, & Speed ----------------------------------------------


    ## Play & Pause Button ##
    output$play_pause_btn_ui <- renderUI({
      is_playing <- .rv$is_playing

      if(isTRUE(is_playing)){
        shinyWidgets::actionBttn(
          ns("pause"), "Pause", icon = icon("pause"),
          style = "material-flat", color = "primary", size = "sm"
        )
      }else{
        shinyWidgets::actionBttn(
          ns("play"), "Play", icon = icon("play"),
          style = "material-flat", color = "primary", size = "sm"
        )
      }
    })

    observeEvent(input$play, {
      .rv$is_playing <- TRUE
      tuneR::play(audio_obj())
    })
    observeEvent(input$pause, {
      .rv$is_playing <- FALSE
    })

    # Audio inspection plot
    audio_plot <- reactive({
      audio_obj <- shiny::req(audio_obj())
      assign("audio_obj", audio_obj, envir = .GlobalEnv)
      plot_wave_audio(audio_obj) %>% add_play_tracker_line()
    })

    output$audio_plot <- plotly::renderPlotly({
      audio_plot()
    })


    # Equalizer ---------------------------------------------------------------

    equalizer <- make_equalizer_server("equalizer")
    equalizer_data <- reactive({equalizer$eq_data()})

    observe({
      shiny::req(equalizer_data())
    })

    return(
      list(

      )
    )

  })
}

