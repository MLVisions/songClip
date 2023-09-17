# Module UI ---------------------------------------------------------------

#' Create shiny module for saving youtube videos as audio files
#' @describeIn download_audio_ui Create module UI for saving youtube videos as audio files
#'
#' @keywords internal
audio_playpack_ui <- function(id) {
  ns <- NS(id)

  tagList(
    br(),
    fluidRow(
      style = "background-color: #252525; padding-bottom: 1em;
                margin-right: 0px; margin-left: 0px;",
      # Inputs
      fluidRow(
        style = "margin-right: 0px; margin-left: 0px;",
        br(),
        column(
          width = 4,
          uiOutput(ns("audio_playback_controls"))
        )
      ),
      plotly::plotlyOutput(ns("audio_plot")) %>% withSpinner(color="#086A87")
    )
  )
}


# Module Server -----------------------------------------------------------

#' @describeIn download_audio_ui Create module server for saving youtube videos as audio files
#'
#'
#' @keywords internal
audio_playpack_server <- function(id, audio_choices, audio_dir, audio_select) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      .rv <- reactiveValues()

      # TODO: figure out if this works when the package is installed
      observeEvent(audio_dir(), {
        addResourcePath("SONGCLIP_audio_library", shiny::req(audio_dir()))
      })


      # Circular shift the order of the songs based on which one is selected
      sorted_audio_files <- reactive({
        sorted_choices <- shift_selected_song(
          audio_select = shiny::req(audio_select()),
          audio_choices = shiny::req(audio_choices())
        )

        file.path("SONGCLIP_audio_library", basename(sorted_choices))
      })



      output$audio_playback_controls <- renderUI({
        # call audio_obj to refresh the stored wav file
        shiny::req(audio_obj())

        audio_files <- shiny::req(sorted_audio_files())
        audio_selected <- file.path("SONGCLIP_audio_library", shiny::req(audio_select()))

        howlerBasicModuleUI("howler_ui", audio_selected)
        # howlerModuleUI("howler_ui", audio_files,  include_current_track = TRUE)
        # This allows playback
        # tags$audio(
        #   src = file.path(src_name, "play.wav"), type = "audio/wav",
        #   controls = NA
        # )
      })

      track_song_info <- howlerModuleServer("howler_ui")



      # Wave Channel Plot -------------------------------------------------------


      # Audio path
      audio_path <- reactive({
        file.path(shiny::req(audio_dir()), shiny::req(audio_select()))
      })

      # Load Chosen audio object
      audio_obj <- reactive({
        audio_path <- shiny::req(audio_path())
        # Save loaded audio file to inst/www/
        setup_audio(audio_path)
      })

      # Audio inspection plot
      audio_plot <- reactive({
        audio_obj <- shiny::req(audio_obj())
        # assign("audio_obj", audio_obj, envir = .GlobalEnv)
        plot_wave_audio(audio_obj) %>% add_play_tracker_line()
      })

      output$audio_plot <- plotly::renderPlotly({
        audio_plot()
      })

    }
  )
}


#' Circular shift the order of the songs based on which one is selected
#'
#' @param audio_select the `basename` of the file selected
#' @param audio_choices vector of file paths
shift_selected_song <- function(audio_select, audio_choices){

  checkmate::assert_true(any(grepl(audio_select, audio_choices$choice_name)))

  shifter <- function(x, n = 1) {
    if (n == 0) x else c(tail(x, -n), head(x, n))
  }

  loc <- grep(audio_select, audio_choices$choice_name)
  if(length(loc) > 1) loc <- loc[1]

  shifter(audio_choices$choice, loc-1)
}
