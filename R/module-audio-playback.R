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
      .rv <- reactiveValues(playing = NULL, track = NULL, duration = NULL, seek = NULL)



      # Setup audio files -------------------------------------------------------


      # TODO: figure out if this works when the package is installed
      observeEvent(audio_dir(), {
        addResourcePath("SONGCLIP_audio_library", shiny::req(audio_dir()))
      })

      # audio paths relative to resource path
      audio_files <- reactive({
        file.path("SONGCLIP_audio_library", shiny::req(audio_choices()$choice_name))
      })



      # Main howler UI ----------------------------------------------------------


      output$audio_playback_controls <- renderUI({
        audio_files <- shiny::req(audio_files())
        make_howler_ui(audio_files, howler_id = ns("howler"))
        # This allows playback
        # tags$audio(
        #   src = file.path(src_name, "play.wav"), type = "audio/wav",
        #   controls = NA
        # )
      })


      observe({
        .rv$playing = shiny::req(input$howler_playing)
        .rv$track = shiny::req(input$howler_track)
        .rv$duration = shiny::req(input$howler_duration)
        .rv$seek = shiny::req(input$howler_seek)
      })


      # Track Changing ----------------------------------------------------------


      # Update selected track when either A) a new file is selected, or B) the
      # 'next'/'previous' buttons are clicked
      selected_track <- reactiveVal("")

      # Update when 'next'/'previous' buttons are clicked
      observeEvent(list(.rv$track, input$howler_track), {
        track_details <- input$howler_track
        new_track <- basename(audio_files()[track_details$id])
        selected_track(new_track)
      })

      # Update when new file is selected
      observeEvent(audio_select(), {
        new_track <- shiny::req(audio_select())
        current_track <- shiny::req(selected_track())
        # avoid running multiple times per track change
        if(current_track != new_track){
          selected_track(new_track)
          changeTrack("howler", new_track)
        }
      })


      observe({
        track_info <- reactiveValuesToList(.rv)
      })


      # Wave Channel Plot -------------------------------------------------------


      # Audio path
      audio_path <- reactive({
        file.path(shiny::req(audio_dir()), shiny::req(selected_track()))
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
        assign("audio_obj", audio_obj, envir = .GlobalEnv)
        # update value of tracker on client side (always start at 0 for new audio object)
        plot_wave_audio(audio_obj) %>% add_play_tracker_line(x_val = 0)
      })

      # Rendered audio inspection plot
      output$audio_plot <- plotly::renderPlotly({
        audio_plot()
      })


      # Update playback line tracker location on client side
      observeEvent(input$howler_seek, {
        seek_value <- input$howler_seek/60
        # Get proxy reference
        proxy <-  plotly::plotlyProxy("audio_plot", session, deferUntilFlush = FALSE)
        # Update the red line's position
        add_play_tracker_line(proxy = proxy, x_val = seek_value)
      })


      return(
        list(
          selected_track = reactive({selected_track()})
        )
      )

    }
  )
}

