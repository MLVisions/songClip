# Module UI ---------------------------------------------------------------

#' Create shiny module for altering the playback of an audio track
#' @describeIn audio_playpack_ui Create module UI for altering the playback of an audio track
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
          width = 6,
          uiOutput(ns("audio_playback_controls"))
        ),
        column(
          width = 3, offset = 3,
          shinyWidgets::dropdown(
            label = "Editing Tools",
            width = "260px",
            right = TRUE,
            icon = icon("gear"),
            status = "primary",
            fluidRow(
              column(
                width = 12, align = "right",
                shinyWidgets::pickerInput(
                  ns("channel_type"), "Wave Channel:",
                  options = shinyWidgets::pickerOptions(
                    container = "body", style = "btn-primary"),
                  choices = list("Single Channel" = c("Left"="left", "Right"="right"),
                                 "Multiple Channels"= c("Stereo" = "stereo")),
                  selected = "left", inline = TRUE, width = "fit"
                )
              )
            ),
            hr(),
            h4("Editing Tools"),
            shinyWidgets::switchInput(
              ns("create_loop"), label = tags$span(
                "Create Loop",
                icon("arrows-spin", verify_fa = FALSE),
                style = "display:inline-block;"
              ),
              width = "220px", inline = TRUE,
              labelWidth = 180,
              onStatus = "success"
            ),
            conditionalPanel("input.create_loop", ns = ns, {
              fluidRow(
                style = "padding-left: 15px;",
                column(
                  width = 12,
                  shinyWidgets::awesomeCheckbox(
                    ns("dynamic_loop"), "Reset on adjustment",
                    value = FALSE
                  )
                )
              )
            }),
            shinyWidgets::switchInput(
              ns("crop_audio"), label = tags$span(
                "Crop Audio",
                icon("crop-simple", verify_fa = FALSE),
                style = "display:inline-block;"
              ),
              width = "220px", inline = TRUE,
              labelWidth = 180,
              onStatus = "success"
            )
          )
        )
      ),
      plotly::plotlyOutput(ns("audio_plot")) %>% withSpinner(color="#086A87"),
      fluidRow(
        style = "margin-right: 0px; margin-left: 0px; color: white;",
        column(
          width = 10, offset = 1,
          conditionalPanel("input.create_loop", ns = ns, {
            tagList(
              h3("Editing: Create a loop"),
              sliderInput(
                ns("loop_range"), label = NULL, value = c(0, 60), min = 0, max = 60,
                timeFormat = "%M:%S (%L ms)", dragRange = TRUE
              )
            )
          }),
          conditionalPanel("input.create_loop", ns = ns, {
            tagList(
              h3("Editing: Crop Audio"),
              sliderInput(
                ns("loop_range"), label = NULL, value = c(0, 60), min = 0, max = 60,
                timeFormat = "%M:%S (%L ms)", dragRange = TRUE
              )
            )
          })
        )
      )
    )
  )
}


# Module Server -----------------------------------------------------------

#' @describeIn audio_playpack_ui Create module server for altering the playback of an audio track
#'
#'
#' @keywords internal
audio_playpack_server <- function(id, audio_choices, audio_dir, audio_select) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      .rv <- reactiveValues(playing = NULL, track = NULL, duration = NULL, seek = NULL)
      .rv_loop <- reactiveValues(start = 0, end = 60)



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
        make_howler_ui(audio_files, howler_id = ns("howler"),
                       seek_ping_rate = 100)
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
        setup_audio(audio_path)
      })

      # Audio inspection plot
      audio_plot <- reactive({
        audio_obj <- shiny::req(audio_obj())
        assign("audio_obj", audio_obj, envir = .GlobalEnv)
        # update value of tracker on client side (always start at 0 for new audio object)
        plot_wave_audio(audio_obj, type = input$channel_type, source = "wave_audio")
      })

      # Rendered audio inspection plot
      output$audio_plot <- plotly::renderPlotly({
        audio_plot()
      })


      # Reset settings on new object
      observeEvent(audio_obj(), {
        # Reset playback
        seekHowl("howler", 0)
      }, priority = 4)

      # Reset settings on new object -or- editing toggles
      observeEvent(list(audio_obj(), input$create_loop), {
        duration <- get_audio_dur(shiny::req(audio_obj()))
        dur_fmt <- format_seconds(duration)
        updateSliderInput(session, "loop_range", value = dur_fmt,
                          timeFormat = "%M:%S (%L ms)",
                          min = min(dur_fmt), max = max(dur_fmt), step = 0.5)
        .rv_loop$start <- min(duration)
        .rv_loop$end <- max(duration)
      }, priority = 3)

      # Reset settings on new object -or- channel type
      observeEvent(list(audio_obj(), input$channel_type), {
        shinyWidgets::updateSwitchInput(session, "create_loop", value = FALSE)
      }, priority = 3)



      # Proxy Updates -----------------------------------------------------------

      ### Main Playback Tracker ###

      # Update playback line tracker and handle end of loop
      observeEvent(input$howler_seek, {
        # Update playback line tracker location on client side
        seek_value <- input$howler_seek
        # Get proxy reference
        proxy <-  plotly::plotlyProxy("audio_plot", session, deferUntilFlush = FALSE)
        # Update the red line's position (in minutes)
        add_play_tracker_line(proxy = proxy, x_val = seek_value/60)

        # Loop handling
        # Set tolerance (sec) for loop (should be larger than `seek_ping_rate`)
        tol <- 1.1e-1
        x_range <- as.numeric(input$loop_range)
        if(isTRUE(input$create_loop) && (abs(seek_value - x_range[2]) <= tol)){
          seekHowl("howler", x_range[1])
        }
      })

      ### Looping ###

      # Add shapes for creating loop on client side
      observeEvent(input$create_loop, {
        # Get proxy reference
        proxy <-  plotly::plotlyProxy("audio_plot", session, deferUntilFlush = TRUE)
        channel_type <- shiny::req(input$channel_type)

        if(isTRUE(input$create_loop)){
          # Create loop (in seconds)
          x_range <- get_audio_dur(shiny::req(audio_obj()))/60
          y_val <- min(get_audio_limits(shiny::req(audio_obj())))
          toggle_loop_trackers(proxy = proxy, toggle = TRUE,
                               channel_type = channel_type,
                               x_range = x_range, y_val = y_val)
        }else{
          # remove loop (doesnt work with stereo atm)
          toggle_loop_trackers(proxy = proxy, toggle = FALSE,
                               channel_type = channel_type)
        }
      }, ignoreInit = TRUE, priority = 2)


      # Updating loop trackers
      observeEvent(input$loop_range, {
        if(isTRUE(input$create_loop)){
          # Get proxy reference
          proxy <-  plotly::plotlyProxy("audio_plot", session, deferUntilFlush = TRUE)
          channel_type <- shiny::req(input$channel_type)

          # Move loop (in seconds) - delete the old one for now
          toggle_loop_trackers(proxy = proxy, toggle = FALSE,
                               channel_type = channel_type)
          x_range <- as.numeric(input$loop_range)
          y_val <- min(get_audio_limits(shiny::req(audio_obj())))
          toggle_loop_trackers(proxy = proxy, toggle = TRUE,
                               channel_type = channel_type,
                               x_range = x_range/60, y_val = y_val)
          .rv_loop$start <- min(x_range)
          .rv_loop$end <- max(x_range)
        }
      }, priority = 1)

      # Update playback when start time is moved or `dynamic_loop` is set
      observeEvent(.rv_loop$start, {
        reset_loop <- input$dynamic_loop || isFALSE(input$howler_playing)
        if(isTRUE(input$create_loop) && reset_loop){
          # Get proxy reference
          proxy <-  plotly::plotlyProxy("audio_plot", session, deferUntilFlush = TRUE)
          channel_type <- shiny::req(input$channel_type)
          # Update play tracking to start at beginning of loop
          x_range <- as.numeric(input$loop_range)
          seekHowl("howler", x_range[1])
        }
      })


      return(
        list(
          selected_track = reactive({selected_track()})
        )
      )

    }
  )
}

