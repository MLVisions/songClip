# Module UI ---------------------------------------------------------------

#' Create shiny module for altering the playback of an audio track
#' @describeIn audio_playpack_ui Create module UI for altering the playback of an audio track
#'
#' @keywords internal
audio_playpack_ui <- function(id) {
  ns <- NS(id)

  tagList(
    br(),
    no_margin_row(
      bg_color = "#252525",
      easy_col = TRUE,
      style = "padding-bottom: 1em;",
      # Inputs
      no_margin_row(
        color = "black",
        br(),
        column(
          width = 6,
          uiOutput(ns("audio_playback_controls"))
        ),
        column(
          width = 2, offset = 2, align = "right",
          shinyWidgets::dropdown(
            label = "View",
            width = "230px",
            right = TRUE,
            icon = icon("eye", verify_fa = FALSE),
            status = "primary",
            easy_row(
              easy_col = TRUE, align = "left", class = "shadow-wrapper",
              h4("View"),
              tags$hr(class = "custom-hr"),
              shinyWidgets::pickerInput(
                ns("channel_type"), "Wave Channel:",
                options = shinyWidgets::pickerOptions(
                  container = "body", style = "btn-primary"),
                choices = list("Single Channel" = c("Left"="left", "Right"="right"),
                               "Multiple Channels"= c("Stereo" = "stereo")),
                selected = "left", inline = TRUE, width = "fit"
              ),
              shinyWidgets::awesomeCheckbox(
                ns("show_info"), "Append information about the audio file",
                value = FALSE
              )
            )
          )
        ),
        column(
          width = 2, align = "right",
          shinyWidgets::dropdown(
            label = "Edit",
            width = "280px",
            right = TRUE,
            icon = icon("pen-to-square", verify_fa = FALSE),
            status = "primary",
            easy_row(
              class = "shadow-wrapper",
              easy_col = TRUE,
              easy_row(
                easy_col = TRUE, align = "left",
                h4("Editing"),
                tags$hr(class = "custom-hr")
              ),
              easy_row(
                easy_col = TRUE, align = "center",
                shinyWidgets::switchInput(
                  ns("enable_edits"), label = tags$span(
                    "Enable Editing",
                    icon("pen-to-square", verify_fa = FALSE)
                  ),
                  width = "240px", inline = TRUE, value = FALSE,
                  labelWidth = 220, onStatus = "success"
                ),
                conditionalPanel("input.enable_edits", ns = ns, {
                  shinyWidgets::radioGroupButtons(
                    ns("edit_type"), label = NULL, justified = TRUE,
                    choices = c('create_loop', 'crop_audio') %>%
                      stats::setNames(
                        c(
                          glue::glue("{tags$span('Create Loop ', icon('arrows-spin'))}"),
                          glue::glue("{tags$span('Crop Audio ', icon('crop-simple'))}")
                        ) %>% gsub("\\n\\s+", "", .) %>% gsub("\n", "", .)
                      )
                  )
                }),
                conditionalPanel(
                  "input.enable_edits && input.edit_type == 'create_loop'", ns = ns, {
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
                  })
              )
            )
          )
        )
      ),
      plotly::plotlyOutput(ns("audio_plot")) %>% withSpinner(color="#086A87"),
      no_margin_row(
        color = "white",
        column(
          width = 10, offset = 1,
          conditionalPanel(
            "input.enable_edits && input.edit_type == 'create_loop'", ns = ns, {
              tagList(
                h3("Editing: Create a loop"),
                sliderInput(
                  ns("loop_range"), label = NULL, value = c(0, 60), min = 0, max = 60,
                  timeFormat = "%M:%S (%L ms)", dragRange = TRUE
                )
              )
            }),
          conditionalPanel("input.enable_edits && input.edit_type == 'crop_audio'", ns = ns, {
            tagList(
              h3("Editing: Crop Audio"),
              sliderInput(
                ns("crop_range"), label = NULL, value = c(0, 60), min = 0, max = 60,
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

      # Set up environment variables --------------------------------------------

      ns <- session$ns
      .rv <- reactiveValues(playing = NULL, track = NULL, duration = NULL, seek = NULL)
      .rv_editing <- reactiveValues(create_loop = FALSE, crop_audio = FALSE, reset_edit = NULL)
      .rv_loop <- reactiveValues(start = 0, end = 60)

      # Editing
      observeEvent(list(input$enable_edits, input$edit_type, input$show_info),{
        # Reactive values to prevent editing until ready
        .rv_editing$create_loop <- input$enable_edits && input$edit_type == "create_loop"
        .rv_editing$crop_audio <- input$enable_edits && input$edit_type == "crop_audio"
        # Reactive value to trigger positional updates to looping and cropping trackers
        # TODO: (B) Try to remove input$show_info. See TODO (A) for more details.
        .rv_editing$reset_edit <- list(input$edit_type, input$enable_edits, input$show_info)
      }, priority = 4)


      # Set up audio files ------------------------------------------------------


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
        make_howler_ui(audio_files, howler_id = ns("howler"), seek_ping_rate = 100)
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
        plot_wave_audio(audio_obj, type = input$channel_type,
                        source = "wave_audio", include_info = input$show_info)
      })

      # Rendered audio inspection plot
      output$audio_plot <- plotly::renderPlotly({
        audio_plot()
      })

      # Reset some settings on new object - slightly bugged
      observeEvent(audio_obj(), {
        # Reset playback
        seekHowl("howler", 0)
      }, priority = 4)

      # Reset some settings on new object -or- editing toggles
      observeEvent(list(audio_obj(), input$edit_type), {
        duration <- get_audio_dur(shiny::req(audio_obj()))
        dur_fmt <- format_seconds(duration)
        updateSliderInput(session, "loop_range", value = dur_fmt,
                          timeFormat = "%M:%S (%L ms)",
                          min = min(dur_fmt), max = max(dur_fmt), step = 0.25)
        updateSliderInput(session, "crop_range", value = dur_fmt,
                          timeFormat = "%M:%S (%L ms)",
                          min = min(dur_fmt), max = max(dur_fmt), step = 0.25)
        .rv_loop$start <- min(duration)
        .rv_loop$end <- max(duration)
      }, priority = 3)

      # Reset some settings on new object -or- channel type
      observeEvent(list(audio_obj(), input$channel_type, input$show_info), {
        # TODO: (A) Instead of this, it should just be resetting the slider and
        # shapes. This has been difficult with stereo types, so this a placeholder
        # solution. Also need linking via shape name to work.
        shinyWidgets::updateSwitchInput(session, "enable_edits", value = FALSE)
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
        add_play_tracker_line(proxy = proxy, x_val = seek_value)

        # Loop handling
        # Set tolerance (sec) for loop (should be larger than `seek_ping_rate`)
        tol <- 1.1e-1
        x_range <- as.numeric(input$loop_range)
        if(isTRUE(.rv_editing$create_loop) && (abs(seek_value - x_range[2]) <= tol)){
          seekHowl("howler", x_range[1])
        }
      })

      ### Looping ###

      # Add shapes for creating loop on client side
      observeEvent(.rv_editing$reset_edit, {
        # Get proxy reference
        proxy <-  plotly::plotlyProxy("audio_plot", session, deferUntilFlush = TRUE)
        channel_type <- shiny::req(input$channel_type)

        if(isTRUE(.rv_editing$create_loop)){
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
        if(isTRUE(.rv_editing$create_loop)){
          # Get proxy reference
          proxy <-  plotly::plotlyProxy("audio_plot", session, deferUntilFlush = FALSE)
          channel_type <- shiny::req(input$channel_type)

          # Move loop (in minutes) - deletes the old one
          x_range <- as.numeric(input$loop_range)/60
          y_val <- min(get_audio_limits(shiny::req(audio_obj())))
          toggle_loop_trackers(proxy = proxy, toggle = TRUE, update = TRUE,
                               channel_type = channel_type,
                               x_range = x_range, y_val = y_val)
          .rv_loop$start <- min(x_range)
          .rv_loop$end <- max(x_range)
        }
      }, priority = 1)

      # Update playback when start time is moved if:
      # audio is not playing -or- `dynamic_loop` is set
      observeEvent(.rv_loop$start, {
        reset_loop <- input$dynamic_loop || isFALSE(input$howler_playing)
        if(isTRUE(.rv_editing$create_loop) && reset_loop){
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


