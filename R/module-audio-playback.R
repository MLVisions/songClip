# Module UI ---------------------------------------------------------------

#' Create shiny module for saving youtube videos as audio files
#' @describeIn download_audio_ui Create module UI for saving youtube videos as audio files
#'
#' @keywords internal
audio_playpack_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # tags$head(
    #   # Tracker JS
    #   tags$script(src = file.path(src_name, "js", "updatePlayTracker.js")),
    # ),
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
      plotly::plotlyOutput(ns("audio_plot")) %>% withSpinner(color="#086A87"),
      # Tracker input (not visible)
      conditionalPanel(1==2, ns = ns, {
        textInput(ns("seek_time"), label = NULL)
      })
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
        # update value of tracker on client side
        tracker <- isolate(.rv$seek)/60 # or input$howler_seek for always 0
        plot_wave_audio(audio_obj) %>% add_play_tracker_line(x_val = tracker)
      })

      output$audio_plot <- plotly::renderPlotly({
        audio_plot() #%>% htmlwidgets::onRender(
        #   tracker_js(plot_id = "audio_plot", ns = ns("")),
        #   data = input$howler_seek
        # )
      })


      observeEvent(input$howler_seek, {
        seekValue <- input$howler_seek

        # Get a reference to your Plotly chart using plotlyProxy
        plotlyProxy("audio_plot", session, deferUntilFlush = FALSE) %>%
          # Use plotlyProxyInvoke to update the red line's position
          plotlyProxyInvoke(
            "relayout",
            list(
              shapes = list(
                list(
                  x0 = seekValue,
                  x1 = seekValue,
                  name = "redTrackerLine"  # Specify the shape ID
                )
              )
            )
          )
      })

      # observeEvent(input$howler_seek, {
      #   browser()
      #   jsCode <- sprintf("updateRedLinePosition(document.getElementById('audio_plot'), %s);", input$howler_seek)
      #   shinyjs::runjs(jsCode)
      # })

      # observeEvent(input$howler_seek, {
      #   seekValue <- input$howler_seek
      #
      #   # Get proxy reference
      #   proxy <- plotly::plotlyProxy("audio_plot", session, deferUntilFlush = FALSE)
      #
      #   browser()
      #   # Find the shape index for the red line
      #   shapeIndex <- which(sapply(proxy$getShapes(), function(shape) {
      #     shape$type == "line" && shape$line$color == "red"
      #   }))
      #
      #   # Update the x0 and x1 attributes of the red line shape
      #   if (length(shapeIndex) > 0) {
      #     plotlyProxyInvoke(
      #       proxy, "relayout",
      #       list(
      #         shapes = list(list(
      #           x0 = seekValue,
      #           x1 = seekValue
      #         )),
      #         indices = shapeIndex
      #       ))
      #   }
      # })

      # observeEvent(input$howler_seek, {
      #   # TODO: figure out a way to add the tracker on the client side (too many updates)
      #   # may be able to do something like this:
      #   # https://stackoverflow.com/questions/54822671/r-plotly-how-to-observe-whether-a-trace-is-hidden-or-shown-through-legend-click/54825337#54825337
      #   # https://stackoverflow.com/questions/50138568/removing-traces-by-name-using-plotlyproxy-or-accessing-output-schema-in-reactiv/53831080#53831080
      #   # in both, they make use of `htmlwidgets::onRender`. To do this completely on the client side
      #   # however, we will need a function that passes the the id of the howler object to
      #   # get the seek information stored in `input$howler_seek`
      #   tracker <- shiny::req(input$howler_seek)/60
      #   plotly::plotlyProxy("audio_plot", session)
      # })

      return(
        list(
          selected_track = reactive({selected_track()})
        )
      )

    }
  )
}



#' This is how we will update the plot line; we *may* need another method for attaining
#' the seek information/storing it on the client side
tracker_js <- function(plot_id = "audio_plot", ns = "module_id"){
  # use {{howler_id}} and {{ns}} to access howler object
  # pull `seek` info and store as variable
  # potentially make use of `howlerSeekSlider`

  # This is placeholder JS that does not do what we need yet
  js <- glue::glue("
      function(el, x) {
        var plotlyElement = document.getElementById('{{ns}}-{{plot_id}}');
        var seekValue = x;

        // Find the red line shape by its name or index
        var shapeIndex = 1; // Set this to the index of your red line shape
        var shapes = plotlyElement.layout.shapes;

        // Update the x0 and x1 attributes of the red line shape
        if (shapeIndex !== null && shapes[shapeIndex]) {
          shapes[shapeIndex].x0 = seekValue;
          shapes[shapeIndex].x1 = seekValue;

          // Update the Plotly chart
          Plotly.update(plotlyElement);
        }
      }
", .open = "{{", .close = "}}")

  return(js)
}
