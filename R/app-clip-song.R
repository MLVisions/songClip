
#' songClip shiny app
#'
#' @param browser Logical (`TRUE`/`FALSE`). If `TRUE`, launch the app in a browser.
#' @param audio_dir a directory containing audio files.
#'
#' @returns a shiny instance
#' @importFrom htmltools tags p HTML tagList
#' @importFrom shinydashboardPlus box
#'
#'
#'
#' @export
clip_song <- function(browser = FALSE, audio_dir = EXAMPLE_AUDIO_DIR){
  options("songClip.audio_dir" = audio_dir)
  op <- options(digits.secs = 6)
  on.exit(options(op), add = TRUE)
  # print(getOption("digits.secs"))

  ui <-
    shinydashboardPlus::dashboardPage(
      header = shinydashboardPlus::dashboardHeader(
        title = "Audio Tuner",
        # Right UI
        shinydashboard::dropdownMenu(
          type = "notifications",
          headerText = strong("Text to Speech"),
          icon = icon("microphone-lines"),
          badgeStatus = NULL,
          tags$li(
            br(),
            fluidRow(
              column(
                width = 10, offset = 1, align = "center",
                textInput(
                  "text_to_speech", label = NULL, placeholder = "Type something..."
                ),
                shinyWidgets::actionBttn(
                  "speak_btn", "Speak", size = "md",
                  style = "bordered", color = "primary", icon = icon("comment")
                )
              )
            ),
            br()
          )
        )
      ),
      sidebar = shinydashboardPlus::dashboardSidebar(
        collapsed = TRUE,
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem("Tune Audio", tabName = "tab_clipsong", icon = icon("scissors"))
        )
      ),
      body = shinydashboard::dashboardBody(
        tags$head(
          # Set up shinyjs
          shinyjs::useShinyjs(),
          # nice closing message
          shinydisconnect::disconnectMessage(
            text = "App has Disconnected. Try refreshing if this was unexpected."
          ),
          # css and js for header
          tags$style(HTML(cssHeader)),
          tags$script(HTML(jqueryHeader)),
          tags$style(HTML(noWrap_css)),
          # Styling
          tags$link(rel = "stylesheet", type = "text/css", href = file.path(src_name, "css", "styles.css")),
          tags$link(rel = "stylesheet", type = "text/css", href = file.path(src_name, "css", "roboto.css")),
          tags$style(HTML(".content-wrapper, .right-side {
                          color: white; background-color:#086A87;
                          box-shadow: inset 0px 110px 120px 2px #000000;}"))
        ),
        shinydashboard::tabItems(
          shinydashboard::tabItem("tab_clipsong",
                                  tune_audio_ui("tune_audio")
          )
        )
      ),
      title = "DashboardPage"
    )


  server <- function(input, output, session) {

    # TODO: use shinyjs to disable widgets requiring a local audio player if
    # tuneR::getWavPlayer() is not set. Add alert and potentially allow user to
    # search for it (low priority)
    observe({
      audio_player_set <-
        !is.null(tuneR::getWavPlayer()) &&
        nzchar(tuneR::getWavPlayer()) &&
        fs::file_exists(tuneR::getWavPlayer())

      if(isFALSE(audio_player_set)){
        shinyjs::hide("speak_btn")
        shinyjs::disable("text_to_speech")
      }
    })

    # Text to speech
    observeEvent(input$speak_btn, {
      to_speak <- shiny::req(input$text_to_speech)
      processx::process$new(
        "say", args = c("-v", "Daniel", glue::glue("'{to_speak}'"))
      )
    })

    # Core app
    tune_audio_server("tune_audio", audio_dir_init = getOption("songClip.audio_dir"))

  }

  app <- shiny::shinyApp(ui, server)

  # Launch app
  if(isTRUE(browser)){
    withr::with_options(
      list(shiny.launch.browser = .rs.invokeShinyWindowExternal),
      shiny::runApp(app)
    )
  }else{
    withr::with_options(
      list(shiny.launch.browser = .rs.invokeShinyPaneViewer),
      shiny::runApp(app)
    )
  }

}




