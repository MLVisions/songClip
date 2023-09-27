
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
        title = "Audio Tuner"
      ),
      sidebar = shinydashboardPlus::dashboardSidebar(
        collapsed = TRUE,
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem("Tune Audio", tabName = "tab_clipsong", icon = icon("scissors"))
        )
      ),
      body = shinydashboard::dashboardBody(
        tags$head(
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
      controlbar = shinydashboardPlus::dashboardControlbar(),
      title = "DashboardPage"
    )


  server <- function(input, output, session) {

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




