
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

  ui <-
    shinydashboardPlus::dashboardPage(
      header = shinydashboardPlus::dashboardHeader(
        title = "Audio Tuner"
      ),
      sidebar = shinydashboardPlus::dashboardSidebar(
        collapsed = TRUE,
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem("Set Audio Library", tabName = "tab_download", icon = icon("download")),
          shinydashboard::menuItem("Cropping & Tuning", tabName = "tab_clipsong", icon = icon("scissors"))
        )
      ),
      body = shinydashboard::dashboardBody(
        tags$head(
          tags$style(HTML(jsHeader)),
          tags$script(HTML(jqueryHeader)),
          tags$style(HTML(noWrap_css)),
          tags$link(rel = "stylesheet", type = "text/css", href = file.path(src_name, "css", "styles.css")),
          tags$link(rel = "stylesheet", type = "text/css", href = file.path(src_name, "css", "roboto.css")),
        ),
        shinydashboard::tabItems(
          shinydashboard::tabItem("tab_download",
                                  import_audio_ui("import_audio"),
                                  download_audio_ui("download_audio")
          ),
          shinydashboard::tabItem("tab_clipsong",
                                  tune_audio_ui("tune_audio")
          )
        )
      ),
      controlbar = shinydashboardPlus::dashboardControlbar(),
      title = "DashboardPage"
    )


  server <- function(input, output, session) {

    # TODO: This should be a nested module (inside import_audio_server)
    dwnld_audio <- download_audio_server("download_audio")

    imported_audio <- import_audio_server("import_audio", audio_dir = getOption("songClip.audio_dir"))

    tune_audio_server("tune_audio",
                      audio_choices = imported_audio$audio_choices,
                      audio_dir = imported_audio$audio_dir
                      )
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

